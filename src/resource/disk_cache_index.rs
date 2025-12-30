// Append-only on-disk journal for disk cache entries. Tracks per-entry sizes and timestamps so
// eviction can be performed without rescanning the entire cache directory on every write. Falls
// back to rebuilding from the actual files when the journal is missing or corrupt.
use super::StoredMetadata;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
pub(super) struct DiskCacheIndex {
  cache_dir: PathBuf,
  journal_path: PathBuf,
  state: Arc<Mutex<IndexState>>,
}

#[derive(Debug, Default)]
struct IndexState {
  loaded: bool,
  entries: HashMap<String, IndexEntry>,
  order: BTreeMap<OrderKey, String>,
  total_bytes: u64,
  journal_len: u64,
  next_order: u64,
  #[cfg(test)]
  rebuilds: usize,
  #[cfg(test)]
  journal_replays: usize,
}

#[derive(Debug, Clone)]
struct IndexEntry {
  stored_at: u64,
  len: u64,
  data_path: PathBuf,
  meta_path: PathBuf,
  order_key: OrderKey,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct OrderKey {
  stored_at: u64,
  order: u64,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "op", rename_all = "snake_case")]
enum JournalRecord {
  Insert {
    key: String,
    stored_at: u64,
    len: u64,
    data_file: String,
    meta_file: String,
  },
  Remove {
    key: String,
  },
}

#[cfg(test)]
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct DebugStats {
  pub rebuilds: usize,
  pub journal_replays: usize,
}

impl DiskCacheIndex {
  pub(super) fn new(cache_dir: PathBuf) -> Self {
    let journal_path = cache_dir.join("index.jsonl");
    Self {
      cache_dir,
      journal_path,
      state: Arc::new(Mutex::new(IndexState::default())),
    }
  }

  pub(super) fn refresh(&self) {
    let mut state = self.state.lock().unwrap();
    if self.refresh_locked(&mut state).is_err() {
      let _ = self.rebuild_from_disk(&mut state);
    }
  }

  pub(super) fn record_insert(
    &self,
    key: &str,
    stored_at: u64,
    len: u64,
    data_path: &Path,
    meta_path: &Path,
  ) {
    let mut state = self.state.lock().unwrap();
    if self.refresh_locked(&mut state).is_err() {
      let _ = self.rebuild_from_disk(&mut state);
    }

    let record = JournalRecord::Insert {
      key: key.to_string(),
      stored_at,
      len,
      data_file: self.relative_path(data_path),
      meta_file: self.relative_path(meta_path),
    };

    if self.append_record_locked(&mut state, &record).is_err() {
      let _ = self.rebuild_from_disk(&mut state);
    }
  }

  pub(super) fn record_removal(&self, key: &str, data_path: &Path, meta_path: &Path) {
    let mut state = self.state.lock().unwrap();
    if self.refresh_locked(&mut state).is_err() {
      let _ = self.rebuild_from_disk(&mut state);
    }

    let _ = self.remove_paths(data_path, meta_path);

    let record = JournalRecord::Remove {
      key: key.to_string(),
    };
    if self.append_record_locked(&mut state, &record).is_err() {
      let _ = self.rebuild_from_disk(&mut state);
    }
  }

  pub(super) fn backfill_if_missing(
    &self,
    key: &str,
    stored_at: u64,
    len: u64,
    data_path: &Path,
    meta_path: &Path,
  ) {
    let mut state = self.state.lock().unwrap();
    if self.refresh_locked(&mut state).is_err() {
      let _ = self.rebuild_from_disk(&mut state);
    }
    if state.entries.contains_key(key) {
      return;
    }
    let record = JournalRecord::Insert {
      key: key.to_string(),
      stored_at,
      len,
      data_file: self.relative_path(data_path),
      meta_file: self.relative_path(meta_path),
    };
    if self.append_record_locked(&mut state, &record).is_err() {
      let _ = self.rebuild_from_disk(&mut state);
    }
  }

  pub(super) fn evict_if_needed<F>(&self, max_bytes: u64, mut can_remove: F)
  where
    F: FnMut(&Path) -> bool,
  {
    if max_bytes == 0 {
      return;
    }

    let mut state = self.state.lock().unwrap();
    if self.refresh_locked(&mut state).is_err() {
      let _ = self.rebuild_from_disk(&mut state);
    }

    let keys: Vec<String> = state.order.iter().map(|(_, key)| key.clone()).collect();
    for key in keys {
      if state.total_bytes <= max_bytes {
        break;
      }
      let Some(entry) = state.entries.get(&key).cloned() else {
        continue;
      };
      if !can_remove(&entry.data_path) {
        continue;
      }
      let _ = self.remove_paths(&entry.data_path, &entry.meta_path);
      let record = JournalRecord::Remove { key };
      if self.append_record_locked(&mut state, &record).is_err() {
        let _ = self.rebuild_from_disk(&mut state);
        break;
      }
    }
  }

  fn refresh_locked(&self, state: &mut IndexState) -> std::io::Result<()> {
    if !state.loaded {
      match self.replay_from_offset(state, 0) {
        Ok(()) => {
          state.loaded = true;
          return Ok(());
        }
        Err(err) => {
          // Fall through to rebuild.
          return Err(err);
        }
      }
    }

    let Ok(meta) = fs::metadata(&self.journal_path) else {
      return Err(std::io::Error::new(
        std::io::ErrorKind::NotFound,
        "index missing",
      ));
    };
    let len = meta.len();
    if len < state.journal_len {
      return Err(std::io::Error::new(
        std::io::ErrorKind::Other,
        "index truncated",
      ));
    }
    if len > state.journal_len {
      self.replay_from_offset(state, state.journal_len)?;
    }
    Ok(())
  }

  fn replay_from_offset(&self, state: &mut IndexState, offset: u64) -> std::io::Result<()> {
    let file = File::open(&self.journal_path)?;
    let mut reader = BufReader::new(file);
    reader.seek(SeekFrom::Start(offset))?;
    let mut line = String::new();
    while reader.read_line(&mut line)? != 0 {
      if line.trim().is_empty() {
        line.clear();
        continue;
      }
      let record: JournalRecord = serde_json::from_str(&line).map_err(|err| {
        std::io::Error::new(
          std::io::ErrorKind::InvalidData,
          format!("invalid journal line: {err}"),
        )
      })?;
      self.apply_record(state, record);
      line.clear();
    }
    #[cfg(test)]
    {
      state.journal_replays += 1;
    }
    state.journal_len = reader.get_ref().metadata()?.len();
    Ok(())
  }

  fn apply_record(&self, state: &mut IndexState, record: JournalRecord) {
    match record {
      JournalRecord::Insert {
        key,
        stored_at,
        len,
        data_file,
        meta_file,
      } => {
        let data_path = self.resolve_path(&data_file);
        let meta_path = self.resolve_path(&meta_file);
        self.apply_insert(state, key, stored_at, len, data_path, meta_path);
      }
      JournalRecord::Remove { key } => {
        self.apply_remove(state, &key);
      }
    }
  }

  fn apply_insert(
    &self,
    state: &mut IndexState,
    key: String,
    stored_at: u64,
    len: u64,
    data_path: PathBuf,
    meta_path: PathBuf,
  ) {
    if let Some(prev) = state.entries.remove(&key) {
      state.order.remove(&prev.order_key);
      state.total_bytes = state.total_bytes.saturating_sub(prev.len);
    }
    let order_key = OrderKey {
      stored_at,
      order: state.next_order,
    };
    state.next_order = state.next_order.wrapping_add(1);
    state.order.insert(order_key, key.clone());
    state.total_bytes = state.total_bytes.saturating_add(len);
    state.entries.insert(
      key,
      IndexEntry {
        stored_at,
        len,
        data_path,
        meta_path,
        order_key,
      },
    );
  }

  fn apply_remove(&self, state: &mut IndexState, key: &str) {
    if let Some(entry) = state.entries.remove(key) {
      state.order.remove(&entry.order_key);
      state.total_bytes = state.total_bytes.saturating_sub(entry.len);
    }
  }

  fn rebuild_from_disk(&self, state: &mut IndexState) -> std::io::Result<()> {
    state.entries.clear();
    state.order.clear();
    state.total_bytes = 0;
    state.journal_len = 0;
    state.next_order = 0;

    let mut entries: Vec<(u64, u64, PathBuf, PathBuf, String)> = Vec::new();
    if let Ok(read_dir) = fs::read_dir(&self.cache_dir) {
      for entry in read_dir.flatten() {
        let path = entry.path();
        if path
          .extension()
          .and_then(|e| e.to_str())
          .map(|ext| ext == "bin")
          != Some(true)
        {
          continue;
        }
        let Some(key) = path
          .file_stem()
          .and_then(|s| s.to_str())
          .map(|s| s.to_string())
        else {
          let meta_path = self.meta_path_for_data(&path);
          let _ = self.remove_paths(&path, &meta_path);
          continue;
        };
        let meta_path = self.meta_path_for_data(&path);
        let Some((meta, len)) = self.read_valid_metadata(&path, &meta_path) else {
          let _ = self.remove_paths(&path, &meta_path);
          continue;
        };
        entries.push((meta.stored_at, len, path.clone(), meta_path, key));
      }
    }

    // `stored_at` has second resolution so many resources will tie. `read_dir` iteration order is
    // filesystem-dependent, so we must provide a deterministic total order when rebuilding.
    entries.sort_by(
      |(a_stored_at, a_len, _, _, a_key), (b_stored_at, b_len, _, _, b_key)| {
        a_stored_at
          .cmp(b_stored_at)
          .then_with(|| a_key.cmp(b_key))
          .then_with(|| a_len.cmp(b_len))
      },
    );
    for (stored_at, len, data_path, meta_path, key) in entries {
      self.apply_insert(state, key, stored_at, len, data_path, meta_path);
    }

    self.write_full_journal(state)?;
    state.loaded = true;
    #[cfg(test)]
    {
      state.rebuilds += 1;
    }
    Ok(())
  }

  fn read_valid_metadata(
    &self,
    data_path: &Path,
    meta_path: &Path,
  ) -> Option<(StoredMetadata, u64)> {
    let meta_bytes = fs::read(meta_path).ok()?;
    let meta: StoredMetadata = serde_json::from_slice(&meta_bytes).ok()?;
    let len = meta.len as u64;
    if len == 0 {
      return None;
    }
    let Ok(data_meta) = fs::metadata(data_path) else {
      return None;
    };
    if data_meta.len() != len {
      return None;
    }
    Some((meta, len))
  }

  fn write_full_journal(&self, state: &mut IndexState) -> std::io::Result<()> {
    let mut file = OpenOptions::new()
      .create(true)
      .write(true)
      .truncate(true)
      .open(&self.journal_path)?;

    for (_, key) in state.order.iter() {
      if let Some(entry) = state.entries.get(key) {
        let record = JournalRecord::Insert {
          key: key.clone(),
          stored_at: entry.stored_at,
          len: entry.len,
          data_file: self.relative_path(&entry.data_path),
          meta_file: self.relative_path(&entry.meta_path),
        };
        let line = serde_json::to_string(&record)?;
        file.write_all(line.as_bytes())?;
        file.write_all(b"\n")?;
      }
    }
    file.flush()?;
    state.journal_len = file.metadata()?.len();
    Ok(())
  }

  fn append_record_locked(
    &self,
    state: &mut IndexState,
    record: &JournalRecord,
  ) -> std::io::Result<()> {
    let start_offset = state.journal_len;
    let mut file = OpenOptions::new()
      .create(true)
      .append(true)
      .open(&self.journal_path)?;
    let line = serde_json::to_string(record)?;
    file.write_all(line.as_bytes())?;
    file.write_all(b"\n")?;
    file.flush()?;
    drop(file);
    self.replay_from_offset(state, start_offset)
  }

  fn meta_path_for_data(&self, data_path: &Path) -> PathBuf {
    let mut meta_path = data_path.to_path_buf();
    meta_path.set_extension("bin.meta");
    meta_path
  }

  fn remove_paths(&self, data_path: &Path, meta_path: &Path) -> std::io::Result<()> {
    let _ = fs::remove_file(data_path);
    let _ = fs::remove_file(meta_path);
    Ok(())
  }

  fn resolve_path(&self, path: &str) -> PathBuf {
    let candidate = PathBuf::from(path);
    if candidate.is_absolute() {
      candidate
    } else {
      self.cache_dir.join(candidate)
    }
  }

  fn relative_path(&self, path: &Path) -> String {
    path
      .strip_prefix(&self.cache_dir)
      .unwrap_or(path)
      .to_string_lossy()
      .to_string()
  }

  #[cfg(test)]
  pub(super) fn debug_stats(&self) -> DebugStats {
    let state = self.state.lock().unwrap();
    DebugStats {
      rebuilds: state.rebuilds,
      journal_replays: state.journal_replays,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::fs;

  fn write_entry(cache_dir: &Path, key: &str, stored_at: u64) {
    let data_path = cache_dir.join(format!("{key}.bin"));
    let body = format!("body-{key}");
    fs::write(&data_path, body.as_bytes()).expect("write data");

    let mut meta_path = data_path.clone();
    meta_path.set_extension("bin.meta");
    let meta = StoredMetadata {
      url: format!("https://example.com/{key}"),
      content_type: Some("application/octet-stream".to_string()),
      etag: None,
      last_modified: None,
      final_url: Some(format!("https://example.com/{key}")),
      stored_at,
      len: body.len(),
      cache: None,
    };
    let meta_bytes = serde_json::to_vec(&meta).expect("serialize meta");
    fs::write(&meta_path, meta_bytes).expect("write meta");
  }

  #[test]
  fn rebuild_orders_colliding_timestamps_by_key() {
    let tmp = tempfile::tempdir().unwrap();
    let stored_at = 1_700_000_000;

    // Write in an order that does not match lexicographic ordering to ensure the rebuild sort is
    // responsible for the final journal ordering.
    write_entry(tmp.path(), "c_key", stored_at);
    write_entry(tmp.path(), "a_key", stored_at);
    write_entry(tmp.path(), "b_key", stored_at);

    let index = DiskCacheIndex::new(tmp.path().to_path_buf());
    index.refresh();
    assert_eq!(
      index.debug_stats().rebuilds,
      1,
      "missing journal should force rebuild"
    );

    let journal_path = tmp.path().join("index.jsonl");
    let journal = fs::read_to_string(journal_path).expect("read journal");
    let mut inserts = Vec::new();
    for line in journal.lines() {
      if line.trim().is_empty() {
        continue;
      }
      let record: JournalRecord = serde_json::from_str(line).expect("parse journal record");
      match record {
        JournalRecord::Insert { key, stored_at, .. } => inserts.push((stored_at, key)),
        JournalRecord::Remove { .. } => panic!("rebuild journal should only contain inserts"),
      }
    }

    let keys: Vec<String> = inserts.iter().map(|(_, key)| key.clone()).collect();
    assert_eq!(
      keys,
      vec![
        "a_key".to_string(),
        "b_key".to_string(),
        "c_key".to_string()
      ]
    );

    assert!(
      inserts.windows(2).all(|w| w[0] <= w[1]),
      "journal inserts should be ordered by (stored_at, key): {inserts:?}"
    );
  }
}
