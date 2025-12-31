// Append-only on-disk journal for disk cache entries. Tracks per-entry sizes and timestamps so
// eviction can be performed without rescanning the entire cache directory on every write. Falls
// back to rebuilding from the actual files when the journal is missing or corrupt.
use super::StoredMetadata;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
pub(super) struct DiskCacheIndex {
  cache_dir: PathBuf,
  journal_path: PathBuf,
  state: Arc<Mutex<IndexState>>,
  loaded: Arc<AtomicBool>,
}

#[derive(Debug, Default)]
struct IndexState {
  loaded: bool,
  entries: HashMap<String, IndexEntry>,
  order: BTreeMap<OrderKey, String>,
  total_bytes: u64,
  journal_len: u64,
  pending_backfills: Vec<JournalRecord>,
  journal_append: Option<File>,
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

#[derive(Debug, Serialize, Deserialize, Clone)]
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
      loaded: Arc::new(AtomicBool::new(false)),
    }
  }

  pub(super) fn refresh(&self) {
    let mut state = self.state.lock().unwrap();
    if self.refresh_locked(&mut state).is_err() {
      let _ = self.rebuild_from_disk(&mut state);
    }
    self.loaded.store(state.loaded, Ordering::Release);
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
    self.loaded.store(state.loaded, Ordering::Release);
  }

  pub(super) fn record_insert_and_evict_if_needed<F>(
    &self,
    key: &str,
    stored_at: u64,
    len: u64,
    data_path: &Path,
    meta_path: &Path,
    max_bytes: u64,
    mut can_remove: F,
  ) where
    F: FnMut(&Path) -> bool,
  {
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
      self.loaded.store(state.loaded, Ordering::Release);
      return;
    }

    if let Err(_) = self.evict_if_needed_locked(&mut state, max_bytes, &mut can_remove) {
      let _ = self.rebuild_from_disk(&mut state);
    }
    self.loaded.store(state.loaded, Ordering::Release);
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
    self.loaded.store(state.loaded, Ordering::Release);
  }

  pub(super) fn backfill_if_missing(
    &self,
    key: &str,
    stored_at: u64,
    len: u64,
    data_path: &Path,
    meta_path: &Path,
  ) {
    if !self.loaded.load(Ordering::Acquire) {
      return;
    }
    let mut state = self.state.lock().unwrap();
    // Reads should remain cheap: most pageset workers only ever *read* the cache, and loading the
    // full journal in every process defeats the point of having a disk cache. Only writer
    // processes (which call `record_*`) need a fully loaded index for eviction.
    if !state.loaded {
      return;
    }
    if state.entries.contains_key(key) {
      return;
    }
    self.apply_insert(
      &mut state,
      key.to_string(),
      stored_at,
      len,
      data_path.to_path_buf(),
      meta_path.to_path_buf(),
    );
    state.pending_backfills.push(JournalRecord::Insert {
      key: key.to_string(),
      stored_at,
      len,
      data_file: self.relative_path(data_path),
      meta_file: self.relative_path(meta_path),
    });
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
    if let Err(_) = self.evict_if_needed_locked(&mut state, max_bytes, &mut can_remove) {
      let _ = self.rebuild_from_disk(&mut state);
    }
    self.loaded.store(state.loaded, Ordering::Release);
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
      self.reset_for_replay(state);
      self.replay_from_offset(state, 0)?;
      state.loaded = true;
      return Ok(());
    }
    if len > state.journal_len {
      self.replay_from_offset(state, state.journal_len)?;
    }
    Ok(())
  }

  fn append_records_locked(
    &self,
    state: &mut IndexState,
    records: &[JournalRecord],
  ) -> std::io::Result<()> {
    if records.is_empty() && state.pending_backfills.is_empty() {
      return Ok(());
    }

    let start_offset = state.journal_len;
    let pending = std::mem::take(&mut state.pending_backfills);

    let mut buf = Vec::new();
    for record in pending.iter().chain(records.iter()) {
      let mut line = serde_json::to_vec(record)?;
      line.push(b'\n');
      buf.extend_from_slice(&line);
    }

    let end_offset = {
      let file = self.append_file_locked(state)?;
      file.write_all(&buf)?;
      file.flush()?;
      file.stream_position()?
    };

    let actual_start = end_offset.saturating_sub(buf.len() as u64);
    if actual_start == start_offset {
      // `pending_backfills` were already applied to the in-memory index.
      for record in records {
        self.apply_record(state, record.clone());
      }
      state.loaded = true;
      state.journal_len = end_offset;
      Ok(())
    } else if actual_start > start_offset {
      self.replay_from_offset(state, start_offset)?;
      state.loaded = true;
      Ok(())
    } else {
      self.reset_for_replay(state);
      self.replay_from_offset(state, 0)?;
      state.loaded = true;
      Ok(())
    }
  }

  fn evict_if_needed_locked<F>(
    &self,
    state: &mut IndexState,
    max_bytes: u64,
    can_remove: &mut F,
  ) -> std::io::Result<()>
  where
    F: FnMut(&Path) -> bool,
  {
    if max_bytes == 0 || state.total_bytes <= max_bytes {
      return Ok(());
    }

    let mut to_remove: Vec<String> = Vec::new();
    let mut projected_total = state.total_bytes;
    for (_, key) in state.order.iter() {
      if projected_total <= max_bytes {
        break;
      }
      let Some(entry) = state.entries.get(key) else {
        continue;
      };
      if !can_remove(&entry.data_path) {
        continue;
      }
      to_remove.push(key.clone());
      projected_total = projected_total.saturating_sub(entry.len);
    }

    if to_remove.is_empty() {
      return Ok(());
    }

    let mut records: Vec<JournalRecord> = Vec::with_capacity(to_remove.len());
    for key in to_remove {
      let Some(entry) = state.entries.get(&key).cloned() else {
        continue;
      };
      let _ = self.remove_paths(&entry.data_path, &entry.meta_path);
      self.apply_remove(state, &key);
      records.push(JournalRecord::Remove { key });
      if state.total_bytes <= max_bytes {
        break;
      }
    }

    self.append_records_locked(state, &records)
  }

  fn replay_from_offset(&self, state: &mut IndexState, offset: u64) -> std::io::Result<()> {
    let file = File::open(&self.journal_path)?;
    let mut reader = BufReader::new(file);
    reader.seek(SeekFrom::Start(offset))?;
    let mut line = String::new();
    let mut processed_bytes: u64 = 0;
    loop {
      let n = reader.read_line(&mut line)?;
      if n == 0 {
        break;
      }
      if line.trim().is_empty() {
        processed_bytes = processed_bytes.saturating_add(n as u64);
        line.clear();
        continue;
      }
      let record: JournalRecord = match serde_json::from_str(&line) {
        Ok(record) => record,
        Err(err) => {
          let valid_len = offset.saturating_add(processed_bytes);
          if valid_len == 0 {
            return Err(std::io::Error::new(
              std::io::ErrorKind::InvalidData,
              format!("invalid journal line: {err}"),
            ));
          }

          // Writers can be hard-killed (pageset timeouts) mid-append, leaving a partial JSON line
          // at EOF (or garbage where subsequent appends were concatenated onto it). Rebuilding the
          // whole index by scanning the cache directory is expensive, so prefer truncating the
          // corrupted tail and continuing with a best-effort index.
          if let Ok(file) = OpenOptions::new().write(true).open(&self.journal_path) {
            if file.set_len(valid_len).is_ok() {
              #[cfg(test)]
              {
                state.journal_replays += 1;
              }
              state.journal_len = valid_len;
              return Ok(());
            }
          }

          return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("invalid journal line: {err}"),
          ));
        }
      };
      self.apply_record(state, record);
      processed_bytes = processed_bytes.saturating_add(n as u64);
      line.clear();
    }
    #[cfg(test)]
    {
      state.journal_replays += 1;
    }
    state.journal_len = offset.saturating_add(processed_bytes);
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
    state.pending_backfills.clear();
    state.journal_append = None;
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
    let Ok(data_meta) = fs::metadata(data_path) else {
      return None;
    };
    if data_meta.len() != len {
      return None;
    }
    Some((meta, len))
  }

  fn write_full_journal(&self, state: &mut IndexState) -> std::io::Result<()> {
    state.journal_append = None;
    state.pending_backfills.clear();
    let mut file = OpenOptions::new()
      .create(true)
      .write(true)
      .truncate(true)
      .open(&self.journal_path)?;

    let mut written: u64 = 0;
    for (_, key) in state.order.iter() {
      if let Some(entry) = state.entries.get(key) {
        let record = JournalRecord::Insert {
          key: key.clone(),
          stored_at: entry.stored_at,
          len: entry.len,
          data_file: self.relative_path(&entry.data_path),
          meta_file: self.relative_path(&entry.meta_path),
        };
        let mut line = serde_json::to_vec(&record)?;
        line.push(b'\n');
        file.write_all(&line)?;
        written = written.saturating_add(line.len() as u64);
      }
    }
    file.flush()?;
    state.journal_len = written;
    Ok(())
  }

  fn append_record_locked(
    &self,
    state: &mut IndexState,
    record: &JournalRecord,
  ) -> std::io::Result<()> {
    self.append_records_locked(state, std::slice::from_ref(record))
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

  fn append_file_locked<'a>(&self, state: &'a mut IndexState) -> std::io::Result<&'a mut File> {
    if state.journal_append.is_none() {
      state.journal_append = Some(
        OpenOptions::new()
          .create(true)
          .append(true)
          .open(&self.journal_path)?,
      );
    }
    Ok(state.journal_append.as_mut().expect("just set"))
  }

  fn reset_for_replay(&self, state: &mut IndexState) {
    state.entries.clear();
    state.order.clear();
    state.total_bytes = 0;
    state.journal_len = 0;
    state.pending_backfills.clear();
    state.journal_append = None;
    state.next_order = 0;
    state.loaded = false;
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::fs;
  use std::sync::Arc;
  use std::sync::Barrier;
  use std::thread;

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

  #[test]
  fn concurrent_journal_appends_remain_parseable() {
    let tmp = tempfile::tempdir().unwrap();
    let journal_path = tmp.path().join("index.jsonl");
    fs::write(&journal_path, b"").expect("seed empty journal");

    let threads = 8usize;
    let inserts_per_thread = 64usize;
    let barrier = Arc::new(Barrier::new(threads));

    let mut handles = Vec::new();
    for t in 0..threads {
      let barrier = Arc::clone(&barrier);
      let cache_dir = tmp.path().to_path_buf();
      handles.push(thread::spawn(move || {
        let index = DiskCacheIndex::new(cache_dir.clone());
        barrier.wait();
        for i in 0..inserts_per_thread {
          let key = format!("k{t}_{i}");
          let data_path = cache_dir.join(format!("{key}.bin"));
          let meta_path = cache_dir.join(format!("{key}.bin.meta"));
          index.record_insert(&key, 1_700_000_000, 1, &data_path, &meta_path);
        }
      }));
    }

    for handle in handles {
      handle.join().expect("thread should not panic");
    }

    let journal = fs::read_to_string(&journal_path).expect("read journal");
    for (idx, line) in journal.lines().enumerate() {
      if line.trim().is_empty() {
        continue;
      }
      serde_json::from_str::<JournalRecord>(line)
        .unwrap_or_else(|err| panic!("invalid journal line {idx}: {err} (raw={line:?})"));
    }
  }

  #[test]
  fn refresh_truncates_corrupt_tail_instead_of_rebuilding() {
    let tmp = tempfile::tempdir().unwrap();
    let journal_path = tmp.path().join("index.jsonl");

    let record1 = JournalRecord::Insert {
      key: "first".to_string(),
      stored_at: 1_700_000_000,
      len: 1,
      data_file: "first.bin".to_string(),
      meta_file: "first.bin.meta".to_string(),
    };
    let record2 = JournalRecord::Insert {
      key: "second".to_string(),
      stored_at: 1_700_000_001,
      len: 2,
      data_file: "second.bin".to_string(),
      meta_file: "second.bin.meta".to_string(),
    };

    let line1 = serde_json::to_string(&record1).unwrap();
    let line2 = serde_json::to_string(&record2).unwrap();

    // Simulate a writer being killed mid-write, leaving a partial line, then another writer
    // appending a full record directly after it (concatenating onto the partial bytes).
    let corrupt_tail = format!("{line1}\n{{\"op\":\"insert\",\"key\":\"partial\"{line2}\n");
    fs::write(&journal_path, corrupt_tail).unwrap();

    let index = DiskCacheIndex::new(tmp.path().to_path_buf());
    index.refresh();
    assert_eq!(
      index.debug_stats().rebuilds,
      0,
      "corrupt journal tail should be truncated instead of forcing a full rebuild"
    );

    let journal = fs::read_to_string(&journal_path).unwrap();
    let lines: Vec<&str> = journal.lines().filter(|l| !l.trim().is_empty()).collect();
    assert_eq!(lines.len(), 1, "corrupt tail should be removed");
    serde_json::from_str::<JournalRecord>(lines[0]).expect("remaining line should parse");
  }

  #[test]
  fn backfills_are_flushed_on_next_journal_append() {
    let tmp = tempfile::tempdir().unwrap();
    let journal_path = tmp.path().join("index.jsonl");
    fs::write(&journal_path, b"").expect("seed empty journal");

    let index = DiskCacheIndex::new(tmp.path().to_path_buf());
    index.refresh();

    let orphan_data = tmp.path().join("orphan.bin");
    let orphan_meta = tmp.path().join("orphan.bin.meta");
    index.backfill_if_missing("orphan", 1_700_000_000, 10, &orphan_data, &orphan_meta);

    let contents = fs::read_to_string(&journal_path).expect("read journal");
    assert!(
      contents.trim().is_empty(),
      "backfill should not write to the journal immediately"
    );

    let new_data = tmp.path().join("new.bin");
    let new_meta = tmp.path().join("new.bin.meta");
    index.record_insert("new", 1_700_000_001, 20, &new_data, &new_meta);

    let journal = fs::read_to_string(&journal_path).expect("read journal after append");
    let mut keys = Vec::new();
    for line in journal.lines() {
      if line.trim().is_empty() {
        continue;
      }
      match serde_json::from_str::<JournalRecord>(line).expect("parse journal record") {
        JournalRecord::Insert { key, .. } => keys.push(key),
        JournalRecord::Remove { .. } => {}
      }
    }
    assert_eq!(
      keys,
      vec!["orphan".to_string(), "new".to_string()],
      "pending backfills should be flushed before the next record"
    );
  }
}
