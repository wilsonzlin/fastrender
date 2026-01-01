use std::fs;
use std::io;
use std::path::Path;
use std::time::{Duration, SystemTime};

/// High-level stats about the disk-backed subresource cache directory.
///
/// Intended for cheap, deterministic "cache health" logging in pageset tooling.
/// Implementation is a single `read_dir` scan (no recursion).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DiskCacheStats {
  pub bin_count: usize,
  pub bin_bytes: u64,
  pub meta_count: usize,
  pub alias_count: usize,
  pub lock_count: usize,
  pub stale_lock_count: usize,
  pub tmp_count: usize,
  pub journal_bytes: u64,
  /// Oldest observed mtime/ctime for `*.bin` entries.
  pub bin_oldest_mtime: Option<SystemTime>,
  /// Newest observed mtime/ctime for `*.bin` entries.
  pub bin_newest_mtime: Option<SystemTime>,
}

impl DiskCacheStats {
  pub fn usage_percent_of_max_bytes(&self, max_bytes: u64) -> Option<f64> {
    if max_bytes == 0 {
      return None;
    }
    Some((self.bin_bytes as f64) * 100.0 / (max_bytes as f64))
  }

  pub fn usage_summary(&self, max_bytes: u64) -> String {
    let bin_desc = match self.usage_percent_of_max_bytes(max_bytes) {
      Some(pct) => format!("bin_bytes={} ({pct:.1}% of max_bytes)", self.bin_bytes),
      None => format!("bin_bytes={} (eviction disabled)", self.bin_bytes),
    };
    format!(
      "{bin_desc} locks={} stale_locks={} tmp={} journal={}",
      self.lock_count, self.stale_lock_count, self.tmp_count, self.journal_bytes
    )
  }
}

fn lock_age_from_metadata(now: SystemTime, meta: &fs::Metadata) -> Option<Duration> {
  meta
    .modified()
    .or_else(|_| meta.created())
    .ok()
    .and_then(|time| now.duration_since(time).ok())
}

/// Scan a disk cache directory and return aggregate counts/sizes.
///
/// This performs a single `read_dir` iteration (no recursion).
pub fn scan_disk_cache_dir(
  cache_dir: &Path,
  lock_stale_after: Duration,
) -> io::Result<DiskCacheStats> {
  let mut stats = DiskCacheStats::default();
  let dir = match fs::read_dir(cache_dir) {
    Ok(dir) => dir,
    Err(err) if err.kind() == io::ErrorKind::NotFound => return Ok(stats),
    Err(err) => return Err(err),
  };

  let now = SystemTime::now();

  for entry in dir {
    let entry = match entry {
      Ok(entry) => entry,
      Err(_) => continue,
    };
    let name = entry.file_name();
    let name = name.to_string_lossy();

    let meta = match entry.metadata() {
      Ok(meta) => meta,
      Err(_) => continue,
    };
    if !meta.file_type().is_file() {
      continue;
    }

    if name == "index.jsonl" {
      stats.journal_bytes = meta.len();
      continue;
    }

    if name.ends_with(".tmp") {
      stats.tmp_count += 1;
      continue;
    }

    if name.ends_with(".lock") {
      stats.lock_count += 1;
      let stale = lock_age_from_metadata(now, &meta)
        .map(|age| age > lock_stale_after)
        .unwrap_or(false);
      if stale {
        stats.stale_lock_count += 1;
      }
      continue;
    }

    if name.ends_with(".bin") {
      stats.bin_count += 1;
      stats.bin_bytes = stats.bin_bytes.saturating_add(meta.len());
      if let Ok(mtime) = meta.modified().or_else(|_| meta.created()) {
        stats.bin_oldest_mtime = match stats.bin_oldest_mtime {
          Some(prev) if mtime < prev => Some(mtime),
          Some(prev) => Some(prev),
          None => Some(mtime),
        };
        stats.bin_newest_mtime = match stats.bin_newest_mtime {
          Some(prev) if mtime > prev => Some(mtime),
          Some(prev) => Some(prev),
          None => Some(mtime),
        };
      }
      continue;
    }

    if name.ends_with(".bin.meta") {
      stats.meta_count += 1;
      continue;
    }

    if name.ends_with(".alias") {
      stats.alias_count += 1;
      continue;
    }
  }

  Ok(stats)
}

#[cfg(test)]
mod tests {
  use super::*;
  use filetime::{set_file_mtime, FileTime};
  use std::fs;

  #[test]
  fn scans_flat_disk_cache_dir() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let dir = tmp.path();

    fs::write(dir.join("a.bin"), [0u8; 3]).unwrap();
    fs::write(dir.join("b.bin"), [0u8; 5]).unwrap();
    fs::write(dir.join("a.bin.meta"), b"{}").unwrap();
    fs::write(dir.join("b.bin.meta"), b"{}").unwrap();
    fs::write(dir.join("a.alias"), b"b").unwrap();

    fs::write(dir.join("fresh.bin.lock"), b"lock").unwrap();
    fs::write(dir.join("stale.bin.lock"), b"lock").unwrap();
    fs::write(dir.join("partial.bin.tmp"), b"partial").unwrap();
    fs::write(dir.join("index.jsonl"), [0u8; 7]).unwrap();

    let now = SystemTime::now();
    let stale_age = Duration::from_secs(10);
    let stale_time = now
      .checked_sub(Duration::from_secs(60))
      .unwrap_or(SystemTime::UNIX_EPOCH);
    let fresh_time = now
      .checked_sub(Duration::from_secs(1))
      .unwrap_or(SystemTime::UNIX_EPOCH);

    set_file_mtime(
      dir.join("stale.bin.lock"),
      FileTime::from_system_time(stale_time),
    )
    .unwrap();
    set_file_mtime(
      dir.join("fresh.bin.lock"),
      FileTime::from_system_time(fresh_time),
    )
    .unwrap();

    let stats = scan_disk_cache_dir(dir, stale_age).unwrap();
    assert_eq!(stats.bin_count, 2);
    assert_eq!(stats.bin_bytes, 8);
    assert_eq!(stats.meta_count, 2);
    assert_eq!(stats.alias_count, 1);
    assert_eq!(stats.lock_count, 2);
    assert_eq!(stats.stale_lock_count, 1);
    assert_eq!(stats.tmp_count, 1);
    assert_eq!(stats.journal_bytes, 7);
  }
}
