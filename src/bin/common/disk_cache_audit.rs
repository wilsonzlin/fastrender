use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

const META_SUFFIX: &str = ".bin.meta";

#[derive(Debug, Clone, Default)]
pub struct DiskCacheAuditOptions {
  pub delete_http_errors: bool,
  pub delete_html_subresources: bool,
  /// Number of URLs to include per category in `top_*` fields (0 disables).
  pub top_n: usize,
}

#[derive(Debug, Clone, Default, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub struct DiskCacheAuditReport {
  pub entries_scanned: usize,
  pub entries_parsed: usize,
  pub invalid_meta_count: usize,

  pub http_error_count: usize,
  pub html_subresource_count: usize,
  pub error_field_count: usize,

  pub deleted_entry_count: usize,
  pub deleted_http_error_entries: usize,
  pub deleted_html_subresource_entries: usize,
  pub deleted_bin_files: usize,
  pub deleted_meta_files: usize,
  pub deleted_alias_files: usize,

  pub top_http_error_urls: Vec<UrlCount>,
  pub top_html_subresource_urls: Vec<UrlCount>,
  pub top_error_urls: Vec<UrlCount>,
}

#[derive(Debug, Clone, Default, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub struct UrlCount {
  pub url: String,
  pub count: usize,
}

#[derive(Debug, Deserialize)]
struct StoredMetadataLite {
  url: String,
  #[serde(default)]
  status: Option<u16>,
  #[serde(default)]
  content_type: Option<String>,
  #[serde(default)]
  error: Option<String>,
}

#[derive(Debug, Deserialize)]
struct StoredAliasLite {
  target: String,
}

fn normalized_mime(content_type: &str) -> String {
  content_type
    .split(';')
    .next()
    .unwrap_or(content_type)
    .trim()
    .to_ascii_lowercase()
}

fn is_html_content_type(content_type: &str) -> bool {
  let mime = normalized_mime(content_type);
  mime == "text/html" || mime == "application/xhtml+xml"
}

fn url_extension(url: &str) -> Option<String> {
  let trimmed = url
    .split('#')
    .next()
    .unwrap_or(url)
    .split('?')
    .next()
    .unwrap_or(url);

  let path = url::Url::parse(trimmed)
    .ok()
    .map(|u| u.path().to_string())
    .unwrap_or_else(|| trimmed.to_string());

  let last = path.rsplit('/').next().unwrap_or(&path);
  if last.is_empty() {
    return None;
  }
  let ext = last.rsplit('.').next().unwrap_or(last);
  if ext == last || ext.is_empty() {
    return None;
  }
  Some(ext.to_ascii_lowercase())
}

fn url_looks_like_static_subresource(url: &str) -> bool {
  let Some(ext) = url_extension(url) else {
    return false;
  };
  matches!(
    ext.as_str(),
    "css"
      | "png"
      | "jpg"
      | "jpeg"
      | "gif"
      | "webp"
      | "avif"
      | "svg"
      | "ico"
      | "bmp"
      | "tif"
      | "tiff"
      | "woff"
      | "woff2"
      | "ttf"
      | "otf"
      | "eot"
  )
}

fn remove_file_if_present(path: &Path) -> usize {
  if !path.exists() {
    return 0;
  }
  match fs::remove_file(path) {
    Ok(()) => 1,
    Err(_) => 0,
  }
}

fn top_urls(map: HashMap<String, usize>, limit: usize) -> Vec<UrlCount> {
  if limit == 0 {
    return Vec::new();
  }
  let mut items: Vec<(String, usize)> = map.into_iter().collect();
  items.sort_by(|(url_a, count_a), (url_b, count_b)| {
    count_b.cmp(count_a).then_with(|| url_a.cmp(url_b))
  });
  items
    .into_iter()
    .take(limit)
    .map(|(url, count)| UrlCount { url, count })
    .collect()
}

fn key_name_from_meta_path(meta_path: &Path) -> Option<String> {
  let name = meta_path.file_name()?.to_string_lossy();
  name.strip_suffix(META_SUFFIX).map(|s| s.to_string())
}

/// Scan a disk cache directory for poisoned entries.
///
/// Cheap/deterministic: single `read_dir` scan (no recursion), best-effort JSON parsing.
pub fn audit_disk_cache_dir(
  cache_dir: &Path,
  options: &DiskCacheAuditOptions,
) -> io::Result<DiskCacheAuditReport> {
  let mut report = DiskCacheAuditReport::default();

  let dir = match fs::read_dir(cache_dir) {
    Ok(dir) => dir,
    Err(err) if err.kind() == io::ErrorKind::NotFound => return Ok(report),
    Err(err) => return Err(err),
  };

  let mut meta_paths: Vec<PathBuf> = Vec::new();
  let mut alias_paths: Vec<PathBuf> = Vec::new();
  for entry in dir {
    let entry = match entry {
      Ok(entry) => entry,
      Err(_) => continue,
    };
    let meta = match entry.metadata() {
      Ok(meta) => meta,
      Err(_) => continue,
    };
    if !meta.file_type().is_file() {
      continue;
    }
    let name = entry.file_name();
    let name = name.to_string_lossy();
    if name.ends_with(META_SUFFIX) {
      meta_paths.push(entry.path());
    } else if name.ends_with(".alias") {
      alias_paths.push(entry.path());
    }
  }
  meta_paths.sort();
  alias_paths.sort();

  let mut http_error_urls: HashMap<String, usize> = HashMap::new();
  let mut html_subresource_urls: HashMap<String, usize> = HashMap::new();
  let mut error_field_urls: HashMap<String, usize> = HashMap::new();
  let mut deleted_entry_urls: HashSet<String> = HashSet::new();

  for meta_path in meta_paths {
    report.entries_scanned += 1;

    let Ok(bytes) = fs::read(&meta_path) else {
      report.invalid_meta_count += 1;
      continue;
    };
    let Ok(meta) = serde_json::from_slice::<StoredMetadataLite>(&bytes) else {
      report.invalid_meta_count += 1;
      continue;
    };
    report.entries_parsed += 1;

    let is_http_error = meta.status.map(|status| status >= 400).unwrap_or(false);
    if is_http_error {
      report.http_error_count += 1;
      *http_error_urls.entry(meta.url.clone()).or_insert(0) += 1;
    }

    let is_html_subresource = meta
      .content_type
      .as_deref()
      .map(is_html_content_type)
      .unwrap_or(false)
      && url_looks_like_static_subresource(&meta.url);
    if is_html_subresource {
      report.html_subresource_count += 1;
      *html_subresource_urls.entry(meta.url.clone()).or_insert(0) += 1;
    }

    let has_error_field = meta.error.is_some();
    if has_error_field {
      report.error_field_count += 1;
      *error_field_urls.entry(meta.url.clone()).or_insert(0) += 1;
    }

    let mut delete_reasons = 0usize;
    if options.delete_http_errors && is_http_error {
      delete_reasons |= 1;
    }
    if options.delete_html_subresources && is_html_subresource {
      delete_reasons |= 2;
    }

    if delete_reasons == 0 {
      continue;
    }

    // Best-effort: derive the cache key from the filename (no recursion, no index parsing).
    let Some(key_name) = key_name_from_meta_path(&meta_path) else {
      continue;
    };
    let data_path = cache_dir.join(format!("{key_name}.bin"));
    let alias_path = cache_dir.join(format!("{key_name}.alias"));

    report.deleted_entry_count += 1;
    if delete_reasons & 1 != 0 {
      report.deleted_http_error_entries += 1;
    }
    if delete_reasons & 2 != 0 {
      report.deleted_html_subresource_entries += 1;
    }

    report.deleted_bin_files += remove_file_if_present(&data_path);
    report.deleted_meta_files += remove_file_if_present(&meta_path);
    report.deleted_alias_files += remove_file_if_present(&alias_path);
    deleted_entry_urls.insert(meta.url);
  }

  // Best-effort cleanup for alias files that redirect to a deleted URL.
  //
  // Alias filenames are derived from hashing the *alias URL* (plus kind + namespace), so we cannot
  // reliably compute which alias files correspond to a deleted entry without parsing the alias file
  // contents. This scan is still a single-level, deterministic iteration.
  if !deleted_entry_urls.is_empty() {
    for alias_path in alias_paths {
      let Ok(bytes) = fs::read(&alias_path) else {
        continue;
      };
      let Ok(alias) = serde_json::from_slice::<StoredAliasLite>(&bytes) else {
        continue;
      };
      if deleted_entry_urls.contains(&alias.target) {
        report.deleted_alias_files += remove_file_if_present(&alias_path);
      }
    }
  }

  report.top_http_error_urls = top_urls(http_error_urls, options.top_n);
  report.top_html_subresource_urls = top_urls(html_subresource_urls, options.top_n);
  report.top_error_urls = top_urls(error_field_urls, options.top_n);

  Ok(report)
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::fs;

  fn write_entry(dir: &Path, key: &str, meta: &str) {
    fs::write(dir.join(format!("{key}.bin")), b"payload").unwrap();
    fs::write(dir.join(format!("{key}.bin.meta")), meta).unwrap();
    fs::write(dir.join(format!("{key}.alias")), b"{\"target\":\"https://example.com/\"}").unwrap();
  }

  #[test]
  fn audits_disk_cache_entries_and_deletes_selected_matches() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path();

    write_entry(
      dir,
      "a",
      r#"{"url":"https://example.com/blocked.css","status":403,"content_type":"text/css","stored_at":1,"len":10}"#,
    );
    write_entry(
      dir,
      "b",
      r#"{"url":"https://example.com/style.css","status":200,"content_type":"text/html; charset=utf-8","stored_at":1,"len":10}"#,
    );
    write_entry(
      dir,
      "c",
      r#"{"url":"https://example.com/image.png","status":null,"content_type":"image/png","stored_at":1,"len":0,"error":"network error"}"#,
    );

    // Alias entries are keyed by the alias URL hash, so create a synthetic alias file that points
    // at the soon-to-be-deleted CSS URL to validate best-effort alias cleanup.
    fs::write(
      dir.join("redirect.alias"),
      br#"{"target":"https://example.com/style.css"}"#,
    )
    .unwrap();

    let opts = DiskCacheAuditOptions {
      delete_http_errors: false,
      delete_html_subresources: false,
      top_n: 10,
    };
    let report = audit_disk_cache_dir(dir, &opts).unwrap();
    assert_eq!(report.entries_scanned, 3);
    assert_eq!(report.entries_parsed, 3);
    assert_eq!(report.invalid_meta_count, 0);
    assert_eq!(report.http_error_count, 1);
    assert_eq!(report.html_subresource_count, 1);
    assert_eq!(report.error_field_count, 1);
    assert_eq!(report.deleted_entry_count, 0);

    let del_opts = DiskCacheAuditOptions {
      delete_http_errors: true,
      delete_html_subresources: true,
      top_n: 0,
    };
    let deleted = audit_disk_cache_dir(dir, &del_opts).unwrap();
    assert_eq!(deleted.deleted_entry_count, 2);
    assert!(deleted.deleted_bin_files >= 2);
    assert!(deleted.deleted_meta_files >= 2);
    assert!(deleted.deleted_alias_files >= 2);

    assert!(!dir.join("a.bin").exists());
    assert!(!dir.join("a.bin.meta").exists());
    assert!(!dir.join("a.alias").exists());
    assert!(!dir.join("b.bin").exists());
    assert!(!dir.join("b.bin.meta").exists());
    assert!(!dir.join("b.alias").exists());
    assert!(
      !dir.join("redirect.alias").exists(),
      "expected alias files pointing at deleted entries to be removed"
    );

    assert!(dir.join("c.bin").exists());
    assert!(dir.join("c.bin.meta").exists());
    assert!(dir.join("c.alias").exists());
  }
}
