use pathdiff::diff_paths;
use std::fs;
use std::path::Path;

/// Escape HTML entities for safe embedding.
pub fn escape_html(input: &str) -> String {
  input
    .replace('&', "&amp;")
    .replace('<', "&lt;")
    .replace('>', "&gt;")
    .replace('"', "&quot;")
    .replace('\'', "&#39;")
}

/// Produce a path relative to the HTML/JSON output (falls back to absolute).
pub fn path_for_report(base: &Path, target: &Path) -> String {
  diff_paths(target, base)
    .unwrap_or_else(|| target.to_path_buf())
    .display()
    .to_string()
}

/// Canonicalize for display while tolerating missing paths.
pub fn display_path(path: &Path) -> String {
  fs::canonicalize(path)
    .unwrap_or_else(|_| path.to_path_buf())
    .display()
    .to_string()
}

/// Ensure the parent directory for a file exists.
pub fn ensure_parent_dir(path: &Path) -> Result<(), String> {
  if let Some(parent) = path.parent() {
    if !parent.as_os_str().is_empty() {
      fs::create_dir_all(parent).map_err(|e| {
        format!(
          "failed to create parent directory {}: {e}",
          parent.display()
        )
      })?;
    }
  }
  Ok(())
}
