//! Shared helpers for the `xtask` developer workflow binary.

/// Extract `--disk-cache-*` flags from an argument vector while preserving ordering.
///
/// The pageset wrappers forward `args.extra` (intended for `pageset_progress`) through to
/// `prefetch_assets` so cache semantics stay consistent across fetch → prefetch → render.
///
/// This helper intentionally forwards *all* `--disk-cache-*` flags so wrappers do not need to be
/// updated whenever a new disk cache knob is added.
pub fn extract_disk_cache_args(extra: &[String]) -> Vec<String> {
  let mut out = Vec::new();
  let mut iter = extra.iter().peekable();

  while let Some(arg) = iter.next() {
    if !arg.starts_with("--disk-cache-") {
      continue;
    }

    out.push(arg.clone());

    // Support `--disk-cache-foo=value`.
    if arg.contains('=') {
      continue;
    }

    // Support `--disk-cache-foo value` while avoiding mistakenly consuming the next flag for
    // boolean options (e.g. `--disk-cache-allow-no-store`).
    if let Some(next) = iter.peek() {
      if !next.starts_with('-') {
        out.push((*next).clone());
        iter.next();
      }
    }
  }

  out
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct FetchPagesFlagOverrides {
  pub allow_http_error_status: bool,
  pub refresh: bool,
}

/// Extract `fetch_pages`-specific flags from an argument vector.
///
/// `cargo xtask pageset` forwards `args.extra` (intended for `pageset_progress run`) through to the
/// underlying binaries. Some flags apply only to the `fetch_pages` step (e.g. `--refresh`). To keep
/// the wrapper forgiving (and consistent with how we forward `prefetch_assets` flags), strip these
/// from the extra args and return them so the caller can forward them to `fetch_pages`.
pub fn extract_fetch_pages_flag_overrides(
  extra: &[String],
) -> (Vec<String>, FetchPagesFlagOverrides) {
  let mut out = Vec::new();
  let mut overrides = FetchPagesFlagOverrides::default();

  for arg in extra {
    match arg.as_str() {
      "--allow-http-error-status" => overrides.allow_http_error_status = true,
      "--refresh" => overrides.refresh = true,
      _ => out.push(arg.clone()),
    }
  }

  (out, overrides)
}
