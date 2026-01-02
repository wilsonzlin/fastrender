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

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct PagesetExtraArgsOverrides {
  pub pages: Option<String>,
  pub shard: Option<String>,
  pub user_agent: Option<String>,
  pub accept_language: Option<String>,
  pub viewport: Option<String>,
  pub dpr: Option<String>,
}

/// Extract pageset wrapper knobs that should apply to fetch/prefetch/render steps.
///
/// `cargo xtask pageset` has first-class flags for common knobs like `--pages`, but callers
/// sometimes accidentally place them after `--` (intended for `pageset_progress run`). Those flags
/// would still affect `pageset_progress`, but would not filter the `fetch_pages`/`prefetch_assets`
/// steps, wasting time during one-page debugging runs. To keep the wrapper forgiving (and aligned
/// with `scripts/pageset.sh`), strip these from the extra args and return them so the caller can
/// apply them uniformly across the pipeline.
pub fn extract_pageset_extra_arg_overrides(
  extra: &[String],
) -> (Vec<String>, PagesetExtraArgsOverrides) {
  let mut out = Vec::new();
  let mut overrides = PagesetExtraArgsOverrides::default();
  let mut iter = extra.iter().peekable();

  while let Some(arg) = iter.next() {
    match arg.as_str() {
      "--pages" => {
        if let Some(next) = iter.peek() {
          if !next.starts_with('-') {
            overrides.pages = Some((*next).clone());
            iter.next();
            continue;
          }
        }
        out.push(arg.clone());
      }
      "--shard" => {
        if let Some(next) = iter.peek() {
          if !next.starts_with('-') {
            overrides.shard = Some((*next).clone());
            iter.next();
            continue;
          }
        }
        out.push(arg.clone());
      }
      "--user-agent" => {
        if let Some(next) = iter.peek() {
          if !next.starts_with('-') {
            overrides.user_agent = Some((*next).clone());
            iter.next();
            continue;
          }
        }
        out.push(arg.clone());
      }
      "--accept-language" => {
        if let Some(next) = iter.peek() {
          if !next.starts_with('-') {
            overrides.accept_language = Some((*next).clone());
            iter.next();
            continue;
          }
        }
        out.push(arg.clone());
      }
      "--viewport" => {
        if let Some(next) = iter.peek() {
          if !next.starts_with('-') {
            overrides.viewport = Some((*next).clone());
            iter.next();
            continue;
          }
        }
        out.push(arg.clone());
      }
      "--dpr" => {
        if let Some(next) = iter.peek() {
          if !next.starts_with('-') {
            overrides.dpr = Some((*next).clone());
            iter.next();
            continue;
          }
        }
        out.push(arg.clone());
      }
      _ => {
        if let Some(value) = arg.strip_prefix("--pages=") {
          overrides.pages = Some(value.to_string());
          continue;
        }
        if let Some(value) = arg.strip_prefix("--shard=") {
          overrides.shard = Some(value.to_string());
          continue;
        }
        if let Some(value) = arg.strip_prefix("--user-agent=") {
          overrides.user_agent = Some(value.to_string());
          continue;
        }
        if let Some(value) = arg.strip_prefix("--accept-language=") {
          overrides.accept_language = Some(value.to_string());
          continue;
        }
        if let Some(value) = arg.strip_prefix("--viewport=") {
          overrides.viewport = Some(value.to_string());
          continue;
        }
        if let Some(value) = arg.strip_prefix("--dpr=") {
          overrides.dpr = Some(value.to_string());
          continue;
        }
        out.push(arg.clone());
      }
    }
  }

  (out, overrides)
}
