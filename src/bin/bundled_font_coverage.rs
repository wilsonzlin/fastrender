//! Bundled font coverage audit tool (pageset-driven, offline).
//!
//! This binary answers: "Which Unicode codepoints used by cached pageset HTML are not covered by
//! the bundled font set?".
//!
//! - Inputs are loaded strictly from disk (`fetches/html/*.html` + optional `.html.meta` sidecars).
//! - Text is extracted from DOM text nodes (skipping script/style/template/hidden/inert subtrees).
//! - Coverage is checked against `FontDatabase::shared_bundled()` (plus bundled emoji when enabled).
//! - Output supports a human summary and a stable JSON report for diffing across commits.

use clap::{ArgAction, Parser};
use fastrender::dom::{self, DomNode, DomNodeType};
use fastrender::html::encoding::decode_html_bytes;
use fastrender::pageset::{
  pageset_entries_with_collisions, pageset_stem, PagesetEntry, PagesetFilter, CACHE_HTML_DIR,
};
use fastrender::{FontDatabase, Script};
use regex::Regex;
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use unicode_general_category::{get_general_category, GeneralCategory};

#[derive(Parser, Debug)]
#[command(
  name = "bundled_font_coverage",
  version,
  about = "Report codepoints used by cached pageset HTML that have no glyph in bundled fonts"
)]
struct Cli {
  /// Scan the canonical pageset (from `src/pageset.rs`).
  #[arg(long, action = ArgAction::SetTrue)]
  pageset: bool,

  /// Pages to scan (comma-separated URLs or cache stems).
  ///
  /// When `--pageset` is present, this filters the pageset. Otherwise it is the explicit list of
  /// cache stems/URLs to scan.
  #[arg(long, value_delimiter = ',')]
  pages: Option<Vec<String>>,

  /// Override the HTML cache directory (defaults to `fetches/html`).
  #[arg(long, default_value = CACHE_HTML_DIR)]
  html_dir: PathBuf,

  /// Also scan inline CSS `content: "..."` strings from `<style>` blocks and `style="..."`.
  #[arg(long, action = ArgAction::SetTrue)]
  include_css_content: bool,

  /// Emit machine-readable JSON to stdout (useful for diffing).
  #[arg(long, action = ArgAction::SetTrue)]
  json: bool,

  /// Maximum uncovered codepoints to print per bucket in the human-readable report.
  #[arg(long, default_value_t = 20)]
  top: usize,

  /// Number of example snippets to store per codepoint (0 disables examples).
  #[arg(long, default_value_t = 1)]
  examples: usize,
}

#[derive(Debug, Clone, Serialize)]
struct PageError {
  cache_stem: String,
  error: String,
}

#[derive(Debug, Clone, Serialize)]
struct Snippet {
  page: String,
  snippet: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
struct Bucket {
  start: u32,
  end: u32,
}

#[derive(Debug, Clone, Serialize)]
struct BucketReport {
  range: String,
  start: u32,
  end: u32,
  uncovered: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
struct JsonReport {
  pages_scanned: Vec<String>,
  missing_cache: Vec<String>,
  read_errors: Vec<PageError>,
  parse_errors: Vec<PageError>,

  unique_codepoints: usize,
  uncovered_codepoints: Vec<String>,
  uncovered_buckets: Vec<BucketReport>,
  examples: BTreeMap<String, Vec<Snippet>>,
}

fn should_skip_codepoint(ch: char) -> bool {
  if ch.is_ascii_whitespace() || ch.is_ascii_control() {
    return true;
  }
  // Format characters (ZWJ, variation selectors, bidi marks, etc.) are not rendered as standalone
  // glyphs; treating them as "missing glyphs" creates noise.
  matches!(get_general_category(ch), GeneralCategory::Format)
}

fn format_codepoint(cp: u32) -> String {
  // Use a minimum of 4 hex digits but allow longer scalars without leading zeros (e.g., U+1F1FA).
  format!("U+{:04X}", cp)
}

fn bucket_for(cp: u32) -> Bucket {
  let start = cp & !0xFF;
  Bucket {
    start,
    end: start.saturating_add(0xFF),
  }
}

fn format_bucket(bucket: Bucket) -> String {
  format!(
    "{}-{}",
    format_codepoint(bucket.start),
    format_codepoint(bucket.end)
  )
}

fn node_is_hidden(attributes: &[(String, String)]) -> bool {
  attributes.iter().any(|(name, value)| {
    if name.eq_ignore_ascii_case("hidden") {
      return true;
    }
    if name.eq_ignore_ascii_case("aria-hidden") {
      return value.is_empty() || value.eq_ignore_ascii_case("true");
    }
    if name.eq_ignore_ascii_case("data-fastr-hidden") {
      return value.eq_ignore_ascii_case("true");
    }
    false
  })
}

fn node_is_inert_like(attributes: &[(String, String)]) -> bool {
  attributes.iter().any(|(name, value)| {
    if name.eq_ignore_ascii_case("inert") {
      return true;
    }
    if name.eq_ignore_ascii_case("data-fastr-inert") {
      return value.eq_ignore_ascii_case("true");
    }
    false
  })
}

fn normalize_snippet(raw: &str, max_chars: usize) -> String {
  let mut out = String::new();
  let mut prev_space = false;
  let mut count = 0usize;
  let mut truncated = false;
  for ch in raw.chars() {
    if ch.is_whitespace() {
      if !prev_space {
        out.push(' ');
        count += 1;
        prev_space = true;
      }
    } else {
      out.push(ch);
      count += 1;
      prev_space = false;
    }
    if count >= max_chars {
      truncated = true;
      break;
    }
  }
  let trimmed = out.trim().to_string();
  if truncated {
    format!("{trimmed}…")
  } else {
    trimmed
  }
}

fn push_example(
  examples: &mut BTreeMap<char, Vec<Snippet>>,
  codepoint: char,
  page: &str,
  snippet: String,
  limit: usize,
) {
  if limit == 0 {
    return;
  }
  let entry = examples.entry(codepoint).or_default();
  if entry.len() >= limit {
    return;
  }
  entry.push(Snippet {
    page: page.to_string(),
    snippet,
  });
}

fn collect_codepoints_from_text(
  text: &str,
  page: &str,
  codepoints: &mut BTreeSet<char>,
  examples: &mut BTreeMap<char, Vec<Snippet>>,
  examples_per_codepoint: usize,
) {
  if text.is_empty() {
    return;
  }

  if examples_per_codepoint == 0 {
    for ch in text.chars() {
      if should_skip_codepoint(ch) {
        continue;
      }
      codepoints.insert(ch);
    }
    return;
  }

  // Store byte offsets for each character so we can slice snippets around a char position without
  // re-scanning the entire string.
  let mut byte_offsets: Vec<usize> = Vec::new();
  for (idx, _) in text.char_indices() {
    byte_offsets.push(idx);
  }
  if byte_offsets.is_empty() {
    return;
  }

  let snippet_radius: usize = 20;
  for (char_idx, ch) in text.chars().enumerate() {
    if should_skip_codepoint(ch) {
      continue;
    }
    codepoints.insert(ch);

    let need_example = examples
      .get(&ch)
      .map(|v| v.len() < examples_per_codepoint)
      .unwrap_or(true);
    if !need_example {
      continue;
    }

    let start_char = char_idx.saturating_sub(snippet_radius);
    let end_char = (char_idx + snippet_radius).min(byte_offsets.len().saturating_sub(1));
    let start_byte = byte_offsets[start_char];
    let end_byte = if end_char + 1 < byte_offsets.len() {
      byte_offsets[end_char + 1]
    } else {
      text.len()
    };
    let window = &text[start_byte..end_byte];
    push_example(
      examples,
      ch,
      page,
      normalize_snippet(window, 120),
      examples_per_codepoint,
    );
  }
}

fn css_unescape(input: &str) -> String {
  let mut out = String::new();
  let mut chars = input.chars().peekable();
  while let Some(ch) = chars.next() {
    if ch != '\\' {
      out.push(ch);
      continue;
    }

    let Some(next) = chars.next() else {
      break;
    };

    if next == '\n' {
      continue;
    }

    if next.is_ascii_hexdigit() {
      let mut value: u32 = next.to_digit(16).unwrap();
      let mut digits = 1usize;
      while digits < 6 {
        match chars.peek().copied() {
          Some(c) if c.is_ascii_hexdigit() => {
            let _ = chars.next();
            value = (value << 4) | c.to_digit(16).unwrap();
            digits += 1;
          }
          _ => break,
        }
      }

      // CSS allows optional whitespace after a hex escape.
      if matches!(chars.peek().copied(), Some(c) if c.is_whitespace()) {
        let _ = chars.next();
      }

      if let Some(escaped) = char::from_u32(value) {
        out.push(escaped);
      }
      continue;
    }

    out.push(next);
  }
  out
}

fn collect_css_content_strings(
  css: &str,
  page: &str,
  codepoints: &mut BTreeSet<char>,
  examples: &mut BTreeMap<char, Vec<Snippet>>,
  examples_per_codepoint: usize,
) {
  // Best-effort extraction: find `content: ...` declarations, then scan quoted strings inside.
  // This keeps the feature lightweight while still catching common pseudo-element content usage.
  static CONTENT_RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
  static STRING_RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();

  let content_re = CONTENT_RE
    .get_or_init(|| Regex::new(r"(?is)(?:^|[;{\s])content\s*:\s*([^;}]*)").expect("content regex"));
  let string_re = STRING_RE.get_or_init(|| {
    Regex::new(r#"(?s)"((?:\\.|[^"\\])*)"|'((?:\\.|[^'\\])*)'"#).expect("string regex")
  });

  // Guard against pathological inline styles (e.g., embedded data URIs).
  let css = if css.len() > 256 * 1024 {
    &css[..256 * 1024]
  } else {
    css
  };

  for caps in content_re.captures_iter(css) {
    let Some(value) = caps.get(1).map(|m| m.as_str()) else {
      continue;
    };
    for str_caps in string_re.captures_iter(value) {
      let raw = str_caps
        .get(1)
        .or_else(|| str_caps.get(2))
        .map(|m| m.as_str())
        .unwrap_or("");
      if raw.is_empty() {
        continue;
      }
      let unescaped = css_unescape(raw);
      collect_codepoints_from_text(
        &unescaped,
        page,
        codepoints,
        examples,
        examples_per_codepoint,
      );
    }
  }
}

fn collect_codepoints_from_dom(
  root: &DomNode,
  page: &str,
  include_css_content: bool,
  codepoints: &mut BTreeSet<char>,
  examples: &mut BTreeMap<char, Vec<Snippet>>,
  examples_per_codepoint: usize,
) {
  let mut stack: Vec<(&DomNode, bool)> = vec![(root, false)];

  while let Some((node, suppressed)) = stack.pop() {
    if suppressed {
      continue;
    }

    match &node.node_type {
      DomNodeType::Text { content } => {
        collect_codepoints_from_text(content, page, codepoints, examples, examples_per_codepoint);
      }
      DomNodeType::Element {
        tag_name,
        attributes,
        ..
      } => {
        let tag = tag_name.as_str();
        if tag.eq_ignore_ascii_case("script") || tag.eq_ignore_ascii_case("template") {
          continue;
        }

        if tag.eq_ignore_ascii_case("style") {
          if include_css_content {
            let mut css = String::new();
            let mut css_stack: Vec<&DomNode> = vec![node];
            while let Some(css_node) = css_stack.pop() {
              match &css_node.node_type {
                DomNodeType::Text { content } => css.push_str(content),
                DomNodeType::Element { .. }
                | DomNodeType::Slot { .. }
                | DomNodeType::ShadowRoot { .. }
                | DomNodeType::Document { .. } => {
                  for child in css_node.children.iter() {
                    css_stack.push(child);
                  }
                }
              }
            }
            collect_css_content_strings(&css, page, codepoints, examples, examples_per_codepoint);
          }
          continue;
        }

        if include_css_content {
          if let Some(style_attr) = attributes
            .iter()
            .find(|(name, _)| name.eq_ignore_ascii_case("style"))
            .map(|(_, v)| v.as_str())
          {
            collect_css_content_strings(
              style_attr,
              page,
              codepoints,
              examples,
              examples_per_codepoint,
            );
          }
        }

        let suppress_children = node_is_hidden(attributes) || node_is_inert_like(attributes);
        for child in node.children.iter() {
          stack.push((child, suppress_children));
        }
      }
      DomNodeType::Slot { attributes, .. } => {
        let suppress_children = node_is_hidden(attributes) || node_is_inert_like(attributes);
        for child in node.children.iter() {
          stack.push((child, suppress_children));
        }
      }
      DomNodeType::ShadowRoot { .. } | DomNodeType::Document { .. } => {
        for child in node.children.iter() {
          stack.push((child, false));
        }
      }
    }
  }
}

fn parse_cached_meta(path: &Path) -> Option<(Option<String>, Option<String>)> {
  let mut meta_path = path.to_path_buf();
  if let Some(ext) = meta_path.extension().and_then(|e| e.to_str()) {
    meta_path.set_extension(format!("{ext}.meta"));
  } else {
    meta_path.set_extension("meta");
  }
  let meta = fs::read_to_string(meta_path).ok()?;
  let mut content_type = None;
  let mut url = None;
  for line in meta.lines() {
    let Some((key, value)) = line.split_once(':') else {
      continue;
    };
    let key = key.trim();
    let value = value.trim();
    if key.eq_ignore_ascii_case("content-type") {
      if !value.is_empty() {
        content_type = Some(value.to_string());
      }
    } else if key.eq_ignore_ascii_case("url") {
      if !value.is_empty() {
        url = Some(value.to_string());
      }
    }
  }
  Some((content_type, url))
}

fn read_cached_html(path: &Path) -> Result<String, String> {
  let bytes = fs::read(path).map_err(|e| format!("read {}: {e}", path.display()))?;
  let (content_type, _) = parse_cached_meta(path).unwrap_or((None, None));
  Ok(decode_html_bytes(&bytes, content_type.as_deref()))
}

fn selected_pages(args: &Cli) -> Result<Vec<PagesetEntry>, String> {
  if args.pageset {
    let (entries, collisions) = pageset_entries_with_collisions();
    if !collisions.is_empty() {
      eprintln!(
        "Warning: pageset stem collisions detected (use the cache stems shown in fetch_pages --allow-collisions):"
      );
      for (stem, urls) in &collisions {
        eprintln!("  {stem}: {}", urls.join(", "));
      }
    }

    let filter = args
      .pages
      .as_ref()
      .and_then(|pages| PagesetFilter::from_inputs(pages));
    let selected: Vec<PagesetEntry> = entries
      .into_iter()
      .filter(|entry| match &filter {
        Some(f) => f.matches_entry(entry),
        None => true,
      })
      .collect();

    if selected.is_empty() {
      if filter.is_some() {
        return Err("No pages matched the provided filter".to_string());
      }
      return Err("No pages to scan".to_string());
    }

    if let Some(filter) = &filter {
      let missing = filter.unmatched(&selected);
      if !missing.is_empty() {
        eprintln!("Warning: unknown pages in filter: {}", missing.join(", "));
      }
    }

    Ok(selected)
  } else {
    let Some(pages) = args.pages.as_ref() else {
      return Err("Pass --pageset to scan the canonical pageset, or provide --pages to scan explicit cache stems/URLs".to_string());
    };

    let mut selected = Vec::new();
    for input in pages.iter().map(|s| s.trim()).filter(|s| !s.is_empty()) {
      let cache_stem = if args.html_dir.join(format!("{input}.html")).exists() {
        input.to_string()
      } else {
        pageset_stem(input).unwrap_or_else(|| input.to_string())
      };
      selected.push(PagesetEntry {
        url: input.to_string(),
        stem: pageset_stem(input).unwrap_or_else(|| cache_stem.clone()),
        cache_stem,
      });
    }

    if selected.is_empty() {
      return Err("No pages to scan (empty --pages)".to_string());
    }
    selected.sort_by(|a, b| a.cache_stem.cmp(&b.cache_stem));
    selected.dedup_by(|a, b| a.cache_stem == b.cache_stem);
    Ok(selected)
  }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let args = Cli::parse();

  let selected = match selected_pages(&args) {
    Ok(p) => p,
    Err(msg) => {
      eprintln!("{msg}");
      std::process::exit(1);
    }
  };

  let db = FontDatabase::shared_bundled();
  let mut codepoints: BTreeSet<char> = BTreeSet::new();
  let mut examples: BTreeMap<char, Vec<Snippet>> = BTreeMap::new();

  let mut pages_scanned: Vec<String> = Vec::new();
  let mut missing_cache: Vec<String> = Vec::new();
  let mut read_errors: Vec<PageError> = Vec::new();
  let mut parse_errors: Vec<PageError> = Vec::new();

  for entry in &selected {
    let cache_path = args.html_dir.join(format!("{}.html", entry.cache_stem));

    if !cache_path.exists() {
      missing_cache.push(entry.cache_stem.clone());
      eprintln!(
        "Warning: missing cache file: {}",
        cache_path.to_string_lossy()
      );
      continue;
    }

    let html = match read_cached_html(&cache_path) {
      Ok(html) => html,
      Err(err) => {
        eprintln!("Warning: {err}");
        read_errors.push(PageError {
          cache_stem: entry.cache_stem.clone(),
          error: err,
        });
        continue;
      }
    };

    let root = match dom::parse_html(&html) {
      Ok(root) => root,
      Err(err) => {
        let msg = err.to_string();
        eprintln!("Warning: failed to parse {}: {}", entry.cache_stem, msg);
        parse_errors.push(PageError {
          cache_stem: entry.cache_stem.clone(),
          error: msg,
        });
        continue;
      }
    };

    pages_scanned.push(entry.cache_stem.clone());
    collect_codepoints_from_dom(
      &root,
      &entry.cache_stem,
      args.include_css_content,
      &mut codepoints,
      &mut examples,
      args.examples,
    );
  }

  if pages_scanned.is_empty() {
    eprintln!("Error: no cached pages were scanned successfully");
    std::process::exit(1);
  }

  let mut uncovered: Vec<char> = Vec::new();
  for ch in &codepoints {
    if !db.any_face_has_glyph_cached(*ch) {
      uncovered.push(*ch);
    }
  }

  let mut uncovered_by_bucket: BTreeMap<Bucket, Vec<char>> = BTreeMap::new();
  for ch in &uncovered {
    uncovered_by_bucket
      .entry(bucket_for(*ch as u32))
      .or_default()
      .push(*ch);
  }

  let uncovered_codepoints: Vec<String> = uncovered
    .iter()
    .map(|c| format_codepoint(*c as u32))
    .collect();
  let uncovered_buckets: Vec<BucketReport> = uncovered_by_bucket
    .into_iter()
    .map(|(bucket, mut chars)| {
      chars.sort_unstable_by(|a, b| (*a as u32).cmp(&(*b as u32)));
      BucketReport {
        range: format_bucket(bucket),
        start: bucket.start,
        end: bucket.end,
        uncovered: chars
          .into_iter()
          .map(|c| format_codepoint(c as u32))
          .collect(),
      }
    })
    .collect();

  if args.json {
    let mut examples_out: BTreeMap<String, Vec<Snippet>> = BTreeMap::new();
    for ch in uncovered.iter().copied() {
      if let Some(snips) = examples.get(&ch) {
        examples_out.insert(format_codepoint(ch as u32), snips.clone());
      }
    }

    let report = JsonReport {
      pages_scanned,
      missing_cache,
      read_errors,
      parse_errors,
      unique_codepoints: codepoints.len(),
      uncovered_codepoints,
      uncovered_buckets,
      examples: examples_out,
    };
    println!("{}", serde_json::to_string_pretty(&report)?);
    if !report.missing_cache.is_empty()
      || !report.read_errors.is_empty()
      || !report.parse_errors.is_empty()
    {
      std::process::exit(1);
    }
    return Ok(());
  }

  println!("bundled_faces: {}", db.font_count());
  println!("pages_requested: {}", selected.len());
  println!("pages_scanned: {}", pages_scanned.len());
  if !missing_cache.is_empty() {
    println!("missing_cache: {}", missing_cache.len());
  }
  if !read_errors.is_empty() {
    println!("read_errors: {}", read_errors.len());
  }
  if !parse_errors.is_empty() {
    println!("parse_errors: {}", parse_errors.len());
  }
  println!("unique_codepoints: {}", codepoints.len());
  println!("uncovered_codepoints: {}", uncovered.len());
  println!();

  if uncovered.is_empty() {
    println!("All codepoints are covered by bundled fonts.");
  } else {
    // Regroup for printing with ordering preserved.
    let mut by_bucket: BTreeMap<Bucket, Vec<char>> = BTreeMap::new();
    for ch in uncovered {
      by_bucket.entry(bucket_for(ch as u32)).or_default().push(ch);
    }

    println!("uncovered_by_bucket:");
    for (bucket, mut chars) in by_bucket {
      chars.sort_unstable_by(|a, b| (*a as u32).cmp(&(*b as u32)));
      println!("  {} ({})", format_bucket(bucket), chars.len());
      for (idx, ch) in chars.iter().copied().enumerate() {
        if idx >= args.top {
          println!("    ... ({} more)", chars.len().saturating_sub(args.top));
          break;
        }
        let cp = ch as u32;
        let script = Script::detect(ch);
        let script_label = format!("{:?}", script);
        if let Some(snips) = examples.get(&ch).and_then(|v| v.first()) {
          println!(
            "    {}\t{}\tscript={}\tpage={}\tsnippet={}",
            format_codepoint(cp),
            ch,
            script_label,
            snips.page,
            snips.snippet
          );
        } else {
          println!(
            "    {}\t{}\tscript={}",
            format_codepoint(cp),
            ch,
            script_label
          );
        }
      }
    }
  }

  // Non-zero exit if the run was incomplete so callers don't accidentally diff partial data.
  if !missing_cache.is_empty() || !read_errors.is_empty() || !parse_errors.is_empty() {
    std::process::exit(1);
  }

  Ok(())
}
