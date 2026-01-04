//! CSS coverage scanner.
//!
//! This tool walks the fixture directories used by the pageset and reports which CSS property
//! names appear in author styles (including unknown and vendor-prefixed properties).
//!
//! The primary goal is to make CSS feature work evidence-driven: if a property/value shows up
//! frequently in fixtures but is currently unsupported/dropped, it should become an explicit
//! priority instead of guesswork.

use clap::{ArgAction, Parser};
use cssparser::{ParseError, Parser as CssParser, ParserInput, Token};
use fastrender::css::parser::{extract_css_sources, StylesheetSource};
use fastrender::css::supports::supports_declaration;
use fastrender::dom::parse_html;
use rustc_hash::FxHashMap;
use serde::Serialize;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

const DEFAULT_FIXTURES_DIR: &str = "tests/pages/fixtures";

// Large inline URLs (data:...), font subsets, etc. make reports hard to diff. We only keep sample
// values that are small enough to be readable/actionable.
const SAMPLE_VALUE_MAX_LEN: usize = 256;

#[derive(Parser, Debug)]
#[command(
  name = "css_coverage",
  version,
  about = "Scan fixture CSS for property/value coverage gaps"
)]
struct Cli {
  /// Root directory containing pageset fixtures (recursively scanned for .css/.html)
  #[arg(long, value_name = "DIR", default_value = DEFAULT_FIXTURES_DIR)]
  fixtures: PathBuf,

  /// Optional directory containing cached HTML pages (e.g. fetches/html)
  #[arg(long, value_name = "DIR")]
  fetches_html: Option<PathBuf>,

  /// Emit machine-readable JSON instead of the human summary.
  #[arg(long, action = ArgAction::SetTrue)]
  json: bool,

  /// Number of unknown properties to show in the human summary.
  #[arg(long, value_name = "N", default_value_t = 30)]
  top: usize,

  /// For known properties, retain up to N distinct values and validate them via the engine's
  /// @supports value checker.
  #[arg(long, value_name = "N", default_value_t = 5)]
  sample_values: usize,
}

#[derive(Debug, Default)]
struct CoverageCollector {
  files_scanned: u64,
  declarations_seen: u64,
  sample_values_per_property: usize,
  properties: FxHashMap<String, PropertyCounts>,
}

#[derive(Debug, Default)]
struct PropertyCounts {
  count: u64,
  sample_values: Vec<String>,
}

impl CoverageCollector {
  fn new(sample_values_per_property: usize) -> Self {
    Self {
      files_scanned: 0,
      declarations_seen: 0,
      sample_values_per_property,
      properties: FxHashMap::default(),
    }
  }

  fn record_declaration(&mut self, property: String, value: &str) {
    self.declarations_seen = self.declarations_seen.saturating_add(1);
    let entry = self.properties.entry(property).or_default();
    entry.count = entry.count.saturating_add(1);

    if self.sample_values_per_property == 0 || entry.sample_values.len() >= self.sample_values_per_property {
      return;
    }

    let trimmed = value.trim();
    if trimmed.is_empty() || trimmed.len() > SAMPLE_VALUE_MAX_LEN {
      return;
    }

    if entry.sample_values.iter().any(|existing| existing == trimmed) {
      return;
    }

    entry.sample_values.push(trimmed.to_string());
  }

  fn into_report(self) -> CoverageReport {
    let mut properties: Vec<PropertyReport> = self
      .properties
      .into_iter()
      .map(|(name, counts)| build_property_report(name, counts))
      .collect();
    properties.sort_by(|a, b| a.name.cmp(&b.name));

    CoverageReport {
      files_scanned: self.files_scanned,
      declarations_seen: self.declarations_seen,
      properties,
    }
  }
}

fn is_html_path(path: &Path) -> bool {
  matches!(
    path.extension().and_then(OsStr::to_str).map(|s| s.to_ascii_lowercase()),
    Some(ext) if ext == "html" || ext == "htm" || ext == "xhtml"
  )
}

fn is_css_path(path: &Path) -> bool {
  matches!(
    path.extension().and_then(OsStr::to_str).map(|s| s.to_ascii_lowercase()),
    Some(ext) if ext == "css"
  )
}

fn should_scan_path(path: &Path) -> bool {
  is_css_path(path) || is_html_path(path)
}

fn gather_files(root: &Path) -> Vec<PathBuf> {
  let mut files = Vec::new();
  let mut stack = vec![root.to_path_buf()];

  while let Some(dir) = stack.pop() {
    let Ok(entries) = fs::read_dir(&dir) else {
      continue;
    };
    for entry in entries.flatten() {
      let Ok(file_type) = entry.file_type() else {
        continue;
      };
      let path = entry.path();
      if file_type.is_dir() {
        stack.push(path);
        continue;
      }
      if !file_type.is_file() {
        continue;
      }
      if should_scan_path(&path) {
        files.push(path);
      }
    }
  }

  files.sort();
  files
}

fn should_skip_at_rule(name: &str) -> bool {
  // These blocks contain at-rule specific descriptors, not element style declarations.
  matches!(
    name,
    "font-face" | "counter-style" | "property" | "font-palette-values" | "page"
  )
}

fn normalize_property_name(mut name: String) -> String {
  if !name.starts_with("--") {
    name.make_ascii_lowercase();
  }
  name
}

fn skip_nested_block_contents<'i, 't>(parser: &mut CssParser<'i, 't>) {
  let _: std::result::Result<(), ParseError<'i, ()>> = parser.parse_nested_block(|nested| {
    while nested.next_including_whitespace().is_ok() {}
    Ok::<_, ParseError<'i, ()>>(())
  });
}

fn consume_statement_until_block_or_semicolon<'i, 't>(parser: &mut CssParser<'i, 't>) {
  while !parser.is_exhausted() {
    match parser.next() {
      Ok(Token::Semicolon) | Err(_) => break,
      Ok(Token::CurlyBracketBlock) => break,
      Ok(Token::Function(_)) | Ok(Token::ParenthesisBlock) | Ok(Token::SquareBracketBlock) => {
        skip_nested_block_contents(parser)
      }
      Ok(_) => {}
    }
  }
}

fn scan_stylesheet(css: &str, collector: &mut CoverageCollector) {
  let mut input = ParserInput::new(css);
  let mut parser = CssParser::new(&mut input);
  scan_rule_list(&mut parser, collector);
}

fn scan_rule_list<'i, 't>(parser: &mut CssParser<'i, 't>, collector: &mut CoverageCollector) {
  while !parser.is_exhausted() {
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }

    let state = parser.state();
    let token = match parser.next_including_whitespace() {
      Ok(token) => token.clone(),
      Err(_) => break,
    };
    parser.reset(&state);

    match token {
      Token::AtKeyword(_) => scan_at_rule(parser, collector),
      _ => scan_qualified_rule(parser, collector),
    }
  }
}

fn scan_at_rule<'i, 't>(parser: &mut CssParser<'i, 't>, collector: &mut CoverageCollector) {
  let at = match parser.next_including_whitespace() {
    Ok(Token::AtKeyword(name)) => name.to_ascii_lowercase(),
    _ => {
      consume_statement_until_block_or_semicolon(parser);
      return;
    }
  };

  // Consume the prelude.
  while !parser.is_exhausted() {
    match parser.next_including_whitespace() {
      Ok(Token::Semicolon) | Err(_) => return,
      Ok(Token::CurlyBracketBlock) => {
        if should_skip_at_rule(&at) {
          let _ = parser.parse_nested_block(|nested| {
            while nested.next_including_whitespace().is_ok() {}
            Ok::<_, ParseError<'i, ()>>(())
          });
          return;
        }

        let _ = parser.parse_nested_block(|nested| {
          scan_rule_list(nested, collector);
          Ok::<_, ParseError<'i, ()>>(())
        });
        return;
      }
      Ok(Token::Function(_)) | Ok(Token::ParenthesisBlock) | Ok(Token::SquareBracketBlock) => {
        skip_nested_block_contents(parser);
      }
      Ok(_) => {}
    }
  }
}

fn scan_qualified_rule<'i, 't>(parser: &mut CssParser<'i, 't>, collector: &mut CoverageCollector) {
  while !parser.is_exhausted() {
    match parser.next_including_whitespace() {
      Ok(Token::CurlyBracketBlock) => {
        let _ = parser.parse_nested_block(|nested| {
          scan_style_block(nested, collector);
          Ok::<_, ParseError<'i, ()>>(())
        });
        return;
      }
      Ok(Token::Semicolon) | Err(_) => return,
      Ok(Token::Function(_)) | Ok(Token::ParenthesisBlock) | Ok(Token::SquareBracketBlock) => {
        skip_nested_block_contents(parser)
      }
      Ok(_) => {}
    }
  }
}

fn scan_style_block<'i, 't>(parser: &mut CssParser<'i, 't>, collector: &mut CoverageCollector) {
  while !parser.is_exhausted() {
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }

    let state = parser.state();
    match parser.next() {
      Ok(Token::Semicolon) => continue,
      Ok(Token::AtKeyword(_)) => {
        parser.reset(&state);
        scan_at_rule(parser, collector);
        continue;
      }
      Ok(_) => {
        parser.reset(&state);
      }
      Err(_) => break,
    }

    match parser.try_parse(|p| parse_declaration_in_style_block(p)) {
      Ok(Some((property, value))) => {
        collector.record_declaration(property, value);
        continue;
      }
      Ok(None) => continue,
      Err(_) => {}
    }

    scan_qualified_rule(parser, collector);
  }
}

fn parse_declaration_in_style_block<'i, 't>(
  parser: &mut CssParser<'i, 't>,
) -> std::result::Result<Option<(String, &'i str)>, ParseError<'i, ()>> {
  let property = match parser.expect_ident() {
    Ok(ident) => normalize_property_name(ident.as_ref().to_string()),
    Err(_) => return Err(parser.new_custom_error(())),
  };
  let is_custom_property = property.starts_with("--");

  if parser.expect_colon().is_err() {
    // Disambiguate between an invalid declaration ("color red;") and a nested rule ("a b { ... }").
    let mut saw_curly_block = false;
    loop {
      match parser.next() {
        Ok(Token::Semicolon) | Err(_) => break,
        Ok(Token::CurlyBracketBlock) => {
          saw_curly_block = true;
          break;
        }
        Ok(Token::Function(_)) | Ok(Token::ParenthesisBlock) | Ok(Token::SquareBracketBlock) => {
          skip_nested_block_contents(parser)
        }
        Ok(_) => {}
      }
    }

    if saw_curly_block {
      return Err(parser.new_custom_error(()));
    }

    return Ok(None);
  }

  let value_start = parser.position();
  let mut important_pos = None;

  loop {
    let token_start = parser.position();
    match parser.next() {
      Ok(Token::Semicolon) | Err(_) => break,
      Ok(Token::Delim('!')) => {
        if parser
          .try_parse(|p| {
            p.skip_whitespace();
            p.expect_ident_matching("important")
          })
          .is_ok()
        {
          important_pos = Some(token_start);
          continue;
        }
      }
      Ok(Token::CurlyBracketBlock) if !is_custom_property => {
        // Nested rules like `a:hover {}` contain a colon, but must not be treated as declarations.
        return Err(parser.new_custom_error(()));
      }
      Ok(Token::Function(_))
      | Ok(Token::ParenthesisBlock)
      | Ok(Token::SquareBracketBlock)
      | Ok(Token::CurlyBracketBlock) => skip_nested_block_contents(parser),
      Ok(_) => {}
    }

    // `!important` is only valid at the end of the declaration. If we see any further tokens after
    // a candidate `!important`, treat it as part of the value instead of truncating.
    if important_pos.is_some() {
      important_pos = None;
    }
  }

  let full_slice_raw = parser.slice_from(value_start);
  let value = if let Some(pos) = important_pos {
    let important_slice = parser.slice_from(pos);
    let prefix_len = full_slice_raw.len().saturating_sub(important_slice.len());
    full_slice_raw.get(..prefix_len).unwrap_or(full_slice_raw)
  } else {
    full_slice_raw
  };
  let value = value.trim_end_matches(';').trim_end();

  if value.is_empty() && !is_custom_property {
    return Ok(None);
  }

  Ok(Some((property, value)))
}

fn scan_inline_style_attribute(value: &str, collector: &mut CoverageCollector) {
  let mut input = ParserInput::new(value);
  let mut parser = CssParser::new(&mut input);
  while !parser.is_exhausted() {
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }
    match parse_declaration_in_inline_style(&mut parser) {
      Some((property, decl_value)) => collector.record_declaration(property, decl_value),
      None => continue,
    }
  }
}

fn parse_declaration_in_inline_style<'i, 't>(
  parser: &mut CssParser<'i, 't>,
) -> Option<(String, &'i str)> {
  let property = match parser.expect_ident() {
    Ok(ident) => normalize_property_name(ident.as_ref().to_string()),
    Err(_) => {
      consume_statement_until_block_or_semicolon(parser);
      return None;
    }
  };

  if parser.expect_colon().is_err() {
    consume_statement_until_block_or_semicolon(parser);
    return None;
  }

  let value_start = parser.position();
  let mut important_pos = None;

  loop {
    let token_start = parser.position();
    match parser.next() {
      Ok(Token::Semicolon) | Err(_) => break,
      Ok(Token::Delim('!')) => {
        if parser
          .try_parse(|p| {
            p.skip_whitespace();
            p.expect_ident_matching("important")
          })
          .is_ok()
        {
          important_pos = Some(token_start);
          continue;
        }
      }
      Ok(Token::Function(_))
      | Ok(Token::ParenthesisBlock)
      | Ok(Token::SquareBracketBlock)
      | Ok(Token::CurlyBracketBlock) => skip_nested_block_contents(parser),
      Ok(_) => {}
    }

    if important_pos.is_some() {
      important_pos = None;
    }
  }

  let full_slice_raw = parser.slice_from(value_start);
  let value = if let Some(pos) = important_pos {
    let important_slice = parser.slice_from(pos);
    let prefix_len = full_slice_raw.len().saturating_sub(important_slice.len());
    full_slice_raw.get(..prefix_len).unwrap_or(full_slice_raw)
  } else {
    full_slice_raw
  };
  let value = value.trim_end_matches(';').trim_end();

  if value.is_empty() && !property.starts_with("--") {
    return None;
  }

  Some((property, value))
}

fn scan_html_document(contents: &str, collector: &mut CoverageCollector) {
  let dom = match parse_html(contents) {
    Ok(dom) => dom,
    Err(_) => return,
  };

  for scoped in extract_css_sources(&dom) {
    if let StylesheetSource::Inline(style) = scoped.source {
      if style.disabled {
        continue;
      }
      if let Some(ty) = style.type_attr.as_deref() {
        if !ty.trim().is_empty() && !ty.trim().eq_ignore_ascii_case("text/css") {
          continue;
        }
      }
      scan_stylesheet(&style.css, collector);
    }
  }

  dom.walk_tree(&mut |node| {
    if !node.is_element() {
      return;
    }
    for (name, value) in node.attributes_iter() {
      if name.eq_ignore_ascii_case("style") {
        scan_inline_style_attribute(value, collector);
      }
    }
  });
}

fn scan_file(path: &Path, collector: &mut CoverageCollector) {
  let data = match fs::read(path) {
    Ok(data) => data,
    Err(_) => return,
  };
  let contents = String::from_utf8_lossy(&data);

  if is_css_path(path) {
    scan_stylesheet(&contents, collector);
  } else if is_html_path(path) {
    scan_html_document(&contents, collector);
  }
}

fn scan_root(root: &Path, collector: &mut CoverageCollector) {
  if root.is_file() {
    collector.files_scanned = collector.files_scanned.saturating_add(1);
    scan_file(root, collector);
    return;
  }

  if !root.is_dir() {
    return;
  }

  let files = gather_files(root);
  for path in files {
    collector.files_scanned = collector.files_scanned.saturating_add(1);
    scan_file(&path, collector);
  }
}

fn vendor_unprefixed(property: &str) -> Option<&str> {
  if property.starts_with("--") || !property.starts_with('-') {
    return None;
  }
  let rest = property.get(1..)?;
  let idx = rest.find('-')?;
  rest.get(idx + 1..).filter(|s| !s.is_empty())
}

fn build_property_report(name: String, counts: PropertyCounts) -> PropertyReport {
  let is_custom_property = name.starts_with("--");
  let known_style_property = if is_custom_property {
    false
  } else {
    supports_declaration(&name, "initial")
  };

  let unprefixed = vendor_unprefixed(&name).map(|s| s.to_string());
  let unprefixed_known_style_property = unprefixed
    .as_deref()
    .filter(|_| !is_custom_property)
    .map(|prop| supports_declaration(prop, "initial"));

  let sample_values = if known_style_property {
    counts
      .sample_values
      .into_iter()
      .map(|value| {
        let accepted = supports_declaration(&name, &value);
        ValueSample { value, accepted }
      })
      .collect()
  } else {
    Vec::new()
  };

  PropertyReport {
    name,
    count: counts.count,
    known_style_property,
    is_custom_property,
    vendor_prefixed: unprefixed.is_some(),
    unprefixed,
    unprefixed_known_style_property,
    sample_values,
  }
}

fn print_human_summary(report: &CoverageReport, top: usize) {
  let total_properties = report.properties.len();
  let custom_properties = report
    .properties
    .iter()
    .filter(|p| p.is_custom_property)
    .count();
  let known_properties = report
    .properties
    .iter()
    .filter(|p| p.known_style_property)
    .count();
  let vendor_prefixed_unknown_properties = report
    .properties
    .iter()
    .filter(|p| !p.known_style_property && !p.is_custom_property && p.vendor_prefixed)
    .count();
  let vendor_prefixed_unknown_unprefixed_known = report
    .properties
    .iter()
    .filter(|p| {
      !p.known_style_property
        && !p.is_custom_property
        && p.vendor_prefixed
        && p.unprefixed_known_style_property == Some(true)
    })
    .count();
  let unknown_properties = report
    .properties
    .iter()
    .filter(|p| !p.known_style_property && !p.is_custom_property)
    .count();

  println!("files_scanned: {}", report.files_scanned);
  println!("declarations_seen: {}", report.declarations_seen);
  println!("unique_properties: {}", total_properties);
  println!("known_style_properties: {}", known_properties);
  println!("unknown_style_properties: {}", unknown_properties);
  println!("custom_properties: {}", custom_properties);
  println!(
    "vendor_prefixed_unknown_properties: {} (unprefixed known: {})",
    vendor_prefixed_unknown_properties, vendor_prefixed_unknown_unprefixed_known
  );
  println!();

  let mut unknown: Vec<&PropertyReport> = report
    .properties
    .iter()
    .filter(|p| !p.known_style_property && !p.is_custom_property)
    .collect();
  unknown.sort_by(|a, b| {
    b.count
      .cmp(&a.count)
      .then_with(|| a.name.cmp(&b.name))
  });

  println!("Top unknown properties (by count):");
  for (idx, prop) in unknown.iter().take(top).enumerate() {
    if prop.vendor_prefixed {
      let suffix = prop
        .unprefixed
        .as_deref()
        .zip(prop.unprefixed_known_style_property)
        .map(|(unprefixed, known)| {
          if known {
            format!(" (unprefixed known: {unprefixed})")
          } else {
            format!(" (unprefixed unknown: {unprefixed})")
          }
        })
        .unwrap_or_default();
      println!("{:>3}. {}: {}{suffix}", idx + 1, prop.name, prop.count);
    } else {
      println!("{:>3}. {}: {}", idx + 1, prop.name, prop.count);
    }
  }

  let mut unknown_non_vendor: Vec<&PropertyReport> =
    unknown.into_iter().filter(|p| !p.vendor_prefixed).collect();
  unknown_non_vendor.sort_by(|a, b| {
    b.count
      .cmp(&a.count)
      .then_with(|| a.name.cmp(&b.name))
  });

  if !unknown_non_vendor.is_empty() {
    println!();
    println!("Top unknown non-vendor properties (by count):");
    for (idx, prop) in unknown_non_vendor.iter().take(top).enumerate() {
      println!("{:>3}. {}: {}", idx + 1, prop.name, prop.count);
    }
  }

  let mut rejected: Vec<(&str, &ValueSample)> = Vec::new();
  for prop in &report.properties {
    if !prop.known_style_property {
      continue;
    }
    for sample in &prop.sample_values {
      if !sample.accepted {
        rejected.push((prop.name.as_str(), sample));
      }
    }
  }
  rejected.sort_by(|(prop_a, sample_a), (prop_b, sample_b)| {
    prop_a
      .cmp(prop_b)
      .then_with(|| sample_a.value.cmp(&sample_b.value))
  });

  if !rejected.is_empty() {
    println!();
    println!("Sampled rejected values (known properties):");
    for (prop, sample) in rejected.iter().take(top) {
      println!("  {prop}: {}", sample.value);
    }
  }
}

#[derive(Debug, Serialize)]
struct CoverageReport {
  files_scanned: u64,
  declarations_seen: u64,
  properties: Vec<PropertyReport>,
}

#[derive(Debug, Serialize)]
struct PropertyReport {
  name: String,
  count: u64,
  known_style_property: bool,
  is_custom_property: bool,
  vendor_prefixed: bool,
  unprefixed: Option<String>,
  unprefixed_known_style_property: Option<bool>,
  sample_values: Vec<ValueSample>,
}

#[derive(Debug, Serialize)]
struct ValueSample {
  value: String,
  accepted: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let cli = Cli::parse();
  let mut collector = CoverageCollector::new(cli.sample_values);

  scan_root(&cli.fixtures, &mut collector);
  if let Some(dir) = cli.fetches_html.as_ref() {
    scan_root(dir, &mut collector);
  }

  let report = collector.into_report();
  if cli.json {
    println!("{}", serde_json::to_string_pretty(&report)?);
  } else {
    print_human_summary(&report, cli.top);
  }

  Ok(())
}
