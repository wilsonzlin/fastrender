//! Font coverage audit tool.
//!
//! This binary is intended for pageset triage: given some input text, load a deterministic set of
//! fonts and report which Unicode scalar values have no glyph in any loaded face.

use clap::{ArgAction, Args, Parser};
use fastrender::dom;
use fastrender::{FontConfig, FontDatabase, Script};
use std::collections::BTreeSet;
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(
  name = "font_coverage",
  version,
  about = "Audit Unicode glyph coverage for loaded fonts"
)]
struct Cli {
  #[command(flatten)]
  fonts: FontSourceArgs,

  /// Text to audit (direct string input)
  #[arg(
    long,
    conflicts_with = "html_file",
    required_unless_present = "html_file"
  )]
  text: Option<String>,

  /// HTML file to audit (extracts text nodes and skips script/style/template/hidden/inert)
  #[arg(
    long,
    value_name = "FILE",
    conflicts_with = "text",
    required_unless_present = "text"
  )]
  html_file: Option<PathBuf>,
}

#[derive(Args, Debug, Clone)]
struct FontSourceArgs {
  /// Include bundled font fixtures shipped with FastRender
  #[arg(long, action = ArgAction::SetTrue)]
  bundled_fonts: bool,

  /// Include system fonts discovered via fontdb/platform APIs
  #[arg(long, action = ArgAction::SetTrue)]
  system_fonts: bool,

  /// Additional font directories to load (repeatable)
  #[arg(long = "font-dir", value_name = "DIR")]
  font_dir: Vec<PathBuf>,
}

impl FontSourceArgs {
  fn to_font_config(&self) -> FontConfig {
    // Prefer deterministic audits by default: bundled-only unless the caller explicitly asks for
    // something else. Matching pageset_progress behavior: providing --font-dir without an
    // explicit font source implies "only these dirs".
    let explicit_sources = self.bundled_fonts || self.system_fonts;
    let (use_bundled_fonts, use_system_fonts) = if explicit_sources {
      (self.bundled_fonts, self.system_fonts)
    } else if !self.font_dir.is_empty() {
      (false, false)
    } else {
      (true, false)
    };

    FontConfig {
      use_system_fonts,
      use_bundled_fonts,
      font_dirs: self.font_dir.clone(),
    }
  }
}

fn should_skip_codepoint(ch: char) -> bool {
  ch.is_ascii_whitespace() || ch.is_ascii_control()
}

fn script_label(script: Script) -> &'static str {
  #[allow(unreachable_patterns)]
  match script {
    Script::Common => "Common",
    Script::Inherited => "Inherited",
    Script::Unknown => "Unknown",
    Script::Latin => "Latin",
    Script::Arabic => "Arabic",
    Script::Syriac => "Syriac",
    Script::Thaana => "Thaana",
    Script::Nko => "N'Ko",
    Script::Hebrew => "Hebrew",
    Script::Greek => "Greek",
    Script::Cyrillic => "Cyrillic",
    Script::Devanagari => "Devanagari",
    Script::Bengali => "Bengali",
    Script::Tamil => "Tamil",
    Script::Thai => "Thai",
    Script::Javanese => "Javanese",
    Script::Han => "Han",
    Script::Hiragana => "Hiragana",
    Script::Katakana => "Katakana",
    Script::Hangul => "Hangul",
    _ => "Other",
  }
}

fn noto_suggestion(script: Script) -> Option<&'static str> {
  #[allow(unreachable_patterns)]
  match script {
    Script::Arabic => Some("Noto Sans Arabic"),
    Script::Syriac => Some("Noto Sans Syriac"),
    Script::Thaana => Some("Noto Sans Thaana"),
    Script::Nko => Some("Noto Sans NKo"),
    Script::Hebrew => Some("Noto Sans Hebrew"),
    Script::Devanagari => Some("Noto Sans Devanagari"),
    Script::Bengali => Some("Noto Sans Bengali"),
    Script::Tamil => Some("Noto Sans Tamil"),
    Script::Thai => Some("Noto Sans Thai"),
    Script::Javanese => Some("Noto Sans Javanese"),
    Script::Han => Some("Noto Sans CJK (SC/TC/JP/KR)"),
    Script::Hiragana | Script::Katakana => Some("Noto Sans JP"),
    Script::Hangul => Some("Noto Sans KR"),
    Script::Latin | Script::Greek | Script::Cyrillic | Script::Inherited => Some("Noto Sans"),
    Script::Common => Some("Noto Sans Symbols 2"),
    Script::Unknown => None,
    _ => None,
  }
}

fn collect_codepoints_from_text(text: &str) -> Vec<char> {
  let mut set = BTreeSet::new();
  for ch in text.chars() {
    if should_skip_codepoint(ch) {
      continue;
    }
    set.insert(ch);
  }
  set.into_iter().collect()
}

fn collect_codepoints_from_html(html: &str) -> Result<Vec<char>, fastrender::Error> {
  let root = dom::parse_html(html)?;
  let mut set = BTreeSet::new();
  for cp in dom::collect_text_codepoints(&root) {
    let Some(ch) = char::from_u32(cp) else {
      continue;
    };
    if should_skip_codepoint(ch) {
      continue;
    }
    set.insert(ch);
  }
  Ok(set.into_iter().collect())
}

fn find_missing_codepoints(db: &FontDatabase, codepoints: &[char]) -> Vec<char> {
  if codepoints.is_empty() {
    return Vec::new();
  }

  let face_ids: Vec<fontdb::ID> = db.faces().map(|face| face.id).collect();
  let mut missing = vec![true; codepoints.len()];
  let mut remaining = codepoints.len();

  for id in face_ids {
    if remaining == 0 {
      break;
    }
    for (idx, ch) in codepoints.iter().copied().enumerate() {
      if !missing[idx] {
        continue;
      }
      if db.has_glyph_cached(id, ch) {
        missing[idx] = false;
        remaining = remaining.saturating_sub(1);
      }
    }
  }

  codepoints
    .iter()
    .copied()
    .enumerate()
    .filter_map(|(idx, ch)| missing[idx].then_some(ch))
    .collect()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let cli = Cli::parse();
  let font_config = cli.fonts.to_font_config();
  let db = FontDatabase::with_config(&font_config);

  let codepoints = if let Some(text) = cli.text.as_deref() {
    collect_codepoints_from_text(text)
  } else if let Some(path) = cli.html_file.as_ref() {
    let data = fs::read(path)?;
    let html = String::from_utf8_lossy(&data);
    collect_codepoints_from_html(&html)?
  } else {
    unreachable!("clap enforces one input source")
  };

  let missing = find_missing_codepoints(&db, &codepoints);

  println!("loaded_faces: {}", db.font_count());
  println!("unique_codepoints: {}", codepoints.len());
  println!("missing_codepoints: {}", missing.len());
  println!("missing:");
  for ch in missing {
    let cp = ch as u32;
    let script = Script::detect(ch);
    let suggestion = noto_suggestion(script).unwrap_or("-");
    println!(
      "U+{:04X}\tscript={}\tsuggest={}",
      cp,
      script_label(script),
      suggestion
    );
  }

  Ok(())
}
