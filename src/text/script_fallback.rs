//! Script-aware "system fallback" families.
//!
//! Browsers perform an additional fallback stage after the author-provided
//! `font-family` list is exhausted, preferring fonts that match the Unicode
//! script of the run. FastRender ships a small set of bundled Noto subsets, so
//! we can model that behaviour deterministically by mapping scripts to the
//! preferred bundled families.
//!
//! This is intentionally small and pageset-motivated; it is *not* a full ICU
//! font fallback engine.
//!
//! The mapping is only consulted when author-specified families (including
//! generics) fail to cover a character/cluster.

use crate::text::pipeline::Script;

pub(crate) fn preferred_families(script: Script, language: &str) -> &'static [&'static str] {
  match script {
    Script::Hebrew => &["Noto Sans Hebrew"],
    Script::Thai => &["Noto Sans Thai"],
    Script::Bengali => &["Noto Sans Bengali"],
    Script::Devanagari => &["Noto Sans Devanagari"],
    Script::Arabic => &["Noto Sans Arabic"],
    Script::Syriac => &["Noto Sans Syriac"],
    Script::Thaana => &["Noto Sans Thaana"],
    Script::Nko => &["Noto Sans NKo"],
    Script::Hangul => &["Noto Sans KR"],
    Script::Han | Script::Hiragana | Script::Katakana => cjk_fallback_families(language),
    _ => &[],
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn preferred_families_include_pageset_script_fonts() {
    assert_eq!(preferred_families(Script::Syriac, ""), &["Noto Sans Syriac"]);
    assert_eq!(preferred_families(Script::Thaana, ""), &["Noto Sans Thaana"]);
    assert_eq!(preferred_families(Script::Nko, ""), &["Noto Sans NKo"]);
  }
}

fn cjk_fallback_families(language: &str) -> &'static [&'static str] {
  let primary = language
    .split(|ch| ch == '-' || ch == '_')
    .next()
    .unwrap_or_default();

  if primary.eq_ignore_ascii_case("ja") {
    // Japanese prefers JP glyph shapes, but keep SC/KR as backups when the face
    // isn't available in the font database.
    &["Noto Sans JP", "Noto Sans SC", "Noto Sans KR"]
  } else if primary.eq_ignore_ascii_case("ko") {
    &["Noto Sans KR", "Noto Sans SC", "Noto Sans JP"]
  } else {
    // Default to Simplified Chinese when language is unknown.
    &["Noto Sans SC", "Noto Sans JP", "Noto Sans KR"]
  }
}
