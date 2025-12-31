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

const NOTO_SANS: &str = "Noto Sans";
const NOTO_SANS_MONO: &str = "Noto Sans Mono";
const NOTO_SANS_SC: &str = "Noto Sans SC";
const NOTO_SANS_JP: &str = "Noto Sans JP";
const NOTO_SANS_KR: &str = "Noto Sans KR";
const NOTO_SANS_SYMBOLS_2: &str = "Noto Sans Symbols 2";
const NOTO_SANS_SYMBOLS: &str = "Noto Sans Symbols";
const STIX_TWO_MATH: &str = "STIX Two Math";

const COMMON_FALLBACKS: &[&str] = &[
  NOTO_SANS,
  NOTO_SANS_SYMBOLS_2,
  NOTO_SANS_SYMBOLS,
  STIX_TWO_MATH,
];
const LATIN_FALLBACKS: &[&str] = &[
  NOTO_SANS,
  NOTO_SANS_SYMBOLS_2,
  NOTO_SANS_SYMBOLS,
  NOTO_SANS_MONO,
  STIX_TWO_MATH,
];

macro_rules! script_fallbacks {
  ($primary:expr) => {
    &[
      $primary,
      NOTO_SANS,
      NOTO_SANS_SYMBOLS_2,
      NOTO_SANS_SYMBOLS,
      STIX_TWO_MATH,
    ]
  };
}

pub(crate) fn preferred_families(script: Script, language: &str) -> &'static [&'static str] {
  match script {
    Script::Common | Script::Inherited | Script::Unknown => COMMON_FALLBACKS,
    Script::Latin | Script::Greek | Script::Cyrillic => LATIN_FALLBACKS,
    Script::Hebrew => script_fallbacks!("Noto Sans Hebrew"),
    Script::Thai => script_fallbacks!("Noto Sans Thai"),
    Script::Bengali => script_fallbacks!("Noto Sans Bengali"),
    Script::Devanagari => script_fallbacks!("Noto Sans Devanagari"),
    Script::Tamil => script_fallbacks!("Noto Sans Tamil"),
    Script::Arabic => script_fallbacks!("Noto Sans Arabic"),
    Script::Syriac => script_fallbacks!("Noto Sans Syriac"),
    Script::Thaana => script_fallbacks!("Noto Sans Thaana"),
    Script::Nko => script_fallbacks!("Noto Sans NKo"),
    Script::Javanese => script_fallbacks!("Noto Sans Javanese"),
    Script::Hangul => script_fallbacks!(NOTO_SANS_KR),
    Script::Han | Script::Hiragana | Script::Katakana => cjk_fallback_families(language),
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn preferred_families_include_pageset_script_fonts() {
    for (script, expected_first) in [
      (Script::Syriac, "Noto Sans Syriac"),
      (Script::Thaana, "Noto Sans Thaana"),
      (Script::Nko, "Noto Sans NKo"),
      (Script::Javanese, "Noto Sans Javanese"),
      (Script::Tamil, "Noto Sans Tamil"),
    ] {
      assert_eq!(preferred_families(script, "")[0], expected_first);
      assert!(
        preferred_families(script, "").contains(&NOTO_SANS),
        "expected {script:?} fallback list to include general text face"
      );
    }
  }

  #[test]
  fn preferred_families_include_bundled_symbol_fonts_for_latin_runs() {
    let families = preferred_families(Script::Latin, "");
    assert_eq!(families[0], NOTO_SANS);
    assert!(families.contains(&NOTO_SANS_SYMBOLS_2));
    assert!(families.contains(&NOTO_SANS_SYMBOLS));
    assert!(families.contains(&STIX_TWO_MATH));
  }

  #[test]
  fn preferred_families_include_symbol_fallbacks_for_script_fonts() {
    let families = preferred_families(Script::Javanese, "");
    assert_eq!(families[0], "Noto Sans Javanese");
    assert!(families.contains(&NOTO_SANS_SYMBOLS_2));
    assert!(families.contains(&NOTO_SANS_SYMBOLS));
  }

  #[test]
  fn preferred_families_use_language_specific_cjk_faces() {
    for (lang, expected) in [
      ("ja", NOTO_SANS_JP),
      ("ja-JP", NOTO_SANS_JP),
      ("ko", NOTO_SANS_KR),
      ("zh", NOTO_SANS_SC),
      ("", NOTO_SANS_SC),
    ] {
      let families = preferred_families(Script::Han, lang);
      assert_eq!(families[0], expected);
      assert!(families.contains(&NOTO_SANS_SYMBOLS_2));
      assert!(families.contains(&NOTO_SANS_SYMBOLS));
      assert!(families.contains(&STIX_TWO_MATH));
    }
  }

  #[test]
  fn preferred_families_treat_neutral_scripts_like_common() {
    for script in [Script::Common, Script::Inherited, Script::Unknown] {
      assert_eq!(preferred_families(script, ""), COMMON_FALLBACKS);
    }
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
    &[
      NOTO_SANS_JP,
      NOTO_SANS_SC,
      NOTO_SANS_KR,
      NOTO_SANS_SYMBOLS_2,
      NOTO_SANS_SYMBOLS,
      STIX_TWO_MATH,
    ]
  } else if primary.eq_ignore_ascii_case("ko") {
    &[
      NOTO_SANS_KR,
      NOTO_SANS_SC,
      NOTO_SANS_JP,
      NOTO_SANS_SYMBOLS_2,
      NOTO_SANS_SYMBOLS,
      STIX_TWO_MATH,
    ]
  } else {
    // Default to Simplified Chinese when language is unknown.
    &[
      NOTO_SANS_SC,
      NOTO_SANS_JP,
      NOTO_SANS_KR,
      NOTO_SANS_SYMBOLS_2,
      NOTO_SANS_SYMBOLS,
      STIX_TWO_MATH,
    ]
  }
}
