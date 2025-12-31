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
    Script::Common => &["Noto Sans Symbols 2", "Noto Sans Symbols", "STIX Two Math"],
    Script::Latin | Script::Greek | Script::Cyrillic => &[
      "Noto Sans",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "Noto Sans Mono",
      "STIX Two Math",
    ],
    Script::Hebrew => &[
      "Noto Sans Hebrew",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ],
    Script::Thai => &[
      "Noto Sans Thai",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ],
    Script::Bengali => &[
      "Noto Sans Bengali",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ],
    Script::Devanagari => &[
      "Noto Sans Devanagari",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ],
    Script::Tamil => &[
      "Noto Sans Tamil",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ],
    Script::Arabic => &[
      "Noto Sans Arabic",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ],
    Script::Syriac => &[
      "Noto Sans Syriac",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ],
    Script::Thaana => &[
      "Noto Sans Thaana",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ],
    Script::Nko => &[
      "Noto Sans NKo",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ],
    Script::Javanese => &[
      "Noto Sans Javanese",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ],
    Script::Hangul => &[
      "Noto Sans KR",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ],
    Script::Han | Script::Hiragana | Script::Katakana => cjk_fallback_families(language),
    _ => &[],
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
    }
  }

  #[test]
  fn preferred_families_include_bundled_symbol_fonts_for_latin_runs() {
    let families = preferred_families(Script::Latin, "");
    assert_eq!(families[0], "Noto Sans");
    assert!(families.contains(&"Noto Sans Symbols 2"));
    assert!(families.contains(&"Noto Sans Symbols"));
    assert!(families.contains(&"STIX Two Math"));
  }

  #[test]
  fn preferred_families_include_symbol_fallbacks_for_script_fonts() {
    let families = preferred_families(Script::Javanese, "");
    assert_eq!(families[0], "Noto Sans Javanese");
    assert!(families.contains(&"Noto Sans Symbols 2"));
    assert!(families.contains(&"Noto Sans Symbols"));
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
      "Noto Sans JP",
      "Noto Sans SC",
      "Noto Sans KR",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ]
  } else if primary.eq_ignore_ascii_case("ko") {
    &[
      "Noto Sans KR",
      "Noto Sans SC",
      "Noto Sans JP",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ]
  } else {
    // Default to Simplified Chinese when language is unknown.
    &[
      "Noto Sans SC",
      "Noto Sans JP",
      "Noto Sans KR",
      "Noto Sans Symbols 2",
      "Noto Sans Symbols",
      "STIX Two Math",
    ]
  }
}
