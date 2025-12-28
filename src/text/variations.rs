use std::collections::BTreeMap;

use rustybuzz::Variation;
use ttf_parser::{Face, Tag};

/// A font variation setting consisting of a 4-byte axis tag and value.
#[derive(Debug, Clone, PartialEq)]
pub struct FontVariation {
  pub tag: Tag,
  pub value: f32,
}

impl From<Variation> for FontVariation {
  fn from(v: Variation) -> Self {
    Self {
      tag: v.tag,
      value: v.value,
    }
  }
}

/// Applies variation coordinates to a parsed `ttf_parser::Face`.
///
/// The provided settings are clamped to the axis min/max values and applied in
/// deterministic tag order. Unknown axes are ignored.
pub fn apply_variations_to_face(face: &mut Face<'_>, variations: &[FontVariation]) {
  if variations.is_empty() {
    return;
  }

  let axes: Vec<_> = face.variation_axes().into_iter().collect();
  if axes.is_empty() {
    return;
  }

  let mut clamped: BTreeMap<[u8; 4], (Tag, f32)> = BTreeMap::new();
  for variation in variations {
    if let Some(axis) = axes.iter().find(|axis| axis.tag == variation.tag) {
      let value = variation.value.clamp(axis.min_value, axis.max_value);
      clamped.insert(variation.tag.to_bytes(), (variation.tag, value));
    }
  }

  for (_, (tag, value)) in clamped {
    let _ = face.set_variation(tag, value);
  }
}

/// Convenience wrapper for applying rustybuzz variations to a `ttf_parser::Face`.
pub fn apply_rustybuzz_variations(face: &mut Face<'_>, variations: &[Variation]) {
  if variations.is_empty() {
    return;
  }
  let mapped: Vec<_> = variations
    .iter()
    .map(|v| FontVariation {
      tag: v.tag,
      value: v.value,
    })
    .collect();
  apply_variations_to_face(face, &mapped);
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::text::font_loader::FontContext;

  #[test]
  fn applying_variations_is_stable_on_non_variable_faces() {
    let ctx = FontContext::new();
    let Some(font) = ctx.get_sans_serif() else {
      return;
    };
    let Ok(mut face) = font.as_ttf_face() else {
      return;
    };
    let Some(glyph) = face.glyph_index('A') else {
      return;
    };

    let before = face.glyph_bounding_box(glyph);
    let variations = vec![
      FontVariation {
        tag: Tag::from_bytes(b"wght"),
        value: 900.0,
      },
      FontVariation {
        tag: Tag::from_bytes(b"wdth"),
        value: 10.0,
      },
    ];
    apply_variations_to_face(&mut face, &variations);
    let after_first = face.glyph_bounding_box(glyph);

    let mut reversed = variations;
    reversed.reverse();
    apply_variations_to_face(&mut face, &reversed);
    let after_second = face.glyph_bounding_box(glyph);

    assert_eq!(before, after_first);
    assert_eq!(after_first, after_second);
  }
}
