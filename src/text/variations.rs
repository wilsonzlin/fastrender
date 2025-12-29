use std::collections::BTreeMap;

use rustybuzz::Variation;
use ttf_parser::{Face, Tag};

/// A font variation setting consisting of a 4-byte axis tag and value.
#[derive(Debug, Clone, PartialEq)]
pub struct FontVariation {
  pub tag: Tag,
  pub value: f32,
}

impl FontVariation {
  pub fn new(tag: Tag, value: f32) -> Self {
    Self { tag, value }
  }
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
  let mapped: Vec<_> = variations.iter().map(|v| FontVariation::from(*v)).collect();
  apply_variations_to_face(face, &mapped);
}

/// Converts [`FontVariation`] records to rustybuzz variation coordinates.
pub fn to_rustybuzz_variations(variations: &[FontVariation]) -> Vec<Variation> {
  variations
    .iter()
    .map(|v| Variation {
      tag: v.tag,
      value: v.value,
    })
    .collect()
}

const FNV_OFFSET_BASIS: u64 = 0xcbf29ce484222325;
const FNV_PRIME: u64 = 0x100000001b3;

fn fnv1a_extend(mut hash: u64, byte: u8) -> u64 {
  hash ^= byte as u64;
  hash.wrapping_mul(FNV_PRIME)
}

/// Stable variation hash used for cache keys.
pub fn variation_hash(variations: &[Variation]) -> u64 {
  let mut entries: Vec<([u8; 4], u32)> = variations
    .iter()
    .map(|v| (v.tag.to_bytes(), v.value.to_bits()))
    .collect();
  entries.sort_unstable_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));

  let mut hash = FNV_OFFSET_BASIS;
  for (tag, value_bits) in entries {
    for byte in tag {
      hash = fnv1a_extend(hash, byte);
    }
    for byte in value_bits.to_be_bytes() {
      hash = fnv1a_extend(hash, byte);
    }
  }
  hash
}

/// Normalized variation coordinates for a font face.
///
/// [`ordered`] follows the axis order returned by [`Face::variation_axes`], while
/// [`by_tag`] allows quick lookups by axis tag.
#[derive(Debug, Clone, Default)]
pub struct NormalizedCoords {
  pub ordered: Vec<f32>,
  pub by_tag: BTreeMap<Tag, f32>,
}

/// Computes normalized coordinates for each variation axis in the face.
///
/// Values are clamped to each axis' min/max, normalized against the default per the
/// OpenType spec, and limited to the [-1.0, 1.0] range.
pub fn normalized_coords(face: &Face<'_>, variations: &[Variation]) -> NormalizedCoords {
  let axes: Vec<_> = face.variation_axes().into_iter().collect();
  if axes.is_empty() {
    return NormalizedCoords::default();
  }

  let mut by_tag = BTreeMap::new();
  let ordered = axes
    .iter()
    .map(|axis| {
      let requested = variations
        .iter()
        .find(|v| v.tag == axis.tag)
        .map(|v| v.value)
        .unwrap_or(axis.def_value);
      let clamped = requested.clamp(axis.min_value, axis.max_value);
      let normalized = if clamped < axis.def_value {
        if axis.def_value == axis.min_value {
          0.0
        } else {
          (clamped - axis.def_value) / (axis.def_value - axis.min_value)
        }
      } else if clamped > axis.def_value {
        if axis.max_value == axis.def_value {
          0.0
        } else {
          (clamped - axis.def_value) / (axis.max_value - axis.def_value)
        }
      } else {
        0.0
      }
      .clamp(-1.0, 1.0);
      by_tag.insert(axis.tag, normalized);
      normalized
    })
    .collect();

  NormalizedCoords { ordered, by_tag }
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
    let Ok(cached_face) = font.as_cached_face() else {
      return;
    };
    let mut face = cached_face.clone_face();
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
