//! Text segmentation helpers for grapheme clusters.
//!
//! These helpers operate purely on Unicode data and do not rely on font shaping
//! results, making them deterministic and suitable as fallbacks when shaping
//! does not report cluster boundaries.

use unicode_segmentation::UnicodeSegmentation;

/// Returns byte offsets for each grapheme cluster boundary in `text`.
///
/// The returned offsets always include the start of the string (0) and the end
/// (`text.len()`), and are sorted and deduplicated.
pub fn segment_grapheme_clusters(text: &str) -> Vec<usize> {
  if text.is_empty() {
    return Vec::new();
  }

  let mut offsets: Vec<usize> = text.grapheme_indices(true).map(|(idx, _)| idx).collect();
  offsets.push(text.len());
  offsets.sort_unstable();
  offsets.dedup();
  offsets
}
