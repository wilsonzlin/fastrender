use std::ops::Range;

use unicode_segmentation::UnicodeSegmentation;

use crate::text::emoji;

/// Returns byte ranges for atomic clusters in `text`.
///
/// Clusters follow extended grapheme boundaries, with emoji sequences merged so
/// they remain indivisible for shaping and font fallback.
pub fn find_atomic_clusters(text: &str) -> Vec<Range<usize>> {
  if text.is_empty() {
    return Vec::new();
  }

  let mut boundaries: Vec<usize> = vec![0];
  boundaries.extend(text.grapheme_indices(true).map(|(idx, _)| idx));
  boundaries.push(text.len());

  let mut emoji_sequences = emoji::find_emoji_sequence_spans(text);
  if !emoji_sequences.is_empty() {
    emoji_sequences.sort_by_key(|seq| seq.start);
    for seq in emoji_sequences {
      boundaries.retain(|&b| b <= seq.start || b >= seq.end);
      boundaries.push(seq.start);
      boundaries.push(seq.end);
    }
  }

  boundaries.sort_unstable();
  boundaries.dedup();

  boundaries.windows(2).map(|pair| pair[0]..pair[1]).collect()
}
