//! Glyph cluster utilities for cursor positioning and hit testing
//!
//! This module provides utility functions for working with shaped text clusters:
//!
//! - **Cursor positioning**: Finding x coordinates for text positions
//! - **Hit testing**: Converting x coordinates to text positions
//! - **Selection**: Getting selection rectangles for text ranges
//!
//! These utilities work with the `GlyphCluster` type from the shaper module.
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::text::{TextShaper, ShapedGlyphs};
//! use fastrender::text::clustering::{byte_offset_to_x, x_to_byte_offset};
//!
//! // After shaping text
//! let shaped = shaper.shape_text("Hello", &font, 16.0, Script::Latin, TextDirection::Ltr)?;
//!
//! // Find x position for cursor at byte 3
//! let cursor_x = byte_offset_to_x(&shaped.clusters, 3);
//!
//! // Find byte offset from click position
//! let byte_pos = x_to_byte_offset(&shaped.clusters, click_x);
//! ```

use super::shaper::GlyphCluster;
use std::cmp::Ordering;

// ============================================================================
// Cluster Lookup
// ============================================================================

/// Result of a cluster lookup operation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClusterLookup {
  /// Found an exact cluster containing the position
  Found(usize),
  /// Position is before all clusters
  BeforeAll,
  /// Position is after all clusters
  AfterAll,
  /// Position falls between clusters (gap in cluster indices)
  Between(usize, usize),
}

/// Finds the cluster containing a byte position
///
/// Uses binary search for efficiency with large cluster lists.
///
/// # Arguments
///
/// * `clusters` - Sorted slice of clusters
/// * `byte_pos` - Byte position to find
///
/// # Returns
///
/// The cluster lookup result indicating where the position falls.
pub fn find_cluster_at_byte(clusters: &[GlyphCluster], byte_pos: usize) -> ClusterLookup {
  if clusters.is_empty() {
    return ClusterLookup::BeforeAll;
  }

  // Check boundaries
  if byte_pos < clusters[0].text_start {
    return ClusterLookup::BeforeAll;
  }

  let last = &clusters[clusters.len() - 1];
  if byte_pos >= last.text_start + last.text_len {
    return ClusterLookup::AfterAll;
  }

  // Binary search
  match clusters.binary_search_by(|c| {
    let end = c.text_start + c.text_len;
    if byte_pos < c.text_start {
      Ordering::Greater
    } else if byte_pos >= end {
      Ordering::Less
    } else {
      Ordering::Equal
    }
  }) {
    Ok(idx) => ClusterLookup::Found(idx),
    Err(idx) => {
      if idx == 0 {
        ClusterLookup::BeforeAll
      } else if idx >= clusters.len() {
        ClusterLookup::AfterAll
      } else {
        ClusterLookup::Between(idx - 1, idx)
      }
    }
  }
}

/// Finds the cluster containing a glyph index
///
/// # Arguments
///
/// * `clusters` - Slice of clusters
/// * `glyph_idx` - Glyph index to find
///
/// # Returns
///
/// Index of the cluster containing the glyph, or None if not found.
pub fn find_cluster_for_glyph(clusters: &[GlyphCluster], glyph_idx: usize) -> Option<usize> {
  clusters
    .iter()
    .position(|c| glyph_idx >= c.glyph_start && glyph_idx < c.glyph_start + c.glyph_count)
}

// ============================================================================
// Position Utilities
// ============================================================================

/// Gets the byte position at the start of a cluster
pub fn cluster_start_position(clusters: &[GlyphCluster], cluster_idx: usize) -> Option<usize> {
  clusters.get(cluster_idx).map(|c| c.text_start)
}

/// Gets the byte position at the end of a cluster
pub fn cluster_end_position(clusters: &[GlyphCluster], cluster_idx: usize) -> Option<usize> {
  clusters.get(cluster_idx).map(|c| c.text_start + c.text_len)
}

/// Calculates the total advance width for a range of clusters
///
/// # Arguments
///
/// * `clusters` - Slice of clusters
/// * `start_cluster` - First cluster index (inclusive)
/// * `end_cluster` - Last cluster index (exclusive)
///
/// # Returns
///
/// Total advance width in pixels.
pub fn cluster_range_advance(
  clusters: &[GlyphCluster],
  start_cluster: usize,
  end_cluster: usize,
) -> f32 {
  clusters
    .iter()
    .skip(start_cluster)
    .take(end_cluster.saturating_sub(start_cluster))
    .map(|c| c.advance)
    .sum()
}

// ============================================================================
// Cursor Positioning
// ============================================================================

/// Calculates the x position at a byte offset within shaped text
///
/// This is useful for positioning the cursor at a specific character.
///
/// # Arguments
///
/// * `clusters` - Slice of clusters from shaped text
/// * `byte_offset` - Byte position in the original text
///
/// # Returns
///
/// X coordinate in pixels.
pub fn byte_offset_to_x(clusters: &[GlyphCluster], byte_offset: usize) -> f32 {
  if clusters.is_empty() {
    return 0.0;
  }

  let mut x = 0.0f32;

  for cluster in clusters {
    let cluster_end = cluster.text_start + cluster.text_len;

    if byte_offset <= cluster.text_start {
      break;
    }

    if byte_offset >= cluster_end {
      // Past this cluster, add full advance
      x += cluster.advance;
    } else {
      // Within this cluster - interpolate
      let offset_within = (byte_offset - cluster.text_start) as f32;
      let cluster_len = cluster.text_len as f32;
      if cluster_len > 0.0 {
        x += cluster.advance * offset_within / cluster_len;
      }
      break;
    }
  }

  x
}

/// Finds the byte offset closest to an x coordinate
///
/// This is useful for hit testing - determining which character
/// the user clicked on.
///
/// # Arguments
///
/// * `clusters` - Slice of clusters from shaped text
/// * `x` - X coordinate in pixels
///
/// # Returns
///
/// Byte offset in the original text closest to the given x position.
pub fn x_to_byte_offset(clusters: &[GlyphCluster], x: f32) -> usize {
  if clusters.is_empty() || x <= 0.0 {
    return 0;
  }

  let mut current_x = 0.0f32;

  for cluster in clusters {
    let next_x = current_x + cluster.advance;

    if x < next_x {
      // x is within this cluster
      let mid = current_x + cluster.advance / 2.0;
      if x < mid {
        return cluster.text_start;
      } else {
        return cluster.text_start + cluster.text_len;
      }
    }

    current_x = next_x;
  }

  // x is past the end of all clusters
  clusters.last().map_or(0, |c| c.text_start + c.text_len)
}

// ============================================================================
// Text Selection
// ============================================================================

/// Information about a cluster range selection
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ClusterSelection {
  /// Start cluster index
  pub start_cluster: usize,
  /// End cluster index (exclusive)
  pub end_cluster: usize,
  /// Start byte offset
  pub start_byte: usize,
  /// End byte offset
  pub end_byte: usize,
  /// Start x coordinate
  pub start_x: f32,
  /// End x coordinate
  pub end_x: f32,
}

impl ClusterSelection {
  /// Returns the width of the selection
  #[inline]
  pub fn width(&self) -> f32 {
    self.end_x - self.start_x
  }

  /// Returns the number of bytes in the selection
  #[inline]
  pub fn byte_len(&self) -> usize {
    self.end_byte.saturating_sub(self.start_byte)
  }
}

/// Gets the selection information for a byte range
///
/// # Arguments
///
/// * `clusters` - Slice of clusters
/// * `start_byte` - Start byte offset
/// * `end_byte` - End byte offset
///
/// # Returns
///
/// Selection information including cluster indices and x coordinates.
pub fn get_selection(
  clusters: &[GlyphCluster],
  start_byte: usize,
  end_byte: usize,
) -> ClusterSelection {
  // Normalize the byte range
  let (start_byte, end_byte) = if start_byte <= end_byte {
    (start_byte, end_byte)
  } else {
    (end_byte, start_byte)
  };

  // Find start and end clusters
  let start_cluster = match find_cluster_at_byte(clusters, start_byte) {
    ClusterLookup::Found(idx) => idx,
    ClusterLookup::BeforeAll => 0,
    ClusterLookup::AfterAll => clusters.len().saturating_sub(1),
    ClusterLookup::Between(_, idx) => idx,
  };

  let end_cluster = match find_cluster_at_byte(clusters, end_byte.saturating_sub(1)) {
    ClusterLookup::Found(idx) => idx + 1,
    ClusterLookup::BeforeAll => 0,
    ClusterLookup::AfterAll => clusters.len(),
    ClusterLookup::Between(_, idx) => idx,
  };

  let start_x = byte_offset_to_x(clusters, start_byte);
  let end_x = byte_offset_to_x(clusters, end_byte);

  ClusterSelection {
    start_cluster,
    end_cluster,
    start_byte,
    end_byte,
    start_x,
    end_x,
  }
}

// ============================================================================
// Grapheme Counting
// ============================================================================

/// Counts the number of characters in a text range
///
/// This provides a character count (Unicode scalar values).
/// For true grapheme cluster counting, use the unicode-segmentation crate.
///
/// # Arguments
///
/// * `text` - The original text
/// * `start_byte` - Start byte offset
/// * `end_byte` - End byte offset
///
/// # Returns
///
/// Number of characters in the range.
pub fn count_chars_in_range(text: &str, start_byte: usize, end_byte: usize) -> usize {
  if start_byte >= text.len() || end_byte <= start_byte {
    return 0;
  }

  let end_byte = end_byte.min(text.len());

  // Ensure we're at valid UTF-8 boundaries
  if !text.is_char_boundary(start_byte) || !text.is_char_boundary(end_byte) {
    return 0;
  }

  text[start_byte..end_byte].chars().count()
}

#[cfg(test)]
mod tests {
  use super::*;

  // Helper to create a test cluster
  fn cluster(
    text_start: usize,
    text_len: usize,
    glyph_start: usize,
    glyph_count: usize,
    advance: f32,
  ) -> GlyphCluster {
    GlyphCluster::new(text_start, text_len, glyph_start, glyph_count, advance)
  }

  // ========================================================================
  // find_cluster_at_byte Tests
  // ========================================================================

  #[test]
  fn test_find_cluster_at_byte_empty() {
    assert_eq!(find_cluster_at_byte(&[], 0), ClusterLookup::BeforeAll);
  }

  #[test]
  fn test_find_cluster_at_byte_found() {
    let clusters = vec![
      cluster(0, 5, 0, 1, 50.0),
      cluster(5, 5, 1, 1, 50.0),
      cluster(10, 5, 2, 1, 50.0),
    ];

    assert_eq!(find_cluster_at_byte(&clusters, 0), ClusterLookup::Found(0));
    assert_eq!(find_cluster_at_byte(&clusters, 3), ClusterLookup::Found(0));
    assert_eq!(find_cluster_at_byte(&clusters, 5), ClusterLookup::Found(1));
    assert_eq!(find_cluster_at_byte(&clusters, 12), ClusterLookup::Found(2));
  }

  #[test]
  fn test_find_cluster_at_byte_before() {
    let clusters = vec![cluster(5, 5, 0, 1, 50.0)];

    assert_eq!(find_cluster_at_byte(&clusters, 0), ClusterLookup::BeforeAll);
    assert_eq!(find_cluster_at_byte(&clusters, 4), ClusterLookup::BeforeAll);
  }

  #[test]
  fn test_find_cluster_at_byte_after() {
    let clusters = vec![cluster(0, 5, 0, 1, 50.0)];

    assert_eq!(find_cluster_at_byte(&clusters, 5), ClusterLookup::AfterAll);
    assert_eq!(
      find_cluster_at_byte(&clusters, 100),
      ClusterLookup::AfterAll
    );
  }

  // ========================================================================
  // find_cluster_for_glyph Tests
  // ========================================================================

  #[test]
  fn test_find_cluster_for_glyph() {
    let clusters = vec![
      cluster(0, 5, 0, 2, 50.0),  // glyphs 0-1
      cluster(5, 5, 2, 3, 50.0),  // glyphs 2-4
      cluster(10, 5, 5, 1, 50.0), // glyph 5
    ];

    assert_eq!(find_cluster_for_glyph(&clusters, 0), Some(0));
    assert_eq!(find_cluster_for_glyph(&clusters, 1), Some(0));
    assert_eq!(find_cluster_for_glyph(&clusters, 2), Some(1));
    assert_eq!(find_cluster_for_glyph(&clusters, 4), Some(1));
    assert_eq!(find_cluster_for_glyph(&clusters, 5), Some(2));
    assert_eq!(find_cluster_for_glyph(&clusters, 6), None);
  }

  // ========================================================================
  // Position Tests
  // ========================================================================

  #[test]
  fn test_cluster_positions() {
    let clusters = vec![cluster(0, 5, 0, 1, 50.0), cluster(5, 5, 1, 1, 60.0)];

    assert_eq!(cluster_start_position(&clusters, 0), Some(0));
    assert_eq!(cluster_end_position(&clusters, 0), Some(5));
    assert_eq!(cluster_start_position(&clusters, 1), Some(5));
    assert_eq!(cluster_end_position(&clusters, 1), Some(10));
    assert_eq!(cluster_start_position(&clusters, 2), None);
  }

  #[test]
  fn test_cluster_range_advance() {
    let clusters = vec![
      cluster(0, 1, 0, 1, 10.0),
      cluster(1, 1, 1, 1, 20.0),
      cluster(2, 1, 2, 1, 15.0),
    ];

    assert_eq!(cluster_range_advance(&clusters, 0, 1), 10.0);
    assert_eq!(cluster_range_advance(&clusters, 0, 2), 30.0);
    assert_eq!(cluster_range_advance(&clusters, 0, 3), 45.0);
    assert_eq!(cluster_range_advance(&clusters, 1, 3), 35.0);
  }

  // ========================================================================
  // Cursor Positioning Tests
  // ========================================================================

  #[test]
  fn test_byte_offset_to_x_empty() {
    assert_eq!(byte_offset_to_x(&[], 0), 0.0);
  }

  #[test]
  fn test_byte_offset_to_x_simple() {
    let clusters = vec![
      cluster(0, 1, 0, 1, 10.0),
      cluster(1, 1, 1, 1, 10.0),
      cluster(2, 1, 2, 1, 10.0),
    ];

    assert_eq!(byte_offset_to_x(&clusters, 0), 0.0);
    assert_eq!(byte_offset_to_x(&clusters, 1), 10.0);
    assert_eq!(byte_offset_to_x(&clusters, 2), 20.0);
    assert_eq!(byte_offset_to_x(&clusters, 3), 30.0);
  }

  #[test]
  fn test_x_to_byte_offset_empty() {
    assert_eq!(x_to_byte_offset(&[], 0.0), 0);
  }

  #[test]
  fn test_x_to_byte_offset_simple() {
    let clusters = vec![
      cluster(0, 1, 0, 1, 10.0),
      cluster(1, 1, 1, 1, 10.0),
      cluster(2, 1, 2, 1, 10.0),
    ];

    // Before start
    assert_eq!(x_to_byte_offset(&clusters, -5.0), 0);

    // First half of first cluster
    assert_eq!(x_to_byte_offset(&clusters, 3.0), 0);

    // Second half of first cluster
    assert_eq!(x_to_byte_offset(&clusters, 7.0), 1);

    // Past end
    assert_eq!(x_to_byte_offset(&clusters, 100.0), 3);
  }

  #[test]
  fn test_x_byte_offset_with_multibyte_cluster() {
    // a😊b where the emoji is 4 bytes (byte boundaries: 0,1,5,6)
    let clusters = vec![
      cluster(0, 1, 0, 1, 10.0),
      cluster(1, 4, 1, 1, 10.0),
      cluster(5, 1, 2, 1, 10.0),
    ];

    // Midpoint of first cluster snaps to its end boundary
    assert_eq!(x_to_byte_offset(&clusters, 5.0), 1);
    // Midpoint of emoji cluster should return its start byte
    assert_eq!(x_to_byte_offset(&clusters, 12.0), 1);
    // Past midpoint of emoji cluster should return its end byte boundary
    assert_eq!(x_to_byte_offset(&clusters, 17.0), 5);

    // Verify byte_to_x uses cluster lengths correctly
    assert_eq!(byte_offset_to_x(&clusters, 0), 0.0);
    assert_eq!(byte_offset_to_x(&clusters, 1), 10.0);
    assert_eq!(byte_offset_to_x(&clusters, 5), 20.0);
    assert_eq!(byte_offset_to_x(&clusters, 6), 30.0);
  }

  // ========================================================================
  // Selection Tests
  // ========================================================================

  #[test]
  fn test_get_selection_basic() {
    let clusters = vec![
      cluster(0, 1, 0, 1, 10.0),
      cluster(1, 1, 1, 1, 10.0),
      cluster(2, 1, 2, 1, 10.0),
    ];

    let selection = get_selection(&clusters, 0, 2);

    assert_eq!(selection.start_byte, 0);
    assert_eq!(selection.end_byte, 2);
    assert_eq!(selection.byte_len(), 2);
    assert_eq!(selection.start_x, 0.0);
    assert_eq!(selection.end_x, 20.0);
    assert_eq!(selection.width(), 20.0);
  }

  #[test]
  fn test_get_selection_reversed() {
    let clusters = vec![cluster(0, 1, 0, 1, 10.0), cluster(1, 1, 1, 1, 10.0)];

    let selection = get_selection(&clusters, 2, 0);

    // Should normalize
    assert_eq!(selection.start_byte, 0);
    assert_eq!(selection.end_byte, 2);
  }

  // ========================================================================
  // Grapheme Counting Tests
  // ========================================================================

  #[test]
  fn test_count_chars_in_range_ascii() {
    assert_eq!(count_chars_in_range("Hello", 0, 5), 5);
    assert_eq!(count_chars_in_range("Hello", 1, 4), 3);
  }

  #[test]
  fn test_count_chars_in_range_unicode() {
    let text = "café";
    assert_eq!(count_chars_in_range(text, 0, text.len()), 4);
  }

  #[test]
  fn test_count_chars_in_range_empty() {
    assert_eq!(count_chars_in_range("", 0, 0), 0);
    assert_eq!(count_chars_in_range("Hello", 5, 5), 0);
    assert_eq!(count_chars_in_range("Hello", 10, 20), 0);
  }
}
