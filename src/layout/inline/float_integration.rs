//! Float Integration for Inline Layout
//!
//! This module integrates the FloatContext with inline layout, enabling text and
//! inline elements to wrap around floated boxes per CSS 2.1 Section 9.5.
//!
//! # Overview
//!
//! When inline content is laid out in a block formatting context (BFC) that contains
//! floats, the inline content must flow around those floats. This module provides
//! the integration layer between the FloatContext and inline layout algorithms.
//!
//! # CSS Specification References
//!
//! - CSS 2.1 Section 9.4.2: Inline formatting contexts
//! - CSS 2.1 Section 9.5: Floats
//! - CSS 2.1 Section 9.5.1: Positioning the float
//!
//! # Key Concepts
//!
//! ## Line Box Shortening
//!
//! Line boxes next to floats are shortened to make room for the float's margin box.
//! If the remaining space is insufficient for content, the line box shifts down
//! until it finds sufficient space or clears past all floats.
//!
//! ## Float-Aware Line Breaking
//!
//! Line breaking must consider that available width varies at different Y positions.
//! A line that starts at one Y position may have different width constraints than
//! a line starting at another Y position.
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::{FloatContext, FloatSide};
//! use fastrender::layout::inline::{InlineFloatIntegration, LineSpace};
//!
//! // Create a float context with some floats
//! let mut float_ctx = FloatContext::new(800.0);
//! float_ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);
//!
//! // Create the integration helper
//! let integration = InlineFloatIntegration::new(&float_ctx);
//!
//! // Find space for a line at y=50
//! let space = integration.find_line_space(50.0, 20.0);
//! assert_eq!(space.left_edge, 200.0);
//! assert_eq!(space.width, 600.0);
//! ```

use crate::geometry::Rect;
use crate::layout::float_context::FloatContext;
use crate::layout::float_context::FloatSide;
use crate::style::float::Clear;

/// Describes the available space for a line box
///
/// This represents the horizontal space available for inline content at a
/// specific vertical position, accounting for floats on both sides.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LineSpace {
  /// The Y position where this line space applies
  pub y: f32,

  /// The left edge where content can begin (x-coordinate)
  pub left_edge: f32,

  /// The available width for content
  pub width: f32,

  /// The right edge where content must end (x-coordinate)
  pub right_edge: f32,
}

impl LineSpace {
  /// Creates a new LineSpace with the given parameters
  pub fn new(y: f32, left_edge: f32, width: f32) -> Self {
    Self {
      y,
      left_edge,
      width,
      right_edge: left_edge + width,
    }
  }

  /// Creates a LineSpace with full width (no float interference)
  pub fn full_width(y: f32, containing_width: f32) -> Self {
    Self {
      y,
      left_edge: 0.0,
      width: containing_width,
      right_edge: containing_width,
    }
  }

  /// Returns true if there's any width available
  pub fn has_space(&self) -> bool {
    self.width > 0.0
  }

  /// Returns true if the given width would fit in this space
  pub fn fits(&self, required_width: f32) -> bool {
    self.width >= required_width
  }
}

/// Result of placing an inline float
///
/// When an inline-level float is encountered during inline layout, it must be
/// placed according to float positioning rules. This struct contains the
/// computed position and relevant information.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PlacedInlineFloat {
  /// The computed position for the float
  pub rect: Rect,

  /// Which side the float is placed on
  pub side: FloatSide,

  /// The Y position after placing this float (for the next line to start)
  pub next_line_y: f32,
}

/// Options for finding line space
#[derive(Debug, Clone, Copy)]
pub struct LineSpaceOptions {
  /// Minimum width required for the line
  pub min_width: f32,

  /// Height of the line box (for multi-line elements)
  pub line_height: f32,

  /// Whether to allow zero-width results
  pub allow_zero_width: bool,
}

impl Default for LineSpaceOptions {
  fn default() -> Self {
    Self {
      min_width: 0.0,
      line_height: 0.0,
      allow_zero_width: true,
    }
  }
}

impl LineSpaceOptions {
  /// Creates options with a minimum width requirement
  pub fn with_min_width(min_width: f32) -> Self {
    Self {
      min_width,
      ..Default::default()
    }
  }

  /// Creates options with a specific line height
  pub fn with_line_height(line_height: f32) -> Self {
    Self {
      line_height,
      ..Default::default()
    }
  }

  /// Sets the minimum width requirement
  pub fn min_width(mut self, min_width: f32) -> Self {
    self.min_width = min_width;
    self
  }

  /// Sets the line height
  pub fn line_height(mut self, line_height: f32) -> Self {
    self.line_height = line_height;
    self
  }
}

/// Integration helper for inline layout with floats
///
/// This struct provides methods to query float positions and compute
/// available space for inline content. It wraps a reference to a FloatContext
/// and provides inline-specific queries.
///
/// # Usage
///
/// This is typically used by an inline formatting context implementation
/// when breaking text into lines and positioning inline boxes.
#[derive(Debug)]
pub struct InlineFloatIntegration<'a> {
  /// Reference to the float context
  float_ctx: &'a FloatContext,
}

impl<'a> InlineFloatIntegration<'a> {
  /// Creates a new InlineFloatIntegration from a FloatContext reference
  pub fn new(float_ctx: &'a FloatContext) -> Self {
    Self { float_ctx }
  }

  /// Returns the containing block width
  pub fn containing_width(&self) -> f32 {
    self.float_ctx.containing_block_width()
  }

  /// Returns true if there are any floats that affect layout
  pub fn has_floats(&self) -> bool {
    !self.float_ctx.is_empty()
  }

  /// Gets the available line space at a specific Y position
  ///
  /// This is the primary method for determining where inline content can be
  /// placed on a line. It returns the horizontal bounds available for content
  /// at the given vertical position.
  ///
  /// # Arguments
  ///
  /// * `y` - The Y position to query
  ///
  /// # Returns
  ///
  /// A `LineSpace` describing the available horizontal space.
  ///
  /// # Example
  ///
  /// ```rust,ignore
  /// let space = integration.get_line_space(50.0);
  /// // Place content starting at space.left_edge
  /// // Content can be at most space.width pixels wide
  /// ```
  pub fn get_line_space(&self, y: f32) -> LineSpace {
    let (left_edge, width) = self.float_ctx.available_width_at_y(y);
    LineSpace::new(y, left_edge, width)
  }

  /// Gets the available line space over a vertical range
  ///
  /// For multi-line elements or elements with specified height, we need to
  /// know the minimum available space over the entire height of the element.
  ///
  /// # Arguments
  ///
  /// * `y_start` - Start of the vertical range (top)
  /// * `y_end` - End of the vertical range (bottom)
  ///
  /// # Returns
  ///
  /// A `LineSpace` with the most constrained values over the range.
  pub fn get_line_space_in_range(&self, y_start: f32, y_end: f32) -> LineSpace {
    let (left_edge, width) = self.float_ctx.available_width_in_range(y_start, y_end);
    LineSpace::new(y_start, left_edge, width)
  }

  /// Finds space for a line with minimum width requirements
  ///
  /// If the available space at the given Y position is insufficient, this
  /// method will search downward until it finds space or determines none exists.
  ///
  /// # Arguments
  ///
  /// * `start_y` - The Y position to start searching from
  /// * `options` - Options specifying requirements for the line space
  ///
  /// # Returns
  ///
  /// A `LineSpace` where the line can be placed. The Y may be greater than
  /// `start_y` if the line needed to be pushed down past floats.
  pub fn find_line_space(&self, start_y: f32, options: LineSpaceOptions) -> LineSpace {
    if !self.has_floats() {
      // No floats - full width available at start_y
      return LineSpace::full_width(start_y, self.containing_width());
    }

    let y = if options.line_height > 0.0 {
      self
        .float_ctx
        .find_fit(options.min_width, options.line_height, start_y)
    } else {
      // For zero-height lines, just check if width fits
      let mut y = start_y;
      loop {
        let (left_edge, width) = self.float_ctx.available_width_at_y(y);
        if width >= options.min_width || !options.allow_zero_width {
          return LineSpace::new(y, left_edge, width);
        }

        // Find the next position where width might change
        let next_y = self.float_ctx.next_float_boundary_after(y);
        if next_y <= y {
          // No more float boundaries, return current position
          return LineSpace::new(y, left_edge, width);
        }
        y = next_y;
      }
    };

    let (left_edge, width) = if options.line_height > 0.0 {
      self
        .float_ctx
        .available_width_in_range(y, y + options.line_height)
    } else {
      self.float_ctx.available_width_at_y(y)
    };

    LineSpace::new(y, left_edge, width)
  }

  /// Computes the position to place a line, handling clearance
  ///
  /// This method accounts for both float interference and clearance
  /// requirements when positioning a line.
  ///
  /// # Arguments
  ///
  /// * `start_y` - The Y position before clearance
  /// * `clear` - The clear property value
  /// * `options` - Line space requirements
  ///
  /// # Returns
  ///
  /// A `LineSpace` at the appropriate position after clearing.
  pub fn find_line_space_with_clear(
    &self,
    start_y: f32,
    clear: Clear,
    options: LineSpaceOptions,
  ) -> LineSpace {
    // First apply clearance
    let cleared_y = self.float_ctx.compute_clearance(start_y, clear);

    // Then find space at the cleared position
    self.find_line_space(cleared_y, options)
  }

  /// Gets the left edge at a specific Y position
  ///
  /// Returns the X coordinate where content can start.
  pub fn left_edge_at(&self, y: f32) -> f32 {
    self.float_ctx.left_edge_at_y(y)
  }

  /// Gets the right edge at a specific Y position
  ///
  /// Returns the X coordinate where content must end.
  pub fn right_edge_at(&self, y: f32) -> f32 {
    self.float_ctx.right_edge_at_y(y)
  }

  /// Returns the Y position where all floats end
  ///
  /// This is useful for knowing when content no longer needs to wrap around floats.
  pub fn floats_bottom(&self) -> f32 {
    self.float_ctx.floats_bottom()
  }

  /// Checks if a line would fit at the given position
  ///
  /// # Arguments
  ///
  /// * `y` - The Y position to check
  /// * `width` - Required width
  /// * `height` - Line height
  ///
  /// # Returns
  ///
  /// True if the line would fit without overlapping floats.
  pub fn line_fits_at(&self, y: f32, width: f32, height: f32) -> bool {
    self.float_ctx.fits_at(y, width, height)
  }
}

/// Mutable integration helper for inline layout with floats
///
/// This struct provides methods to both query and modify the float context
/// during inline layout. It's used when inline-level floats need to be
/// placed during the inline layout process.
#[derive(Debug)]
pub struct InlineFloatIntegrationMut<'a> {
  /// Mutable reference to the float context
  float_ctx: &'a mut FloatContext,
}

impl<'a> InlineFloatIntegrationMut<'a> {
  /// Creates a new InlineFloatIntegrationMut from a mutable FloatContext reference
  pub fn new(float_ctx: &'a mut FloatContext) -> Self {
    Self { float_ctx }
  }

  /// Returns the containing block width
  pub fn containing_width(&self) -> f32 {
    self.float_ctx.containing_block_width()
  }

  /// Returns true if there are any floats
  pub fn has_floats(&self) -> bool {
    !self.float_ctx.is_empty()
  }

  /// Gets the available line space at a specific Y position
  pub fn get_line_space(&self, y: f32) -> LineSpace {
    let (left_edge, width) = self.float_ctx.available_width_at_y(y);
    LineSpace::new(y, left_edge, width)
  }

  /// Gets the available line space over a vertical range
  pub fn get_line_space_in_range(&self, y_start: f32, y_end: f32) -> LineSpace {
    let (left_edge, width) = self.float_ctx.available_width_in_range(y_start, y_end);
    LineSpace::new(y_start, left_edge, width)
  }

  /// Finds space for a line with minimum width requirements
  pub fn find_line_space(&self, start_y: f32, options: LineSpaceOptions) -> LineSpace {
    if !self.has_floats() {
      return LineSpace::full_width(start_y, self.containing_width());
    }

    let y = if options.line_height > 0.0 {
      self
        .float_ctx
        .find_fit(options.min_width, options.line_height, start_y)
    } else {
      let mut y = start_y;
      loop {
        let (left_edge, width) = self.float_ctx.available_width_at_y(y);
        if width >= options.min_width {
          return LineSpace::new(y, left_edge, width);
        }

        let next_y = self.float_ctx.next_float_boundary_after(y);
        if next_y <= y {
          return LineSpace::new(y, left_edge, width);
        }
        y = next_y;
      }
    };

    let (left_edge, width) = if options.line_height > 0.0 {
      self
        .float_ctx
        .available_width_in_range(y, y + options.line_height)
    } else {
      self.float_ctx.available_width_at_y(y)
    };

    LineSpace::new(y, left_edge, width)
  }

  /// Computes the position to place a line, handling clearance
  pub fn find_line_space_with_clear(
    &self,
    start_y: f32,
    clear: Clear,
    options: LineSpaceOptions,
  ) -> LineSpace {
    let cleared_y = self.float_ctx.compute_clearance(start_y, clear);
    self.find_line_space(cleared_y, options)
  }

  /// Gets the left edge at a specific Y position
  pub fn left_edge_at(&self, y: f32) -> f32 {
    self.float_ctx.left_edge_at_y(y)
  }

  /// Gets the right edge at a specific Y position
  pub fn right_edge_at(&self, y: f32) -> f32 {
    self.float_ctx.right_edge_at_y(y)
  }

  /// Returns the Y position where all floats end
  pub fn floats_bottom(&self) -> f32 {
    self.float_ctx.floats_bottom()
  }

  /// Places an inline-level float
  ///
  /// When a floated element is encountered within inline content, it must be
  /// placed according to float positioning rules. This method computes the
  /// position and adds the float to the context.
  ///
  /// # Arguments
  ///
  /// * `side` - Which side to place the float (left or right)
  /// * `width` - Width of the float's margin box
  /// * `height` - Height of the float's margin box
  /// * `current_line_y` - The Y position of the current line being laid out
  ///
  /// # Returns
  ///
  /// Information about where the float was placed.
  ///
  /// # CSS Behavior
  ///
  /// Per CSS 2.1 Section 9.5.1, a float's top may not be higher than the
  /// current line box. The float is placed as high as possible, but no
  /// higher than the current line.
  pub fn place_inline_float(
    &mut self,
    side: FloatSide,
    width: f32,
    height: f32,
    current_line_y: f32,
  ) -> PlacedInlineFloat {
    // Float's top must not be higher than current line
    let min_y = current_line_y;

    // Compute position using float context algorithm
    let (x, y) = self
      .float_ctx
      .compute_float_position(side, width, height, min_y);

    // Add the float to the context
    self.float_ctx.add_float_at(side, x, y, width, height);

    // The rect for the placed float
    let rect = Rect::from_xywh(x, y, width, height);

    // Determine where the next line should start
    // If the float was placed at the current line, the next line might need
    // to account for the float. If pushed down, next line starts there.
    let next_line_y = current_line_y;

    PlacedInlineFloat {
      rect,
      side,
      next_line_y,
    }
  }

  /// Computes clearance needed at a position
  ///
  /// Returns the Y position after applying clearance.
  pub fn compute_clearance(&self, y: f32, clear: Clear) -> f32 {
    self.float_ctx.compute_clearance(y, clear)
  }

  /// Gets the clearance amount as a delta
  ///
  /// Returns how much Y needs to increase to clear floats.
  pub fn clearance_amount(&self, y: f32, clear: Clear) -> f32 {
    self.float_ctx.clearance_amount(y, clear)
  }

  /// Updates the current Y position in the float context
  pub fn set_current_y(&mut self, y: f32) {
    self.float_ctx.set_current_y(y);
  }

  /// Gets the current Y position from the float context
  pub fn current_y(&self) -> f32 {
    self.float_ctx.current_y()
  }
}

/// Iterator over line spaces, yielding positions where available width changes
///
/// This iterator is useful when laying out content that spans multiple
/// vertical positions and needs to know where the available width changes.
#[derive(Debug)]
pub struct LineSpaceIterator<'a> {
  float_ctx: &'a FloatContext,
  current_y: f32,
  end_y: f32,
  /// Reserved for future use in width calculations
  #[allow(dead_code)]
  containing_width: f32,
}

impl<'a> LineSpaceIterator<'a> {
  /// Creates a new iterator starting at the given Y position
  pub fn new(float_ctx: &'a FloatContext, start_y: f32, end_y: f32) -> Self {
    Self {
      float_ctx,
      current_y: start_y,
      end_y,
      containing_width: float_ctx.containing_block_width(),
    }
  }
}

impl<'a> Iterator for LineSpaceIterator<'a> {
  type Item = LineSpace;

  fn next(&mut self) -> Option<Self::Item> {
    if self.current_y >= self.end_y {
      return None;
    }

    let (left_edge, width) = self.float_ctx.available_width_at_y(self.current_y);
    let space = LineSpace::new(self.current_y, left_edge, width);

    // Find the next position where width might change
    let next_y = self
      .float_ctx
      .next_float_boundary_after(self.current_y)
      .min(self.end_y);
    self.current_y = if next_y <= self.current_y {
      self.end_y
    } else {
      next_y
    };

    Some(space)
  }
}

/// Creates an iterator over line spaces within a Y range
pub fn line_spaces<'a>(
  float_ctx: &'a FloatContext,
  start_y: f32,
  end_y: f32,
) -> LineSpaceIterator<'a> {
  LineSpaceIterator::new(float_ctx, start_y, end_y)
}

#[cfg(test)]
mod tests {
  use super::*;

  // ==================== LineSpace Tests ====================

  #[test]
  fn test_line_space_new() {
    let space = LineSpace::new(10.0, 20.0, 100.0);
    assert_eq!(space.y, 10.0);
    assert_eq!(space.left_edge, 20.0);
    assert_eq!(space.width, 100.0);
    assert_eq!(space.right_edge, 120.0);
  }

  #[test]
  fn test_line_space_full_width() {
    let space = LineSpace::full_width(50.0, 800.0);
    assert_eq!(space.y, 50.0);
    assert_eq!(space.left_edge, 0.0);
    assert_eq!(space.width, 800.0);
    assert_eq!(space.right_edge, 800.0);
  }

  #[test]
  fn test_line_space_has_space() {
    let space_with = LineSpace::new(0.0, 0.0, 100.0);
    let space_without = LineSpace::new(0.0, 0.0, 0.0);

    assert!(space_with.has_space());
    assert!(!space_without.has_space());
  }

  #[test]
  fn test_line_space_fits() {
    let space = LineSpace::new(0.0, 0.0, 100.0);

    assert!(space.fits(50.0));
    assert!(space.fits(100.0));
    assert!(!space.fits(150.0));
  }

  // ==================== LineSpaceOptions Tests ====================

  #[test]
  fn test_line_space_options_default() {
    let opts = LineSpaceOptions::default();
    assert_eq!(opts.min_width, 0.0);
    assert_eq!(opts.line_height, 0.0);
    assert!(opts.allow_zero_width);
  }

  #[test]
  fn test_line_space_options_with_min_width() {
    let opts = LineSpaceOptions::with_min_width(50.0);
    assert_eq!(opts.min_width, 50.0);
  }

  #[test]
  fn test_line_space_options_builder() {
    let opts = LineSpaceOptions::default()
      .min_width(100.0)
      .line_height(20.0);
    assert_eq!(opts.min_width, 100.0);
    assert_eq!(opts.line_height, 20.0);
  }

  // ==================== InlineFloatIntegration Tests ====================

  #[test]
  fn test_integration_no_floats() {
    let ctx = FloatContext::new(800.0);
    let integration = InlineFloatIntegration::new(&ctx);

    assert_eq!(integration.containing_width(), 800.0);
    assert!(!integration.has_floats());

    let space = integration.get_line_space(0.0);
    assert_eq!(space.left_edge, 0.0);
    assert_eq!(space.width, 800.0);
  }

  #[test]
  fn test_integration_with_left_float() {
    let mut ctx = FloatContext::new(800.0);
    ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

    let integration = InlineFloatIntegration::new(&ctx);
    assert!(integration.has_floats());

    // Within float range
    let space = integration.get_line_space(50.0);
    assert_eq!(space.left_edge, 200.0);
    assert_eq!(space.width, 600.0);

    // Below float range
    let space = integration.get_line_space(150.0);
    assert_eq!(space.left_edge, 0.0);
    assert_eq!(space.width, 800.0);
  }

  #[test]
  fn test_integration_with_right_float() {
    let mut ctx = FloatContext::new(800.0);
    ctx.add_float_at(FloatSide::Right, 600.0, 0.0, 200.0, 100.0);

    let integration = InlineFloatIntegration::new(&ctx);

    let space = integration.get_line_space(50.0);
    assert_eq!(space.left_edge, 0.0);
    assert_eq!(space.width, 600.0);
    assert_eq!(space.right_edge, 600.0);
  }

  #[test]
  fn test_integration_with_both_floats() {
    let mut ctx = FloatContext::new(800.0);
    ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);
    ctx.add_float_at(FloatSide::Right, 600.0, 0.0, 200.0, 100.0);

    let integration = InlineFloatIntegration::new(&ctx);

    let space = integration.get_line_space(50.0);
    assert_eq!(space.left_edge, 200.0);
    assert_eq!(space.width, 400.0);
    assert_eq!(space.right_edge, 600.0);
  }

  #[test]
  fn test_integration_line_space_in_range() {
    let mut ctx = FloatContext::new(800.0);
    ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);
    ctx.add_float_at(FloatSide::Left, 0.0, 50.0, 300.0, 100.0);

    let integration = InlineFloatIntegration::new(&ctx);

    // Range that spans both floats
    let space = integration.get_line_space_in_range(0.0, 100.0);
    assert_eq!(space.left_edge, 300.0); // Most constrained
    assert_eq!(space.width, 500.0);
  }

  #[test]
  fn test_integration_find_line_space() {
    let mut ctx = FloatContext::new(800.0);
    ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 600.0, 100.0);

    let integration = InlineFloatIntegration::new(&ctx);

    // Looking for 300 width, which doesn't fit at y=0 (only 200 available)
    let opts = LineSpaceOptions::with_min_width(300.0).line_height(20.0);
    let space = integration.find_line_space(0.0, opts);

    // Should be pushed down to y=100 where float ends
    assert_eq!(space.y, 100.0);
    assert_eq!(space.width, 800.0);
  }

  #[test]
  fn test_integration_find_line_space_with_clear() {
    let mut ctx = FloatContext::new(800.0);
    ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

    let integration = InlineFloatIntegration::new(&ctx);

    let opts = LineSpaceOptions::default();
    let space = integration.find_line_space_with_clear(50.0, Clear::Left, opts);

    // Should be pushed below the float
    assert_eq!(space.y, 100.0);
  }

  #[test]
  fn test_integration_edges() {
    let mut ctx = FloatContext::new(800.0);
    ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);
    ctx.add_float_at(FloatSide::Right, 600.0, 0.0, 200.0, 100.0);

    let integration = InlineFloatIntegration::new(&ctx);

    assert_eq!(integration.left_edge_at(50.0), 200.0);
    assert_eq!(integration.right_edge_at(50.0), 600.0);
    assert_eq!(integration.left_edge_at(150.0), 0.0);
    assert_eq!(integration.right_edge_at(150.0), 800.0);
  }

  #[test]
  fn test_integration_floats_bottom() {
    let mut ctx = FloatContext::new(800.0);
    ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);
    ctx.add_float_at(FloatSide::Right, 600.0, 50.0, 200.0, 150.0);

    let integration = InlineFloatIntegration::new(&ctx);

    assert_eq!(integration.floats_bottom(), 200.0); // 50 + 150
  }

  #[test]
  fn test_integration_line_fits_at() {
    let mut ctx = FloatContext::new(800.0);
    ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 600.0, 100.0);

    let integration = InlineFloatIntegration::new(&ctx);

    // Only 200px available at y=0
    assert!(integration.line_fits_at(0.0, 100.0, 20.0));
    assert!(!integration.line_fits_at(0.0, 300.0, 20.0));

    // Full width available at y=100
    assert!(integration.line_fits_at(100.0, 700.0, 20.0));
  }

  // ==================== InlineFloatIntegrationMut Tests ====================

  #[test]
  fn test_integration_mut_place_inline_float() {
    let mut ctx = FloatContext::new(800.0);
    let mut integration = InlineFloatIntegrationMut::new(&mut ctx);

    // Place a left float at current line y=50
    let result = integration.place_inline_float(FloatSide::Left, 200.0, 100.0, 50.0);

    assert_eq!(result.rect.x(), 0.0);
    assert_eq!(result.rect.y(), 50.0);
    assert_eq!(result.rect.width(), 200.0);
    assert_eq!(result.rect.height(), 100.0);
    assert_eq!(result.side, FloatSide::Left);

    // Verify float was added
    assert!(integration.has_floats());
    let space = integration.get_line_space(75.0);
    assert_eq!(space.left_edge, 200.0);
  }

  #[test]
  fn test_integration_mut_place_multiple_floats() {
    let mut ctx = FloatContext::new(800.0);
    let mut integration = InlineFloatIntegrationMut::new(&mut ctx);

    // Place first left float
    integration.place_inline_float(FloatSide::Left, 200.0, 100.0, 0.0);

    // Place second left float - should stack horizontally
    let result = integration.place_inline_float(FloatSide::Left, 200.0, 100.0, 0.0);
    assert_eq!(result.rect.x(), 200.0);

    // Place right float
    let result = integration.place_inline_float(FloatSide::Right, 200.0, 100.0, 0.0);
    assert_eq!(result.rect.x(), 600.0);
  }

  #[test]
  fn test_integration_mut_clearance() {
    let mut ctx = FloatContext::new(800.0);
    let mut integration = InlineFloatIntegrationMut::new(&mut ctx);

    // Place a left float
    integration.place_inline_float(FloatSide::Left, 200.0, 100.0, 0.0);

    // Check clearance
    assert_eq!(integration.compute_clearance(50.0, Clear::Left), 100.0);
    assert_eq!(integration.compute_clearance(50.0, Clear::Right), 50.0);
    assert_eq!(integration.clearance_amount(50.0, Clear::Left), 50.0);
  }

  #[test]
  fn test_integration_mut_current_y() {
    let mut ctx = FloatContext::new(800.0);
    let mut integration = InlineFloatIntegrationMut::new(&mut ctx);

    assert_eq!(integration.current_y(), 0.0);
    integration.set_current_y(50.0);
    assert_eq!(integration.current_y(), 50.0);
  }

  // ==================== LineSpaceIterator Tests ====================

  #[test]
  fn test_line_space_iterator_no_floats() {
    let ctx = FloatContext::new(800.0);
    let mut iter = line_spaces(&ctx, 0.0, 100.0);

    let space = iter.next().unwrap();
    assert_eq!(space.y, 0.0);
    assert_eq!(space.width, 800.0);

    assert!(iter.next().is_none());
  }

  #[test]
  fn test_line_space_iterator_with_float() {
    let mut ctx = FloatContext::new(800.0);
    ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);

    let spaces: Vec<_> = line_spaces(&ctx, 0.0, 200.0).collect();

    assert_eq!(spaces.len(), 2);

    // First space: within float
    assert_eq!(spaces[0].y, 0.0);
    assert_eq!(spaces[0].left_edge, 200.0);
    assert_eq!(spaces[0].width, 600.0);

    // Second space: after float
    assert_eq!(spaces[1].y, 100.0);
    assert_eq!(spaces[1].left_edge, 0.0);
    assert_eq!(spaces[1].width, 800.0);
  }

  #[test]
  fn test_line_space_iterator_staggered_floats() {
    let mut ctx = FloatContext::new(800.0);
    ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 200.0, 100.0);
    ctx.add_float_at(FloatSide::Right, 600.0, 50.0, 200.0, 100.0);

    let spaces: Vec<_> = line_spaces(&ctx, 0.0, 200.0).collect();

    // Should have boundaries at 0, 50, 100, 150
    assert_eq!(spaces.len(), 4);

    // 0-100: left float only
    assert_eq!(spaces[0].y, 0.0);
    // 50-100: both floats
    assert_eq!(spaces[1].y, 50.0);
    // 100-150: right float only (left float ended)
    assert_eq!(spaces[2].y, 100.0);
    // 150-200: no floats
    assert_eq!(spaces[3].y, 150.0);
  }

  // ==================== Complex Scenario Tests ====================

  #[test]
  fn test_narrow_passage_between_floats() {
    let mut ctx = FloatContext::new(800.0);
    // Create a narrow passage between floats
    ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 350.0, 100.0);
    ctx.add_float_at(FloatSide::Right, 450.0, 0.0, 350.0, 100.0);

    let integration = InlineFloatIntegration::new(&ctx);

    let space = integration.get_line_space(50.0);
    assert_eq!(space.left_edge, 350.0);
    assert_eq!(space.width, 100.0); // Narrow passage
  }

  #[test]
  fn test_no_space_between_floats() {
    let mut ctx = FloatContext::new(800.0);
    // Floats completely fill the width
    ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 400.0, 100.0);
    ctx.add_float_at(FloatSide::Right, 400.0, 0.0, 400.0, 100.0);

    let integration = InlineFloatIntegration::new(&ctx);

    let space = integration.get_line_space(50.0);
    assert_eq!(space.width, 0.0);
    assert!(!space.has_space());

    // Find space should push down
    let opts = LineSpaceOptions::with_min_width(100.0).line_height(20.0);
    let space = integration.find_line_space(0.0, opts);
    assert_eq!(space.y, 100.0);
  }

  #[test]
  fn test_multiple_stacked_floats() {
    let mut ctx = FloatContext::new(800.0);
    // Stack of left floats, each progressively narrower
    ctx.add_float_at(FloatSide::Left, 0.0, 0.0, 300.0, 50.0);
    ctx.add_float_at(FloatSide::Left, 0.0, 50.0, 200.0, 50.0);
    ctx.add_float_at(FloatSide::Left, 0.0, 100.0, 100.0, 50.0);

    let integration = InlineFloatIntegration::new(&ctx);

    // At y=25, left edge is 300
    let space = integration.get_line_space(25.0);
    assert_eq!(space.left_edge, 300.0);

    // At y=75, left edge is 200
    let space = integration.get_line_space(75.0);
    assert_eq!(space.left_edge, 200.0);

    // At y=125, left edge is 100
    let space = integration.get_line_space(125.0);
    assert_eq!(space.left_edge, 100.0);

    // At y=175, no floats
    let space = integration.get_line_space(175.0);
    assert_eq!(space.left_edge, 0.0);
  }
}
