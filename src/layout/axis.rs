use crate::geometry::{Point, Rect};
use crate::style::types::{Direction, WritingMode};
use crate::style::{block_axis_is_horizontal, block_axis_positive, inline_axis_positive};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PhysicalAxis {
  X,
  Y,
}

/// Describes the physical axes used for fragmentation calculations.
///
/// Fragmentation operates in a monotonic block-axis coordinate system even when the physical
/// direction is reversed (e.g., vertical-rl). This helper converts between physical rectangles and
/// that monotonic space.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FragmentAxes {
  block_axis: PhysicalAxis,
  block_positive: bool,
  inline_positive: bool,
}

impl FragmentAxes {
  pub fn new(block_axis: PhysicalAxis, block_positive: bool, inline_positive: bool) -> Self {
    Self {
      block_axis,
      block_positive,
      inline_positive,
    }
  }

  pub fn from_writing_mode_and_direction(writing_mode: WritingMode, direction: Direction) -> Self {
    let block_axis = if block_axis_is_horizontal(writing_mode) {
      PhysicalAxis::X
    } else {
      PhysicalAxis::Y
    };
    let block_positive = block_axis_positive(writing_mode);
    let inline_positive = inline_axis_positive(writing_mode, direction);
    Self {
      block_axis,
      block_positive,
      inline_positive,
    }
  }

  pub fn block_axis(&self) -> PhysicalAxis {
    self.block_axis
  }

  pub fn inline_axis(&self) -> PhysicalAxis {
    match self.block_axis {
      PhysicalAxis::X => PhysicalAxis::Y,
      PhysicalAxis::Y => PhysicalAxis::X,
    }
  }

  pub fn block_positive(&self) -> bool {
    self.block_positive
  }

  pub fn inline_positive(&self) -> bool {
    self.inline_positive
  }

  pub fn block_size(&self, rect: &Rect) -> f32 {
    match self.block_axis {
      PhysicalAxis::X => rect.width(),
      PhysicalAxis::Y => rect.height(),
    }
  }

  pub fn inline_size(&self, rect: &Rect) -> f32 {
    match self.block_axis {
      PhysicalAxis::X => rect.height(),
      PhysicalAxis::Y => rect.width(),
    }
  }

  /// Computes the block-start of a rect relative to the absolute block-start of its parent.
  /// `parent_block_size` is required for reversed block progression (e.g., vertical-rl).
  pub fn abs_block_start(&self, rect: &Rect, parent_abs_start: f32, parent_block_size: f32) -> f32 {
    parent_abs_start + self.block_start(rect, parent_block_size)
  }

  pub fn abs_block_end(&self, rect: &Rect, parent_abs_start: f32, parent_block_size: f32) -> f32 {
    self.abs_block_start(rect, parent_abs_start, parent_block_size) + self.block_size(rect)
  }

  /// Returns the block-start position of a rect in monotonic block coordinates relative to its
  /// parent. `parent_block_size` is only used when block progression is reversed.
  pub fn block_start(&self, rect: &Rect, parent_block_size: f32) -> f32 {
    match self.block_axis {
      PhysicalAxis::Y => {
        if self.block_positive {
          rect.y()
        } else {
          parent_block_size - (rect.y() + rect.height())
        }
      }
      PhysicalAxis::X => {
        if self.block_positive {
          rect.x()
        } else {
          parent_block_size - (rect.x() + rect.width())
        }
      }
    }
  }

  pub fn block_end(&self, rect: &Rect, parent_block_size: f32) -> f32 {
    self.block_start(rect, parent_block_size) + self.block_size(rect)
  }

  /// Sets the block-start and block-size of a rect while preserving the inline-axis geometry.
  pub fn set_block_start_and_size(
    &self,
    rect: Rect,
    parent_block_size: f32,
    block_start: f32,
    block_size: f32,
  ) -> Rect {
    match self.block_axis {
      PhysicalAxis::Y => {
        let y = if self.block_positive {
          block_start
        } else {
          parent_block_size - block_start - block_size
        };
        Rect::from_xywh(rect.x(), y, rect.width(), block_size)
      }
      PhysicalAxis::X => {
        let x = if self.block_positive {
          block_start
        } else {
          parent_block_size - block_start - block_size
        };
        Rect::from_xywh(x, rect.y(), block_size, rect.height())
      }
    }
  }

  pub fn translate_block(&self, rect: Rect, delta: f32) -> Rect {
    rect.translate(self.block_offset(delta))
  }

  pub fn block_offset(&self, delta: f32) -> Point {
    match self.block_axis {
      PhysicalAxis::Y => Point::new(0.0, if self.block_positive { delta } else { -delta }),
      PhysicalAxis::X => Point::new(if self.block_positive { delta } else { -delta }, 0.0),
    }
  }

  pub fn inline_offset(&self, delta: f32) -> Point {
    match self.inline_axis() {
      PhysicalAxis::Y => Point::new(0.0, if self.inline_positive { delta } else { -delta }),
      PhysicalAxis::X => Point::new(if self.inline_positive { delta } else { -delta }, 0.0),
    }
  }
}

impl Default for FragmentAxes {
  fn default() -> Self {
    Self::new(PhysicalAxis::Y, true, true)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn computes_block_start_positive_y() {
    let axes = FragmentAxes::default();
    let rect = Rect::from_xywh(10.0, 20.0, 30.0, 40.0);
    assert_eq!(axes.block_start(&rect, 0.0), 20.0);
    assert_eq!(axes.block_end(&rect, 0.0), 60.0);
  }

  #[test]
  fn computes_block_start_reversed_x() {
    let axes = FragmentAxes::new(PhysicalAxis::X, false, true);
    let parent_size = 200.0;
    let rect = Rect::from_xywh(50.0, 5.0, 40.0, 10.0);
    assert!((axes.block_start(&rect, parent_size) - 110.0).abs() < 0.001);

    let resized = axes.set_block_start_and_size(rect, parent_size, 60.0, 30.0);
    assert!((axes.block_start(&resized, parent_size) - 60.0).abs() < 0.001);
    assert!((resized.x() - (parent_size - 60.0 - 30.0)).abs() < 0.001);
  }

  #[test]
  fn inline_and_block_offsets_follow_direction() {
    let axes = FragmentAxes::new(PhysicalAxis::X, false, false);
    assert_eq!(axes.block_offset(10.0), Point::new(-10.0, 0.0));
    assert_eq!(axes.inline_offset(5.0), Point::new(0.0, -5.0));
  }
}
