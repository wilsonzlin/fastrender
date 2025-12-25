//! Filter outset computation helpers.
//!
//! These utilities estimate how far a chain of CSS filters can extend content
//! beyond its original bounds. The calculations are intentionally conservative
//! to avoid culling or clipping visible pixels when filters are applied.

use crate::paint::display_list::ResolvedFilter;

/// Outset distances on each side of a rectangle.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FilterOutset {
  pub left: f32,
  pub top: f32,
  pub right: f32,
  pub bottom: f32,
}

impl FilterOutset {
  pub const ZERO: Self = Self {
    left: 0.0,
    top: 0.0,
    right: 0.0,
    bottom: 0.0,
  };

  pub fn max_side(&self) -> f32 {
    self.left.max(self.top).max(self.right).max(self.bottom)
  }

  pub fn as_tuple(self) -> (f32, f32, f32, f32) {
    (self.left, self.top, self.right, self.bottom)
  }
}

/// Compute a conservative outset for a chain of resolved filters.
///
/// The calculation accumulates blurs across the chain (radius * 3 per blur) and
/// unions drop shadows with the current bounds including existing outsets. All
/// other filters are treated as in-bounds operations.
pub fn filter_outset(filters: &[ResolvedFilter], scale: f32) -> FilterOutset {
  let mut outset = FilterOutset::ZERO;

  for filter in filters {
    match filter {
      ResolvedFilter::Blur(radius) => {
        let delta = (*radius * scale).abs() * 3.0;
        outset.left += delta;
        outset.top += delta;
        outset.right += delta;
        outset.bottom += delta;
      }
      ResolvedFilter::DropShadow {
        offset_x,
        offset_y,
        blur_radius,
        spread,
        ..
      } => {
        let dx = offset_x * scale;
        let dy = offset_y * scale;
        let blur = blur_radius * scale;
        let spread = spread * scale;
        let delta = (blur.abs() * 3.0 + spread).max(0.0);

        // Union the existing bounds with the new shadow bounds, taking the
        // current outset as the base rectangle.
        let shadow_left = outset.left + delta - dx;
        let shadow_right = outset.right + delta + dx;
        let shadow_top = outset.top + delta - dy;
        let shadow_bottom = outset.bottom + delta + dy;

        outset.left = outset.left.max(shadow_left);
        outset.top = outset.top.max(shadow_top);
        outset.right = outset.right.max(shadow_right);
        outset.bottom = outset.bottom.max(shadow_bottom);
      }
      ResolvedFilter::SvgFilter(_) => {
        // Unknown geometry; treated as in-bounds until we can compute filter
        // regions. Keeping this conservative avoids oversizing render surfaces.
      }
      _ => {}
    }
  }

  FilterOutset {
    left: outset.left.max(0.0),
    top: outset.top.max(0.0),
    right: outset.right.max(0.0),
    bottom: outset.bottom.max(0.0),
  }
}
