//! Filter outset computation helpers.
//!
//! These utilities estimate how far a chain of CSS filters can extend content
//! beyond its original bounds. Calculations are conservative to avoid culling
//! or clipping visible pixels when filters are applied.

use crate::geometry::Rect;
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

/// Compute a conservative outset for a chain of resolved filters without needing a bbox.
///
/// The calculation accumulates blurs across the chain (radius * 3 per blur) and unions drop
/// shadows with the current bounds including existing outsets. SVG filters are treated as
/// in-bounds; callers that need region awareness should provide a bbox to
/// `filter_outset_with_bounds`.
pub fn filter_outset(filters: &[ResolvedFilter], scale: f32) -> FilterOutset {
  filter_outset_with_bounds(filters, scale, None)
}

/// Compute a conservative outset for a chain of resolved filters using an optional bounding box.
///
/// When `bbox` is provided, SVG filters expand according to their resolved filter region in CSS
/// pixels.
pub fn filter_outset_with_bounds(
  filters: &[ResolvedFilter],
  scale: f32,
  bbox: Option<Rect>,
) -> FilterOutset {
  let mut outset = FilterOutset::ZERO;
  let scale = if scale.is_finite() { scale.abs() } else { 0.0 };

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
      ResolvedFilter::SvgFilter(filter) => {
        if let Some(bbox) = bbox {
          let region = filter.resolve_region(bbox);
          let delta_left = (bbox.min_x() - region.min_x()).max(0.0) * scale;
          let delta_top = (bbox.min_y() - region.min_y()).max(0.0) * scale;
          let delta_right = (region.max_x() - bbox.max_x()).max(0.0) * scale;
          let delta_bottom = (region.max_y() - bbox.max_y()).max(0.0) * scale;
          outset.left = outset.left.max(delta_left);
          outset.top = outset.top.max(delta_top);
          outset.right = outset.right.max(delta_right);
          outset.bottom = outset.bottom.max(delta_bottom);
        }
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

/// Compute the maximum outset needed to accommodate the provided filters, returning a tuple.
///
/// `bbox` is the bounding box of the filtered content in CSS pixels. `scale`
/// is the device scale to convert CSS lengths to the target coordinate space.
pub fn compute_filter_outset(
  filters: &[impl FilterOutsetExt],
  bbox: Rect,
  scale: f32,
) -> (f32, f32, f32, f32) {
  let mut out = (0.0, 0.0, 0.0, 0.0);
  let scale = if scale.is_finite() { scale.abs() } else { 0.0 };

  for filter in filters {
    filter.expand_outset(bbox, scale, &mut out);
  }

  (
    out.0.max(0.0),
    out.1.max(0.0),
    out.2.max(0.0),
    out.3.max(0.0),
  )
}

/// Trait implemented by filters that can contribute to an outset.
pub trait FilterOutsetExt {
  fn expand_outset(&self, bbox: Rect, scale: f32, out: &mut (f32, f32, f32, f32));
}

impl FilterOutsetExt for ResolvedFilter {
  fn expand_outset(&self, bbox: Rect, scale: f32, out: &mut (f32, f32, f32, f32)) {
    match self {
      ResolvedFilter::Blur(radius) => {
        let delta = (radius * scale).abs() * 3.0;
        out.0 += delta;
        out.1 += delta;
        out.2 += delta;
        out.3 += delta;
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
        let shadow_left = out.0 + delta - dx;
        let shadow_right = out.2 + delta + dx;
        let shadow_top = out.1 + delta - dy;
        let shadow_bottom = out.3 + delta + dy;
        out.0 = out.0.max(shadow_left);
        out.2 = out.2.max(shadow_right);
        out.1 = out.1.max(shadow_top);
        out.3 = out.3.max(shadow_bottom);
      }
      ResolvedFilter::SvgFilter(filter) => {
        let region = filter.resolve_region(bbox);
        let delta_left = (bbox.min_x() - region.min_x()).max(0.0) * scale;
        let delta_top = (bbox.min_y() - region.min_y()).max(0.0) * scale;
        let delta_right = (region.max_x() - bbox.max_x()).max(0.0) * scale;
        let delta_bottom = (region.max_y() - bbox.max_y()).max(0.0) * scale;
        out.0 = out.0.max(delta_left);
        out.1 = out.1.max(delta_top);
        out.2 = out.2.max(delta_right);
        out.3 = out.3.max(delta_bottom);
      }
      _ => {}
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::paint::svg_filter::{
    ColorInterpolationFilters, SvgFilter, SvgFilterRegion, SvgFilterUnits, SvgLength,
  };
  use std::sync::Arc;

  #[test]
  fn blur_filters_accumulate() {
    let filters = vec![ResolvedFilter::Blur(2.0), ResolvedFilter::Blur(3.0)];
    let bbox = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
    let (l, t, r, b) = compute_filter_outset(&filters, bbox, 1.0);
    assert!(
      (l - 15.0).abs() < 0.01
        && (t - 15.0).abs() < 0.01
        && (r - 15.0).abs() < 0.01
        && (b - 15.0).abs() < 0.01,
      "blur outsets should accumulate across filters"
    );
  }

  #[test]
  fn svg_filter_region_expands_outset() {
    let filter = SvgFilter {
      color_interpolation_filters: ColorInterpolationFilters::LinearRGB,
      steps: Vec::new(),
      region: SvgFilterRegion {
        x: SvgLength::Percent(-0.1),
        y: SvgLength::Percent(-0.1),
        width: SvgLength::Percent(1.2),
        height: SvgLength::Percent(1.2),
        units: SvgFilterUnits::ObjectBoundingBox,
      },
    };
    let filters = vec![ResolvedFilter::SvgFilter(Arc::new(filter))];
    let bbox = Rect::from_xywh(10.0, 20.0, 100.0, 50.0);
    let (l, t, r, b) = compute_filter_outset(&filters, bbox, 1.0);
    assert!(
      (l - 10.0).abs() < 0.01
        && (r - 10.0).abs() < 0.01
        && (t - 5.0).abs() < 0.01
        && (b - 5.0).abs() < 0.01,
      "expected outset of 10,5,10,5 but got {l},{t},{r},{b}"
    );
  }
}
