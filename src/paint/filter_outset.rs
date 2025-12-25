use crate::geometry::Rect;
use crate::paint::display_list::ResolvedFilter;

/// Compute the maximum outset needed to accommodate the provided filters.
///
/// `bbox` is the bounding box of the filtered content in CSS pixels. `scale`
/// is the device scale to convert CSS lengths to the target coordinate space.
pub fn compute_filter_outset(
  filters: &[ResolvedFilter],
  bbox: Rect,
  scale: f32,
) -> (f32, f32, f32, f32) {
  let mut left: f32 = 0.0;
  let mut top: f32 = 0.0;
  let mut right: f32 = 0.0;
  let mut bottom: f32 = 0.0;
  let scale = if scale.is_finite() { scale.abs() } else { 0.0 };

  for filter in filters {
    match filter {
      ResolvedFilter::Blur(radius) => {
        let delta = (radius * scale).abs() * 3.0;
        left += delta;
        right += delta;
        top += delta;
        bottom += delta;
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
        let shadow_left = left + delta - dx;
        let shadow_right = right + delta + dx;
        let shadow_top = top + delta - dy;
        let shadow_bottom = bottom + delta + dy;
        left = left.max(shadow_left);
        right = right.max(shadow_right);
        top = top.max(shadow_top);
        bottom = bottom.max(shadow_bottom);
      }
      ResolvedFilter::SvgFilter(filter) => {
        let region = filter.resolve_region(bbox);
        let delta_left = (bbox.min_x() - region.min_x()).max(0.0) * scale;
        let delta_top = (bbox.min_y() - region.min_y()).max(0.0) * scale;
        let delta_right = (region.max_x() - bbox.max_x()).max(0.0) * scale;
        let delta_bottom = (region.max_y() - bbox.max_y()).max(0.0) * scale;
        left = left.max(delta_left);
        top = top.max(delta_top);
        right = right.max(delta_right);
        bottom = bottom.max(delta_bottom);
      }
      _ => {}
    }
  }

  (left.max(0.0), top.max(0.0), right.max(0.0), bottom.max(0.0))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::paint::svg_filter::{SvgFilter, SvgFilterRegion, SvgFilterUnits, SvgLength};
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
