// Aggregator for paint regression tests under tests/paint/.
// Cargo only executes test files at the root of `tests/`, so we include the
// nested paint modules here.

#[path = "paint/marker_image_shadow.rs"]
mod marker_image_shadow;
#[path = "paint/marker_paint_order.rs"]
mod marker_paint_order;
#[path = "paint/marker_shadow_bidi.rs"]
mod marker_shadow_bidi;
#[path = "paint/marker_shadow_opacity.rs"]
mod marker_shadow_opacity;
#[path = "paint/marker_shadow_order.rs"]
mod marker_shadow_order;
#[path = "paint/marker_text_shadow_vertical.rs"]
mod marker_text_shadow_vertical;
#[path = "paint/marker_underline_order.rs"]
mod marker_underline_order;
#[path = "paint/util.rs"]
mod util;
