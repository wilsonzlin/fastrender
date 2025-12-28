//! Paint integration tests.

mod util;

mod avif_test;
mod backend_parity;
mod backface_culling_test;
mod canvas_test;
mod color_mix_display_list_test;
mod color_mix_polar_display_list_test;
mod display_list_renderer_test;
mod display_list_skip_ink_test;
mod display_list_test;
mod fit_canvas_to_content;
mod fragmented_root_paint;
mod math_render;
mod optimize_test;
mod overflow_clip_test;
mod paged_media_margin_boxes;
mod parallel_paint_test;
mod picture_source_selection;
mod preserve3d_scene;
mod preserve_3d_test;
mod projective_mapping;
mod rasterize_test;
mod stacking_test;
mod svg_filter_color_interpolation;
mod svg_filter_display_none_defs_test;
mod svg_filter_offset_test;
mod svg_filter_painter_test;
mod svg_filter_test;
mod svg_foreign_object_css_limits_test;
mod svg_inline_css_cdata_test;
mod svg_inline_test;
mod text_rasterize_test;
mod top_layer_test;

mod marker_paint_order;
mod marker_shadow_bidi;
mod marker_shadow_opacity;
mod marker_shadow_order;
mod marker_text_shadow_vertical;
mod marker_underline_order;
