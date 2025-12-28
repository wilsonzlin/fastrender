//! Display List Builder - Converts Fragment Tree to Display List
//!
//! This module builds a display list from the fragment tree by traversing
//! fragments and emitting paint commands in correct CSS paint order.
//!
//! # Pipeline
//!
//! ```text
//! Fragment Tree → Display List Builder → Display List → Rasterizer → Pixels
//! ```
//!
//! # Paint Order (CSS 2.1 Appendix E)
//!
//! For each fragment:
//! 1. Background color
//! 2. Background image
//! 3. Border
//! 4. Children (recursively)
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::paint::{DisplayListBuilder, DisplayList};
//!
//! let builder = DisplayListBuilder::new();
//! let display_list = builder.build_tree(&fragment_tree);
//! ```

use crate::css::types::ColorStop;
use crate::css::types::RadialGradientShape;
use crate::css::types::RadialGradientSize;
use crate::debug::runtime;
use crate::geometry::Point;
use crate::geometry::Rect;
use crate::geometry::Size;
use crate::image_loader::ImageCache;
use crate::layout::contexts::inline::baseline::compute_line_height_with_metrics_viewport;
use crate::layout::contexts::inline::line_builder::TextItem as InlineTextItem;
use crate::layout::utils::resolve_font_relative_length;
use crate::math::{layout_mathml, MathFragment};
use crate::paint::clip_path::resolve_clip_path;
use crate::paint::display_list::BlendMode;
use crate::paint::display_list::BlendModeItem;
use crate::paint::display_list::BorderImageItem;
use crate::paint::display_list::BorderImageSourceItem;
use crate::paint::display_list::BorderItem;
use crate::paint::display_list::BorderRadii;
use crate::paint::display_list::BorderSide;
use crate::paint::display_list::BoxShadowItem;
use crate::paint::display_list::ClipItem;
use crate::paint::display_list::ClipShape;
use crate::paint::display_list::ConicGradientItem;
use crate::paint::display_list::DecorationPaint;
use crate::paint::display_list::DecorationStroke;
use crate::paint::display_list::DisplayItem;
use crate::paint::display_list::DisplayList;
use crate::paint::display_list::EmphasisMark;
use crate::paint::display_list::EmphasisText;
use crate::paint::display_list::FillRectItem;
use crate::paint::display_list::FillRoundedRectItem;
use crate::paint::display_list::FontId;
use crate::paint::display_list::GlyphInstance;
use crate::paint::display_list::GradientSpread;
use crate::paint::display_list::GradientStop;
use crate::paint::display_list::ImageData;
use crate::paint::display_list::ImageFilterQuality;
use crate::paint::display_list::ImageItem;
use crate::paint::display_list::LinearGradientItem;
use crate::paint::display_list::ListMarkerItem;
use crate::paint::display_list::MaskReferenceRects;
use crate::paint::display_list::OpacityItem;
use crate::paint::display_list::OutlineItem;
use crate::paint::display_list::RadialGradientItem;
use crate::paint::display_list::ResolvedFilter;
use crate::paint::display_list::ResolvedMask;
use crate::paint::display_list::ResolvedMaskImage;
use crate::paint::display_list::ResolvedMaskLayer;
use crate::paint::display_list::StackingContextItem;
use crate::paint::display_list::StrokeRectItem;
use crate::paint::display_list::StrokeRoundedRectItem;
use crate::paint::display_list::TextDecorationItem;
use crate::paint::display_list::TextEmphasis;
use crate::paint::display_list::TextItem;
use crate::paint::display_list::TextShadowItem;
use crate::paint::display_list::Transform3D;
use crate::paint::object_fit::compute_object_fit;
use crate::paint::object_fit::default_object_position;
use crate::paint::stacking::Layer6Item;
use crate::paint::stacking::StackingContext;
use crate::paint::svg_filter::SvgFilterResolver;
use crate::paint::text_shadow::resolve_text_shadows;
use crate::paint::transform3d::backface_is_hidden;
use crate::paint::transform_resolver::ResolvedTransforms;
use crate::style::block_axis_is_horizontal;
use crate::style::block_axis_positive;
use crate::style::color::Rgba;
use crate::style::types::AccentColor;
use crate::style::types::Appearance;
use crate::style::types::BackfaceVisibility;
use crate::style::types::BackgroundAttachment;
use crate::style::types::BackgroundBox;
use crate::style::types::BackgroundImage;
use crate::style::types::BackgroundLayer;
use crate::style::types::BackgroundPosition;
use crate::style::types::BackgroundRepeatKeyword;
use crate::style::types::BackgroundSize;
use crate::style::types::BackgroundSizeComponent;
use crate::style::types::BackgroundSizeKeyword;
use crate::style::types::BorderImageSource;
use crate::style::types::BoxDecorationBreak;
use crate::style::types::ImageOrientation;
use crate::style::types::ImageRendering;
use crate::style::types::Isolation;
use crate::style::types::MaskClip;
use crate::style::types::MaskComposite;
use crate::style::types::MaskMode;
use crate::style::types::MaskOrigin;
use crate::style::types::MixBlendMode;
use crate::style::types::ObjectFit;
use crate::style::types::ResolvedTextDecoration;
use crate::style::types::TextDecorationLine;
use crate::style::types::TextDecorationSkipInk;
use crate::style::types::TextDecorationStyle;
use crate::style::types::TextDecorationThickness;
use crate::style::types::TextEmphasisPosition;
use crate::style::types::TextEmphasisStyle;
use crate::style::types::TextUnderlineOffset;
use crate::style::types::TextUnderlinePosition;
use crate::style::types::TransformBox;
use crate::style::types::TransformStyle;
use crate::style::values::Length;
use crate::style::values::LengthUnit;
use crate::style::ComputedStyle;
use crate::text::font_db::FontStretch;
use crate::text::font_db::FontStyle;
use crate::text::font_db::ScaledMetrics;
use crate::text::font_loader::FontContext;
use crate::text::pipeline::ShapedRun;
use crate::text::pipeline::ShapingPipeline;
use crate::tree::box_tree::FormControl;
use crate::tree::box_tree::FormControlKind;
use crate::tree::box_tree::ReplacedType;
use crate::tree::box_tree::TextControlKind;
use crate::tree::fragment_tree::FragmentContent;
use crate::tree::fragment_tree::FragmentNode;
use crate::tree::fragment_tree::FragmentTree;
use rayon::prelude::*;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// Builder that converts a fragment tree to a display list
///
/// Walks the fragment tree depth-first, emitting display items
/// for backgrounds, borders, and content in correct CSS paint order.
pub struct DisplayListBuilder {
  /// The display list being built
  list: DisplayList,
  image_cache: Option<ImageCache>,
  /// Serialized SVG filter definitions collected from the document DOM.
  svg_filter_defs: Option<Arc<HashMap<String, String>>>,
  viewport: Option<(f32, f32)>,
  font_ctx: FontContext,
  shaper: ShapingPipeline,
  device_pixel_ratio: f32,
  parallel_enabled: bool,
  parallel_min: usize,
  scroll_state: ScrollState,
}

#[derive(Clone, Copy)]
struct BackgroundRects {
  border: Rect,
  padding: Rect,
  content: Rect,
}

fn parallel_config_from_env() -> (bool, usize) {
  let toggles = runtime::runtime_toggles();
  let enabled = toggles.truthy_with_default("FASTR_DISPLAY_LIST_PARALLEL", true);
  let min = toggles
    .usize_with_default("FASTR_DISPLAY_LIST_PARALLEL_MIN", 32)
    .max(1);
  (enabled, min)
}

impl DisplayListBuilder {
  fn resolve_scaled_metrics(
    style: &ComputedStyle,
    font_ctx: &FontContext,
  ) -> Option<ScaledMetrics> {
    let italic = matches!(style.font_style, crate::style::types::FontStyle::Italic);
    let oblique = matches!(style.font_style, crate::style::types::FontStyle::Oblique(_));
    let stretch = FontStretch::from_percentage(style.font_stretch.to_percentage());

    font_ctx
      .get_font_full(
        &style.font_family,
        style.font_weight.to_u16(),
        if italic {
          FontStyle::Italic
        } else if oblique {
          FontStyle::Oblique
        } else {
          FontStyle::Normal
        },
        stretch,
      )
      .or_else(|| font_ctx.get_sans_serif())
      .and_then(|font| font.metrics().ok())
      .map(|m| m.scale(style.font_size))
  }

  fn element_scroll_offset(&self, fragment: &FragmentNode) -> Point {
    fragment
      .box_id()
      .and_then(|id| self.scroll_state.elements.get(&id).copied())
      .unwrap_or(Point::ZERO)
  }

  /// Creates a new display list builder
  pub fn new() -> Self {
    let (parallel_enabled, parallel_min) = parallel_config_from_env();
    Self {
      list: DisplayList::new(),
      image_cache: Some(ImageCache::new()),
      svg_filter_defs: None,
      viewport: None,
      font_ctx: FontContext::new(),
      shaper: ShapingPipeline::new(),
      device_pixel_ratio: 1.0,
      parallel_enabled,
      parallel_min,
      scroll_state: ScrollState::default(),
    }
  }

  /// Creates a display list builder backed by an image cache to rasterize replaced images.
  pub fn with_image_cache(image_cache: ImageCache) -> Self {
    let (parallel_enabled, parallel_min) = parallel_config_from_env();
    Self {
      list: DisplayList::new(),
      image_cache: Some(image_cache),
      svg_filter_defs: None,
      viewport: None,
      font_ctx: FontContext::new(),
      shaper: ShapingPipeline::new(),
      device_pixel_ratio: 1.0,
      parallel_enabled,
      parallel_min,
      scroll_state: ScrollState::default(),
    }
  }

  /// Sets the base URL used for resolving relative image URLs when decoding backgrounds/replaced elements.
  pub fn with_base_url(mut self, base_url: impl Into<String>) -> Self {
    let url = base_url.into();
    self.image_cache = self.image_cache.take().map(|mut cache| {
      cache.set_base_url(url);
      cache
    });
    self
  }

  /// Updates the base URL on the underlying image cache.
  pub fn set_base_url(&mut self, base_url: impl Into<String>) {
    if let Some(cache) = self.image_cache.as_mut() {
      cache.set_base_url(base_url);
    }
  }

  /// Sets serialized SVG filter definitions to use when resolving `url(#...)` filters.
  pub fn with_svg_filter_defs(mut self, defs: Option<Arc<HashMap<String, String>>>) -> Self {
    self.svg_filter_defs = defs;
    self
  }

  /// Updates the SVG filter registry used for `url(#...)` filters.
  pub fn set_svg_filter_defs(&mut self, defs: Option<Arc<HashMap<String, String>>>) {
    self.svg_filter_defs = defs;
  }

  /// Sets the font context for shaping text into the display list.
  pub fn with_font_context(mut self, font_ctx: FontContext) -> Self {
    self.font_ctx = font_ctx;
    self
  }

  /// Sets the device pixel ratio for density selection (e.g., srcset/image-set).
  pub fn with_device_pixel_ratio(mut self, dpr: f32) -> Self {
    self.device_pixel_ratio = if dpr.is_finite() && dpr > 0.0 {
      dpr
    } else {
      1.0
    };
    self
  }

  /// Updates the device pixel ratio in place.
  pub fn set_device_pixel_ratio(&mut self, dpr: f32) {
    self.device_pixel_ratio = if dpr.is_finite() && dpr > 0.0 {
      dpr
    } else {
      1.0
    };
  }

  /// Sets the viewport size for resolving viewport-relative units (vw/vh) in object-position.
  pub fn with_viewport_size(mut self, width: f32, height: f32) -> Self {
    self.viewport = Some((width, height));
    self
  }

  /// Sets the scroll state used when translating element content during display list construction.
  pub fn with_scroll_state(mut self, scroll_state: ScrollState) -> Self {
    self.scroll_state = scroll_state;
    self
  }

  /// Builds a display list from a fragment tree root
  pub fn build(mut self, root: &FragmentNode) -> DisplayList {
    if self.viewport.is_none() {
      self.viewport = Some((root.bounds.width(), root.bounds.height()));
    }
    self.build_fragment(root, Point::ZERO);
    self.list
  }

  /// Builds a display list from a FragmentTree
  pub fn build_tree(mut self, tree: &FragmentTree) -> DisplayList {
    if self.viewport.is_none() {
      let viewport = tree.viewport_size();
      self.viewport = Some((viewport.width, viewport.height));
    }
    for root in std::iter::once(&tree.root).chain(tree.additional_fragments.iter()) {
      self.build_fragment(root, Point::ZERO);
    }
    self.list
  }

  /// Builds a stacking-context-aware display list from a `FragmentTree`.
  pub fn build_tree_with_stacking(mut self, tree: &FragmentTree) -> DisplayList {
    if self.viewport.is_none() {
      let viewport = tree.viewport_size();
      self.viewport = Some((viewport.width, viewport.height));
    }

    let svg_roots: Vec<&FragmentNode> = std::iter::once(&tree.root)
      .chain(tree.additional_fragments.iter())
      .collect();
    let image_cache = self.image_cache.clone();
    let defs = tree
      .svg_filter_defs
      .clone()
      .or_else(|| self.svg_filter_defs.clone());
    let mut svg_filters = SvgFilterResolver::new(defs, svg_roots, image_cache.as_ref());

    let contexts = crate::paint::stacking::build_stacking_tree_from_tree(tree);
    for context in &contexts {
      self.build_stacking_context(context, Point::ZERO, true, &mut svg_filters);
    }

    self.list
  }

  /// Builds a display list from a stacking context tree (respecting z-order).
  pub fn build_from_stacking(mut self, stacking: &StackingContext) -> DisplayList {
    if self.viewport.is_none() {
      self.viewport = Some((stacking.bounds.width(), stacking.bounds.height()));
    }
    let mut svg_roots = Vec::new();
    Self::collect_stacking_fragments(stacking, &mut svg_roots);
    let image_cache = self.image_cache.clone();
    let mut svg_filters = SvgFilterResolver::new(
      self.svg_filter_defs.clone(),
      svg_roots,
      image_cache.as_ref(),
    );
    self.build_stacking_context(stacking, Point::ZERO, true, &mut svg_filters);
    self.list
  }

  /// Builds a display list by first constructing a stacking context tree from the fragment tree.
  pub fn build_with_stacking_tree(mut self, root: &FragmentNode) -> DisplayList {
    if self.viewport.is_none() {
      self.viewport = Some((root.bounds.width(), root.bounds.height()));
    }
    let stacking = crate::paint::stacking::build_stacking_tree_from_fragment_tree(root);
    let image_cache = self.image_cache.clone();
    let mut svg_filters = SvgFilterResolver::new(
      self.svg_filter_defs.clone(),
      vec![root],
      image_cache.as_ref(),
    );
    self.build_stacking_context(&stacking, Point::ZERO, true, &mut svg_filters);
    self.list
  }

  /// Builds a display list by first constructing a stacking context tree from the fragment tree
  /// and applying an additional offset to all fragments.
  pub fn build_with_stacking_tree_offset(
    mut self,
    root: &FragmentNode,
    offset: Point,
  ) -> DisplayList {
    if self.viewport.is_none() {
      self.viewport = Some((root.bounds.width(), root.bounds.height()));
    }
    let stacking = crate::paint::stacking::build_stacking_tree_from_fragment_tree(root);
    let mut svg_roots = Vec::new();
    Self::collect_stacking_fragments(&stacking, &mut svg_roots);
    let image_cache = self.image_cache.clone();
    let mut svg_filters = SvgFilterResolver::new(
      self.svg_filter_defs.clone(),
      svg_roots,
      image_cache.as_ref(),
    );
    self.build_stacking_context(&stacking, offset, true, &mut svg_filters);
    self.list
  }

  /// Builds a display list from multiple stacking context roots.
  pub fn build_from_stacking_contexts(mut self, stackings: &[StackingContext]) -> DisplayList {
    if self.viewport.is_none() {
      if let Some(first) = stackings.first() {
        self.viewport = Some((first.bounds.width(), first.bounds.height()));
      }
    }
    let mut svg_roots = Vec::new();
    for stacking in stackings {
      Self::collect_stacking_fragments(stacking, &mut svg_roots);
    }
    let image_cache = self.image_cache.clone();
    let mut svg_filters = SvgFilterResolver::new(
      self.svg_filter_defs.clone(),
      svg_roots,
      image_cache.as_ref(),
    );
    for stacking in stackings {
      self.build_stacking_context(stacking, Point::ZERO, true, &mut svg_filters);
    }
    self.list
  }

  /// Builds a display list by first constructing stacking context trees from a fragment tree.
  pub fn build_with_stacking_tree_from_tree(mut self, tree: &FragmentTree) -> DisplayList {
    if self.viewport.is_none() {
      let viewport = tree.viewport_size();
      self.viewport = Some((viewport.width, viewport.height));
    }
    let defs = tree
      .svg_filter_defs
      .clone()
      .or_else(|| self.svg_filter_defs.clone());
    self.svg_filter_defs = defs;
    let stackings = crate::paint::stacking::build_stacking_tree_from_tree(tree);
    self.build_from_stacking_contexts(&stackings)
  }

  /// Builds a display list with clipping support
  ///
  /// Fragments with box_ids in the `clips` set will have clipping applied.
  pub fn build_with_clips(
    mut self,
    root: &FragmentNode,
    clips: &HashSet<Option<usize>>,
  ) -> DisplayList {
    self.build_fragment_with_clips(root, Point::ZERO, clips);
    self.list
  }

  fn collect_stacking_fragments<'a>(context: &'a StackingContext, out: &mut Vec<&'a FragmentNode>) {
    out.extend(context.fragments.iter());
    out.extend(context.layer3_blocks.iter());
    out.extend(context.layer4_floats.iter());
    out.extend(context.layer5_inlines.iter());
    out.extend(
      context
        .layer6_positioned
        .iter()
        .map(|ordered| &ordered.fragment),
    );
    for child in &context.children {
      Self::collect_stacking_fragments(child, out);
    }
  }

  /// Recursively builds display items for a fragment
  fn build_fragment(&mut self, fragment: &FragmentNode, offset: Point) {
    self.build_fragment_internal(fragment, offset, true, false);
  }

  /// Builds display items for a fragment without descending into children.
  fn build_fragment_shallow(&mut self, fragment: &FragmentNode, offset: Point) {
    self.build_fragment_internal(fragment, offset, false, false);
  }

  fn build_fragment_internal(
    &mut self,
    fragment: &FragmentNode,
    offset: Point,
    recurse_children: bool,
    suppress_opacity: bool,
  ) {
    let style_opt = fragment.style.as_deref();
    if let Some(style) = style_opt {
      if !matches!(
        style.visibility,
        crate::style::computed::Visibility::Visible
      ) {
        return;
      }
    }

    if matches!(fragment.content, FragmentContent::RunningAnchor { .. }) {
      return;
    }

    let opacity = style_opt.map(|s| s.opacity).unwrap_or(1.0);
    if opacity <= f32::EPSILON {
      return;
    }
    let push_opacity = !suppress_opacity && opacity < 1.0 - f32::EPSILON;
    if push_opacity {
      self.push_opacity(opacity);
    }

    let absolute_rect = Rect::new(
      Point::new(
        fragment.bounds.origin.x + offset.x,
        fragment.bounds.origin.y + offset.y,
      ),
      fragment.bounds.size,
    );

    if let Some(style) = style_opt {
      if matches!(style.backface_visibility, BackfaceVisibility::Hidden)
        && (!style.transform.is_empty() || style.perspective.is_some() || style.has_motion_path())
      {
        let transforms = Self::build_transform(style, absolute_rect, self.viewport);
        if let Some(transform) = transforms.self_transform.as_ref() {
          if backface_is_hidden(transform) {
            if push_opacity {
              self.pop_opacity();
            }
            return;
          }
        }
      }
    }

    let (overflow_clip, clip_rect) = if let Some(style) = style_opt {
      (
        Self::overflow_clip_from_style(style, absolute_rect, self.viewport),
        Self::clip_rect_from_style(style, absolute_rect, self.viewport),
      )
    } else {
      (None, None)
    };

    if let Some(style) = style_opt {
      let (decoration_rect, decoration_clip) =
        Self::decoration_rect_and_clip(fragment, absolute_rect, style);
      if let Some(clip) = decoration_clip.as_ref() {
        self.list.push(DisplayItem::PushClip(clip.clone()));
      }
      self.emit_box_shadows_from_style(decoration_rect, style, false);
      self.emit_background_from_style(decoration_rect, style);
      self.emit_box_shadows_from_style(decoration_rect, style, true);
      self.emit_border_from_style(decoration_rect, style);
      if decoration_clip.is_some() {
        self.list.push(DisplayItem::PopClip);
      }
    }

    // CSS Paint Order:
    // 1. Background (handled by caller if style available)
    // 2. Border (handled by caller if style available)
    // 3. Content (text, images)
    // 4. Children

    // Clip descendant/content painting but leave outer effects (e.g., box shadows, outlines)
    // unaffected.
    let mut pushed_clips = 0;
    if let Some(clip) = overflow_clip {
      self.list.push(DisplayItem::PushClip(clip));
      pushed_clips += 1;
    }
    if let Some(clip) = clip_rect {
      self.list.push(DisplayItem::PushClip(clip));
      pushed_clips += 1;
    }

    self.emit_content(fragment, absolute_rect);

    if recurse_children {
      let element_scroll = self.element_scroll_offset(fragment);
      let child_offset = Point::new(
        absolute_rect.origin.x - element_scroll.x,
        absolute_rect.origin.y - element_scroll.y,
      );
      for child in &fragment.children {
        self.build_fragment_internal(child, child_offset, true, false);
      }
    }

    for _ in 0..pushed_clips {
      self.list.push(DisplayItem::PopClip);
    }

    if let Some(style) = style_opt {
      self.emit_outline(absolute_rect, style);
    }

    if push_opacity {
      self.pop_opacity();
    }
  }

  /// Recursively builds display items with clipping support
  fn build_fragment_with_clips(
    &mut self,
    fragment: &FragmentNode,
    offset: Point,
    clips: &HashSet<Option<usize>>,
  ) {
    if let Some(style) = fragment.style.as_deref() {
      if !matches!(
        style.visibility,
        crate::style::computed::Visibility::Visible
      ) {
        return;
      }
    }

    if matches!(fragment.content, FragmentContent::RunningAnchor { .. }) {
      return;
    }

    let opacity = fragment.style.as_deref().map(|s| s.opacity).unwrap_or(1.0);
    let push_opacity = opacity < 1.0 - f32::EPSILON;
    if push_opacity {
      self.push_opacity(opacity);
    }

    let absolute_rect = Rect::new(
      Point::new(
        fragment.bounds.origin.x + offset.x,
        fragment.bounds.origin.y + offset.y,
      ),
      fragment.bounds.size,
    );

    if let Some(style) = fragment.style.as_deref() {
      let (decoration_rect, decoration_clip) =
        Self::decoration_rect_and_clip(fragment, absolute_rect, style);
      if let Some(clip) = decoration_clip.as_ref() {
        self.list.push(DisplayItem::PushClip(clip.clone()));
      }
      self.emit_background_from_style(decoration_rect, style);
      self.emit_border_from_style(decoration_rect, style);
      if decoration_clip.is_some() {
        self.list.push(DisplayItem::PopClip);
      }
    }

    let box_id = Self::get_box_id(fragment);
    let should_clip = clips.contains(&box_id);

    // Emit content before clipping children
    self.emit_content(fragment, absolute_rect);

    // Push clip if needed
    if should_clip {
      self.list.push(DisplayItem::PushClip(ClipItem {
        shape: ClipShape::Rect {
          rect: absolute_rect,
          radii: None,
        },
      }));
    }

    // Recurse to children
    let element_scroll = self.element_scroll_offset(fragment);
    let child_offset = Point::new(
      absolute_rect.origin.x - element_scroll.x,
      absolute_rect.origin.y - element_scroll.y,
    );
    for child in &fragment.children {
      self.build_fragment_with_clips(child, child_offset, clips);
    }

    // Pop clip
    if should_clip {
      self.list.push(DisplayItem::PopClip);
    }

    if let Some(style) = fragment.style.as_deref() {
      self.emit_outline(absolute_rect, style);
    }

    if push_opacity {
      self.pop_opacity();
    }
  }

  fn build_stacking_context(
    &mut self,
    context: &StackingContext,
    offset: Point,
    is_root: bool,
    svg_filters: &mut SvgFilterResolver,
  ) {
    let mut children: Vec<&StackingContext> = context.children.iter().collect();
    children.sort_by(|a, b| {
      a.z_index
        .cmp(&b.z_index)
        .then_with(|| a.tree_order.cmp(&b.tree_order))
    });

    let (neg, non_neg): (Vec<_>, Vec<_>) = children.into_iter().partition(|c| c.z_index < 0);
    let (_zero, pos): (Vec<_>, Vec<_>) = non_neg.into_iter().partition(|c| c.z_index == 0);

    // Descendants are positioned relative to the stacking context's origin (the first fragment).
    let descendant_offset = Point::new(
      offset.x + context.offset_from_parent_context.x,
      offset.y + context.offset_from_parent_context.y,
    );

    let root_fragment = context.fragments.first();
    let root_style = root_fragment.and_then(|f| f.style.as_deref());
    let root_opacity = root_style.map(|s| s.opacity).unwrap_or(1.0);
    if root_opacity <= f32::EPSILON {
      return;
    }
    let apply_opacity = root_opacity < 1.0 - f32::EPSILON;
    let paint_contained = root_style.map(|s| s.containment.paint).unwrap_or(false);
    let context_bounds = context.bounds.translate(offset);
    let root_fragment_offset = root_fragment
      .map(|fragment| {
        Point::new(
          descendant_offset.x - fragment.bounds.origin.x,
          descendant_offset.y - fragment.bounds.origin.y,
        )
      })
      .unwrap_or(offset);
    let layer6_items = context.layer6_items();
    let mask = root_style.and_then(|style| self.resolve_mask(style, context_bounds));

    let root_fragment_rect = root_fragment.map(|fragment| {
      Rect::new(
        Point::new(
          fragment.bounds.origin.x + offset.x,
          fragment.bounds.origin.y + offset.y,
        ),
        fragment.bounds.size,
      )
    });
    let plane_rect = match (root_style, root_fragment_rect) {
      (Some(style), Some(rect)) => Self::transform_reference_box(style, rect, self.viewport),
      (_, Some(rect)) => rect,
      _ => context_bounds,
    };

    let mix_blend_mode = root_style
      .map(|s| Self::convert_blend_mode(s.mix_blend_mode))
      .unwrap_or(BlendMode::Normal);
    let is_isolated = root_style
      .map(|s| matches!(s.isolation, Isolation::Isolate) || !s.backdrop_filter.is_empty())
      .unwrap_or(false);
    let (filters, backdrop_filters, radii) = root_style
      .map(|style| {
        (
          Self::resolve_filters(
            &style.filter,
            style,
            self.viewport,
            &self.font_ctx,
            svg_filters,
          ),
          Self::resolve_filters(
            &style.backdrop_filter,
            style,
            self.viewport,
            &self.font_ctx,
            svg_filters,
          ),
          Self::resolve_border_radii(Some(style), context.bounds, self.viewport),
        )
      })
      .unwrap_or((
        Vec::new(),
        Vec::new(),
        crate::paint::display_list::BorderRadii::ZERO,
      ));
    let transform_bounds = root_fragment_rect.unwrap_or(context_bounds);
    let transforms = root_style
      .map(|style| {
        crate::paint::transform_resolver::resolve_transforms(style, transform_bounds, self.viewport)
      })
      .unwrap_or_default();
    let transform = transforms.self_transform;
    let child_perspective = transforms.child_perspective;
    let transform_style = root_style
      .map(|style| Self::used_transform_style(style))
      .unwrap_or(TransformStyle::Flat);
    let backface_visibility = root_style
      .map(|style| style.backface_visibility)
      .unwrap_or(BackfaceVisibility::Visible);

    if matches!(backface_visibility, BackfaceVisibility::Hidden) {
      if let Some(t) = transform.as_ref() {
        if backface_is_hidden(t) {
          return;
        }
      }
    }

    let viewport = self
      .viewport
      .unwrap_or_else(|| (context_bounds.width(), context_bounds.height()));
    let clip_path = root_style
      .and_then(|style| resolve_clip_path(style, context_bounds, viewport, &self.font_ctx));
    let clip_rect = root_style
      .and_then(|style| Self::clip_rect_from_style(style, context_bounds, Some(viewport)));
    let overflow_clip = root_style
      .and_then(|style| Self::overflow_clip_from_style(style, context_bounds, self.viewport));
    let paint_containment_clip = if paint_contained {
      root_fragment
        .and_then(|fragment| {
          let style = root_style?;
          let rect = Rect::new(
            Point::new(
              fragment.bounds.origin.x + root_fragment_offset.x,
              fragment.bounds.origin.y + root_fragment_offset.y,
            ),
            fragment.bounds.size,
          );
          let rects = Self::background_rects(rect, style, self.viewport);
          let radii =
            Self::resolve_clip_radii(style, &rects, BackgroundBox::PaddingBox, self.viewport);
          Some(ClipItem {
            shape: ClipShape::Rect {
              rect: rects.padding,
              radii: if radii.is_zero() { None } else { Some(radii) },
            },
          })
        })
        .or_else(|| {
          Some(ClipItem {
            shape: ClipShape::Rect {
              rect: context_bounds,
              radii: None,
            },
          })
        })
    } else {
      None
    };
    let has_paint_containment_clip = paint_containment_clip.is_some();

    let has_effects = is_isolated
      || transform.is_some()
      || child_perspective.is_some()
      || mix_blend_mode != BlendMode::Normal
      || !filters.is_empty()
      || !backdrop_filters.is_empty()
      || clip_path.is_some()
      || clip_rect.is_some()
      || overflow_clip.is_some()
      || paint_contained
      || !radii.is_zero()
      || mask.is_some();

    let mut pushed_opacity = false;
    if apply_opacity {
      self.push_opacity(root_opacity);
      pushed_opacity = true;
    }

    if is_root && !has_effects {
      for child in neg {
        self.build_stacking_context(child, descendant_offset, false, svg_filters);
      }

      self.emit_fragment_list_shallow(&context.fragments, root_fragment_offset, apply_opacity);
      self.emit_fragment_list(&context.layer3_blocks, descendant_offset);
      self.emit_fragment_list(&context.layer4_floats, descendant_offset);
      self.emit_fragment_list(&context.layer5_inlines, descendant_offset);
      for item in &layer6_items {
        match item {
          Layer6Item::Positioned(fragment) => {
            self.build_fragment(&fragment.fragment, descendant_offset)
          }
          Layer6Item::ZeroContext(child) => {
            self.build_stacking_context(child, descendant_offset, false, svg_filters)
          }
        }
      }

      for child in pos {
        self.build_stacking_context(child, descendant_offset, false, svg_filters);
      }
      if pushed_opacity {
        self.pop_opacity();
      }
      return;
    }

    if let Some(clip) = paint_containment_clip {
      self.list.push(DisplayItem::PushClip(clip));
    }

    self
      .list
      .push(DisplayItem::PushStackingContext(StackingContextItem {
        z_index: context.z_index,
        creates_stacking_context: true,
        bounds: context_bounds,
        plane_rect,
        mix_blend_mode,
        is_isolated,
        transform,
        child_perspective,
        transform_style,
        backface_visibility,
        filters,
        backdrop_filters,
        radii,
        mask,
      }));

    let mut pushed_clips = 0;
    if let Some(path) = clip_path {
      self.list.push(DisplayItem::PushClip(ClipItem {
        shape: ClipShape::Path { path },
      }));
      pushed_clips += 1;
    }
    if let Some(clip) = clip_rect {
      self.list.push(DisplayItem::PushClip(clip));
      pushed_clips += 1;
    }

    // Paint the stacking context root (backgrounds, borders, shadows) before applying overflow
    // clipping so outer effects remain visible.
    self.emit_fragment_list_shallow(&context.fragments, root_fragment_offset, apply_opacity);

    let mut overflow_clip_pushed = false;
    if let Some(clip) = overflow_clip {
      self.list.push(DisplayItem::PushClip(clip));
      overflow_clip_pushed = true;
    }

    for child in neg {
      self.build_stacking_context(child, descendant_offset, false, svg_filters);
    }
    self.emit_fragment_list(&context.layer3_blocks, descendant_offset);
    self.emit_fragment_list(&context.layer4_floats, descendant_offset);
    self.emit_fragment_list(&context.layer5_inlines, descendant_offset);
    for item in &layer6_items {
      match item {
        Layer6Item::Positioned(fragment) => {
          self.build_fragment(&fragment.fragment, descendant_offset)
        }
        Layer6Item::ZeroContext(child) => {
          self.build_stacking_context(child, descendant_offset, false, svg_filters)
        }
      }
    }

    for child in pos {
      self.build_stacking_context(child, descendant_offset, false, svg_filters);
    }

    if overflow_clip_pushed {
      self.list.push(DisplayItem::PopClip);
    }
    for _ in 0..pushed_clips {
      self.list.push(DisplayItem::PopClip);
    }
    self.list.push(DisplayItem::PopStackingContext);
    if has_paint_containment_clip {
      self.list.push(DisplayItem::PopClip);
    }
    if pushed_opacity {
      self.pop_opacity();
    }
  }

  fn overflow_clip_from_style(
    style: &ComputedStyle,
    bounds: Rect,
    viewport: Option<(f32, f32)>,
  ) -> Option<ClipItem> {
    let clip_x = matches!(
      style.overflow_x,
      crate::style::types::Overflow::Hidden
        | crate::style::types::Overflow::Scroll
        | crate::style::types::Overflow::Auto
        | crate::style::types::Overflow::Clip
    );
    let clip_y = matches!(
      style.overflow_y,
      crate::style::types::Overflow::Hidden
        | crate::style::types::Overflow::Scroll
        | crate::style::types::Overflow::Auto
        | crate::style::types::Overflow::Clip
    );
    if !clip_x && !clip_y {
      return None;
    }

    let rects = Self::background_rects(bounds, style, viewport);
    let clip_rect = rects.padding;
    if clip_rect.width() <= 0.0 || clip_rect.height() <= 0.0 {
      return None;
    }
    let radii = Self::resolve_clip_radii(style, &rects, BackgroundBox::PaddingBox, viewport);
    Some(ClipItem {
      shape: ClipShape::Rect {
        rect: clip_rect,
        radii: if radii.is_zero() { None } else { Some(radii) },
      },
    })
  }

  fn clip_rect_from_style(
    style: &ComputedStyle,
    bounds: Rect,
    viewport: Option<(f32, f32)>,
  ) -> Option<ClipItem> {
    let clip = style.clip.as_ref()?;
    let viewport = viewport.unwrap_or_else(|| (bounds.width(), bounds.height()));
    let width = bounds.width();
    let height = bounds.height();
    let resolve = |component: &crate::style::types::ClipComponent, base: f32| -> f32 {
      match component {
        crate::style::types::ClipComponent::Auto => base,
        crate::style::types::ClipComponent::Length(len) => len
          .resolve_with_context(
            Some(base),
            viewport.0,
            viewport.1,
            style.font_size,
            style.root_font_size,
          )
          .unwrap_or_else(|| len.to_px()),
      }
    };

    let left = match &clip.left {
      crate::style::types::ClipComponent::Auto => bounds.x(),
      crate::style::types::ClipComponent::Length(len) => {
        bounds.x()
          + len
            .resolve_with_context(
              Some(width),
              viewport.0,
              viewport.1,
              style.font_size,
              style.root_font_size,
            )
            .unwrap_or_else(|| len.to_px())
      }
    };
    let top = match &clip.top {
      crate::style::types::ClipComponent::Auto => bounds.y(),
      crate::style::types::ClipComponent::Length(len) => {
        bounds.y()
          + len
            .resolve_with_context(
              Some(height),
              viewport.0,
              viewport.1,
              style.font_size,
              style.root_font_size,
            )
            .unwrap_or_else(|| len.to_px())
      }
    };
    let right = resolve(&clip.right, bounds.x() + width);
    let bottom = resolve(&clip.bottom, bounds.y() + height);

    let rect = Rect::from_xywh(left, top, right - left, bottom - top);
    if rect.width() <= 0.0 || rect.height() <= 0.0 {
      None
    } else {
      Some(ClipItem {
        shape: ClipShape::Rect { rect, radii: None },
      })
    }
  }

  fn used_transform_style(style: &ComputedStyle) -> TransformStyle {
    if Self::is_3d_flattening_boundary(style) {
      TransformStyle::Flat
    } else {
      style.transform_style
    }
  }

  fn is_3d_flattening_boundary(style: &ComputedStyle) -> bool {
    if !style.filter.is_empty() || !style.backdrop_filter.is_empty() {
      return true;
    }
    if style.opacity < 1.0 - f32::EPSILON {
      return true;
    }
    if !matches!(style.clip_path, crate::style::types::ClipPath::None) {
      return true;
    }
    if style.mask_layers.iter().any(|layer| layer.image.is_some()) {
      return true;
    }
    if !matches!(style.mix_blend_mode, MixBlendMode::Normal) {
      return true;
    }
    if matches!(style.isolation, Isolation::Isolate) {
      return true;
    }
    false
  }

  fn convert_blend_mode(mode: MixBlendMode) -> BlendMode {
    match mode {
      MixBlendMode::Normal => BlendMode::Normal,
      MixBlendMode::Multiply => BlendMode::Multiply,
      MixBlendMode::Screen => BlendMode::Screen,
      MixBlendMode::Overlay => BlendMode::Overlay,
      MixBlendMode::Darken => BlendMode::Darken,
      MixBlendMode::Lighten => BlendMode::Lighten,
      MixBlendMode::ColorDodge => BlendMode::ColorDodge,
      MixBlendMode::ColorBurn => BlendMode::ColorBurn,
      MixBlendMode::HardLight => BlendMode::HardLight,
      MixBlendMode::SoftLight => BlendMode::SoftLight,
      MixBlendMode::Difference => BlendMode::Difference,
      MixBlendMode::Exclusion => BlendMode::Exclusion,
      MixBlendMode::Hue => BlendMode::Hue,
      MixBlendMode::Saturation => BlendMode::Saturation,
      MixBlendMode::Color => BlendMode::Color,
      MixBlendMode::Luminosity => BlendMode::Luminosity,
      MixBlendMode::PlusLighter => BlendMode::PlusLighter,
      MixBlendMode::PlusDarker => BlendMode::PlusDarker,
      MixBlendMode::HueHsv => BlendMode::HueHsv,
      MixBlendMode::SaturationHsv => BlendMode::SaturationHsv,
      MixBlendMode::ColorHsv => BlendMode::ColorHsv,
      MixBlendMode::LuminosityHsv => BlendMode::LuminosityHsv,
      MixBlendMode::HueOklch => BlendMode::HueOklch,
      MixBlendMode::ChromaOklch => BlendMode::ChromaOklch,
      MixBlendMode::ColorOklch => BlendMode::ColorOklch,
      MixBlendMode::LuminosityOklch => BlendMode::LuminosityOklch,
    }
  }

  fn decoration_rect_and_clip(
    fragment: &FragmentNode,
    absolute_rect: Rect,
    style: &ComputedStyle,
  ) -> (Rect, Option<ClipItem>) {
    if !matches!(style.box_decoration_break, BoxDecorationBreak::Slice) {
      return (absolute_rect, None);
    }

    let info = fragment.slice_info;
    if info.is_first && info.is_last {
      return (absolute_rect, None);
    }

    let horizontal_block = block_axis_is_horizontal(style.writing_mode);
    let block_positive = block_axis_positive(style.writing_mode);
    let original_block = if horizontal_block {
      info.original_block_size.max(absolute_rect.width()).max(0.0)
    } else {
      info
        .original_block_size
        .max(absolute_rect.height())
        .max(0.0)
    };
    let slice_offset = info.slice_offset.clamp(0.0, original_block);
    let rect = if horizontal_block {
      let x = if block_positive {
        absolute_rect.x() - slice_offset
      } else {
        absolute_rect.max_x() + slice_offset - original_block
      };
      Rect::from_xywh(x, absolute_rect.y(), original_block, absolute_rect.height())
    } else {
      Rect::from_xywh(
        absolute_rect.x(),
        absolute_rect.y() - slice_offset,
        absolute_rect.width(),
        original_block,
      )
    };
    let clip = ClipItem {
      shape: ClipShape::Rect {
        rect: absolute_rect,
        radii: None,
      },
    };
    (rect, Some(clip))
  }

  fn background_rects(
    rect: Rect,
    style: &ComputedStyle,
    viewport: Option<(f32, f32)>,
  ) -> BackgroundRects {
    let font_size = style.font_size;
    let base = rect.width().max(0.0);

    let border_left = Self::resolve_length_for_paint(
      &style.border_left_width,
      font_size,
      style.root_font_size,
      base,
      viewport,
    );
    let border_right = Self::resolve_length_for_paint(
      &style.border_right_width,
      font_size,
      style.root_font_size,
      base,
      viewport,
    );
    let border_top = Self::resolve_length_for_paint(
      &style.border_top_width,
      font_size,
      style.root_font_size,
      base,
      viewport,
    );
    let border_bottom = Self::resolve_length_for_paint(
      &style.border_bottom_width,
      font_size,
      style.root_font_size,
      base,
      viewport,
    );

    let padding_left = Self::resolve_length_for_paint(
      &style.padding_left,
      font_size,
      style.root_font_size,
      base,
      viewport,
    );
    let padding_right = Self::resolve_length_for_paint(
      &style.padding_right,
      font_size,
      style.root_font_size,
      base,
      viewport,
    );
    let padding_top = Self::resolve_length_for_paint(
      &style.padding_top,
      font_size,
      style.root_font_size,
      base,
      viewport,
    );
    let padding_bottom = Self::resolve_length_for_paint(
      &style.padding_bottom,
      font_size,
      style.root_font_size,
      base,
      viewport,
    );

    let border_rect = rect;
    let padding_rect = Self::inset_rect(
      border_rect,
      border_left,
      border_top,
      border_right,
      border_bottom,
    );
    let content_rect = Self::inset_rect(
      padding_rect,
      padding_left,
      padding_top,
      padding_right,
      padding_bottom,
    );

    BackgroundRects {
      border: border_rect,
      padding: padding_rect,
      content: content_rect,
    }
  }

  fn resolve_mask(&self, style: &ComputedStyle, context_bounds: Rect) -> Option<ResolvedMask> {
    if !style.mask_layers.iter().any(|layer| layer.image.is_some()) {
      return None;
    }

    let rects = Self::background_rects(context_bounds, style, self.viewport);
    let rects = MaskReferenceRects {
      border: rects.border,
      padding: rects.padding,
      content: rects.content,
    };

    let mut layers = Vec::new();
    for layer in &style.mask_layers {
      let Some(image) = &layer.image else { continue };
      let resolved_image = match image {
        BackgroundImage::LinearGradient { .. }
        | BackgroundImage::RepeatingLinearGradient { .. }
        | BackgroundImage::RadialGradient { .. }
        | BackgroundImage::RepeatingRadialGradient { .. }
        | BackgroundImage::ConicGradient { .. }
        | BackgroundImage::RepeatingConicGradient { .. } => {
          ResolvedMaskImage::Generated(Box::new(image.clone()))
        }
        BackgroundImage::Url(src) => {
          let Some(image) = self.decode_image(src, Some(style), true) else {
            continue;
          };
          ResolvedMaskImage::Raster(image)
        }
        BackgroundImage::None => continue,
      };

      layers.push(ResolvedMaskLayer {
        image: resolved_image,
        repeat: layer.repeat,
        position: layer.position.clone(),
        size: layer.size.clone(),
        origin: layer.origin,
        clip: layer.clip,
        mode: layer.mode,
        composite: layer.composite,
      });
    }

    if layers.is_empty() {
      return None;
    }

    Some(ResolvedMask {
      layers,
      color: style.color,
      font_size: style.font_size,
      root_font_size: style.root_font_size,
      rects,
    })
  }

  fn resolve_border_radii(
    style: Option<&ComputedStyle>,
    bounds: Rect,
    viewport: Option<(f32, f32)>,
  ) -> crate::paint::display_list::BorderRadii {
    let Some(style) = style else {
      return crate::paint::display_list::BorderRadii::ZERO;
    };
    let w = bounds.width().max(0.0);
    let h = bounds.height().max(0.0);
    if w <= 0.0 || h <= 0.0 {
      return crate::paint::display_list::BorderRadii::ZERO;
    }

    let resolve_radius = |len: &Length, reference: f32| -> f32 {
      let resolved = Self::resolve_length_for_paint(
        len,
        style.font_size,
        style.root_font_size,
        reference,
        viewport,
      );
      if resolved.is_finite() {
        resolved.max(0.0)
      } else {
        0.0
      }
    };

    crate::paint::display_list::BorderRadii {
      top_left: crate::paint::display_list::BorderRadius {
        x: resolve_radius(&style.border_top_left_radius.x, w),
        y: resolve_radius(&style.border_top_left_radius.y, h),
      },
      top_right: crate::paint::display_list::BorderRadius {
        x: resolve_radius(&style.border_top_right_radius.x, w),
        y: resolve_radius(&style.border_top_right_radius.y, h),
      },
      bottom_right: crate::paint::display_list::BorderRadius {
        x: resolve_radius(&style.border_bottom_right_radius.x, w),
        y: resolve_radius(&style.border_bottom_right_radius.y, h),
      },
      bottom_left: crate::paint::display_list::BorderRadius {
        x: resolve_radius(&style.border_bottom_left_radius.x, w),
        y: resolve_radius(&style.border_bottom_left_radius.y, h),
      },
    }
    .clamped(w, h)
  }

  fn resolve_clip_radii(
    style: &ComputedStyle,
    rects: &BackgroundRects,
    clip: BackgroundBox,
    viewport: Option<(f32, f32)>,
  ) -> crate::paint::display_list::BorderRadii {
    let base = Self::resolve_border_radii(Some(style), rects.border, viewport);
    if base.is_zero() {
      return base;
    }

    let percentage_base = rects.border.width().max(0.0);
    let font_size = style.font_size;
    let border_left = Self::resolve_length_for_paint(
      &style.border_left_width,
      font_size,
      style.root_font_size,
      percentage_base,
      viewport,
    );
    let border_right = Self::resolve_length_for_paint(
      &style.border_right_width,
      font_size,
      style.root_font_size,
      percentage_base,
      viewport,
    );
    let border_top = Self::resolve_length_for_paint(
      &style.border_top_width,
      font_size,
      style.root_font_size,
      percentage_base,
      viewport,
    );
    let border_bottom = Self::resolve_length_for_paint(
      &style.border_bottom_width,
      font_size,
      style.root_font_size,
      percentage_base,
      viewport,
    );

    let padding_left = Self::resolve_length_for_paint(
      &style.padding_left,
      font_size,
      style.root_font_size,
      percentage_base,
      viewport,
    );
    let padding_right = Self::resolve_length_for_paint(
      &style.padding_right,
      font_size,
      style.root_font_size,
      percentage_base,
      viewport,
    );
    let padding_top = Self::resolve_length_for_paint(
      &style.padding_top,
      font_size,
      style.root_font_size,
      percentage_base,
      viewport,
    );
    let padding_bottom = Self::resolve_length_for_paint(
      &style.padding_bottom,
      font_size,
      style.root_font_size,
      percentage_base,
      viewport,
    );

    match clip {
      BackgroundBox::BorderBox => base,
      BackgroundBox::PaddingBox => {
        let shrunk = crate::paint::display_list::BorderRadii {
          top_left: crate::paint::display_list::BorderRadius {
            x: (base.top_left.x - border_left).max(0.0),
            y: (base.top_left.y - border_top).max(0.0),
          },
          top_right: crate::paint::display_list::BorderRadius {
            x: (base.top_right.x - border_right).max(0.0),
            y: (base.top_right.y - border_top).max(0.0),
          },
          bottom_right: crate::paint::display_list::BorderRadius {
            x: (base.bottom_right.x - border_right).max(0.0),
            y: (base.bottom_right.y - border_bottom).max(0.0),
          },
          bottom_left: crate::paint::display_list::BorderRadius {
            x: (base.bottom_left.x - border_left).max(0.0),
            y: (base.bottom_left.y - border_bottom).max(0.0),
          },
        };
        shrunk.clamped(rects.padding.width(), rects.padding.height())
      }
      BackgroundBox::ContentBox => {
        let shrink_left = border_left + padding_left;
        let shrink_right = border_right + padding_right;
        let shrink_top = border_top + padding_top;
        let shrink_bottom = border_bottom + padding_bottom;
        let shrunk = crate::paint::display_list::BorderRadii {
          top_left: crate::paint::display_list::BorderRadius {
            x: (base.top_left.x - shrink_left).max(0.0),
            y: (base.top_left.y - shrink_top).max(0.0),
          },
          top_right: crate::paint::display_list::BorderRadius {
            x: (base.top_right.x - shrink_right).max(0.0),
            y: (base.top_right.y - shrink_top).max(0.0),
          },
          bottom_right: crate::paint::display_list::BorderRadius {
            x: (base.bottom_right.x - shrink_right).max(0.0),
            y: (base.bottom_right.y - shrink_bottom).max(0.0),
          },
          bottom_left: crate::paint::display_list::BorderRadius {
            x: (base.bottom_left.x - shrink_left).max(0.0),
            y: (base.bottom_left.y - shrink_bottom).max(0.0),
          },
        };
        shrunk.clamped(rects.content.width(), rects.content.height())
      }
    }
  }

  fn normalize_color_stops(stops: &[ColorStop], current_color: Rgba) -> Vec<(f32, Rgba)> {
    if stops.is_empty() {
      return Vec::new();
    }

    let mut positions: Vec<Option<f32>> = stops.iter().map(|s| s.position).collect();
    if positions.iter().all(|p| p.is_none()) {
      if stops.len() == 1 {
        return vec![(0.0, stops[0].color.to_rgba(current_color))];
      }
      let denom = (stops.len() - 1) as f32;
      return stops
        .iter()
        .enumerate()
        .map(|(i, s)| (i as f32 / denom, s.color.to_rgba(current_color)))
        .collect();
    }

    if positions.first().and_then(|p| *p).is_none() {
      positions[0] = Some(0.0);
    }
    if positions.last().and_then(|p| *p).is_none() {
      if let Some(last) = positions.last_mut() {
        *last = Some(1.0);
      }
    }

    let mut last_known: Option<(usize, f32)> = None;
    for i in 0..positions.len() {
      if let Some(pos) = positions[i] {
        if let Some((start_idx, start_pos)) = last_known {
          let gap = i.saturating_sub(start_idx + 1);
          if gap > 0 {
            let step = (pos - start_pos) / (gap as f32 + 1.0);
            for (offset, slot) in positions[start_idx + 1..i].iter_mut().enumerate() {
              *slot = Some((start_pos + step * (offset + 1) as f32).max(start_pos));
            }
          }
        } else if i > 0 {
          let gap = i;
          let step = pos / gap as f32;
          for (j, slot) in positions.iter_mut().take(i).enumerate() {
            *slot = Some(step * j as f32);
          }
        }
        last_known = Some((i, pos));
      }
    }

    let mut output = Vec::with_capacity(stops.len());
    let mut prev = 0.0;
    for (idx, pos_opt) in positions.into_iter().enumerate() {
      let pos = pos_opt.unwrap_or(prev);
      let clamped = pos.max(prev).clamp(0.0, 1.0);
      prev = clamped;
      output.push((clamped, stops[idx].color.to_rgba(current_color)));
    }

    output
  }

  fn normalize_color_stops_unclamped(stops: &[ColorStop], current_color: Rgba) -> Vec<(f32, Rgba)> {
    if stops.is_empty() {
      return Vec::new();
    }

    let mut positions: Vec<Option<f32>> = stops.iter().map(|s| s.position).collect();
    if positions.iter().all(|p| p.is_none()) {
      if stops.len() == 1 {
        return vec![(0.0, stops[0].color.to_rgba(current_color))];
      }
      let denom = (stops.len() - 1) as f32;
      return stops
        .iter()
        .enumerate()
        .map(|(i, s)| (i as f32 / denom, s.color.to_rgba(current_color)))
        .collect();
    }

    if positions.first().and_then(|p| *p).is_none() {
      positions[0] = Some(0.0);
    }
    if positions.last().and_then(|p| *p).is_none() {
      if let Some(last) = positions.last_mut() {
        *last = Some(1.0);
      }
    }

    let mut last_known: Option<(usize, f32)> = None;
    for i in 0..positions.len() {
      if let Some(pos) = positions[i] {
        if let Some((start_idx, start_pos)) = last_known {
          let gap = i.saturating_sub(start_idx + 1);
          if gap > 0 {
            let step = (pos - start_pos) / (gap as f32 + 1.0);
            for (offset, slot) in positions[start_idx + 1..i].iter_mut().enumerate() {
              *slot = Some(start_pos + step * (offset + 1) as f32);
            }
          }
        } else if i > 0 {
          let gap = i;
          let step = pos / gap as f32;
          for (j, slot) in positions.iter_mut().take(i).enumerate() {
            *slot = Some(step * j as f32);
          }
        }
        last_known = Some((i, pos));
      }
    }

    let mut output = Vec::with_capacity(stops.len());
    let mut prev = 0.0;
    for (idx, pos_opt) in positions.into_iter().enumerate() {
      let pos = pos_opt.unwrap_or(prev);
      let monotonic = pos.max(prev);
      prev = monotonic;
      output.push((monotonic, stops[idx].color.to_rgba(current_color)));
    }
    output
  }

  fn gradient_stops(stops: &[(f32, Rgba)]) -> Vec<GradientStop> {
    stops
      .iter()
      .map(|(pos, color)| GradientStop {
        position: pos.clamp(0.0, 1.0),
        color: *color,
      })
      .collect()
  }

  fn gradient_stops_unclamped(stops: &[(f32, Rgba)]) -> Vec<GradientStop> {
    stops
      .iter()
      .map(|(pos, color)| GradientStop {
        position: *pos,
        color: *color,
      })
      .collect()
  }

  fn radial_geometry(
    rect: Rect,
    position: &BackgroundPosition,
    size: &RadialGradientSize,
    shape: RadialGradientShape,
    font_size: f32,
    root_font_size: f32,
    viewport: Option<(f32, f32)>,
  ) -> (f32, f32, f32, f32) {
    let (align_x, off_x, align_y, off_y) = match position {
      BackgroundPosition::Position { x, y } => {
        let ox = Self::resolve_length_for_paint(
          &x.offset,
          font_size,
          root_font_size,
          rect.width(),
          viewport,
        );
        let oy = Self::resolve_length_for_paint(
          &y.offset,
          font_size,
          root_font_size,
          rect.height(),
          viewport,
        );
        (x.alignment, ox, y.alignment, oy)
      }
    };
    let cx = rect.x() + align_x * rect.width() + off_x;
    let cy = rect.y() + align_y * rect.height() + off_y;

    let dx_left = (cx - rect.x()).max(0.0);
    let dx_right = (rect.x() + rect.width() - cx).max(0.0);
    let dy_top = (cy - rect.y()).max(0.0);
    let dy_bottom = (rect.y() + rect.height() - cy).max(0.0);

    let (mut rx, mut ry) = match size {
      RadialGradientSize::ClosestSide => (dx_left.min(dx_right), dy_top.min(dy_bottom)),
      RadialGradientSize::FarthestSide => (dx_left.max(dx_right), dy_top.max(dy_bottom)),
      RadialGradientSize::ClosestCorner => {
        let corners = [
          (dx_left, dy_top),
          (dx_left, dy_bottom),
          (dx_right, dy_top),
          (dx_right, dy_bottom),
        ];
        let mut best = f32::INFINITY;
        let mut best_pair = (0.0, 0.0);
        for (dx, dy) in corners {
          let dist = (dx * dx + dy * dy).sqrt();
          if dist < best {
            best = dist;
            best_pair = (dx, dy);
          }
        }
        (
          best_pair.0 * std::f32::consts::SQRT_2,
          best_pair.1 * std::f32::consts::SQRT_2,
        )
      }
      RadialGradientSize::FarthestCorner => {
        let corners = [
          (dx_left, dy_top),
          (dx_left, dy_bottom),
          (dx_right, dy_top),
          (dx_right, dy_bottom),
        ];
        let mut best = -f32::INFINITY;
        let mut best_pair = (0.0, 0.0);
        for (dx, dy) in corners {
          let dist = (dx * dx + dy * dy).sqrt();
          if dist > best {
            best = dist;
            best_pair = (dx, dy);
          }
        }
        (
          best_pair.0 * std::f32::consts::SQRT_2,
          best_pair.1 * std::f32::consts::SQRT_2,
        )
      }
      RadialGradientSize::Explicit { x, y } => {
        let rx =
          Self::resolve_length_for_paint(x, font_size, root_font_size, rect.width(), viewport)
            .max(0.0);
        let ry = y
          .as_ref()
          .map(|yy| {
            Self::resolve_length_for_paint(yy, font_size, root_font_size, rect.height(), viewport)
              .max(0.0)
          })
          .unwrap_or(rx);
        (rx, ry)
      }
    };

    if matches!(shape, RadialGradientShape::Circle) {
      let r = if matches!(
        size,
        RadialGradientSize::ClosestCorner | RadialGradientSize::FarthestCorner
      ) {
        let avg = (rx * rx + ry * ry) / 2.0;
        avg.sqrt()
      } else {
        rx.min(ry)
      };
      rx = r;
      ry = r;
    }

    (cx, cy, rx, ry)
  }

  fn resolve_gradient_center(
    rect: Rect,
    position: &BackgroundPosition,
    font_size: f32,
    root_font_size: f32,
    viewport: Option<(f32, f32)>,
  ) -> Point {
    let (align_x, off_x, align_y, off_y) = match position {
      BackgroundPosition::Position { x, y } => {
        let ox = Self::resolve_length_for_paint(
          &x.offset,
          font_size,
          root_font_size,
          rect.width(),
          viewport,
        );
        let oy = Self::resolve_length_for_paint(
          &y.offset,
          font_size,
          root_font_size,
          rect.height(),
          viewport,
        );
        (x.alignment, ox, y.alignment, oy)
      }
    };
    let cx = rect.x() + align_x * rect.width() + off_x;
    let cy = rect.y() + align_y * rect.height() + off_y;
    Point::new(cx, cy)
  }

  fn resolve_filters(
    filters: &[crate::style::types::FilterFunction],
    style: &ComputedStyle,
    viewport: Option<(f32, f32)>,
    font_ctx: &FontContext,
    svg_filters: &mut SvgFilterResolver,
  ) -> Vec<ResolvedFilter> {
    let viewport = viewport.unwrap_or((0.0, 0.0));
    filters
      .iter()
      .filter_map(|f| match f {
        crate::style::types::FilterFunction::Blur(len) => {
          let radius = Self::resolve_filter_length(len, style, viewport, font_ctx)?;
          (radius >= 0.0).then_some(ResolvedFilter::Blur(radius))
        }
        crate::style::types::FilterFunction::Brightness(v) => {
          Some(ResolvedFilter::Brightness((*v).max(0.0)))
        }
        crate::style::types::FilterFunction::Contrast(v) => {
          Some(ResolvedFilter::Contrast((*v).max(0.0)))
        }
        crate::style::types::FilterFunction::Grayscale(v) => {
          Some(ResolvedFilter::Grayscale(v.clamp(0.0, 1.0)))
        }
        crate::style::types::FilterFunction::Sepia(v) => {
          Some(ResolvedFilter::Sepia(v.clamp(0.0, 1.0)))
        }
        crate::style::types::FilterFunction::Saturate(v) => {
          Some(ResolvedFilter::Saturate((*v).max(0.0)))
        }
        crate::style::types::FilterFunction::HueRotate(deg) => {
          Some(ResolvedFilter::HueRotate(*deg))
        }
        crate::style::types::FilterFunction::Invert(v) => {
          Some(ResolvedFilter::Invert(v.clamp(0.0, 1.0)))
        }
        crate::style::types::FilterFunction::Opacity(v) => {
          Some(ResolvedFilter::Opacity(v.clamp(0.0, 1.0)))
        }
        crate::style::types::FilterFunction::DropShadow(shadow) => {
          let color = match shadow.color {
            crate::style::types::FilterColor::CurrentColor => style.color,
            crate::style::types::FilterColor::Color(c) => c,
          };
          let offset_x = Self::resolve_filter_length(&shadow.offset_x, style, viewport, font_ctx)?;
          let offset_y = Self::resolve_filter_length(&shadow.offset_y, style, viewport, font_ctx)?;
          let blur_radius =
            Self::resolve_filter_length(&shadow.blur_radius, style, viewport, font_ctx)?;
          if blur_radius < 0.0 {
            return None;
          }
          let spread = Self::resolve_filter_length(&shadow.spread, style, viewport, font_ctx)?;
          Some(ResolvedFilter::DropShadow {
            offset_x,
            offset_y,
            blur_radius,
            spread,
            color,
          })
        }
        crate::style::types::FilterFunction::Url(url) => {
          svg_filters.resolve(url).map(ResolvedFilter::SvgFilter)
        }
      })
      .collect()
  }

  fn resolve_filter_length(
    len: &Length,
    style: &ComputedStyle,
    viewport: (f32, f32),
    font_ctx: &FontContext,
  ) -> Option<f32> {
    let resolved = match len.unit {
      LengthUnit::Percent => None,
      unit if unit.is_font_relative() => Some(resolve_font_relative_length(*len, style, font_ctx)),
      unit if unit.is_viewport_relative() => len.resolve_with_viewport(viewport.0, viewport.1),
      unit if unit.is_absolute() => Some(len.to_px()),
      _ => None,
    }?;
    if resolved.is_finite() {
      Some(resolved)
    } else {
      None
    }
  }

  fn compute_background_size(
    layer: &BackgroundLayer,
    font_size: f32,
    root_font_size: f32,
    viewport: Option<(f32, f32)>,
    area_w: f32,
    area_h: f32,
    img_w: f32,
    img_h: f32,
  ) -> (f32, f32) {
    let natural_w = if img_w > 0.0 { Some(img_w) } else { None };
    let natural_h = if img_h > 0.0 { Some(img_h) } else { None };
    let ratio = if img_w > 0.0 && img_h > 0.0 {
      Some(img_w / img_h)
    } else {
      None
    };

    match layer.size {
      BackgroundSize::Keyword(BackgroundSizeKeyword::Cover) => {
        if let (Some(w), Some(h)) = (natural_w, natural_h) {
          let scale = (area_w / w).max(area_h / h);
          (w * scale, h * scale)
        } else {
          (area_w.max(0.0), area_h.max(0.0))
        }
      }
      BackgroundSize::Keyword(BackgroundSizeKeyword::Contain) => {
        if let (Some(w), Some(h)) = (natural_w, natural_h) {
          let scale = (area_w / w).min(area_h / h);
          (w * scale, h * scale)
        } else {
          (area_w.max(0.0), area_h.max(0.0))
        }
      }
      BackgroundSize::Explicit(x, y) => {
        let resolve = |component: BackgroundSizeComponent, area: f32| -> Option<f32> {
          match component {
            BackgroundSizeComponent::Auto => None,
            BackgroundSizeComponent::Length(len) => {
              let (vw, vh) = viewport.unwrap_or((area_w, area_h));
              len
                .resolve_with_context(Some(area), vw, vh, font_size, root_font_size)
                .or_else(|| {
                  if len.unit.is_absolute() {
                    Some(len.to_px())
                  } else {
                    None
                  }
                })
                .map(|v| v.max(0.0))
            }
          }
        };

        let resolved_x = resolve(x, area_w);
        let resolved_y = resolve(y, area_h);

        match (resolved_x, resolved_y) {
          (Some(w), Some(h)) => (w, h),
          (Some(w), None) => {
            if let Some(r) = ratio {
              (w, (w / r).max(0.0))
            } else if let Some(h) = natural_h {
              (w, h)
            } else {
              (w, area_h.max(0.0))
            }
          }
          (None, Some(h)) => {
            if let Some(r) = ratio {
              ((h * r).max(0.0), h)
            } else if let Some(w) = natural_w {
              (w, h)
            } else {
              (area_w.max(0.0), h)
            }
          }
          (None, None) => {
            if let (Some(w), Some(h)) = (natural_w, natural_h) {
              (w, h)
            } else {
              (area_w.max(0.0), area_h.max(0.0))
            }
          }
        }
      }
    }
  }

  fn resolve_background_offset(
    pos: BackgroundPosition,
    area_w: f32,
    area_h: f32,
    tile_w: f32,
    tile_h: f32,
    font_size: f32,
    root_font_size: f32,
    viewport: Option<(f32, f32)>,
  ) -> (f32, f32) {
    let resolve_axis =
      |comp: crate::style::types::BackgroundPositionComponent, area: f32, tile: f32| -> f32 {
        let available = area - tile;
        let needs_viewport = comp.offset.unit.is_viewport_relative()
          || comp
            .offset
            .calc
            .as_ref()
            .map(|c| c.has_viewport_relative())
            .unwrap_or(false);
        let (vw, vh) = match viewport {
          Some(vp) => vp,
          None if needs_viewport => (f32::NAN, f32::NAN),
          None => (0.0, 0.0),
        };
        let offset = comp
          .offset
          .resolve_with_context(Some(available), vw, vh, font_size, root_font_size)
          .unwrap_or_else(|| {
            if comp.offset.unit.is_absolute() {
              comp.offset.to_px()
            } else {
              0.0
            }
          });
        comp.alignment * available + offset
      };

    match pos {
      BackgroundPosition::Position { x, y } => {
        let x = resolve_axis(x, area_w, tile_w);
        let y = resolve_axis(y, area_h, tile_h);
        (x, y)
      }
    }
  }

  fn aligned_start(origin: f32, tile: f32, clip_min: f32) -> f32 {
    if tile == 0.0 {
      return origin;
    }
    let steps = ((clip_min - origin) / tile).floor();
    origin + steps * tile
  }

  fn round_tile_length(area_len: f32, tile_len: f32) -> f32 {
    if tile_len == 0.0 {
      return 0.0;
    }
    let count = (area_len / tile_len).round().max(1.0);
    area_len / count
  }

  fn tile_positions(
    repeat: BackgroundRepeatKeyword,
    area_start: f32,
    area_len: f32,
    tile_len: f32,
    offset: f32,
    clip_min: f32,
    clip_max: f32,
  ) -> Vec<f32> {
    if tile_len <= 0.0 {
      return Vec::new();
    }

    match repeat {
      BackgroundRepeatKeyword::NoRepeat => vec![area_start + offset],
      BackgroundRepeatKeyword::Repeat | BackgroundRepeatKeyword::Round => {
        let start = Self::aligned_start(area_start + offset, tile_len, clip_min);
        let mut positions = Vec::new();
        let mut pos = start;
        while pos < clip_max {
          positions.push(pos);
          pos += tile_len;
        }
        positions
      }
      BackgroundRepeatKeyword::Space => {
        let count = (area_len / tile_len).floor() as i32;
        if count >= 2 {
          let spacing = (area_len - tile_len * count as f32) / (count as f32 - 1.0);
          let step = tile_len + spacing;
          let anchor = area_start;
          let mut positions = Vec::new();
          let k = ((clip_min - anchor) / step).floor();
          let mut pos = anchor + k * step;
          while pos < clip_max {
            positions.push(pos);
            pos += step;
          }
          positions
        } else {
          let centered = area_start + offset + (area_len - tile_len) * 0.5;
          vec![centered]
        }
      }
    }
  }

  fn resolve_length_for_paint(
    len: &Length,
    font_size: f32,
    root_font_size: f32,
    percentage_base: f32,
    viewport: Option<(f32, f32)>,
  ) -> f32 {
    let needs_viewport = len.unit.is_viewport_relative()
      || len
        .calc
        .as_ref()
        .map(|c| c.has_viewport_relative())
        .unwrap_or(false);
    let (vw, vh) = match viewport {
      Some(vp) => vp,
      None if needs_viewport => (f32::NAN, f32::NAN),
      None => (percentage_base, percentage_base),
    };
    let resolved = len
      .resolve_with_context(Some(percentage_base), vw, vh, font_size, root_font_size)
      .unwrap_or_else(|| {
        if len.unit.is_absolute() {
          len.to_px()
        } else {
          len.value * font_size
        }
      });
    if resolved.is_finite() {
      resolved
    } else {
      0.0
    }
  }

  fn inset_rect(rect: Rect, left: f32, top: f32, right: f32, bottom: f32) -> Rect {
    let new_x = rect.x() + left;
    let new_y = rect.y() + top;
    let new_w = (rect.width() - left - right).max(0.0);
    let new_h = (rect.height() - top - bottom).max(0.0);
    Rect::from_xywh(new_x, new_y, new_w, new_h)
  }

  fn border_radii(rect: Rect, style: &ComputedStyle) -> crate::paint::display_list::BorderRadii {
    Self::resolve_border_radii(Some(style), rect, None)
  }

  fn border_style_visible(style: crate::style::types::BorderStyle) -> bool {
    !matches!(
      style,
      crate::style::types::BorderStyle::None | crate::style::types::BorderStyle::Hidden
    )
  }

  fn border_side_visible(side: &BorderSide) -> bool {
    side.width > 0.0 && Self::border_style_visible(side.style) && !side.color.is_transparent()
  }

  fn transform_reference_box(
    style: &ComputedStyle,
    bounds: Rect,
    viewport: Option<(f32, f32)>,
  ) -> Rect {
    let rects = Self::background_rects(bounds, style, viewport);
    match style.transform_box {
      TransformBox::ContentBox => rects.content,
      TransformBox::BorderBox
      | TransformBox::FillBox
      | TransformBox::StrokeBox
      | TransformBox::ViewBox => rects.border,
    }
  }

  fn build_transform(
    style: &ComputedStyle,
    bounds: Rect,
    viewport: Option<(f32, f32)>,
  ) -> ResolvedTransforms {
    crate::paint::transform_resolver::resolve_transforms(style, bounds, viewport)
  }

  /// Exposes transform resolution for debugging/inspection tools.
  pub fn debug_resolve_transform(
    style: &ComputedStyle,
    bounds: Rect,
    viewport: Option<(f32, f32)>,
  ) -> Option<Transform3D> {
    crate::paint::transform_resolver::resolve_transform3d(style, bounds, viewport)
  }

  fn emit_fragment_list(&mut self, fragments: &[FragmentNode], offset: Point) {
    if self.should_parallelize(fragments.len()) {
      let mut partials: Vec<(usize, DisplayList)> = fragments
        .par_iter()
        .enumerate()
        .map(|(idx, fragment)| {
          let mut builder = self.fork();
          builder.build_fragment(fragment, offset);
          (idx, builder.list)
        })
        .collect();
      partials.sort_by_key(|(idx, _)| *idx);
      for (_, list) in partials {
        self.list.append(list);
      }
      return;
    }

    for fragment in fragments {
      self.build_fragment(fragment, offset);
    }
  }

  fn emit_fragment_list_shallow(
    &mut self,
    fragments: &[FragmentNode],
    offset: Point,
    suppress_opacity: bool,
  ) {
    if self.should_parallelize(fragments.len()) {
      let mut partials: Vec<(usize, DisplayList)> = fragments
        .par_iter()
        .enumerate()
        .map(|(idx, fragment)| {
          let mut builder = self.fork();
          if suppress_opacity {
            builder.build_fragment_internal(fragment, offset, false, true);
          } else {
            builder.build_fragment_shallow(fragment, offset);
          }
          (idx, builder.list)
        })
        .collect();
      partials.sort_by_key(|(idx, _)| *idx);
      for (_, list) in partials {
        self.list.append(list);
      }
      return;
    }

    for fragment in fragments {
      if suppress_opacity {
        self.build_fragment_internal(fragment, offset, false, true);
      } else {
        self.build_fragment_shallow(fragment, offset);
      }
    }
  }

  fn should_parallelize(&self, len: usize) -> bool {
    self.parallel_enabled && len >= self.parallel_min && rayon::current_num_threads() > 1
  }

  fn fork(&self) -> DisplayListBuilder {
    DisplayListBuilder {
      list: DisplayList::new(),
      image_cache: self.image_cache.clone(),
      svg_filter_defs: self.svg_filter_defs.clone(),
      viewport: self.viewport,
      font_ctx: self.font_ctx.clone(),
      shaper: self.shaper.clone(),
      device_pixel_ratio: self.device_pixel_ratio,
      parallel_enabled: self.parallel_enabled,
      parallel_min: self.parallel_min,
    }
  }

  /// Emits display items for fragment content
  fn emit_content(&mut self, fragment: &FragmentNode, rect: Rect) {
    match &fragment.content {
      FragmentContent::Text {
        text,
        baseline_offset,
        shaped,
        is_marker,
        ..
      } => {
        if text.is_empty() {
          return;
        }

        let style_opt = fragment.style.as_deref();
        let color = style_opt.map(|s| s.color).unwrap_or(Rgba::BLACK);
        let shadows = Self::text_shadows_from_style(style_opt);
        let inline_vertical = style_opt.is_some_and(|s| {
          matches!(
            s.writing_mode,
            crate::style::types::WritingMode::VerticalRl
              | crate::style::types::WritingMode::VerticalLr
              | crate::style::types::WritingMode::SidewaysRl
              | crate::style::types::WritingMode::SidewaysLr
          )
        });
        let (baseline_block, baseline_inline) = if inline_vertical {
          (rect.origin.x + baseline_offset, rect.origin.y)
        } else {
          (rect.origin.x, rect.origin.y + baseline_offset)
        };

        let mut shaped_storage: Option<Vec<ShapedRun>> = None;
        let runs_ref: Option<&[ShapedRun]> = if let Some(runs) = shaped {
          Some(runs.as_slice())
        } else if let Some(style) = style_opt {
          if let Ok(mut runs) = self.shaper.shape(text, style, &self.font_ctx) {
            InlineTextItem::apply_spacing_to_runs(
              &mut runs,
              text,
              style.letter_spacing,
              style.word_spacing,
            );
            shaped_storage = Some(runs);
          }
          shaped_storage.as_deref()
        } else {
          None
        };

        if let Some(runs) = runs_ref {
          if inline_vertical {
            if *is_marker {
              self.emit_list_marker_runs_vertical(
                runs,
                color,
                baseline_block,
                baseline_inline,
                &shadows,
                style_opt,
              );
            } else {
              self.emit_shaped_runs_vertical(
                runs,
                color,
                baseline_block,
                baseline_inline,
                &shadows,
                style_opt,
              );
            }
          } else if *is_marker {
            self.emit_list_marker_runs(
              runs,
              color,
              baseline_inline,
              baseline_block,
              &shadows,
              style_opt,
              inline_vertical,
            );
          } else {
            self.emit_shaped_runs(
              runs,
              color,
              baseline_inline,
              baseline_block,
              &shadows,
              style_opt,
              inline_vertical,
            );
          }
        } else {
          // Fallback: naive glyphs when shaping fails or no style is present
          let font_size = style_opt.map(|s| s.font_size).unwrap_or(16.0);
          let char_width = font_size * 0.6;
          let glyphs: Vec<GlyphInstance> = if inline_vertical {
            text
              .chars()
              .enumerate()
              .map(|(i, _c)| GlyphInstance {
                glyph_id: i as u32,
                offset: Point::new(0.0, i as f32 * char_width),
                advance: 0.0,
              })
              .collect()
          } else {
            text
              .chars()
              .enumerate()
              .map(|(i, _c)| GlyphInstance {
                glyph_id: i as u32,
                offset: Point::new(i as f32 * char_width, 0.0),
                advance: char_width,
              })
              .collect()
          };
          let advance_width = if inline_vertical {
            0.0
          } else {
            text.len() as f32 * char_width
          };
          let origin = if inline_vertical {
            Point::new(baseline_block, baseline_inline)
          } else {
            Point::new(baseline_block, baseline_inline)
          };

          if *is_marker {
            self.list.push(DisplayItem::ListMarker(ListMarkerItem {
              origin,
              glyphs,
              color,
              shadows: shadows.clone(),
              font_size,
              advance_width,
              font_id: None,
              synthetic_bold: 0.0,
              synthetic_oblique: 0.0,
              emphasis: None,
              background: None,
            }));
          } else {
            self.list.push(DisplayItem::Text(TextItem {
              origin,
              glyphs,
              color,
              shadows: shadows.clone(),
              font_size,
              advance_width,
              font_id: None,
              synthetic_bold: 0.0,
              synthetic_oblique: 0.0,
              emphasis: None,
              decorations: Vec::new(),
            }));
          }
        }

        if let Some(style) = style_opt {
          let (inline_start, inline_len) = if inline_vertical {
            (rect.y(), rect.height())
          } else {
            (rect.x(), rect.width())
          };
          let decoration_baseline = baseline_block;
          self.emit_text_decorations(
            style,
            runs_ref,
            inline_start,
            inline_len,
            decoration_baseline,
            inline_vertical,
          );
        }
      }

      FragmentContent::Replaced { replaced_type, .. } => {
        if let ReplacedType::FormControl(control) = replaced_type {
          if self.emit_form_control(control, fragment, rect) {
            return;
          }
        }

        if let ReplacedType::Math(math) = replaced_type {
          let fallback_style = ComputedStyle::default();
          let style_ref = fragment.style.as_deref().unwrap_or(&fallback_style);
          let layout_owned = math
            .layout
            .as_ref()
            .map(|l| l.as_ref().clone())
            .unwrap_or_else(|| layout_mathml(&math.root, style_ref, &self.font_ctx));
          let color = style_ref.color;
          let shadows = Self::text_shadows_from_style(Some(style_ref));
          let layout_w = layout_owned.width.max(0.01);
          let layout_h = layout_owned.height.max(0.01);
          let scale_x = if layout_w > 0.0 {
            rect.width() / layout_w
          } else {
            1.0
          };
          let scale_y = if layout_h > 0.0 {
            rect.height() / layout_h
          } else {
            1.0
          };
          for frag in layout_owned.fragments {
            match frag {
              MathFragment::Glyph { origin, run } => {
                let scaled_run = Self::scale_run(&run, scale_x, scale_y);
                let baseline_y = rect.y() + origin.y * scale_y;
                let start_x = rect.x() + origin.x * scale_x;
                self.emit_shaped_runs(
                  &[scaled_run],
                  color,
                  baseline_y,
                  start_x,
                  &shadows,
                  Some(style_ref),
                  false,
                );
              }
              MathFragment::Rule(r) => {
                let scaled_rect = Rect::from_xywh(
                  rect.x() + r.x() * scale_x,
                  rect.y() + r.y() * scale_y,
                  r.width() * scale_x,
                  r.height() * scale_y,
                );
                self.list.push(DisplayItem::FillRect(FillRectItem {
                  rect: scaled_rect,
                  color,
                }));
              }
              MathFragment::StrokeRect {
                rect: stroke_rect,
                radius,
                width,
              } => {
                let scaled_rect = Rect::from_xywh(
                  rect.x() + stroke_rect.x() * scale_x,
                  rect.y() + stroke_rect.y() * scale_y,
                  stroke_rect.width() * scale_x,
                  stroke_rect.height() * scale_y,
                );
                let uniform = ((scale_x + scale_y) * 0.5).max(0.0);
                let stroke_width = width * uniform;
                let scaled_radius = radius * uniform;
                if scaled_radius > 0.0 {
                  self
                    .list
                    .push(DisplayItem::StrokeRoundedRect(StrokeRoundedRectItem {
                      rect: scaled_rect,
                      color,
                      width: stroke_width,
                      radii: BorderRadii::uniform(scaled_radius),
                    }));
                } else {
                  self.list.push(DisplayItem::StrokeRect(StrokeRectItem {
                    rect: scaled_rect,
                    color,
                    width: stroke_width,
                    blend_mode: BlendMode::Normal,
                  }));
                }
              }
            }
          }
          return;
        }

        let media_ctx = self.viewport.map(|(w, h)| {
          crate::style::media::MediaContext::screen(w, h)
            .with_device_pixel_ratio(self.device_pixel_ratio)
            .with_env_overrides()
        });
        let style_for_image = fragment.style.as_deref();
        let cache_base = self.image_cache.as_ref().and_then(|cache| cache.base_url());
        let sources =
          replaced_type.image_sources_with_fallback(crate::tree::box_tree::ImageSelectionContext {
            scale: self.device_pixel_ratio,
            slot_width: Some(rect.width()),
            viewport: self.viewport.map(|(w, h)| crate::geometry::Size::new(w, h)),
            media_context: media_ctx.as_ref(),
            font_size: fragment.style.as_deref().map(|s| s.font_size),
            base_url: cache_base.as_deref(),
          });

        if let Some(image) = sources
          .iter()
          .find_map(|s| self.decode_image(s, style_for_image, false))
        {
          let (dest_x, dest_y, dest_w, dest_h) = {
            let (fit, position, font_size) = if let Some(style) = fragment.style.as_deref() {
              (style.object_fit, style.object_position, style.font_size)
            } else {
              (ObjectFit::Fill, default_object_position(), 16.0)
            };

            compute_object_fit(
              fit,
              position,
              rect.width(),
              rect.height(),
              image.css_width,
              image.css_height,
              font_size,
              self.viewport,
            )
            .unwrap_or_else(|| (0.0, 0.0, rect.width(), rect.height()))
          };

          let dest_rect = Rect::from_xywh(rect.x() + dest_x, rect.y() + dest_y, dest_w, dest_h);
          self.list.push(DisplayItem::Image(ImageItem {
            dest_rect,
            image: Arc::new(image),
            filter_quality: Self::image_filter_quality(fragment.style.as_deref()),
            src_rect: None,
          }));
          return;
        }

        if let ReplacedType::Image { alt: Some(alt), .. } = replaced_type {
          if self.emit_alt_text(alt, fragment, rect) {
            return;
          }
        }

        self.emit_replaced_placeholder(replaced_type, fragment, rect);
      }

      // Block, Inline, Line, other replaced types - no direct content
      _ => {}
    }
  }

  /// Gets the box_id from a fragment
  fn get_box_id(fragment: &FragmentNode) -> Option<usize> {
    match &fragment.content {
      FragmentContent::Block { box_id } => *box_id,
      FragmentContent::Inline { box_id, .. } => *box_id,
      FragmentContent::Text { box_id, .. } => *box_id,
      FragmentContent::Replaced { box_id, .. } => *box_id,
      FragmentContent::RunningAnchor { .. } => None,
      FragmentContent::Line { .. } => None,
    }
  }

  /// Emits a background fill for a fragment
  pub fn emit_background(&mut self, rect: Rect, color: Rgba) {
    if !color.is_transparent() {
      self
        .list
        .push(DisplayItem::FillRect(FillRectItem { rect, color }));
    }
  }

  /// Emits border strokes for a fragment
  pub fn emit_border(&mut self, rect: Rect, width: f32, color: Rgba) {
    if width > 0.0 && !color.is_transparent() {
      self.list.push(DisplayItem::StrokeRect(StrokeRectItem {
        rect,
        color,
        width,
        blend_mode: BlendMode::Normal,
      }));
    }
  }

  fn emit_background_from_style(&mut self, rect: Rect, style: &ComputedStyle) {
    let has_images = style.background_layers.iter().any(|l| l.image.is_some());
    if style.background_color.is_transparent() && !has_images {
      return;
    }

    let rects = Self::background_rects(rect, style, self.viewport);
    let fallback = BackgroundLayer::default();
    let color_layer = style.background_layers.first().unwrap_or(&fallback);
    let color_clip_rect = match color_layer.clip {
      BackgroundBox::BorderBox => rects.border,
      BackgroundBox::PaddingBox => rects.padding,
      BackgroundBox::ContentBox => rects.content,
    };
    if style.background_color.alpha_u8() > 0
      && color_clip_rect.width() > 0.0
      && color_clip_rect.height() > 0.0
    {
      let radii = Self::resolve_clip_radii(style, &rects, color_layer.clip, self.viewport);
      if radii.is_zero() {
        self.emit_background(color_clip_rect, style.background_color);
      } else {
        self.list.push(DisplayItem::FillRoundedRect(
          crate::paint::display_list::FillRoundedRectItem {
            rect: color_clip_rect,
            color: style.background_color,
            radii,
          },
        ));
      }
    }

    for layer in style.background_layers.iter().rev() {
      if let Some(image) = &layer.image {
        self.emit_background_layer(&rects, style, layer, image);
      }
    }
  }

  fn emit_background_layer(
    &mut self,
    rects: &BackgroundRects,
    style: &ComputedStyle,
    layer: &BackgroundLayer,
    bg: &BackgroundImage,
  ) {
    let is_local = layer.attachment == BackgroundAttachment::Local;
    let clip_box = if is_local {
      match layer.clip {
        BackgroundBox::ContentBox => BackgroundBox::ContentBox,
        _ => BackgroundBox::PaddingBox,
      }
    } else {
      layer.clip
    };
    let clip_rect = match clip_box {
      BackgroundBox::BorderBox => rects.border,
      BackgroundBox::PaddingBox => rects.padding,
      BackgroundBox::ContentBox => rects.content,
    };
    let origin_rect = if layer.attachment == BackgroundAttachment::Fixed {
      if let Some((w, h)) = self.viewport {
        Rect::from_xywh(0.0, 0.0, w, h)
      } else {
        rects.border
      }
    } else if is_local {
      match layer.origin {
        BackgroundBox::ContentBox => rects.content,
        _ => rects.padding,
      }
    } else {
      match layer.origin {
        BackgroundBox::BorderBox => rects.border,
        BackgroundBox::PaddingBox => rects.padding,
        BackgroundBox::ContentBox => rects.content,
      }
    };

    if clip_rect.width() <= 0.0
      || clip_rect.height() <= 0.0
      || origin_rect.width() <= 0.0
      || origin_rect.height() <= 0.0
    {
      return;
    }

    let clip_radii = Self::resolve_clip_radii(style, rects, clip_box, self.viewport);
    let blend_mode = Self::convert_blend_mode(layer.blend_mode);
    let use_blend = blend_mode != BlendMode::Normal;
    let pushed_clip = !clip_radii.is_zero() && {
      self.list.push(DisplayItem::PushClip(ClipItem {
        shape: ClipShape::Rect {
          rect: clip_rect,
          radii: Some(clip_radii),
        },
      }));
      true
    };
    if use_blend {
      self.list.push(DisplayItem::PushBlendMode(BlendModeItem {
        mode: blend_mode,
      }));
    }

    let compute_tiles = |img_w: f32, img_h: f32| -> Option<(f32, f32, Vec<f32>, Vec<f32>)> {
      let (mut tile_w, mut tile_h) = Self::compute_background_size(
        layer,
        style.font_size,
        style.root_font_size,
        self.viewport,
        origin_rect.width(),
        origin_rect.height(),
        img_w,
        img_h,
      );

      if tile_w <= 0.0 || tile_h <= 0.0 {
        return None;
      }

      let mut rounded_x = false;
      let mut rounded_y = false;
      if layer.repeat.x == BackgroundRepeatKeyword::Round {
        tile_w = Self::round_tile_length(origin_rect.width(), tile_w);
        rounded_x = true;
      }
      if layer.repeat.y == BackgroundRepeatKeyword::Round {
        tile_h = Self::round_tile_length(origin_rect.height(), tile_h);
        rounded_y = true;
      }
      if rounded_x ^ rounded_y
        && matches!(
          layer.size,
          BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto)
        )
      {
        let aspect = if img_h != 0.0 { img_w / img_h } else { 1.0 };
        if rounded_x {
          tile_h = tile_w / aspect;
        } else {
          tile_w = tile_h * aspect;
        }
      }

      let (offset_x, offset_y) = Self::resolve_background_offset(
        layer.position,
        origin_rect.width(),
        origin_rect.height(),
        tile_w,
        tile_h,
        style.font_size,
        style.root_font_size,
        self.viewport,
      );

      let positions_x = Self::tile_positions(
        layer.repeat.x,
        origin_rect.x(),
        origin_rect.width(),
        tile_w,
        offset_x,
        clip_rect.min_x(),
        clip_rect.max_x(),
      );
      let positions_y = Self::tile_positions(
        layer.repeat.y,
        origin_rect.y(),
        origin_rect.height(),
        tile_h,
        offset_y,
        clip_rect.min_y(),
        clip_rect.max_y(),
      );

      Some((tile_w, tile_h, positions_x, positions_y))
    };

    match bg {
      BackgroundImage::LinearGradient { angle, stops } => {
        let resolved = Self::normalize_color_stops(stops, style.color);
        if !resolved.is_empty() {
          if let Some((tile_w, tile_h, positions_x, positions_y)) = compute_tiles(0.0, 0.0) {
            let max_x = clip_rect.max_x();
            let max_y = clip_rect.max_y();
            let stops = Self::gradient_stops(&resolved);
            let rad = angle.to_radians();
            let dx = rad.sin();
            let dy = -rad.cos();
            let len = 0.5 * (tile_w * dx.abs() + tile_h * dy.abs());

            for ty in positions_y.iter().copied() {
              for tx in positions_x.iter().copied() {
                if tx >= max_x || ty >= max_y {
                  continue;
                }

                let tile_rect = Rect::from_xywh(tx, ty, tile_w, tile_h);
                let Some(intersection) = tile_rect.intersection(clip_rect) else {
                  continue;
                };
                if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
                  continue;
                }

                let cx = tile_rect.x() + tile_w / 2.0;
                let cy = tile_rect.y() + tile_h / 2.0;
                let start = Point::new(
                  cx - dx * len - intersection.x(),
                  cy - dy * len - intersection.y(),
                );
                let end = Point::new(
                  cx + dx * len - intersection.x(),
                  cy + dy * len - intersection.y(),
                );

                self
                  .list
                  .push(DisplayItem::LinearGradient(LinearGradientItem {
                    rect: intersection,
                    start,
                    end,
                    stops: stops.clone(),
                    spread: GradientSpread::Pad,
                  }));
              }
            }
          }
        }
      }
      BackgroundImage::RepeatingLinearGradient { angle, stops } => {
        let resolved = Self::normalize_color_stops(stops, style.color);
        if !resolved.is_empty() {
          if let Some((tile_w, tile_h, positions_x, positions_y)) = compute_tiles(0.0, 0.0) {
            let max_x = clip_rect.max_x();
            let max_y = clip_rect.max_y();
            let stops = Self::gradient_stops(&resolved);
            let rad = angle.to_radians();
            let dx = rad.sin();
            let dy = -rad.cos();
            let len = 0.5 * (tile_w * dx.abs() + tile_h * dy.abs());

            for ty in positions_y.iter().copied() {
              for tx in positions_x.iter().copied() {
                if tx >= max_x || ty >= max_y {
                  continue;
                }

                let tile_rect = Rect::from_xywh(tx, ty, tile_w, tile_h);
                let Some(intersection) = tile_rect.intersection(clip_rect) else {
                  continue;
                };
                if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
                  continue;
                }

                let cx = tile_rect.x() + tile_w / 2.0;
                let cy = tile_rect.y() + tile_h / 2.0;
                let start = Point::new(
                  cx - dx * len - intersection.x(),
                  cy - dy * len - intersection.y(),
                );
                let end = Point::new(
                  cx + dx * len - intersection.x(),
                  cy + dy * len - intersection.y(),
                );

                self
                  .list
                  .push(DisplayItem::LinearGradient(LinearGradientItem {
                    rect: intersection,
                    start,
                    end,
                    stops: stops.clone(),
                    spread: GradientSpread::Repeat,
                  }));
              }
            }
          }
        }
      }
      BackgroundImage::ConicGradient {
        from_angle,
        position,
        stops,
      } => {
        let resolved = Self::normalize_color_stops_unclamped(stops, style.color);
        if !resolved.is_empty() {
          if let Some((tile_w, tile_h, positions_x, positions_y)) = compute_tiles(0.0, 0.0) {
            let max_x = clip_rect.max_x();
            let max_y = clip_rect.max_y();
            let stops = Self::gradient_stops_unclamped(&resolved);

            for ty in positions_y.iter().copied() {
              for tx in positions_x.iter().copied() {
                if tx >= max_x || ty >= max_y {
                  continue;
                }

                let tile_rect = Rect::from_xywh(tx, ty, tile_w, tile_h);
                let Some(intersection) = tile_rect.intersection(clip_rect) else {
                  continue;
                };
                if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
                  continue;
                }

                let center_abs = Self::resolve_gradient_center(
                  Rect::from_xywh(tile_rect.x(), tile_rect.y(), tile_w, tile_h),
                  position,
                  style.font_size,
                  style.root_font_size,
                  self.viewport,
                );
                let center = Point::new(
                  center_abs.x - intersection.x(),
                  center_abs.y - intersection.y(),
                );

                self
                  .list
                  .push(DisplayItem::ConicGradient(ConicGradientItem {
                    rect: intersection,
                    center,
                    from_angle: *from_angle,
                    stops: stops.clone(),
                    repeating: false,
                  }));
              }
            }
          }
        }
      }
      BackgroundImage::RepeatingConicGradient {
        from_angle,
        position,
        stops,
      } => {
        let resolved = Self::normalize_color_stops_unclamped(stops, style.color);
        if !resolved.is_empty() {
          if let Some((tile_w, tile_h, positions_x, positions_y)) = compute_tiles(0.0, 0.0) {
            let max_x = clip_rect.max_x();
            let max_y = clip_rect.max_y();
            let stops = Self::gradient_stops_unclamped(&resolved);

            for ty in positions_y.iter().copied() {
              for tx in positions_x.iter().copied() {
                if tx >= max_x || ty >= max_y {
                  continue;
                }

                let tile_rect = Rect::from_xywh(tx, ty, tile_w, tile_h);
                let Some(intersection) = tile_rect.intersection(clip_rect) else {
                  continue;
                };
                if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
                  continue;
                }

                let center_abs = Self::resolve_gradient_center(
                  Rect::from_xywh(tile_rect.x(), tile_rect.y(), tile_w, tile_h),
                  position,
                  style.font_size,
                  style.root_font_size,
                  self.viewport,
                );
                let center = Point::new(
                  center_abs.x - intersection.x(),
                  center_abs.y - intersection.y(),
                );

                self
                  .list
                  .push(DisplayItem::ConicGradient(ConicGradientItem {
                    rect: intersection,
                    center,
                    from_angle: *from_angle,
                    stops: stops.clone(),
                    repeating: true,
                  }));
              }
            }
          }
        }
      }
      BackgroundImage::RadialGradient {
        shape,
        size,
        position,
        stops,
      } => {
        let resolved = Self::normalize_color_stops(stops, style.color);
        if !resolved.is_empty() {
          if let Some((tile_w, tile_h, positions_x, positions_y)) = compute_tiles(0.0, 0.0) {
            let max_x = clip_rect.max_x();
            let max_y = clip_rect.max_y();
            let stops = Self::gradient_stops(&resolved);

            for ty in positions_y.iter().copied() {
              for tx in positions_x.iter().copied() {
                if tx >= max_x || ty >= max_y {
                  continue;
                }

                let tile_rect = Rect::from_xywh(tx, ty, tile_w, tile_h);
                let Some(intersection) = tile_rect.intersection(clip_rect) else {
                  continue;
                };
                if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
                  continue;
                }

                let (cx, cy, radius_x, radius_y) = Self::radial_geometry(
                  Rect::from_xywh(tile_rect.x(), tile_rect.y(), tile_w, tile_h),
                  position,
                  size,
                  *shape,
                  style.font_size,
                  style.root_font_size,
                  self.viewport,
                );
                let center = Point::new(cx - intersection.x(), cy - intersection.y());

                self
                  .list
                  .push(DisplayItem::RadialGradient(RadialGradientItem {
                    rect: intersection,
                    center,
                    radii: Point::new(radius_x, radius_y),
                    stops: stops.clone(),
                    spread: GradientSpread::Pad,
                  }));
              }
            }
          }
        }
      }
      BackgroundImage::RepeatingRadialGradient {
        shape,
        size,
        position,
        stops,
      } => {
        let resolved = Self::normalize_color_stops(stops, style.color);
        if !resolved.is_empty() {
          if let Some((tile_w, tile_h, positions_x, positions_y)) = compute_tiles(0.0, 0.0) {
            let max_x = clip_rect.max_x();
            let max_y = clip_rect.max_y();
            let stops = Self::gradient_stops(&resolved);

            for ty in positions_y.iter().copied() {
              for tx in positions_x.iter().copied() {
                if tx >= max_x || ty >= max_y {
                  continue;
                }

                let tile_rect = Rect::from_xywh(tx, ty, tile_w, tile_h);
                let Some(intersection) = tile_rect.intersection(clip_rect) else {
                  continue;
                };
                if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
                  continue;
                }

                let (cx, cy, radius_x, radius_y) = Self::radial_geometry(
                  Rect::from_xywh(tile_rect.x(), tile_rect.y(), tile_w, tile_h),
                  position,
                  size,
                  *shape,
                  style.font_size,
                  style.root_font_size,
                  self.viewport,
                );
                let center = Point::new(cx - intersection.x(), cy - intersection.y());

                self
                  .list
                  .push(DisplayItem::RadialGradient(RadialGradientItem {
                    rect: intersection,
                    center,
                    radii: Point::new(radius_x, radius_y),
                    stops: stops.clone(),
                    spread: GradientSpread::Repeat,
                  }));
              }
            }
          }
        }
      }
      BackgroundImage::Url(src) => {
        if let Some(image) = self.decode_image(src, Some(style), true) {
          let img_w = image.css_width;
          let img_h = image.css_height;
          if img_w > 0.0 && img_h > 0.0 {
            let (mut tile_w, mut tile_h) = Self::compute_background_size(
              layer,
              style.font_size,
              style.root_font_size,
              self.viewport,
              origin_rect.width(),
              origin_rect.height(),
              img_w,
              img_h,
            );

            let mut rounded_x = false;
            let mut rounded_y = false;
            if layer.repeat.x == BackgroundRepeatKeyword::Round {
              tile_w = Self::round_tile_length(origin_rect.width(), tile_w);
              rounded_x = true;
            }
            if layer.repeat.y == BackgroundRepeatKeyword::Round {
              tile_h = Self::round_tile_length(origin_rect.height(), tile_h);
              rounded_y = true;
            }
            if rounded_x ^ rounded_y
              && matches!(
                layer.size,
                BackgroundSize::Explicit(
                  BackgroundSizeComponent::Auto,
                  BackgroundSizeComponent::Auto
                )
              )
            {
              let aspect = if img_h != 0.0 { img_w / img_h } else { 1.0 };
              if rounded_x {
                tile_h = tile_w / aspect;
              } else {
                tile_w = tile_h * aspect;
              }
            }

            if tile_w > 0.0 && tile_h > 0.0 {
              let (offset_x, offset_y) = Self::resolve_background_offset(
                layer.position,
                origin_rect.width(),
                origin_rect.height(),
                tile_w,
                tile_h,
                style.font_size,
                style.root_font_size,
                self.viewport,
              );

              let positions_x = Self::tile_positions(
                layer.repeat.x,
                origin_rect.x(),
                origin_rect.width(),
                tile_w,
                offset_x,
                clip_rect.min_x(),
                clip_rect.max_x(),
              );
              let positions_y = Self::tile_positions(
                layer.repeat.y,
                origin_rect.y(),
                origin_rect.height(),
                tile_h,
                offset_y,
                clip_rect.min_y(),
                clip_rect.max_y(),
              );

              let max_x = clip_rect.max_x();
              let max_y = clip_rect.max_y();
              let quality = Self::image_filter_quality(Some(style));
              let image = Arc::new(image);

              for ty in positions_y.iter().copied() {
                for tx in positions_x.iter().copied() {
                  if tx >= max_x || ty >= max_y {
                    continue;
                  }

                  let tile_rect = Rect::from_xywh(tx, ty, tile_w, tile_h);
                  let Some(intersection) = tile_rect.intersection(clip_rect) else {
                    continue;
                  };
                  if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
                    continue;
                  }

                  let scale_x = image.width as f32 / tile_w;
                  let scale_y = image.height as f32 / tile_h;
                  if !scale_x.is_finite() || !scale_y.is_finite() {
                    continue;
                  }

                  let src_rect = Rect::from_xywh(
                    (intersection.x() - tile_rect.x()) * scale_x,
                    (intersection.y() - tile_rect.y()) * scale_y,
                    intersection.width() * scale_x,
                    intersection.height() * scale_y,
                  );

                  self.list.push(DisplayItem::Image(ImageItem {
                    dest_rect: intersection,
                    image: image.clone(),
                    filter_quality: quality,
                    src_rect: Some(src_rect),
                  }));
                }
              }
            }
          }
        }
      }
      BackgroundImage::None => {}
    }

    if use_blend {
      self.list.push(DisplayItem::PopBlendMode);
    }
    if pushed_clip {
      self.list.push(DisplayItem::PopClip);
    }
  }

  fn emit_box_shadows_from_style(&mut self, rect: Rect, style: &ComputedStyle, inset: bool) {
    if style.box_shadow.is_empty() {
      return;
    }
    let rects = Self::background_rects(rect, style, self.viewport);
    let outer_radii = Self::border_radii(rect, style).clamped(rect.width(), rect.height());
    let inner_radii =
      Self::resolve_clip_radii(style, &rects, BackgroundBox::PaddingBox, self.viewport);
    let base_rect = if inset { rects.padding } else { rects.border };

    for shadow in &style.box_shadow {
      if shadow.inset != inset {
        continue;
      }
      let offset_x = Self::resolve_length_for_paint(
        &shadow.offset_x,
        style.font_size,
        style.root_font_size,
        rect.width(),
        self.viewport,
      );
      let offset_y = Self::resolve_length_for_paint(
        &shadow.offset_y,
        style.font_size,
        style.root_font_size,
        rect.width(),
        self.viewport,
      );
      let blur = Self::resolve_length_for_paint(
        &shadow.blur_radius,
        style.font_size,
        style.root_font_size,
        rect.width(),
        self.viewport,
      )
      .max(0.0);
      let spread = Self::resolve_length_for_paint(
        &shadow.spread_radius,
        style.font_size,
        style.root_font_size,
        rect.width(),
        self.viewport,
      )
      .max(-1e6);

      self.list.push(DisplayItem::BoxShadow(BoxShadowItem {
        rect: base_rect,
        radii: if inset { inner_radii } else { outer_radii },
        offset: Point::new(offset_x, offset_y),
        blur_radius: blur,
        spread_radius: spread,
        color: shadow.color,
        inset,
      }));
    }
  }

  fn emit_border_from_style(&mut self, rect: Rect, style: &ComputedStyle) {
    let widths = (
      Self::resolve_length_for_paint(
        &style.border_top_width,
        style.font_size,
        style.root_font_size,
        rect.width(),
        self.viewport,
      ),
      Self::resolve_length_for_paint(
        &style.border_right_width,
        style.font_size,
        style.root_font_size,
        rect.width(),
        self.viewport,
      ),
      Self::resolve_length_for_paint(
        &style.border_bottom_width,
        style.font_size,
        style.root_font_size,
        rect.width(),
        self.viewport,
      ),
      Self::resolve_length_for_paint(
        &style.border_left_width,
        style.font_size,
        style.root_font_size,
        rect.width(),
        self.viewport,
      ),
    );

    let sides = (
      BorderSide {
        width: widths.0,
        style: style.border_top_style,
        color: style.border_top_color,
      },
      BorderSide {
        width: widths.1,
        style: style.border_right_style,
        color: style.border_right_color,
      },
      BorderSide {
        width: widths.2,
        style: style.border_bottom_style,
        color: style.border_bottom_color,
      },
      BorderSide {
        width: widths.3,
        style: style.border_left_style,
        color: style.border_left_color,
      },
    );

    let any_visible = Self::border_side_visible(&sides.0)
      || Self::border_side_visible(&sides.1)
      || Self::border_side_visible(&sides.2)
      || Self::border_side_visible(&sides.3);
    if !any_visible {
      return;
    }

    let border_image = match &style.border_image.source {
      BorderImageSource::Image(bg) => {
        let source = match bg.as_ref() {
          BackgroundImage::Url(src) => self
            .decode_image(src, Some(style), true)
            .map(BorderImageSourceItem::Raster),
          BackgroundImage::LinearGradient { .. }
          | BackgroundImage::RepeatingLinearGradient { .. }
          | BackgroundImage::RadialGradient { .. }
          | BackgroundImage::RepeatingRadialGradient { .. }
          | BackgroundImage::ConicGradient { .. }
          | BackgroundImage::RepeatingConicGradient { .. } => {
            Some(BorderImageSourceItem::Generated(Box::new((**bg).clone())))
          }
          BackgroundImage::None => None,
        };
        source.map(|source| BorderImageItem {
          source,
          slice: style.border_image.slice.clone(),
          width: style.border_image.width.clone(),
          outset: style.border_image.outset.clone(),
          repeat: style.border_image.repeat,
          current_color: style.color,
          font_size: style.font_size,
          root_font_size: style.root_font_size,
          viewport: self.viewport,
        })
      }
      BorderImageSource::None => None,
    };

    let radii = Self::border_radii(rect, style).clamped(rect.width(), rect.height());
    self.list.push(DisplayItem::Border(Box::new(BorderItem {
      rect,
      top: sides.0,
      right: sides.1,
      bottom: sides.2,
      left: sides.3,
      image: border_image,
      radii,
    })));
  }

  fn emit_outline(&mut self, rect: Rect, style: &ComputedStyle) {
    let ow = style.outline_width.to_px();
    let outline_style = style.outline_style.to_border_style();
    if ow <= 0.0
      || matches!(
        outline_style,
        crate::style::types::BorderStyle::None | crate::style::types::BorderStyle::Hidden
      )
    {
      return;
    }
    let offset = style.outline_offset.to_px();
    let (color, invert) = style.outline_color.resolve(style.color);
    if ow > 0.0 && !color.is_transparent() {
      self.list.push(DisplayItem::Outline(OutlineItem {
        rect,
        width: ow,
        style: outline_style,
        color,
        offset,
        invert,
      }));
    }
  }

  /// Begins an opacity layer
  pub fn push_opacity(&mut self, opacity: f32) {
    self
      .list
      .push(DisplayItem::PushOpacity(OpacityItem { opacity }));
  }

  /// Ends an opacity layer
  pub fn pop_opacity(&mut self) {
    self.list.push(DisplayItem::PopOpacity);
  }

  /// Begins a clip region
  pub fn push_clip(&mut self, rect: Rect) {
    self.list.push(DisplayItem::PushClip(ClipItem {
      shape: ClipShape::Rect { rect, radii: None },
    }));
  }

  /// Ends a clip region
  pub fn pop_clip(&mut self) {
    self.list.push(DisplayItem::PopClip);
  }

  fn emit_shaped_runs(
    &mut self,
    runs: &[ShapedRun],
    color: Rgba,
    baseline_y: f32,
    start_x: f32,
    shadows: &[TextShadowItem],
    style: Option<&ComputedStyle>,
    inline_vertical: bool,
  ) {
    let mut pen_x = start_x;
    for run in runs {
      let origin_x = if run.direction.is_rtl() {
        pen_x + run.advance
      } else {
        pen_x
      };
      let glyphs = self.glyphs_from_run(run, origin_x, baseline_y);
      let font_id = self.font_id_from_run(run);
      let emphasis =
        style.and_then(|s| self.build_emphasis(run, s, origin_x, baseline_y, inline_vertical));

      self.list.push(DisplayItem::Text(TextItem {
        origin: Point::new(origin_x, baseline_y),
        glyphs,
        color,
        shadows: shadows.to_vec(),
        font_size: run.font_size,
        advance_width: run.advance,
        font_id: Some(font_id),
        synthetic_bold: run.synthetic_bold,
        synthetic_oblique: run.synthetic_oblique,
        emphasis,
        decorations: Vec::new(),
      }));

      pen_x += run.advance;
    }
  }

  fn emit_shaped_runs_vertical(
    &mut self,
    runs: &[ShapedRun],
    color: Rgba,
    block_baseline: f32,
    inline_start: f32,
    shadows: &[TextShadowItem],
    style: Option<&ComputedStyle>,
  ) {
    let mut pen_inline = inline_start;
    for run in runs {
      let run_origin_inline = if run.direction.is_rtl() {
        pen_inline + run.advance
      } else {
        pen_inline
      };
      let glyphs =
        self.glyphs_from_run_vertical(run, block_baseline, run_origin_inline, inline_start);
      let font_id = self.font_id_from_run(run);
      let emphasis =
        style.and_then(|s| self.build_emphasis(run, s, block_baseline, run_origin_inline, true));

      self.list.push(DisplayItem::Text(TextItem {
        origin: Point::new(block_baseline, inline_start),
        glyphs,
        color,
        shadows: shadows.to_vec(),
        font_size: run.font_size,
        advance_width: run.advance,
        font_id: Some(font_id),
        synthetic_bold: run.synthetic_bold,
        synthetic_oblique: run.synthetic_oblique,
        emphasis,
        decorations: Vec::new(),
      }));

      pen_inline += run.advance;
    }
  }

  fn emit_list_marker_runs(
    &mut self,
    runs: &[ShapedRun],
    color: Rgba,
    baseline_y: f32,
    start_x: f32,
    shadows: &[TextShadowItem],
    style: Option<&ComputedStyle>,
    inline_vertical: bool,
  ) {
    let mut pen_x = start_x;
    for run in runs {
      let origin_x = if run.direction.is_rtl() {
        pen_x + run.advance
      } else {
        pen_x
      };
      let glyphs = self.glyphs_from_run(run, origin_x, baseline_y);
      let font_id = self.font_id_from_run(run);
      let emphasis =
        style.and_then(|s| self.build_emphasis(run, s, origin_x, baseline_y, inline_vertical));
      self.list.push(DisplayItem::ListMarker(ListMarkerItem {
        origin: Point::new(origin_x, baseline_y),
        glyphs,
        font_size: run.font_size,
        color,
        shadows: shadows.to_vec(),
        advance_width: run.advance,
        font_id: Some(font_id),
        synthetic_bold: run.synthetic_bold,
        synthetic_oblique: run.synthetic_oblique,
        emphasis,
        background: None,
      }));

      pen_x += run.advance;
    }
  }

  fn emit_list_marker_runs_vertical(
    &mut self,
    runs: &[ShapedRun],
    color: Rgba,
    block_baseline: f32,
    inline_start: f32,
    shadows: &[TextShadowItem],
    style: Option<&ComputedStyle>,
  ) {
    let mut pen_inline = inline_start;
    for run in runs {
      let run_origin_inline = if run.direction.is_rtl() {
        pen_inline + run.advance
      } else {
        pen_inline
      };
      let glyphs =
        self.glyphs_from_run_vertical(run, block_baseline, run_origin_inline, inline_start);
      let font_id = self.font_id_from_run(run);
      let emphasis =
        style.and_then(|s| self.build_emphasis(run, s, block_baseline, run_origin_inline, true));
      self.list.push(DisplayItem::ListMarker(ListMarkerItem {
        origin: Point::new(block_baseline, inline_start),
        glyphs,
        font_size: run.font_size,
        color,
        shadows: shadows.to_vec(),
        advance_width: run.advance,
        font_id: Some(font_id),
        synthetic_bold: run.synthetic_bold,
        synthetic_oblique: run.synthetic_oblique,
        emphasis,
        background: None,
      }));

      pen_inline += run.advance;
    }
  }

  fn emit_text_decorations(
    &mut self,
    style: &ComputedStyle,
    runs: Option<&[ShapedRun]>,
    inline_start: f32,
    inline_len: f32,
    block_baseline: f32,
    inline_vertical: bool,
  ) {
    if inline_len <= 0.0 {
      return;
    }

    let decorations = if !style.applied_text_decorations.is_empty() {
      style.applied_text_decorations.clone()
    } else if !style.text_decoration.lines.is_empty() {
      vec![ResolvedTextDecoration {
        decoration: style.text_decoration.clone(),
        skip_ink: style.text_decoration_skip_ink,
        underline_offset: style.text_underline_offset,
        underline_position: style.text_underline_position,
      }]
    } else {
      Vec::new()
    };
    if decorations.is_empty() {
      return;
    }

    let Some(metrics) = self.decoration_metrics(runs, style) else {
      return;
    };

    let mut paints = Vec::new();
    let mut min_block = f32::INFINITY;
    let mut max_block = f32::NEG_INFINITY;

    for deco in decorations {
      let decoration_color = deco.decoration.color.unwrap_or(style.color);
      if decoration_color.alpha_u8() == 0 {
        continue;
      }

      let used_thickness = match deco.decoration.thickness {
        TextDecorationThickness::Auto => None,
        TextDecorationThickness::FromFont => None,
        TextDecorationThickness::Length(l) => {
          if l.unit == LengthUnit::Percent {
            l.resolve_against(style.font_size)
          } else if l.unit.is_viewport_relative() {
            self
              .viewport
              .and_then(|(vw, vh)| l.resolve_with_viewport(vw, vh))
          } else {
            Some(resolve_font_relative_length(l, style, &self.font_ctx))
          }
        }
      };

      let underline_offset = self.resolve_underline_offset_value(deco.underline_offset, style);
      let mut paint = DecorationPaint {
        style: deco.decoration.style,
        color: decoration_color,
        underline: None,
        overline: None,
        line_through: None,
      };

      if deco
        .decoration
        .lines
        .contains(TextDecorationLine::UNDERLINE)
      {
        let thickness = used_thickness.unwrap_or(metrics.underline_thickness);
        let center = block_baseline
          - self.underline_position(
            &metrics,
            deco.underline_position,
            underline_offset,
            thickness,
          );
        let segments = if matches!(
          deco.skip_ink,
          TextDecorationSkipInk::Auto | TextDecorationSkipInk::All
        ) {
          runs.map(|r| {
            self.build_underline_segments(
              r,
              inline_start,
              inline_len,
              center,
              thickness,
              block_baseline,
              inline_vertical,
              deco.skip_ink,
            )
          })
        } else {
          None
        };
        paint.underline = Some(DecorationStroke {
          center,
          thickness,
          segments,
        });
        let half_extent = Self::stroke_half_extent(deco.decoration.style, thickness);
        min_block = min_block.min(center - half_extent);
        max_block = max_block.max(center + half_extent);
      }
      if deco.decoration.lines.contains(TextDecorationLine::OVERLINE) {
        let thickness = used_thickness.unwrap_or(metrics.underline_thickness);
        let center = block_baseline - metrics.ascent;
        paint.overline = Some(DecorationStroke {
          center,
          thickness,
          segments: None,
        });
        let half_extent = Self::stroke_half_extent(deco.decoration.style, thickness);
        min_block = min_block.min(center - half_extent);
        max_block = max_block.max(center + half_extent);
      }
      if deco
        .decoration
        .lines
        .contains(TextDecorationLine::LINE_THROUGH)
      {
        let thickness = used_thickness.unwrap_or(metrics.strike_thickness);
        let center = block_baseline - metrics.strike_pos;
        paint.line_through = Some(DecorationStroke {
          center,
          thickness,
          segments: None,
        });
        let half_extent = Self::stroke_half_extent(deco.decoration.style, thickness);
        min_block = min_block.min(center - half_extent);
        max_block = max_block.max(center + half_extent);
      }

      if paint.underline.is_some() || paint.overline.is_some() || paint.line_through.is_some() {
        paints.push(paint);
      }
    }

    if paints.is_empty() {
      return;
    }

    let block_span = (max_block - min_block).max(0.0);
    let bounds = if inline_vertical {
      Rect::from_xywh(min_block, inline_start, block_span, inline_len)
    } else {
      Rect::from_xywh(inline_start, min_block, inline_len, block_span)
    };
    self
      .list
      .push(DisplayItem::TextDecoration(TextDecorationItem {
        bounds,
        line_start: inline_start,
        line_width: inline_len,
        inline_vertical,
        decorations: paints,
      }));
  }

  fn stroke_half_extent(style: TextDecorationStyle, thickness: f32) -> f32 {
    match style {
      TextDecorationStyle::Double => thickness * 2.5,
      TextDecorationStyle::Wavy => thickness * 2.0,
      _ => thickness * 0.5,
    }
  }

  fn resolve_underline_offset_value(
    &self,
    offset: TextUnderlineOffset,
    style: &ComputedStyle,
  ) -> f32 {
    match offset {
      TextUnderlineOffset::Auto => 0.0,
      TextUnderlineOffset::Length(l) => {
        if l.unit == LengthUnit::Percent {
          l.resolve_against(style.font_size).unwrap_or(0.0)
        } else if l.unit.is_font_relative() {
          resolve_font_relative_length(l, style, &self.font_ctx)
        } else if l.unit.is_viewport_relative() {
          self
            .viewport
            .and_then(|(vw, vh)| l.resolve_with_viewport(vw, vh))
            .unwrap_or_else(|| l.to_px())
        } else if l.unit.is_absolute() {
          l.to_px()
        } else {
          l.value * style.font_size
        }
      }
    }
  }

  fn underline_position(
    &self,
    metrics: &DecorationMetrics,
    position: TextUnderlinePosition,
    offset: f32,
    thickness: f32,
  ) -> f32 {
    let under_base = -metrics.descent - thickness * 0.5;
    let base = match position {
      TextUnderlinePosition::Auto | TextUnderlinePosition::FromFont => metrics.underline_pos,
      TextUnderlinePosition::Under
      | TextUnderlinePosition::UnderLeft
      | TextUnderlinePosition::UnderRight => metrics.underline_pos.min(under_base),
      TextUnderlinePosition::Left | TextUnderlinePosition::Right => metrics.underline_pos,
    };

    metrics.underline_position_with_offset(base, offset)
  }

  fn decoration_metrics(
    &self,
    runs: Option<&[ShapedRun]>,
    style: &ComputedStyle,
  ) -> Option<DecorationMetrics> {
    let mut metrics_source = runs.and_then(|rs| {
      rs.iter()
        .find_map(|run| run.font.metrics().ok().map(|m| (m, run.font_size)))
    });

    if metrics_source.is_none() {
      let italic = matches!(style.font_style, crate::style::types::FontStyle::Italic);
      let oblique = matches!(style.font_style, crate::style::types::FontStyle::Oblique(_));
      let stretch =
        crate::text::font_db::FontStretch::from_percentage(style.font_stretch.to_percentage());
      metrics_source = self
        .font_ctx
        .get_font_full(
          &style.font_family,
          style.font_weight.to_u16(),
          if italic {
            crate::text::font_db::FontStyle::Italic
          } else if oblique {
            crate::text::font_db::FontStyle::Oblique
          } else {
            crate::text::font_db::FontStyle::Normal
          },
          stretch,
        )
        .or_else(|| self.font_ctx.get_sans_serif())
        .and_then(|font| font.metrics().ok().map(|m| (m, style.font_size)));
    }

    if let Some((metrics, size)) = metrics_source {
      let scale = size / (metrics.units_per_em as f32);

      let underline_pos = metrics.underline_position as f32 * scale;
      let underline_thickness = (metrics.underline_thickness as f32 * scale).max(1.0);
      let descent = (metrics.descent as f32 * scale).abs();
      let strike_pos = metrics
        .strikeout_position
        .map(|p| p as f32 * scale)
        .unwrap_or_else(|| metrics.ascent as f32 * scale * 0.3);
      let strike_thickness = metrics
        .strikeout_thickness
        .map(|t| t as f32 * scale)
        .unwrap_or(underline_thickness);
      let ascent = metrics.ascent as f32 * scale;

      Some(DecorationMetrics {
        underline_pos,
        underline_thickness,
        strike_pos,
        strike_thickness,
        ascent,
        descent,
      })
    } else {
      // Fallback heuristic metrics when we cannot obtain font metrics.
      let size = style.font_size.max(1.0);
      let ascent = size * 0.8;
      let descent = size - ascent;
      let underline_thickness = (size * 0.05).max(1.0);
      let underline_pos = descent * 0.5;
      let strike_pos = ascent * 0.4;

      Some(DecorationMetrics {
        underline_pos,
        underline_thickness,
        strike_pos,
        strike_thickness: underline_thickness,
        ascent,
        descent,
      })
    }
  }

  fn build_underline_segments(
    &self,
    runs: &[ShapedRun],
    line_start: f32,
    line_width: f32,
    center: f32,
    thickness: f32,
    baseline_y: f32,
    inline_vertical: bool,
    skip_ink: TextDecorationSkipInk,
  ) -> Vec<(f32, f32)> {
    if line_width <= 0.0 {
      return Vec::new();
    }

    let band_half = (thickness * 0.5).abs();
    let mut exclusions = if inline_vertical {
      let band_left = center - band_half;
      let band_right = center + band_half;
      collect_underline_exclusions_vertical(
        runs,
        line_start,
        baseline_y,
        band_left,
        band_right,
        skip_ink == TextDecorationSkipInk::All,
      )
    } else {
      let band_top = center - band_half;
      let band_bottom = center + band_half;
      collect_underline_exclusions(
        runs,
        line_start,
        baseline_y,
        band_top,
        band_bottom,
        skip_ink == TextDecorationSkipInk::All,
      )
    };

    let mut segments = subtract_intervals((line_start, line_start + line_width), &mut exclusions);
    if segments.is_empty() && skip_ink != TextDecorationSkipInk::All {
      // Never drop the underline entirely when skipping ink; fall back to a full span.
      segments.push((line_start, line_start + line_width));
    }

    segments
  }

  fn scale_run(run: &ShapedRun, scale_x: f32, scale_y: f32) -> ShapedRun {
    if (scale_x - 1.0).abs() < 0.001 && (scale_y - 1.0).abs() < 0.001 {
      return run.clone();
    }
    let mut scaled = run.clone();
    let uniform = ((scale_x + scale_y) * 0.5).max(0.0);
    scaled.font_size *= uniform;
    scaled.advance *= scale_x;
    scaled.baseline_shift *= scale_y;
    scaled.synthetic_bold *= uniform;
    scaled.scale *= uniform.max(1e-3);
    for glyph in &mut scaled.glyphs {
      glyph.x_offset *= scale_x;
      glyph.y_offset *= scale_y;
      glyph.x_advance *= scale_x;
      glyph.y_advance *= scale_y;
    }
    scaled
  }

  fn glyphs_from_run(&self, run: &ShapedRun, origin_x: f32, baseline_y: f32) -> Vec<GlyphInstance> {
    let mut glyphs = Vec::with_capacity(run.glyphs.len());

    for glyph in &run.glyphs {
      let x = match run.direction {
        crate::text::pipeline::Direction::RightToLeft => origin_x - glyph.x_offset,
        crate::text::pipeline::Direction::LeftToRight => origin_x + glyph.x_offset,
      };
      let y = baseline_y - glyph.y_offset;
      glyphs.push(GlyphInstance {
        glyph_id: glyph.glyph_id,
        offset: Point::new(x - origin_x, y - baseline_y),
        advance: glyph.x_advance,
      });
    }

    glyphs
  }

  fn glyphs_from_run_vertical(
    &self,
    run: &ShapedRun,
    block_baseline: f32,
    inline_origin: f32,
    inline_start: f32,
  ) -> Vec<GlyphInstance> {
    let mut glyphs = Vec::with_capacity(run.glyphs.len());

    for glyph in &run.glyphs {
      let inline_pos = match run.direction {
        crate::text::pipeline::Direction::RightToLeft => inline_origin - glyph.x_offset,
        crate::text::pipeline::Direction::LeftToRight => inline_origin + glyph.x_offset,
      };
      let block_pos = block_baseline - glyph.y_offset;
      glyphs.push(GlyphInstance {
        glyph_id: glyph.glyph_id,
        offset: Point::new(block_pos - block_baseline, inline_pos - inline_start),
        advance: 0.0,
      });
    }

    glyphs
  }

  fn build_emphasis(
    &self,
    run: &ShapedRun,
    style: &ComputedStyle,
    inline_origin: f32,
    block_baseline: f32,
    inline_vertical: bool,
  ) -> Option<TextEmphasis> {
    if style.text_emphasis_style.is_none() {
      return None;
    }
    let mark_color = style.text_emphasis_color.unwrap_or(style.color);
    let (ascent, descent) = if let Ok(metrics) = run.font.metrics() {
      let scaled = metrics.scale(run.font_size);
      (scaled.ascent, scaled.descent)
    } else {
      (run.font_size * 0.8, run.font_size * 0.2)
    };
    let mark_size = (style.font_size * 0.5).max(1.0);
    let gap = mark_size * 0.3;
    let resolved_position = match style.text_emphasis_position {
      TextEmphasisPosition::Auto => TextEmphasisPosition::Over,
      other => other,
    };
    let block_center = if inline_vertical {
      let offset = gap + mark_size * 0.5;
      match resolved_position {
        TextEmphasisPosition::Over
        | TextEmphasisPosition::OverLeft
        | TextEmphasisPosition::OverRight => block_baseline + offset,
        TextEmphasisPosition::Under
        | TextEmphasisPosition::UnderLeft
        | TextEmphasisPosition::UnderRight => block_baseline - offset,
        TextEmphasisPosition::Auto => block_baseline + offset,
      }
    } else {
      match resolved_position {
        TextEmphasisPosition::Over
        | TextEmphasisPosition::OverLeft
        | TextEmphasisPosition::OverRight => block_baseline - ascent - gap - mark_size * 0.5,
        TextEmphasisPosition::Under
        | TextEmphasisPosition::UnderLeft
        | TextEmphasisPosition::UnderRight => block_baseline + descent + gap + mark_size * 0.5,
        TextEmphasisPosition::Auto => block_baseline - ascent - gap - mark_size * 0.5,
      }
    };

    let mut marks = Vec::new();
    let mut seen_clusters = HashSet::new();
    let run_origin_inline = if run.direction.is_rtl() {
      inline_origin + run.advance
    } else {
      inline_origin
    };
    for glyph in &run.glyphs {
      if !seen_clusters.insert(glyph.cluster) {
        continue;
      }
      let text_byte = glyph.cluster as usize;
      if text_byte < run.text.len() {
        if let Some(ch) = run.text[text_byte..].chars().next() {
          if ch.is_whitespace() || ch.is_control() {
            continue;
          }
        }
      }
      let inline_center = match run.direction {
        crate::text::pipeline::Direction::RightToLeft => {
          run_origin_inline - (glyph.x_offset + glyph.x_advance * 0.5)
        }
        crate::text::pipeline::Direction::LeftToRight => {
          run_origin_inline + glyph.x_offset + glyph.x_advance * 0.5
        }
      };
      let center = if inline_vertical {
        Point::new(block_center, inline_center)
      } else {
        Point::new(inline_center, block_center)
      };
      marks.push(EmphasisMark { center });
    }

    let text = if let TextEmphasisStyle::String(ref s) = style.text_emphasis_style {
      if s.is_empty() {
        None
      } else {
        let mut mark_style = style.clone();
        mark_style.font_size = style.font_size * 0.5;
        match self.shaper.shape(s, &mark_style, &self.font_ctx) {
          Ok(mark_runs) if !mark_runs.is_empty() => {
            let mark_font_id = self.font_id_from_run(&mark_runs[0]);
            let mut glyphs = Vec::new();
            let mut width = 0.0;
            let mut ascent: f32 = 0.0;
            let mut descent: f32 = 0.0;
            for r in &mark_runs {
              if let Ok(m) = r.font.metrics() {
                let scaled = m.scale(r.font_size);
                ascent = ascent.max(scaled.ascent);
                descent = descent.max(scaled.descent);
              }
            }
            if ascent == 0.0 && descent == 0.0 {
              ascent = mark_style.font_size * 0.8;
              descent = mark_style.font_size * 0.2;
            }
            for r in mark_runs {
              let mark_origin = if r.direction.is_rtl() {
                width + r.advance
              } else {
                width
              };
              for g in r.glyphs {
                let x = match r.direction {
                  crate::text::pipeline::Direction::RightToLeft => mark_origin - g.x_offset,
                  crate::text::pipeline::Direction::LeftToRight => mark_origin + g.x_offset,
                };
                glyphs.push(GlyphInstance {
                  glyph_id: g.glyph_id,
                  offset: Point::new(x, -g.y_offset),
                  advance: g.x_advance,
                });
              }
              width += r.advance;
            }
            Some(EmphasisText {
              glyphs,
              font_id: Some(mark_font_id),
              font_size: mark_style.font_size,
              width,
              height: ascent + descent,
              baseline_offset: ascent,
            })
          }
          _ => None,
        }
      }
    } else {
      None
    };

    Some(TextEmphasis {
      style: style.text_emphasis_style.clone(),
      color: mark_color,
      position: resolved_position,
      size: mark_size,
      marks,
      inline_vertical,
      text,
    })
  }

  fn font_id_from_run(&self, run: &ShapedRun) -> FontId {
    FontId {
      family: run.font.family.clone(),
      weight: run.font.weight.value(),
      style: run.font.style,
      stretch: run.font.stretch,
    }
  }

  fn text_shadows_from_style(style: Option<&ComputedStyle>) -> Vec<TextShadowItem> {
    style
      .map(|s| {
        resolve_text_shadows(s)
          .into_iter()
          .map(|shadow| TextShadowItem {
            offset: Point::new(shadow.offset_x, shadow.offset_y),
            blur_radius: shadow.blur_radius,
            color: shadow.color,
          })
          .collect()
      })
      .unwrap_or_default()
  }

  fn emit_replaced_placeholder(
    &mut self,
    replaced_type: &ReplacedType,
    fragment: &FragmentNode,
    rect: Rect,
  ) {
    let placeholder_color = Rgba::rgb(200, 200, 200);
    self.list.push(DisplayItem::FillRect(FillRectItem {
      rect,
      color: placeholder_color,
    }));

    let stroke_color = Rgba::rgb(150, 150, 150);
    self.list.push(DisplayItem::StrokeRect(StrokeRectItem {
      rect,
      color: stroke_color,
      width: 1.0,
      blend_mode: BlendMode::Normal,
    }));

    let label = replaced_type.placeholder_label();

    if let Some(label_text) = label {
      let label_style = fragment.style.as_deref().map(|style| {
        let mut clone = style.clone();
        clone.color = Rgba::rgb(120, 120, 120);
        clone
      });
      let inset = 2.0;
      let label_rect = Rect::from_xywh(
        rect.x() + inset,
        rect.y() + inset,
        (rect.width() - inset * 2.0).max(0.0),
        (rect.height() - inset * 2.0).max(0.0),
      );
      let label_style_ref = label_style
        .as_ref()
        .map(|s| s as &ComputedStyle)
        .or(fragment.style.as_deref());
      let _ = self.emit_text_with_style(label_text, label_style_ref, label_rect);
    }
  }

  fn resolved_accent_color(style: &ComputedStyle) -> Rgba {
    match style.accent_color {
      AccentColor::Color(c) => c,
      AccentColor::Auto => style.color,
    }
  }

  fn emit_form_control(
    &mut self,
    control: &FormControl,
    fragment: &FragmentNode,
    rect: Rect,
  ) -> bool {
    let Some(style) = fragment.style.as_deref() else {
      return false;
    };

    let mut accent = Self::resolved_accent_color(style);
    if control.invalid {
      accent = Rgba {
        r: 212,
        g: 43,
        b: 43,
        a: 1.0,
      };
    }
    let muted_accent = if control.disabled {
      accent.with_alpha((accent.a * 0.7).clamp(0.0, 1.0))
    } else {
      accent
    };

    let inset_rect = |rect: Rect, inset: f32| {
      Rect::from_xywh(
        rect.x() + inset,
        rect.y() + inset,
        (rect.width() - 2.0 * inset).max(0.0),
        (rect.height() - 2.0 * inset).max(0.0),
      )
    };

    let highlight = if control.invalid {
      Some(muted_accent.with_alpha((muted_accent.a * 0.25).max(0.18)))
    } else if control.focus_visible {
      Some(muted_accent.with_alpha((muted_accent.a * 0.22).max(0.14)))
    } else if control.focused {
      Some(muted_accent.with_alpha((muted_accent.a * 0.16).max(0.1)))
    } else if control.required {
      Some(muted_accent.with_alpha(0.08))
    } else {
      None
    };
    if let Some(tint) = highlight {
      let rect = inset_rect(rect, 1.0);
      self
        .list
        .push(DisplayItem::FillRoundedRect(FillRoundedRectItem {
          rect,
          color: tint,
          radii: BorderRadii::uniform((rect.height().min(rect.width()) / 6.0).max(2.0)),
        }));
    }

    match &control.control {
      FormControlKind::Text {
        value,
        placeholder,
        kind,
        ..
      } => {
        let value_trimmed = value.trim();
        let base_color = if control.invalid { accent } else { style.color };
        let placeholder_color = base_color.with_alpha(0.6);
        let mut generated: Option<String> = None;
        let (text, color) = match kind {
          TextControlKind::Password => {
            if !value_trimmed.is_empty() {
              let mask_len = value_trimmed.chars().count().clamp(3, 50);
              generated = Some("•".repeat(mask_len));
              (generated.as_deref().unwrap(), base_color)
            } else if let Some(ph) = placeholder.as_deref() {
              (ph.trim(), placeholder_color)
            } else {
              return true;
            }
          }
          TextControlKind::Number => {
            if !value_trimmed.is_empty() {
              (value_trimmed, base_color)
            } else if let Some(ph) = placeholder.as_deref() {
              (ph.trim(), placeholder_color)
            } else {
              return true;
            }
          }
          TextControlKind::Date => {
            if !value_trimmed.is_empty() {
              (value_trimmed, base_color)
            } else if let Some(ph) = placeholder.as_deref() {
              (ph.trim(), placeholder_color)
            } else {
              ("yyyy-mm-dd", placeholder_color)
            }
          }
          TextControlKind::Plain => {
            if !value_trimmed.is_empty() {
              (value_trimmed, base_color)
            } else if let Some(ph) = placeholder.as_deref() {
              (ph.trim(), placeholder_color)
            } else {
              return true;
            }
          }
        };

        let mut text_style = style.clone();
        text_style.color = color;
        let mut rect = inset_rect(rect, 2.0);
        let mut affordance_space = 0.0;
        match kind {
          TextControlKind::Number => affordance_space = 14.0,
          TextControlKind::Date => affordance_space = 12.0,
          _ => {}
        }
        if affordance_space > 0.0 {
          rect = Rect::from_xywh(
            rect.x(),
            rect.y(),
            (rect.width() - affordance_space).max(0.0),
            rect.height(),
          );
        }
        let _ = self.emit_text_with_style(text, Some(&text_style), rect);
        if affordance_space > 0.0 {
          let mut affordance_style = style.clone();
          affordance_style.color = muted_accent;
          let affordance_rect = Rect::from_xywh(
            rect.x() + rect.width(),
            rect.y(),
            affordance_space,
            rect.height(),
          );
          match kind {
            TextControlKind::Number => {
              let half = affordance_rect.height() / 2.0;
              let upper = Rect::from_xywh(
                affordance_rect.x(),
                affordance_rect.y(),
                affordance_rect.width(),
                half,
              );
              let lower = Rect::from_xywh(
                affordance_rect.x(),
                affordance_rect.y() + half,
                affordance_rect.width(),
                affordance_rect.height() - half,
              );
              let _ = self.emit_text_with_style("▲", Some(&affordance_style), upper);
              let _ = self.emit_text_with_style("▼", Some(&affordance_style), lower);
            }
            TextControlKind::Date => {
              let _ = self.emit_text_with_style("▾", Some(&affordance_style), affordance_rect);
            }
            _ => {}
          }
        }
        true
      }
      FormControlKind::TextArea { value, .. } => {
        if value.is_empty() {
          return true;
        }
        let rect = inset_rect(rect, 2.0);
        let mut text_style = style.clone();
        if control.invalid {
          text_style.color = accent;
        }
        let line_height = compute_line_height_with_metrics_viewport(
          &text_style,
          None,
          self.viewport.map(|(w, h)| Size::new(w, h)),
        );
        let mut y = rect.y();
        for line in value.split('\n') {
          if y > rect.y() + rect.height() {
            break;
          }
          let line_rect = Rect::from_xywh(rect.x(), y, rect.width(), rect.height());
          let _ = self.emit_text_with_style(line.trim_end(), Some(&text_style), line_rect);
          y += line_height;
        }
        true
      }
      FormControlKind::Select { label, .. } => {
        let rect = inset_rect(rect, 2.0);
        let arrow_space = if matches!(control.appearance, Appearance::None) {
          0.0
        } else {
          14.0
        };
        let mut select_style = style.clone();
        if control.invalid {
          select_style.color = accent;
        }
        let text_rect = Rect::from_xywh(
          rect.x(),
          rect.y(),
          (rect.width() - arrow_space).max(0.0),
          rect.height(),
        );
        let _ = self.emit_text_with_style(label, Some(&select_style), text_rect);

        if arrow_space > 0.0 {
          let mut arrow_style = select_style;
          arrow_style.color = muted_accent;
          let arrow_rect = Rect::from_xywh(
            rect.x() + rect.width() - arrow_space,
            rect.y(),
            arrow_space,
            rect.height(),
          );
          let _ = self.emit_text_with_style("▾", Some(&arrow_style), arrow_rect);
        }
        true
      }
      FormControlKind::Button { label } => {
        if label.trim().is_empty() {
          return true;
        }
        let rect = inset_rect(rect, 2.0);
        let mut button_style = style.clone();
        if control.invalid {
          button_style.color = accent;
        }
        let _ = self.emit_text_with_style(label, Some(&button_style), rect);
        true
      }
      FormControlKind::Checkbox {
        is_radio,
        checked,
        indeterminate,
      } => {
        if (!*checked && !*indeterminate) || matches!(control.appearance, Appearance::None) {
          return true;
        }
        let mut mark_style = style.clone();
        mark_style.color = muted_accent;
        let glyph = if *is_radio {
          "●"
        } else if *indeterminate {
          "−"
        } else {
          "✓"
        };
        let rect = inset_rect(rect, 2.0);
        let _ = self.emit_text_with_style(glyph, Some(&mark_style), rect);
        true
      }
      FormControlKind::Range { value, min, max } => {
        let track_height = 4.0_f32.min(rect.height());
        let track_y = rect.y() + (rect.height() - track_height) / 2.0;
        let track_rect = Rect::from_xywh(rect.x(), track_y, rect.width(), track_height);
        self
          .list
          .push(DisplayItem::FillRoundedRect(FillRoundedRectItem {
            rect: track_rect,
            color: style
              .background_color
              .with_alpha((style.background_color.a * 0.8).max(0.1)),
            radii: BorderRadii::uniform(track_height / 2.0),
          }));

        let min_val = min.unwrap_or(0.0);
        let max_val = max.unwrap_or(100.0);
        let span = (max_val - min_val).abs().max(0.0001);
        let clamped = ((*value - min_val) / span).clamp(0.0, 1.0);
        let knob_radius = (rect.height().min(16.0)) / 2.0;
        let knob_center_x = rect.x() + knob_radius + clamped * (rect.width() - 2.0 * knob_radius);
        let knob_rect = Rect::from_xywh(
          knob_center_x - knob_radius,
          rect.y() + (rect.height() - knob_radius * 2.0) / 2.0,
          knob_radius * 2.0,
          knob_radius * 2.0,
        );
        self
          .list
          .push(DisplayItem::FillRoundedRect(FillRoundedRectItem {
            rect: knob_rect,
            color: muted_accent,
            radii: BorderRadii::uniform(knob_radius),
          }));
        true
      }
      FormControlKind::Color { value, raw } => {
        let rect = inset_rect(rect, 3.0);
        self
          .list
          .push(DisplayItem::FillRoundedRect(FillRoundedRectItem {
            rect,
            color: *value,
            radii: BorderRadii::uniform((rect.height().min(rect.width()) / 5.0).max(2.0)),
          }));
        let luminance =
          (0.299 * value.r as f32 + 0.587 * value.g as f32 + 0.114 * value.b as f32) / 255.0;
        let text_color = if luminance > 0.5 {
          Rgba {
            r: 24,
            g: 24,
            b: 24,
            a: 1.0,
          }
        } else {
          Rgba {
            r: 245,
            g: 245,
            b: 245,
            a: 1.0,
          }
        };
        let mut text_style = style.clone();
        text_style.color = text_color;
        let label = raw
          .clone()
          .unwrap_or_else(|| format!("#{:02X}{:02X}{:02X}", value.r, value.g, value.b));
        let _ = self.emit_text_with_style(&label, Some(&text_style), rect);
        true
      }
      FormControlKind::Unknown { label } => {
        if let Some(text) = label {
          let rect = inset_rect(rect, 2.0);
          let mut unknown_style = style.clone();
          if control.invalid {
            unknown_style.color = accent;
          }
          let _ = self.emit_text_with_style(text, Some(&unknown_style), rect);
        }
        true
      }
    }
  }

  fn emit_alt_text(&mut self, alt: &str, fragment: &FragmentNode, rect: Rect) -> bool {
    self.emit_text_with_style(alt, fragment.style.as_deref(), rect)
  }

  fn emit_text_with_style(
    &mut self,
    text: &str,
    style: Option<&ComputedStyle>,
    rect: Rect,
  ) -> bool {
    let text = text.trim();
    if text.is_empty() {
      return false;
    }

    let Some(style) = style else {
      return self.emit_naive_text(text, rect, None);
    };

    let mut runs = match self.shaper.shape(text, style, &self.font_ctx) {
      Ok(r) => r,
      Err(_) => return self.emit_naive_text(text, rect, Some(style)),
    };
    InlineTextItem::apply_spacing_to_runs(
      &mut runs,
      text,
      style.letter_spacing,
      style.word_spacing,
    );

    let metrics_scaled = Self::resolve_scaled_metrics(style, &self.font_ctx);
    let viewport = self.viewport.map(|(w, h)| Size::new(w, h));
    let line_height =
      compute_line_height_with_metrics_viewport(style, metrics_scaled.as_ref(), viewport);
    let metrics = InlineTextItem::metrics_from_runs(&runs, line_height, style.font_size);
    let half_leading = (metrics.line_height - (metrics.ascent + metrics.descent)) / 2.0;
    let baseline = rect.y() + half_leading + metrics.baseline_offset;

    let shadows = Self::text_shadows_from_style(Some(style));
    self.emit_shaped_runs(
      &runs,
      style.color,
      baseline,
      rect.x(),
      &shadows,
      Some(style),
      false,
    );
    true
  }

  fn emit_naive_text(&mut self, text: &str, rect: Rect, style: Option<&ComputedStyle>) -> bool {
    let font_size = style.map(|s| s.font_size).unwrap_or(16.0);
    let color = style.map(|s| s.color).unwrap_or(Rgba::BLACK);
    let shadows = Self::text_shadows_from_style(style);
    let char_width = font_size * 0.6;
    let origin = Point::new(rect.x(), rect.y() + font_size * 0.8);
    let glyphs: Vec<GlyphInstance> = text
      .chars()
      .enumerate()
      .map(|(i, _)| GlyphInstance {
        glyph_id: 0,
        offset: Point::new(i as f32 * char_width, 0.0),
        advance: char_width,
      })
      .collect();
    let advance_width = text.len() as f32 * char_width;

    self.list.push(DisplayItem::Text(TextItem {
      origin,
      glyphs,
      color,
      shadows,
      font_size,
      advance_width,
      font_id: None,
      synthetic_bold: 0.0,
      synthetic_oblique: 0.0,
      emphasis: None,
      decorations: Vec::new(),
    }));
    true
  }

  fn decode_image(
    &self,
    src: &str,
    style: Option<&ComputedStyle>,
    decorative: bool,
  ) -> Option<ImageData> {
    let cache = self.image_cache.as_ref()?;
    let image = match cache.load(src) {
      Ok(img) => img,
      Err(_) if src.trim_start().starts_with('<') => cache.render_svg(src).ok()?,
      Err(_) => return None,
    };
    let image_resolution = style.map(|s| s.image_resolution).unwrap_or_default();
    let orientation = style
      .map(|s| s.image_orientation.resolve(image.orientation, decorative))
      .unwrap_or_else(|| ImageOrientation::default().resolve(image.orientation, decorative));
    let (css_w, css_h) = image.css_dimensions(
      orientation,
      &image_resolution,
      self.device_pixel_ratio,
      None,
    )?;
    let rgba = image.to_oriented_rgba(orientation);
    let (w, h) = rgba.dimensions();
    if w == 0 || h == 0 {
      return None;
    }
    Some(ImageData::new(w, h, css_w, css_h, rgba.into_raw()))
  }

  fn image_filter_quality(style: Option<&ComputedStyle>) -> ImageFilterQuality {
    match style.map(|s| s.image_rendering) {
      Some(ImageRendering::CrispEdges) | Some(ImageRendering::Pixelated) => {
        ImageFilterQuality::Nearest
      }
      _ => ImageFilterQuality::Linear,
    }
  }
}

#[derive(Debug, Clone)]
struct DecorationMetrics {
  underline_pos: f32,
  underline_thickness: f32,
  strike_pos: f32,
  strike_thickness: f32,
  ascent: f32,
  descent: f32,
}

impl DecorationMetrics {
  fn underline_position_with_offset(&self, base: f32, offset: f32) -> f32 {
    let direction = if base >= 0.0 { 1.0 } else { -1.0 };
    base + offset * direction
  }
}

fn collect_underline_exclusions(
  runs: &[ShapedRun],
  line_start: f32,
  baseline_y: f32,
  band_top: f32,
  band_bottom: f32,
  skip_all: bool,
) -> Vec<(f32, f32)> {
  let mut intervals = Vec::new();
  let tolerance = 0.5;

  let mut pen_x = line_start;
  for run in runs {
    let face = match ttf_parser::Face::parse(&run.font.data, run.font.index) {
      Ok(f) => f,
      Err(_) => continue,
    };
    let units_per_em = face.units_per_em() as f32;
    if units_per_em == 0.0 {
      continue;
    }
    let scale = run.font_size / units_per_em;
    let run_origin = if run.direction.is_rtl() {
      pen_x + run.advance
    } else {
      pen_x
    };

    for glyph in &run.glyphs {
      let glyph_x = match run.direction {
        crate::text::pipeline::Direction::RightToLeft => run_origin - glyph.x_offset,
        crate::text::pipeline::Direction::LeftToRight => run_origin + glyph.x_offset,
      };
      let glyph_y = baseline_y - glyph.y_offset;
      if let Some(bbox) = face.glyph_bounding_box(ttf_parser::GlyphId(glyph.glyph_id as u16)) {
        let left = glyph_x + bbox.x_min as f32 * scale - tolerance;
        let right = glyph_x + bbox.x_max as f32 * scale + tolerance;
        let top = glyph_y - bbox.y_max as f32 * scale - tolerance;
        let bottom = glyph_y - bbox.y_min as f32 * scale + tolerance;

        if skip_all || (bottom >= band_top && top <= band_bottom) {
          intervals.push((left, right));
        }
      }
    }

    pen_x += run.advance;
  }

  intervals
}

fn collect_underline_exclusions_vertical(
  runs: &[ShapedRun],
  inline_start: f32,
  block_baseline: f32,
  band_left: f32,
  band_right: f32,
  skip_all: bool,
) -> Vec<(f32, f32)> {
  let mut intervals = Vec::new();
  let mut pen_inline = inline_start;

  for run in runs {
    let face = match ttf_parser::Face::parse(&run.font.data, run.font.index) {
      Ok(f) => f,
      Err(_) => continue,
    };
    let units_per_em = face.units_per_em() as f32;
    if units_per_em == 0.0 {
      continue;
    }
    let scale = run.font_size / units_per_em * run.scale;
    let advance = run.advance;
    let run_origin = if run.direction.is_rtl() {
      pen_inline + advance
    } else {
      pen_inline
    };

    for glyph in &run.glyphs {
      let inline_pos = match run.direction {
        crate::text::pipeline::Direction::RightToLeft => run_origin - glyph.x_offset,
        crate::text::pipeline::Direction::LeftToRight => run_origin + glyph.x_offset,
      };
      let block_pos = block_baseline - glyph.y_offset;
      if let Some(bbox) = face.glyph_bounding_box(ttf_parser::GlyphId(glyph.glyph_id as u16)) {
        let inline_left = inline_pos + bbox.x_min as f32 * scale;
        let inline_right = inline_pos + bbox.x_max as f32 * scale;
        let block_top = block_pos - bbox.y_max as f32 * scale;
        let block_bottom = block_pos - bbox.y_min as f32 * scale;

        if skip_all || (block_bottom >= band_left && block_top <= band_right) {
          intervals.push((inline_left, inline_right));
        }
      }
    }

    pen_inline += advance;
  }

  intervals
}

fn subtract_intervals(total: (f32, f32), exclusions: &mut [(f32, f32)]) -> Vec<(f32, f32)> {
  exclusions.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
  let mut start = total.0;
  let mut allowed = Vec::new();

  for &(ex_start, ex_end) in exclusions.iter() {
    if ex_end <= start {
      continue;
    }
    if ex_start > total.1 {
      break;
    }
    let seg_end = ex_start.min(total.1);
    if seg_end > start {
      allowed.push((start, seg_end));
    }
    start = ex_end.max(start);
    if start >= total.1 {
      break;
    }
  }

  if start < total.1 {
    allowed.push((start, total.1));
  }

  allowed
}

impl Default for DisplayListBuilder {
  fn default() -> Self {
    Self::new()
  }
}

/// Resolves the computed `transform`/`perspective`/motion path into a 3D matrix for painting.
pub(crate) fn resolve_transform3d(
  style: &ComputedStyle,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
) -> Option<Transform3D> {
  DisplayListBuilder::build_transform(style, bounds, viewport).self_transform
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
  use super::*;
  use crate::css::types::Declaration;
  use crate::css::types::PropertyValue;
  use crate::css::types::Transform;
  use crate::image_loader::ImageCache;
  use crate::paint::display_list::ResolvedMaskImage;
  use crate::paint::stacking::StackingContext;
  use crate::paint::stacking::StackingContextReason;
  use crate::style::color::Color;
  use crate::style::color::Rgba;
  use crate::style::content::parse_content;
  use crate::style::content::ContentItem;
  use crate::style::content::ContentValue;
  use crate::style::display::Display;
  use crate::style::position::Position;
  use crate::style::properties::apply_declaration;
  use crate::style::properties::with_image_set_dpr;
  use crate::style::types::BackgroundImage;
  use crate::style::types::BackgroundLayer;
  use crate::style::types::BackgroundRepeat;
  use crate::style::types::BasicShape;
  use crate::style::types::ClipPath;
  use crate::style::types::ImageRendering;
  use crate::style::types::MixBlendMode;
  use crate::style::types::MotionPathCommand;
  use crate::style::types::MotionPosition;
  use crate::style::types::OffsetAnchor;
  use crate::style::types::OffsetPath;
  use crate::style::types::Overflow;
  use crate::style::types::TextDecorationLine;
  use crate::style::types::TransformBox;
  use crate::style::values::CalcLength;
  use crate::style::values::Length;
  use crate::style::values::LengthUnit;
  use crate::style::ComputedStyle;
  use crate::tree::box_tree::ReplacedType;
  use base64::engine::general_purpose;
  use base64::Engine as _;
  use image::codecs::png::PngEncoder;
  use image::ColorType;
  use image::ImageEncoder;
  use std::path::PathBuf;
  use std::sync::Arc;

  fn create_block_fragment(x: f32, y: f32, width: f32, height: f32) -> FragmentNode {
    FragmentNode::new_block(Rect::from_xywh(x, y, width, height), vec![])
  }

  fn create_text_fragment(x: f32, y: f32, width: f32, height: f32, text: &str) -> FragmentNode {
    FragmentNode::new_text(Rect::from_xywh(x, y, width, height), text.to_string(), 12.0)
  }

  fn data_url_for_color(color: [u8; 4]) -> String {
    let mut buf = Vec::new();
    PngEncoder::new(&mut buf)
      .write_image(&color, 1, 1, ColorType::Rgba8.into())
      .expect("encode png");
    format!(
      "data:image/png;base64,{}",
      general_purpose::STANDARD.encode(buf)
    )
  }

  fn create_image_fragment(x: f32, y: f32, width: f32, height: f32, src: &str) -> FragmentNode {
    FragmentNode::new_replaced(
      Rect::from_xywh(x, y, width, height),
      ReplacedType::Image {
        src: src.to_string(),
        alt: None,
        sizes: None,
        srcset: Vec::new(),
        picture_sources: Vec::new(),
      },
    )
  }

  fn text_fragment_at(x: f32, label: &str) -> FragmentNode {
    FragmentNode::new_text(Rect::from_xywh(x, 0.0, 10.0, 10.0), label.to_string(), 12.0)
  }

  fn stacking_clip_order(items: &[DisplayItem]) -> (usize, usize, usize, usize) {
    let push_sc = items
      .iter()
      .position(|item| matches!(item, DisplayItem::PushStackingContext(_)))
      .expect("stacking context push missing");
    let push_clip = items
      .iter()
      .position(|item| matches!(item, DisplayItem::PushClip(_)))
      .expect("clip push missing");
    let pop_clip = items
      .iter()
      .rposition(|item| matches!(item, DisplayItem::PopClip))
      .expect("clip pop missing");
    let pop_sc = items
      .iter()
      .rposition(|item| matches!(item, DisplayItem::PopStackingContext))
      .expect("stacking context pop missing");

    assert!(
      push_sc < push_clip,
      "clip should be emitted inside the stacking context"
    );
    assert!(push_clip < pop_clip, "clip should wrap painted items");
    assert!(
      pop_clip < pop_sc,
      "stacking context should be popped after its clips"
    );

    (push_sc, push_clip, pop_clip, pop_sc)
  }

  fn has_paint_between(items: &[DisplayItem], start: usize, end: usize) -> bool {
    items.iter().enumerate().any(|(idx, item)| {
      idx > start
        && idx < end
        && !matches!(
          item,
          DisplayItem::PushClip(_)
            | DisplayItem::PopClip
            | DisplayItem::PushStackingContext(_)
            | DisplayItem::PopStackingContext
        )
    })
  }

  #[test]
  fn test_builder_empty_fragment() {
    let fragment = create_block_fragment(0.0, 0.0, 100.0, 100.0);
    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);

    assert!(list.is_empty());
  }

  #[test]
  fn test_builder_text_fragment() {
    let fragment = create_text_fragment(10.0, 20.0, 100.0, 20.0, "Hello");
    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);

    assert_eq!(list.len(), 1);
    assert!(matches!(list.items()[0], DisplayItem::Text(_)));
  }

  #[test]
  fn test_builder_text_position() {
    let fragment = create_text_fragment(10.0, 20.0, 100.0, 20.0, "Hello");
    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);

    if let DisplayItem::Text(text) = &list.items()[0] {
      assert_eq!(text.origin.x, 10.0);
      assert_eq!(text.glyphs.len(), 5);
    } else {
      panic!("Expected Text item");
    }
  }

  #[test]
  fn parallel_builder_matches_sequential_output() {
    std::env::set_var("FASTR_DISPLAY_LIST_PARALLEL_MIN", "1");
    std::env::set_var("FASTR_DISPLAY_LIST_PARALLEL", "1");

    let child1 = create_text_fragment(0.0, 0.0, 20.0, 10.0, "One");
    let child2 = create_text_fragment(0.0, 10.0, 20.0, 10.0, "Two");
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), vec![child1, child2]);

    let parallel = DisplayListBuilder::new().build(&root);

    std::env::set_var("FASTR_DISPLAY_LIST_PARALLEL", "0");
    let sequential = DisplayListBuilder::new().build(&root);

    std::env::remove_var("FASTR_DISPLAY_LIST_PARALLEL_MIN");
    std::env::remove_var("FASTR_DISPLAY_LIST_PARALLEL");

    assert_eq!(parallel.len(), sequential.len());
    let parallel_debug: Vec<String> = parallel
      .items()
      .iter()
      .map(|item| format!("{:?}", item))
      .collect();
    let sequential_debug: Vec<String> = sequential
      .items()
      .iter()
      .map(|item| format!("{:?}", item))
      .collect();
    assert_eq!(parallel_debug, sequential_debug);
  }

  #[test]
  fn builder_emits_text_decorations() {
    let mut style = ComputedStyle::default();
    style.text_decoration.lines = TextDecorationLine::UNDERLINE;
    style.text_decoration.color = Some(Rgba::BLACK);
    let fragment = FragmentNode::new_text_styled(
      Rect::from_xywh(0.0, 0.0, 50.0, 16.0),
      "Hi".to_string(),
      12.0,
      Arc::new(style),
    );

    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);
    assert!(
      list
        .items()
        .iter()
        .any(|i| matches!(i, DisplayItem::TextDecoration(_))),
      "display list should include text decoration items"
    );
  }

  #[test]
  fn builder_text_decoration_uses_current_color_when_none() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::GREEN;
    style.text_decoration.lines = TextDecorationLine::UNDERLINE;
    // Leave text_decoration.color as None so it should resolve to currentColor.

    let fragment = FragmentNode::new_text_styled(
      Rect::from_xywh(0.0, 0.0, 50.0, 16.0),
      "Hi".to_string(),
      12.0,
      Arc::new(style),
    );

    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);

    let deco_color = list
      .items()
      .iter()
      .find_map(|item| {
        if let DisplayItem::TextDecoration(dec) = item {
          dec.decorations.first().map(|d| d.color)
        } else {
          None
        }
      })
      .expect("text decoration emitted");

    assert_eq!(deco_color, Rgba::GREEN);
  }

  #[test]
  fn builder_prefers_explicit_text_decoration_color() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::GREEN;
    style.text_decoration.lines = TextDecorationLine::UNDERLINE;
    style.text_decoration.color = Some(Rgba::BLUE);

    let fragment = FragmentNode::new_text_styled(
      Rect::from_xywh(0.0, 0.0, 50.0, 16.0),
      "Hi".to_string(),
      12.0,
      Arc::new(style),
    );

    let list = DisplayListBuilder::new().build(&fragment);
    let deco_color = list
      .items()
      .iter()
      .find_map(|item| match item {
        DisplayItem::TextDecoration(dec) => dec.decorations.first().map(|d| d.color),
        _ => None,
      })
      .expect("decoration color present");

    assert_eq!(deco_color, Rgba::BLUE);
  }

  #[test]
  fn builder_resolves_font_relative_underline_offset() {
    let mut style = ComputedStyle::default();
    style.text_decoration.lines = TextDecorationLine::UNDERLINE;
    style.text_decoration.color = Some(Rgba::BLACK);
    style.font_size = 20.0;

    let fragment = FragmentNode::new_text_styled(
      Rect::from_xywh(0.0, 0.0, 50.0, 16.0),
      "Hi".to_string(),
      12.0,
      Arc::new(style.clone()),
    );

    let list_auto = DisplayListBuilder::new().build(&fragment);
    let auto_center = list_auto
      .items()
      .iter()
      .find_map(|item| {
        if let DisplayItem::TextDecoration(dec) = item {
          dec
            .decorations
            .first()
            .and_then(|d| d.underline.as_ref())
            .map(|u| u.center)
        } else {
          None
        }
      })
      .expect("underline present");

    let mut ex_style = style;
    ex_style.text_underline_offset = TextUnderlineOffset::Length(Length::ex(1.0));
    let fragment_ex = FragmentNode::new_text_styled(
      Rect::from_xywh(0.0, 0.0, 50.0, 16.0),
      "Hi".to_string(),
      12.0,
      Arc::new(ex_style),
    );
    let list_ex = DisplayListBuilder::new().build(&fragment_ex);
    let ex_center = list_ex
      .items()
      .iter()
      .find_map(|item| {
        if let DisplayItem::TextDecoration(dec) = item {
          dec
            .decorations
            .first()
            .and_then(|d| d.underline.as_ref())
            .map(|u| u.center)
        } else {
          None
        }
      })
      .expect("underline present");

    assert!(
      ex_center > auto_center,
      "font-relative underline offset should move the underline further from the baseline"
    );
  }

  #[test]
  fn test_builder_image_fragment() {
    let fragment = create_image_fragment(
            0.0,
            0.0,
            100.0,
            100.0,
            "data:image/svg+xml,%3Csvg%20xmlns=%22http://www.w3.org/2000/svg%22%20width=%221%22%20height=%221%22%3E%3C/svg%3E",
        );
    let builder = DisplayListBuilder::with_image_cache(ImageCache::new());
    let list = builder.build(&fragment);

    assert_eq!(list.len(), 1);
    assert!(matches!(list.items()[0], DisplayItem::Image(_)));
  }

  #[test]
  fn default_builder_decodes_images_without_explicit_cache() {
    // 1x1 blue inline SVG
    let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" width="1" height="1"><rect width="1" height="1" fill="blue"/></svg>"#;
    let fragment = create_image_fragment(0.0, 0.0, 10.0, 10.0, svg);

    let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);

    let img = list
      .items()
      .iter()
      .find_map(|item| match item {
        DisplayItem::Image(img) => Some(img),
        _ => None,
      })
      .expect("expected image display item");
    assert_eq!(img.image.width, 1);
    assert_eq!(img.image.height, 1);
  }

  #[test]
  fn stacking_context_order_respected() {
    let mut root = StackingContext::root();
    root.layer5_inlines.push(text_fragment_at(20.0, "root"));

    let mut neg = StackingContext::with_reason(-1, StackingContextReason::PositionedWithZIndex, 1);
    neg.layer5_inlines.push(text_fragment_at(0.0, "neg"));

    let mut pos = StackingContext::with_reason(1, StackingContextReason::PositionedWithZIndex, 2);
    pos.layer5_inlines.push(text_fragment_at(40.0, "pos"));

    root.add_child(neg);
    root.add_child(pos);

    let list = DisplayListBuilder::new().build_from_stacking(&root);
    let origins: Vec<f32> = list
      .items()
      .iter()
      .filter_map(|item| match item {
        DisplayItem::Text(t) => Some(t.origin.x),
        _ => None,
      })
      .collect();

    assert_eq!(origins, vec![0.0, 20.0, 40.0]);
  }

  #[test]
  fn stacking_context_resolves_mask_images() {
    let mut style = ComputedStyle::default();
    let mut layer = crate::style::types::MaskLayer::default();
    layer.image = Some(BackgroundImage::Url(data_url_for_color([0, 0, 0, 0])));
    layer.repeat = BackgroundRepeat::no_repeat();
    style.set_mask_layers(vec![layer]);

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );
    let list =
      DisplayListBuilder::with_image_cache(ImageCache::new()).build_with_stacking_tree(&fragment);

    let push = list
      .items()
      .iter()
      .find_map(|item| match item {
        DisplayItem::PushStackingContext(ctx) => Some(ctx),
        _ => None,
      })
      .expect("stacking context emitted");
    let mask = push.mask.as_ref().expect("mask resolved");
    assert_eq!(mask.layers.len(), 1);
    match &mask.layers[0].image {
      ResolvedMaskImage::Raster(image) => {
        assert_eq!(image.width, 1);
        assert_eq!(image.height, 1);
      }
      other => panic!("expected raster mask image, got {other:?}"),
    }
  }

  #[test]
  fn build_with_stacking_tree_respects_z_order_from_styles() {
    fn styled_fragment(x: f32, label: &str, z: i32) -> FragmentNode {
      let mut style = ComputedStyle::default();
      style.position = Position::Relative;
      style.z_index = Some(z);
      FragmentNode::new_inline_styled(
        Rect::from_xywh(x, 0.0, 10.0, 10.0),
        0,
        vec![FragmentNode::new_text(
          Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
          label.to_string(),
          12.0,
        )],
        Arc::new(style),
      )
    }

    let child_neg = styled_fragment(0.0, "neg", -1);
    let child_zero = styled_fragment(20.0, "zero", 0);
    let child_pos = styled_fragment(40.0, "pos", 1);

    let root = FragmentNode::new_block(
      Rect::from_xywh(0.0, 0.0, 100.0, 20.0),
      vec![child_neg, child_zero, child_pos],
    );

    let list = DisplayListBuilder::new().build_with_stacking_tree(&root);
    let origins: Vec<f32> = list
      .items()
      .iter()
      .filter_map(|item| match item {
        DisplayItem::Text(t) => Some(t.origin.x),
        _ => None,
      })
      .collect();

    assert_eq!(origins, vec![0.0, 20.0, 40.0]);
  }

  #[test]
  fn stacking_context_opacity_wraps_entire_subtree() {
    let mut style = ComputedStyle::default();
    style.opacity = 0.5;
    style.background_color = Rgba::RED;

    let child = create_text_fragment(0.0, 0.0, 10.0, 10.0, "child");
    let root = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      vec![child],
      Arc::new(style),
    );

    let list = DisplayListBuilder::new().build_with_stacking_tree(&root);
    let items = list.items();

    let push_opacity = items
      .iter()
      .position(|item| matches!(item, DisplayItem::PushOpacity(_)))
      .expect("expected push opacity");
    let pop_opacity = items
      .iter()
      .rposition(|item| matches!(item, DisplayItem::PopOpacity))
      .expect("expected pop opacity");

    let background = items
      .iter()
      .position(|item| matches!(item, DisplayItem::FillRect(_)))
      .expect("expected root background");
    let text = items
      .iter()
      .position(|item| matches!(item, DisplayItem::Text(_)))
      .expect("expected child text");

    assert_eq!(
      items
        .iter()
        .filter(|item| matches!(item, DisplayItem::PushOpacity(_)))
        .count(),
      1
    );
    assert_eq!(
      items
        .iter()
        .filter(|item| matches!(item, DisplayItem::PopOpacity))
        .count(),
      1
    );
    assert!(push_opacity < background && background < text && text < pop_opacity);
    if let DisplayItem::PushOpacity(opacity) = &items[push_opacity] {
      assert!((opacity.opacity - 0.5).abs() < f32::EPSILON);
    }
  }

  #[test]
  fn stacking_context_plane_rect_uses_root_fragment_bounds() {
    let mut style = ComputedStyle::default();
    style
      .transform
      .push(Transform::Translate(Length::px(0.0), Length::px(0.0)));

    let child = FragmentNode::new_block(Rect::from_xywh(80.0, 80.0, 50.0, 50.0), vec![]);
    let root = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      vec![child],
      Arc::new(style),
    );

    let list = DisplayListBuilder::new().build_with_stacking_tree(&root);
    let stacking = list
      .items()
      .iter()
      .find_map(|item| match item {
        DisplayItem::PushStackingContext(context) => Some(context),
        _ => None,
      })
      .expect("stacking context present");

    assert_eq!(stacking.bounds, Rect::from_xywh(0.0, 0.0, 130.0, 130.0));
    assert_eq!(stacking.plane_rect, Rect::from_xywh(0.0, 0.0, 100.0, 100.0));
  }

  #[test]
  fn stacking_context_transform_origin_uses_plane_rect() {
    let mut style = ComputedStyle::default();
    style.transform.push(Transform::Scale(2.0, 2.0));

    let child = FragmentNode::new_block(Rect::from_xywh(80.0, 80.0, 50.0, 50.0), vec![]);
    let root = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      vec![child],
      Arc::new(style),
    );

    let list = DisplayListBuilder::new().build_with_stacking_tree(&root);
    let stacking = list
      .items()
      .iter()
      .find_map(|item| match item {
        DisplayItem::PushStackingContext(context) => Some(context),
        _ => None,
      })
      .expect("stacking context present");

    let transform = stacking.transform.as_ref().expect("transform present");
    let transform = transform.to_2d().expect("transform should be 2d");
    assert!((transform.a - 2.0).abs() < 1e-3);
    assert!((transform.e + 50.0).abs() < 1e-3);
    assert!((transform.f + 50.0).abs() < 1e-3);
  }

  #[test]
  fn stacking_context_offsets_include_non_context_ancestors() {
    let text = FragmentNode::new_text(
      Rect::from_xywh(3.0, 4.0, 10.0, 10.0),
      "hello".to_string(),
      0.0,
    );

    let mut stacking_style = ComputedStyle::default();
    stacking_style.opacity = 0.5;
    let stacking = FragmentNode::new_block_styled(
      Rect::from_xywh(5.0, 6.0, 20.0, 20.0),
      vec![text],
      Arc::new(stacking_style),
    );

    let intermediate =
      FragmentNode::new_block(Rect::from_xywh(10.0, 20.0, 50.0, 50.0), vec![stacking]);

    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![intermediate]);

    let list = DisplayListBuilder::new().build_with_stacking_tree(&root);
    let text_item = list.items().iter().find_map(|item| match item {
      DisplayItem::Text(t) => Some(t),
      _ => None,
    });

    let text_item = text_item.expect("text item emitted");
    assert_eq!(text_item.origin.x, 10.0 + 5.0 + 3.0);
    assert_eq!(text_item.origin.y, 20.0 + 6.0 + 4.0);
  }

  #[test]
  fn zero_z_contexts_interleave_with_positioned_descendants() {
    let stacking_text = FragmentNode::new_text(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      "stack".to_string(),
      12.0,
    );
    let mut stacking_style = ComputedStyle::default();
    stacking_style.opacity = 0.5;
    let stacking_context = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![stacking_text],
      Arc::new(stacking_style),
    );

    let positioned_text = FragmentNode::new_text(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      "pos".to_string(),
      12.0,
    );
    let mut positioned_style = ComputedStyle::default();
    positioned_style.position = Position::Relative;
    let positioned = FragmentNode::new_block_styled(
      Rect::from_xywh(20.0, 0.0, 10.0, 10.0),
      vec![positioned_text],
      Arc::new(positioned_style),
    );

    let root = FragmentNode::new_block(
      Rect::from_xywh(0.0, 0.0, 100.0, 20.0),
      vec![stacking_context, positioned],
    );

    let list = DisplayListBuilder::new().build_with_stacking_tree(&root);
    let origins: Vec<f32> = list
      .items()
      .iter()
      .filter_map(|item| match item {
        DisplayItem::Text(t) => Some(t.origin.x),
        _ => None,
      })
      .collect();

    assert_eq!(origins, vec![0.0, 20.0]);
  }

  #[test]
  fn test_builder_nested_fragments() {
    let child1 = create_text_fragment(0.0, 0.0, 50.0, 20.0, "One");
    let child2 = create_text_fragment(0.0, 20.0, 50.0, 20.0, "Two");
    let parent = FragmentNode::new_block(
      Rect::from_xywh(10.0, 10.0, 100.0, 50.0),
      vec![child1, child2],
    );

    let builder = DisplayListBuilder::new();
    let list = builder.build(&parent);

    assert_eq!(list.len(), 2);
  }

  #[test]
  fn test_builder_position_offset() {
    let child = create_text_fragment(10.0, 10.0, 50.0, 20.0, "Hi");
    let parent = FragmentNode::new_block(Rect::from_xywh(20.0, 20.0, 100.0, 50.0), vec![child]);

    let builder = DisplayListBuilder::new();
    let list = builder.build(&parent);

    if let DisplayItem::Text(text) = &list.items()[0] {
      assert_eq!(text.origin.x, 30.0);
    } else {
      panic!("Expected Text item");
    }
  }

  #[test]
  fn test_builder_with_clips() {
    let child = create_text_fragment(0.0, 0.0, 50.0, 20.0, "Clipped");
    let parent =
      FragmentNode::new_block_with_id(Rect::from_xywh(0.0, 0.0, 100.0, 50.0), 42, vec![child]);

    let mut clips = HashSet::new();
    clips.insert(Some(42));

    let builder = DisplayListBuilder::new();
    let list = builder.build_with_clips(&parent, &clips);

    assert_eq!(list.len(), 3);
    assert!(matches!(list.items()[0], DisplayItem::PushClip(_)));
    assert!(matches!(list.items()[1], DisplayItem::Text(_)));
    assert!(matches!(list.items()[2], DisplayItem::PopClip));
  }

  #[test]
  fn test_builder_no_clips() {
    let child = create_text_fragment(0.0, 0.0, 50.0, 20.0, "NotClipped");
    let parent = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 50.0), vec![child]);

    let clips = HashSet::new();

    let builder = DisplayListBuilder::new();
    let list = builder.build_with_clips(&parent, &clips);

    assert_eq!(list.len(), 1);
    assert!(matches!(list.items()[0], DisplayItem::Text(_)));
  }

  #[test]
  fn clip_property_emits_clip_item() {
    let mut style = ComputedStyle::default();
    style.clip = Some(crate::style::types::ClipRect {
      top: crate::style::types::ClipComponent::Length(Length::px(5.0)),
      right: crate::style::types::ClipComponent::Length(Length::px(15.0)),
      bottom: crate::style::types::ClipComponent::Length(Length::px(15.0)),
      left: crate::style::types::ClipComponent::Length(Length::px(5.0)),
    });
    style.background_color = Rgba::RED;
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      vec![],
      Arc::new(style),
    );

    let builder = DisplayListBuilder::new();
    let list = builder.build_with_stacking_tree(&fragment);

    assert!(list
      .items()
      .iter()
      .any(|item| matches!(item, DisplayItem::PushClip(_))));
  }

  #[test]
  fn overflow_hidden_emits_clip_item() {
    let mut style = ComputedStyle::default();
    style.overflow_x = crate::style::types::Overflow::Hidden;
    style.overflow_y = crate::style::types::Overflow::Hidden;
    style.background_color = Rgba::BLUE;
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      vec![],
      Arc::new(style),
    );

    let builder = DisplayListBuilder::new();
    let list = builder.build_with_stacking_tree(&fragment);

    assert!(list
      .items()
      .iter()
      .any(|item| matches!(item, DisplayItem::PushClip(_))));
  }

  #[test]
  fn clip_path_clip_is_inside_stacking_context() {
    let mut style = ComputedStyle::default();
    style.clip_path = ClipPath::BasicShape(
      Box::new(BasicShape::Inset {
        top: Length::px(2.0),
        right: Length::px(2.0),
        bottom: Length::px(2.0),
        left: Length::px(2.0),
        border_radius: Box::new(None),
      }),
      None,
    );
    style.background_color = Rgba::RED;
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      vec![],
      Arc::new(style),
    );

    let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
    let items = list.items();
    let (_, push_clip, pop_clip, _) = stacking_clip_order(items);

    assert!(
      has_paint_between(items, push_clip, pop_clip),
      "expected paint between clip operations inside stacking context"
    );
  }

  #[test]
  fn transformed_overflow_clip_is_inside_stacking_context() {
    let mut style = ComputedStyle::default();
    style.transform = vec![Transform::Translate(Length::px(5.0), Length::px(0.0))];
    style.overflow_x = Overflow::Hidden;
    style.overflow_y = Overflow::Hidden;
    style.background_color = Rgba::GREEN;

    let mut child_style = ComputedStyle::default();
    child_style.background_color = Rgba::RED;
    let child = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 40.0, 40.0),
      vec![],
      Arc::new(child_style),
    );

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 30.0, 30.0),
      vec![child],
      Arc::new(style),
    );

    let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
    let items = list.items();
    let (push_sc, push_clip, pop_clip, _pop_sc) = stacking_clip_order(items);

    if let DisplayItem::PushStackingContext(stacking) = &items[push_sc] {
      assert!(
        stacking.transform.is_some(),
        "stacking context should carry transform"
      );
    } else {
      panic!("expected stacking context push item");
    }
    assert!(
      has_paint_between(items, push_clip, pop_clip),
      "expected painted content to be clipped inside stacking context"
    );
  }

  #[test]
  fn test_emit_background() {
    let mut builder = DisplayListBuilder::new();
    builder.emit_background(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), Rgba::RED);

    let list = builder.list;
    assert_eq!(list.len(), 1);
    assert!(matches!(list.items()[0], DisplayItem::FillRect(_)));
  }

  #[test]
  fn test_emit_background_transparent_skipped() {
    let mut builder = DisplayListBuilder::new();
    builder.emit_background(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), Rgba::TRANSPARENT);

    let list = builder.list;
    assert!(list.is_empty());
  }

  #[test]
  fn background_blend_mode_emits_push_and_pop() {
    let mut style = ComputedStyle::default();
    style.background_color = Rgba::BLUE;
    style.set_background_layers(vec![BackgroundLayer {
      image: Some(BackgroundImage::LinearGradient {
        angle: 0.0,
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::RED),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::RED),
            position: Some(1.0),
          },
        ],
      }),
      repeat: BackgroundRepeat::no_repeat(),
      blend_mode: MixBlendMode::Multiply,
      ..BackgroundLayer::default()
    }]);
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 10.0),
      vec![],
      Arc::new(style),
    );

    let list = DisplayListBuilder::new().build(&fragment);
    let mut push_idx = None;
    let mut gradient_idx = None;
    let mut pop_idx = None;
    for (idx, item) in list.items().iter().enumerate() {
      match item {
        DisplayItem::PushBlendMode(mode) => {
          push_idx = Some(idx);
          assert_eq!(mode.mode, BlendMode::Multiply);
        }
        DisplayItem::LinearGradient(_) => gradient_idx = Some(idx),
        DisplayItem::PopBlendMode => pop_idx = Some(idx),
        _ => {}
      }
    }

    assert!(push_idx.is_some(), "blend push missing");
    assert!(pop_idx.is_some(), "blend pop missing");
    assert!(gradient_idx.is_some(), "background gradient missing");
    assert!(
      push_idx.unwrap() < gradient_idx.unwrap() && gradient_idx.unwrap() < pop_idx.unwrap(),
      "blend mode should wrap background layer"
    );
  }

  #[test]
  fn background_url_resolves_relative_to_base() {
    // Create a 1x1 PNG on disk.
    let mut path: PathBuf = std::env::temp_dir();
    path.push(format!(
      "fastrender_dl_base_url_{}_{}.png",
      std::process::id(),
      std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos()
    ));
    let img = image::RgbaImage::from_raw(1, 1, vec![0, 0, 0, 255]).expect("raw rgba");
    img.save(&path).expect("write png");

    let dir = path.parent().unwrap().to_path_buf();
    let base_url = format!("file://{}", dir.display());

    let mut style = ComputedStyle::default();
    style.set_background_layers(vec![BackgroundLayer {
      image: Some(BackgroundImage::Url(
        path.file_name().unwrap().to_str().unwrap().to_string(),
      )),
      repeat: BackgroundRepeat::no_repeat(),
      ..BackgroundLayer::default()
    }]);

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );
    let list = DisplayListBuilder::new()
      .with_base_url(base_url)
      .build(&fragment);

    // Expect one image item in the list (background image decoded).
    assert!(
      list
        .items()
        .iter()
        .any(|item| matches!(item, DisplayItem::Image(_))),
      "background image should decode via base URL"
    );
  }

  #[test]
  fn background_image_set_respects_device_pixel_ratio() {
    let low = data_url_for_color([255, 0, 0, 255]);
    let high = data_url_for_color([0, 255, 0, 255]);

    let mut style = ComputedStyle::default();
    with_image_set_dpr(2.0, || {
      apply_declaration(
        &mut style,
        &Declaration {
          property: "background-image".to_string(),
          value: PropertyValue::Keyword(format!(
            "image-set(url(\"{}\") 1x, url(\"{}\") 2x)",
            low, high
          )),
          raw_value: String::new(),
          important: false,
        },
        &ComputedStyle::default(),
        16.0,
        16.0,
      );
    });
    style.background_color = Rgba::TRANSPARENT;

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );

    let list = DisplayListBuilder::new().build(&fragment);
    let image = list
      .items()
      .iter()
      .find_map(|item| match item {
        DisplayItem::Image(img) => Some(img),
        _ => None,
      })
      .expect("background image should emit an image item");

    assert_eq!(&image.image.pixels[..4], &[0, 255, 0, 255]);
  }

  #[test]
  fn content_image_set_respects_device_pixel_ratio() {
    let low = data_url_for_color([255, 0, 0, 255]);
    let high = data_url_for_color([0, 0, 255, 255]);

    let content = with_image_set_dpr(2.0, || {
      parse_content(&format!(
        "image-set(url(\"{}\") 1x, url(\"{}\") 2x)",
        low, high
      ))
      .expect("parse content")
    });

    let chosen = match content {
      ContentValue::Items(items) if items.len() == 1 => match &items[0] {
        ContentItem::Url(url) => url.clone(),
        other => panic!("unexpected content item: {other:?}"),
      },
      other => panic!("unexpected content value: {other:?}"),
    };

    let fragment = FragmentNode::new_replaced(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      ReplacedType::Image {
        src: chosen,
        alt: None,
        sizes: None,
        srcset: Vec::new(),
        picture_sources: Vec::new(),
      },
    );

    let list = DisplayListBuilder::new().build(&fragment);
    let image = list
      .items()
      .iter()
      .find_map(|item| match item {
        DisplayItem::Image(img) => Some(img),
        _ => None,
      })
      .expect("content image should emit an image item");

    assert_eq!(&image.image.pixels[..4], &[0, 0, 255, 255]);
  }

  #[test]
  fn list_style_image_image_set_respects_device_pixel_ratio() {
    let low = data_url_for_color([255, 0, 0, 255]);
    let high = data_url_for_color([0, 255, 0, 255]);

    let mut style = ComputedStyle::default();
    with_image_set_dpr(2.0, || {
      apply_declaration(
        &mut style,
        &Declaration {
          property: "list-style-image".to_string(),
          value: PropertyValue::Keyword(format!(
            "image-set(url(\"{}\") 1x, url(\"{}\") 2x)",
            low, high
          )),
          raw_value: String::new(),
          important: false,
        },
        &ComputedStyle::default(),
        16.0,
        16.0,
      );
    });

    let chosen = match &style.list_style_image {
      crate::style::types::ListStyleImage::Url(url) => url.clone(),
      crate::style::types::ListStyleImage::None => panic!("unexpected list-style-image: None"),
    };

    let mut fragment = FragmentNode::new_replaced(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      ReplacedType::Image {
        src: chosen,
        alt: None,
        sizes: None,
        srcset: Vec::new(),
        picture_sources: Vec::new(),
      },
    );
    fragment.style = Some(Arc::new(style));

    let list = DisplayListBuilder::with_image_cache(ImageCache::new()).build(&fragment);
    let image = list
      .items()
      .iter()
      .find_map(|item| match item {
        DisplayItem::Image(img) => Some(img),
        _ => None,
      })
      .expect("marker image should emit an image item");

    assert_eq!(&image.image.pixels[..4], &[0, 255, 0, 255]);
  }

  #[test]
  fn test_emit_border() {
    let mut builder = DisplayListBuilder::new();
    builder.emit_border(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), 2.0, Rgba::BLACK);

    let list = builder.list;
    assert_eq!(list.len(), 1);
    assert!(matches!(list.items()[0], DisplayItem::StrokeRect(_)));
  }

  #[test]
  fn test_push_pop_opacity() {
    let mut builder = DisplayListBuilder::new();
    builder.push_opacity(0.5);
    builder.emit_background(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), Rgba::RED);
    builder.pop_opacity();

    let list = builder.list;
    assert_eq!(list.len(), 3);
    assert!(matches!(list.items()[0], DisplayItem::PushOpacity(_)));
    assert!(matches!(list.items()[1], DisplayItem::FillRect(_)));
    assert!(matches!(list.items()[2], DisplayItem::PopOpacity));
  }

  #[test]
  fn fragments_with_zero_opacity_emit_nothing() {
    let mut frag = create_block_fragment(0.0, 0.0, 50.0, 50.0);
    let mut style = ComputedStyle::default();
    style.opacity = 0.0;
    frag.style = Some(Arc::new(style));

    let list = DisplayListBuilder::new().build(&frag);
    assert!(list.is_empty(), "zero-opacity fragments should be skipped");
  }

  #[test]
  fn test_push_pop_clip() {
    let mut builder = DisplayListBuilder::new();
    builder.push_clip(Rect::from_xywh(0.0, 0.0, 50.0, 50.0));
    builder.emit_background(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), Rgba::RED);
    builder.pop_clip();

    let list = builder.list;
    assert_eq!(list.len(), 3);
    assert!(matches!(list.items()[0], DisplayItem::PushClip(_)));
    assert!(matches!(list.items()[1], DisplayItem::FillRect(_)));
    assert!(matches!(list.items()[2], DisplayItem::PopClip));
  }

  #[test]
  fn test_fragment_tree_wrapper() {
    let child = create_text_fragment(10.0, 10.0, 50.0, 20.0, "Tree");
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 50.0), vec![child]);
    let tree = FragmentTree::new(root);

    let builder = DisplayListBuilder::new();
    let list = builder.build_tree(&tree);

    assert_eq!(list.len(), 1);
  }

  #[test]
  fn test_empty_text_skipped() {
    let fragment = create_text_fragment(0.0, 0.0, 100.0, 20.0, "");
    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);

    assert!(list.is_empty());
  }

  #[test]
  fn visibility_hidden_skips_display_items() {
    let mut style = ComputedStyle::default();
    style.visibility = crate::style::computed::Visibility::Hidden;
    let fragment = FragmentNode::new_text_styled(
      Rect::from_xywh(0.0, 0.0, 100.0, 20.0),
      "hidden".to_string(),
      16.0,
      Arc::new(style),
    );
    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);

    assert!(list.is_empty());
  }

  #[test]
  fn outline_emits_stroke_rect() {
    let mut style = ComputedStyle::default();
    style.outline_style = crate::style::types::OutlineStyle::Solid;
    style.outline_width = Length::px(2.0);
    style.outline_color = crate::style::types::OutlineColor::Color(Rgba::RED);
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );

    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);
    assert!(
      list
        .items()
        .iter()
        .any(|item| matches!(item, DisplayItem::Outline(_))),
      "outline should emit outline item"
    );
  }

  #[test]
  fn outline_emits_even_when_clipped() {
    let mut style = ComputedStyle::default();
    style.outline_style = crate::style::types::OutlineStyle::Solid;
    style.outline_width = Length::px(2.0);
    style.outline_color = crate::style::types::OutlineColor::Color(Rgba::RED);
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );
    let clips = vec![None].into_iter().collect();
    let builder = DisplayListBuilder::new();
    let list = builder.build_with_clips(&fragment, &clips);
    assert!(
      list
        .items()
        .iter()
        .any(|item| matches!(item, DisplayItem::Outline(_))),
      "outline should be emitted even when fragment is clipped"
    );
  }

  #[test]
  fn hidden_fragment_skipped_with_clips() {
    let mut style = ComputedStyle::default();
    style.visibility = crate::style::computed::Visibility::Hidden;
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![FragmentNode::new_text(
        Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
        "hidden".to_string(),
        12.0,
      )],
      Arc::new(style),
    );
    let clips = HashSet::from([None]);
    let builder = DisplayListBuilder::new();
    let list = builder.build_with_clips(&fragment, &clips);
    assert!(
      list.is_empty(),
      "hidden fragments should not emit display items"
    );
  }

  #[test]
  fn test_deeply_nested() {
    let text = create_text_fragment(5.0, 5.0, 20.0, 10.0, "X");
    let level3 = FragmentNode::new_block(Rect::from_xywh(5.0, 5.0, 30.0, 20.0), vec![text]);
    let level2 = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 50.0, 40.0), vec![level3]);
    let level1 = FragmentNode::new_block(Rect::from_xywh(20.0, 20.0, 70.0, 60.0), vec![level2]);

    let builder = DisplayListBuilder::new();
    let list = builder.build(&level1);

    assert_eq!(list.len(), 1);
    if let DisplayItem::Text(text) = &list.items()[0] {
      assert_eq!(text.origin.x, 40.0);
    }
  }

  #[test]
  fn test_complex_tree() {
    let text1 = create_text_fragment(0.0, 0.0, 100.0, 20.0, "Line1");
    let text2 = create_text_fragment(0.0, 20.0, 100.0, 20.0, "Line2");
    let image = create_image_fragment(
            0.0,
            40.0,
            50.0,
            50.0,
            "data:image/svg+xml,%3Csvg%20xmlns=%22http://www.w3.org/2000/svg%22%20width=%221%22%20height=%221%22%3E%3C/svg%3E",
        );

    let inner = FragmentNode::new_block(
      Rect::from_xywh(10.0, 10.0, 120.0, 100.0),
      vec![text1, text2, image],
    );
    let outer = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 200.0, 200.0), vec![inner]);

    let builder = DisplayListBuilder::with_image_cache(ImageCache::new());
    let list = builder.build(&outer);

    assert_eq!(list.len(), 3);

    let text_count = list
      .items()
      .iter()
      .filter(|i| matches!(i, DisplayItem::Text(_)))
      .count();
    let image_count = list
      .items()
      .iter()
      .filter(|i| matches!(i, DisplayItem::Image(_)))
      .count();

    assert_eq!(text_count, 2);
    assert_eq!(image_count, 1);
  }

  #[test]
  fn test_image_decoding_uses_cache() {
    // 1x1 red inline SVG
    let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" width="1" height="1"><rect width="1" height="1" fill="red"/></svg>"#;
    let fragment = create_image_fragment(0.0, 0.0, 10.0, 10.0, svg);
    let builder = DisplayListBuilder::with_image_cache(ImageCache::new());
    let list = builder.build(&fragment);

    assert_eq!(list.len(), 1);
    let DisplayItem::Image(img) = &list.items()[0] else {
      panic!("Expected image item");
    };
    assert_eq!(img.image.width, 1);
    assert_eq!(img.image.height, 1);
    let pixels = img.image.pixels.as_ref();
    assert_eq!(pixels.len(), 4);
    assert_eq!(pixels, &[255, 0, 0, 255]);
  }

  #[test]
  fn embed_and_object_decode_images() {
    let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" width="2" height="2"><rect width="2" height="2" fill="blue"/></svg>"#;

    let embed_fragment = FragmentNode::new_replaced(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      ReplacedType::Embed {
        src: svg.to_string(),
      },
    );
    let embed_list = DisplayListBuilder::with_image_cache(ImageCache::new()).build(&embed_fragment);
    let DisplayItem::Image(embed_img) = &embed_list.items()[0] else {
      panic!("expected image item for embed");
    };
    assert_eq!(embed_img.image.width, 2);
    assert_eq!(embed_img.image.height, 2);

    let object_fragment = FragmentNode::new_replaced(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      ReplacedType::Object {
        data: svg.to_string(),
      },
    );
    let object_list =
      DisplayListBuilder::with_image_cache(ImageCache::new()).build(&object_fragment);
    let DisplayItem::Image(object_img) = &object_list.items()[0] else {
      panic!("expected image item for object");
    };
    assert_eq!(object_img.image.width, 2);
    assert_eq!(object_img.image.height, 2);
  }

  #[test]
  fn test_object_fit_contain_applied_in_display_list() {
    let mut style = ComputedStyle::default();
    style.display = Display::Inline;
    style.object_fit = crate::style::types::ObjectFit::Contain;
    style.object_position = crate::style::types::ObjectPosition {
      x: crate::style::types::PositionComponent::Keyword(
        crate::style::types::PositionKeyword::Center,
      ),
      y: crate::style::types::PositionComponent::Keyword(
        crate::style::types::PositionKeyword::Center,
      ),
    };

    let fragment = FragmentNode::new_with_style(
      Rect::from_xywh(0.0, 0.0, 200.0, 100.0),
      FragmentContent::Replaced {
        box_id: None,
        replaced_type: ReplacedType::Image {
          src: "data:image/svg+xml,%3Csvg%20xmlns=%22http://www.w3.org/2000/svg%22%20width=%221%22%20height=%221%22%3E%3C/svg%3E".to_string(),
          alt: None,
          sizes: None,
          srcset: Vec::new(),
          picture_sources: Vec::new(),
        },
      },
      vec![],
      Arc::new(style),
    );

    let builder = DisplayListBuilder::with_image_cache(ImageCache::new());
    let list = builder.build(&fragment);

    assert_eq!(list.len(), 1);
    let DisplayItem::Image(img) = &list.items()[0] else {
      panic!("Expected image item");
    };
    // Image is 1x1, box is 200x100, contain => scale to min(200,100) => 100x100, centered horizontally.
    assert!((img.dest_rect.width() - 100.0).abs() < 0.1);
    assert!((img.dest_rect.height() - 100.0).abs() < 0.1);
    assert!((img.dest_rect.x() - 50.0).abs() < 0.1);
    assert!((img.dest_rect.y() - 0.0).abs() < 0.1);
  }

  #[test]
  fn object_position_viewport_units_resolve_in_display_list() {
    let mut style = ComputedStyle::default();
    style.object_fit = crate::style::types::ObjectFit::None;
    // Position 10vw from the left of the box. With 200px viewport width, free space is 50px (100-50).
    style.object_position = crate::style::types::ObjectPosition {
      x: crate::style::types::PositionComponent::Length(crate::style::values::Length::new(
        10.0,
        crate::style::values::LengthUnit::Vw,
      )),
      y: crate::style::types::PositionComponent::Keyword(
        crate::style::types::PositionKeyword::Start,
      ),
    };

    let fragment = FragmentNode::new_with_style(
      Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
      FragmentContent::Replaced {
        box_id: None,
        replaced_type: ReplacedType::Image {
          src: "data:image/svg+xml,%3Csvg%20xmlns=%22http://www.w3.org/2000/svg%22%20width=%221%22%20height=%221%22%3E%3C/svg%3E".to_string(),
          alt: None,
          sizes: None,
          srcset: Vec::new(),
          picture_sources: Vec::new(),
        },
      },
      vec![],
      Arc::new(style),
    );

    let tree = FragmentTree::with_viewport(fragment, crate::geometry::Size::new(200.0, 200.0));
    let builder = DisplayListBuilder::with_image_cache(ImageCache::new());
    let list = builder.build_tree(&tree);

    assert_eq!(list.len(), 1);
    let DisplayItem::Image(img) = &list.items()[0] else {
      panic!("Expected image item");
    };
    // free_x = 100 - 1 = 99; but we align with 10vw (20px), so dest_rect.x should be ~20.
    assert!((img.dest_rect.x() - 20.0).abs() < 0.5);
  }

  #[test]
  fn image_rendering_pixelated_sets_nearest_filter_quality() {
    let mut style = ComputedStyle::default();
    style.image_rendering = ImageRendering::Pixelated;
    let fragment = FragmentNode::new_with_style(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      FragmentContent::Replaced {
        box_id: None,
        replaced_type: ReplacedType::Image {
          src: "data:image/svg+xml,%3Csvg%20xmlns=%22http://www.w3.org/2000/svg%22%20width=%221%22%20height=%221%22%3E%3C/svg%3E".to_string(),
          alt: None,
          sizes: None,
          srcset: Vec::new(),
          picture_sources: Vec::new(),
        },
      },
      vec![],
      Arc::new(style),
    );

    let list = DisplayListBuilder::new().build(&fragment);
    let DisplayItem::Image(img) = &list.items()[0] else {
      panic!("Expected image item");
    };
    assert_eq!(img.filter_quality, ImageFilterQuality::Nearest);
  }

  #[test]
  fn image_rendering_crisp_edges_sets_nearest_filter_quality() {
    let mut style = ComputedStyle::default();
    style.image_rendering = ImageRendering::CrispEdges;
    let fragment = FragmentNode::new_with_style(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      FragmentContent::Replaced {
        box_id: None,
        replaced_type: ReplacedType::Image {
          src: "data:image/svg+xml,%3Csvg%20xmlns=%22http://www.w3.org/2000/svg%22%20width=%221%22%20height=%221%22%3E%3C/svg%3E".to_string(),
          alt: None,
          sizes: None,
          srcset: Vec::new(),
          picture_sources: Vec::new(),
        },
      },
      vec![],
      Arc::new(style),
    );

    let list = DisplayListBuilder::new().build(&fragment);
    let DisplayItem::Image(img) = &list.items()[0] else {
      panic!("Expected image item");
    };
    assert_eq!(img.filter_quality, ImageFilterQuality::Nearest);
  }

  #[test]
  fn background_image_rendering_crisp_edges_sets_nearest_filter_quality() {
    let url = data_url_for_color([255, 0, 0, 255]);

    let mut style = ComputedStyle::default();
    style.image_rendering = ImageRendering::CrispEdges;
    style.background_color = Rgba::TRANSPARENT;
    style.background_layers = vec![BackgroundLayer {
      image: Some(BackgroundImage::Url(url)),
      repeat: BackgroundRepeat::no_repeat(),
      ..BackgroundLayer::default()
    }];

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );

    let list = DisplayListBuilder::new().build(&fragment);
    let image = list
      .items()
      .iter()
      .find_map(|item| match item {
        DisplayItem::Image(img) => Some(img),
        _ => None,
      })
      .expect("background image should emit an image item");

    assert_eq!(image.filter_quality, ImageFilterQuality::Nearest);
  }

  #[test]
  fn background_image_rendering_pixelated_sets_nearest_filter_quality() {
    let url = data_url_for_color([255, 0, 0, 255]);

    let mut style = ComputedStyle::default();
    style.image_rendering = ImageRendering::Pixelated;
    style.background_color = Rgba::TRANSPARENT;
    style.background_layers = vec![BackgroundLayer {
      image: Some(BackgroundImage::Url(url)),
      repeat: BackgroundRepeat::no_repeat(),
      ..BackgroundLayer::default()
    }];

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );

    let list = DisplayListBuilder::new().build(&fragment);
    let image = list
      .items()
      .iter()
      .find_map(|item| match item {
        DisplayItem::Image(img) => Some(img),
        _ => None,
      })
      .expect("background image should emit an image item");

    assert_eq!(image.filter_quality, ImageFilterQuality::Nearest);
  }

  #[test]
  fn filters_resolve_font_relative_lengths_in_display_list() {
    let mut style = ComputedStyle::default();
    style.font_size = 20.0;
    style.filter = vec![crate::style::types::FilterFunction::Blur(Length::new(
      1.0,
      LengthUnit::Ex,
    ))];

    let mut resolver = SvgFilterResolver::new(None, Vec::new(), None);
    let filters = DisplayListBuilder::resolve_filters(
      &style.filter,
      &style,
      Some((200.0, 100.0)),
      &FontContext::new(),
      &mut resolver,
    );

    match filters.first() {
      Some(ResolvedFilter::Blur(radius)) => assert!(
        (radius - 10.0).abs() < 2.0,
        "expected ex to resolve near half the font size (got {radius})"
      ),
      other => panic!("expected blur filter, got {:?}", other),
    }
  }

  #[test]
  fn plus_lighter_mix_blend_mode_converts() {
    assert!(matches!(
      DisplayListBuilder::convert_blend_mode(MixBlendMode::PlusLighter),
      BlendMode::PlusLighter
    ));
  }

  #[test]
  fn unit_interval_filters_clamp_in_display_list() {
    let mut style = ComputedStyle::default();
    style.filter = vec![
      crate::style::types::FilterFunction::Grayscale(2.0),
      crate::style::types::FilterFunction::Sepia(1.5),
      crate::style::types::FilterFunction::Invert(1.3),
      crate::style::types::FilterFunction::Opacity(1.8),
    ];

    let mut resolver = SvgFilterResolver::new(None, Vec::new(), None);
    let filters = DisplayListBuilder::resolve_filters(
      &style.filter,
      &style,
      Some((200.0, 100.0)),
      &FontContext::new(),
      &mut resolver,
    );
    assert_eq!(filters.len(), 4);
    assert!(filters.iter().all(|f| match f {
      ResolvedFilter::Grayscale(v) | ResolvedFilter::Sepia(v) | ResolvedFilter::Invert(v) =>
        (*v - 1.0).abs() < 0.001,
      ResolvedFilter::Opacity(v) => (*v - 1.0).abs() < 0.001,
      _ => false,
    }));
  }

  #[test]
  fn multiplicative_filters_keep_values_in_display_list() {
    let mut style = ComputedStyle::default();
    style.filter = vec![
      crate::style::types::FilterFunction::Brightness(2.25),
      crate::style::types::FilterFunction::Contrast(1.5),
      crate::style::types::FilterFunction::Saturate(3.75),
    ];

    let mut resolver = SvgFilterResolver::new(None, Vec::new(), None);
    let filters = DisplayListBuilder::resolve_filters(
      &style.filter,
      &style,
      Some((200.0, 100.0)),
      &FontContext::new(),
      &mut resolver,
    );
    assert_eq!(filters.len(), 3);
    assert!(filters
      .iter()
      .any(|f| matches!(f, ResolvedFilter::Brightness(v) if (*v - 2.25).abs() < 0.001)));
    assert!(filters
      .iter()
      .any(|f| matches!(f, ResolvedFilter::Contrast(v) if (*v - 1.5).abs() < 0.001)));
    assert!(filters
      .iter()
      .any(|f| matches!(f, ResolvedFilter::Saturate(v) if (*v - 3.75).abs() < 0.001)));
  }

  #[test]
  fn alt_text_emitted_when_image_missing() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.font_size = 12.0;

    let fragment = FragmentNode::new_with_style(
      Rect::from_xywh(0.0, 0.0, 50.0, 20.0),
      FragmentContent::Replaced {
        box_id: None,
        replaced_type: ReplacedType::Image {
          src: String::new(),
          alt: Some("alt text".to_string()),
          sizes: None,
          srcset: Vec::new(),
          picture_sources: Vec::new(),
        },
      },
      vec![],
      Arc::new(style),
    );

    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);

    assert!(!list.is_empty());
    let DisplayItem::Text(text) = &list.items()[0] else {
      panic!("Expected text item for alt fallback");
    };
    assert!(text.advance_width > 0.0);
  }

  #[test]
  fn missing_image_without_alt_emits_placeholder() {
    let fragment = FragmentNode::new_replaced(
      Rect::from_xywh(0.0, 0.0, 40.0, 20.0),
      ReplacedType::Image {
        src: String::new(),
        alt: None,
        sizes: None,
        srcset: Vec::new(),
        picture_sources: Vec::new(),
      },
    );
    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);

    assert_eq!(list.len(), 2, "expected fill + stroke placeholder items");
    assert!(matches!(list.items()[0], DisplayItem::FillRect(_)));
    assert!(matches!(list.items()[1], DisplayItem::StrokeRect(_)));
  }

  #[test]
  fn non_image_replaced_uses_labeled_placeholder() {
    let fragment = FragmentNode::new_replaced(
      Rect::from_xywh(0.0, 0.0, 40.0, 20.0),
      ReplacedType::Video {
        src: String::new(),
        poster: None,
      },
    );
    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);

    assert_eq!(list.len(), 3, "fill, stroke, and label text expected");
    assert!(matches!(list.items()[0], DisplayItem::FillRect(_)));
    assert!(matches!(list.items()[1], DisplayItem::StrokeRect(_)));
    assert!(matches!(list.items()[2], DisplayItem::Text(_)));
  }

  #[test]
  fn audio_replaced_uses_labeled_placeholder() {
    let fragment = FragmentNode::new_replaced(
      Rect::from_xywh(0.0, 0.0, 30.0, 12.0),
      ReplacedType::Audio { src: String::new() },
    );
    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);

    assert_eq!(list.len(), 3, "audio placeholder should include label text");
    assert!(matches!(list.items()[0], DisplayItem::FillRect(_)));
    assert!(matches!(list.items()[1], DisplayItem::StrokeRect(_)));
    assert!(matches!(list.items()[2], DisplayItem::Text(_)));
  }

  #[test]
  fn video_poster_decodes_before_placeholder() {
    let poster = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"4\" height=\"2\"><rect width=\"4\" height=\"2\" fill=\"red\"/></svg>";
    let fragment = FragmentNode::new_replaced(
      Rect::from_xywh(0.0, 0.0, 40.0, 20.0),
      ReplacedType::Video {
        src: String::new(),
        poster: Some(poster.to_string()),
      },
    );
    let builder = DisplayListBuilder::with_image_cache(ImageCache::new());
    let list = builder.build(&fragment);

    assert_eq!(list.len(), 1, "poster image should render as content");
    assert!(matches!(list.items()[0], DisplayItem::Image(_)));
  }

  #[test]
  fn transform_box_uses_content_box_for_translate_percentages() {
    let mut style = ComputedStyle::default();
    style.transform_box = TransformBox::ContentBox;
    style.padding_left = Length::px(10.0);
    style.padding_right = Length::px(10.0);
    style.border_left_width = Length::px(5.0);
    style.border_right_width = Length::px(5.0);
    style.transform.push(Transform::Translate(
      Length::percent(50.0),
      Length::percent(0.0),
    ));

    let bounds = Rect::from_xywh(0.0, 0.0, 200.0, 100.0);
    let transform = DisplayListBuilder::build_transform(&style, bounds, None)
      .self_transform
      .expect("transform should build");
    let transform = transform.to_2d().expect("2d transform");

    assert!((transform.e - 85.0).abs() < 1e-3);
    assert!((transform.f).abs() < 1e-3);
  }

  #[test]
  fn transform_box_shifts_origin_to_content_box() {
    let mut style = ComputedStyle::default();
    style.transform_box = TransformBox::ContentBox;
    style.padding_left = Length::px(10.0);
    style.border_left_width = Length::px(5.0);
    style.transform_origin = crate::style::types::TransformOrigin {
      x: Length::percent(0.0),
      y: Length::percent(0.0),
    };
    style.transform.push(Transform::Scale(2.0, 1.0));

    let bounds = Rect::from_xywh(0.0, 0.0, 200.0, 100.0);
    let transform = DisplayListBuilder::build_transform(&style, bounds, None)
      .self_transform
      .expect("transform should build");
    let transform = transform.to_2d().expect("2d transform");

    assert!((transform.e + 15.0).abs() < 1e-3);
    assert!((transform.f).abs() < 1e-3);
  }

  #[test]
  fn motion_path_combines_with_existing_transforms() {
    let mut style = ComputedStyle::default();
    style.transform_origin = crate::style::types::TransformOrigin {
      x: Length::percent(0.0),
      y: Length::percent(0.0),
    };
    style.transform.push(Transform::Scale(2.0, 1.0));
    style.offset_path = OffsetPath::Path(vec![
      MotionPathCommand::MoveTo(MotionPosition {
        x: Length::px(0.0),
        y: Length::px(0.0),
      }),
      MotionPathCommand::LineTo(MotionPosition {
        x: Length::px(100.0),
        y: Length::px(0.0),
      }),
    ]);
    style.offset_anchor = OffsetAnchor::Position {
      x: Length::px(0.0),
      y: Length::px(0.0),
    };
    style.offset_distance = Length::percent(100.0);

    let bounds = Rect::from_xywh(0.0, 0.0, 20.0, 20.0);
    let transform = DisplayListBuilder::build_transform(&style, bounds, None)
      .self_transform
      .expect("transform should build");
    let transform = transform.to_2d().expect("2d transform");

    // Motion path translation composes before the transform list, so scaling affects translation.
    assert!((transform.e - 200.0).abs() < 1e-3);
    assert!((transform.a - 2.0).abs() < 1e-3);
  }

  #[test]
  fn motion_path_composes_with_parent_transform() {
    let mut style = ComputedStyle::default();
    style.transform_origin = crate::style::types::TransformOrigin {
      x: Length::percent(0.0),
      y: Length::percent(0.0),
    };
    style.transform.push(Transform::Scale(2.0, 1.0));
    style.offset_path = OffsetPath::Path(vec![
      MotionPathCommand::MoveTo(MotionPosition {
        x: Length::px(0.0),
        y: Length::px(0.0),
      }),
      MotionPathCommand::LineTo(MotionPosition {
        x: Length::px(100.0),
        y: Length::px(0.0),
      }),
    ]);
    style.offset_anchor = OffsetAnchor::Position {
      x: Length::px(0.0),
      y: Length::px(0.0),
    };
    style.offset_distance = Length::percent(50.0);

    let bounds = Rect::from_xywh(0.0, 0.0, 20.0, 20.0);
    let child = DisplayListBuilder::build_transform(&style, bounds, None)
      .self_transform
      .expect("transform");
    let parent = Transform3D::translate(25.0, 5.0, 0.0);
    let combined = parent.multiply(&child).to_2d().expect("2d transform");

    assert!((combined.e - 125.0).abs() < 1e-3);
    assert!((combined.f - 5.0).abs() < 1e-3);
    assert!((combined.a - 2.0).abs() < 1e-3);
  }

  #[test]
  fn transform_translate_resolves_calc_components() {
    let mut style = ComputedStyle::default();
    style.font_size = 10.0;
    style.root_font_size = 12.0;
    let calc = CalcLength::single(LengthUnit::Percent, 50.0)
      .add_scaled(&CalcLength::single(LengthUnit::Em, 2.0), 1.0)
      .expect("calc terms");
    style.transform.push(Transform::Translate(
      Length::calc(calc),
      Length::percent(0.0),
    ));

    let bounds = Rect::from_xywh(0.0, 0.0, 200.0, 100.0);
    let transform = DisplayListBuilder::build_transform(&style, bounds, None)
      .self_transform
      .expect("transform should build");
    let transform = transform.to_2d().expect("2d transform");

    // 50% of 200 = 100; 2em at 10px = 20 -> total 120.
    assert!((transform.e - 120.0).abs() < 1e-3);
    assert!((transform.f).abs() < 1e-3);
  }

  #[test]
  fn matrix3d_transform_preserves_values() {
    let mut style = ComputedStyle::default();
    let values = [
      1.0, 0.0, 0.0, 0.0, // column 1
      0.0, 1.0, 0.0, 0.0, // column 2
      0.0, 0.0, 1.0, 0.0, // column 3
      5.0, 6.0, 0.0, 1.0, // translation
    ];
    style.transform.push(Transform::Matrix3d(values));

    let bounds = Rect::from_xywh(0.0, 0.0, 50.0, 50.0);
    let transform = DisplayListBuilder::build_transform(&style, bounds, None)
      .self_transform
      .expect("matrix3d should build");

    assert_eq!(transform.m[12], 5.0);
    assert_eq!(transform.m[13], 6.0);
    assert_eq!(transform.m[15], 1.0);
    let affine = transform
      .to_2d()
      .expect("matrix3d translation stays affine");
    assert!((affine.e - 5.0).abs() < 1e-6);
    assert!((affine.f - 6.0).abs() < 1e-6);
  }

  #[test]
  fn perspective_property_builds_transform() {
    let mut style = ComputedStyle::default();
    style.perspective = Some(Length::px(500.0));

    let bounds = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
    let transforms = DisplayListBuilder::build_transform(&style, bounds, None);

    assert!(
      transforms.self_transform.is_none(),
      "perspective property should not affect self"
    );
    let perspective = transforms
      .child_perspective
      .expect("perspective builds child transform");
    assert!(
      perspective.to_2d().is_none(),
      "perspective should keep 3d components"
    );
    assert!((perspective.m[11] + 1.0 / 500.0).abs() < 1e-6);
  }

  #[test]
  fn background_position_calc_vw_requires_viewport() {
    let pos = BackgroundPosition::Position {
      x: crate::style::types::BackgroundPositionComponent {
        alignment: 0.0,
        offset: Length::calc(CalcLength::single(LengthUnit::Vw, 10.0)),
      },
      y: crate::style::types::BackgroundPositionComponent {
        alignment: 0.0,
        offset: Length::percent(0.0),
      },
    };

    // With a 200px viewport, 10vw = 20px offset along the x-axis.
    let (x, y) = DisplayListBuilder::resolve_background_offset(
      pos,
      100.0,
      100.0,
      0.0,
      0.0,
      16.0,
      16.0,
      Some((200.0, 100.0)),
    );
    assert!((x - 20.0).abs() < 0.01);
    assert!((y - 0.0).abs() < 0.01);

    // Without a viewport, viewport-relative calc stays unresolved and falls back to zero.
    let (x, y) =
      DisplayListBuilder::resolve_background_offset(pos, 100.0, 100.0, 0.0, 0.0, 16.0, 16.0, None);
    assert!((x - 0.0).abs() < 0.01);
    assert!((y - 0.0).abs() < 0.01);
  }

  #[test]
  fn backface_hidden_skips_display_items() {
    let mut style = ComputedStyle::default();
    style.backface_visibility = BackfaceVisibility::Hidden;
    style.transform.push(Transform::RotateY(180.0));
    style.background_color = Rgba::BLACK;

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      vec![],
      Arc::new(style),
    );

    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);

    assert!(
      list.is_empty(),
      "backface hidden element should not emit display items"
    );
  }

  #[test]
  fn backface_hidden_with_perspective_still_culls() {
    let mut style = ComputedStyle::default();
    style.backface_visibility = BackfaceVisibility::Hidden;
    style.perspective = Some(Length::px(400.0));
    style.transform.push(Transform::RotateX(190.0));
    style.background_color = Rgba::BLACK;

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      vec![],
      Arc::new(style),
    );

    let builder = DisplayListBuilder::new();
    let list = builder.build(&fragment);

    assert!(
      list.is_empty(),
      "backface should be culled with perspective"
    );
  }
}
