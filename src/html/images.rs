//! Responsive image selection (`srcset`/`sizes`/`picture`).
//!
//! Implements the HTML "select an image source" algorithm for `<img>` and
//! `<picture>` elements. Selection is centralized here so layout, paint, and
//! diagnostics agree on which resource was chosen.

use crate::geometry::Size;
use crate::image_loader::resolve_against_base;
use crate::style::media::MediaContext;
use crate::tree::box_tree::PictureSource;
use crate::tree::box_tree::ReplacedType;
use crate::tree::box_tree::SizesList;
use crate::tree::box_tree::SrcsetCandidate;
use crate::tree::box_tree::SrcsetDescriptor;
use std::collections::HashSet;
use url::Url;

/// Selection inputs describing the rendering environment.
#[derive(Clone, Copy, Debug)]
pub struct ImageSelectionContext<'a> {
  /// Device pixel ratio used for DPR-dependent selection.
  pub device_pixel_ratio: f32,
  /// Slot width in CSS px, when known (used as a hint for width descriptors).
  pub slot_width: Option<f32>,
  /// Viewport size in CSS px, used for evaluating `sizes` and viewport units.
  pub viewport: Option<Size>,
  /// Active media context for evaluating media conditions in `<source media>` and `sizes`.
  pub media_context: Option<&'a MediaContext>,
  /// Font size used to resolve `em`/`rem` units in `sizes` when provided.
  pub font_size: Option<f32>,
  /// Document base URL for resolving relative references when deduplicating fallbacks.
  pub base_url: Option<&'a str>,
}

/// Result of image source selection.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SelectedImageSource<'a> {
  pub url: &'a str,
  /// Declared descriptor on the chosen candidate, when any.
  pub descriptor: Option<SrcsetDescriptor>,
  /// Effective resource pixel density (image pixels per CSS px).
  pub density: Option<f32>,
  /// True when chosen from a `<picture>` `<source>`.
  pub from_picture: bool,
}

impl<'a> SelectedImageSource<'a> {
  fn empty() -> Self {
    Self {
      url: "",
      descriptor: None,
      density: None,
      from_picture: false,
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum DescriptorKind {
  Density,
  Width,
}

fn normalized_image_mime(mime: &str) -> String {
  mime
    .split(';')
    .next()
    .unwrap_or("")
    .trim()
    .to_ascii_lowercase()
}

fn is_supported_image_mime(mime: &str) -> bool {
  let normalized = normalized_image_mime(mime);
  if normalized == "image/svg+xml" {
    return true;
  }

  image::ImageFormat::from_mime_type(&normalized).is_some()
}

fn picture_source_matches(source: &PictureSource, ctx: ImageSelectionContext<'_>) -> bool {
  if source.srcset.is_empty() {
    return false;
  }

  if let Some(mime) = &source.mime_type {
    if !is_supported_image_mime(mime) {
      return false;
    }
  }

  match &source.media {
    Some(queries) => ctx
      .media_context
      .map(|m| m.evaluate_list(queries))
      .unwrap_or(true),
    None => true,
  }
}

fn select_picture_source<'a>(
  sources: &'a [PictureSource],
  ctx: ImageSelectionContext<'_>,
) -> Option<&'a PictureSource> {
  sources
    .iter()
    .find(|source| picture_source_matches(source, ctx))
}

fn descriptor_kind(candidates: &[SrcsetCandidate]) -> DescriptorKind {
  if candidates
    .iter()
    .any(|c| matches!(c.descriptor, SrcsetDescriptor::Width(_)))
  {
    DescriptorKind::Width
  } else {
    DescriptorKind::Density
  }
}

fn sanitized_target_density(target: f32) -> f32 {
  if target.is_finite() && target > 0.0 {
    target
  } else {
    1.0
  }
}

fn resolve_source_size(sizes: Option<&SizesList>, ctx: ImageSelectionContext<'_>) -> Option<f32> {
  if let Some(slot) = ctx.slot_width {
    if slot.is_finite() && slot > 0.0 {
      return Some(slot);
    }
  }

  let viewport = ctx.viewport?;
  let font_size = ctx.font_size.unwrap_or(16.0);
  let media_ctx = ctx.media_context.cloned().unwrap_or_else(|| {
    MediaContext::screen(viewport.width, viewport.height)
      .with_device_pixel_ratio(ctx.device_pixel_ratio)
      .with_env_overrides()
  });

  if let Some(list) = sizes {
    Some(list.evaluate(&media_ctx, viewport, font_size))
  } else {
    Some(viewport.width)
  }
  .filter(|v| v.is_finite() && *v > 0.0)
}

fn pick_best_density<'a, I>(candidates: I, target: f32) -> Option<(&'a SrcsetCandidate, f32)>
where
  I: Iterator<Item = (&'a SrcsetCandidate, f32)>,
{
  let mut best_ge: Option<(&SrcsetCandidate, f32)> = None;
  let mut best_lt: Option<(&SrcsetCandidate, f32)> = None;

  for (candidate, density) in candidates {
    if density <= 0.0 || !density.is_finite() {
      continue;
    }
    if density >= target {
      let replace = best_ge.as_ref().map(|(_, d)| density < *d).unwrap_or(true);
      if replace {
        best_ge = Some((candidate, density));
      }
    } else {
      let replace = best_lt.as_ref().map(|(_, d)| density > *d).unwrap_or(true);
      if replace {
        best_lt = Some((candidate, density));
      }
    }
  }

  best_ge.or(best_lt)
}

fn select_from_srcset<'a>(
  srcset: &'a [SrcsetCandidate],
  sizes: Option<&'a SizesList>,
  ctx: ImageSelectionContext<'_>,
  from_picture: bool,
) -> Option<SelectedImageSource<'a>> {
  if srcset.is_empty() {
    return None;
  }

  let kind = descriptor_kind(srcset);
  let source_size = if kind == DescriptorKind::Width {
    resolve_source_size(sizes, ctx)
  } else {
    None
  };

  if kind == DescriptorKind::Width && source_size.is_none() {
    return None;
  }

  let target = sanitized_target_density(ctx.device_pixel_ratio);

  let best = pick_best_density(
    srcset.iter().filter_map(|candidate| {
      let density = match (kind, candidate.descriptor) {
        (DescriptorKind::Width, SrcsetDescriptor::Width(w)) => {
          let slot = source_size?;
          Some(w as f32 / slot)
        }
        (DescriptorKind::Density, SrcsetDescriptor::Density(d)) => Some(d),
        // Ignore density descriptors when width descriptors are present.
        _ => None,
      }?;
      Some((candidate, density))
    }),
    target,
  )?;

  Some(SelectedImageSource {
    url: best.0.url.as_str(),
    descriptor: Some(best.0.descriptor),
    density: Some(best.1),
    from_picture,
  })
}

fn select_img_source<'a>(
  src: &'a str,
  srcset: &'a [SrcsetCandidate],
  sizes: Option<&'a SizesList>,
  ctx: ImageSelectionContext<'_>,
  from_picture: bool,
) -> SelectedImageSource<'a> {
  if let Some(selected) = select_from_srcset(srcset, sizes, ctx, from_picture) {
    return selected;
  }

  SelectedImageSource {
    url: src,
    descriptor: None,
    density: None,
    from_picture,
  }
}

/// Select the best candidate for an `<img>` element with optional `<picture>` art-direction.
pub fn select_image_source<'a>(
  img_src: &'a str,
  img_srcset: &'a [SrcsetCandidate],
  img_sizes: Option<&'a SizesList>,
  picture_sources: &'a [PictureSource],
  ctx: ImageSelectionContext<'_>,
) -> SelectedImageSource<'a> {
  if let Some(source) = select_picture_source(picture_sources, ctx) {
    if let Some(selected) = select_from_srcset(
      &source.srcset,
      source.sizes.as_ref().or(img_sizes),
      ctx,
      true,
    ) {
      return selected;
    }
  }

  select_img_source(img_src, img_srcset, img_sizes, ctx, false)
}

fn dedup_key(url: &str, base_url: Option<&str>) -> String {
  let trimmed = url.trim();
  if trimmed.is_empty() {
    return String::new();
  }

  if let Ok(parsed) = Url::parse(trimmed) {
    return parsed.to_string();
  }

  if let Some(base) = base_url {
    if let Some(resolved) = resolve_against_base(base, trimmed) {
      return resolved;
    }
  }

  trimmed.to_string()
}

fn push_unique<'a>(
  out: &mut Vec<SelectedImageSource<'a>>,
  seen: &mut HashSet<String>,
  candidate: SelectedImageSource<'a>,
  base_url: Option<&str>,
) {
  let trimmed = candidate.url.trim();
  if trimmed.is_empty()
    || trimmed.starts_with('#')
    || trimmed.to_ascii_lowercase().starts_with("javascript:")
    || trimmed.to_ascii_lowercase().starts_with("vbscript:")
    || trimmed.to_ascii_lowercase().starts_with("mailto:")
  {
    return;
  }

  let key = dedup_key(trimmed, base_url);
  if key.is_empty() {
    return;
  }

  if seen.insert(key) {
    out.push(candidate);
  }
}

/// Returns a priority-ordered list of candidate image sources for painting.
///
/// The first entry is the best-fit srcset candidate (or the base src when no
/// srcset applies). The second entry is the `<img>` fallback when a `<picture>`
/// source was chosen. Additional entries include the base `src` when distinct.
pub fn image_sources_with_fallback<'a>(
  img_src: &'a str,
  img_srcset: &'a [SrcsetCandidate],
  img_sizes: Option<&'a SizesList>,
  picture_sources: &'a [PictureSource],
  ctx: ImageSelectionContext<'_>,
) -> Vec<SelectedImageSource<'a>> {
  let mut ordered = Vec::new();
  let mut seen: HashSet<String> = HashSet::new();

  let primary = select_image_source(img_src, img_srcset, img_sizes, picture_sources, ctx);
  push_unique(&mut ordered, &mut seen, primary, ctx.base_url);

  if primary.from_picture {
    let fallback = select_image_source(img_src, img_srcset, img_sizes, &[], ctx);
    push_unique(&mut ordered, &mut seen, fallback, ctx.base_url);
  }

  if !img_src.is_empty() {
    push_unique(
      &mut ordered,
      &mut seen,
      SelectedImageSource {
        url: img_src,
        descriptor: None,
        density: None,
        from_picture: false,
      },
      ctx.base_url,
    );
  }

  ordered
}

impl ReplacedType {
  /// Selects the best image source along with its declared resolution (when known).
  pub fn selected_image_source_for_context<'a>(
    &'a self,
    ctx: ImageSelectionContext<'_>,
  ) -> SelectedImageSource<'a> {
    match self {
      ReplacedType::Image {
        src,
        srcset,
        sizes,
        picture_sources,
        ..
      } => select_image_source(src, srcset, sizes.as_ref(), picture_sources, ctx),
      _ => SelectedImageSource::empty(),
    }
  }

  /// Selects the best image source for the given device scale and slot width.
  ///
  /// Returns the authored `src` when no better candidate exists.
  pub fn image_source_for_context<'a>(&'a self, ctx: ImageSelectionContext<'_>) -> &'a str {
    self.selected_image_source_for_context(ctx).url
  }

  /// Returns a priority-ordered list of candidate image sources for painting.
  ///
  /// The first entry is the best-fit srcset candidate (or the base src when
  /// no srcset applies), followed by the base `src` (if different) and any
  /// fallback selections when a `<picture>` source was chosen.
  pub fn image_sources_with_fallback<'a>(
    &'a self,
    ctx: ImageSelectionContext<'_>,
  ) -> Vec<SelectedImageSource<'a>> {
    match self {
      ReplacedType::Image {
        src,
        srcset,
        sizes,
        picture_sources,
        ..
      } => image_sources_with_fallback(src, srcset, sizes.as_ref(), picture_sources, ctx),
      ReplacedType::Video { src: _, poster } => poster
        .iter()
        .map(|p| SelectedImageSource {
          url: p.as_str(),
          descriptor: None,
          density: None,
          from_picture: false,
        })
        .collect(),
      ReplacedType::Svg { content } => vec![SelectedImageSource {
        url: content.svg.as_str(),
        descriptor: None,
        density: None,
        from_picture: false,
      }],
      ReplacedType::Embed { src: content }
      | ReplacedType::Object { data: content }
      | ReplacedType::Iframe { src: content, .. } => vec![SelectedImageSource {
        url: content.as_str(),
        descriptor: None,
        density: None,
        from_picture: false,
      }],
      _ => Vec::new(),
    }
  }
}
