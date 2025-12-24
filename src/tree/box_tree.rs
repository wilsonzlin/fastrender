//! Box Tree - Represents CSS boxes before layout
//!
//! The box tree is generated from the styled DOM tree and represents
//! the CSS box model. It's independent of layout - it only represents
//! what boxes exist and their styling, not where they're positioned.
//!
//! # Separation of Concerns
//!
//! **Box Tree (this module)**:
//! - Immutable
//! - No positions or final sizes
//! - Represents "what to layout"
//! - Generated once from DOM
//!
//! **Fragment Tree**:
//! - Result of layout
//! - Has positions and sizes
//! - Represents "what was laid out"
//! - Generated per layout pass
//!
//! Reference: CSS Display Module Level 3
//! <https://www.w3.org/TR/css-display-3/>

use crate::geometry::Size;
use crate::math::MathLayout;
use crate::style::color::Rgba;
use crate::style::display::FormattingContextType;
use crate::style::media::MediaQuery;
use crate::style::types::Appearance;
use crate::style::types::Overflow;
use crate::style::ComputedStyle;
use crate::tree::debug::DebugInfo;
use std::fmt;
use std::sync::Arc;

/// A block-level box
///
/// Block boxes stack vertically and establish block formatting contexts.
/// Examples: div, p, h1, section
///
/// Reference: CSS 2.1 Section 9.2.1
#[derive(Debug, Clone)]
pub struct BlockBox {
  /// What formatting context does this establish?
  pub formatting_context: FormattingContextType,
}

/// An inline-level box
///
/// Inline boxes flow horizontally within lines.
/// Examples: span, a, em, strong
///
/// Reference: CSS 2.1 Section 9.2.2
#[derive(Debug, Clone)]
pub struct InlineBox {
  /// For inline-block, this establishes a formatting context
  /// For regular inline, this is None
  pub formatting_context: Option<FormattingContextType>,
}

/// A text box containing actual text content
///
/// Text boxes are always inline-level and contain strings to be shaped.
///
/// # Note
///
/// The text stored here is the raw text. It will be shaped (with font,
/// bidi, script analysis) during inline layout.
#[derive(Debug, Clone)]
pub struct TextBox {
  /// The text content
  ///
  /// This is UTF-8 text that may contain multiple scripts, emojis, etc.
  pub text: String,
}

#[derive(Debug, Clone)]
pub enum MarkerContent {
  Text(String),
  Image(ReplacedBox),
}

/// A list marker box
///
/// Generated for list items. Carries marker text but participates as its own
/// box type so inline/layout can treat markers specially (e.g., position
/// outside the principal block).
#[derive(Debug, Clone)]
pub struct MarkerBox {
  /// Marker payload (text or image)
  pub content: MarkerContent,
}

/// A form control description used by the painter when rendering native controls.
#[derive(Debug, Clone, PartialEq)]
pub struct FormControl {
  /// Specific control type and metadata
  pub control: FormControlKind,
  /// Resolved appearance value (Auto/None/Keyword)
  pub appearance: Appearance,
  /// Whether the control is disabled
  pub disabled: bool,
}

/// Specific form control kinds
#[derive(Debug, Clone, PartialEq)]
pub enum FormControlKind {
  /// Text-like control (<input type=text>, search, etc.)
  Text {
    /// Current value attribute
    value: String,
    /// Placeholder text to render when value is empty
    placeholder: Option<String>,
    /// Optional size attribute hint for intrinsic width
    size_attr: Option<u32>,
  },
  /// Multiline control (<textarea>)
  TextArea {
    /// Raw text content
    value: String,
    /// Optional rows hint (default 2)
    rows: Option<u32>,
    /// Optional cols hint (default 20)
    cols: Option<u32>,
  },
  /// Button control (<button> and <input type=button|submit|reset>)
  Button { label: String },
  /// Selection control (<select>)
  Select {
    /// User-visible label for the current selection
    label: String,
    /// Whether multiple selections are allowed
    multiple: bool,
  },
  /// Checkbox or radio input
  Checkbox {
    /// Whether this represents a radio input (circle) instead of checkbox (square)
    is_radio: bool,
    /// Current checked state
    checked: bool,
  },
  /// Range control (<input type=range>)
  Range {
    /// Current numeric value
    value: f32,
    /// Minimum value (default 0)
    min: Option<f32>,
    /// Maximum value (default 100)
    max: Option<f32>,
  },
  /// Fallback for unknown input types
  Unknown { label: Option<String> },
}

/// A replaced element box
///
/// Replaced elements have intrinsic dimensions provided by external content.
/// Examples: img, canvas, video, iframe
///
/// Reference: CSS 2.1 Section 10.3.2
#[derive(Debug, Clone, PartialEq)]
pub struct ReplacedBox {
  /// Type of replaced element
  pub replaced_type: ReplacedType,

  /// Intrinsic size (if known)
  ///
  /// Some replaced elements have intrinsic dimensions (images with width/height),
  /// others don't (iframes without size attributes).
  pub intrinsic_size: Option<Size>,

  /// Intrinsic aspect ratio (width / height)
  ///
  /// Used for sizing when only one dimension is specified.
  pub aspect_ratio: Option<f32>,
}

/// MathML replaced content with cached layout.
#[derive(Debug, Clone)]
pub struct MathReplaced {
  pub root: crate::math::MathNode,
  pub layout: Option<Arc<MathLayout>>,
}

impl PartialEq for MathReplaced {
  fn eq(&self, other: &Self) -> bool {
    self.root == other.root
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SrcsetDescriptor {
  Density(f32),
  Width(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SrcsetCandidate {
  pub url: String,
  pub descriptor: SrcsetDescriptor,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SizesEntry {
  pub media: Option<Vec<crate::style::media::MediaQuery>>,
  pub length: crate::style::values::Length,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SizesList {
  pub entries: Vec<SizesEntry>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PictureSource {
  pub srcset: Vec<SrcsetCandidate>,
  pub sizes: Option<SizesList>,
  pub media: Option<Vec<MediaQuery>>,
  pub mime_type: Option<String>,
}

/// Serialized SVG content plus any inlined foreignObject metadata.
#[derive(Debug, Clone, PartialEq)]
pub struct SvgContent {
  /// Serialized SVG markup (may include foreignObject placeholders).
  pub svg: String,
  /// Placeholder rendering when nested HTML rendering fails.
  pub fallback_svg: String,
  /// Serialized `<foreignObject>` subtrees to be rendered separately.
  pub foreign_objects: Vec<ForeignObjectInfo>,
  /// Document-level CSS collected while serializing the SVG subtree.
  pub shared_css: String,
}

impl SvgContent {
  /// Creates SVG content without any foreignObject handling.
  pub fn raw(svg: impl Into<String>) -> Self {
    let svg = svg.into();
    Self {
      svg: svg.clone(),
      fallback_svg: svg,
      foreign_objects: Vec::new(),
      shared_css: String::new(),
    }
  }
}

/// Captured details for a `<foreignObject>` subtree that should be rendered via the HTML
/// pipeline and injected back into the SVG during painting.
#[derive(Debug, Clone, PartialEq)]
pub struct ForeignObjectInfo {
  /// Placeholder token emitted into the serialized SVG.
  pub placeholder: String,
  /// Original attributes (x/y/width/height/etc.).
  pub attributes: Vec<(String, String)>,
  pub x: f32,
  pub y: f32,
  pub width: f32,
  pub height: f32,
  pub opacity: f32,
  pub background: Option<Rgba>,
  pub html: String,
  pub style: Arc<ComputedStyle>,
  pub overflow_x: Overflow,
  pub overflow_y: Overflow,
}

/// Types of replaced elements
#[derive(Debug, Clone, PartialEq)]
pub enum ReplacedType {
  /// Image element
  Image {
    /// Source URL or data URI
    src: String,
    /// Alternative text for fallback rendering
    alt: Option<String>,
    /// Srcset candidates for density-aware selection
    srcset: Vec<SrcsetCandidate>,
    /// Sizes attribute values for width-descriptor selection
    sizes: Option<SizesList>,
    /// Ordered `<source>` elements from a parent `<picture>` (if any).
    picture_sources: Vec<PictureSource>,
  },

  /// Video element
  Video {
    /// Source URL
    src: String,
    /// Poster image URL or data URI
    poster: Option<String>,
  },
  /// Audio element
  Audio {
    /// Source URL
    src: String,
  },

  /// Canvas element
  Canvas,

  /// SVG embedded content
  Svg {
    /// SVG content (inline or reference)
    content: SvgContent,
  },

  /// Iframe (nested browsing context)
  Iframe {
    /// Source URL
    src: String,
    /// Inline HTML content overriding src
    srcdoc: Option<String>,
  },

  /// `<embed>` element
  Embed {
    /// Source URL
    src: String,
  },

  /// `<object>` element
  Object {
    /// Data URL
    data: String,
  },

  /// MathML content
  Math(MathReplaced),

  /// Native form controls (input/select/textarea/button)
  FormControl(FormControl),
}

#[derive(Debug, Clone, Copy)]
pub struct ImageSelectionContext<'a> {
  pub scale: f32,
  pub slot_width: Option<f32>,
  pub viewport: Option<crate::geometry::Size>,
  pub media_context: Option<&'a crate::style::media::MediaContext>,
  pub font_size: Option<f32>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SelectedImageSource<'a> {
  pub url: &'a str,
  /// Resolution in image pixels per CSS px (e.g. srcset density). None when unknown.
  pub resolution: Option<f32>,
}

impl PictureSource {
  fn matches_media(&self, ctx: ImageSelectionContext<'_>) -> bool {
    match &self.media {
      Some(queries) => ctx
        .media_context
        .map(|m| m.evaluate_list(queries))
        .unwrap_or(true),
      None => true,
    }
  }

  fn is_applicable(&self, ctx: ImageSelectionContext<'_>) -> bool {
    if self.srcset.is_empty() {
      return false;
    }

    if let Some(mime) = &self.mime_type {
      if !is_supported_image_mime(mime) {
        return false;
      }
    }

    self.matches_media(ctx)
  }
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
  match normalized_image_mime(mime).as_str() {
    "image/avif" | "image/webp" | "image/png" | "image/jpeg" | "image/jpg" | "image/gif"
    | "image/svg+xml" => true,
    _ => false,
  }
}

#[derive(Clone, Copy)]
struct ActiveImageSources<'a> {
  primary_src: &'a str,
  srcset: &'a [SrcsetCandidate],
  sizes: Option<&'a SizesList>,
  fallback_src: Option<&'a str>,
  fallback_srcset: Option<&'a [SrcsetCandidate]>,
}

impl<'a> ActiveImageSources<'a> {
  fn base_src(&self) -> &'a str {
    self
      .fallback_src
      .filter(|s| !s.is_empty())
      .unwrap_or(self.primary_src)
  }
}

impl ReplacedType {
  fn active_image_sources<'a>(&'a self, ctx: ImageSelectionContext<'_>) -> ActiveImageSources<'a> {
    match self {
      ReplacedType::Image {
        src, srcset, sizes, ..
      } => {
        if let Some(source) = self.select_picture_source(ctx) {
          let primary_src = source
            .srcset
            .first()
            .map(|c| c.url.as_str())
            .unwrap_or(src.as_str());
          ActiveImageSources {
            primary_src,
            srcset: source.srcset.as_slice(),
            sizes: source.sizes.as_ref(),
            fallback_src: Some(src.as_str()),
            fallback_srcset: Some(srcset.as_slice()),
          }
        } else {
          ActiveImageSources {
            primary_src: src.as_str(),
            srcset: srcset.as_slice(),
            sizes: sizes.as_ref(),
            fallback_src: None,
            fallback_srcset: None,
          }
        }
      }
      _ => ActiveImageSources {
        primary_src: "",
        srcset: &[],
        sizes: None,
        fallback_src: None,
        fallback_srcset: None,
      },
    }
  }

  fn select_picture_source<'a>(
    &'a self,
    ctx: ImageSelectionContext<'_>,
  ) -> Option<&'a PictureSource> {
    match self {
      ReplacedType::Image {
        picture_sources, ..
      } => picture_sources
        .iter()
        .find(|source| source.is_applicable(ctx)),
      _ => None,
    }
  }

  /// Returns a priority-ordered list of candidate image sources for painting.
  ///
  /// The first entry is the best-fit srcset candidate (or the base src when
  /// no srcset applies), followed by the base `src` (if different) and any
  /// remaining srcset URLs. Duplicate URLs are removed while preserving order.
  pub fn image_sources_with_fallback(&self, ctx: ImageSelectionContext<'_>) -> Vec<String> {
    match self {
      ReplacedType::Image { .. } => {
        let active = self.active_image_sources(ctx);
        let primary = self.image_source_for_context(ctx);
        let mut seen = std::collections::HashSet::new();
        let mut ordered = Vec::new();
        let push_unique =
          |url: &str, out: &mut Vec<String>, seen: &mut std::collections::HashSet<String>| {
            if seen.insert(url.to_string()) {
              out.push(url.to_string());
            }
          };

        push_unique(primary, &mut ordered, &mut seen);
        push_unique(active.base_src(), &mut ordered, &mut seen);
        if let Some(fallback) = active.fallback_src {
          push_unique(fallback, &mut ordered, &mut seen);
        }
        for cand in active.srcset {
          let url = &cand.url;
          // Skip candidates whose descriptors depend on sizes when sizes cannot be evaluated.
          if matches!(cand.descriptor, SrcsetDescriptor::Width(_))
            && ctx.slot_width.is_none()
            && ctx.viewport.is_none()
          {
            continue;
          }
          push_unique(url, &mut ordered, &mut seen);
        }
        if let Some(fallback_srcset) = active.fallback_srcset {
          for cand in fallback_srcset {
            push_unique(&cand.url, &mut ordered, &mut seen);
          }
        }
        ordered
      }
      ReplacedType::Video { src: _, poster } => poster.iter().cloned().collect(),
      ReplacedType::Svg { content } => vec![content.svg.clone()],
      ReplacedType::Embed { src: content }
      | ReplacedType::Object { data: content }
      | ReplacedType::Iframe { src: content, .. } => vec![content.clone()],
      _ => Vec::new(),
    }
  }

  /// Returns a placeholder label for non-image replaced content.
  pub fn placeholder_label(&self) -> Option<&str> {
    match self {
      ReplacedType::Video { .. } => Some("video"),
      ReplacedType::Audio { .. } => Some("audio"),
      ReplacedType::Iframe { srcdoc, .. } => srcdoc.as_deref().or(Some("iframe")),
      ReplacedType::Canvas => Some("canvas"),
      ReplacedType::Embed { .. } => Some("embed"),
      ReplacedType::Object { .. } => Some("object"),
      ReplacedType::FormControl(_) => Some("control"),
      ReplacedType::Math(_) => Some("math"),
      _ => None,
    }
  }

  /// Selects the best image source for the given device scale and slot width.
  ///
  /// Returns the authored `src` when no better candidate exists.
  pub fn image_source_for_context<'a>(&'a self, ctx: ImageSelectionContext<'_>) -> &'a str {
    self.selected_image_source_for_context(ctx).url
  }

  /// Selects the best image source along with its declared resolution (when known).
  pub fn selected_image_source_for_context<'a>(
    &'a self,
    ctx: ImageSelectionContext<'_>,
  ) -> SelectedImageSource<'a> {
    match self {
      ReplacedType::Image { .. } => {
        let active = self.active_image_sources(ctx);
        if active.srcset.is_empty() || !ctx.scale.is_finite() || ctx.scale <= 0.0 {
          return SelectedImageSource {
            url: active.base_src(),
            resolution: None,
          };
        }

        let slot_width = ctx.slot_width.filter(|w| *w > 0.0 && w.is_finite());

        let has_width_descriptors = active
          .srcset
          .iter()
          .any(|c| matches!(c.descriptor, SrcsetDescriptor::Width(_)));

        let effective_slot_width = slot_width.or_else(|| {
          let viewport = ctx.viewport?;
          if let (Some(sizes_list), Some(media_ctx)) = (active.sizes, ctx.media_context) {
            Some(sizes_list.evaluate(media_ctx, viewport, ctx.font_size.unwrap_or(16.0)))
          } else if has_width_descriptors {
            Some(viewport.width)
          } else {
            Some(viewport.width)
          }
        });

        // Pick the smallest density >= scale; if none, the largest below.
        let mut best: Option<(&SrcsetCandidate, f32)> = None;
        for candidate in active.srcset {
          let density = candidate.density_for_slot(effective_slot_width);
          if let Some(density) = density {
            if density <= 0.0 || !density.is_finite() {
              continue;
            }
            match best {
              Some((_, current_density)) if current_density >= ctx.scale => {
                if density >= ctx.scale && density < current_density {
                  best = Some((candidate, density));
                }
              }
              Some((_, current_density)) => {
                if density >= ctx.scale {
                  best = Some((candidate, density));
                } else if current_density < ctx.scale && density > current_density {
                  best = Some((candidate, density));
                }
              }
              None => best = Some((candidate, density)),
            }
          }
        }

        if let Some((candidate, density)) = best {
          SelectedImageSource {
            url: candidate.url.as_str(),
            resolution: Some(density),
          }
        } else {
          SelectedImageSource {
            url: active.base_src(),
            resolution: None,
          }
        }
      }
      _ => SelectedImageSource {
        url: "",
        resolution: None,
      },
    }
  }
}

impl SrcsetCandidate {
  pub fn density_for_slot(&self, slot_width: Option<f32>) -> Option<f32> {
    match self.descriptor {
      SrcsetDescriptor::Density(d) => Some(d),
      SrcsetDescriptor::Width(w) => {
        let slot = slot_width?;
        if slot <= 0.0 || !slot.is_finite() {
          return None;
        }
        Some(w as f32 / slot)
      }
    }
  }
}

impl SizesList {
  pub fn evaluate(
    &self,
    media_ctx: &crate::style::media::MediaContext,
    viewport: crate::geometry::Size,
    font_size: f32,
  ) -> f32 {
    let mut last_entry: Option<crate::style::values::Length> = None;
    for entry in &self.entries {
      last_entry = Some(entry.length);
      let media_matches = entry
        .media
        .as_ref()
        .map(|q| media_ctx.evaluate_list(q))
        .unwrap_or(true);
      if media_matches {
        if let Some(resolved) = resolve_sizes_length(entry.length, viewport, font_size) {
          let clamped = resolved.max(0.0);
          if clamped.is_finite() {
            return clamped;
          }
        }
      }
    }

    if let Some(len) = last_entry {
      if let Some(resolved) = resolve_sizes_length(len, viewport, font_size) {
        let clamped = resolved.max(0.0);
        if clamped.is_finite() {
          return clamped;
        }
      }
    }

    // Spec fallback: 100vw when all entries are missing/invalid.
    resolve_sizes_length(
      crate::style::values::Length::new(100.0, crate::style::values::LengthUnit::Vw),
      viewport,
      font_size,
    )
    .unwrap_or(viewport.width)
  }
}

fn resolve_sizes_length(
  length: crate::style::values::Length,
  viewport: crate::geometry::Size,
  font_size: f32,
) -> Option<f32> {
  use crate::style::values::LengthUnit;
  match length.unit {
    LengthUnit::Percent => length.resolve_against(viewport.width),
    LengthUnit::Vw
    | LengthUnit::Vh
    | LengthUnit::Vmin
    | LengthUnit::Vmax
    | LengthUnit::Dvw
    | LengthUnit::Dvh
    | LengthUnit::Dvmin
    | LengthUnit::Dvmax => length.resolve_with_viewport(viewport.width, viewport.height),
    LengthUnit::Em | LengthUnit::Rem => Some(font_size * length.value),
    LengthUnit::Ex | LengthUnit::Ch => Some(font_size * length.value * 0.5),
    _ if length.unit.is_absolute() => Some(length.to_px()),
    // Unsupported units in sizes make the entry invalid; caller will fall back.
    _ => None,
  }
}

/// An anonymous box generated by the layout algorithm
///
/// Anonymous boxes don't correspond to DOM elements. They're inserted
/// to satisfy CSS layout rules.
///
/// Example: When a block container has both block and inline children,
/// anonymous block boxes wrap the inline children.
///
/// Reference: CSS 2.1 Section 9.2.1.1 (Anonymous block boxes)
#[derive(Debug, Clone)]
pub struct AnonymousBox {
  /// What kind of anonymous box?
  pub anonymous_type: AnonymousType,
}

/// Types of anonymous boxes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnonymousType {
  /// Anonymous block box
  ///
  /// Generated when a block container has mixed inline/block children.
  Block,

  /// Anonymous inline box
  ///
  /// Generated to wrap text nodes that aren't in explicit inline elements.
  Inline,

  /// Anonymous table wrapper box
  ///
  /// Generated around tables to contain captions.
  TableWrapper,

  /// Anonymous table row group box (tbody)
  ///
  /// Generated when rows aren't in explicit row groups.
  TableRowGroup,

  /// Anonymous table row box
  ///
  /// Generated when table cells aren't in explicit rows.
  TableRow,

  /// Anonymous table cell box
  ///
  /// Generated when content isn't in explicit cells.
  TableCell,
}

/// Different types of boxes in the box tree
///
/// This enum discriminates between the different kinds of CSS boxes.
/// Each variant contains type-specific data.
#[derive(Debug, Clone)]
pub enum BoxType {
  /// Block-level box (div, p, h1, etc.)
  Block(BlockBox),

  /// Inline-level box (span, a, em, etc.)
  Inline(InlineBox),

  /// Text box (actual text content)
  Text(TextBox),

  /// List marker box
  Marker(MarkerBox),

  /// Replaced element (img, video, canvas, etc.)
  Replaced(ReplacedBox),

  /// Anonymous box (generated by layout algorithm)
  Anonymous(AnonymousBox),
}

impl BoxType {
  /// Returns true if this box type is block-level
  pub fn is_block_level(&self) -> bool {
    match self {
      BoxType::Block(_) | BoxType::Replaced(_) => true,
      BoxType::Anonymous(anon) => matches!(
        anon.anonymous_type,
        AnonymousType::Block
          | AnonymousType::TableWrapper
          | AnonymousType::TableRowGroup
          | AnonymousType::TableRow
          | AnonymousType::TableCell
      ),
      _ => false,
    }
  }

  /// Returns true if this box type is inline-level
  pub fn is_inline_level(&self) -> bool {
    match self {
      BoxType::Inline(_) | BoxType::Text(_) | BoxType::Marker(_) => true,
      BoxType::Anonymous(anon) => matches!(anon.anonymous_type, AnonymousType::Inline),
      _ => false,
    }
  }

  /// Returns true if this is a text box
  pub fn is_text(&self) -> bool {
    matches!(self, BoxType::Text(_) | BoxType::Marker(_))
  }

  /// Returns true if this is a list marker box
  pub fn is_marker(&self) -> bool {
    matches!(self, BoxType::Marker(_))
  }

  /// Returns true if this is a replaced element
  pub fn is_replaced(&self) -> bool {
    matches!(self, BoxType::Replaced(_))
  }

  /// Returns true if this is an anonymous box
  pub fn is_anonymous(&self) -> bool {
    matches!(self, BoxType::Anonymous(_))
  }

  /// Gets the formatting context this box establishes (if any)
  pub fn formatting_context(&self) -> Option<FormattingContextType> {
    match self {
      BoxType::Block(block) => Some(block.formatting_context),
      BoxType::Inline(inline) => inline.formatting_context,
      BoxType::Replaced(_) => Some(FormattingContextType::Block),
      _ => None,
    }
  }
}

impl fmt::Display for BoxType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      BoxType::Block(_) => write!(f, "Block"),
      BoxType::Inline(_) => write!(f, "Inline"),
      BoxType::Text(_) => write!(f, "Text"),
      BoxType::Marker(_) => write!(f, "Marker"),
      BoxType::Replaced(_) => write!(f, "Replaced"),
      BoxType::Anonymous(anon) => match anon.anonymous_type {
        AnonymousType::Block => write!(f, "AnonymousBlock"),
        AnonymousType::Inline => write!(f, "AnonymousInline"),
        AnonymousType::TableWrapper => write!(f, "AnonymousTableWrapper"),
        AnonymousType::TableRowGroup => write!(f, "AnonymousTableRowGroup"),
        AnonymousType::TableRow => write!(f, "AnonymousTableRow"),
        AnonymousType::TableCell => write!(f, "AnonymousTableCell"),
      },
    }
  }
}

/// A single box in the box tree
///
/// Represents a CSS box (could be element, text, anonymous, etc.)
///
/// # Important Properties
///
/// - **Immutable**: Once created, a BoxNode doesn't change
/// - **No Position**: Boxes don't know where they'll be positioned (that's fragments)
/// - **Shared Styles**: ComputedStyle is Arc-ed and shared with fragments
/// - **Recursive**: Children form a tree
///
/// # Examples
///
/// ```
/// use std::sync::Arc;
/// use fastrender::{BoxNode, FormattingContextType};
/// use fastrender::ComputedStyle;
///
/// let style = Arc::new(ComputedStyle::default());
/// let box_node = BoxNode::new_block(
///     style,
///     FormattingContextType::Block,
///     vec![],
/// );
///
/// assert!(box_node.is_block_level());
/// ```
#[derive(Debug, Clone)]
pub struct BoxNode {
  /// Computed style for this box (shared with fragments)
  ///
  /// Using Arc because:
  /// - Shared between box and its fragments
  /// - Immutable after computation
  /// - Reduces memory usage for cloned trees
  pub style: Arc<ComputedStyle>,

  /// What kind of box is this?
  pub box_type: BoxType,

  /// Child boxes in document order
  pub children: Vec<BoxNode>,

  /// Unique identifier for caching and debugging
  pub id: usize,

  /// Debug information (element name, class, id)
  ///
  /// Optional - only populated in debug builds or with dev tools enabled
  pub debug_info: Option<DebugInfo>,

  /// Styled node identifier that produced this box (pre-order traversal id).
  pub styled_node_id: Option<usize>,

  /// Optional computed style overrides for `::first-line`.
  pub first_line_style: Option<Arc<ComputedStyle>>,

  /// Optional computed style overrides for `::first-letter`.
  pub first_letter_style: Option<Arc<ComputedStyle>>,
}

impl BoxNode {
  /// Creates a new block box
  ///
  /// # Examples
  ///
  /// ```
  /// use std::sync::Arc;
  /// use fastrender::{BoxNode, FormattingContextType};
  /// use fastrender::ComputedStyle;
  ///
  /// let style = Arc::new(ComputedStyle::default());
  /// let box_node = BoxNode::new_block(
  ///     style,
  ///     FormattingContextType::Block,
  ///     vec![],
  /// );
  ///
  /// assert!(box_node.is_block_level());
  /// ```
  pub fn new_block(
    style: Arc<ComputedStyle>,
    fc: FormattingContextType,
    children: Vec<BoxNode>,
  ) -> Self {
    Self {
      style,
      box_type: BoxType::Block(BlockBox {
        formatting_context: fc,
      }),
      children,
      id: 0,
      debug_info: None,
      styled_node_id: None,
      first_line_style: None,
      first_letter_style: None,
    }
  }

  /// Creates a new inline box
  pub fn new_inline(style: Arc<ComputedStyle>, children: Vec<BoxNode>) -> Self {
    Self {
      style,
      box_type: BoxType::Inline(InlineBox {
        formatting_context: None,
      }),
      children,
      id: 0,
      debug_info: None,
      styled_node_id: None,
      first_line_style: None,
      first_letter_style: None,
    }
  }

  /// Creates a new inline-block box
  pub fn new_inline_block(
    style: Arc<ComputedStyle>,
    fc: FormattingContextType,
    children: Vec<BoxNode>,
  ) -> Self {
    Self {
      style,
      box_type: BoxType::Inline(InlineBox {
        formatting_context: Some(fc),
      }),
      children,
      id: 0,
      debug_info: None,
      styled_node_id: None,
      first_line_style: None,
      first_letter_style: None,
    }
  }

  /// Creates a new text box
  pub fn new_text(style: Arc<ComputedStyle>, text: String) -> Self {
    Self {
      style,
      box_type: BoxType::Text(TextBox { text }),
      children: Vec::new(),
      id: 0,
      debug_info: None,
      styled_node_id: None,
      first_line_style: None,
      first_letter_style: None,
    }
  }

  /// Creates a new list marker box
  pub fn new_marker(style: Arc<ComputedStyle>, content: MarkerContent) -> Self {
    Self {
      style,
      box_type: BoxType::Marker(MarkerBox { content }),
      children: Vec::new(),
      id: 0,
      debug_info: None,
      styled_node_id: None,
      first_line_style: None,
      first_letter_style: None,
    }
  }

  /// Creates a new replaced box
  pub fn new_replaced(
    style: Arc<ComputedStyle>,
    replaced_type: ReplacedType,
    intrinsic_size: Option<Size>,
    aspect_ratio: Option<f32>,
  ) -> Self {
    Self {
      style,
      box_type: BoxType::Replaced(ReplacedBox {
        replaced_type,
        intrinsic_size,
        aspect_ratio,
      }),
      children: Vec::new(),
      id: 0,
      debug_info: None,
      styled_node_id: None,
      first_line_style: None,
      first_letter_style: None,
    }
  }

  /// Creates an anonymous block box
  pub fn new_anonymous_block(style: Arc<ComputedStyle>, children: Vec<BoxNode>) -> Self {
    Self {
      style,
      box_type: BoxType::Anonymous(AnonymousBox {
        anonymous_type: AnonymousType::Block,
      }),
      children,
      id: 0,
      debug_info: None,
      styled_node_id: None,
      first_line_style: None,
      first_letter_style: None,
    }
  }

  /// Creates an anonymous inline box
  pub fn new_anonymous_inline(style: Arc<ComputedStyle>, children: Vec<BoxNode>) -> Self {
    Self {
      style,
      box_type: BoxType::Anonymous(AnonymousBox {
        anonymous_type: AnonymousType::Inline,
      }),
      children,
      id: 0,
      debug_info: None,
      styled_node_id: None,
      first_line_style: None,
      first_letter_style: None,
    }
  }

  /// Adds debug information
  ///
  /// This is a builder-style method for convenience.
  pub fn with_debug_info(mut self, info: DebugInfo) -> Self {
    self.debug_info = Some(info);
    self
  }

  /// Unique identifier for this box within the box tree.
  pub fn id(&self) -> usize {
    self.id
  }

  // Type query methods

  /// Returns true if this is a block-level box
  ///
  /// Block-level boxes participate in block formatting context.
  pub fn is_block_level(&self) -> bool {
    self.box_type.is_block_level()
  }

  /// Returns true if this is an inline-level box
  ///
  /// Inline-level boxes participate in inline formatting context.
  pub fn is_inline_level(&self) -> bool {
    self.box_type.is_inline_level()
  }

  /// Returns true if this is a text box
  pub fn is_text(&self) -> bool {
    self.box_type.is_text()
  }

  /// Returns true if this is a replaced element
  pub fn is_replaced(&self) -> bool {
    self.box_type.is_replaced()
  }

  /// Returns true if this is an anonymous box
  pub fn is_anonymous(&self) -> bool {
    self.box_type.is_anonymous()
  }

  /// Gets the formatting context this box establishes (if any)
  ///
  /// Returns None for inline and text boxes that don't establish contexts.
  pub fn formatting_context(&self) -> Option<FormattingContextType> {
    self.box_type.formatting_context()
  }

  /// Returns true if this box is a block container
  ///
  /// Block containers can contain block-level children and establish
  /// a block formatting context (or participate in one).
  pub fn is_block_container(&self) -> bool {
    match &self.box_type {
      BoxType::Block(_) => true,
      BoxType::Inline(inline) => inline.formatting_context.is_some(), // inline-block
      BoxType::Anonymous(anon) => matches!(
        anon.anonymous_type,
        AnonymousType::Block | AnonymousType::TableCell
      ),
      _ => false,
    }
  }

  /// Returns true if this box is an inline container
  ///
  /// Inline containers contain inline-level children and participate
  /// in inline formatting context.
  pub fn is_inline_container(&self) -> bool {
    matches!(&self.box_type, BoxType::Inline(_))
  }

  /// Returns true if this box generates a formatting context
  ///
  /// Boxes that generate formatting contexts are independent layout roots.
  /// Their internal layout doesn't affect outside, and vice versa.
  pub fn generates_formatting_context(&self) -> bool {
    self.formatting_context().is_some()
  }

  /// Returns true if this is a table-internal box
  ///
  /// Table-internal boxes participate in table layout algorithms.
  pub fn is_table_internal(&self) -> bool {
    match &self.box_type {
      BoxType::Anonymous(anon) => matches!(
        anon.anonymous_type,
        AnonymousType::TableWrapper
          | AnonymousType::TableRowGroup
          | AnonymousType::TableRow
          | AnonymousType::TableCell
      ),
      _ => false,
    }
  }

  /// Gets text content if this is a text box
  pub fn text(&self) -> Option<&str> {
    match &self.box_type {
      BoxType::Text(text_box) => Some(&text_box.text),
      BoxType::Marker(marker_box) => match &marker_box.content {
        MarkerContent::Text(text) => Some(text.as_str()),
        MarkerContent::Image(_) => None,
      },
      _ => None,
    }
  }

  /// Returns the number of children
  pub fn child_count(&self) -> usize {
    self.children.len()
  }

  /// Returns an iterator over children
  pub fn children_iter(&self) -> impl Iterator<Item = &BoxNode> {
    self.children.iter()
  }
}

/// A tree of CSS boxes
///
/// The box tree is generated from styled DOM and consumed by layout algorithms.
/// It's immutable after construction.
///
/// # Examples
///
/// ```
/// use std::sync::Arc;
/// use fastrender::{BoxTree, BoxNode, FormattingContextType};
/// use fastrender::ComputedStyle;
///
/// let style = Arc::new(ComputedStyle::default());
/// let root = BoxNode::new_block(
///     style,
///     FormattingContextType::Block,
///     vec![],
/// );
///
/// let tree = BoxTree::new(root);
/// assert!(tree.root.is_block_level());
/// ```
#[derive(Debug, Clone)]
pub struct BoxTree {
  /// The root box (typically the root element's principal box)
  pub root: BoxNode,
}

fn assign_box_ids(node: &mut BoxNode, next_id: &mut usize) {
  node.id = *next_id;
  *next_id += 1;
  for child in &mut node.children {
    assign_box_ids(child, next_id);
  }
}

impl BoxTree {
  /// Creates a new box tree with the given root
  pub fn new(root: BoxNode) -> Self {
    let mut root = root;
    let mut next_id = 1;
    assign_box_ids(&mut root, &mut next_id);
    Self { root }
  }

  /// Counts total boxes in the tree (including root)
  pub fn count_boxes(&self) -> usize {
    fn count_recursive(node: &BoxNode) -> usize {
      1 + node.children.iter().map(count_recursive).sum::<usize>()
    }
    count_recursive(&self.root)
  }

  /// Counts text boxes in the tree
  pub fn count_text_boxes(&self) -> usize {
    fn count_recursive(node: &BoxNode) -> usize {
      let self_count = usize::from(node.is_text());
      self_count + node.children.iter().map(count_recursive).sum::<usize>()
    }
    count_recursive(&self.root)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::geometry::Size;
  use crate::style::display::FormattingContextType;
  use crate::style::media::MediaContext;
  use crate::style::values::Length;
  use crate::style::values::LengthUnit;

  fn default_style() -> Arc<ComputedStyle> {
    Arc::new(ComputedStyle::default())
  }

  #[test]
  fn test_create_block_box() {
    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

    assert!(box_node.is_block_level());
    assert!(!box_node.is_inline_level());
    assert_eq!(
      box_node.formatting_context(),
      Some(FormattingContextType::Block)
    );
  }

  #[test]
  fn test_create_inline_box() {
    let box_node = BoxNode::new_inline(default_style(), vec![]);

    assert!(box_node.is_inline_level());
    assert!(!box_node.is_block_level());
    assert_eq!(box_node.formatting_context(), None);
  }

  #[test]
  fn test_create_text_box() {
    let box_node = BoxNode::new_text(default_style(), "Hello".to_string());

    assert!(box_node.is_inline_level());
    assert!(box_node.is_text());
    assert_eq!(box_node.text(), Some("Hello"));
    assert_eq!(box_node.children.len(), 0);
  }

  #[test]
  fn test_create_replaced_box() {
    let box_node = BoxNode::new_replaced(
      default_style(),
      ReplacedType::Image {
        src: "image.png".to_string(),
        alt: None,
        sizes: None,
        srcset: Vec::new(),
        picture_sources: Vec::new(),
      },
      Some(Size::new(100.0, 50.0)),
      Some(2.0),
    );

    assert!(box_node.is_replaced());
    assert!(box_node.is_block_level());
  }

  #[test]
  fn test_create_inline_block() {
    let box_node = BoxNode::new_inline_block(default_style(), FormattingContextType::Block, vec![]);

    assert!(box_node.is_inline_level());
    assert_eq!(
      box_node.formatting_context(),
      Some(FormattingContextType::Block)
    );
  }

  #[test]
  fn test_box_hierarchy() {
    let text1 = BoxNode::new_text(default_style(), "Text 1".to_string());
    let text2 = BoxNode::new_text(default_style(), "Text 2".to_string());

    let inline_box = BoxNode::new_inline(default_style(), vec![text1, text2]);

    let block_box = BoxNode::new_block(
      default_style(),
      FormattingContextType::Block,
      vec![inline_box],
    );

    assert_eq!(block_box.children.len(), 1);
    assert_eq!(block_box.children[0].children.len(), 2);
    assert_eq!(block_box.child_count(), 1);
  }

  #[test]
  fn test_debug_info() {
    let debug_info = DebugInfo::new(
      Some("div".to_string()),
      Some("header".to_string()),
      vec!["navbar".to_string(), "sticky".to_string()],
    );

    assert_eq!(debug_info.to_selector(), "div#header.navbar.sticky");

    let box_node = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![])
      .with_debug_info(debug_info);

    assert!(box_node.debug_info.is_some());
  }

  #[test]
  fn test_box_tree() {
    let root = BoxNode::new_block(
      default_style(),
      FormattingContextType::Block,
      vec![
        BoxNode::new_text(default_style(), "Text".to_string()),
        BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]),
      ],
    );

    let tree = BoxTree::new(root);

    assert_eq!(tree.count_boxes(), 3); // root + text + block
    assert_eq!(tree.count_text_boxes(), 1);
  }

  #[test]
  fn test_anonymous_block_box() {
    let box_node = BoxNode::new_anonymous_block(default_style(), vec![]);

    assert!(box_node.is_anonymous());
    assert!(box_node.is_block_level());
  }

  #[test]
  fn test_children_iterator() {
    let text1 = BoxNode::new_text(default_style(), "Text 1".to_string());
    let text2 = BoxNode::new_text(default_style(), "Text 2".to_string());
    let box_node = BoxNode::new_inline(default_style(), vec![text1, text2]);

    let count = box_node.children_iter().count();
    assert_eq!(count, 2);
  }

  #[test]
  fn test_is_block_container() {
    let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    let inline_block =
      BoxNode::new_inline_block(default_style(), FormattingContextType::Block, vec![]);
    let inline = BoxNode::new_inline(default_style(), vec![]);
    let text = BoxNode::new_text(default_style(), "text".to_string());

    assert!(block.is_block_container());
    assert!(inline_block.is_block_container());
    assert!(!inline.is_block_container());
    assert!(!text.is_block_container());
  }

  #[test]
  fn test_is_inline_container() {
    let inline = BoxNode::new_inline(default_style(), vec![]);
    let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    let text = BoxNode::new_text(default_style(), "text".to_string());

    assert!(inline.is_inline_container());
    assert!(!block.is_inline_container());
    assert!(!text.is_inline_container());
  }

  #[test]
  fn test_generates_formatting_context() {
    let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    let inline_block =
      BoxNode::new_inline_block(default_style(), FormattingContextType::Flex, vec![]);
    let inline = BoxNode::new_inline(default_style(), vec![]);
    let text = BoxNode::new_text(default_style(), "text".to_string());
    let replaced = BoxNode::new_replaced(
      default_style(),
      ReplacedType::Image {
        src: "img.png".to_string(),
        alt: None,
        sizes: None,
        srcset: Vec::new(),
        picture_sources: Vec::new(),
      },
      None,
      None,
    );

    assert!(block.generates_formatting_context());
    assert!(inline_block.generates_formatting_context());
    assert!(replaced.generates_formatting_context());
    assert!(!inline.generates_formatting_context());
    assert!(!text.generates_formatting_context());
  }

  #[test]
  fn test_is_table_internal() {
    let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    let anon_block = BoxNode::new_anonymous_block(default_style(), vec![]);

    assert!(!block.is_table_internal());
    assert!(!anon_block.is_table_internal());
  }

  #[test]
  fn test_block_container_with_flex() {
    let flex_block = BoxNode::new_block(default_style(), FormattingContextType::Flex, vec![]);

    assert!(flex_block.is_block_container());
    assert!(flex_block.generates_formatting_context());
    assert_eq!(
      flex_block.formatting_context(),
      Some(FormattingContextType::Flex)
    );
  }

  #[test]
  fn test_block_container_with_grid() {
    let grid_block = BoxNode::new_block(default_style(), FormattingContextType::Grid, vec![]);

    assert!(grid_block.is_block_container());
    assert!(grid_block.generates_formatting_context());
    assert_eq!(
      grid_block.formatting_context(),
      Some(FormattingContextType::Grid)
    );
  }

  #[test]
  fn test_inline_block_formatting_context() {
    let inline_block_flex =
      BoxNode::new_inline_block(default_style(), FormattingContextType::Flex, vec![]);
    let inline_block_grid =
      BoxNode::new_inline_block(default_style(), FormattingContextType::Grid, vec![]);

    assert!(inline_block_flex.is_block_container());
    assert!(inline_block_flex.generates_formatting_context());
    assert!(inline_block_flex.is_inline_level());
    assert_eq!(
      inline_block_flex.formatting_context(),
      Some(FormattingContextType::Flex)
    );

    assert!(inline_block_grid.is_block_container());
    assert!(inline_block_grid.generates_formatting_context());
    assert!(inline_block_grid.is_inline_level());
    assert_eq!(
      inline_block_grid.formatting_context(),
      Some(FormattingContextType::Grid)
    );
  }

  #[test]
  fn test_box_type_display() {
    let block = BoxType::Block(BlockBox {
      formatting_context: FormattingContextType::Block,
    });
    let inline = BoxType::Inline(InlineBox {
      formatting_context: None,
    });
    let text = BoxType::Text(TextBox {
      text: "hello".to_string(),
    });
    let anon_block = BoxType::Anonymous(AnonymousBox {
      anonymous_type: AnonymousType::Block,
    });
    let anon_inline = BoxType::Anonymous(AnonymousBox {
      anonymous_type: AnonymousType::Inline,
    });

    assert_eq!(format!("{}", block), "Block");
    assert_eq!(format!("{}", inline), "Inline");
    assert_eq!(format!("{}", text), "Text");
    assert_eq!(format!("{}", anon_block), "AnonymousBlock");
    assert_eq!(format!("{}", anon_inline), "AnonymousInline");
  }

  #[test]
  fn image_source_prefers_width_descriptor_with_sizes() {
    let img = ReplacedType::Image {
      src: "fallback".to_string(),
      alt: None,
      srcset: vec![
        SrcsetCandidate {
          url: "100w".to_string(),
          descriptor: SrcsetDescriptor::Width(100),
        },
        SrcsetCandidate {
          url: "300w".to_string(),
          descriptor: SrcsetDescriptor::Width(300),
        },
      ],
      sizes: Some(SizesList {
        entries: vec![SizesEntry {
          media: None,
          length: Length::new(50.0, LengthUnit::Vw),
        }],
      }),
      picture_sources: Vec::new(),
    };

    let viewport = Size::new(200.0, 100.0);
    let media_ctx =
      MediaContext::screen(viewport.width, viewport.height).with_device_pixel_ratio(2.0);
    let chosen = img.image_source_for_context(ImageSelectionContext {
      scale: 2.0,
      slot_width: None,
      viewport: Some(viewport),
      media_context: Some(&media_ctx),
      font_size: Some(16.0),
    });

    assert_eq!(chosen, "300w");
  }

  #[test]
  fn sizes_default_to_last_entry_when_no_media_match() {
    let img = ReplacedType::Image {
      src: "fallback".to_string(),
      alt: None,
      srcset: vec![
        SrcsetCandidate {
          url: "100w".to_string(),
          descriptor: SrcsetDescriptor::Width(100),
        },
        SrcsetCandidate {
          url: "400w".to_string(),
          descriptor: SrcsetDescriptor::Width(400),
        },
      ],
      sizes: Some(SizesList {
        entries: vec![
          SizesEntry {
            media: Some(vec![crate::style::media::MediaQuery::parse(
              "(max-width: 10px)",
            )
            .unwrap()]),
            length: Length::new(50.0, LengthUnit::Vw),
          },
          SizesEntry {
            media: None,
            length: Length::px(300.0),
          },
        ],
      }),
      picture_sources: Vec::new(),
    };

    let viewport = Size::new(1200.0, 800.0);
    let media_ctx =
      MediaContext::screen(viewport.width, viewport.height).with_device_pixel_ratio(1.0);
    let chosen = img.image_source_for_context(ImageSelectionContext {
      scale: 1.0,
      slot_width: None,
      viewport: Some(viewport),
      media_context: Some(&media_ctx),
      font_size: Some(16.0),
    });

    assert_eq!(
      chosen, "400w",
      "last sizes entry (300px) should drive selection"
    );
  }

  #[test]
  fn sizes_resolve_ex_ch_lengths() {
    let img = ReplacedType::Image {
      src: "fallback".to_string(),
      alt: None,
      srcset: vec![
        SrcsetCandidate {
          url: "100w".to_string(),
          descriptor: SrcsetDescriptor::Width(100),
        },
        SrcsetCandidate {
          url: "300w".to_string(),
          descriptor: SrcsetDescriptor::Width(300),
        },
      ],
      sizes: Some(SizesList {
        entries: vec![SizesEntry {
          media: None,
          // 10ch at 16px font size = 80px slot width; smallest density >=1 is 100w.
          length: Length::new(10.0, LengthUnit::Ch),
        }],
      }),
      picture_sources: Vec::new(),
    };

    let viewport = Size::new(200.0, 100.0);
    let media_ctx =
      MediaContext::screen(viewport.width, viewport.height).with_device_pixel_ratio(1.0);
    let chosen = img.image_source_for_context(ImageSelectionContext {
      scale: 1.0,
      slot_width: None,
      viewport: Some(viewport),
      media_context: Some(&media_ctx),
      font_size: Some(16.0),
    });

    assert_eq!(chosen, "100w");
  }

  #[test]
  fn width_descriptors_default_to_viewport_when_no_sizes_and_no_slot() {
    let img = ReplacedType::Image {
      src: "fallback".to_string(),
      alt: None,
      srcset: vec![
        SrcsetCandidate {
          url: "400w".to_string(),
          descriptor: SrcsetDescriptor::Width(400),
        },
        SrcsetCandidate {
          url: "800w".to_string(),
          descriptor: SrcsetDescriptor::Width(800),
        },
      ],
      sizes: None,
      picture_sources: Vec::new(),
    };

    // With viewport width 500 and DPR 2, density candidates are 0.8 and 1.6.
    let viewport = Size::new(500.0, 300.0);
    let media_ctx =
      MediaContext::screen(viewport.width, viewport.height).with_device_pixel_ratio(2.0);
    let chosen = img.image_source_for_context(ImageSelectionContext {
      scale: 2.0,
      slot_width: None,
      viewport: Some(viewport),
      media_context: Some(&media_ctx),
      font_size: Some(16.0),
    });

    assert_eq!(
      chosen, "800w",
      "viewport fallback should make 800w best for DPR=2"
    );
  }

  #[test]
  fn non_positive_slot_width_falls_back_to_viewport_for_width_descriptors() {
    let img = ReplacedType::Image {
      src: "fallback".to_string(),
      alt: None,
      srcset: vec![
        SrcsetCandidate {
          url: "100w".to_string(),
          descriptor: SrcsetDescriptor::Width(100),
        },
        SrcsetCandidate {
          url: "400w".to_string(),
          descriptor: SrcsetDescriptor::Width(400),
        },
      ],
      sizes: None,
      picture_sources: Vec::new(),
    };

    // Slot width is zero (e.g., auto-sized placeholder), so selection should fall back to viewport.
    let viewport = Size::new(400.0, 300.0);
    let media_ctx =
      MediaContext::screen(viewport.width, viewport.height).with_device_pixel_ratio(1.0);
    let chosen = img.image_source_for_context(ImageSelectionContext {
      scale: 1.0,
      slot_width: Some(0.0),
      viewport: Some(viewport),
      media_context: Some(&media_ctx),
      font_size: Some(16.0),
    });

    assert_eq!(
      chosen, "400w",
      "zero-width slot should use viewport width for width descriptors"
    );
  }

  #[test]
  fn image_sources_with_fallback_prioritizes_selected_then_src() {
    let img = ReplacedType::Image {
      src: "base".to_string(),
      alt: None,
      srcset: vec![
        SrcsetCandidate {
          url: "2x".to_string(),
          descriptor: SrcsetDescriptor::Density(2.0),
        },
        SrcsetCandidate {
          url: "1x".to_string(),
          descriptor: SrcsetDescriptor::Density(1.0),
        },
      ],
      sizes: None,
      picture_sources: Vec::new(),
    };

    let media_ctx = MediaContext::screen(800.0, 600.0).with_device_pixel_ratio(2.0);
    let sources = img.image_sources_with_fallback(ImageSelectionContext {
      scale: 2.0,
      slot_width: Some(400.0),
      viewport: Some(Size::new(800.0, 600.0)),
      media_context: Some(&media_ctx),
      font_size: Some(16.0),
    });

    assert_eq!(sources[0], "2x", "selected srcset candidate should lead");
    assert!(
      sources.contains(&"base".to_string()),
      "base src should remain available as fallback"
    );
    assert_eq!(
      sources.len(),
      3,
      "srcset entries and base src should be unique"
    );
  }

  #[test]
  fn picture_source_respects_media_and_type_ordering() {
    let img = ReplacedType::Image {
      src: "fallback".to_string(),
      alt: None,
      srcset: vec![],
      sizes: None,
      picture_sources: vec![
        PictureSource {
          srcset: vec![SrcsetCandidate {
            url: "skip-me".to_string(),
            descriptor: SrcsetDescriptor::Density(1.0),
          }],
          sizes: None,
          media: None,
          mime_type: Some("application/json".to_string()),
        },
        PictureSource {
          srcset: vec![SrcsetCandidate {
            url: "avif-2x".to_string(),
            descriptor: SrcsetDescriptor::Density(2.0),
          }],
          sizes: None,
          media: Some(vec![crate::style::media::MediaQuery::parse(
            "(max-width: 500px)",
          )
          .unwrap()]),
          mime_type: Some("image/avif".to_string()),
        },
        PictureSource {
          srcset: vec![SrcsetCandidate {
            url: "webp-2x".to_string(),
            descriptor: SrcsetDescriptor::Density(2.0),
          }],
          sizes: None,
          media: None,
          mime_type: Some("image/webp; codecs=vp8".to_string()),
        },
      ],
    };

    let small_viewport = Size::new(400.0, 300.0);
    let small_media = MediaContext::screen(small_viewport.width, small_viewport.height)
      .with_device_pixel_ratio(2.0);
    let chosen_small = img.image_source_for_context(ImageSelectionContext {
      scale: 2.0,
      slot_width: None,
      viewport: Some(small_viewport),
      media_context: Some(&small_media),
      font_size: Some(16.0),
    });
    assert_eq!(chosen_small, "avif-2x");

    let large_viewport = Size::new(1200.0, 800.0);
    let large_media = MediaContext::screen(large_viewport.width, large_viewport.height)
      .with_device_pixel_ratio(2.0);
    let chosen_large = img.image_source_for_context(ImageSelectionContext {
      scale: 2.0,
      slot_width: None,
      viewport: Some(large_viewport),
      media_context: Some(&large_media),
      font_size: Some(16.0),
    });
    assert_eq!(chosen_large, "webp-2x");
  }

  #[test]
  fn picture_source_uses_sizes_with_width_descriptors() {
    let img = ReplacedType::Image {
      src: "fallback".to_string(),
      alt: None,
      srcset: vec![],
      sizes: None,
      picture_sources: vec![PictureSource {
        srcset: vec![
          SrcsetCandidate {
            url: "400w".to_string(),
            descriptor: SrcsetDescriptor::Width(400),
          },
          SrcsetCandidate {
            url: "800w".to_string(),
            descriptor: SrcsetDescriptor::Width(800),
          },
        ],
        sizes: Some(SizesList {
          entries: vec![SizesEntry {
            media: None,
            length: Length::new(50.0, LengthUnit::Vw),
          }],
        }),
        media: None,
        mime_type: None,
      }],
    };

    let viewport = Size::new(800.0, 600.0);
    let media_ctx =
      MediaContext::screen(viewport.width, viewport.height).with_device_pixel_ratio(2.0);
    let chosen = img.image_source_for_context(ImageSelectionContext {
      scale: 2.0,
      slot_width: None,
      viewport: Some(viewport),
      media_context: Some(&media_ctx),
      font_size: Some(16.0),
    });

    assert_eq!(chosen, "800w");
  }

  #[test]
  fn iframe_srcdoc_prefers_inline_html_for_placeholder() {
    let iframe = ReplacedType::Iframe {
      src: "https://example.com".to_string(),
      srcdoc: Some("hello world".to_string()),
    };
    assert_eq!(iframe.placeholder_label(), Some("hello world"));

    let iframe_no_srcdoc = ReplacedType::Iframe {
      src: "https://example.com".to_string(),
      srcdoc: None,
    };
    assert_eq!(iframe_no_srcdoc.placeholder_label(), Some("iframe"));
  }

  #[test]
  fn video_image_sources_prefer_poster_only() {
    let video = ReplacedType::Video {
      src: "video.mp4".to_string(),
      poster: Some("thumb.png".to_string()),
    };
    let sources = video.image_sources_with_fallback(ImageSelectionContext {
      scale: 2.0,
      slot_width: Some(400.0),
      viewport: Some(Size::new(800.0, 600.0)),
      media_context: None,
      font_size: Some(16.0),
    });

    assert_eq!(
      sources,
      vec!["thumb.png".to_string()],
      "video should only expose poster for imaging"
    );
  }
}
