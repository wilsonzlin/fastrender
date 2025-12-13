//! CSS type definitions
//!
//! Core types for representing CSS stylesheets, rules, and values.

use crate::style::color::{Color, Rgba};
use crate::style::media::{MediaContext, MediaQuery};
use crate::style::values::Length;
use cssparser::ToCss;
use selectors::parser::SelectorList;
use std::fmt;

use super::selectors::FastRenderSelectorImpl;

// ============================================================================
// CssString wrapper for selectors crate compatibility
// ============================================================================

/// Wrapper for String that implements ToCss
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct CssString(pub String);

impl From<&str> for CssString {
    fn from(s: &str) -> Self {
        CssString(s.to_string())
    }
}

impl From<String> for CssString {
    fn from(s: String) -> Self {
        CssString(s)
    }
}

impl std::ops::Deref for CssString {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::borrow::Borrow<str> for CssString {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl ToCss for CssString {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        dest.write_str(&self.0)
    }
}

impl precomputed_hash::PrecomputedHash for CssString {
    fn precomputed_hash(&self) -> u32 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        self.0.hash(&mut hasher);
        hasher.finish() as u32
    }
}

// ============================================================================
// Stylesheet structures
// ============================================================================

/// Flattened style rule with its cascade layer ordering.
#[derive(Debug, Clone)]
pub struct CollectedRule<'a> {
    pub rule: &'a StyleRule,
    /// Cascade layer order (lexicographic; unlayered rules use a sentinel of u32::MAX).
    pub layer_order: Vec<u32>,
}

/// Stylesheet containing CSS rules
#[derive(Debug, Clone)]
pub struct StyleSheet {
    /// All CSS rules in the stylesheet (style rules and @-rules)
    pub rules: Vec<CssRule>,
}

/// A minimal interface for loading imported stylesheets.
///
/// Implementations can fetch from network, filesystem, or an in-memory map.
pub trait CssImportLoader {
    fn load(&self, url: &str) -> crate::error::Result<String>;
}

impl StyleSheet {
    /// Creates an empty stylesheet
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    /// Collects all applicable style rules, evaluating @media queries
    ///
    /// This flattens nested @media rules and filters by the given media context.
    pub fn collect_style_rules(&self, media_ctx: &MediaContext) -> Vec<CollectedRule<'_>> {
        let mut result = Vec::new();
        let mut registry = LayerRegistry::new();
        collect_rules_recursive(&self.rules, media_ctx, &mut registry, &[], &mut result);
        result
    }

    /// Collects all @font-face rules that apply to the current media context.
    pub fn collect_font_face_rules(&self, media_ctx: &MediaContext) -> Vec<FontFaceRule> {
        let mut result = Vec::new();
        collect_font_faces_recursive(&self.rules, media_ctx, &mut result);
        result
    }

    /// Resolve @import rules by fetching external stylesheets and inlining their rules.
    ///
    /// Imports are processed in order; only imports whose media lists match the provided
    /// `media_ctx` are inlined. Relative URLs are resolved against `base_url` when provided.
    pub fn resolve_imports<L: CssImportLoader + ?Sized>(
        &self,
        loader: &L,
        base_url: Option<&str>,
        media_ctx: &MediaContext,
    ) -> Self {
        let mut resolved = Vec::new();
        let mut seen = std::collections::HashSet::new();
        resolve_rules(&self.rules, loader, base_url, media_ctx, &mut seen, &mut resolved);
        StyleSheet { rules: resolved }
    }
}

/// Helper to recursively collect style rules from nested @media/@layer blocks
fn collect_rules_recursive<'a>(
    rules: &'a [CssRule],
    media_ctx: &MediaContext,
    registry: &mut LayerRegistry,
    current_layer: &[u32],
    out: &mut Vec<CollectedRule<'a>>,
) {
    for rule in rules {
        match rule {
            CssRule::Style(style_rule) => {
                let layer_order = if current_layer.is_empty() {
                    vec![u32::MAX]
                } else {
                    current_layer.to_vec()
                };
                out.push(CollectedRule {
                    rule: style_rule,
                    layer_order,
                });
            }
            CssRule::Media(media_rule) => {
                // Only include rules from @media blocks that match
                if media_ctx.evaluate(&media_rule.query) {
                    collect_rules_recursive(&media_rule.rules, media_ctx, registry, current_layer, out);
                }
            }
            CssRule::Import(_) => {
                // Imports are resolved before collection; nothing to add here.
            }
            CssRule::Layer(layer_rule) => {
                if layer_rule.rules.is_empty() {
                    for name in &layer_rule.names {
                        registry.ensure_path(current_layer, name);
                    }
                    if layer_rule.anonymous {
                        registry.ensure_anonymous(current_layer);
                    }
                    continue;
                }

                if layer_rule.anonymous {
                    let path = registry.ensure_anonymous(current_layer);
                    collect_rules_recursive(&layer_rule.rules, media_ctx, registry, &path, out);
                    continue;
                }

                if layer_rule.names.len() != 1 {
                    // Invalid layer block; skip the rules.
                    continue;
                }
                let path = registry.ensure_path(current_layer, &layer_rule.names[0]);
                collect_rules_recursive(&layer_rule.rules, media_ctx, registry, &path, out);
            }
            CssRule::FontFace(_) => {}
        }
    }
}

fn collect_font_faces_recursive(rules: &[CssRule], media_ctx: &MediaContext, out: &mut Vec<FontFaceRule>) {
    for rule in rules {
        match rule {
            CssRule::FontFace(face) => out.push(face.clone()),
            CssRule::Media(media_rule) => {
                if media_ctx.evaluate(&media_rule.query) {
                    collect_font_faces_recursive(&media_rule.rules, media_ctx, out);
                }
            }
            CssRule::Layer(layer_rule) => {
                if layer_rule.rules.is_empty() {
                    continue;
                }
                if layer_rule.anonymous {
                    collect_font_faces_recursive(&layer_rule.rules, media_ctx, out);
                    continue;
                }
                if layer_rule.names.len() != 1 {
                    continue;
                }
                collect_font_faces_recursive(&layer_rule.rules, media_ctx, out);
            }
            CssRule::Style(_) | CssRule::Import(_) => {}
        }
    }
}

impl Default for StyleSheet {
    fn default() -> Self {
        Self::new()
    }
}

/// A CSS rule (style rule or @-rule)
#[derive(Debug, Clone)]
pub enum CssRule {
    /// A style rule (selectors + declarations)
    Style(StyleRule),
    /// A @media rule containing conditional rules
    Media(MediaRule),
    /// An @import rule (href + optional media list)
    Import(ImportRule),
    /// A @layer rule establishing cascade layers
    Layer(LayerRule),
    /// A @font-face rule defining a downloadable font.
    FontFace(FontFaceRule),
}

/// A @media rule containing conditional rules
#[derive(Debug, Clone)]
pub struct MediaRule {
    /// The media query to evaluate
    pub query: MediaQuery,
    /// Rules that apply when query matches (can be nested)
    pub rules: Vec<CssRule>,
}

/// A @import rule targeting another stylesheet
#[derive(Debug, Clone)]
pub struct ImportRule {
    /// Target stylesheet URL (as authored, possibly relative)
    pub href: String,
    /// Optional list of media queries that gate the import
    pub media: Vec<MediaQuery>,
}

/// A single CSS style rule (selectors + declarations)
#[derive(Debug, Clone)]
pub struct StyleRule {
    pub selectors: SelectorList<FastRenderSelectorImpl>,
    pub declarations: Vec<Declaration>,
}

/// A @layer rule (blockless or with nested rules).
#[derive(Debug, Clone)]
pub struct LayerRule {
    /// Named layers declared in the prelude (empty for anonymous layers).
    pub names: Vec<Vec<String>>,
    /// Nested rules (empty for blockless declarations).
    pub rules: Vec<CssRule>,
    /// Whether this is an anonymous layer (no names).
    pub anonymous: bool,
}

/// A @font-face rule with parsed descriptors.
#[derive(Debug, Clone)]
pub struct FontFaceRule {
    /// The family name exposed to CSS.
    pub family: Option<String>,
    /// Ordered font sources from the `src` descriptor.
    pub sources: Vec<FontFaceSource>,
    /// Style descriptor (normal/italic/oblique with optional angle range).
    pub style: FontFaceStyle,
    /// Weight range expressed in CSS absolute weights.
    pub weight: (u16, u16),
    /// Stretch range in percentages.
    pub stretch: (f32, f32),
}

impl Default for FontFaceRule {
    fn default() -> Self {
        Self {
            family: None,
            sources: Vec::new(),
            style: FontFaceStyle::Normal,
            weight: (400, 400),
            stretch: (100.0, 100.0),
        }
    }
}

/// A single font source in `src`.
#[derive(Debug, Clone)]
pub enum FontFaceSource {
    /// A downloadable URL source.
    Url(String),
    /// A locally installed font name.
    Local(String),
}

/// Font style descriptor for @font-face.
#[derive(Debug, Clone)]
pub enum FontFaceStyle {
    Normal,
    Italic,
    /// Oblique with an optional angle range (degrees).
    Oblique {
        range: Option<(f32, f32)>,
    },
}

/// A CSS property declaration
#[derive(Debug, Clone)]
pub struct Declaration {
    pub property: String,
    pub value: PropertyValue,
    /// Raw token string as authored (after stripping !important/semicolon). Used for custom properties.
    pub raw_value: String,
    pub important: bool,
}

fn resolve_rules<L: CssImportLoader + ?Sized>(
    rules: &[CssRule],
    loader: &L,
    base_url: Option<&str>,
    media_ctx: &MediaContext,
    seen: &mut std::collections::HashSet<String>,
    out: &mut Vec<CssRule>,
) {
    use url::Url;

    for rule in rules {
        match rule {
            CssRule::Style(_) | CssRule::Media(_) => out.push(rule.clone()),
            CssRule::Layer(layer_rule) => {
                let mut resolved_children = Vec::new();
                resolve_rules(
                    &layer_rule.rules,
                    loader,
                    base_url,
                    media_ctx,
                    seen,
                    &mut resolved_children,
                );
                out.push(CssRule::Layer(LayerRule {
                    names: layer_rule.names.clone(),
                    rules: resolved_children,
                    anonymous: layer_rule.anonymous,
                }));
            }
            CssRule::Import(import) => {
                let media_matches = import.media.is_empty() || media_ctx.evaluate_list(&import.media);
                if !media_matches {
                    continue;
                }

                let mut resolved_href = import.href.clone();
                if let Some(base) = base_url {
                    if let Ok(base_url) = Url::parse(base)
                        .or_else(|_| Url::from_file_path(base).map_err(|_| url::ParseError::RelativeUrlWithoutBase))
                    {
                        if let Ok(resolved) = base_url.join(&import.href) {
                            resolved_href = resolved.to_string();
                        }
                    }
                }

                if seen.contains(&resolved_href) {
                    continue;
                }

                match loader.load(&resolved_href) {
                    Ok(css_text) => {
                        seen.insert(resolved_href.clone());
                        if let Ok(sheet) = crate::css::parser::parse_stylesheet(&css_text) {
                            resolve_rules(&sheet.rules, loader, Some(&resolved_href), media_ctx, seen, out);
                        }
                    }
                    Err(_) => {
                        // Per spec, failed imports are ignored.
                    }
                }
            }
            CssRule::FontFace(_) => out.push(rule.clone()),
        }
    }
}

#[derive(Debug, Default, Clone)]
struct LayerNode {
    children: Vec<LayerChild>,
}

#[derive(Debug, Clone)]
struct LayerChild {
    name: String,
    order: u32,
    node: LayerNode,
}

#[derive(Debug, Default, Clone)]
struct LayerRegistry {
    root: LayerNode,
    next_anonymous: u32,
}

impl LayerRegistry {
    fn new() -> Self {
        Self::default()
    }

    fn ensure_path(&mut self, base: &[u32], name: &[String]) -> Vec<u32> {
        let mut path = base.to_vec();
        let mut node: &mut LayerNode = if let Some(existing) = self.get_node_mut(base) {
            existing
        } else {
            &mut self.root
        };
        for component in name {
            let (order, next) = node.ensure_child(component);
            path.push(order);
            node = next;
        }
        path
    }

    fn ensure_anonymous(&mut self, base: &[u32]) -> Vec<u32> {
        let name = format!("__anon{}", self.next_anonymous);
        self.next_anonymous += 1;
        self.ensure_path(base, &[name])
    }

    fn get_node_mut(&mut self, path: &[u32]) -> Option<&mut LayerNode> {
        fn descend<'a>(node: &'a mut LayerNode, path: &[u32]) -> Option<&'a mut LayerNode> {
            if path.is_empty() {
                return Some(node);
            }
            let (first, rest) = path.split_first()?;
            let pos = node.children.iter().position(|c| c.order == *first)?;
            let child = &mut node.children[pos].node;
            if rest.is_empty() {
                Some(child)
            } else {
                descend(child, rest)
            }
        }
        descend(&mut self.root, path)
    }
}

impl LayerNode {
    fn ensure_child(&mut self, name: &str) -> (u32, &mut LayerNode) {
        if let Some(pos) = self.children.iter().position(|c| c.name == name) {
            let order = self.children[pos].order;
            let node = &mut self.children[pos].node;
            return (order, node);
        }
        let order = self.children.len() as u32;
        self.children.push(LayerChild {
            name: name.to_string(),
            order,
            node: LayerNode::default(),
        });
        let len = self.children.len();
        let node = &mut self.children[len - 1].node;
        (order, node)
    }
}

/// CSS property values
#[derive(Debug, Clone)]
pub enum PropertyValue {
    Color(Rgba),
    Length(Length),
    Percentage(f32),
    Number(f32),
    Keyword(String),
    String(String),
    Url(String),
    Multiple(Vec<PropertyValue>),
    FontFamily(Vec<String>),
    BoxShadow(Vec<BoxShadow>),
    TextShadow(Vec<TextShadow>),
    Transform(Vec<Transform>),
    LinearGradient {
        angle: f32,
        stops: Vec<ColorStop>,
    },
    RadialGradient {
        shape: RadialGradientShape,
        size: RadialGradientSize,
        position: GradientPosition,
        stops: Vec<ColorStop>,
    },
    RepeatingLinearGradient {
        angle: f32,
        stops: Vec<ColorStop>,
    },
    RepeatingRadialGradient {
        shape: RadialGradientShape,
        size: RadialGradientSize,
        position: GradientPosition,
        stops: Vec<ColorStop>,
    },
    ConicGradient {
        from_angle: f32,
        position: GradientPosition,
        stops: Vec<ColorStop>,
    },
    RepeatingConicGradient {
        from_angle: f32,
        position: GradientPosition,
        stops: Vec<ColorStop>,
    },
    /// Raw custom property value (stored unparsed)
    Custom(String),
}

// ============================================================================
// Shadow and gradient types
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct BoxShadow {
    pub offset_x: Length,
    pub offset_y: Length,
    pub blur_radius: Length,
    pub spread_radius: Length,
    pub color: Rgba,
    pub inset: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextShadow {
    pub offset_x: Length,
    pub offset_y: Length,
    pub blur_radius: Length,
    /// None represents currentColor.
    pub color: Option<Rgba>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RadialGradientShape {
    Circle,
    Ellipse,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RadialGradientSize {
    ClosestSide,
    FarthestSide,
    ClosestCorner,
    FarthestCorner,
    Explicit { x: Length, y: Option<Length> },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GradientPositionComponent {
    pub alignment: f32,
    pub offset: Length,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GradientPosition {
    pub x: GradientPositionComponent,
    pub y: GradientPositionComponent,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ColorStop {
    pub color: Color,
    pub position: Option<f32>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Transform {
    Translate(Length, Length),
    TranslateX(Length),
    TranslateY(Length),
    Scale(f32, f32),
    ScaleX(f32),
    ScaleY(f32),
    Rotate(f32),
    SkewX(f32),
    SkewY(f32),
    Matrix(f32, f32, f32, f32, f32, f32),
}
