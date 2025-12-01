//! CSS type definitions
//!
//! Core types for representing CSS stylesheets, rules, and values.

use crate::style::media::MediaQuery;
use crate::style::{Length, Rgba};
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

/// Stylesheet containing CSS rules
#[derive(Debug, Clone)]
pub struct StyleSheet {
    /// All CSS rules in the stylesheet (style rules and @-rules)
    pub rules: Vec<CssRule>,
}

impl StyleSheet {
    /// Creates an empty stylesheet
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    /// Collects all applicable style rules, evaluating @media queries
    ///
    /// This flattens nested @media rules and filters by the given media context.
    pub fn collect_style_rules(&self, media_ctx: &crate::style::media::MediaContext) -> Vec<&StyleRule> {
        let mut result = Vec::new();
        collect_rules_recursive(&self.rules, media_ctx, &mut result);
        result
    }
}

/// Helper to recursively collect style rules from nested @media blocks
fn collect_rules_recursive<'a>(
    rules: &'a [CssRule],
    media_ctx: &crate::style::media::MediaContext,
    out: &mut Vec<&'a StyleRule>,
) {
    for rule in rules {
        match rule {
            CssRule::Style(style_rule) => {
                out.push(style_rule);
            }
            CssRule::Media(media_rule) => {
                // Only include rules from @media blocks that match
                if media_ctx.evaluate(&media_rule.query) {
                    collect_rules_recursive(&media_rule.rules, media_ctx, out);
                }
            }
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
}

/// A @media rule containing conditional rules
#[derive(Debug, Clone)]
pub struct MediaRule {
    /// The media query to evaluate
    pub query: MediaQuery,
    /// Rules that apply when query matches (can be nested)
    pub rules: Vec<CssRule>,
}

/// A single CSS style rule (selectors + declarations)
#[derive(Debug, Clone)]
pub struct StyleRule {
    pub selectors: SelectorList<FastRenderSelectorImpl>,
    pub declarations: Vec<Declaration>,
}

/// A CSS property declaration
#[derive(Debug, Clone)]
pub struct Declaration {
    pub property: String,
    pub value: PropertyValue,
    pub important: bool,
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
    LinearGradient { angle: f32, stops: Vec<ColorStop> },
    RadialGradient { stops: Vec<ColorStop> },
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
    pub color: Rgba,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ColorStop {
    pub color: Rgba,
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
