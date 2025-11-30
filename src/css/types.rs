//! CSS type definitions
//!
//! Core types for representing CSS stylesheets, rules, and values.

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
    pub rules: Vec<StyleRule>,
}

/// A single CSS rule (selectors + declarations)
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
