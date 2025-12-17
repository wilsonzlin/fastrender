# Phase 5: Media Queries Support

**Duration:** 5-6 days
**Prerequisites:**
- Phase 1 Style Engine complete
- CSS Parser complete
**Dependencies:**
- CSS parser (nested rules)
- Cascade system
- Viewport dimensions
**Output:** Full @media query support for responsive design

## Objectives

Implement CSS Media Queries Level 4 to enable responsive web design:

1. **@media rule parsing** - Parse @media blocks in stylesheets
2. **Media types** - screen, print, all, etc.
3. **Media features** - width, height, orientation, resolution, etc.
4. **Media query evaluation** - Determine which rules apply
5. **Viewport meta tag** - HTML viewport configuration
6. **Responsive layout** - Apply different styles based on viewport

## Context

Media queries are the foundation of responsive web design. They allow stylesheets to adapt to different devices, screen sizes, and capabilities.

**From CSS Media Queries Level 4:**
> "A media query is a method of testing certain aspects of the user agent or device that the document is being displayed in. Media queries are used to conditionally apply styles in CSS with the @media rule."

**Common uses:**
```css
/* Mobile-first design */
.container {
  width: 100%;
}

@media (min-width: 768px) {
  .container {
    width: 750px;
  }
}

@media (min-width: 1024px) {
  .container {
    width: 970px;
  }
}

@media (min-width: 1200px) {
  .container {
    width: 1170px;
  }
}

/* Orientation */
@media (orientation: portrait) {
  .sidebar {
    display: none;
  }
}

/* High-DPI displays */
@media (min-resolution: 2dppx) {
  .logo {
    background-image: url('logo@2x.png');
  }
}

/* Print styles */
@media print {
  .no-print {
    display: none;
  }
}
```

## The Problem V1 Has

V1 has no media query support:
- No @media parsing
- No responsive layouts
- No viewport meta tag handling
- Fixed to a single viewport size

This makes it impossible to render modern responsive websites correctly.

## The Solution

Implement media queries in three layers:

1. **Parser layer:** Parse @media rules and media queries
2. **Evaluation layer:** Evaluate media queries against current context
3. **Cascade layer:** Apply conditional styles based on matching queries

```
@media (min-width: 768px) {
  .container { width: 750px; }
}
    ↓ (Parse)
MediaRule {
  query: MediaQuery {
    media_type: All,
    features: [MinWidth(768px)]
  },
  rules: [...]
}
    ↓ (Evaluate against viewport: 1024px)
Query matches! → Apply rules
    ↓ (Cascade)
.container gets width: 750px
```

## CSS Media Queries Specification

### Media Query Syntax

```
@media <media-query-list> {
  <rules>
}

media-query-list = <media-query> [ ',' <media-query> ]*

media-query = [ <media-type> | <media-condition> ]
            | [ <media-type> 'and' <media-condition> ]

media-condition = <media-feature>
                | <media-not>
                | <media-and>
                | <media-or>

media-feature = '(' <feature-name> [ ':' <feature-value> ] ')'
```

**Examples:**
```css
@media screen { ... }
@media (min-width: 768px) { ... }
@media screen and (min-width: 768px) { ... }
@media (min-width: 768px) and (max-width: 1024px) { ... }
@media (width >= 768px) { ... }  /* Level 4 range syntax */
@media not screen and (color) { ... }
@media print, screen and (min-width: 1024px) { ... }
```

### Media Types

- **all:** Matches all devices (default)
- **screen:** Computer screens, tablets, phones
- **print:** Print preview and printed pages
- **speech:** Screen readers (rarely used)

Deprecated: tv, tty, projection, handheld, braille, embossed, aural

### Media Features

#### Viewport Features (Most Common)

| Feature | Type | Description | Example |
|---------|------|-------------|---------|
| `width` | length | Viewport width | `(width: 768px)` |
| `min-width` | length | Minimum width | `(min-width: 768px)` |
| `max-width` | length | Maximum width | `(max-width: 1024px)` |
| `height` | length | Viewport height | `(height: 600px)` |
| `min-height` | length | Minimum height | `(min-height: 400px)` |
| `max-height` | length | Maximum height | `(max-height: 900px)` |
| `aspect-ratio` | ratio | Width/height ratio | `(aspect-ratio: 16/9)` |
| `orientation` | keyword | Portrait or landscape | `(orientation: portrait)` |

#### Display Features

| Feature | Type | Description | Example |
|---------|------|-------------|---------|
| `resolution` | resolution | Pixel density | `(min-resolution: 2dppx)` |
| `color` | integer | Bits per color | `(color)` or `(min-color: 8)` |
| `color-index` | integer | Colors in palette | `(min-color-index: 256)` |
| `monochrome` | integer | Monochrome bits | `(monochrome)` |

#### Interaction Features (Level 4)

| Feature | Type | Description | Example |
|---------|------|-------------|---------|
| `pointer` | keyword | Pointing accuracy | `(pointer: coarse)` |
| `hover` | keyword | Can hover | `(hover: hover)` |
| `any-pointer` | keyword | Any input pointer | `(any-pointer: fine)` |
| `any-hover` | keyword | Any input hover | `(any-hover: hover)` |

#### User Preference Features (Level 5)

| Feature | Type | Description | Example |
|---------|------|-------------|---------|
| `prefers-color-scheme` | keyword | Dark/light theme | `(prefers-color-scheme: dark)` |
| `prefers-reduced-motion` | keyword | Reduce animations | `(prefers-reduced-motion: reduce)` |
| `prefers-contrast` | keyword | Contrast preference | `(prefers-contrast: high)` |

### Media Query Logic

```css
/* AND: both conditions must be true */
@media screen and (min-width: 768px) { ... }

/* OR: any condition can be true (comma-separated) */
@media screen, print { ... }
@media (min-width: 768px), (orientation: portrait) { ... }

/* NOT: negates the query */
@media not screen { ... }
@media not (min-width: 768px) { ... }

/* ONLY: hides from old browsers */
@media only screen and (color) { ... }
```

### Range Syntax (Level 4)

Modern syntax for ranges:

```css
/* Old syntax */
@media (min-width: 768px) and (max-width: 1024px) { ... }

/* New syntax (Level 4) */
@media (768px <= width <= 1024px) { ... }
@media (width >= 768px) { ... }
@media (400px < height < 1000px) { ... }
```

## Step-by-Step Implementation

### Step 1: Extend CSS Parser for @media (Day 1)

**File: `src/css/parser.rs`** (additions)

```rust
/// Media rule
#[derive(Debug, Clone)]
pub struct MediaRule {
    /// Media query list
    pub queries: Vec<MediaQuery>,

    /// Rules that apply when query matches
    pub rules: Vec<Rule>,
}

/// A single media query
#[derive(Debug, Clone)]
pub struct MediaQuery {
    /// Optional media type (screen, print, all, etc.)
    pub media_type: Option<MediaType>,

    /// Logic modifier (not, only)
    pub modifier: Option<MediaModifier>,

    /// Media features (width, height, etc.)
    pub features: Vec<MediaFeature>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MediaType {
    All,
    Screen,
    Print,
    Speech,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MediaModifier {
    Not,
    Only,
}

/// A media feature test
#[derive(Debug, Clone, PartialEq)]
pub enum MediaFeature {
    /// (width: 768px)
    Width(Length),

    /// (min-width: 768px)
    MinWidth(Length),

    /// (max-width: 1024px)
    MaxWidth(Length),

    /// (height: 600px)
    Height(Length),

    /// (min-height: 400px)
    MinHeight(Length),

    /// (max-height: 900px)
    MaxHeight(Length),

    /// (orientation: portrait)
    Orientation(Orientation),

    /// (aspect-ratio: 16/9)
    AspectRatio { width: u32, height: u32 },

    /// (min-aspect-ratio: 16/9)
    MinAspectRatio { width: u32, height: u32 },

    /// (max-aspect-ratio: 16/9)
    MaxAspectRatio { width: u32, height: u32 },

    /// (resolution: 2dppx)
    Resolution(Resolution),

    /// (min-resolution: 2dppx)
    MinResolution(Resolution),

    /// (max-resolution: 3dppx)
    MaxResolution(Resolution),

    /// (color)
    Color,

    /// (min-color: 8)
    MinColor(u32),

    /// (monochrome)
    Monochrome,

    /// (hover: hover)
    Hover(HoverCapability),

    /// (pointer: coarse)
    Pointer(PointerCapability),

    /// (prefers-color-scheme: dark)
    PrefersColorScheme(ColorScheme),

    /// (prefers-reduced-motion: reduce)
    PrefersReducedMotion(ReducedMotion),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Orientation {
    Portrait,
    Landscape,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Resolution {
    pub value: f32,
    pub unit: ResolutionUnit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolutionUnit {
    Dpi,   // dots per inch
    Dpcm,  // dots per cm
    Dppx,  // dots per pixel (same as 'x')
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HoverCapability {
    None,
    Hover,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PointerCapability {
    None,
    Coarse,
    Fine,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorScheme {
    Light,
    Dark,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReducedMotion {
    NoPreference,
    Reduce,
}

impl Parser {
    /// Parses an @media rule
    ///
    /// Syntax: @media <media-query-list> { <rules> }
    fn parse_media_rule(&mut self) -> Result<MediaRule> {
        // Expect @media
        self.expect_keyword("@media")?;
        self.skip_whitespace();

        // Parse media query list
        let queries = self.parse_media_query_list()?;

        // Expect {
        self.skip_whitespace();
        self.expect('{')?;

        // Parse rules
        let mut rules = Vec::new();
        loop {
            self.skip_whitespace();

            if self.peek() == Some('}') {
                self.consume('}');
                break;
            }

            if self.is_eof() {
                return Err(Error::Parse("Unterminated @media block".into()));
            }

            // Parse nested rule
            let rule = self.parse_rule()?;
            rules.push(rule);
        }

        Ok(MediaRule { queries, rules })
    }

    /// Parses a comma-separated list of media queries
    fn parse_media_query_list(&mut self) -> Result<Vec<MediaQuery>> {
        let mut queries = Vec::new();

        loop {
            self.skip_whitespace();
            let query = self.parse_media_query()?;
            queries.push(query);

            self.skip_whitespace();
            if self.peek() == Some(',') {
                self.consume(',');
            } else {
                break;
            }
        }

        Ok(queries)
    }

    /// Parses a single media query
    ///
    /// Syntax:
    ///   [not|only] <media-type> [and <media-feature>]*
    ///   <media-feature> [and <media-feature>]*
    fn parse_media_query(&mut self) -> Result<MediaQuery> {
        self.skip_whitespace();

        let mut modifier = None;
        let mut media_type = None;
        let mut features = Vec::new();

        // Check for 'not' or 'only'
        if let Some(ident) = self.peek_ident() {
            match ident.as_str() {
                "not" => {
                    self.parse_ident()?;
                    modifier = Some(MediaModifier::Not);
                    self.skip_whitespace();
                }
                "only" => {
                    self.parse_ident()?;
                    modifier = Some(MediaModifier::Only);
                    self.skip_whitespace();
                }
                _ => {}
            }
        }

        // Check for media type
        if let Some(ident) = self.peek_ident() {
            if let Some(mt) = MediaType::from_str(&ident) {
                self.parse_ident()?;
                media_type = Some(mt);
                self.skip_whitespace();
            }
        }

        // Parse media features (with 'and')
        loop {
            // Check for 'and'
            if media_type.is_some() || !features.is_empty() {
                if let Some(ident) = self.peek_ident() {
                    if ident == "and" {
                        self.parse_ident()?;
                        self.skip_whitespace();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            // Parse feature: (name: value) or (name)
            if self.peek() == Some('(') {
                let feature = self.parse_media_feature()?;
                features.push(feature);
                self.skip_whitespace();
            } else {
                break;
            }
        }

        // At least one of media_type or features must be present
        if media_type.is_none() && features.is_empty() {
            return Err(Error::Parse("Empty media query".into()));
        }

        Ok(MediaQuery {
            media_type,
            modifier,
            features,
        })
    }

    /// Parses a media feature: (name: value) or (name)
    fn parse_media_feature(&mut self) -> Result<MediaFeature> {
        self.expect('(')?;
        self.skip_whitespace();

        let name = self.parse_ident()?;
        self.skip_whitespace();

        // Check if there's a value
        let feature = if self.peek() == Some(':') {
            self.consume(':');
            self.skip_whitespace();

            // Parse value based on feature name
            match name.as_str() {
                "width" => {
                    let length = self.parse_length()?;
                    MediaFeature::Width(length)
                }
                "min-width" => {
                    let length = self.parse_length()?;
                    MediaFeature::MinWidth(length)
                }
                "max-width" => {
                    let length = self.parse_length()?;
                    MediaFeature::MaxWidth(length)
                }
                "height" => {
                    let length = self.parse_length()?;
                    MediaFeature::Height(length)
                }
                "min-height" => {
                    let length = self.parse_length()?;
                    MediaFeature::MinHeight(length)
                }
                "max-height" => {
                    let length = self.parse_length()?;
                    MediaFeature::MaxHeight(length)
                }
                "orientation" => {
                    let value = self.parse_ident()?;
                    let orientation = match value.as_str() {
                        "portrait" => Orientation::Portrait,
                        "landscape" => Orientation::Landscape,
                        _ => return Err(Error::Parse(format!("Invalid orientation: {}", value))),
                    };
                    MediaFeature::Orientation(orientation)
                }
                "aspect-ratio" => {
                    let ratio = self.parse_ratio()?;
                    MediaFeature::AspectRatio {
                        width: ratio.0,
                        height: ratio.1,
                    }
                }
                "resolution" => {
                    let res = self.parse_resolution()?;
                    MediaFeature::Resolution(res)
                }
                "min-resolution" => {
                    let res = self.parse_resolution()?;
                    MediaFeature::MinResolution(res)
                }
                "max-resolution" => {
                    let res = self.parse_resolution()?;
                    MediaFeature::MaxResolution(res)
                }
                "prefers-color-scheme" => {
                    let value = self.parse_ident()?;
                    let scheme = match value.as_str() {
                        "light" => ColorScheme::Light,
                        "dark" => ColorScheme::Dark,
                        _ => return Err(Error::Parse(format!("Invalid color scheme: {}", value))),
                    };
                    MediaFeature::PrefersColorScheme(scheme)
                }
                _ => {
                    return Err(Error::Parse(format!("Unknown media feature: {}", name)));
                }
            }
        } else {
            // Boolean feature (no value)
            match name.as_str() {
                "color" => MediaFeature::Color,
                "monochrome" => MediaFeature::Monochrome,
                _ => {
                    return Err(Error::Parse(format!("Unknown boolean media feature: {}", name)));
                }
            }
        };

        self.skip_whitespace();
        self.expect(')')?;

        Ok(feature)
    }

    fn parse_ratio(&mut self) -> Result<(u32, u32)> {
        let width = self.parse_number()? as u32;
        self.skip_whitespace();
        self.expect('/')?;
        self.skip_whitespace();
        let height = self.parse_number()? as u32;
        Ok((width, height))
    }

    fn parse_resolution(&mut self) -> Result<Resolution> {
        let value = self.parse_number()?;
        let unit_str = self.parse_ident()?;

        let unit = match unit_str.as_str() {
            "dpi" => ResolutionUnit::Dpi,
            "dpcm" => ResolutionUnit::Dpcm,
            "dppx" | "x" => ResolutionUnit::Dppx,
            _ => return Err(Error::Parse(format!("Invalid resolution unit: {}", unit_str))),
        };

        Ok(Resolution { value, unit })
    }
}

impl MediaType {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "all" => Some(Self::All),
            "screen" => Some(Self::Screen),
            "print" => Some(Self::Print),
            "speech" => Some(Self::Speech),
            _ => None,
        }
    }
}
```

### Step 2: Media Query Evaluation (Day 2)

**File: `src/css/media_query.rs`**

```rust
//! Media query evaluation
//!
//! Determines if a media query matches the current context.

use super::parser::{MediaQuery, MediaFeature, MediaType, MediaModifier, Orientation};

/// Context for evaluating media queries
#[derive(Debug, Clone)]
pub struct MediaContext {
    /// Viewport width in pixels
    pub viewport_width: f32,

    /// Viewport height in pixels
    pub viewport_height: f32,

    /// Device pixel ratio (DPR)
    pub device_pixel_ratio: f32,

    /// Media type (screen, print, etc.)
    pub media_type: MediaType,

    /// Color depth (bits per color)
    pub color_depth: u32,

    /// Whether device can hover
    pub can_hover: bool,

    /// Pointer accuracy
    pub pointer_accuracy: PointerCapability,

    /// User preferences
    pub prefers_color_scheme: Option<ColorScheme>,
    pub prefers_reduced_motion: bool,
}

impl MediaContext {
    /// Creates a default screen context
    pub fn screen(width: f32, height: f32) -> Self {
        Self {
            viewport_width: width,
            viewport_height: height,
            device_pixel_ratio: 1.0,
            media_type: MediaType::Screen,
            color_depth: 8,
            can_hover: true,
            pointer_accuracy: PointerCapability::Fine,
            prefers_color_scheme: Some(ColorScheme::NoPreference),
            prefers_reduced_motion: false,
        }
    }

    /// Creates a print context
    pub fn print(width: f32, height: f32) -> Self {
        Self {
            viewport_width: width,
            viewport_height: height,
            device_pixel_ratio: 1.0,
            media_type: MediaType::Print,
            color_depth: 0, // Assume black & white
            can_hover: false,
            pointer_accuracy: PointerCapability::None,
            prefers_color_scheme: Some(ColorScheme::NoPreference),
            prefers_reduced_motion: false,
        }
    }

    /// Evaluates a media query list
    ///
    /// A query list matches if ANY query matches (OR logic).
    pub fn evaluate_query_list(&self, queries: &[MediaQuery]) -> bool {
        queries.iter().any(|q| self.evaluate_query(q))
    }

    /// Evaluates a single media query
    pub fn evaluate_query(&self, query: &MediaQuery) -> bool {
        // Check media type
        if let Some(media_type) = query.media_type {
            if !self.matches_media_type(media_type) {
                // Type doesn't match
                let result = false;

                // Apply modifier
                return match query.modifier {
                    Some(MediaModifier::Not) => !result,
                    _ => result,
                };
            }
        }

        // Evaluate all features (AND logic)
        let features_match = query.features.iter().all(|f| self.evaluate_feature(f));

        // Apply modifier
        match query.modifier {
            Some(MediaModifier::Not) => !features_match,
            Some(MediaModifier::Only) => features_match, // 'only' just hides from old browsers
            None => features_match,
        }
    }

    fn matches_media_type(&self, media_type: MediaType) -> bool {
        media_type == MediaType::All || media_type == self.media_type
    }

    /// Evaluates a single media feature
    fn evaluate_feature(&self, feature: &MediaFeature) -> bool {
        match feature {
            MediaFeature::Width(length) => {
                let width_px = self.resolve_length(length);
                (self.viewport_width - width_px).abs() < 1.0
            }

            MediaFeature::MinWidth(length) => {
                let width_px = self.resolve_length(length);
                self.viewport_width >= width_px
            }

            MediaFeature::MaxWidth(length) => {
                let width_px = self.resolve_length(length);
                self.viewport_width <= width_px
            }

            MediaFeature::Height(length) => {
                let height_px = self.resolve_length(length);
                (self.viewport_height - height_px).abs() < 1.0
            }

            MediaFeature::MinHeight(length) => {
                let height_px = self.resolve_length(length);
                self.viewport_height >= height_px
            }

            MediaFeature::MaxHeight(length) => {
                let height_px = self.resolve_length(length);
                self.viewport_height <= height_px
            }

            MediaFeature::Orientation(orientation) => {
                let is_portrait = self.viewport_height > self.viewport_width;
                match orientation {
                    Orientation::Portrait => is_portrait,
                    Orientation::Landscape => !is_portrait,
                }
            }

            MediaFeature::AspectRatio { width, height } => {
                let target_ratio = *width as f32 / *height as f32;
                let actual_ratio = self.viewport_width / self.viewport_height;
                (target_ratio - actual_ratio).abs() < 0.01
            }

            MediaFeature::MinAspectRatio { width, height } => {
                let target_ratio = *width as f32 / *height as f32;
                let actual_ratio = self.viewport_width / self.viewport_height;
                actual_ratio >= target_ratio
            }

            MediaFeature::MaxAspectRatio { width, height } => {
                let target_ratio = *width as f32 / *height as f32;
                let actual_ratio = self.viewport_width / self.viewport_height;
                actual_ratio <= target_ratio
            }

            MediaFeature::Resolution(res) => {
                let target_dppx = self.resolution_to_dppx(res);
                (self.device_pixel_ratio - target_dppx).abs() < 0.1
            }

            MediaFeature::MinResolution(res) => {
                let target_dppx = self.resolution_to_dppx(res);
                self.device_pixel_ratio >= target_dppx
            }

            MediaFeature::MaxResolution(res) => {
                let target_dppx = self.resolution_to_dppx(res);
                self.device_pixel_ratio <= target_dppx
            }

            MediaFeature::Color => {
                self.color_depth > 0
            }

            MediaFeature::MinColor(bits) => {
                self.color_depth >= *bits
            }

            MediaFeature::Monochrome => {
                self.color_depth == 0
            }

            MediaFeature::Hover(capability) => {
                match capability {
                    HoverCapability::None => !self.can_hover,
                    HoverCapability::Hover => self.can_hover,
                }
            }

            MediaFeature::Pointer(capability) => {
                self.pointer_accuracy == *capability
            }

            MediaFeature::PrefersColorScheme(scheme) => match self.prefers_color_scheme {
                Some(current) => current == *scheme,
                None => matches!(scheme, ColorScheme::NoPreference),
            },

            MediaFeature::PrefersReducedMotion(motion) => {
                match motion {
                    ReducedMotion::NoPreference => !self.prefers_reduced_motion,
                    ReducedMotion::Reduce => self.prefers_reduced_motion,
                }
            }
        }
    }

    fn resolve_length(&self, length: &Length) -> f32 {
        match length.unit {
            LengthUnit::Px => length.value,
            LengthUnit::Em => length.value * 16.0, // Assume 16px base
            LengthUnit::Rem => length.value * 16.0,
            // TODO: Other units
            _ => length.value,
        }
    }

    fn resolution_to_dppx(&self, res: &Resolution) -> f32 {
        match res.unit {
            ResolutionUnit::Dppx => res.value,
            ResolutionUnit::Dpi => res.value / 96.0, // 96 DPI = 1 dppx
            ResolutionUnit::Dpcm => res.value / 37.8, // ~37.8 dpcm = 1 dppx
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_min_width() {
        let ctx = MediaContext::screen(1024.0, 768.0);

        let query = MediaQuery {
            media_type: None,
            modifier: None,
            features: vec![MediaFeature::MinWidth(Length::px(768.0))],
        };

        assert!(ctx.evaluate_query(&query)); // 1024 >= 768

        let query2 = MediaQuery {
            media_type: None,
            modifier: None,
            features: vec![MediaFeature::MinWidth(Length::px(1200.0))],
        };

        assert!(!ctx.evaluate_query(&query2)); // 1024 < 1200
    }

    #[test]
    fn test_orientation() {
        let portrait_ctx = MediaContext::screen(375.0, 667.0); // iPhone

        let query = MediaQuery {
            media_type: None,
            modifier: None,
            features: vec![MediaFeature::Orientation(Orientation::Portrait)],
        };

        assert!(portrait_ctx.evaluate_query(&query));

        let landscape_ctx = MediaContext::screen(667.0, 375.0);
        assert!(!landscape_ctx.evaluate_query(&query));
    }

    #[test]
    fn test_media_type() {
        let screen_ctx = MediaContext::screen(1024.0, 768.0);

        let screen_query = MediaQuery {
            media_type: Some(MediaType::Screen),
            modifier: None,
            features: vec![],
        };

        assert!(screen_ctx.evaluate_query(&screen_query));

        let print_query = MediaQuery {
            media_type: Some(MediaType::Print),
            modifier: None,
            features: vec![],
        };

        assert!(!screen_ctx.evaluate_query(&print_query));
    }

    #[test]
    fn test_not_modifier() {
        let ctx = MediaContext::screen(1024.0, 768.0);

        let query = MediaQuery {
            media_type: Some(MediaType::Screen),
            modifier: Some(MediaModifier::Not),
            features: vec![],
        };

        assert!(!ctx.evaluate_query(&query)); // NOT screen = false
    }
}
```

### Step 3: Integrate with Style Engine (Day 3)

**File: `src/style/engine.rs`** (modifications)

```rust
use crate::css::media_query::MediaContext;

impl StyleEngine {
    /// Computes styles for a DOM tree with media context
    pub fn compute_styles_with_media(
        &self,
        dom: &DomNode,
        stylesheet: &Stylesheet,
        media_context: &MediaContext,
    ) -> Result<()> {
        // Traverse DOM tree
        self.compute_styles_recursive(dom, stylesheet, media_context)
    }

    fn compute_styles_recursive(
        &self,
        node: &DomNode,
        stylesheet: &Stylesheet,
        media_context: &MediaContext,
    ) -> Result<()> {
        // Collect matching rules, including conditional rules from @media
        let mut matching_rules = Vec::new();

        for rule in &stylesheet.rules {
            match rule {
                Rule::Style(style_rule) => {
                    // Check if selector matches
                    if self.selector_matches(node, &style_rule.selector) {
                        matching_rules.push(&style_rule.declarations);
                    }
                }

                Rule::Media(media_rule) => {
                    // Evaluate media query
                    if media_context.evaluate_query_list(&media_rule.queries) {
                        // Media query matches, process nested rules
                        for nested_rule in &media_rule.rules {
                            if let Rule::Style(style_rule) = nested_rule {
                                if self.selector_matches(node, &style_rule.selector) {
                                    matching_rules.push(&style_rule.declarations);
                                }
                            }
                        }
                    }
                }

                _ => {} // Other rule types
            }
        }

        // Apply cascade
        let computed_style = self.cascade(matching_rules, node.parent_style());

        // Store computed style on node
        node.set_computed_style(computed_style);

        // Recurse to children
        for child in node.children() {
            self.compute_styles_recursive(child, stylesheet, media_context)?;
        }

        Ok(())
    }
}
```

### Step 4: Viewport Meta Tag Support (Day 4)

**File: `src/html/viewport.rs`**

```rust
//! Viewport meta tag parsing
//!
//! Handles: <meta name="viewport" content="width=device-width, initial-scale=1">

#[derive(Debug, Clone)]
pub struct ViewportMeta {
    /// Viewport width (px or "device-width")
    pub width: ViewportDimension,

    /// Viewport height (px or "device-height")
    pub height: ViewportDimension,

    /// Initial zoom level
    pub initial_scale: f32,

    /// Minimum zoom level
    pub minimum_scale: f32,

    /// Maximum zoom level
    pub maximum_scale: f32,

    /// Allow user to zoom
    pub user_scalable: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ViewportDimension {
    /// Device width/height
    DeviceSize,

    /// Fixed pixel value
    Pixels(f32),

    /// Not specified
    Auto,
}

impl ViewportMeta {
    pub fn parse(content: &str) -> Result<Self> {
        let mut width = ViewportDimension::Auto;
        let mut height = ViewportDimension::Auto;
        let mut initial_scale = 1.0;
        let mut minimum_scale = 0.1;
        let mut maximum_scale = 10.0;
        let mut user_scalable = true;

        // Parse comma-separated properties
        for part in content.split(',') {
            let part = part.trim();

            if let Some((key, value)) = part.split_once('=') {
                let key = key.trim();
                let value = value.trim();

                match key {
                    "width" => {
                        width = if value == "device-width" {
                            ViewportDimension::DeviceSize
                        } else {
                            ViewportDimension::Pixels(value.parse()?)
                        };
                    }

                    "height" => {
                        height = if value == "device-height" {
                            ViewportDimension::DeviceSize
                        } else {
                            ViewportDimension::Pixels(value.parse()?)
                        };
                    }

                    "initial-scale" => {
                        initial_scale = value.parse()?;
                    }

                    "minimum-scale" => {
                        minimum_scale = value.parse()?;
                    }

                    "maximum-scale" => {
                        maximum_scale = value.parse()?;
                    }

                    "user-scalable" => {
                        user_scalable = value == "yes" || value == "1";
                    }

                    _ => {
                        // Unknown property, ignore
                    }
                }
            }
        }

        Ok(Self {
            width,
            height,
            initial_scale,
            minimum_scale,
            maximum_scale,
            user_scalable,
        })
    }

    /// Computes actual viewport dimensions
    pub fn compute_viewport_size(
        &self,
        device_width: f32,
        device_height: f32,
    ) -> (f32, f32) {
        let width = match self.width {
            ViewportDimension::DeviceSize => device_width,
            ViewportDimension::Pixels(px) => px,
            ViewportDimension::Auto => device_width,
        };

        let height = match self.height {
            ViewportDimension::DeviceSize => device_height,
            ViewportDimension::Pixels(px) => px,
            ViewportDimension::Auto => device_height,
        };

        // Apply initial scale
        let scaled_width = width / self.initial_scale;
        let scaled_height = height / self.initial_scale;

        (scaled_width, scaled_height)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_viewport_meta() {
        let content = "width=device-width, initial-scale=1";
        let viewport = ViewportMeta::parse(content).unwrap();

        assert_eq!(viewport.width, ViewportDimension::DeviceSize);
        assert_eq!(viewport.initial_scale, 1.0);
    }

    #[test]
    fn test_compute_viewport_size() {
        let viewport = ViewportMeta {
            width: ViewportDimension::DeviceSize,
            height: ViewportDimension::Auto,
            initial_scale: 1.0,
            minimum_scale: 0.1,
            maximum_scale: 10.0,
            user_scalable: true,
        };

        let (width, height) = viewport.compute_viewport_size(375.0, 667.0);
        assert_eq!(width, 375.0);
        assert_eq!(height, 667.0);
    }

    #[test]
    fn test_initial_scale() {
        let viewport = ViewportMeta {
            width: ViewportDimension::Pixels(750.0),
            height: ViewportDimension::Auto,
            initial_scale: 2.0, // Zoom in 2x
            minimum_scale: 0.1,
            maximum_scale: 10.0,
            user_scalable: true,
        };

        let (width, _) = viewport.compute_viewport_size(375.0, 667.0);
        assert_eq!(width, 375.0); // 750 / 2
    }
}
```

### Step 5: End-to-End Tests (Day 5-6)

**File: `tests/media_queries_test.rs`**

```rust
//! Tests for media queries

use fastrender::*;

#[test]
fn test_min_width_media_query() {
    let html = r#"<div class="container">Content</div>"#;
    let css = r#"
        .container {
            width: 100%;
            background: red;
        }

        @media (min-width: 768px) {
            .container {
                width: 750px;
                background: blue;
            }
        }
    "#;

    // Test at 375px width (mobile)
    let rendered_mobile = render_at_size(html, css, 375, 667);
    let container = rendered_mobile.find(".container");
    assert_eq!(container.width, 375.0); // 100% of viewport
    assert_eq!(container.background, Color::RED);

    // Test at 1024px width (desktop)
    let rendered_desktop = render_at_size(html, css, 1024, 768);
    let container = rendered_desktop.find(".container");
    assert_eq!(container.width, 750.0); // Fixed width
    assert_eq!(container.background, Color::BLUE);
}

#[test]
fn test_orientation_media_query() {
    let html = r#"<div class="sidebar">Sidebar</div>"#;
    let css = r#"
        .sidebar {
            display: block;
        }

        @media (orientation: portrait) {
            .sidebar {
                display: none;
            }
        }
    "#;

    // Portrait (mobile)
    let rendered = render_at_size(html, css, 375, 667);
    let sidebar = rendered.find(".sidebar");
    assert!(!sidebar.is_visible()); // display: none

    // Landscape (desktop)
    let rendered = render_at_size(html, css, 1024, 768);
    let sidebar = rendered.find(".sidebar");
    assert!(sidebar.is_visible()); // display: block
}

#[test]
fn test_multiple_breakpoints() {
    let html = r#"<div class="container">Content</div>"#;
    let css = r#"
        .container { width: 100%; }

        @media (min-width: 576px) {
            .container { width: 540px; }
        }

        @media (min-width: 768px) {
            .container { width: 720px; }
        }

        @media (min-width: 992px) {
            .container { width: 960px; }
        }

        @media (min-width: 1200px) {
            .container { width: 1140px; }
        }
    "#;

    // Test various widths
    assert_container_width(html, css, 375, 375.0);   // 100%
    assert_container_width(html, css, 576, 540.0);   // sm
    assert_container_width(html, css, 768, 720.0);   // md
    assert_container_width(html, css, 992, 960.0);   // lg
    assert_container_width(html, css, 1200, 1140.0); // xl
}

#[test]
fn test_print_media_query() {
    let html = r#"<div class="no-print">Don't print me</div>"#;
    let css = r#"
        @media print {
            .no-print {
                display: none;
            }
        }
    "#;

    // Screen context
    let rendered_screen = render_for_screen(html, css, 1024, 768);
    assert!(rendered_screen.find(".no-print").is_visible());

    // Print context
    let rendered_print = render_for_print(html, css, 8.5 * 96.0, 11.0 * 96.0);
    assert!(!rendered_print.find(".no-print").is_visible());
}

#[test]
fn test_high_dpi_media_query() {
    let html = r#"<div class="logo"></div>"#;
    let css = r#"
        .logo {
            background-image: url('logo.png');
        }

        @media (min-resolution: 2dppx) {
            .logo {
                background-image: url('logo@2x.png');
            }
        }
    "#;

    // 1x display
    let mut ctx = MediaContext::screen(1024.0, 768.0);
    ctx.device_pixel_ratio = 1.0;
    let rendered = render_with_context(html, css, &ctx);
    assert_eq!(rendered.find(".logo").background_image, "logo.png");

    // 2x display (Retina)
    ctx.device_pixel_ratio = 2.0;
    let rendered = render_with_context(html, css, &ctx);
    assert_eq!(rendered.find(".logo").background_image, "logo@2x.png");
}

#[test]
fn test_viewport_meta_tag() {
    let html = r#"
        <html>
        <head>
            <meta name="viewport" content="width=device-width, initial-scale=1">
        </head>
        <body>
            <div class="container">Content</div>
        </body>
        </html>
    "#;

    let css = r#"
        .container {
            width: 100%;
        }
    "#;

    // Device: 375x667 (iPhone)
    // Viewport meta makes viewport = device-width
    let rendered = render(html, css);
    assert_eq!(rendered.viewport_width, 375.0);
}
```

## Acceptance Criteria

- [ ] @media rules parse correctly
- [ ] Media types work (screen, print, all)
- [ ] width/height features work
- [ ] min-width/max-width features work
- [ ] orientation feature works
- [ ] aspect-ratio feature works
- [ ] resolution feature works (dpi, dppx)
- [ ] Media query logic works (and, or, not)
- [ ] Viewport meta tag parses
- [ ] Viewport dimensions computed correctly
- [ ] Responsive layouts render at different sizes
- [ ] All tests pass: `cargo test media_queries`
- [ ] Real-world responsive sites render correctly
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Not Re-evaluating on Resize

**Wrong:**
```rust
// Evaluate once and cache forever
let matches = evaluate_query(&query);
```

**Right:**
```rust
// Re-evaluate when viewport changes
fn on_viewport_change(&mut self, new_width: f32, new_height: f32) {
    self.media_context.viewport_width = new_width;
    self.media_context.viewport_height = new_height;
    self.recompute_styles(); // Re-evaluate media queries
}
```

### Pitfall 2: Incorrect OR Logic

**Wrong:**
```rust
// ALL queries must match (wrong!)
queries.iter().all(|q| evaluate(q))
```

**Right:**
```rust
// ANY query can match (OR logic)
queries.iter().any(|q| evaluate(q))
```

### Pitfall 3: Forgetting to Apply Cascade Order

**Wrong:**
```rust
// Apply media query rules first, then normal rules
apply_media_rules();
apply_normal_rules(); // Overwrites media rules!
```

**Right:**
```rust
// Collect ALL matching rules (normal + conditional)
// Then apply cascade based on specificity and order
let all_rules = collect_matching_rules_including_media();
apply_cascade(all_rules);
```

## Performance Considerations

- **Cache media query evaluation:** Don't re-evaluate on every style lookup
- **Invalidate on viewport change:** Only re-compute when dimensions change
- **Lazy evaluation:** Only evaluate queries for loaded stylesheets

## Next Steps

- Test with real responsive frameworks (Bootstrap, Tailwind)
- Add support for more media features
- Implement container queries (future)

## References

- **CSS Media Queries Level 4:**
  - https://www.w3.org/TR/mediaqueries-4/
- **CSS Media Queries Level 5:**
  - https://www.w3.org/TR/mediaqueries-5/
- **MDN Media Queries:**
  - https://developer.mozilla.org/en-US/docs/Web/CSS/Media_Queries
- **Viewport meta tag:**
  - https://developer.mozilla.org/en-US/docs/Web/HTML/Viewport_meta_tag

---

**Last Updated:** 2025-01-19
**Status:** Ready for Implementation
