//! Color types for CSS colors
//!
//! This module provides types for representing and manipulating colors.
//! It supports all CSS color formats: hex, rgb, rgba, hsl, hsla, and
//! named colors.
//!
//! # Color Spaces
//!
//! - **RGB**: Red, Green, Blue (0-255 each)
//! - **HSL**: Hue (0-360), Saturation (0-100%), Lightness (0-100%)
//! - **Alpha**: Opacity (0.0-1.0, where 0.0 is transparent and 1.0 is opaque)
//!
//! # Examples
//!
//! ```
//! use fastrender::style::Color;
//!
//! // Parse from hex
//! let color = Color::parse("#ff0000").unwrap();
//!
//! // Parse from rgb
//! let color = Color::parse("rgb(255, 0, 0)").unwrap();
//!
//! // Parse from named color
//! let color = Color::parse("red").unwrap();
//! ```

use std::fmt;

/// RGBA color representation
///
/// Represents a color in the RGB color space with an alpha channel.
/// - R, G, B: 0-255 (stored as u8)
/// - A: 0.0-1.0 (stored as f32, where 0.0 is fully transparent, 1.0 is fully opaque)
///
/// # Examples
///
/// ```
/// use fastrender::style::Rgba;
///
/// let red = Rgba::new(255, 0, 0, 1.0);
/// let semi_transparent_blue = Rgba::new(0, 0, 255, 0.5);
/// let transparent = Rgba::TRANSPARENT;
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Rgba {
    /// Red component (0-255)
    pub r: u8,
    /// Green component (0-255)
    pub g: u8,
    /// Blue component (0-255)
    pub b: u8,
    /// Alpha component (0.0-1.0)
    pub a: f32,
}

impl Rgba {
    /// Fully transparent black
    pub const TRANSPARENT: Self = Self {
        r: 0,
        g: 0,
        b: 0,
        a: 0.0,
    };

    /// Opaque black
    pub const BLACK: Self = Self {
        r: 0,
        g: 0,
        b: 0,
        a: 1.0,
    };

    /// Opaque white
    pub const WHITE: Self = Self {
        r: 255,
        g: 255,
        b: 255,
        a: 1.0,
    };

    /// Opaque red
    pub const RED: Self = Self {
        r: 255,
        g: 0,
        b: 0,
        a: 1.0,
    };

    /// Opaque green
    pub const GREEN: Self = Self {
        r: 0,
        g: 255,
        b: 0,
        a: 1.0,
    };

    /// Opaque blue
    pub const BLUE: Self = Self {
        r: 0,
        g: 0,
        b: 255,
        a: 1.0,
    };

    /// Creates a new RGBA color
    ///
    /// # Arguments
    /// * `r` - Red component (0-255)
    /// * `g` - Green component (0-255)
    /// * `b` - Blue component (0-255)
    /// * `a` - Alpha component (0.0-1.0)
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Rgba;
    ///
    /// let color = Rgba::new(255, 128, 0, 1.0); // Orange
    /// ```
    pub const fn new(r: u8, g: u8, b: u8, a: f32) -> Self {
        Self { r, g, b, a }
    }

    /// Creates an opaque RGB color (alpha = 1.0)
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Rgba;
    ///
    /// let purple = Rgba::rgb(128, 0, 128);
    /// assert_eq!(purple.a, 1.0);
    /// ```
    pub const fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b, a: 1.0 }
    }

    /// Returns true if the color is fully transparent
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Rgba;
    ///
    /// assert!(Rgba::TRANSPARENT.is_transparent());
    /// assert!(!Rgba::BLACK.is_transparent());
    /// ```
    pub fn is_transparent(self) -> bool {
        self.a == 0.0
    }

    /// Returns true if the color is fully opaque
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Rgba;
    ///
    /// assert!(Rgba::BLACK.is_opaque());
    /// assert!(!Rgba::new(0, 0, 0, 0.5).is_opaque());
    /// ```
    pub fn is_opaque(self) -> bool {
        self.a == 1.0
    }

    /// Returns a new color with the given alpha value
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Rgba;
    ///
    /// let red = Rgba::RED;
    /// let semi_transparent_red = red.with_alpha(0.5);
    /// assert_eq!(semi_transparent_red.a, 0.5);
    /// ```
    pub fn with_alpha(self, alpha: f32) -> Self {
        Self {
            r: self.r,
            g: self.g,
            b: self.b,
            a: alpha.clamp(0.0, 1.0),
        }
    }

    /// Converts to premultiplied alpha
    ///
    /// Useful for alpha compositing operations.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Rgba;
    ///
    /// let color = Rgba::new(255, 0, 0, 0.5);
    /// let premul = color.premultiplied();
    /// ```
    pub fn premultiplied(self) -> Self {
        Self {
            r: (self.r as f32 * self.a) as u8,
            g: (self.g as f32 * self.a) as u8,
            b: (self.b as f32 * self.a) as u8,
            a: self.a,
        }
    }

    /// Converts to an array [r, g, b, a] for rendering
    pub fn to_array(self) -> [u8; 4] {
        [self.r, self.g, self.b, (self.a * 255.0) as u8]
    }

    /// Converts RGB to HSL
    ///
    /// Uses the algorithm from CSS Color Module Level 3.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Rgba;
    ///
    /// let rgb = Rgba::RED;
    /// let hsl = rgb.to_hsla();
    /// assert_eq!(hsl.h, 0.0);
    /// assert_eq!(hsl.s, 100.0);
    /// assert_eq!(hsl.l, 50.0);
    /// ```
    pub fn to_hsla(self) -> Hsla {
        let r = self.r as f32 / 255.0;
        let g = self.g as f32 / 255.0;
        let b = self.b as f32 / 255.0;

        let max = r.max(g).max(b);
        let min = r.min(g).min(b);
        let delta = max - min;

        let l = (max + min) / 2.0;

        let (h, s) = if delta == 0.0 {
            // Achromatic
            (0.0, 0.0)
        } else {
            let s = if l < 0.5 {
                delta / (max + min)
            } else {
                delta / (2.0 - max - min)
            };

            let h = if max == r {
                ((g - b) / delta + if g < b { 6.0 } else { 0.0 }) / 6.0
            } else if max == g {
                ((b - r) / delta + 2.0) / 6.0
            } else {
                ((r - g) / delta + 4.0) / 6.0
            };

            (h * 360.0, s * 100.0)
        };

        Hsla::new(h, s, l * 100.0, self.a)
    }
}

impl fmt::Display for Rgba {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.a == 1.0 {
            write!(f, "rgb({}, {}, {})", self.r, self.g, self.b)
        } else {
            write!(f, "rgba({}, {}, {}, {:.3})", self.r, self.g, self.b, self.a)
        }
    }
}

/// HSLA color representation
///
/// Represents a color in the HSL color space with an alpha channel.
/// - H: Hue (0-360 degrees)
/// - S: Saturation (0-100%)
/// - L: Lightness (0-100%)
/// - A: Alpha (0.0-1.0)
///
/// # Examples
///
/// ```
/// use fastrender::style::Hsla;
///
/// let red = Hsla::new(0.0, 100.0, 50.0, 1.0);
/// let green = Hsla::new(120.0, 100.0, 50.0, 1.0);
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Hsla {
    /// Hue in degrees (0-360)
    pub h: f32,
    /// Saturation percentage (0-100)
    pub s: f32,
    /// Lightness percentage (0-100)
    pub l: f32,
    /// Alpha (0.0-1.0)
    pub a: f32,
}

impl Hsla {
    /// Creates a new HSLA color
    ///
    /// # Arguments
    /// * `h` - Hue (0-360 degrees)
    /// * `s` - Saturation (0-100%)
    /// * `l` - Lightness (0-100%)
    /// * `a` - Alpha (0.0-1.0)
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Hsla;
    ///
    /// let cyan = Hsla::new(180.0, 100.0, 50.0, 1.0);
    /// ```
    pub fn new(h: f32, s: f32, l: f32, a: f32) -> Self {
        Self {
            h: h % 360.0,
            s: s.clamp(0.0, 100.0),
            l: l.clamp(0.0, 100.0),
            a: a.clamp(0.0, 1.0),
        }
    }

    /// Creates an opaque HSL color (alpha = 1.0)
    pub fn hsl(h: f32, s: f32, l: f32) -> Self {
        Self::new(h, s, l, 1.0)
    }

    /// Converts HSL to RGB
    ///
    /// Uses the algorithm from CSS Color Module Level 3.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::{Hsla, Rgba};
    ///
    /// let hsl = Hsla::new(0.0, 100.0, 50.0, 1.0); // Red
    /// let rgb = hsl.to_rgba();
    /// assert_eq!(rgb, Rgba::RED);
    /// ```
    pub fn to_rgba(self) -> Rgba {
        let h = self.h / 360.0;
        let s = self.s / 100.0;
        let l = self.l / 100.0;

        let (r, g, b) = if s == 0.0 {
            // Achromatic (gray)
            (l, l, l)
        } else {
            let q = if l < 0.5 { l * (1.0 + s) } else { l + s - l * s };
            let p = 2.0 * l - q;

            (
                hue_to_rgb(p, q, h + 1.0 / 3.0),
                hue_to_rgb(p, q, h),
                hue_to_rgb(p, q, h - 1.0 / 3.0),
            )
        };

        Rgba::new(
            (r * 255.0).round() as u8,
            (g * 255.0).round() as u8,
            (b * 255.0).round() as u8,
            self.a,
        )
    }
}

/// Helper function for HSL to RGB conversion
fn hue_to_rgb(p: f32, q: f32, mut t: f32) -> f32 {
    if t < 0.0 {
        t += 1.0;
    }
    if t > 1.0 {
        t -= 1.0;
    }
    if t < 1.0 / 6.0 {
        return p + (q - p) * 6.0 * t;
    }
    if t < 1.0 / 2.0 {
        return q;
    }
    if t < 2.0 / 3.0 {
        return p + (q - p) * (2.0 / 3.0 - t) * 6.0;
    }
    p
}

impl fmt::Display for Hsla {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.a == 1.0 {
            write!(f, "hsl({:.1}, {:.1}%, {:.1}%)", self.h, self.s, self.l)
        } else {
            write!(f, "hsla({:.1}, {:.1}%, {:.1}%, {:.3})", self.h, self.s, self.l, self.a)
        }
    }
}

/// CSS Color value
///
/// Represents any valid CSS color, including special keywords.
///
/// # Examples
///
/// ```
/// use fastrender::style::{Color, Rgba};
///
/// let red = Color::Rgba(Rgba::RED);
/// let current = Color::CurrentColor;
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Color {
    /// RGBA color
    Rgba(Rgba),

    /// HSLA color
    Hsla(Hsla),

    /// Special keyword: currentColor
    /// Uses the current value of the 'color' property
    CurrentColor,
}

impl Color {
    /// Converts to RGBA, resolving CurrentColor to the given fallback
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::{Color, Rgba};
    ///
    /// let color = Color::Rgba(Rgba::RED);
    /// assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::RED);
    ///
    /// let current = Color::CurrentColor;
    /// assert_eq!(current.to_rgba(Rgba::BLUE), Rgba::BLUE);
    /// ```
    pub fn to_rgba(self, current_color: Rgba) -> Rgba {
        match self {
            Color::Rgba(rgba) => rgba,
            Color::Hsla(hsla) => hsla.to_rgba(),
            Color::CurrentColor => current_color,
        }
    }

    /// Returns true if this is CurrentColor
    pub fn is_current_color(self) -> bool {
        matches!(self, Color::CurrentColor)
    }

    /// Returns the transparent color
    pub const fn transparent() -> Self {
        Color::Rgba(Rgba::TRANSPARENT)
    }

    /// Returns opaque black
    pub const fn black() -> Self {
        Color::Rgba(Rgba::BLACK)
    }

    /// Returns opaque white
    pub const fn white() -> Self {
        Color::Rgba(Rgba::WHITE)
    }

    /// Parse a color from a CSS color string
    ///
    /// Supports:
    /// - Hex: #RGB, #RRGGBB, #RGBA, #RRGGBBAA
    /// - RGB: rgb(r, g, b), rgba(r, g, b, a)
    /// - HSL: hsl(h, s%, l%), hsla(h, s%, l%, a)
    /// - Named colors: red, blue, etc.
    /// - Special: transparent, currentColor
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::Color;
    ///
    /// assert!(Color::parse("#ff0000").is_ok());
    /// assert!(Color::parse("rgb(255, 0, 0)").is_ok());
    /// assert!(Color::parse("red").is_ok());
    /// assert!(Color::parse("transparent").is_ok());
    /// ```
    pub fn parse(s: &str) -> Result<Self, ColorParseError> {
        let s = s.trim();

        // Special keywords
        if s.eq_ignore_ascii_case("transparent") {
            return Ok(Color::transparent());
        }
        if s.eq_ignore_ascii_case("currentcolor") || s.eq_ignore_ascii_case("currentColor") {
            return Ok(Color::CurrentColor);
        }

        // Hex colors
        if s.starts_with('#') {
            return parse_hex(s);
        }

        // RGB/RGBA functions
        if s.starts_with("rgb(") || s.starts_with("rgba(") {
            return parse_rgb(s);
        }

        // HSL/HSLA functions
        if s.starts_with("hsl(") || s.starts_with("hsla(") {
            return parse_hsl(s);
        }

        // Named colors
        if let Some(rgba) = parse_named_color(s) {
            return Ok(Color::Rgba(rgba));
        }

        Err(ColorParseError::InvalidFormat(s.to_string()))
    }
}

impl From<Rgba> for Color {
    fn from(rgba: Rgba) -> Self {
        Color::Rgba(rgba)
    }
}

impl From<Hsla> for Color {
    fn from(hsla: Hsla) -> Self {
        Color::Hsla(hsla)
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Color::Rgba(rgba) => write!(f, "{}", rgba),
            Color::Hsla(hsla) => write!(f, "{}", hsla),
            Color::CurrentColor => write!(f, "currentColor"),
        }
    }
}

/// Parse error for color strings
#[derive(Debug, Clone, PartialEq)]
pub enum ColorParseError {
    InvalidFormat(String),
    InvalidHex(String),
    InvalidComponent(String),
    OutOfRange(String),
}

impl fmt::Display for ColorParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ColorParseError::InvalidFormat(s) => write!(f, "Invalid color format: {}", s),
            ColorParseError::InvalidHex(s) => write!(f, "Invalid hex color: {}", s),
            ColorParseError::InvalidComponent(s) => write!(f, "Invalid color component: {}", s),
            ColorParseError::OutOfRange(s) => write!(f, "Color component out of range: {}", s),
        }
    }
}

impl std::error::Error for ColorParseError {}

/// Parse hex color (#RGB, #RRGGBB, #RGBA, #RRGGBBAA)
fn parse_hex(s: &str) -> Result<Color, ColorParseError> {
    let hex = &s[1..]; // Skip '#'

    let (r, g, b, a) = match hex.len() {
        3 => {
            // #RGB -> #RRGGBB
            let r =
                u8::from_str_radix(&hex[0..1].repeat(2), 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            let g =
                u8::from_str_radix(&hex[1..2].repeat(2), 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            let b =
                u8::from_str_radix(&hex[2..3].repeat(2), 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            (r, g, b, 1.0)
        }
        4 => {
            // #RGBA -> #RRGGBBAA
            let r =
                u8::from_str_radix(&hex[0..1].repeat(2), 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            let g =
                u8::from_str_radix(&hex[1..2].repeat(2), 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            let b =
                u8::from_str_radix(&hex[2..3].repeat(2), 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            let a =
                u8::from_str_radix(&hex[3..4].repeat(2), 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            (r, g, b, a as f32 / 255.0)
        }
        6 => {
            // #RRGGBB
            let r = u8::from_str_radix(&hex[0..2], 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            let g = u8::from_str_radix(&hex[2..4], 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            let b = u8::from_str_radix(&hex[4..6], 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            (r, g, b, 1.0)
        }
        8 => {
            // #RRGGBBAA
            let r = u8::from_str_radix(&hex[0..2], 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            let g = u8::from_str_radix(&hex[2..4], 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            let b = u8::from_str_radix(&hex[4..6], 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            let a = u8::from_str_radix(&hex[6..8], 16).map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
            (r, g, b, a as f32 / 255.0)
        }
        _ => return Err(ColorParseError::InvalidHex(s.to_string())),
    };

    Ok(Color::Rgba(Rgba::new(r, g, b, a)))
}

/// Parse rgb() or rgba() function
fn parse_rgb(s: &str) -> Result<Color, ColorParseError> {
    let is_rgba = s.starts_with("rgba");
    let start = if is_rgba { 5 } else { 4 };

    let end = s
        .find(')')
        .ok_or_else(|| ColorParseError::InvalidFormat(s.to_string()))?;
    let inner = &s[start..end];

    let parts: Vec<&str> = inner.split(',').map(|s| s.trim()).collect();

    if parts.len() < 3 || (is_rgba && parts.len() < 4) {
        return Err(ColorParseError::InvalidFormat(s.to_string()));
    }

    let r = parse_color_component(parts[0])?;
    let g = parse_color_component(parts[1])?;
    let b = parse_color_component(parts[2])?;
    let a = if parts.len() >= 4 {
        parts[3]
            .parse::<f32>()
            .map_err(|_| ColorParseError::InvalidComponent(parts[3].to_string()))?
    } else {
        1.0
    };

    Ok(Color::Rgba(Rgba::new(r, g, b, a)))
}

/// Parse hsl() or hsla() function
fn parse_hsl(s: &str) -> Result<Color, ColorParseError> {
    let is_hsla = s.starts_with("hsla");
    let start = if is_hsla { 5 } else { 4 };

    let end = s
        .find(')')
        .ok_or_else(|| ColorParseError::InvalidFormat(s.to_string()))?;
    let inner = &s[start..end];

    let parts: Vec<&str> = inner.split(',').map(|s| s.trim()).collect();

    if parts.len() < 3 || (is_hsla && parts.len() < 4) {
        return Err(ColorParseError::InvalidFormat(s.to_string()));
    }

    let h = parts[0]
        .parse::<f32>()
        .map_err(|_| ColorParseError::InvalidComponent(parts[0].to_string()))?;
    let s = parse_percentage(parts[1])?;
    let l = parse_percentage(parts[2])?;
    let a = if parts.len() >= 4 {
        parts[3]
            .parse::<f32>()
            .map_err(|_| ColorParseError::InvalidComponent(parts[3].to_string()))?
    } else {
        1.0
    };

    Ok(Color::Hsla(Hsla::new(h, s, l, a)))
}

/// Parse color component (0-255 or 0-100%)
fn parse_color_component(s: &str) -> Result<u8, ColorParseError> {
    if let Some(percent_str) = s.strip_suffix('%') {
        let percent = percent_str
            .parse::<f32>()
            .map_err(|_| ColorParseError::InvalidComponent(s.to_string()))?;
        Ok((percent / 100.0 * 255.0).round() as u8)
    } else {
        s.parse::<u8>()
            .map_err(|_| ColorParseError::InvalidComponent(s.to_string()))
    }
}

/// Parse percentage (0-100%)
fn parse_percentage(s: &str) -> Result<f32, ColorParseError> {
    let percent_str = s
        .strip_suffix('%')
        .ok_or_else(|| ColorParseError::InvalidComponent(s.to_string()))?;
    percent_str
        .parse::<f32>()
        .map_err(|_| ColorParseError::InvalidComponent(s.to_string()))
}

/// Parse named color (all 147 CSS named colors)
fn parse_named_color(s: &str) -> Option<Rgba> {
    let lower = s.to_lowercase();
    match lower.as_str() {
        "aliceblue" => Some(Rgba::rgb(240, 248, 255)),
        "antiquewhite" => Some(Rgba::rgb(250, 235, 215)),
        "aqua" => Some(Rgba::rgb(0, 255, 255)),
        "aquamarine" => Some(Rgba::rgb(127, 255, 212)),
        "azure" => Some(Rgba::rgb(240, 255, 255)),
        "beige" => Some(Rgba::rgb(245, 245, 220)),
        "bisque" => Some(Rgba::rgb(255, 228, 196)),
        "black" => Some(Rgba::BLACK),
        "blanchedalmond" => Some(Rgba::rgb(255, 235, 205)),
        "blue" => Some(Rgba::BLUE),
        "blueviolet" => Some(Rgba::rgb(138, 43, 226)),
        "brown" => Some(Rgba::rgb(165, 42, 42)),
        "burlywood" => Some(Rgba::rgb(222, 184, 135)),
        "cadetblue" => Some(Rgba::rgb(95, 158, 160)),
        "chartreuse" => Some(Rgba::rgb(127, 255, 0)),
        "chocolate" => Some(Rgba::rgb(210, 105, 30)),
        "coral" => Some(Rgba::rgb(255, 127, 80)),
        "cornflowerblue" => Some(Rgba::rgb(100, 149, 237)),
        "cornsilk" => Some(Rgba::rgb(255, 248, 220)),
        "crimson" => Some(Rgba::rgb(220, 20, 60)),
        "cyan" => Some(Rgba::rgb(0, 255, 255)),
        "darkblue" => Some(Rgba::rgb(0, 0, 139)),
        "darkcyan" => Some(Rgba::rgb(0, 139, 139)),
        "darkgoldenrod" => Some(Rgba::rgb(184, 134, 11)),
        "darkgray" => Some(Rgba::rgb(169, 169, 169)),
        "darkgrey" => Some(Rgba::rgb(169, 169, 169)),
        "darkgreen" => Some(Rgba::rgb(0, 100, 0)),
        "darkkhaki" => Some(Rgba::rgb(189, 183, 107)),
        "darkmagenta" => Some(Rgba::rgb(139, 0, 139)),
        "darkolivegreen" => Some(Rgba::rgb(85, 107, 47)),
        "darkorange" => Some(Rgba::rgb(255, 140, 0)),
        "darkorchid" => Some(Rgba::rgb(153, 50, 204)),
        "darkred" => Some(Rgba::rgb(139, 0, 0)),
        "darksalmon" => Some(Rgba::rgb(233, 150, 122)),
        "darkseagreen" => Some(Rgba::rgb(143, 188, 143)),
        "darkslateblue" => Some(Rgba::rgb(72, 61, 139)),
        "darkslategray" => Some(Rgba::rgb(47, 79, 79)),
        "darkslategrey" => Some(Rgba::rgb(47, 79, 79)),
        "darkturquoise" => Some(Rgba::rgb(0, 206, 209)),
        "darkviolet" => Some(Rgba::rgb(148, 0, 211)),
        "deeppink" => Some(Rgba::rgb(255, 20, 147)),
        "deepskyblue" => Some(Rgba::rgb(0, 191, 255)),
        "dimgray" => Some(Rgba::rgb(105, 105, 105)),
        "dimgrey" => Some(Rgba::rgb(105, 105, 105)),
        "dodgerblue" => Some(Rgba::rgb(30, 144, 255)),
        "firebrick" => Some(Rgba::rgb(178, 34, 34)),
        "floralwhite" => Some(Rgba::rgb(255, 250, 240)),
        "forestgreen" => Some(Rgba::rgb(34, 139, 34)),
        "fuchsia" => Some(Rgba::rgb(255, 0, 255)),
        "gainsboro" => Some(Rgba::rgb(220, 220, 220)),
        "ghostwhite" => Some(Rgba::rgb(248, 248, 255)),
        "gold" => Some(Rgba::rgb(255, 215, 0)),
        "goldenrod" => Some(Rgba::rgb(218, 165, 32)),
        "gray" => Some(Rgba::rgb(128, 128, 128)),
        "grey" => Some(Rgba::rgb(128, 128, 128)),
        "green" => Some(Rgba::GREEN),
        "greenyellow" => Some(Rgba::rgb(173, 255, 47)),
        "honeydew" => Some(Rgba::rgb(240, 255, 240)),
        "hotpink" => Some(Rgba::rgb(255, 105, 180)),
        "indianred" => Some(Rgba::rgb(205, 92, 92)),
        "indigo" => Some(Rgba::rgb(75, 0, 130)),
        "ivory" => Some(Rgba::rgb(255, 255, 240)),
        "khaki" => Some(Rgba::rgb(240, 230, 140)),
        "lavender" => Some(Rgba::rgb(230, 230, 250)),
        "lavenderblush" => Some(Rgba::rgb(255, 240, 245)),
        "lawngreen" => Some(Rgba::rgb(124, 252, 0)),
        "lemonchiffon" => Some(Rgba::rgb(255, 250, 205)),
        "lightblue" => Some(Rgba::rgb(173, 216, 230)),
        "lightcoral" => Some(Rgba::rgb(240, 128, 128)),
        "lightcyan" => Some(Rgba::rgb(224, 255, 255)),
        "lightgoldenrodyellow" => Some(Rgba::rgb(250, 250, 210)),
        "lightgray" => Some(Rgba::rgb(211, 211, 211)),
        "lightgrey" => Some(Rgba::rgb(211, 211, 211)),
        "lightgreen" => Some(Rgba::rgb(144, 238, 144)),
        "lightpink" => Some(Rgba::rgb(255, 182, 193)),
        "lightsalmon" => Some(Rgba::rgb(255, 160, 122)),
        "lightseagreen" => Some(Rgba::rgb(32, 178, 170)),
        "lightskyblue" => Some(Rgba::rgb(135, 206, 250)),
        "lightslategray" => Some(Rgba::rgb(119, 136, 153)),
        "lightslategrey" => Some(Rgba::rgb(119, 136, 153)),
        "lightsteelblue" => Some(Rgba::rgb(176, 196, 222)),
        "lightyellow" => Some(Rgba::rgb(255, 255, 224)),
        "lime" => Some(Rgba::rgb(0, 255, 0)),
        "limegreen" => Some(Rgba::rgb(50, 205, 50)),
        "linen" => Some(Rgba::rgb(250, 240, 230)),
        "magenta" => Some(Rgba::rgb(255, 0, 255)),
        "maroon" => Some(Rgba::rgb(128, 0, 0)),
        "mediumaquamarine" => Some(Rgba::rgb(102, 205, 170)),
        "mediumblue" => Some(Rgba::rgb(0, 0, 205)),
        "mediumorchid" => Some(Rgba::rgb(186, 85, 211)),
        "mediumpurple" => Some(Rgba::rgb(147, 112, 219)),
        "mediumseagreen" => Some(Rgba::rgb(60, 179, 113)),
        "mediumslateblue" => Some(Rgba::rgb(123, 104, 238)),
        "mediumspringgreen" => Some(Rgba::rgb(0, 250, 154)),
        "mediumturquoise" => Some(Rgba::rgb(72, 209, 204)),
        "mediumvioletred" => Some(Rgba::rgb(199, 21, 133)),
        "midnightblue" => Some(Rgba::rgb(25, 25, 112)),
        "mintcream" => Some(Rgba::rgb(245, 255, 250)),
        "mistyrose" => Some(Rgba::rgb(255, 228, 225)),
        "moccasin" => Some(Rgba::rgb(255, 228, 181)),
        "navajowhite" => Some(Rgba::rgb(255, 222, 173)),
        "navy" => Some(Rgba::rgb(0, 0, 128)),
        "oldlace" => Some(Rgba::rgb(253, 245, 230)),
        "olive" => Some(Rgba::rgb(128, 128, 0)),
        "olivedrab" => Some(Rgba::rgb(107, 142, 35)),
        "orange" => Some(Rgba::rgb(255, 165, 0)),
        "orangered" => Some(Rgba::rgb(255, 69, 0)),
        "orchid" => Some(Rgba::rgb(218, 112, 214)),
        "palegoldenrod" => Some(Rgba::rgb(238, 232, 170)),
        "palegreen" => Some(Rgba::rgb(152, 251, 152)),
        "paleturquoise" => Some(Rgba::rgb(175, 238, 238)),
        "palevioletred" => Some(Rgba::rgb(219, 112, 147)),
        "papayawhip" => Some(Rgba::rgb(255, 239, 213)),
        "peachpuff" => Some(Rgba::rgb(255, 218, 185)),
        "peru" => Some(Rgba::rgb(205, 133, 63)),
        "pink" => Some(Rgba::rgb(255, 192, 203)),
        "plum" => Some(Rgba::rgb(221, 160, 221)),
        "powderblue" => Some(Rgba::rgb(176, 224, 230)),
        "purple" => Some(Rgba::rgb(128, 0, 128)),
        "rebeccapurple" => Some(Rgba::rgb(102, 51, 153)),
        "red" => Some(Rgba::RED),
        "rosybrown" => Some(Rgba::rgb(188, 143, 143)),
        "royalblue" => Some(Rgba::rgb(65, 105, 225)),
        "saddlebrown" => Some(Rgba::rgb(139, 69, 19)),
        "salmon" => Some(Rgba::rgb(250, 128, 114)),
        "sandybrown" => Some(Rgba::rgb(244, 164, 96)),
        "seagreen" => Some(Rgba::rgb(46, 139, 87)),
        "seashell" => Some(Rgba::rgb(255, 245, 238)),
        "sienna" => Some(Rgba::rgb(160, 82, 45)),
        "silver" => Some(Rgba::rgb(192, 192, 192)),
        "skyblue" => Some(Rgba::rgb(135, 206, 235)),
        "slateblue" => Some(Rgba::rgb(106, 90, 205)),
        "slategray" => Some(Rgba::rgb(112, 128, 144)),
        "slategrey" => Some(Rgba::rgb(112, 128, 144)),
        "snow" => Some(Rgba::rgb(255, 250, 250)),
        "springgreen" => Some(Rgba::rgb(0, 255, 127)),
        "steelblue" => Some(Rgba::rgb(70, 130, 180)),
        "tan" => Some(Rgba::rgb(210, 180, 140)),
        "teal" => Some(Rgba::rgb(0, 128, 128)),
        "thistle" => Some(Rgba::rgb(216, 191, 216)),
        "tomato" => Some(Rgba::rgb(255, 99, 71)),
        "turquoise" => Some(Rgba::rgb(64, 224, 208)),
        "violet" => Some(Rgba::rgb(238, 130, 238)),
        "wheat" => Some(Rgba::rgb(245, 222, 179)),
        "white" => Some(Rgba::WHITE),
        "whitesmoke" => Some(Rgba::rgb(245, 245, 245)),
        "yellow" => Some(Rgba::rgb(255, 255, 0)),
        "yellowgreen" => Some(Rgba::rgb(154, 205, 50)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Rgba tests
    #[test]
    fn test_rgba_creation() {
        let color = Rgba::new(255, 128, 0, 1.0);
        assert_eq!(color.r, 255);
        assert_eq!(color.g, 128);
        assert_eq!(color.b, 0);
        assert_eq!(color.a, 1.0);
    }

    #[test]
    fn test_rgba_constants() {
        assert_eq!(Rgba::BLACK, Rgba::new(0, 0, 0, 1.0));
        assert_eq!(Rgba::WHITE, Rgba::new(255, 255, 255, 1.0));
        assert_eq!(Rgba::RED, Rgba::new(255, 0, 0, 1.0));
        assert_eq!(Rgba::TRANSPARENT, Rgba::new(0, 0, 0, 0.0));
    }

    #[test]
    fn test_rgba_with_alpha() {
        let color = Rgba::RED.with_alpha(0.5);
        assert_eq!(color.a, 0.5);
        assert_eq!(color.r, 255);
    }

    #[test]
    fn test_rgba_is_transparent() {
        assert!(Rgba::TRANSPARENT.is_transparent());
        assert!(!Rgba::BLACK.is_transparent());
        assert!(!Rgba::new(0, 0, 0, 0.5).is_transparent());
    }

    #[test]
    fn test_rgba_is_opaque() {
        assert!(Rgba::BLACK.is_opaque());
        assert!(!Rgba::TRANSPARENT.is_opaque());
        assert!(!Rgba::new(0, 0, 0, 0.5).is_opaque());
    }

    // Hsla tests
    #[test]
    fn test_hsla_creation() {
        let color = Hsla::new(120.0, 100.0, 50.0, 1.0);
        assert_eq!(color.h, 120.0);
        assert_eq!(color.s, 100.0);
        assert_eq!(color.l, 50.0);
        assert_eq!(color.a, 1.0);
    }

    #[test]
    fn test_hsla_clamping() {
        let color = Hsla::new(400.0, 150.0, -10.0, 2.0);
        assert_eq!(color.h, 40.0); // 400 % 360
        assert_eq!(color.s, 100.0); // clamped
        assert_eq!(color.l, 0.0); // clamped
        assert_eq!(color.a, 1.0); // clamped
    }

    // RGB to HSL conversion tests
    #[test]
    fn test_rgb_to_hsl_red() {
        let rgb = Rgba::RED;
        let hsl = rgb.to_hsla();
        assert_eq!(hsl.h, 0.0);
        assert_eq!(hsl.s, 100.0);
        assert_eq!(hsl.l, 50.0);
    }

    #[test]
    fn test_rgb_to_hsl_green() {
        let rgb = Rgba::GREEN;
        let hsl = rgb.to_hsla();
        assert_eq!(hsl.h, 120.0);
        assert_eq!(hsl.s, 100.0);
        assert_eq!(hsl.l, 50.0);
    }

    #[test]
    fn test_rgb_to_hsl_blue() {
        let rgb = Rgba::BLUE;
        let hsl = rgb.to_hsla();
        assert_eq!(hsl.h, 240.0);
        assert_eq!(hsl.s, 100.0);
        assert_eq!(hsl.l, 50.0);
    }

    #[test]
    fn test_rgb_to_hsl_gray() {
        let rgb = Rgba::rgb(128, 128, 128);
        let hsl = rgb.to_hsla();
        assert_eq!(hsl.h, 0.0);
        assert_eq!(hsl.s, 0.0);
        assert!((hsl.l - 50.2).abs() < 0.1); // ~50%
    }

    // HSL to RGB conversion tests
    #[test]
    fn test_hsl_to_rgb_red() {
        let hsl = Hsla::new(0.0, 100.0, 50.0, 1.0);
        let rgb = hsl.to_rgba();
        assert_eq!(rgb, Rgba::RED);
    }

    #[test]
    fn test_hsl_to_rgb_green() {
        let hsl = Hsla::new(120.0, 100.0, 50.0, 1.0);
        let rgb = hsl.to_rgba();
        assert_eq!(rgb, Rgba::GREEN);
    }

    #[test]
    fn test_hsl_to_rgb_blue() {
        let hsl = Hsla::new(240.0, 100.0, 50.0, 1.0);
        let rgb = hsl.to_rgba();
        assert_eq!(rgb, Rgba::BLUE);
    }

    // Hex parsing tests
    #[test]
    fn test_parse_hex_3() {
        let color = Color::parse("#f00").unwrap();
        assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::RED);
    }

    #[test]
    fn test_parse_hex_6() {
        let color = Color::parse("#ff0000").unwrap();
        assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::RED);
    }

    #[test]
    fn test_parse_hex_4() {
        let color = Color::parse("#f008").unwrap();
        let rgba = color.to_rgba(Rgba::BLACK);
        assert_eq!(rgba.r, 255);
        assert_eq!(rgba.g, 0);
        assert_eq!(rgba.b, 0);
        assert!((rgba.a - 0.533).abs() < 0.01); // 0x88 / 0xFF
    }

    #[test]
    fn test_parse_hex_8() {
        let color = Color::parse("#ff000080").unwrap();
        let rgba = color.to_rgba(Rgba::BLACK);
        assert_eq!(rgba.r, 255);
        assert_eq!(rgba.g, 0);
        assert_eq!(rgba.b, 0);
        assert!((rgba.a - 0.5).abs() < 0.01);
    }

    // RGB parsing tests
    #[test]
    fn test_parse_rgb() {
        let color = Color::parse("rgb(255, 0, 0)").unwrap();
        assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::RED);
    }

    #[test]
    fn test_parse_rgba() {
        let color = Color::parse("rgba(255, 0, 0, 0.5)").unwrap();
        let rgba = color.to_rgba(Rgba::BLACK);
        assert_eq!(rgba.r, 255);
        assert_eq!(rgba.a, 0.5);
    }

    #[test]
    fn test_parse_rgb_percent() {
        let color = Color::parse("rgb(100%, 0%, 0%)").unwrap();
        assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::RED);
    }

    // HSL parsing tests
    #[test]
    fn test_parse_hsl() {
        let color = Color::parse("hsl(0, 100%, 50%)").unwrap();
        assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::RED);
    }

    #[test]
    fn test_parse_hsla() {
        let color = Color::parse("hsla(120, 100%, 50%, 0.5)").unwrap();
        let rgba = color.to_rgba(Rgba::BLACK);
        assert_eq!(rgba.g, 255);
        assert_eq!(rgba.a, 0.5);
    }

    // Named color tests
    #[test]
    fn test_parse_named_red() {
        let color = Color::parse("red").unwrap();
        assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::RED);
    }

    #[test]
    fn test_parse_named_rebeccapurple() {
        let color = Color::parse("rebeccapurple").unwrap();
        let rgba = color.to_rgba(Rgba::BLACK);
        assert_eq!(rgba.r, 102);
        assert_eq!(rgba.g, 51);
        assert_eq!(rgba.b, 153);
    }

    #[test]
    fn test_parse_named_case_insensitive() {
        let color1 = Color::parse("RED").unwrap();
        let color2 = Color::parse("Red").unwrap();
        assert_eq!(color1.to_rgba(Rgba::BLACK), color2.to_rgba(Rgba::BLACK));
    }

    // Special keyword tests
    #[test]
    fn test_parse_transparent() {
        let color = Color::parse("transparent").unwrap();
        assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::TRANSPARENT);
    }

    #[test]
    fn test_parse_currentcolor() {
        let color = Color::parse("currentColor").unwrap();
        assert!(color.is_current_color());
        assert_eq!(color.to_rgba(Rgba::BLUE), Rgba::BLUE);
    }

    // Error tests
    #[test]
    fn test_parse_invalid() {
        assert!(Color::parse("invalid").is_err());
        assert!(Color::parse("#xyz").is_err());
        assert!(Color::parse("rgb(300, 0, 0)").is_err());
    }

    // Display tests
    #[test]
    fn test_rgba_display() {
        let color = Rgba::new(255, 0, 0, 1.0);
        assert_eq!(format!("{}", color), "rgb(255, 0, 0)");

        let color = Rgba::new(255, 0, 0, 0.5);
        assert_eq!(format!("{}", color), "rgba(255, 0, 0, 0.500)");
    }

    #[test]
    fn test_hsla_display() {
        let color = Hsla::new(120.0, 100.0, 50.0, 1.0);
        assert_eq!(format!("{}", color), "hsl(120.0, 100.0%, 50.0%)");

        let color = Hsla::new(120.0, 100.0, 50.0, 0.5);
        assert_eq!(format!("{}", color), "hsla(120.0, 100.0%, 50.0%, 0.500)");
    }
}
