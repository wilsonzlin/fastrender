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
//! use fastrender::Color;
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

fn srgb_to_linear_component(c: u8) -> f32 {
  let c = c as f32 / 255.0;
  srgb_to_linear_value(c)
}

fn srgb_to_linear_value(c: f32) -> f32 {
  if c <= 0.04045 {
    c / 12.92
  } else {
    ((c + 0.055) / 1.055).powf(2.4)
  }
}

fn linear_to_srgb_component(c: f32) -> u8 {
  let c = c.clamp(0.0, 1.0);
  let srgb = if c <= 0.0031308 {
    12.92 * c
  } else {
    1.055 * c.powf(1.0 / 2.4) - 0.055
  };
  (srgb * 255.0).round().clamp(0.0, 255.0) as u8
}

fn linear_rgb_to_xyz_d65(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  let x = 0.4124 * r + 0.3576 * g + 0.1805 * b;
  let y = 0.2126 * r + 0.7152 * g + 0.0722 * b;
  let z = 0.0193 * r + 0.1192 * g + 0.9505 * b;
  (x, y, z)
}

fn xyz_d65_to_linear_rgb(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  let r = 3.2406 * x - 1.5372 * y - 0.4986 * z;
  let g = -0.9689 * x + 1.8758 * y + 0.0415 * z;
  let b = 0.0557 * x - 0.204 * y + 1.057 * z;
  (r, g, b)
}

fn xyz_d65_to_d50(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  let xr = 1.0478112 * x + 0.0228866 * y - 0.0501270 * z;
  let yr = 0.0295424 * x + 0.9904844 * y - 0.0170491 * z;
  let zr = -0.0092345 * x + 0.0150436 * y + 0.7521316 * z;
  (xr, yr, zr)
}

fn xyz_d50_to_d65(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  let xr = 0.9555766 * x - 0.0230393 * y + 0.0631636 * z;
  let yr = -0.0282895 * x + 1.0099416 * y + 0.0210077 * z;
  let zr = 0.0122982 * x - 0.020483 * y + 1.3299098 * z;
  (xr, yr, zr)
}

fn lab_to_xyz_d50(l: f32, a: f32, b: f32) -> (f32, f32, f32) {
  // CIE Lab uses D50 white point
  const EPSILON: f32 = 216.0 / 24389.0;
  const KAPPA: f32 = 24389.0 / 27.0;
  const XN: f32 = 0.96422;
  const YN: f32 = 1.0;
  const ZN: f32 = 0.82521;

  let fy = (l + 16.0) / 116.0;
  let fx = fy + a / 500.0;
  let fz = fy - b / 200.0;

  let fx3 = fx * fx * fx;
  let fz3 = fz * fz * fz;

  let xr = if fx3 > EPSILON {
    fx3
  } else {
    (116.0 * fx - 16.0) / KAPPA
  };
  let yr = if l > KAPPA * EPSILON {
    ((l + 16.0) / 116.0).powi(3)
  } else {
    l / KAPPA
  };
  let zr = if fz3 > EPSILON {
    fz3
  } else {
    (116.0 * fz - 16.0) / KAPPA
  };

  (xr * XN, yr * YN, zr * ZN)
}

fn xyz_d50_to_lab(x: f32, y: f32, z: f32) -> (f32, f32, f32) {
  const EPSILON: f32 = 216.0 / 24389.0;
  const KAPPA: f32 = 24389.0 / 27.0;
  const XN: f32 = 0.96422;
  const YN: f32 = 1.0;
  const ZN: f32 = 0.82521;

  let xr = x / XN;
  let yr = y / YN;
  let zr = z / ZN;

  let fx = if xr > EPSILON {
    xr.cbrt()
  } else {
    (KAPPA * xr + 16.0) / 116.0
  };
  let fy = if yr > EPSILON {
    yr.cbrt()
  } else {
    (KAPPA * yr + 16.0) / 116.0
  };
  let fz = if zr > EPSILON {
    zr.cbrt()
  } else {
    (KAPPA * zr + 16.0) / 116.0
  };

  let l = (116.0 * fy - 16.0).max(0.0);
  let a = 500.0 * (fx - fy);
  let b = 200.0 * (fy - fz);
  (l, a, b)
}

fn oklab_to_linear_srgb(l: f32, a: f32, b: f32) -> (f32, f32, f32) {
  let l_ = (l + 0.396_337_78 * a + 0.215_803_76 * b).powi(3);
  let m_ = (l - 0.105_561_35 * a - 0.063_854_17 * b).powi(3);
  let s_ = (l - 0.089_484_18 * a - 1.291_485_5 * b).powi(3);

  let r = 4.076_741_7 * l_ - 3.307_711_6 * m_ + 0.230_969_93 * s_;
  let g = -1.268_438 * l_ + 2.609_757_4 * m_ - 0.341_319_4 * s_;
  let b = 0.004_514_37 * l_ - 0.005_718_789 * m_ + 1.065_574_4 * s_;
  (r, g, b)
}

fn linear_srgb_to_oklab(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  let l = 0.412_221_46 * r + 0.536_332_55 * g + 0.051_446 * b;
  let m = 0.211_903_5 * r + 0.680_699_5 * g + 0.107_396_96 * b;
  let s = 0.088_302_46 * r + 0.281_718_85 * g + 0.629_978_7 * b;

  let l_ = l.cbrt();
  let m_ = m.cbrt();
  let s_ = s.cbrt();

  let l_ok = 0.210_454_26 * l_ + 0.793_617_8 * m_ - 0.004_072_047 * s_;
  let a_ok = 1.977_998_5 * l_ - 2.428_592_2 * m_ + 0.450_593_7 * s_;
  let b_ok = 0.025_904_037 * l_ + 0.782_771_77 * m_ - 0.808_675_77 * s_;
  (l_ok, a_ok, b_ok)
}

fn rgba_to_lab(color: Rgba) -> (f32, f32, f32, f32) {
  let r = srgb_to_linear_component(color.r);
  let g = srgb_to_linear_component(color.g);
  let b = srgb_to_linear_component(color.b);
  let (x_d65, y_d65, z_d65) = linear_rgb_to_xyz_d65(r, g, b);
  let (x_d50, y_d50, z_d50) = xyz_d65_to_d50(x_d65, y_d65, z_d65);
  let (l, a, b) = xyz_d50_to_lab(x_d50, y_d50, z_d50);
  (l, a, b, color.a)
}

fn lab_to_rgba(l: f32, a: f32, b: f32, alpha: f32) -> Rgba {
  let (x_d50, y_d50, z_d50) = lab_to_xyz_d50(l, a, b);
  let (x_d65, y_d65, z_d65) = xyz_d50_to_d65(x_d50, y_d50, z_d50);
  let (r_lin, g_lin, b_lin) = xyz_d65_to_linear_rgb(x_d65, y_d65, z_d65);
  let r = linear_to_srgb_component(r_lin);
  let g = linear_to_srgb_component(g_lin);
  let b = linear_to_srgb_component(b_lin);
  Rgba::new(r, g, b, alpha)
}

fn rgba_to_oklab(color: Rgba) -> (f32, f32, f32, f32) {
  let r = srgb_to_linear_component(color.r);
  let g = srgb_to_linear_component(color.g);
  let b = srgb_to_linear_component(color.b);
  let (l, a, b) = linear_srgb_to_oklab(r, g, b);
  (l, a, b, color.a)
}

fn oklab_to_rgba(l: f32, a: f32, b: f32, alpha: f32) -> Rgba {
  let (r_lin, g_lin, b_lin) = oklab_to_linear_srgb(l, a, b);
  let r = linear_to_srgb_component(r_lin);
  let g = linear_to_srgb_component(g_lin);
  let b = linear_to_srgb_component(b_lin);
  Rgba::new(r, g, b, alpha)
}

/// RGBA color representation
///
/// Represents a color in the RGB color space with an alpha channel.
/// - R, G, B: 0-255 (stored as u8)
/// - A: 0.0-1.0 (stored as f32, where 0.0 is fully transparent, 1.0 is fully opaque)
///
/// # Examples
///
/// ```
/// use fastrender::Rgba;
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
  /// Opaque black
  pub const BLACK: Self = Self {
    r: 0,
    g: 0,
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
  /// Opaque green
  pub const GREEN: Self = Self {
    r: 0,
    g: 255,
    b: 0,
    a: 1.0,
  };
  /// Opaque red
  pub const RED: Self = Self {
    r: 255,
    g: 0,
    b: 0,
    a: 1.0,
  };
  /// Fully transparent black
  pub const TRANSPARENT: Self = Self {
    r: 0,
    g: 0,
    b: 0,
    a: 0.0,
  };
  /// Opaque white
  pub const WHITE: Self = Self {
    r: 255,
    g: 255,
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
  /// use fastrender::Rgba;
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
  /// use fastrender::Rgba;
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
  /// use fastrender::Rgba;
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
  /// use fastrender::Rgba;
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
  /// use fastrender::Rgba;
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
  /// use fastrender::Rgba;
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

  /// Returns the alpha component as a u8 (0-255)
  ///
  /// Useful for APIs that expect u8 alpha like tiny-skia.
  #[inline]
  pub fn alpha_u8(self) -> u8 {
    (self.a * 255.0) as u8
  }

  /// Creates an Rgba from r, g, b, a where alpha is u8 (0-255)
  ///
  /// Converts u8 alpha to f32 internally.
  #[inline]
  pub const fn from_rgba8(r: u8, g: u8, b: u8, a: u8) -> Self {
    Self {
      r,
      g,
      b,
      a: a as f32 / 255.0,
    }
  }

  /// Converts RGB to HSL
  ///
  /// Uses the algorithm from CSS Color Module Level 3.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Rgba;
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
/// use fastrender::Hsla;
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
  /// use fastrender::Hsla;
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
  /// use fastrender::{Hsla, Rgba};
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
      let q = if l < 0.5 {
        l * (1.0 + s)
      } else {
        l + s - l * s
      };
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
      write!(
        f,
        "hsla({:.1}, {:.1}%, {:.1}%, {:.3})",
        self.h, self.s, self.l, self.a
      )
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
/// use fastrender::{Color, Rgba};
///
/// let red = Color::Rgba(Rgba::RED);
/// let current = Color::CurrentColor;
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Color {
  /// RGBA color
  Rgba(Rgba),

  /// HSLA color
  Hsla(Hsla),

  /// Interpolated color mix
  Mix {
    components: [(Box<Color>, f32); 2],
    space: ColorMixSpace,
  },

  /// Special keyword: currentColor
  /// Uses the current value of the 'color' property
  CurrentColor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorMixSpace {
  Srgb,
  SrgbLinear,
  Lab,
  Lch,
  Oklab,
  Oklch,
}

impl Color {
  /// Converts to RGBA, resolving CurrentColor to the given fallback
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::{Color, Rgba};
  ///
  /// let color = Color::Rgba(Rgba::RED);
  /// assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::RED);
  ///
  /// let current = Color::CurrentColor;
  /// assert_eq!(current.to_rgba(Rgba::BLUE), Rgba::BLUE);
  /// ```
  pub fn to_rgba(&self, current_color: Rgba) -> Rgba {
    match self {
      Color::Rgba(rgba) => *rgba,
      Color::Hsla(hsla) => hsla.to_rgba(),
      Color::Mix { components, space } => {
        mix_colors(*space, &components[0], &components[1], current_color)
      }
      Color::CurrentColor => current_color,
    }
  }

  /// Returns true if this is CurrentColor
  pub fn is_current_color(&self) -> bool {
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
  /// - RGB: rgb()/rgba() with comma- or space-separated components, optional slash alpha, number or percentage channels
  /// - HSL: hsl()/hsla() with commas or spaces, optional slash alpha, hue angles deg/rad/grad/turn
  /// - HWB: hwb() with hue angle, whiteness/blackness percentages, optional slash alpha
  /// - Named colors: red, blue, etc.
  /// - Special: transparent, currentColor
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Color;
  ///
  /// assert!(Color::parse("#ff0000").is_ok());
  /// assert!(Color::parse("rgb(255, 0, 0)").is_ok());
  /// assert!(Color::parse("red").is_ok());
  /// assert!(Color::parse("transparent").is_ok());
  /// ```
  pub fn parse(s: &str) -> Result<Self, ColorParseError> {
    let s = s.trim();
    let lower = s.to_ascii_lowercase();

    // Special keywords
    if lower == "transparent" {
      return Ok(Color::transparent());
    }
    if lower == "currentcolor" {
      return Ok(Color::CurrentColor);
    }

    if lower.starts_with("color-mix(") {
      return parse_color_mix(s);
    }

    // Hex colors
    if s.starts_with('#') {
      return parse_hex(s);
    }

    // RGB/RGBA functions
    if lower.starts_with("rgb(") || lower.starts_with("rgba(") {
      return parse_rgb(s);
    }

    // HSL/HSLA functions
    if lower.starts_with("hsl(") || lower.starts_with("hsla(") {
      return parse_hsl(s);
    }

    // HWB function
    if lower.starts_with("hwb(") {
      return parse_hwb(s);
    }

    if lower.starts_with("color(") {
      return parse_color_function(s);
    }

    // Lab/Lch/Oklab/Oklch
    if lower.starts_with("lab(") {
      return parse_lab(s);
    }
    if lower.starts_with("lch(") {
      return parse_lch(s);
    }
    if lower.starts_with("oklab(") {
      return parse_oklab(s);
    }
    if lower.starts_with("oklch(") {
      return parse_oklch(s);
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
      Color::Mix { .. } => write!(f, "color-mix(...)"),
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
      let r = u8::from_str_radix(&hex[0..1].repeat(2), 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      let g = u8::from_str_radix(&hex[1..2].repeat(2), 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      let b = u8::from_str_radix(&hex[2..3].repeat(2), 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      (r, g, b, 1.0)
    }
    4 => {
      // #RGBA -> #RRGGBBAA
      let r = u8::from_str_radix(&hex[0..1].repeat(2), 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      let g = u8::from_str_radix(&hex[1..2].repeat(2), 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      let b = u8::from_str_radix(&hex[2..3].repeat(2), 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      let a = u8::from_str_radix(&hex[3..4].repeat(2), 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      (r, g, b, a as f32 / 255.0)
    }
    6 => {
      // #RRGGBB
      let r = u8::from_str_radix(&hex[0..2], 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      let g = u8::from_str_radix(&hex[2..4], 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      let b = u8::from_str_radix(&hex[4..6], 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      (r, g, b, 1.0)
    }
    8 => {
      // #RRGGBBAA
      let r = u8::from_str_radix(&hex[0..2], 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      let g = u8::from_str_radix(&hex[2..4], 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      let b = u8::from_str_radix(&hex[4..6], 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      let a = u8::from_str_radix(&hex[6..8], 16)
        .map_err(|_| ColorParseError::InvalidHex(s.to_string()))?;
      (r, g, b, a as f32 / 255.0)
    }
    _ => return Err(ColorParseError::InvalidHex(s.to_string())),
  };

  Ok(Color::Rgba(Rgba::new(r, g, b, a)))
}

/// Parse rgb() or rgba() function (modern syntax)
fn parse_rgb(s: &str) -> Result<Color, ColorParseError> {
  let inner = s
    .split_once('(')
    .and_then(|(_, rest)| rest.rsplit_once(')').map(|(body, _)| body))
    .ok_or_else(|| ColorParseError::InvalidFormat(s.to_string()))?;

  let mut input = cssparser::ParserInput::new(inner);
  let mut parser = cssparser::Parser::new(&mut input);

  let mut channels = Vec::new();
  let mut comma_syntax = false;

  channels.push(parse_rgb_channel(&mut parser)?);
  if parser.try_parse(|p| p.expect_comma()).is_ok() {
    comma_syntax = true;
  }

  channels.push(parse_rgb_channel(&mut parser)?);
  if comma_syntax && parser.try_parse(|p| p.expect_comma()).is_err() {
    return Err(ColorParseError::InvalidFormat(s.to_string()));
  }
  channels.push(parse_rgb_channel(&mut parser)?);

  let alpha = if comma_syntax {
    if parser.try_parse(|p| p.expect_comma()).is_ok() {
      parse_alpha_component(&mut parser)?
    } else {
      1.0
    }
  } else if parser.try_parse(|p| p.expect_delim('/')).is_ok() {
    parse_alpha_component(&mut parser)?
  } else {
    1.0
  };

  if channels.len() != 3 {
    return Err(ColorParseError::InvalidFormat(s.to_string()));
  }

  Ok(Color::Rgba(Rgba::new(
    channels[0],
    channels[1],
    channels[2],
    alpha,
  )))
}

/// Parse hsl() or hsla() function (modern syntax)
fn parse_hsl(s: &str) -> Result<Color, ColorParseError> {
  let inner = s
    .split_once('(')
    .and_then(|(_, rest)| rest.rsplit_once(')').map(|(body, _)| body))
    .ok_or_else(|| ColorParseError::InvalidFormat(s.to_string()))?;

  let mut input = cssparser::ParserInput::new(inner);
  let mut parser = cssparser::Parser::new(&mut input);

  let mut comma_syntax = false;
  let h = parse_hue_component(&mut parser)?;
  if parser.try_parse(|p| p.expect_comma()).is_ok() {
    comma_syntax = true;
  }
  let s_val = parse_percentage_component(&mut parser)?;
  if comma_syntax && parser.try_parse(|p| p.expect_comma()).is_err() {
    return Err(ColorParseError::InvalidFormat(s.to_string()));
  }
  let l_val = parse_percentage_component(&mut parser)?;

  let mut alpha = 1.0;
  if comma_syntax {
    if parser.try_parse(|p| p.expect_comma()).is_ok() {
      alpha = parse_alpha_component(&mut parser)?;
    }
  } else if parser.try_parse(|p| p.expect_delim('/')).is_ok() {
    alpha = parse_alpha_component(&mut parser)?;
  }

  Ok(Color::Hsla(Hsla::new(
    h,
    s_val * 100.0,
    l_val * 100.0,
    alpha,
  )))
}

/// Parse hwb() function
fn parse_hwb(s: &str) -> Result<Color, ColorParseError> {
  let inner = s
    .split_once('(')
    .and_then(|(_, rest)| rest.rsplit_once(')').map(|(body, _)| body))
    .ok_or_else(|| ColorParseError::InvalidFormat(s.to_string()))?;

  let mut input = cssparser::ParserInput::new(inner);
  let mut parser = cssparser::Parser::new(&mut input);

  let h = parse_hue_component(&mut parser)?;
  let w = parse_percentage_component(&mut parser)?;
  let b = parse_percentage_component(&mut parser)?;
  let alpha = if parser.try_parse(|p| p.expect_delim('/')).is_ok() {
    parse_alpha_component(&mut parser)?
  } else {
    1.0
  };

  Ok(Color::Rgba(hwb_to_rgba(h, w, b, alpha)))
}

/// Parse an RGB channel (number or percentage) clamped to 0-255.
fn parse_rgb_channel(parser: &mut cssparser::Parser<'_, '_>) -> Result<u8, ColorParseError> {
  use cssparser::Token;
  let token = parser
    .next()
    .map_err(|_| ColorParseError::InvalidFormat("rgb".to_string()))?;
  let value = match token {
    Token::Number { value, .. } => value.clamp(0.0, 255.0),
    Token::Percentage { unit_value, .. } => (unit_value * 255.0).clamp(0.0, 255.0),
    _ => return Err(ColorParseError::InvalidComponent(format!("{:?}", token))),
  };
  Ok(value.round() as u8)
}

fn parse_alpha_component(parser: &mut cssparser::Parser<'_, '_>) -> Result<f32, ColorParseError> {
  use cssparser::Token;
  let token = parser
    .next()
    .map_err(|_| ColorParseError::InvalidFormat("alpha".to_string()))?;
  let value = match token {
    Token::Number { value, .. } => *value,
    Token::Percentage { unit_value, .. } => unit_value * 1.0,
    _ => return Err(ColorParseError::InvalidComponent(format!("{:?}", token))),
  };
  Ok(value.clamp(0.0, 1.0))
}

fn parse_hue_component(parser: &mut cssparser::Parser<'_, '_>) -> Result<f32, ColorParseError> {
  let token = parser
    .next()
    .map_err(|_| ColorParseError::InvalidFormat("hue".to_string()))?;
  parse_hue_token(&token)
}

fn parse_hue_token(token: &cssparser::Token) -> Result<f32, ColorParseError> {
  match token {
    cssparser::Token::Dimension {
      value, ref unit, ..
    } => {
      let unit = unit.as_ref().to_ascii_lowercase();
      let degrees = match unit.as_str() {
        "deg" => *value,
        "grad" => *value * 0.9,
        "turn" => *value * 360.0,
        "rad" => *value * (180.0 / std::f32::consts::PI),
        _ => return Err(ColorParseError::InvalidComponent(unit)),
      };
      Ok(normalize_hue(degrees))
    }
    cssparser::Token::Number { value, .. } => Ok(normalize_hue(*value)),
    other => Err(ColorParseError::InvalidComponent(format!("{:?}", other))),
  }
}

fn parse_percentage_component(
  parser: &mut cssparser::Parser<'_, '_>,
) -> Result<f32, ColorParseError> {
  use cssparser::Token;
  let token = parser
    .next()
    .map_err(|_| ColorParseError::InvalidFormat("percentage".to_string()))?;
  match token {
    Token::Percentage { unit_value, .. } => Ok(unit_value.clamp(0.0, 1.0)),
    other => Err(ColorParseError::InvalidComponent(format!("{:?}", other))),
  }
}

fn normalize_hue(mut h: f32) -> f32 {
  h %= 360.0;
  if h < 0.0 {
    h += 360.0;
  }
  h
}

fn hwb_to_rgba(h: f32, whiteness: f32, blackness: f32, alpha: f32) -> Rgba {
  let mut w = whiteness.clamp(0.0, 1.0);
  let mut b = blackness.clamp(0.0, 1.0);
  if w + b > 1.0 {
    let sum = w + b;
    w /= sum;
    b /= sum;
  }

  let v = 1.0 - b;
  let s = if v == 0.0 { 0.0 } else { 1.0 - w / v };
  let l = v * (1.0 - s / 2.0);
  let sat = if l == 0.0 || l == 1.0 {
    0.0
  } else {
    (v - l) / l.min(1.0 - l)
  };

  let hsla = Hsla::new(h, sat * 100.0, l * 100.0, alpha);
  hsla.to_rgba()
}

fn parse_lab(s: &str) -> Result<Color, ColorParseError> {
  parse_lab_like(s, false)
}

fn parse_lch(s: &str) -> Result<Color, ColorParseError> {
  parse_lab_like(s, true)
}

fn parse_oklab(s: &str) -> Result<Color, ColorParseError> {
  parse_oklab_like(s, false)
}

fn parse_oklch(s: &str) -> Result<Color, ColorParseError> {
  parse_oklab_like(s, true)
}

fn parse_lab_like(input: &str, polar: bool) -> Result<Color, ColorParseError> {
  let inner = input
    .split_once('(')
    .and_then(|(_, rest)| rest.rsplit_once(')').map(|(body, _)| body))
    .ok_or_else(|| ColorParseError::InvalidFormat(input.to_string()))?;

  let mut parser_input = cssparser::ParserInput::new(inner);
  let mut parser = cssparser::Parser::new(&mut parser_input);

  let l = parse_lab_number(&mut parser, 100.0)?;
  let a_or_c = parse_lab_number(&mut parser, 100.0)?;
  let b_or_h = if polar {
    parse_hue_component(&mut parser)?
  } else {
    parse_lab_number(&mut parser, 100.0)?
  };

  let alpha = if parser.try_parse(|p| p.expect_delim('/')).is_ok() {
    parse_alpha_component(&mut parser)?
  } else {
    1.0
  };

  if !parser.is_exhausted() {
    return Err(ColorParseError::InvalidFormat(input.to_string()));
  }

  let rgba = if polar {
    let l = l.clamp(0.0, 100.0);
    let c = a_or_c.max(0.0);
    let h_rad = b_or_h.to_radians();
    let a = c * h_rad.cos();
    let b = c * h_rad.sin();
    lab_to_rgba(l, a, b, alpha)
  } else {
    lab_to_rgba(l.clamp(0.0, 100.0), a_or_c, b_or_h, alpha)
  };

  Ok(Color::Rgba(rgba))
}

fn parse_oklab_like(input: &str, polar: bool) -> Result<Color, ColorParseError> {
  let inner = input
    .split_once('(')
    .and_then(|(_, rest)| rest.rsplit_once(')').map(|(body, _)| body))
    .ok_or_else(|| ColorParseError::InvalidFormat(input.to_string()))?;

  let mut parser_input = cssparser::ParserInput::new(inner);
  let mut parser = cssparser::Parser::new(&mut parser_input);

  let l_raw = parse_lab_number(&mut parser, 1.0)?;
  let l = if l_raw > 1.0 { l_raw / 100.0 } else { l_raw }.clamp(0.0, 1.0);
  let a_or_c = parse_lab_number(&mut parser, 1.0)?;
  let b_or_h = if polar {
    parse_hue_component(&mut parser)?
  } else {
    parse_lab_number(&mut parser, 1.0)?
  };

  let alpha = if parser.try_parse(|p| p.expect_delim('/')).is_ok() {
    parse_alpha_component(&mut parser)?
  } else {
    1.0
  };

  if !parser.is_exhausted() {
    return Err(ColorParseError::InvalidFormat(input.to_string()));
  }

  let rgba = if polar {
    let c = a_or_c;
    let h = b_or_h.to_radians();
    let a = c * h.cos();
    let b = c * h.sin();
    oklab_to_rgba(l, a, b, alpha)
  } else {
    oklab_to_rgba(l, a_or_c, b_or_h, alpha)
  };

  Ok(Color::Rgba(rgba))
}

fn parse_lab_number(
  parser: &mut cssparser::Parser<'_, '_>,
  percent_scale: f32,
) -> Result<f32, ColorParseError> {
  use cssparser::Token;
  let token = parser
    .next()
    .map_err(|_| ColorParseError::InvalidFormat("lab component".to_string()))?;
  match token {
    Token::Number { value, .. } => Ok(*value),
    Token::Percentage { unit_value, .. } => Ok(unit_value * percent_scale),
    other => Err(ColorParseError::InvalidComponent(format!("{:?}", other))),
  }
}

fn parse_color_function(input: &str) -> Result<Color, ColorParseError> {
  let inner = input
    .strip_prefix("color(")
    .and_then(|rest| rest.strip_suffix(')'))
    .ok_or_else(|| ColorParseError::InvalidFormat(input.to_string()))?;
  let mut input = cssparser::ParserInput::new(inner);
  let mut parser = cssparser::Parser::new(&mut input);

  parser.skip_whitespace();
  let space_ident = parser
    .next()
    .map_err(|_| ColorParseError::InvalidFormat("color() space".to_string()))?;
  let space_name = match space_ident {
    cssparser::Token::Ident(ref ident) => ident.to_ascii_lowercase(),
    other => return Err(ColorParseError::InvalidComponent(format!("{:?}", other))),
  };

  let space = match space_name.as_str() {
    "srgb" => ColorFunctionSpace::Srgb,
    "srgb-linear" => ColorFunctionSpace::SrgbLinear,
    "display-p3" => ColorFunctionSpace::DisplayP3,
    "lab" => ColorFunctionSpace::Lab,
    "lch" => ColorFunctionSpace::Lch,
    "oklab" => ColorFunctionSpace::Oklab,
    "oklch" => ColorFunctionSpace::Oklch,
    "a98-rgb" => ColorFunctionSpace::A98Rgb,
    "prophoto-rgb" => ColorFunctionSpace::ProphotoRgb,
    "rec2020" | "rec-2020" => ColorFunctionSpace::Rec2020,
    "xyz" | "xyz-d65" => ColorFunctionSpace::XyzD65,
    "xyz-d50" => ColorFunctionSpace::XyzD50,
    _ => return Err(ColorParseError::InvalidComponent(space_name)),
  };

  let mut channels: Vec<ColorChannelValue> = Vec::new();
  let mut saw_slash = false;
  loop {
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }
    if parser.try_parse(|p| p.expect_delim('/')).is_ok() {
      saw_slash = true;
      break;
    }
    let token = parser
      .next()
      .map_err(|_| ColorParseError::InvalidFormat("color() channel".to_string()))?;
    let value = match token {
      ref t @ cssparser::Token::Dimension { .. }
        if matches!(space, ColorFunctionSpace::Lch | ColorFunctionSpace::Oklch)
          && channels.len() == 2 =>
      {
        ColorChannelValue::Number(parse_hue_token(t)?)
      }
      cssparser::Token::Percentage { unit_value, .. } => ColorChannelValue::Percentage(*unit_value),
      cssparser::Token::Number { value, .. } => ColorChannelValue::Number(*value),
      other => return Err(ColorParseError::InvalidComponent(format!("{:?}", other))),
    };
    channels.push(value);
  }

  parser.skip_whitespace();
  let alpha = if saw_slash {
    parser.skip_whitespace();
    let alpha = parse_alpha_component(&mut parser)?;
    parser.skip_whitespace();
    alpha
  } else {
    1.0
  };

  if !parser.is_exhausted() {
    return Err(ColorParseError::InvalidFormat(
      "color() trailing tokens".to_string(),
    ));
  }

  let rgba = color_function_to_rgba(space, &channels, alpha)?;
  Ok(Color::Rgba(rgba))
}

enum ColorFunctionSpace {
  Srgb,
  SrgbLinear,
  DisplayP3,
  Lab,
  Lch,
  Oklab,
  Oklch,
  A98Rgb,
  ProphotoRgb,
  Rec2020,
  XyzD50,
  XyzD65,
}

#[derive(Debug, Clone, Copy)]
enum ColorChannelValue {
  Number(f32),
  Percentage(f32),
}

fn display_p3_to_xyz_d65(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // Using D65 reference white
  let x = 0.486_570_95 * r + 0.265_667_7 * g + 0.198_217_29 * b;
  let y = 0.228_974_56 * r + 0.691_738_5 * g + 0.079_286_91 * b;
  let z = 0.0 * r + 0.045_113_38 * g + 1.043_944_4 * b;
  (x, y, z)
}

fn a98_rgb_to_xyz_d65(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // Adobe RGB (1998) linear to XYZ (D65)
  let x = 0.576_669 * r + 0.185_558_2 * g + 0.188_228_6 * b;
  let y = 0.297_345 * r + 0.627_363_6 * g + 0.075_291_5 * b;
  let z = 0.027_031_4 * r + 0.070_688_9 * g + 0.991_337_5 * b;
  (x, y, z)
}

fn prophoto_rgb_to_xyz_d50(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // ROMM/ProPhoto RGB linear to XYZ (D50)
  let x = 0.797_674_9 * r + 0.135_191_7 * g + 0.031_353_4 * b;
  let y = 0.288_040_2 * r + 0.711_874_1 * g + 0.000_085_7 * b;
  let z = 0.0 * r + 0.0 * g + 0.825_21 * b;
  (x, y, z)
}

fn rec2020_to_xyz_d65(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
  // ITU-R BT.2020 linear to XYZ (D65)
  let x = 0.636_958 * r + 0.144_617 * g + 0.168_881 * b;
  let y = 0.262_7 * r + 0.677_998 * g + 0.059_302 * b;
  let z = 0.0 * r + 0.028_073 * g + 1.060_985 * b;
  (x, y, z)
}

fn channel_value(channel: &ColorChannelValue) -> f32 {
  match channel {
    ColorChannelValue::Number(v) => *v,
    ColorChannelValue::Percentage(p) => *p,
  }
}

fn decode_a98_rgb(v: f32) -> f32 {
  let sign = if v < 0.0 { -1.0 } else { 1.0 };
  sign * v.abs().powf(563.0 / 256.0)
}

fn decode_prophoto_rgb(v: f32) -> f32 {
  let sign = if v < 0.0 { -1.0 } else { 1.0 };
  let abs = v.abs();
  if abs < 16.0 / 512.0 {
    sign * abs / 16.0
  } else {
    sign * abs.powf(1.8)
  }
}

fn decode_rec2020(v: f32) -> f32 {
  let sign = if v < 0.0 { -1.0 } else { 1.0 };
  let abs = v.abs();
  if abs < 0.08145 {
    sign * abs / 4.5
  } else {
    sign * ((abs + 0.099) / 1.099).powf(1.0 / 0.45)
  }
}

fn color_function_to_rgba(
  space: ColorFunctionSpace,
  channels: &[ColorChannelValue],
  alpha: f32,
) -> Result<Rgba, ColorParseError> {
  if channels.len() != 3 {
    return Err(ColorParseError::InvalidFormat(
      "color() requires three channels".to_string(),
    ));
  }

  let alpha = alpha.clamp(0.0, 1.0);

  match space {
    ColorFunctionSpace::Srgb => {
      let r = (channel_value(&channels[0]) * 255.0)
        .round()
        .clamp(0.0, 255.0) as u8;
      let g = (channel_value(&channels[1]) * 255.0)
        .round()
        .clamp(0.0, 255.0) as u8;
      let b = (channel_value(&channels[2]) * 255.0)
        .round()
        .clamp(0.0, 255.0) as u8;
      Ok(Rgba::new(r, g, b, alpha))
    }
    ColorFunctionSpace::SrgbLinear => {
      let r = linear_to_srgb_component(channel_value(&channels[0]));
      let g = linear_to_srgb_component(channel_value(&channels[1]));
      let b = linear_to_srgb_component(channel_value(&channels[2]));
      Ok(Rgba::new(r, g, b, alpha))
    }
    ColorFunctionSpace::DisplayP3 => {
      let r = srgb_to_linear_value(channel_value(&channels[0]));
      let g = srgb_to_linear_value(channel_value(&channels[1]));
      let b = srgb_to_linear_value(channel_value(&channels[2]));
      let (x, y, z) = display_p3_to_xyz_d65(r, g, b);
      let (rs, gs, bs) = xyz_d65_to_linear_rgb(x, y, z);
      Ok(Rgba::new(
        linear_to_srgb_component(rs),
        linear_to_srgb_component(gs),
        linear_to_srgb_component(bs),
        alpha,
      ))
    }
    ColorFunctionSpace::A98Rgb => {
      let r = decode_a98_rgb(channel_value(&channels[0]));
      let g = decode_a98_rgb(channel_value(&channels[1]));
      let b = decode_a98_rgb(channel_value(&channels[2]));
      let (x, y, z) = a98_rgb_to_xyz_d65(r, g, b);
      let (rs, gs, bs) = xyz_d65_to_linear_rgb(x, y, z);
      Ok(Rgba::new(
        linear_to_srgb_component(rs),
        linear_to_srgb_component(gs),
        linear_to_srgb_component(bs),
        alpha,
      ))
    }
    ColorFunctionSpace::ProphotoRgb => {
      let r = decode_prophoto_rgb(channel_value(&channels[0]));
      let g = decode_prophoto_rgb(channel_value(&channels[1]));
      let b = decode_prophoto_rgb(channel_value(&channels[2]));
      let (x_d50, y_d50, z_d50) = prophoto_rgb_to_xyz_d50(r, g, b);
      let (x, y, z) = xyz_d50_to_d65(x_d50, y_d50, z_d50);
      let (rs, gs, bs) = xyz_d65_to_linear_rgb(x, y, z);
      Ok(Rgba::new(
        linear_to_srgb_component(rs),
        linear_to_srgb_component(gs),
        linear_to_srgb_component(bs),
        alpha,
      ))
    }
    ColorFunctionSpace::Rec2020 => {
      let r = decode_rec2020(channel_value(&channels[0]));
      let g = decode_rec2020(channel_value(&channels[1]));
      let b = decode_rec2020(channel_value(&channels[2]));
      let (x, y, z) = rec2020_to_xyz_d65(r, g, b);
      let (rs, gs, bs) = xyz_d65_to_linear_rgb(x, y, z);
      Ok(Rgba::new(
        linear_to_srgb_component(rs),
        linear_to_srgb_component(gs),
        linear_to_srgb_component(bs),
        alpha,
      ))
    }
    ColorFunctionSpace::XyzD65 => {
      let x = channel_value(&channels[0]);
      let y = channel_value(&channels[1]);
      let z = channel_value(&channels[2]);
      let (rs, gs, bs) = xyz_d65_to_linear_rgb(x, y, z);
      Ok(Rgba::new(
        linear_to_srgb_component(rs),
        linear_to_srgb_component(gs),
        linear_to_srgb_component(bs),
        alpha,
      ))
    }
    ColorFunctionSpace::XyzD50 => {
      let x_d50 = channel_value(&channels[0]);
      let y_d50 = channel_value(&channels[1]);
      let z_d50 = channel_value(&channels[2]);
      let (x, y, z) = xyz_d50_to_d65(x_d50, y_d50, z_d50);
      let (rs, gs, bs) = xyz_d65_to_linear_rgb(x, y, z);
      Ok(Rgba::new(
        linear_to_srgb_component(rs),
        linear_to_srgb_component(gs),
        linear_to_srgb_component(bs),
        alpha,
      ))
    }
    ColorFunctionSpace::Lab => {
      let l = match channels[0] {
        ColorChannelValue::Percentage(p) => p * 100.0,
        ColorChannelValue::Number(n) => n,
      }
      .clamp(0.0, 100.0);
      let a = match channels[1] {
        ColorChannelValue::Percentage(p) => p * 100.0,
        ColorChannelValue::Number(n) => n,
      };
      let b = match channels[2] {
        ColorChannelValue::Percentage(p) => p * 100.0,
        ColorChannelValue::Number(n) => n,
      };
      Ok(lab_to_rgba(l, a, b, alpha))
    }
    ColorFunctionSpace::Lch => {
      let l = match channels[0] {
        ColorChannelValue::Percentage(p) => p * 100.0,
        ColorChannelValue::Number(n) => n,
      }
      .clamp(0.0, 100.0);
      let c = match channels[1] {
        ColorChannelValue::Percentage(p) => p * 100.0,
        ColorChannelValue::Number(n) => n,
      }
      .max(0.0);
      let h_deg = match channels[2] {
        ColorChannelValue::Percentage(p) => p * 360.0,
        ColorChannelValue::Number(n) => n,
      };
      let h_rad = normalize_hue(h_deg).to_radians();
      let a = c * h_rad.cos();
      let b = c * h_rad.sin();
      Ok(lab_to_rgba(l, a, b, alpha))
    }
    ColorFunctionSpace::Oklab => {
      let l_raw = channel_value(&channels[0]);
      let l = if l_raw > 1.0 { l_raw / 100.0 } else { l_raw }.clamp(0.0, 1.0);
      let a = channel_value(&channels[1]);
      let b = channel_value(&channels[2]);
      Ok(oklab_to_rgba(l, a, b, alpha))
    }
    ColorFunctionSpace::Oklch => {
      let l_raw = channel_value(&channels[0]);
      let l = if l_raw > 1.0 { l_raw / 100.0 } else { l_raw }.clamp(0.0, 1.0);
      let c = channel_value(&channels[1]).max(0.0);
      let h_deg = match channels[2] {
        ColorChannelValue::Percentage(p) => p * 360.0,
        ColorChannelValue::Number(n) => n,
      };
      let h = normalize_hue(h_deg).to_radians();
      let a = c * h.cos();
      let b = c * h.sin();
      Ok(oklab_to_rgba(l, a, b, alpha))
    }
  }
}

fn parse_color_mix(input: &str) -> Result<Color, ColorParseError> {
  let inner = input
    .strip_prefix("color-mix(")
    .and_then(|rest| rest.strip_suffix(')'))
    .ok_or_else(|| ColorParseError::InvalidFormat(input.to_string()))?;
  let parts = split_top_level_commas(inner);
  if parts.len() != 3 {
    return Err(ColorParseError::InvalidFormat(input.to_string()));
  }

  let space = {
    let mut iter = parts[0].split_whitespace();
    if !matches!(iter.next(), Some(tok) if tok.eq_ignore_ascii_case("in")) {
      return Err(ColorParseError::InvalidFormat(input.to_string()));
    }
    let name = iter
      .next()
      .ok_or_else(|| ColorParseError::InvalidFormat(input.to_string()))?;
    if iter.next().is_some() {
      return Err(ColorParseError::InvalidFormat(input.to_string()));
    }
    match name.to_ascii_lowercase().as_str() {
      "srgb" => ColorMixSpace::Srgb,
      "srgb-linear" => ColorMixSpace::SrgbLinear,
      "lab" => ColorMixSpace::Lab,
      "lch" => ColorMixSpace::Lch,
      "oklab" => ColorMixSpace::Oklab,
      "oklch" => ColorMixSpace::Oklch,
      other => return Err(ColorParseError::InvalidComponent(other.to_string())),
    }
  };

  let (c0, w0) = parse_mix_component(parts[1].trim())?;
  let (c1, w1) = parse_mix_component(parts[2].trim())?;
  let (w0, w1) = normalize_mix_weights(w0, w1);

  Ok(Color::Mix {
    components: [(Box::new(c0), w0), (Box::new(c1), w1)],
    space,
  })
}

fn parse_mix_component(component: &str) -> Result<(Color, Option<f32>), ColorParseError> {
  let (color_part, percent) = split_color_and_percentage(component);
  let color = Color::parse(&color_part)?;
  Ok((color, percent))
}

fn split_top_level_commas(input: &str) -> Vec<&str> {
  let mut parts = Vec::new();
  let mut paren = 0i32;
  let mut bracket = 0i32;
  let mut brace = 0i32;
  let mut start = 0usize;
  let mut in_string: Option<char> = None;
  let mut escape = false;
  for (idx, ch) in input.char_indices() {
    if escape {
      escape = false;
      continue;
    }
    if ch == '\\' {
      escape = true;
      continue;
    }
    if let Some(q) = in_string {
      if ch == q {
        in_string = None;
      }
      continue;
    }
    match ch {
      '"' | '\'' => in_string = Some(ch),
      '(' => paren += 1,
      ')' => paren -= 1,
      '[' => bracket += 1,
      ']' => bracket -= 1,
      '{' => brace += 1,
      '}' => brace -= 1,
      ',' if paren == 0 && bracket == 0 && brace == 0 => {
        parts.push(input[start..idx].trim());
        start = idx + 1;
      }
      _ => {}
    }
  }
  if start < input.len() {
    parts.push(input[start..].trim());
  }
  parts
}

fn split_color_and_percentage(component: &str) -> (String, Option<f32>) {
  let mut depth = 0i32;
  let mut last_space = None;
  for (idx, ch) in component.char_indices() {
    match ch {
      '(' => depth += 1,
      ')' => depth -= 1,
      c if c.is_whitespace() && depth == 0 => last_space = Some(idx),
      _ => {}
    }
  }

  if let Some(idx) = last_space {
    let color = component[..idx].trim_end();
    let tail = component[idx + 1..].trim();
    if let Some(val) = tail.strip_suffix('%') {
      if let Ok(p) = val.trim().parse::<f32>() {
        return (color.to_string(), Some(p.max(0.0)));
      }
    }
    (color.to_string(), None)
  } else {
    (component.trim().to_string(), None)
  }
}

fn normalize_mix_weights(w0: Option<f32>, w1: Option<f32>) -> (f32, f32) {
  match (w0, w1) {
    (Some(a), Some(b)) => {
      let sum = a + b;
      if sum <= f32::EPSILON {
        (0.5, 0.5)
      } else {
        (a / sum, b / sum)
      }
    }
    (Some(a), None) => {
      let remaining = (100.0 - a).max(0.0);
      let sum = a + remaining;
      if sum <= f32::EPSILON {
        (0.5, 0.5)
      } else {
        (a / sum, remaining / sum)
      }
    }
    (None, Some(b)) => {
      let remaining = (100.0 - b).max(0.0);
      let sum = b + remaining;
      if sum <= f32::EPSILON {
        (0.5, 0.5)
      } else {
        (remaining / sum, b / sum)
      }
    }
    (None, None) => (0.5, 0.5),
  }
}

fn mix_colors(
  space: ColorMixSpace,
  first: &(Box<Color>, f32),
  second: &(Box<Color>, f32),
  current: Rgba,
) -> Rgba {
  let (w0, w1) = normalize_mix_weights(Some(first.1), Some(second.1));

  match space {
    ColorMixSpace::Srgb => {
      let c0 = first.0.to_rgba(current);
      let c1 = second.0.to_rgba(current);
      let premix = |c: u8, a: f32| (c as f32 / 255.0) * a;
      let alpha = (c0.a * w0 + c1.a * w1).clamp(0.0, 1.0);
      if alpha == 0.0 {
        return Rgba::TRANSPARENT;
      }
      let r = (premix(c0.r, c0.a) * w0 + premix(c1.r, c1.a) * w1) / alpha;
      let g = (premix(c0.g, c0.a) * w0 + premix(c1.g, c1.a) * w1) / alpha;
      let b = (premix(c0.b, c0.a) * w0 + premix(c1.b, c1.a) * w1) / alpha;
      Rgba::new(
        (r * 255.0).round().clamp(0.0, 255.0) as u8,
        (g * 255.0).round().clamp(0.0, 255.0) as u8,
        (b * 255.0).round().clamp(0.0, 255.0) as u8,
        alpha,
      )
    }
    ColorMixSpace::SrgbLinear => {
      let c0 = first.0.to_rgba(current);
      let c1 = second.0.to_rgba(current);
      let premix = |c: u8, a: f32| srgb_to_linear_component(c) * a;
      let alpha = (c0.a * w0 + c1.a * w1).clamp(0.0, 1.0);
      if alpha == 0.0 {
        return Rgba::TRANSPARENT;
      }
      let r = (premix(c0.r, c0.a) * w0 + premix(c1.r, c1.a) * w1) / alpha;
      let g = (premix(c0.g, c0.a) * w0 + premix(c1.g, c1.a) * w1) / alpha;
      let b = (premix(c0.b, c0.a) * w0 + premix(c1.b, c1.a) * w1) / alpha;
      Rgba::new(
        linear_to_srgb_component(r),
        linear_to_srgb_component(g),
        linear_to_srgb_component(b),
        alpha,
      )
    }
    ColorMixSpace::Lab => {
      let (l0, a0, b0, alpha0) = rgba_to_lab(first.0.to_rgba(current));
      let (l1, a1, b1, alpha1) = rgba_to_lab(second.0.to_rgba(current));
      let l = l0 * w0 + l1 * w1;
      let a = a0 * w0 + a1 * w1;
      let b = b0 * w0 + b1 * w1;
      let alpha = alpha0 * w0 + alpha1 * w1;
      lab_to_rgba(l, a, b, alpha)
    }
    ColorMixSpace::Lch => {
      let (l0, a0, b0, alpha0) = rgba_to_lab(first.0.to_rgba(current));
      let (l1, a1, b1, alpha1) = rgba_to_lab(second.0.to_rgba(current));
      let c0 = (a0 * a0 + b0 * b0).sqrt();
      let c1 = (a1 * a1 + b1 * b1).sqrt();
      let h0 = b0.atan2(a0);
      let h1 = b1.atan2(a1);
      let a_vec = c0 * h0.cos() * w0 + c1 * h1.cos() * w1;
      let b_vec = c0 * h0.sin() * w0 + c1 * h1.sin() * w1;
      let l = l0 * w0 + l1 * w1;
      let alpha = alpha0 * w0 + alpha1 * w1;
      lab_to_rgba(l, a_vec, b_vec, alpha)
    }
    ColorMixSpace::Oklab => {
      let (l0, a0, b0, alpha0) = rgba_to_oklab(first.0.to_rgba(current));
      let (l1, a1, b1, alpha1) = rgba_to_oklab(second.0.to_rgba(current));
      let l = l0 * w0 + l1 * w1;
      let a = a0 * w0 + a1 * w1;
      let b = b0 * w0 + b1 * w1;
      let alpha = alpha0 * w0 + alpha1 * w1;
      oklab_to_rgba(l, a, b, alpha)
    }
    ColorMixSpace::Oklch => {
      let (l0, a0, b0, alpha0) = rgba_to_oklab(first.0.to_rgba(current));
      let (l1, a1, b1, alpha1) = rgba_to_oklab(second.0.to_rgba(current));
      let c0 = (a0 * a0 + b0 * b0).sqrt();
      let c1 = (a1 * a1 + b1 * b1).sqrt();
      let h0 = b0.atan2(a0);
      let h1 = b1.atan2(a1);
      let a_vec = c0 * h0.cos() * w0 + c1 * h1.cos() * w1;
      let b_vec = c0 * h0.sin() * w0 + c1 * h1.sin() * w1;
      let l = l0 * w0 + l1 * w1;
      let alpha = alpha0 * w0 + alpha1 * w1;
      oklab_to_rgba(l, a_vec, b_vec, alpha)
    }
  }
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
    "green" => Some(Rgba::rgb(0, 128, 0)),
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

  #[test]
  fn test_parse_rgb_modern_syntax_with_slash_alpha() {
    let color = Color::parse("rgb(10% 20% 30% / 0.5)").unwrap();
    assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::new(26, 51, 77, 0.5));

    let color = Color::parse("rgb(255 0 0 / 50%)").unwrap();
    assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::new(255, 0, 0, 0.5));
  }

  // HSL parsing tests
  #[test]
  fn test_parse_hsl() {
    let color = Color::parse("hsl(0, 100%, 50%)").unwrap();
    assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::RED);
  }

  #[test]
  fn test_parse_hsl_modern_syntax_and_hwb() {
    let color = Color::parse("hsl(120 100% 50% / 25%)").unwrap();
    assert_eq!(color.to_rgba(Rgba::BLACK), Rgba::new(0, 255, 0, 0.25));

    let hwb = Color::parse("hwb(90 40% 10% / 0.5)").unwrap();
    let rgba = hwb.to_rgba(Rgba::BLACK);
    assert_eq!(rgba.a, 0.5);
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

  #[test]
  fn parses_lab_and_lch() {
    let lab = Color::parse("lab(50% 0 0)").unwrap().to_rgba(Rgba::BLACK);
    assert!((lab.r as i32 - lab.g as i32).abs() < 2 && (lab.g as i32 - lab.b as i32).abs() < 2);

    let lch = Color::parse("lch(60% 40 200 / 0.5)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert!((lch.a - 0.5).abs() < 1e-6);
  }

  #[test]
  fn parses_oklab_and_oklch() {
    let gray = Color::parse("oklab(50% 0 0)").unwrap().to_rgba(Rgba::BLACK);
    assert!(gray.r > 0 && gray.g > 0 && gray.b > 0);

    let oklch = Color::parse("oklch(60% 0.1 40deg / 0.25)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert!((oklch.a - 0.25).abs() < 1e-6);
  }

  #[test]
  fn color_mix_accepts_lab_spaces() {
    let mixed = Color::parse("color-mix(in lab, red 75%, red)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert_eq!(mixed.r, 255);
    assert_eq!(mixed.g, 0);
    assert_eq!(mixed.b, 0);
  }

  #[test]
  fn color_mix_accepts_oklch_space() {
    let mixed = Color::parse("color-mix(in oklch, red, blue)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert!(mixed.r > 0 && mixed.b > 0);
  }

  #[test]
  fn parses_color_mix_srgb_linear() {
    let color = Color::parse("color-mix(in srgb-linear, red 25%, blue)").expect("parsed");
    let rgba = color.to_rgba(Rgba::BLACK);
    assert!(rgba.r > 0 && rgba.b > rgba.r && rgba.g == 0);
  }

  #[test]
  fn color_mix_defaults_to_even_weights() {
    let color = Color::parse("color-mix(in srgb-linear, red, blue)").expect("parsed");
    let rgba = color.to_rgba(Rgba::BLACK);
    assert_eq!(rgba.r, rgba.b);
    assert_eq!(rgba.g, 0);
  }

  #[test]
  fn color_mix_resolves_current_color() {
    // currentColor should be supplied by the caller when resolving the mix.
    let color = Color::parse("color-mix(in srgb, currentColor 50%, blue)").expect("parsed");
    let rgba = color.to_rgba(Rgba::RED);
    // Mixing red and blue 50/50 in srgb should yield purple.
    assert!(rgba.r > 0 && rgba.b > 0);
    assert!(rgba.r == rgba.b);
    assert_eq!(rgba.g, 0);
  }

  #[test]
  fn parses_color_function_srgb_and_p3() {
    let c = Color::parse("color(srgb 1 0 0 / 50%)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert_eq!(c.r, 255);
    assert_eq!(c.g, 0);
    assert_eq!(c.b, 0);
    assert!((c.a - 0.5).abs() < 1e-6);

    let p3 = Color::parse("color(display-p3 0.5 0.4 0.3)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert!(p3.r > 0 && p3.g > 0 && p3.b > 0);
  }

  #[test]
  fn parses_color_function_lab_like_spaces() {
    let lab = Color::parse("color(lab 50% 10 -5)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    let lab_direct = Color::parse("lab(50% 10 -5)").unwrap().to_rgba(Rgba::BLACK);
    assert_eq!(lab, lab_direct);

    let lch = Color::parse("color(lch 60% 30 200deg / 0.25)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    let lch_direct = Color::parse("lch(60% 30 200deg / 0.25)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert_eq!(lch, lch_direct);
  }

  #[test]
  fn parses_color_function_oklab_like_spaces() {
    let oklab = Color::parse("color(oklab 60% 0.1 -0.05)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    let oklab_direct = Color::parse("oklab(60% 0.1 -0.05)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert_eq!(oklab, oklab_direct);

    let oklch = Color::parse("color(oklch 70% 0.1 45deg / 0.4)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    let oklch_direct = Color::parse("oklch(70% 0.1 45deg / 0.4)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert_eq!(oklch, oklch_direct);
  }

  #[test]
  fn parses_color_function_extra_rgb_spaces() {
    let gray_a98 = Color::parse("color(a98-rgb 0.5 0.5 0.5)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert_eq!(gray_a98.r, gray_a98.g);
    assert_eq!(gray_a98.g, gray_a98.b);

    let gray_prophoto = Color::parse("color(prophoto-rgb 50% 50% 50%)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert_eq!(gray_prophoto.r, gray_prophoto.g);
    assert_eq!(gray_prophoto.g, gray_prophoto.b);

    let green_rec2020 = Color::parse("color(rec2020 0 1 0)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert!(green_rec2020.g > green_rec2020.r && green_rec2020.g > green_rec2020.b);
  }

  #[test]
  fn parses_color_function_xyz_spaces() {
    // D65 white point for sRGB
    let white_d65 = Color::parse("color(xyz-d65 0.95047 1 1.08883)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert!(white_d65.r >= 254 && white_d65.g >= 254 && white_d65.b >= 254);

    // D50 white point should also map to near-white after adaptation
    let white_d50 = Color::parse("color(xyz-d50 0.96422 1 0.82521)")
      .unwrap()
      .to_rgba(Rgba::BLACK);
    assert!(white_d50.r >= 254 && white_d50.g >= 254 && white_d50.b >= 252);
  }

  #[test]
  fn split_top_level_commas_respects_strings_and_brackets() {
    let parts =
      super::split_top_level_commas("foo('a, b'), bar[baz,qux], color-mix(in srgb, red, blue)");
    assert_eq!(parts.len(), 3);
    assert!(parts[0].contains("a, b"));
    assert!(parts[1].starts_with("bar["));
    assert!(parts[2].starts_with("color-mix"));
  }

  // Error tests
  #[test]
  fn test_parse_invalid() {
    assert!(Color::parse("invalid").is_err());
    assert!(Color::parse("#xyz").is_err());
    let clamped = Color::parse("rgb(300, 0, 0)").unwrap().to_rgba(Rgba::BLACK);
    assert_eq!(clamped, Rgba::new(255, 0, 0, 1.0));
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
