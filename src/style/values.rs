//! CSS value types
//!
//! This module provides types for representing CSS values in their computed form.
//! These types are used throughout the style and layout systems.
//!
//! # Units
//!
//! CSS supports various length units. We categorize them as:
//! - **Absolute**: px, pt, pc, in, cm, mm
//! - **Font-relative**: em, rem, ex, ch
//! - **Viewport-relative**: vw, vh, vmin, vmax
//! - **Percentages**: Relative to containing block or font size
//!
//! Reference: CSS Values and Units Module Level 3
//! <https://www.w3.org/TR/css-values-3/>

use std::fmt;

/// CSS length units
///
/// Represents the unit portion of a CSS length value.
///
/// # Examples
///
/// ```
/// use fastrender::LengthUnit;
///
/// let unit = LengthUnit::Px;
/// assert!(unit.is_absolute());
///
/// let font_unit = LengthUnit::Em;
/// assert!(font_unit.is_font_relative());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LengthUnit {
    /// Pixels (px) - CSS reference unit, 1/96th of an inch
    Px,

    /// Points (pt) - 1/72nd of an inch
    Pt,

    /// Picas (pc) - 12 points
    Pc,

    /// Inches (in)
    In,

    /// Centimeters (cm)
    Cm,

    /// Millimeters (mm)
    Mm,

    /// Quarter-millimeters (Q)
    Q,

    /// Em units - relative to element's font size
    Em,

    /// Rem units - relative to root element's font size
    Rem,

    /// Ex units - relative to x-height of the font
    Ex,

    /// Ch units - relative to width of '0' character
    Ch,

    /// Viewport width percentage (vw) - 1% of viewport width
    Vw,

    /// Viewport height percentage (vh) - 1% of viewport height
    Vh,

    /// Viewport minimum (vmin) - 1% of smaller viewport dimension
    Vmin,

    /// Viewport maximum (vmax) - 1% of larger viewport dimension
    Vmax,

    /// Percentage (%) - relative to containing block or font size
    Percent,

    /// Calculated length from `calc()`, `min()`, `max()`, or `clamp()`
    Calc,
}

impl LengthUnit {
    /// Returns true if this is an absolute unit (px, pt, pc, in, cm, mm)
    ///
    /// Absolute units have fixed physical sizes and can be converted
    /// between each other without context.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::LengthUnit;
    ///
    /// assert!(LengthUnit::Px.is_absolute());
    /// assert!(LengthUnit::In.is_absolute());
    /// assert!(!LengthUnit::Em.is_absolute());
    /// ```
    pub fn is_absolute(self) -> bool {
        matches!(
            self,
            Self::Px | Self::Pt | Self::Pc | Self::In | Self::Cm | Self::Mm | Self::Q
        )
    }

    /// Returns true if this is a font-relative unit (em, rem, ex, ch)
    ///
    /// Font-relative units require font metrics to resolve.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::LengthUnit;
    ///
    /// assert!(LengthUnit::Em.is_font_relative());
    /// assert!(LengthUnit::Rem.is_font_relative());
    /// assert!(!LengthUnit::Px.is_font_relative());
    /// ```
    pub fn is_font_relative(self) -> bool {
        matches!(self, Self::Em | Self::Rem | Self::Ex | Self::Ch)
    }

    /// Returns true if this is a viewport-relative unit (vw, vh, vmin, vmax)
    ///
    /// Viewport-relative units require viewport dimensions to resolve.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::LengthUnit;
    ///
    /// assert!(LengthUnit::Vw.is_viewport_relative());
    /// assert!(LengthUnit::Vh.is_viewport_relative());
    /// assert!(!LengthUnit::Px.is_viewport_relative());
    /// ```
    pub fn is_viewport_relative(self) -> bool {
        matches!(self, Self::Vw | Self::Vh | Self::Vmin | Self::Vmax)
    }

    /// Returns true if this is a percentage
    pub fn is_percentage(self) -> bool {
        matches!(self, Self::Percent)
    }

    /// Returns the canonical string representation of this unit
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::LengthUnit;
    ///
    /// assert_eq!(LengthUnit::Px.as_str(), "px");
    /// assert_eq!(LengthUnit::Em.as_str(), "em");
    /// assert_eq!(LengthUnit::Percent.as_str(), "%");
    /// ```
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Px => "px",
            Self::Pt => "pt",
            Self::Pc => "pc",
            Self::In => "in",
            Self::Cm => "cm",
            Self::Mm => "mm",
            Self::Q => "q",
            Self::Em => "em",
            Self::Rem => "rem",
            Self::Ex => "ex",
            Self::Ch => "ch",
            Self::Vw => "vw",
            Self::Vh => "vh",
            Self::Vmin => "vmin",
            Self::Vmax => "vmax",
            Self::Percent => "%",
            Self::Calc => "calc",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CalcTerm {
    pub unit: LengthUnit,
    pub value: f32,
}

const MAX_CALC_TERMS: usize = 8;
const EMPTY_TERM: CalcTerm = CalcTerm {
    unit: LengthUnit::Px,
    value: 0.0,
};

/// Linear combination of length units produced by `calc()`, `min()`, `max()`, or `clamp()`.
///
/// Terms are stored as unit coefficients (e.g., `50% + 10px - 2vw`), and resolved later with
/// the appropriate percentage base, viewport, and font metrics.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CalcLength {
    terms: [CalcTerm; MAX_CALC_TERMS],
    term_count: u8,
}

impl CalcLength {
    pub const fn empty() -> Self {
        Self {
            terms: [EMPTY_TERM; MAX_CALC_TERMS],
            term_count: 0,
        }
    }

    pub fn single(unit: LengthUnit, value: f32) -> Self {
        let mut calc = Self::empty();
        let _ = calc.push(unit, value);
        calc
    }

    pub fn terms(&self) -> &[CalcTerm] {
        &self.terms[..self.term_count as usize]
    }

    fn push(&mut self, unit: LengthUnit, value: f32) -> Result<(), ()> {
        if value == 0.0 {
            return Ok(());
        }
        if let Some(existing) = self.terms().iter().position(|t| t.unit == unit).map(|idx| idx as usize) {
            self.terms[existing].value += value;
            if self.terms[existing].value == 0.0 {
                // Remove zeroed term
                let len = self.term_count as usize;
                for i in existing..len - 1 {
                    self.terms[i] = self.terms[i + 1];
                }
                self.terms[len - 1] = EMPTY_TERM;
                self.term_count -= 1;
            }
            return Ok(());
        }

        let len = self.term_count as usize;
        if len >= MAX_CALC_TERMS {
            return Err(()); // overflow; reject overly complex expressions
        }
        self.terms[len] = CalcTerm { unit, value };
        self.term_count += 1;
        Ok(())
    }

    pub fn scale(&self, factor: f32) -> Self {
        let mut out = Self::empty();
        for term in self.terms() {
            let _ = out.push(term.unit, term.value * factor);
        }
        out
    }

    pub fn add_scaled(&self, other: &CalcLength, scale: f32) -> Option<Self> {
        let mut out = *self;
        for term in other.terms() {
            if out.push(term.unit, term.value * scale).is_err() {
                return None;
            }
        }
        Some(out)
    }

    pub fn is_zero(&self) -> bool {
        self.term_count == 0 || self.terms().iter().all(|t| t.value == 0.0)
    }

    pub fn has_percentage(&self) -> bool {
        self.terms().iter().any(|t| t.unit == LengthUnit::Percent)
    }

    pub fn resolve(
        &self,
        percentage_base: Option<f32>,
        viewport_width: f32,
        viewport_height: f32,
        font_size_px: f32,
        root_font_size_px: f32,
    ) -> Option<f32> {
        let mut total = 0.0;
        for term in self.terms() {
            let resolved = match term.unit {
                LengthUnit::Percent => percentage_base.map(|b| (term.value / 100.0) * b),
                u if u.is_absolute() => Some(Length::new(term.value, u).to_px()),
                u if u.is_viewport_relative() => {
                    Length::new(term.value, u).resolve_with_viewport(viewport_width, viewport_height)
                }
                LengthUnit::Em => Some(term.value * font_size_px),
                LengthUnit::Ex | LengthUnit::Ch => Some(term.value * font_size_px * 0.5),
                LengthUnit::Rem => Some(term.value * root_font_size_px),
                LengthUnit::Calc => None,
                _ => None,
            }?;
            total += resolved;
        }
        Some(total)
    }

    pub fn single_term(&self) -> Option<CalcTerm> {
        if self.term_count == 1 {
            Some(self.terms[0])
        } else {
            None
        }
    }

    pub fn requires_context(&self) -> bool {
        self.terms().iter().any(|t| {
            t.unit.is_percentage()
                || t.unit.is_viewport_relative()
                || t.unit.is_font_relative()
                || matches!(t.unit, LengthUnit::Calc)
        })
    }

    pub fn absolute_sum(&self) -> Option<f32> {
        let mut total = 0.0;
        for term in self.terms() {
            match term.unit {
                u if u.is_absolute() => total += Length::new(term.value, u).to_px(),
                _ => return None,
            }
        }
        Some(total)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // LengthUnit tests
    #[test]
    fn test_length_unit_classification() {
        assert!(LengthUnit::Px.is_absolute());
        assert!(LengthUnit::Pt.is_absolute());
        assert!(LengthUnit::In.is_absolute());
        assert!(LengthUnit::Q.is_absolute());

        assert!(LengthUnit::Em.is_font_relative());
        assert!(LengthUnit::Rem.is_font_relative());

        assert!(LengthUnit::Vw.is_viewport_relative());
        assert!(LengthUnit::Vh.is_viewport_relative());

        assert!(LengthUnit::Percent.is_percentage());
    }

    #[test]
    fn test_length_unit_as_str() {
        assert_eq!(LengthUnit::Px.as_str(), "px");
        assert_eq!(LengthUnit::Em.as_str(), "em");
        assert_eq!(LengthUnit::Percent.as_str(), "%");
    }

    // Length tests
    #[test]
    fn test_length_constructors() {
        let px = Length::px(100.0);
        assert_eq!(px.value, 100.0);
        assert_eq!(px.unit, LengthUnit::Px);

        let em = Length::em(2.0);
        assert_eq!(em.value, 2.0);
        assert_eq!(em.unit, LengthUnit::Em);
    }

    #[test]
    fn test_length_to_px() {
        assert_eq!(Length::px(100.0).to_px(), 100.0);
        assert_eq!(Length::inches(1.0).to_px(), 96.0);
        assert!((Length::pt(72.0).to_px() - 96.0).abs() < 0.1); // 72pt = 1in = 96px
    }

    #[test]
    fn test_length_unit_conversions() {
        // 1 inch = 96px
        assert_eq!(Length::inches(1.0).to_px(), 96.0);

        // 1 point = 1/72 inch
        let pt_to_px = Length::pt(72.0).to_px();
        assert!((pt_to_px - 96.0).abs() < 0.01);

        // 1 pica = 12 points = 16px
        assert_eq!(Length::pc(1.0).to_px(), 16.0);

        // 1 cm = 96/2.54 px
        let cm_to_px = Length::cm(2.54).to_px();
        assert!((cm_to_px - 96.0).abs() < 0.1);
    }

    #[test]
    fn test_length_percentage_resolution() {
        let percent = Length::percent(50.0);
        assert_eq!(percent.resolve_against(200.0), Some(100.0));
        assert_eq!(percent.resolve_against(100.0), Some(50.0));
    }

    #[test]
    fn test_length_resolution_without_context_returns_none() {
        let em = Length::em(2.0);
        assert_eq!(em.resolve_against(100.0), None);

        let vw = Length::new(10.0, LengthUnit::Vw);
        assert_eq!(vw.resolve_against(100.0), None);
    }

    #[test]
    fn test_length_font_size_resolution() {
        let em = Length::em(2.0);
        assert_eq!(em.resolve_with_font_size(16.0), Some(32.0));

        let rem = Length::rem(1.5);
        assert_eq!(rem.resolve_with_font_size(16.0), Some(24.0));

        let ex = Length::ex(2.0);
        assert_eq!(ex.resolve_with_font_size(16.0), Some(16.0));

        let ch = Length::ch(3.0);
        assert_eq!(ch.resolve_with_font_size(16.0), Some(24.0));
    }

    #[test]
    fn test_length_viewport_resolution() {
        let vw = Length::new(50.0, LengthUnit::Vw);
        assert_eq!(vw.resolve_with_viewport(800.0, 600.0), Some(400.0));

        let vh = Length::new(50.0, LengthUnit::Vh);
        assert_eq!(vh.resolve_with_viewport(800.0, 600.0), Some(300.0));

        let vmin = Length::new(10.0, LengthUnit::Vmin);
        assert_eq!(vmin.resolve_with_viewport(800.0, 600.0), Some(60.0)); // 10% of 600

        let vmax = Length::new(10.0, LengthUnit::Vmax);
        assert_eq!(vmax.resolve_with_viewport(800.0, 600.0), Some(80.0)); // 10% of 800
    }

    #[test]
    fn test_length_is_zero() {
        assert!(Length::px(0.0).is_zero());
        assert!(Length::em(0.0).is_zero());
        assert!(!Length::px(0.1).is_zero());
    }

    #[test]
    fn test_length_to_px_relative_units_fallback() {
        assert_eq!(Length::em(2.0).to_px(), 2.0);
        assert_eq!(Length::percent(50.0).to_px(), 50.0);
    }

    // LengthOrAuto tests
    #[test]
    fn test_length_or_auto_constructors() {
        let auto = LengthOrAuto::Auto;
        assert!(auto.is_auto());

        let length = LengthOrAuto::px(100.0);
        assert!(!length.is_auto());
        assert_eq!(length.to_px(), Some(100.0));
    }

    #[test]
    fn test_length_or_auto_length() {
        let value = LengthOrAuto::px(100.0);
        assert_eq!(value.length(), Some(Length::px(100.0)));

        let auto = LengthOrAuto::Auto;
        assert_eq!(auto.length(), None);
    }

    #[test]
    fn test_length_or_auto_to_px() {
        assert_eq!(LengthOrAuto::px(100.0).to_px(), Some(100.0));
        assert_eq!(LengthOrAuto::Auto.to_px(), None);

        // Relative units return None (need context)
        let em = LengthOrAuto::Length(Length::em(2.0));
        assert_eq!(em.to_px(), None);
    }

    #[test]
    fn test_length_or_auto_resolve_against() {
        let percent = LengthOrAuto::percent(50.0);
        assert_eq!(percent.resolve_against(200.0), Some(100.0));

        let px = LengthOrAuto::px(75.0);
        assert_eq!(px.resolve_against(200.0), Some(75.0));

        let auto = LengthOrAuto::Auto;
        assert_eq!(auto.resolve_against(200.0), None);
    }

    #[test]
    fn test_length_or_auto_resolve_or() {
        assert_eq!(LengthOrAuto::px(100.0).resolve_or(50.0, 0.0), 100.0);
        assert_eq!(LengthOrAuto::Auto.resolve_or(50.0, 0.0), 50.0);

        let percent = LengthOrAuto::percent(25.0);
        assert_eq!(percent.resolve_or(0.0, 200.0), 50.0);
    }

    #[test]
    fn test_length_or_auto_from_length() {
        let length = Length::px(100.0);
        let auto_length: LengthOrAuto = length.into();
        assert_eq!(auto_length, LengthOrAuto::Length(length));
    }

    #[test]
    fn test_length_display() {
        assert_eq!(format!("{}", Length::px(100.0)), "100px");
        assert_eq!(format!("{}", Length::em(2.5)), "2.5em");
        assert_eq!(format!("{}", Length::percent(50.0)), "50%");
    }

    #[test]
    fn test_length_or_auto_display() {
        assert_eq!(format!("{}", LengthOrAuto::Auto), "auto");
        assert_eq!(format!("{}", LengthOrAuto::px(100.0)), "100px");
    }
}

impl fmt::Display for LengthUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// A CSS length value with a specific unit
///
/// Represents a computed length value that may need further resolution
/// depending on context (containing block size, font size, etc.).
///
/// # Examples
///
/// ```
/// use fastrender::{Length, LengthUnit};
///
/// let length = Length::px(100.0);
/// assert_eq!(length.value, 100.0);
/// assert_eq!(length.unit, LengthUnit::Px);
///
/// let em_length = Length::em(2.0);
/// let resolved = em_length.resolve_with_font_size(16.0);
/// assert_eq!(resolved, Some(32.0));
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Length {
    /// The numeric value
    pub value: f32,
    /// The unit
    pub unit: LengthUnit,
    /// Optional calc() expression (takes precedence over `value`/`unit`)
    pub calc: Option<CalcLength>,
}

impl Length {
    /// Creates a new length with the given value and unit
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::{Length, LengthUnit};
    ///
    /// let length = Length::new(10.0, LengthUnit::Px);
    /// assert_eq!(length.value, 10.0);
    /// ```
    pub const fn new(value: f32, unit: LengthUnit) -> Self {
        Self {
            value,
            unit,
            calc: None,
        }
    }

    /// Creates a length from a calc expression
    pub const fn calc(calc: CalcLength) -> Self {
        Self {
            value: 0.0,
            unit: LengthUnit::Calc,
            calc: Some(calc),
        }
    }

    // Convenience constructors for absolute units

    /// Creates a length in pixels
    pub const fn px(value: f32) -> Self {
        Self::new(value, LengthUnit::Px)
    }

    /// Creates a length in points (1pt = 1.333px)
    pub const fn pt(value: f32) -> Self {
        Self::new(value, LengthUnit::Pt)
    }

    /// Creates a length in picas (1pc = 16px)
    pub const fn pc(value: f32) -> Self {
        Self::new(value, LengthUnit::Pc)
    }

    /// Creates a length in inches (1in = 96px)
    pub const fn inches(value: f32) -> Self {
        Self::new(value, LengthUnit::In)
    }

    /// Creates a length in centimeters (1cm = 37.8px)
    pub const fn cm(value: f32) -> Self {
        Self::new(value, LengthUnit::Cm)
    }

    /// Creates a length in millimeters (1mm = 3.78px)
    pub const fn mm(value: f32) -> Self {
        Self::new(value, LengthUnit::Mm)
    }

    /// Creates a length in quarter-millimeters (1Q = 0.25mm)
    pub const fn q(value: f32) -> Self {
        Self::new(value, LengthUnit::Q)
    }

    // Convenience constructors for relative units

    /// Creates a length in em units
    pub const fn em(value: f32) -> Self {
        Self::new(value, LengthUnit::Em)
    }

    /// Creates a length in rem units
    pub const fn rem(value: f32) -> Self {
        Self::new(value, LengthUnit::Rem)
    }

    /// Creates a length in ex units
    pub const fn ex(value: f32) -> Self {
        Self::new(value, LengthUnit::Ex)
    }

    /// Creates a length in ch units
    pub const fn ch(value: f32) -> Self {
        Self::new(value, LengthUnit::Ch)
    }

    /// Creates a percentage value
    pub const fn percent(value: f32) -> Self {
        Self::new(value, LengthUnit::Percent)
    }

    // Unit conversion methods

    /// Converts this length to pixels
    ///
    /// For absolute units, this performs unit conversion. For relative or
    /// percentage units, this is a best-effort fallback that returns the raw
    /// numeric value when no context is available; use the context-aware
    /// resolve helpers when you need spec-accurate resolution.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Length;
    ///
    /// assert_eq!(Length::px(100.0).to_px(), 100.0);
    /// assert_eq!(Length::pt(72.0).to_px(), 96.0); // 72pt = 1in = 96px
    /// assert_eq!(Length::em(2.0).to_px(), 2.0);   // relative units return raw value without context
    /// ```
    pub fn to_px(self) -> f32 {
        if let Some(calc) = self.calc {
            if let Some(abs) = calc.absolute_sum() {
                return abs;
            }
            // Best-effort fallback when context is missing: treat unresolved units as raw values.
            return calc.terms().iter().map(|t| t.value).sum();
        }
        match self.unit {
            LengthUnit::Px => self.value,
            LengthUnit::Pt => self.value * (96.0 / 72.0), // 1pt = 1/72 inch
            LengthUnit::Pc => self.value * 16.0,          // 1pc = 12pt = 16px
            LengthUnit::In => self.value * 96.0,          // 1in = 96px (CSS spec)
            LengthUnit::Cm => self.value * 37.795276,     // 1cm = 96px/2.54
            LengthUnit::Mm => self.value * 3.7795276,     // 1mm = 1/10 cm
            LengthUnit::Q => self.value * 0.944882,       // 1Q = 1/4 mm
            _ => self.value,
        }
    }

    /// Resolves this length to pixels using a percentage base.
    ///
    /// Returns `None` when the unit cannot be resolved with the provided base
    /// (e.g., font-relative or viewport-relative units).
    ///
    /// Used when the length is relative to a containing block dimension.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Length;
    ///
    /// let length = Length::percent(50.0);
    /// assert_eq!(length.resolve_against(200.0), Some(100.0));
    ///
    /// let px_length = Length::px(100.0);
    /// assert_eq!(px_length.resolve_against(200.0), Some(100.0)); // Absolute units ignore base
    /// ```
    pub fn resolve_against(self, percentage_base: f32) -> Option<f32> {
        if let Some(calc) = self.calc {
            return calc.resolve(Some(percentage_base), 0.0, 0.0, 0.0, 0.0);
        }
        match self.unit {
            LengthUnit::Percent => Some((self.value / 100.0) * percentage_base),
            _ if self.unit.is_absolute() => Some(self.to_px()),
            _ => None,
        }
    }

    /// Resolves this length using a font size (for em/rem units)
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Length;
///
/// let length = Length::em(2.0);
/// assert_eq!(length.resolve_with_font_size(16.0), Some(32.0));
///
/// let rem_length = Length::rem(1.5);
/// assert_eq!(rem_length.resolve_with_font_size(16.0), Some(24.0));
///
/// // ex/ch fallback to 0.5em when font metrics are unavailable
/// let ex_length = Length::ex(2.0);
/// assert_eq!(ex_length.resolve_with_font_size(16.0), Some(16.0));
    /// ```
    pub fn resolve_with_font_size(self, font_size_px: f32) -> Option<f32> {
        if let Some(calc) = self.calc {
            return calc
                .resolve(None, 0.0, 0.0, font_size_px, font_size_px)
                .or_else(|| Some(self.value * font_size_px));
        }
        match self.unit {
            LengthUnit::Em | LengthUnit::Rem => Some(self.value * font_size_px),
            // Approximate ex/ch with font metrics; fallback to 0.5em when actual x-height/zero-width is unknown.
            LengthUnit::Ex | LengthUnit::Ch => Some(self.value * font_size_px * 0.5),
            _ if self.unit.is_absolute() => Some(self.to_px()),
            _ => None,
        }
    }

    /// Resolves this length using viewport dimensions
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::{Length, LengthUnit};
///
/// let length = Length::new(50.0, LengthUnit::Vw);
/// assert_eq!(length.resolve_with_viewport(800.0, 600.0), Some(400.0));
///
/// let vh_length = Length::new(50.0, LengthUnit::Vh);
/// assert_eq!(vh_length.resolve_with_viewport(800.0, 600.0), Some(300.0));
/// ```
    pub fn resolve_with_viewport(self, viewport_width: f32, viewport_height: f32) -> Option<f32> {
        if let Some(calc) = self.calc {
            return calc
                .resolve(None, viewport_width, viewport_height, 0.0, 0.0)
                .or_else(|| Some(self.value));
        }
        match self.unit {
            LengthUnit::Vw => Some((self.value / 100.0) * viewport_width),
            LengthUnit::Vh => Some((self.value / 100.0) * viewport_height),
            LengthUnit::Vmin => Some((self.value / 100.0) * viewport_width.min(viewport_height)),
            LengthUnit::Vmax => Some((self.value / 100.0) * viewport_width.max(viewport_height)),
            _ if self.unit.is_absolute() => Some(self.to_px()),
            _ => None,
        }
    }

    /// Resolves a length (including calc expressions) with all available context.
    ///
    /// Returns `None` when a percentage-based term lacks a base.
    pub fn resolve_with_context(
        &self,
        percentage_base: Option<f32>,
        viewport_width: f32,
        viewport_height: f32,
        font_size_px: f32,
        root_font_size_px: f32,
    ) -> Option<f32> {
        if let Some(calc) = self.calc {
            return calc.resolve(
                percentage_base,
                viewport_width,
                viewport_height,
                font_size_px,
                root_font_size_px,
            );
        }

        if self.unit.is_percentage() {
            percentage_base.map(|b| (self.value / 100.0) * b)
        } else if self.unit.is_viewport_relative() {
            self.resolve_with_viewport(viewport_width, viewport_height)
        } else if self.unit.is_font_relative() {
            self.resolve_with_font_size(if self.unit == LengthUnit::Rem {
                root_font_size_px
            } else {
                font_size_px
            })
        } else if self.unit.is_absolute() {
            Some(self.to_px())
        } else {
            Some(self.value)
        }
    }

    /// Returns true if this length (or any calc term) uses a percentage component.
    ///
    /// Percentages in the block axis require a containing block height to resolve,
    /// so callers can use this to decide whether available block-size changes should
    /// invalidate cached measurements.
    pub fn has_percentage(&self) -> bool {
        if let Some(calc) = self.calc {
            calc.has_percentage()
        } else {
            self.unit.is_percentage()
        }
    }

    /// Returns true if this is a zero length
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Length;
    ///
    /// assert!(Length::px(0.0).is_zero());
    /// assert!(!Length::px(0.1).is_zero());
    /// ```
    pub fn is_zero(self) -> bool {
        if let Some(calc) = self.calc {
            return calc.is_zero();
        }
        self.value == 0.0
    }
}

impl fmt::Display for Length {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.value, self.unit)
    }
}

/// A CSS length value or the `auto` keyword
///
/// Many CSS properties accept either a specific length or `auto`,
/// which means "compute automatically based on context".
///
/// # Examples
///
/// ```
/// use fastrender::{LengthOrAuto, Length};
///
/// let auto_width = LengthOrAuto::Auto;
/// assert!(auto_width.is_auto());
///
/// let fixed_width = LengthOrAuto::Length(Length::px(100.0));
/// assert_eq!(fixed_width.to_px().unwrap(), 100.0);
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LengthOrAuto {
    /// A specific length value
    Length(Length),
    /// The `auto` keyword
    Auto,
}

impl LengthOrAuto {
    /// Creates a length in pixels
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::LengthOrAuto;
    ///
    /// let width = LengthOrAuto::px(100.0);
    /// assert_eq!(width.to_px().unwrap(), 100.0);
    /// ```
    pub const fn px(value: f32) -> Self {
        Self::Length(Length::px(value))
    }

    /// Creates a percentage value
    pub const fn percent(value: f32) -> Self {
        Self::Length(Length::percent(value))
    }

    /// Returns true if this is `auto`
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::LengthOrAuto;
    ///
    /// assert!(LengthOrAuto::Auto.is_auto());
    /// assert!(!LengthOrAuto::px(100.0).is_auto());
    /// ```
    pub fn is_auto(self) -> bool {
        matches!(self, Self::Auto)
    }

    /// Returns the length if this is not auto
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::{LengthOrAuto, Length};
    ///
    /// let value = LengthOrAuto::px(100.0);
    /// assert_eq!(value.length(), Some(Length::px(100.0)));
    ///
    /// assert_eq!(LengthOrAuto::Auto.length(), None);
    /// ```
    pub fn length(self) -> Option<Length> {
        match self {
            Self::Length(length) => Some(length),
            Self::Auto => None,
        }
    }

    /// Converts to pixels if this is an absolute length, otherwise returns None
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::LengthOrAuto;
    ///
    /// assert_eq!(LengthOrAuto::px(100.0).to_px(), Some(100.0));
    /// assert_eq!(LengthOrAuto::Auto.to_px(), None);
    /// ```
    pub fn to_px(self) -> Option<f32> {
        match self {
            Self::Length(length) if length.unit.is_absolute() => Some(length.to_px()),
            _ => None,
        }
    }

    /// Resolves this value against a percentage base
    ///
    /// Returns None if this is Auto.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::LengthOrAuto;
    ///
    /// let percent = LengthOrAuto::percent(50.0);
    /// assert_eq!(percent.resolve_against(200.0), Some(100.0));
    ///
    /// assert_eq!(LengthOrAuto::Auto.resolve_against(200.0), None);
    /// ```
    pub fn resolve_against(self, percentage_base: f32) -> Option<f32> {
        self.length().and_then(|length| length.resolve_against(percentage_base))
    }

    /// Resolves this value, substituting a default for Auto
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::LengthOrAuto;
    ///
    /// assert_eq!(LengthOrAuto::px(100.0).resolve_or(50.0, 0.0), 100.0);
    /// assert_eq!(LengthOrAuto::Auto.resolve_or(50.0, 0.0), 50.0);
    /// ```
    pub fn resolve_or(self, default: f32, percentage_base: f32) -> f32 {
        match self {
            Self::Length(length) => length.resolve_against(percentage_base).unwrap_or(default),
            Self::Auto => default,
        }
    }
}

impl From<Length> for LengthOrAuto {
    fn from(length: Length) -> Self {
        Self::Length(length)
    }
}

impl fmt::Display for LengthOrAuto {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Length(length) => write!(f, "{}", length),
            Self::Auto => write!(f, "auto"),
        }
    }
}
