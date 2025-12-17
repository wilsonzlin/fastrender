//! Absolute Positioning Algorithm
//!
//! Implements CSS absolute positioning as specified in CSS 2.1 Sections 10.3.7 and 10.6.4.
//! Absolute positioning removes elements from normal flow and positions them relative to
//! their containing block.
//!
//! # CSS Specification References
//!
//! - **CSS 2.1 Section 10.3.7**: Absolutely positioned, non-replaced elements (width)
//! - **CSS 2.1 Section 10.6.4**: Absolutely positioned, non-replaced elements (height)
//! - **CSS 2.1 Section 10.1**: Definition of "containing block"
//!
//! # Constraint Equations
//!
//! ## Horizontal Constraint (CSS 2.1 Section 10.3.7)
//!
//! ```text
//! left + margin-left + border-left + padding-left + width +
//! padding-right + border-right + margin-right + right = containing block width
//! ```
//!
//! ## Vertical Constraint (CSS 2.1 Section 10.6.4)
//!
//! ```text
//! top + margin-top + border-top + padding-top + height +
//! padding-bottom + border-bottom + margin-bottom + bottom = containing block height
//! ```
//!
//! # Key Features
//!
//! - Complete constraint equation resolution
//! - Auto margin centering support
//! - Static position fallback for unspecified offsets
//! - Overconstrained case handling (LTR: ignore right, ignore bottom)
//! - Percentage value resolution
//!
//! # Example
//!
//! ```rust,ignore
//! use fastrender::layout::absolute_positioning::{AbsoluteLayout, AbsoluteLayoutInput};
//! use fastrender::layout::ContainingBlock;
//!
//! let layout = AbsoluteLayout::new();
//! let result = layout.layout_absolute(
//!     &input,
//!     &containing_block,
//! )?;
//! ```

use crate::geometry::{Point, Rect, Size};
use crate::layout::formatting_context::LayoutError;
use crate::layout::profile::{layout_timer, LayoutKind};
use crate::layout::utils::{
    content_size_from_box_sizing, resolve_font_relative_length_for_positioned, resolve_offset_for_positioned,
};
use crate::style::computed::PositionedStyle;
use crate::style::position::Position;
use crate::style::values::{Length, LengthOrAuto, LengthUnit};
use crate::style::ComputedStyle;
use crate::text::font_loader::FontContext;
use crate::tree::fragment_tree::FragmentNode;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::OnceLock;

use super::contexts::positioned::ContainingBlock;

/// Result type for position and size calculations
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AbsoluteLayoutResult {
    /// The computed position (relative to containing block origin)
    pub position: Point,
    /// The computed size
    pub size: Size,
    /// The computed margins (may be auto-resolved for centering)
    pub margins: ResolvedMargins,
}

fn log_abs_clamp(kind: &str, min: f32, max: f32, style_min: Length, style_max: Length) {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    let enabled = *ENABLED.get_or_init(|| {
        std::env::var("FASTR_LOG_ABS_CLAMP")
            .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
            .unwrap_or(false)
    });
    if !enabled {
        return;
    }
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    if COUNTER.fetch_add(1, Ordering::Relaxed) < 8 {
        eprintln!(
            "[abs-{kind}-clamp] adjusting max: min={:.3} max={:.3} style_min={:?} style_max={:?}",
            min, max, style_min, style_max
        );
    }
}

fn resolve_length_for_positioned_size(
    len: &Length,
    percentage_base: Option<f32>,
    viewport: Size,
    style: &PositionedStyle,
    font_context: &FontContext,
) -> Option<f32> {
    if len.unit == LengthUnit::Calc {
        return len.resolve_with_context(
            percentage_base,
            viewport.width,
            viewport.height,
            style.font_size,
            style.root_font_size,
        );
    }
    if len.unit.is_percentage() {
        percentage_base.and_then(|base| len.resolve_against(base))
    } else if len.unit.is_absolute() {
        Some(len.to_px())
    } else if len.unit.is_viewport_relative() {
        len.resolve_with_viewport(viewport.width, viewport.height)
    } else {
        Some(resolve_font_relative_length_for_positioned(*len, style, font_context))
    }
}

/// Resolved margin values after auto-margin calculation
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct ResolvedMargins {
    pub top: f32,
    pub right: f32,
    pub bottom: f32,
    pub left: f32,
}

impl ResolvedMargins {
    /// Creates resolved margins from specified values
    pub fn new(top: f32, right: f32, bottom: f32, left: f32) -> Self {
        Self {
            top,
            right,
            bottom,
            left,
        }
    }

    /// Creates zero margins
    pub const fn zero() -> Self {
        Self {
            top: 0.0,
            right: 0.0,
            bottom: 0.0,
            left: 0.0,
        }
    }

    /// Returns horizontal margin sum
    pub fn horizontal(&self) -> f32 {
        self.left + self.right
    }

    /// Returns vertical margin sum
    pub fn vertical(&self) -> f32 {
        self.top + self.bottom
    }
}

/// Input for absolute layout calculation
#[derive(Debug, Clone)]
pub struct AbsoluteLayoutInput {
    /// The computed style of the absolutely positioned element
    pub style: PositionedStyle,
    /// Preferred minimum inline size (min-content), in CSS pixels (content box).
    pub preferred_min_inline_size: Option<f32>,
    /// Preferred inline size (max-content), in CSS pixels (content box).
    pub preferred_inline_size: Option<f32>,
    /// Preferred minimum block size (min-content), in CSS pixels (content box).
    pub preferred_min_block_size: Option<f32>,
    /// Preferred block size (max-content), in CSS pixels (content box).
    pub preferred_block_size: Option<f32>,
    /// The intrinsic size (used when width/height are auto)
    pub intrinsic_size: Size,
    /// The static position (where element would be in normal flow)
    pub static_position: Point,
}

impl AbsoluteLayoutInput {
    /// Creates a new absolute layout input
    pub fn new(style: PositionedStyle, intrinsic_size: Size, static_position: Point) -> Self {
        Self {
            style,
            preferred_min_inline_size: None,
            preferred_inline_size: None,
            preferred_min_block_size: None,
            preferred_block_size: None,
            intrinsic_size,
            static_position,
        }
    }
}

/// Absolute positioning layout engine
///
/// Handles CSS absolute and fixed positioning. Computes position and size
/// based on the constraint equations from CSS 2.1.
///
/// # Thread Safety
///
/// AbsoluteLayout is stateless and can be shared across threads.
#[derive(Clone)]
pub struct AbsoluteLayout {
    font_context: FontContext,
}

impl Default for AbsoluteLayout {
    fn default() -> Self {
        Self::new()
    }
}

impl AbsoluteLayout {
    /// Creates a new absolute layout handler
    pub fn new() -> Self {
        Self {
            font_context: FontContext::new(),
        }
    }

    /// Creates an absolute layout handler that reuses an existing font context.
    ///
    /// Sharing the font context avoids repeatedly scanning system fonts during
    /// positioned layout, which can be very expensive on pages with many
    /// out-of-flow elements.
    pub fn with_font_context(font_context: FontContext) -> Self {
        Self { font_context }
    }

    /// Resolves a computed style into a positioned style using the containing block for percentage bases.
    pub fn resolve_positioned_style(
        &self,
        style: &ComputedStyle,
        containing_block: &ContainingBlock,
    ) -> PositionedStyle {
        resolve_positioned_style(
            style,
            containing_block,
            containing_block.viewport_size(),
            &self.font_context,
        )
    }

    /// Performs complete absolute layout calculation
    ///
    /// This method handles all cases of the CSS constraint equations:
    /// - All specified values (overconstrained case)
    /// - Auto margins for centering
    /// - Static position fallback
    /// - Intrinsic size for auto width/height
    ///
    /// # Arguments
    ///
    /// * `input` - The input parameters including style and intrinsic size
    /// * `containing_block` - The containing block for percentage resolution
    ///
    /// # Returns
    ///
    /// The computed position, size, and resolved margins
    ///
    /// # CSS Specification
    ///
    /// Implements CSS 2.1 Sections 10.3.7 and 10.6.4
    pub fn layout_absolute(
        &self,
        input: &AbsoluteLayoutInput,
        containing_block: &ContainingBlock,
    ) -> Result<AbsoluteLayoutResult, LayoutError> {
        let _profile = layout_timer(LayoutKind::Absolute);
        let style = &input.style;
        let cb_width = containing_block.width();
        let cb_height = containing_block.height();
        let viewport = containing_block.viewport_size();
        let inline_base = containing_block.inline_percentage_base();
        let block_base = containing_block.block_percentage_base();

        // Resolve horizontal position and width
        let (x, mut width, margin_left, margin_right) = self.compute_horizontal(
            style,
            cb_width,
            inline_base,
            viewport,
            input.preferred_min_inline_size,
            input.preferred_inline_size,
            input.intrinsic_size.width,
            input.static_position.x,
        )?;

        // Resolve vertical position and height
        let (y, mut height, margin_top, margin_bottom) = self.compute_vertical(
            style,
            cb_height,
            block_base,
            viewport,
            input.preferred_min_block_size,
            input.preferred_block_size,
            input.intrinsic_size.height,
            input.static_position.y,
        )?;

        // Apply aspect-ratio if authored (CSS Sizing L4).
        if let crate::style::types::AspectRatio::Ratio(ratio) = style.aspect_ratio {
            if ratio > 0.0 {
                let width_auto = matches!(style.width, crate::style::values::LengthOrAuto::Auto);
                let height_auto = matches!(style.height, crate::style::values::LengthOrAuto::Auto);
                if width_auto && !height_auto {
                    width = height * ratio;
                } else if height_auto && !width_auto {
                    height = width / ratio;
                } else if width_auto && height_auto {
                    if input.intrinsic_size.width > 0.0 {
                        width = input.intrinsic_size.width;
                        height = width / ratio;
                    } else if input.intrinsic_size.height > 0.0 {
                        height = input.intrinsic_size.height;
                        width = height * ratio;
                    }
                }
            }
        }

        // Re-apply min/max constraints after aspect-ratio adjustments.
        let horizontal_spacing =
            style.padding.left + style.padding.right + style.border_width.left + style.border_width.right;
        let vertical_spacing =
            style.padding.top + style.padding.bottom + style.border_width.top + style.border_width.bottom;
        let min_width = content_size_from_box_sizing(style.min_width.to_px(), horizontal_spacing, style.box_sizing);
        let mut max_width = content_size_from_box_sizing(style.max_width.to_px(), horizontal_spacing, style.box_sizing);
        if max_width.is_finite() && max_width < min_width {
            log_abs_clamp("width", min_width, max_width, style.min_width, style.max_width);
            max_width = min_width;
        }
        width = width.clamp(min_width, max_width);
        let min_height = content_size_from_box_sizing(style.min_height.to_px(), vertical_spacing, style.box_sizing);
        let mut max_height = content_size_from_box_sizing(style.max_height.to_px(), vertical_spacing, style.box_sizing);
        if max_height.is_finite() && max_height < min_height {
            log_abs_clamp("height", min_height, max_height, style.min_height, style.max_height);
            max_height = min_height;
        }
        height = height.clamp(min_height, max_height);

        // Position relative to containing block origin
        let position = Point::new(containing_block.origin().x + x, containing_block.origin().y + y);

        Ok(AbsoluteLayoutResult {
            position,
            size: Size::new(width, height),
            margins: ResolvedMargins::new(margin_top, margin_right, margin_bottom, margin_left),
        })
    }

    /// Computes horizontal position, width, and margins
    ///
    /// Implements CSS 2.1 Section 10.3.7 constraint equation:
    /// `left + margin-left + border-left + padding-left + width +
    ///  padding-right + border-right + margin-right + right = CB width`
    ///
    /// # Cases
    ///
    /// 1. All three (left, width, right) specified → overconstrained, ignore right (LTR)
    /// 2. left + width specified → compute right (ignored)
    /// 3. right + width specified → compute left
    /// 4. left + right specified → compute width
    /// 5. Only left specified → use intrinsic width, static position for other
    /// 6. Only right specified → use intrinsic width
    /// 7. Only width specified → use static position for left
    /// 8. None specified → use static position and intrinsic width
    ///
    /// Auto margins:
    /// - If overconstrained with auto margins → treat as 0
    /// - If left + right + width specified with auto margins → center
    fn compute_horizontal(
        &self,
        style: &PositionedStyle,
        cb_width: f32,
        inline_base: Option<f32>,
        viewport: Size,
        preferred_min_inline_size: Option<f32>,
        preferred_inline_size: Option<f32>,
        intrinsic_width: f32,
        static_x: f32,
    ) -> Result<(f32, f32, f32, f32), LayoutError> {
        let left = resolve_offset_for_positioned(&style.left, inline_base, viewport, style, &self.font_context);
        let right = resolve_offset_for_positioned(&style.right, inline_base, viewport, style, &self.font_context);

        // Get padding and border (never auto)
        let padding_left = style.padding.left;
        let padding_right = style.padding.right;
        let border_left = style.border_width.left;
        let border_right = style.border_width.right;
        let total_horizontal_spacing = padding_left + padding_right + border_left + border_right;

        let specified_width = match style.width {
            LengthOrAuto::Auto => None,
            LengthOrAuto::Length(len) => {
                resolve_length_for_positioned_size(&len, inline_base, viewport, style, &self.font_context)
                    .map(|px| content_size_from_box_sizing(px, total_horizontal_spacing, style.box_sizing))
            }
        };
        let min_width =
            content_size_from_box_sizing(style.min_width.to_px(), total_horizontal_spacing, style.box_sizing);
        let mut max_width =
            content_size_from_box_sizing(style.max_width.to_px(), total_horizontal_spacing, style.box_sizing);
        if max_width.is_finite() && max_width < min_width {
            log_abs_clamp("width", min_width, max_width, style.min_width, style.max_width);
            max_width = min_width;
        }

        // Compute shrink-to-fit candidates for auto width.
        let preferred_min = preferred_min_inline_size.unwrap_or(intrinsic_width);
        let preferred = preferred_inline_size.unwrap_or(preferred_min);
        let shrink = |available: f32| -> f32 {
            let available = available.max(0.0);
            preferred.min(available.max(preferred_min))
        };

        // Default margin values (auto resolved only when the constraint equation includes both edges)
        let margin_left_auto = style.margin_left_auto;
        let margin_right_auto = style.margin_right_auto;
        let mut margin_left = if margin_left_auto { 0.0 } else { style.margin.left };
        let mut margin_right = if margin_right_auto { 0.0 } else { style.margin.right };

        let (mut x, mut width) = match (left, specified_width, right) {
            // Case 1: All three specified (overconstrained) - ignore right for LTR
            (Some(l), Some(w), Some(_r)) => {
                // For LTR, ignore right value
                let x = l + margin_left + border_left + padding_left;
                (x, w)
            }

            // Case 2: left and width specified, right is auto
            (Some(l), Some(w), None) => {
                let x = l + margin_left + border_left + padding_left;
                (x, w)
            }

            // Case 3: right and width specified, left is auto
            (None, Some(w), Some(r)) => {
                let x = cb_width - r - margin_right - border_right - padding_right - w;
                (x, w)
            }

            // Case 4: left and right specified, width is auto (shrink-to-fit)
            (Some(l), None, Some(r)) => {
                let available = cb_width - l - r - margin_left - margin_right - total_horizontal_spacing;
                let width = shrink(available);
                let x = l + margin_left + border_left + padding_left;
                (x, width)
            }

            // Case 5: Only left specified - width auto shrink-to-fit
            (Some(l), None, None) => {
                let available = cb_width - l - margin_left - margin_right - total_horizontal_spacing;
                let width = shrink(available);
                let x = l + margin_left + border_left + padding_left;
                (x, width)
            }

            // Case 6: Only right specified - width auto shrink-to-fit
            (None, None, Some(r)) => {
                let available = cb_width - r - margin_left - margin_right - total_horizontal_spacing;
                let width = shrink(available);
                let x = cb_width - r - margin_right - border_right - padding_right - width;
                (x, width)
            }

            // Case 7: Only width specified - use static position for left
            (None, Some(w), None) => {
                // Use static position
                let x = static_x + margin_left + border_left + padding_left;
                (x, w)
            }

            // Case 8: None specified - shrink-to-fit using available space
            (None, None, None) => {
                let available = cb_width - margin_left - margin_right - total_horizontal_spacing;
                let width = shrink(available);
                let x = static_x + margin_left + border_left + padding_left;
                (x, width)
            }
        };

        width = width.clamp(min_width, max_width);

        // If the leading edge was auto-resolved from the opposite inset, keep the specified inset
        // satisfied after clamping width (CSS 2.1 constraint equation).
        if left.is_none() {
            if let Some(r) = right {
                x = cb_width - r - margin_right - border_right - padding_right - width;
            }
        }

        // Apply auto margin resolution only when both edges participate in the constraint (CSS 2.1 §10.3.7).
        if left.is_some() && right.is_some() && specified_width.is_some() {
            let left_edge = x - (margin_left + border_left + padding_left);
            let remaining_without_margins =
                cb_width - (left_edge + border_left + padding_left + width + padding_right + border_right);

            if margin_left_auto && margin_right_auto {
                let remaining = remaining_without_margins.max(0.0);
                margin_left = remaining / 2.0;
                margin_right = remaining - margin_left;
                x = left_edge + margin_left + border_left + padding_left;
            } else if margin_left_auto {
                margin_left = remaining_without_margins - margin_right;
                x = left_edge + margin_left + border_left + padding_left;
            } else if margin_right_auto {
                margin_right = remaining_without_margins - margin_left;
            }
        }

        Ok((x, width, margin_left, margin_right))
    }

    /// Computes vertical position, height, and margins
    ///
    /// Implements CSS 2.1 Section 10.6.4 constraint equation
    fn compute_vertical(
        &self,
        style: &PositionedStyle,
        cb_height: f32,
        block_base: Option<f32>,
        viewport: Size,
        preferred_min_block_size: Option<f32>,
        preferred_block_size: Option<f32>,
        intrinsic_height: f32,
        static_y: f32,
    ) -> Result<(f32, f32, f32, f32), LayoutError> {
        let top = resolve_offset_for_positioned(&style.top, block_base, viewport, style, &self.font_context);
        let bottom = resolve_offset_for_positioned(&style.bottom, block_base, viewport, style, &self.font_context);

        // Get padding and border
        let padding_top = style.padding.top;
        let padding_bottom = style.padding.bottom;
        let border_top = style.border_width.top;
        let border_bottom = style.border_width.bottom;
        let total_vertical_spacing = padding_top + padding_bottom + border_top + border_bottom;

        let margin_top_auto = style.margin_top_auto;
        let margin_bottom_auto = style.margin_bottom_auto;
        let mut margin_top = if margin_top_auto { 0.0 } else { style.margin.top };
        let mut margin_bottom = if margin_bottom_auto { 0.0 } else { style.margin.bottom };

        let specified_height = match style.height {
            LengthOrAuto::Auto => None,
            LengthOrAuto::Length(len) => {
                resolve_length_for_positioned_size(&len, block_base, viewport, style, &self.font_context)
                    .map(|px| content_size_from_box_sizing(px, total_vertical_spacing, style.box_sizing))
            }
        };
        let min_height =
            content_size_from_box_sizing(style.min_height.to_px(), total_vertical_spacing, style.box_sizing);
        let mut max_height =
            content_size_from_box_sizing(style.max_height.to_px(), total_vertical_spacing, style.box_sizing);
        if max_height.is_finite() && max_height < min_height {
            log_abs_clamp("height", min_height, max_height, style.min_height, style.max_height);
            max_height = min_height;
        }

        // Compute shrink-to-fit candidates for auto height.
        let preferred_min = preferred_min_block_size.unwrap_or(intrinsic_height);
        let preferred = preferred_block_size.unwrap_or(preferred_min);
        let shrink = |available: f32| -> f32 {
            let available = available.max(0.0);
            preferred.min(available.max(preferred_min))
        };

        let (mut y, mut height) = match (top, specified_height, bottom) {
            // All three specified (overconstrained) - ignore bottom
            (Some(t), Some(h), Some(_b)) => {
                let y = t + margin_top + border_top + padding_top;
                (y, h)
            }

            // top and height specified
            (Some(t), Some(h), None) => {
                let y = t + margin_top + border_top + padding_top;
                (y, h)
            }

            // bottom and height specified
            (None, Some(h), Some(b)) => {
                let y = cb_height - b - margin_bottom - border_bottom - padding_bottom - h;
                (y, h)
            }

            // top and bottom specified, height is auto (fill available space per CSS 2.1)
            (Some(t), None, Some(b)) => {
                let available = cb_height - t - b - margin_top - margin_bottom - total_vertical_spacing;
                let height = available.max(0.0);
                let y = t + margin_top + border_top + padding_top;
                (y, height)
            }

            // Only top specified - shrink-to-fit
            (Some(t), None, None) => {
                let available = cb_height - t - margin_top - margin_bottom - total_vertical_spacing;
                // For an unspecified opposite inset, CSS 2.1 treats bottom as auto; shrink-to-fit against the available CB height.
                let height = shrink(available);
                let y = t + margin_top + border_top + padding_top;
                (y, height)
            }

            // Only bottom specified - shrink-to-fit
            (None, None, Some(b)) => {
                let available = cb_height - b - margin_top - margin_bottom - total_vertical_spacing;
                let height = shrink(available);
                let y = cb_height - b - margin_bottom - border_bottom - padding_bottom - height;
                (y, height)
            }

            // Only height specified - use static position
            (None, Some(h), None) => {
                let y = static_y + margin_top + border_top + padding_top;
                (y, h)
            }

            // None specified - use preferred block size (shrink-to-fit without constraints)
            (None, None, None) => {
                let height = preferred;
                let y = static_y + margin_top + border_top + padding_top;
                (y, height)
            }
        };

        height = height.clamp(min_height, max_height);

        // If the top edge was auto-resolved from the bottom inset, keep the authored bottom inset
        // satisfied after clamping height (CSS 2.1 constraint equation).
        if top.is_none() {
            if let Some(b) = bottom {
                y = cb_height - b - margin_bottom - border_bottom - padding_bottom - height;
            }
        }

        if top.is_some() && bottom.is_some() && specified_height.is_some() {
            let top_edge = y - (margin_top + border_top + padding_top);
            let remaining_without_margins =
                cb_height - (top_edge + border_top + padding_top + height + padding_bottom + border_bottom);

            if margin_top_auto && margin_bottom_auto {
                let remaining = remaining_without_margins.max(0.0);
                margin_top = remaining / 2.0;
                margin_bottom = remaining - margin_top;
                y = top_edge + margin_top + border_top + padding_top;
            } else if margin_top_auto {
                margin_top = remaining_without_margins - margin_bottom;
                y = top_edge + margin_top + border_top + padding_top;
            } else if margin_bottom_auto {
                margin_bottom = remaining_without_margins - margin_top;
            }
        }

        Ok((y, height, margin_top, margin_bottom))
    }

    /// Computes centering for absolutely positioned elements with auto margins
    ///
    /// When left, right, and width are all specified (or implied), and one or both
    /// margins are auto, the element should be centered.
    ///
    /// # CSS 2.1 Section 10.3.7
    ///
    /// "If both 'left' and 'right' are set, and 'width' is not 'auto', and
    /// 'margin-left' or 'margin-right' are 'auto', solve the equation to
    /// distribute the remaining space equally."
    pub fn compute_centered_horizontal(
        &self,
        style: &PositionedStyle,
        cb_width: f32,
        viewport: Size,
        width: f32,
    ) -> (f32, f32, f32) {
        let left = resolve_offset_for_positioned(&style.left, Some(cb_width), viewport, style, &self.font_context)
            .unwrap_or(0.0);
        let right = resolve_offset_for_positioned(&style.right, Some(cb_width), viewport, style, &self.font_context)
            .unwrap_or(0.0);

        let padding_left = style.padding.left;
        let padding_right = style.padding.right;
        let border_left = style.border_width.left;
        let border_right = style.border_width.right;

        let total_spacing = padding_left + padding_right + border_left + border_right + width;
        let remaining = cb_width - left - right - total_spacing;

        // Check which margins are auto (represented as 0 with auto flag)
        let margin_left_specified = style.margin.left;
        let margin_right_specified = style.margin.right;

        // For true auto margin detection, we'd need additional metadata
        // For now, assume margin values of 0 could be auto for centering
        if margin_left_specified == 0.0 && margin_right_specified == 0.0 {
            // Both auto - center
            let half_margin = (remaining / 2.0).max(0.0);
            let x = left + half_margin + border_left + padding_left;
            (x, half_margin, half_margin)
        } else if margin_left_specified == 0.0 {
            // Only left auto
            let margin_left = (remaining - margin_right_specified).max(0.0);
            let x = left + margin_left + border_left + padding_left;
            (x, margin_left, margin_right_specified)
        } else if margin_right_specified == 0.0 {
            // Only right auto
            let margin_right = (remaining - margin_left_specified).max(0.0);
            let x = left + margin_left_specified + border_left + padding_left;
            (x, margin_left_specified, margin_right)
        } else {
            // Neither auto - use specified
            let x = left + margin_left_specified + border_left + padding_left;
            (x, margin_left_specified, margin_right_specified)
        }
    }

    /// Computes centering for vertical axis
    pub fn compute_centered_vertical(
        &self,
        style: &PositionedStyle,
        cb_height: f32,
        viewport: Size,
        height: f32,
    ) -> (f32, f32, f32) {
        let top = resolve_offset_for_positioned(&style.top, Some(cb_height), viewport, style, &self.font_context)
            .unwrap_or(0.0);
        let bottom = resolve_offset_for_positioned(&style.bottom, Some(cb_height), viewport, style, &self.font_context)
            .unwrap_or(0.0);

        let padding_top = style.padding.top;
        let padding_bottom = style.padding.bottom;
        let border_top = style.border_width.top;
        let border_bottom = style.border_width.bottom;

        let total_spacing = padding_top + padding_bottom + border_top + border_bottom + height;
        let remaining = cb_height - top - bottom - total_spacing;

        let margin_top_specified = style.margin.top;
        let margin_bottom_specified = style.margin.bottom;

        if margin_top_specified == 0.0 && margin_bottom_specified == 0.0 {
            let half_margin = (remaining / 2.0).max(0.0);
            let y = top + half_margin + border_top + padding_top;
            (y, half_margin, half_margin)
        } else if margin_top_specified == 0.0 {
            let margin_top = (remaining - margin_bottom_specified).max(0.0);
            let y = top + margin_top + border_top + padding_top;
            (y, margin_top, margin_bottom_specified)
        } else if margin_bottom_specified == 0.0 {
            let margin_bottom = (remaining - margin_top_specified).max(0.0);
            let y = top + margin_top_specified + border_top + padding_top;
            (y, margin_top_specified, margin_bottom)
        } else {
            let y = top + margin_top_specified + border_top + padding_top;
            (y, margin_top_specified, margin_bottom_specified)
        }
    }

    /// Creates a fragment from the layout result
    ///
    /// Convenience method to create a FragmentNode from the computed layout.
    pub fn create_fragment(&self, result: &AbsoluteLayoutResult, children: Vec<FragmentNode>) -> FragmentNode {
        FragmentNode::new_block(Rect::new(result.position, result.size), children)
    }

    /// Checks if an element should use absolute positioning
    pub fn is_absolutely_positioned(style: &PositionedStyle) -> bool {
        matches!(style.position, Position::Absolute | Position::Fixed)
    }

    /// Checks if an element should be laid out with absolute positioning
    pub fn should_layout_absolute(style: &PositionedStyle) -> bool {
        style.position.is_absolutely_positioned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::EdgeOffsets;
    use crate::layout::utils::resolve_offset;
    use crate::style::types::FontSizeAdjust;
    use crate::style::values::{Length, LengthOrAuto, LengthUnit};
    use crate::style::ComputedStyle;
    use crate::text::font_loader::FontContext;

    fn default_style() -> PositionedStyle {
        let mut style = PositionedStyle::default();
        style.border_width = EdgeOffsets::ZERO;
        style
    }

    fn create_containing_block(width: f32, height: f32) -> ContainingBlock {
        ContainingBlock::viewport(Size::new(width, height))
    }

    fn create_containing_block_at(x: f32, y: f32, width: f32, height: f32) -> ContainingBlock {
        ContainingBlock::new(Rect::from_xywh(x, y, width, height))
    }

    // ========== AbsoluteLayout basic tests ==========

    #[test]
    fn test_layout_absolute_all_specified() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(20.0);
        style.top = LengthOrAuto::px(30.0);
        style.width = LengthOrAuto::px(100.0);
        style.height = LengthOrAuto::px(80.0);

        let input = AbsoluteLayoutInput::new(style, Size::new(50.0, 50.0), Point::ZERO);
        let cb = create_containing_block(800.0, 600.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        assert_eq!(result.position.x, 20.0);
        assert_eq!(result.position.y, 30.0);
        assert_eq!(result.size.width, 100.0);
        assert_eq!(result.size.height, 80.0);
    }

    #[test]
    fn test_layout_absolute_right_bottom() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.right = LengthOrAuto::px(50.0);
        style.bottom = LengthOrAuto::px(50.0);
        style.width = LengthOrAuto::px(100.0);
        style.height = LengthOrAuto::px(100.0);

        let input = AbsoluteLayoutInput::new(style, Size::new(50.0, 50.0), Point::ZERO);
        let cb = create_containing_block(400.0, 400.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // x = 400 - 50 - 100 = 250
        // y = 400 - 50 - 100 = 250
        assert_eq!(result.position.x, 250.0);
        assert_eq!(result.position.y, 250.0);
    }

    #[test]
    fn test_layout_absolute_shrink_width_between_insets() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(50.0);
        style.right = LengthOrAuto::px(50.0);
        // width auto - should shrink-to-fit intrinsic width

        let input = AbsoluteLayoutInput::new(style, Size::new(100.0, 100.0), Point::ZERO);
        let cb = create_containing_block(500.0, 400.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // width should honor intrinsic 100px even though 400px is available
        assert_eq!(result.position.x, 50.0);
        assert_eq!(result.size.width, 100.0);
    }

    #[test]
    fn test_layout_absolute_height_fills_between_insets() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.top = LengthOrAuto::px(25.0);
        style.bottom = LengthOrAuto::px(25.0);
        // height auto - should fill the remaining space between top/bottom (preferred block sizes unused without auto)

        let mut input = AbsoluteLayoutInput::new(style, Size::new(100.0, 100.0), Point::ZERO);
        input.preferred_min_block_size = Some(80.0);
        input.preferred_block_size = Some(300.0);
        let cb = create_containing_block(400.0, 300.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        assert!(
            (result.size.height - 250.0).abs() < 0.001,
            "auto height should fill the space between top and bottom"
        );
        assert_eq!(result.position.y, 25.0);
    }

    #[test]
    fn layout_absolute_top_bottom_auto_height_ignores_preferred_block_shrink() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.top = LengthOrAuto::px(10.0);
        style.bottom = LengthOrAuto::px(10.0);
        style.height = LengthOrAuto::Auto;

        let mut input = AbsoluteLayoutInput::new(style, Size::new(50.0, 50.0), Point::ZERO);
        input.preferred_min_block_size = Some(20.0);
        input.preferred_block_size = Some(30.0); // smaller than available
        let cb = create_containing_block(200.0, 200.0); // available height = 180

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.height - 180.0).abs() < 0.001,
            "height auto with top/bottom should use available space, not shrink-to-fit preferred block size"
        );
    }

    #[test]
    fn layout_absolute_auto_height_uses_preferred_block_sizes_without_insets() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.height = LengthOrAuto::Auto;

        let mut input = AbsoluteLayoutInput::new(style, Size::new(10.0, 10.0), Point::ZERO);
        input.preferred_min_block_size = Some(40.0);
        input.preferred_block_size = Some(90.0);
        // available = 200 (no insets)
        let cb = create_containing_block(200.0, 200.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.height - 90.0).abs() < 0.001,
            "auto height with no insets should shrink-to-fit preferred block size"
        );
    }

    #[test]
    fn layout_absolute_single_inset_shrinks_height_to_available() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.top = LengthOrAuto::px(20.0);
        style.height = LengthOrAuto::Auto;

        let mut input = AbsoluteLayoutInput::new(style, Size::new(10.0, 10.0), Point::ZERO);
        input.preferred_min_block_size = Some(80.0);
        input.preferred_block_size = Some(200.0);
        // available = 200 - 20 = 180
        let cb = create_containing_block(200.0, 200.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.height - 180.0).abs() < 0.001,
            "auto height with single inset should shrink to available while respecting min-content"
        );
        assert!(
            (result.position.y - 20.0).abs() < 0.001,
            "top inset should remain at 20"
        );
    }

    #[test]
    fn layout_absolute_auto_height_respects_min_height_in_shrink_case() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.top = LengthOrAuto::px(10.0);
        style.height = LengthOrAuto::Auto;
        style.min_height = Length::px(150.0);

        let mut input = AbsoluteLayoutInput::new(style, Size::new(0.0, 0.0), Point::ZERO);
        input.preferred_min_block_size = Some(20.0);
        input.preferred_block_size = Some(40.0); // would shrink to 190 without min-height clamp
        let cb = create_containing_block(200.0, 200.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.height - 150.0).abs() < 0.001,
            "min-height should clamp auto height when shrink-to-fit yields smaller"
        );
        assert!(
            (result.position.y - 10.0).abs() < 0.001,
            "top inset should be preserved even when min-height expands the box"
        );
    }

    #[test]
    fn layout_absolute_auto_height_respects_min_height_with_bottom_inset() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.bottom = LengthOrAuto::px(15.0);
        style.height = LengthOrAuto::Auto;
        style.min_height = Length::px(90.0);

        let mut input = AbsoluteLayoutInput::new(style, Size::new(0.0, 0.0), Point::ZERO);
        input.preferred_min_block_size = Some(10.0);
        input.preferred_block_size = Some(40.0); // would shrink to 40 without min-height clamp
        let cb = create_containing_block(200.0, 150.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.height - 90.0).abs() < 0.001,
            "min-height should clamp auto height when shrink-to-fit yields smaller"
        );
        assert!(
            (result.position.y - (150.0 - 15.0 - 90.0)).abs() < 0.001,
            "bottom inset should position the clamped box"
        );
    }

    #[test]
    fn layout_absolute_auto_height_respects_min_height_with_top_inset() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.top = LengthOrAuto::px(12.0);
        style.height = LengthOrAuto::Auto;
        style.min_height = Length::px(80.0);

        let mut input = AbsoluteLayoutInput::new(style, Size::new(0.0, 0.0), Point::ZERO);
        input.preferred_min_block_size = Some(10.0);
        input.preferred_block_size = Some(30.0); // would shrink to 30 without min-height clamp
        let cb = create_containing_block(200.0, 150.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.height - 80.0).abs() < 0.001,
            "min-height should clamp auto height when shrink-to-fit yields smaller"
        );
        assert!(
            (result.position.y - 12.0).abs() < 0.001,
            "top inset should remain satisfied after min-height expansion"
        );
    }

    #[test]
    fn layout_absolute_auto_height_respects_max_height_in_shrink_case() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.bottom = LengthOrAuto::px(10.0);
        style.height = LengthOrAuto::Auto;
        style.max_height = Length::px(60.0);
        style.border_width = crate::geometry::EdgeOffsets::ZERO;

        let mut input = AbsoluteLayoutInput::new(style, Size::new(0.0, 0.0), Point::ZERO);
        input.preferred_min_block_size = Some(50.0);
        input.preferred_block_size = Some(120.0); // would shrink to 190 without max-height clamp
        let cb = create_containing_block(200.0, 200.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.height - 60.0).abs() < 0.001,
            "max-height should clamp auto height when shrink-to-fit exceeds cap"
        );
        assert!(
            (result.position.y - (200.0 - 10.0 - 60.0)).abs() < 0.001,
            "bottom inset should position the clamped box"
        );
    }

    #[test]
    fn layout_absolute_auto_width_respects_max_width_with_only_right_inset() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.right = LengthOrAuto::px(10.0);
        style.width = LengthOrAuto::Auto;
        style.max_width = Length::px(60.0);

        let mut input = AbsoluteLayoutInput::new(style, Size::new(0.0, 0.0), Point::ZERO);
        input.preferred_min_inline_size = Some(40.0);
        input.preferred_inline_size = Some(150.0); // would shrink to 150 without max clamp
        let cb = create_containing_block(200.0, 200.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.width - 60.0).abs() < 0.001,
            "max-width should clamp auto width when shrink-to-fit exceeds cap"
        );
        assert!(
            (result.position.x - (200.0 - 10.0 - 60.0)).abs() < 0.001,
            "right inset should still be satisfied after clamping"
        );
    }

    #[test]
    fn layout_absolute_auto_width_respects_min_width_with_only_right_inset() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.right = LengthOrAuto::px(8.0);
        style.width = LengthOrAuto::Auto;
        style.min_width = Length::px(70.0);

        let mut input = AbsoluteLayoutInput::new(style, Size::new(0.0, 0.0), Point::ZERO);
        input.preferred_min_inline_size = Some(20.0);
        input.preferred_inline_size = Some(30.0); // would shrink to 30 without min-width
        let cb = create_containing_block(120.0, 200.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.width - 70.0).abs() < 0.001,
            "min-width should clamp auto width when shrink-to-fit is smaller"
        );
        assert!(
            (result.position.x - (120.0 - 8.0 - 70.0)).abs() < 0.001,
            "right inset should remain satisfied after min-width expansion"
        );
    }

    #[test]
    fn layout_absolute_auto_width_respects_min_width_with_only_left_inset() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(14.0);
        style.width = LengthOrAuto::Auto;
        style.min_width = Length::px(90.0);

        let mut input = AbsoluteLayoutInput::new(style, Size::new(0.0, 0.0), Point::ZERO);
        input.preferred_min_inline_size = Some(20.0);
        input.preferred_inline_size = Some(30.0); // would shrink to 30 without min-width
        let cb = create_containing_block(160.0, 200.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.width - 90.0).abs() < 0.001,
            "min-width should clamp auto width when shrink-to-fit is smaller"
        );
        assert!(
            (result.position.x - 14.0).abs() < 0.001,
            "left inset should remain satisfied after min-width expansion"
        );
    }

    #[test]
    fn absolute_margin_percentages_use_containing_width() {
        let layout = AbsoluteLayout::new();
        let cb = create_containing_block(200.0, 100.0);

        let mut style = ComputedStyle::default();
        style.position = Position::Absolute;
        style.left = Some(Length::px(0.0));
        style.top = Some(Length::px(0.0));
        style.width = Some(Length::px(50.0));
        style.height = Some(Length::px(50.0));
        style.margin_left = Some(Length::percent(20.0));
        style.margin_right = Some(Length::percent(0.0));
        style.margin_top = Some(Length::percent(10.0));
        style.margin_bottom = Some(Length::percent(5.0));

        let positioned = layout.resolve_positioned_style(&style, &cb);
        let input = AbsoluteLayoutInput::new(positioned, Size::new(10.0, 10.0), Point::ZERO);
        let result = layout.layout_absolute(&input, &cb).unwrap();

        assert!(
            (result.margins.left - 40.0).abs() < 0.001,
            "margin-left should be 20% of 200px"
        );
        assert!(
            (result.margins.top - 20.0).abs() < 0.001,
            "margin-top should be 10% of 200px"
        );
        assert!(
            (result.margins.bottom - 10.0).abs() < 0.001,
            "margin-bottom should be 5% of 200px"
        );
    }

    #[test]
    fn test_layout_absolute_intrinsic_size() {
        let layout = AbsoluteLayout::new();

        let style = default_style();
        // All auto

        let input = AbsoluteLayoutInput::new(style, Size::new(150.0, 100.0), Point::ZERO);
        let cb = create_containing_block(800.0, 600.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // Should use intrinsic size
        assert_eq!(result.size.width, 150.0);
        assert_eq!(result.size.height, 100.0);
    }

    #[test]
    fn test_layout_absolute_static_position() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.width = LengthOrAuto::px(100.0);
        style.height = LengthOrAuto::px(80.0);
        // left/right/top/bottom all auto

        let input = AbsoluteLayoutInput::new(
            style,
            Size::new(50.0, 50.0),
            Point::new(75.0, 120.0), // static position
        );
        let cb = create_containing_block(800.0, 600.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // Should use static position
        assert_eq!(result.position.x, 75.0);
        assert_eq!(result.position.y, 120.0);
    }

    #[test]
    fn test_layout_absolute_with_containing_block_offset() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(10.0);
        style.top = LengthOrAuto::px(10.0);
        style.width = LengthOrAuto::px(50.0);
        style.height = LengthOrAuto::px(50.0);

        let input = AbsoluteLayoutInput::new(style, Size::new(50.0, 50.0), Point::ZERO);
        let cb = create_containing_block_at(100.0, 100.0, 400.0, 300.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // Position relative to containing block origin
        assert_eq!(result.position.x, 110.0); // 100 + 10
        assert_eq!(result.position.y, 110.0); // 100 + 10
    }

    #[test]
    fn test_layout_absolute_overconstrained_horizontal() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(10.0);
        style.width = LengthOrAuto::px(100.0);
        style.right = LengthOrAuto::px(999.0); // Should be ignored for LTR

        let input = AbsoluteLayoutInput::new(style, Size::new(50.0, 50.0), Point::ZERO);
        let cb = create_containing_block(400.0, 300.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // Uses left and width, ignores right
        assert_eq!(result.position.x, 10.0);
        assert_eq!(result.size.width, 100.0);
    }

    #[test]
    fn test_layout_absolute_overconstrained_vertical() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.top = LengthOrAuto::px(20.0);
        style.height = LengthOrAuto::px(80.0);
        style.bottom = LengthOrAuto::px(888.0); // Should be ignored

        let input = AbsoluteLayoutInput::new(style, Size::new(50.0, 50.0), Point::ZERO);
        let cb = create_containing_block(400.0, 300.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // Uses top and height, ignores bottom
        assert_eq!(result.position.y, 20.0);
        assert_eq!(result.size.height, 80.0);
    }

    #[test]
    fn test_layout_absolute_percentage_values() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::percent(10.0); // 10% of 500 = 50
        style.top = LengthOrAuto::percent(20.0); // 20% of 400 = 80
        style.width = LengthOrAuto::percent(50.0); // 50% of 500 = 250
        style.height = LengthOrAuto::percent(25.0); // 25% of 400 = 100

        let input = AbsoluteLayoutInput::new(style, Size::new(100.0, 100.0), Point::ZERO);
        let cb = create_containing_block(500.0, 400.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        assert_eq!(result.position.x, 50.0);
        assert_eq!(result.position.y, 80.0);
        assert_eq!(result.size.width, 250.0);
        assert_eq!(result.size.height, 100.0);
    }

    #[test]
    fn font_relative_offsets_use_font_metrics() {
        let font_context = FontContext::new();
        let Some(font) = font_context.get_sans_serif() else {
            return;
        };
        let Ok(metrics) = font.metrics() else { return };
        let font_size = 20.0;
        let Some(x_height) = metrics.scale(font_size).x_height else {
            return;
        };

        let mut style = default_style();
        style.position = Position::Absolute;
        style.font_family = vec![font.family.clone()];
        style.font_size = font_size;
        style.root_font_size = font_size;
        style.top = LengthOrAuto::Length(Length::new(1.0, LengthUnit::Ex));

        let input = AbsoluteLayoutInput::new(style, Size::new(10.0, 10.0), Point::ZERO);
        let cb = create_containing_block(200.0, 200.0);

        let result = AbsoluteLayout::new().layout_absolute(&input, &cb).unwrap();
        assert!((result.position.y - x_height).abs() < 1e-3);
    }

    #[test]
    fn font_size_adjust_scales_offsets() {
        let font_context = FontContext::new();
        let Some(font) = font_context.get_sans_serif() else {
            return;
        };
        let Ok(metrics) = font.metrics() else { return };
        let Some(aspect) = metrics.aspect_ratio() else { return };
        let adjust = aspect * 1.5;
        let font_size = 18.0;
        let adjusted_size = if aspect > 0.0 {
            font_size * (adjust / aspect)
        } else {
            font_size
        };
        let Some(x_height) = metrics.scale(adjusted_size).x_height else {
            return;
        };

        let mut style = default_style();
        style.position = Position::Absolute;
        style.font_family = vec![font.family.clone()];
        style.font_size = font_size;
        style.root_font_size = font_size;
        style.font_size_adjust = FontSizeAdjust::Number(adjust);
        style.top = LengthOrAuto::Length(Length::new(1.0, LengthUnit::Ex));

        let input = AbsoluteLayoutInput::new(style, Size::new(10.0, 10.0), Point::ZERO);
        let cb = create_containing_block(200.0, 200.0);

        let result = AbsoluteLayout::new().layout_absolute(&input, &cb).unwrap();
        assert!((result.position.y - x_height).abs() < 1e-3);
    }

    #[test]
    fn test_layout_absolute_auto_margins_center_horizontally() {
        let layout = AbsoluteLayout::new();
        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(50.0);
        style.right = LengthOrAuto::px(50.0);
        style.width = LengthOrAuto::px(100.0);
        style.margin_left_auto = true;
        style.margin_right_auto = true;

        let input = AbsoluteLayoutInput::new(style, Size::new(0.0, 20.0), Point::ZERO);
        let cb = create_containing_block(400.0, 200.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.position.x - 175.0).abs() < 0.001,
            "expected centered position at x=175, got {}",
            result.position.x
        );
        assert!((result.margins.left - 125.0).abs() < 0.001);
        assert!((result.margins.right - 125.0).abs() < 0.001);
    }

    #[test]
    fn test_layout_absolute_single_auto_margin_consumes_remaining_space() {
        let layout = AbsoluteLayout::new();
        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(30.0);
        style.right = LengthOrAuto::px(40.0);
        style.width = LengthOrAuto::px(100.0);
        style.margin.left = 20.0;
        style.margin_right_auto = true;

        let input = AbsoluteLayoutInput::new(style, Size::new(0.0, 20.0), Point::ZERO);
        let cb = create_containing_block(400.0, 200.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.position.x - 50.0).abs() < 0.001,
            "expected left edge at 50 (30 + 20 margin), got {}",
            result.position.x
        );
        assert!((result.margins.right - 250.0).abs() < 0.001);
    }

    #[test]
    fn test_layout_absolute_shrink_to_fit_when_width_auto_and_both_insets() {
        let layout = AbsoluteLayout::new();
        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(20.0);
        style.right = LengthOrAuto::px(20.0);
        style.width = LengthOrAuto::Auto;
        // intrinsic width is 150, available is 200 - shrink should keep 150
        let input = AbsoluteLayoutInput::new(style, Size::new(150.0, 40.0), Point::ZERO);
        let cb = create_containing_block(400.0, 200.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.width - 150.0).abs() < 0.001,
            "auto width with both insets should shrink-to-fit intrinsic width"
        );
        assert!(
            (result.position.x - 20.0).abs() < 0.001,
            "left inset should stay at 20 when margins are zero"
        );
    }

    #[test]
    fn test_layout_absolute_shrink_to_available_when_intrinsic_exceeds_space() {
        let layout = AbsoluteLayout::new();
        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(30.0);
        style.right = LengthOrAuto::px(30.0);
        style.width = LengthOrAuto::Auto;
        // intrinsic 300, available 300 -> keep 300; shrink-to-fit should not exceed
        let input = AbsoluteLayoutInput::new(style, Size::new(320.0, 60.0), Point::ZERO);
        let cb = create_containing_block(390.0, 200.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.width - 320.0).abs() < 0.001,
            "auto width should clamp to available space when intrinsic is larger"
        );
        assert!(
            (result.position.x - 30.0).abs() < 0.001,
            "left inset should remain unchanged by shrinking width"
        );
    }

    #[test]
    fn test_layout_absolute_height_auto_fills_between_insets() {
        let layout = AbsoluteLayout::new();
        let mut style = default_style();
        style.position = Position::Absolute;
        style.top = LengthOrAuto::px(10.0);
        style.bottom = LengthOrAuto::px(20.0);
        style.height = LengthOrAuto::Auto;
        let mut input = AbsoluteLayoutInput::new(style, Size::new(80.0, 120.0), Point::ZERO);
        input.preferred_min_block_size = Some(50.0);
        input.preferred_block_size = Some(200.0);
        let cb = create_containing_block(300.0, 200.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.height - 170.0).abs() < 0.001,
            "auto height should fill the space between top and bottom"
        );
        assert!(
            (result.position.y - 10.0).abs() < 0.001,
            "top inset should remain at 10 when margins are zero"
        );
    }

    #[test]
    fn test_layout_absolute_shrink_to_fit_uses_preferred_width() {
        let layout = AbsoluteLayout::new();
        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(20.0);
        style.right = LengthOrAuto::px(20.0);
        style.width = LengthOrAuto::Auto;

        let mut input = AbsoluteLayoutInput::new(style, Size::new(10.0, 20.0), Point::ZERO);
        input.preferred_min_inline_size = Some(30.0);
        input.preferred_inline_size = Some(70.0);

        // available content width = 100 - 20 - 20 = 60
        let cb = create_containing_block(100.0, 200.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.width - 60.0).abs() < 0.001,
            "shrink-to-fit width should use min(max(min, available), pref) when provided"
        );
    }

    #[test]
    fn test_layout_absolute_width_respects_min_max_constraints() {
        let layout = AbsoluteLayout::new();
        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(10.0);
        style.right = LengthOrAuto::px(10.0);
        style.width = LengthOrAuto::Auto;
        style.min_width = Length::px(90.0);
        style.max_width = Length::px(100.0);

        let mut input = AbsoluteLayoutInput::new(style, Size::new(20.0, 20.0), Point::ZERO);
        input.preferred_min_inline_size = Some(30.0);
        input.preferred_inline_size = Some(40.0);

        // available content width = 120 - 10 - 10 = 100
        let cb = create_containing_block(120.0, 200.0);
        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.width - 90.0).abs() < 0.001,
            "min-width should clamp shrink-to-fit width up to the authored minimum"
        );

        // Now cap via max-width
        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(10.0);
        style.right = LengthOrAuto::px(10.0);
        style.width = LengthOrAuto::Auto;
        style.max_width = Length::px(50.0);

        let mut input = AbsoluteLayoutInput::new(style, Size::new(20.0, 20.0), Point::ZERO);
        input.preferred_min_inline_size = Some(30.0);
        input.preferred_inline_size = Some(80.0);
        let cb = create_containing_block(200.0, 200.0);
        let result = layout.layout_absolute(&input, &cb).unwrap();
        assert!(
            (result.size.width - 50.0).abs() < 0.001,
            "max-width should clamp shrink-to-fit width down to the authored maximum"
        );
    }

    #[test]
    fn aspect_ratio_honors_min_height_after_adjustment() {
        let layout = AbsoluteLayout::new();
        let mut style = default_style();
        style.position = Position::Absolute;
        style.width = LengthOrAuto::px(100.0);
        style.height = LengthOrAuto::Auto;
        style.aspect_ratio = crate::style::types::AspectRatio::Ratio(2.0); // width/height = 2
        style.min_height = Length::px(70.0); // taller than the 50px aspect height

        let input = AbsoluteLayoutInput::new(style, Size::new(0.0, 0.0), Point::ZERO);
        let cb = create_containing_block(300.0, 300.0);
        let result = layout.layout_absolute(&input, &cb).unwrap();

        assert!(
            (result.size.height - 70.0).abs() < 0.001,
            "min-height should clamp aspect-ratio derived height"
        );
    }

    #[test]
    fn test_layout_absolute_only_left() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.left = LengthOrAuto::px(100.0);
        // width, right auto

        let input = AbsoluteLayoutInput::new(style, Size::new(150.0, 80.0), Point::ZERO);
        let cb = create_containing_block(800.0, 600.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        assert_eq!(result.position.x, 100.0);
        assert_eq!(result.size.width, 150.0); // intrinsic
    }

    #[test]
    fn test_layout_absolute_only_right() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.right = LengthOrAuto::px(75.0);
        // width, left auto

        let input = AbsoluteLayoutInput::new(style, Size::new(100.0, 50.0), Point::ZERO);
        let cb = create_containing_block(400.0, 300.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // x = 400 - 75 - 100 = 225
        assert_eq!(result.position.x, 225.0);
        assert_eq!(result.size.width, 100.0);
    }

    #[test]
    fn test_layout_absolute_only_top() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.top = LengthOrAuto::px(50.0);

        let input = AbsoluteLayoutInput::new(style, Size::new(100.0, 60.0), Point::ZERO);
        let cb = create_containing_block(800.0, 600.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        assert_eq!(result.position.y, 50.0);
        assert_eq!(result.size.height, 60.0);
    }

    #[test]
    fn test_layout_absolute_only_bottom() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.position = Position::Absolute;
        style.bottom = LengthOrAuto::px(40.0);

        let input = AbsoluteLayoutInput::new(style, Size::new(100.0, 70.0), Point::ZERO);
        let cb = create_containing_block(800.0, 500.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // y = 500 - 40 - 70 = 390
        assert_eq!(result.position.y, 390.0);
        assert_eq!(result.size.height, 70.0);
    }

    // ========== ResolvedMargins tests ==========

    #[test]
    fn test_resolved_margins_new() {
        let margins = ResolvedMargins::new(10.0, 20.0, 30.0, 40.0);
        assert_eq!(margins.top, 10.0);
        assert_eq!(margins.right, 20.0);
        assert_eq!(margins.bottom, 30.0);
        assert_eq!(margins.left, 40.0);
    }

    #[test]
    fn test_resolved_margins_zero() {
        let margins = ResolvedMargins::zero();
        assert_eq!(margins.horizontal(), 0.0);
        assert_eq!(margins.vertical(), 0.0);
    }

    #[test]
    fn test_resolved_margins_sums() {
        let margins = ResolvedMargins::new(5.0, 10.0, 15.0, 20.0);
        assert_eq!(margins.horizontal(), 30.0);
        assert_eq!(margins.vertical(), 20.0);
    }

    // ========== Helper function tests ==========

    #[test]
    fn test_resolve_offset_auto() {
        assert_eq!(
            resolve_offset(&LengthOrAuto::Auto, 100.0, Size::new(800.0, 600.0), 16.0, 16.0),
            None
        );
    }

    #[test]
    fn test_resolve_offset_px() {
        assert_eq!(
            resolve_offset(&LengthOrAuto::px(50.0), 100.0, Size::new(800.0, 600.0), 16.0, 16.0),
            Some(50.0)
        );
    }

    #[test]
    fn test_resolve_offset_percent() {
        assert_eq!(
            resolve_offset(&LengthOrAuto::percent(25.0), 200.0, Size::new(800.0, 600.0), 16.0, 16.0),
            Some(50.0)
        );
    }

    // ========== Static method tests ==========

    #[test]
    fn test_is_absolutely_positioned() {
        let mut style = default_style();
        assert!(!AbsoluteLayout::is_absolutely_positioned(&style));

        style.position = Position::Absolute;
        assert!(AbsoluteLayout::is_absolutely_positioned(&style));

        style.position = Position::Fixed;
        assert!(AbsoluteLayout::is_absolutely_positioned(&style));

        style.position = Position::Relative;
        assert!(!AbsoluteLayout::is_absolutely_positioned(&style));
    }

    #[test]
    fn test_create_fragment() {
        let layout = AbsoluteLayout::new();
        let result = AbsoluteLayoutResult {
            position: Point::new(50.0, 75.0),
            size: Size::new(100.0, 80.0),
            margins: ResolvedMargins::zero(),
        };

        let fragment = layout.create_fragment(&result, vec![]);

        assert_eq!(fragment.bounds.x(), 50.0);
        assert_eq!(fragment.bounds.y(), 75.0);
        assert_eq!(fragment.bounds.width(), 100.0);
        assert_eq!(fragment.bounds.height(), 80.0);
    }

    // ========== Edge cases ==========

    #[test]
    fn test_zero_size_containing_block() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.left = LengthOrAuto::percent(50.0);
        style.top = LengthOrAuto::percent(50.0);

        let input = AbsoluteLayoutInput::new(style, Size::new(100.0, 100.0), Point::ZERO);
        let cb = create_containing_block(0.0, 0.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // 50% of 0 = 0
        assert_eq!(result.position.x, 0.0);
        assert_eq!(result.position.y, 0.0);
    }

    #[test]
    fn test_negative_computed_size() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.left = LengthOrAuto::px(200.0);
        style.right = LengthOrAuto::px(200.0);
        // In a 300px wide CB, this would give -100px width

        let input = AbsoluteLayoutInput::new(style, Size::new(50.0, 50.0), Point::ZERO);
        let cb = create_containing_block(300.0, 300.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        // Width should shrink to the minimum (intrinsic) instead of going negative
        assert_eq!(result.size.width, 50.0);
    }

    #[test]
    fn test_large_values() {
        let layout = AbsoluteLayout::new();

        let mut style = default_style();
        style.left = LengthOrAuto::px(1_000_000.0);
        style.top = LengthOrAuto::px(1_000_000.0);
        style.width = LengthOrAuto::px(10_000.0);
        style.height = LengthOrAuto::px(10_000.0);

        let input = AbsoluteLayoutInput::new(style, Size::new(100.0, 100.0), Point::ZERO);
        let cb = create_containing_block(800.0, 600.0);

        let result = layout.layout_absolute(&input, &cb).unwrap();

        assert_eq!(result.position.x, 1_000_000.0);
        assert_eq!(result.position.y, 1_000_000.0);
    }
}

/// Resolve a computed style into a positioned style with pixel-resolved edges.
pub fn resolve_positioned_style(
    style: &ComputedStyle,
    containing_block: &ContainingBlock,
    viewport: Size,
    font_context: &FontContext,
) -> PositionedStyle {
    let mut resolved = PositionedStyle::default();
    resolved.position = style.position;
    resolved.left = style.left.map_or(LengthOrAuto::Auto, LengthOrAuto::Length);
    resolved.right = style.right.map_or(LengthOrAuto::Auto, LengthOrAuto::Length);
    resolved.top = style.top.map_or(LengthOrAuto::Auto, LengthOrAuto::Length);
    resolved.bottom = style.bottom.map_or(LengthOrAuto::Auto, LengthOrAuto::Length);
    resolved.width = style.width.map_or(LengthOrAuto::Auto, LengthOrAuto::Length);
    resolved.height = style.height.map_or(LengthOrAuto::Auto, LengthOrAuto::Length);
    resolved.box_sizing = style.box_sizing;
    resolved.aspect_ratio = style.aspect_ratio;
    resolved.font_family = style.font_family.clone();
    resolved.font_size = style.font_size;
    resolved.root_font_size = style.root_font_size;
    resolved.font_weight = style.font_weight.to_u16();
    resolved.font_style = match style.font_style {
        crate::style::types::FontStyle::Normal => crate::style::computed::FontStyle::Normal,
        crate::style::types::FontStyle::Italic => crate::style::computed::FontStyle::Italic,
        crate::style::types::FontStyle::Oblique(angle) => crate::style::computed::FontStyle::Oblique(angle),
    };
    resolved.font_stretch = style.font_stretch;
    resolved.font_size_adjust = style.font_size_adjust;

    let cb_width = containing_block.width();
    let inline_base = containing_block.inline_percentage_base().or(Some(cb_width));
    let block_base = containing_block.block_percentage_base();
    let resolve_len = |len: &crate::style::values::Length, base: Option<f32>| -> Option<f32> {
        if len.unit == LengthUnit::Calc {
            return len.resolve_with_context(
                base,
                viewport.width,
                viewport.height,
                style.font_size,
                style.root_font_size,
            );
        }
        if len.unit.is_percentage() {
            base.and_then(|b| len.resolve_against(b))
        } else if len.unit.is_absolute() {
            Some(len.to_px())
        } else if len.unit.is_viewport_relative() {
            len.resolve_with_viewport(viewport.width, viewport.height)
        } else {
            Some(crate::layout::utils::resolve_font_relative_length(
                *len,
                style,
                font_context,
            ))
        }
    };

    resolved.padding.left = resolve_len(&style.padding_left, inline_base).unwrap_or(0.0);
    resolved.padding.right = resolve_len(&style.padding_right, inline_base).unwrap_or(0.0);
    resolved.padding.top = resolve_len(&style.padding_top, inline_base).unwrap_or(0.0);
    resolved.padding.bottom = resolve_len(&style.padding_bottom, inline_base).unwrap_or(0.0);

    resolved.border_width.left = resolve_len(&style.border_left_width, inline_base).unwrap_or(0.0);
    resolved.border_width.right = resolve_len(&style.border_right_width, inline_base).unwrap_or(0.0);
    resolved.border_width.top = resolve_len(&style.border_top_width, inline_base).unwrap_or(0.0);
    resolved.border_width.bottom = resolve_len(&style.border_bottom_width, inline_base).unwrap_or(0.0);

    resolved.min_width = Length::px(
        style
            .min_width
            .as_ref()
            .and_then(|l| resolve_len(l, inline_base))
            .unwrap_or(0.0),
    );
    resolved.max_width = Length::px(
        style
            .max_width
            .as_ref()
            .and_then(|l| resolve_len(l, inline_base))
            .unwrap_or(f32::INFINITY),
    );
    resolved.min_height = Length::px(
        style
            .min_height
            .as_ref()
            .and_then(|l| resolve_len(l, block_base))
            .unwrap_or(0.0),
    );
    resolved.max_height = Length::px(
        style
            .max_height
            .as_ref()
            .and_then(|l| resolve_len(l, block_base))
            .unwrap_or(f32::INFINITY),
    );

    resolved.margin_left_auto = style.margin_left.is_none();
    resolved.margin_right_auto = style.margin_right.is_none();
    resolved.margin_top_auto = style.margin_top.is_none();
    resolved.margin_bottom_auto = style.margin_bottom.is_none();

    resolved.margin.left = style
        .margin_left
        .as_ref()
        .and_then(|l| resolve_len(l, inline_base))
        .unwrap_or(0.0);
    resolved.margin.right = style
        .margin_right
        .as_ref()
        .and_then(|l| resolve_len(l, inline_base))
        .unwrap_or(0.0);
    // CSS 2.1 §8.3: percentage margins resolve against the containing block width on both axes.
    resolved.margin.top = style
        .margin_top
        .as_ref()
        .and_then(|l| resolve_len(l, inline_base))
        .unwrap_or(0.0);
    resolved.margin.bottom = style
        .margin_bottom
        .as_ref()
        .and_then(|l| resolve_len(l, inline_base))
        .unwrap_or(0.0);

    resolved
}
