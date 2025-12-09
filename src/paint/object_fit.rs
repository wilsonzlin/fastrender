//! Object-fit and object-position utilities shared by paint and display list building.
use crate::style::types::{ObjectFit, ObjectPosition, PositionComponent, PositionKeyword};

/// CSS initial value for `object-position` (50% 50%).
pub fn default_object_position() -> ObjectPosition {
    ObjectPosition {
        x: PositionComponent::Keyword(PositionKeyword::Center),
        y: PositionComponent::Keyword(PositionKeyword::Center),
    }
}

/// Resolve a single position component (x or y) against the available free space.
pub fn resolve_object_position(
    comp: PositionComponent,
    free: f32,
    font_size: f32,
    viewport: Option<(f32, f32)>,
) -> f32 {
    match comp {
        PositionComponent::Keyword(PositionKeyword::Start) => 0.0,
        PositionComponent::Keyword(PositionKeyword::Center) => free * 0.5,
        PositionComponent::Keyword(PositionKeyword::End) => free,
        PositionComponent::Length(len) => {
            match () {
                _ if len.unit.is_percentage() => len.resolve_against(free),
                _ if len.unit.is_absolute() => len.to_px(),
                _ if len.unit.is_font_relative() => len.resolve_with_font_size(font_size),
                _ if len.unit.is_viewport_relative() => {
                    if let Some((vw, vh)) = viewport {
                        len.resolve_with_viewport(vw, vh)
                    } else {
                        // Fallback: treat unresolved viewport units as raw numbers to avoid panics.
                        len.value
                    }
                }
                _ => len.value,
            }
        }
        PositionComponent::Percentage(pct) => free * pct,
    }
}

/// Compute the destination rectangle for a replaced element with object-fit/object-position.
///
/// Returns the offset inside the element's box along with the used width/height.
pub fn compute_object_fit(
    fit: ObjectFit,
    position: ObjectPosition,
    box_width: f32,
    box_height: f32,
    image_width: f32,
    image_height: f32,
    font_size: f32,
    viewport: Option<(f32, f32)>,
) -> Option<(f32, f32, f32, f32)> {
    if box_width <= 0.0 || box_height <= 0.0 || image_width <= 0.0 || image_height <= 0.0 {
        return None;
    }

    let scale_x = box_width / image_width;
    let scale_y = box_height / image_height;

    let scale = match fit {
        ObjectFit::Fill => (scale_x, scale_y),
        ObjectFit::Contain => {
            let s = scale_x.min(scale_y);
            (s, s)
        }
        ObjectFit::Cover => {
            let s = scale_x.max(scale_y);
            (s, s)
        }
        ObjectFit::None => (1.0, 1.0),
        ObjectFit::ScaleDown => {
            if image_width <= box_width && image_height <= box_height {
                (1.0, 1.0)
            } else {
                let s = scale_x.min(scale_y);
                (s, s)
            }
        }
    };

    let dest_w = image_width * scale.0;
    let dest_h = image_height * scale.1;
    let free_x = box_width - dest_w;
    let free_y = box_height - dest_h;

    let offset_x = resolve_object_position(position.x, free_x, font_size, viewport);
    let offset_y = resolve_object_position(position.y, free_y, font_size, viewport);

    Some((offset_x, offset_y, dest_w, dest_h))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn contain_centers_image() {
        let fit = ObjectFit::Contain;
        let position = ObjectPosition {
            x: PositionComponent::Keyword(PositionKeyword::Center),
            y: PositionComponent::Keyword(PositionKeyword::Center),
        };

        let (offset_x, offset_y, dest_w, dest_h) = compute_object_fit(
            fit,
            position,
            200.0,
            100.0,
            100.0,
            100.0,
            16.0,
            None,
        )
        .expect("fit computed");
        assert_eq!(dest_h, 100.0);
        assert_eq!(dest_w, 100.0);
        assert!((offset_x - 50.0).abs() < 0.01);
        assert!((offset_y - 0.0).abs() < 0.01);
    }

    #[test]
    fn oversized_image_allows_negative_alignment_space() {
        let position = ObjectPosition {
            x: PositionComponent::Keyword(PositionKeyword::Center),
            y: PositionComponent::Keyword(PositionKeyword::Center),
        };

        let (offset_x, offset_y, dest_w, dest_h) = compute_object_fit(
            ObjectFit::None,
            position,
            100.0,
            100.0,
            200.0,
            50.0,
            16.0,
            None,
        )
        .expect("fit computed");
        assert_eq!(dest_w, 200.0);
        assert_eq!(dest_h, 50.0);
        // Alignment space is negative; center should shift left instead of clamping to zero.
        assert!((offset_x + 50.0).abs() < 0.01);
        assert!((offset_y - 25.0).abs() < 0.01);
    }

    #[test]
    fn percentage_position_uses_alignment_space() {
        let position = ObjectPosition {
            x: PositionComponent::Percentage(0.25),
            y: PositionComponent::Percentage(0.75),
        };

        let (offset_x, offset_y, _, _) = compute_object_fit(
            ObjectFit::Contain,
            position,
            120.0,
            60.0,
            60.0,
            60.0,
            16.0,
            None,
        )
        .expect("fit computed");
        // Contain yields dest_w=60,dest_h=60 so free_x=60, free_y=0.
        assert!((offset_x - 15.0).abs() < 0.01);
        assert!((offset_y - 0.0).abs() < 0.01);
    }

    #[test]
    fn font_relative_lengths_use_font_size() {
        let position = ObjectPosition {
            x: PositionComponent::Length(crate::style::values::Length::em(2.0)),
            y: PositionComponent::Keyword(PositionKeyword::Start),
        };

        // free_x is 50 (100 - 50). 2em at 20px font size => 40px.
        let (offset_x, offset_y, _, _) = compute_object_fit(
            ObjectFit::None,
            position,
            100.0,
            50.0,
            50.0,
            50.0,
            20.0,
            None,
        )
        .expect("fit computed");
        assert!((offset_x - 40.0).abs() < 0.01);
        assert!((offset_y - 0.0).abs() < 0.01);
    }

    #[test]
    fn viewport_units_resolve_when_available() {
        use crate::style::values::{Length, LengthUnit};
        let position = ObjectPosition {
            x: PositionComponent::Length(Length::new(10.0, LengthUnit::Vw)),
            y: PositionComponent::Keyword(PositionKeyword::Start),
        };

        // free_x = 50 (100-50). 10vw with 200px viewport => 20px offset.
        let (offset_x, offset_y, _, _) = compute_object_fit(
            ObjectFit::None,
            position,
            100.0,
            50.0,
            50.0,
            50.0,
            16.0,
            Some((200.0, 100.0)),
        )
        .expect("fit computed");
        assert!((offset_x - 20.0).abs() < 0.01);
        assert!((offset_y - 0.0).abs() < 0.01);
    }
}
