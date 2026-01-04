use crate::style::types::TextUnderlinePosition;
use crate::style::types::WritingMode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnderlineSide {
  Left,
  Right,
}

pub(crate) fn resolve_underline_side(
  writing_mode: WritingMode,
  underline_position: TextUnderlinePosition,
) -> UnderlineSide {
  match underline_position {
    TextUnderlinePosition::Left | TextUnderlinePosition::UnderLeft => UnderlineSide::Left,
    TextUnderlinePosition::Right | TextUnderlinePosition::UnderRight => UnderlineSide::Right,
    TextUnderlinePosition::Auto
    | TextUnderlinePosition::FromFont
    | TextUnderlinePosition::Under => match writing_mode {
      WritingMode::VerticalRl | WritingMode::SidewaysRl => UnderlineSide::Right,
      WritingMode::VerticalLr | WritingMode::SidewaysLr => UnderlineSide::Left,
      WritingMode::HorizontalTb => UnderlineSide::Right,
    },
  }
}
