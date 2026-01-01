//! MathML parsing and layout
//!
//! Provides a minimal MathML-to-layout pipeline that parses MathML elements
//! and produces a renderable math layout with glyph fragments and simple
//! vector primitives (rules for fraction bars/radicals).

use crate::dom::{DomNode, DomNodeType, MATHML_NAMESPACE};
use crate::geometry::{Point, Rect, Size};
use crate::style::types::FontStyle as CssFontStyle;
use crate::style::types::FontWeight as CssFontWeight;
use crate::style::ComputedStyle;
use crate::text::font_db::{FontStretch, FontStyle, LoadedFont, ScaledMetrics};
use crate::text::font_loader::{FontContext, MathConstants, MathKernSide};
use crate::text::pipeline::{Direction as TextDirection, ShapedRun, ShapingPipeline};
use rustybuzz::ttf_parser;
use std::sync::Arc;

const SCRIPT_SCALE: f32 = 0.71;

/// Math variant requested by MathML.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathVariant {
  Normal,
  Bold,
  Italic,
  BoldItalic,
  DoubleStruck,
  Script,
  BoldScript,
  Fraktur,
  BoldFraktur,
  SansSerif,
  SansSerifBold,
  SansSerifItalic,
  SansSerifBoldItalic,
  Monospace,
}

impl MathVariant {
  fn is_italic(self) -> bool {
    matches!(
      self,
      MathVariant::Italic
        | MathVariant::BoldItalic
        | MathVariant::Script
        | MathVariant::BoldScript
        | MathVariant::Fraktur
        | MathVariant::BoldFraktur
        | MathVariant::SansSerifItalic
        | MathVariant::SansSerifBoldItalic
    )
  }

  fn is_bold(self) -> bool {
    matches!(
      self,
      MathVariant::Bold
        | MathVariant::BoldItalic
        | MathVariant::BoldScript
        | MathVariant::BoldFraktur
        | MathVariant::SansSerifBold
        | MathVariant::SansSerifBoldItalic
    )
  }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MathLength {
  Em(f32),
  Ex(f32),
  Px(f32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RowAlign {
  Axis,
  Baseline,
  Center,
  Top,
  Bottom,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColumnAlign {
  Left,
  Center,
  Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MencloseNotation {
  Box,
  RoundedBox,
  Circle,
  Top,
  Bottom,
  Left,
  Right,
  HorizontalStrike,
  VerticalStrike,
  UpDiagonalStrike,
  DownDiagonalStrike,
  LongDiv,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MathSize {
  Scale(f32),
  Absolute(f32),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MathStyleOverrides {
  pub display_style: Option<bool>,
  pub math_size: Option<MathSize>,
  pub math_variant: Option<MathVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MathTableCell {
  pub content: MathNode,
  pub row_align: Option<RowAlign>,
  pub column_align: Option<ColumnAlign>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MathTableRow {
  pub cells: Vec<MathTableCell>,
  pub row_align: Option<RowAlign>,
  pub column_aligns: Vec<ColumnAlign>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MathTable {
  pub rows: Vec<MathTableRow>,
  pub column_aligns: Vec<ColumnAlign>,
  pub row_aligns: Vec<RowAlign>,
}

/// Parsed MathML node
#[derive(Debug, Clone, PartialEq)]
pub enum MathNode {
  Math {
    display: bool,
    children: Vec<MathNode>,
  },
  Row(Vec<MathNode>),
  Identifier {
    text: String,
    variant: Option<MathVariant>,
  },
  Number {
    text: String,
    variant: Option<MathVariant>,
  },
  Operator {
    text: String,
    stretchy: bool,
    variant: Option<MathVariant>,
  },
  Text {
    text: String,
    variant: Option<MathVariant>,
  },
  Space {
    width: MathLength,
    height: MathLength,
    depth: MathLength,
  },
  Fraction {
    numerator: Box<MathNode>,
    denominator: Box<MathNode>,
  },
  Sqrt(Box<MathNode>),
  Root {
    radicand: Box<MathNode>,
    index: Box<MathNode>,
  },
  Superscript {
    base: Box<MathNode>,
    superscript: Box<MathNode>,
  },
  Subscript {
    base: Box<MathNode>,
    subscript: Box<MathNode>,
  },
  SubSuperscript {
    base: Box<MathNode>,
    subscript: Box<MathNode>,
    superscript: Box<MathNode>,
  },
  Over {
    base: Box<MathNode>,
    over: Box<MathNode>,
  },
  Under {
    base: Box<MathNode>,
    under: Box<MathNode>,
  },
  UnderOver {
    base: Box<MathNode>,
    under: Box<MathNode>,
    over: Box<MathNode>,
  },
  Multiscripts {
    base: Box<MathNode>,
    prescripts: Vec<(Option<MathNode>, Option<MathNode>)>,
    postscripts: Vec<(Option<MathNode>, Option<MathNode>)>,
  },
  Style {
    overrides: MathStyleOverrides,
    children: Vec<MathNode>,
  },
  Enclose {
    notation: Vec<MencloseNotation>,
    child: Box<MathNode>,
  },
  Table(MathTable),
}

/// Renderable fragment produced by math layout.
#[derive(Debug, Clone)]
pub enum MathFragment {
  Glyph { origin: Point, run: ShapedRun },
  Rule(Rect),
  StrokeRect { rect: Rect, radius: f32, width: f32 },
}

#[derive(Debug, Clone, Default)]
pub struct MathLayoutAnnotations {
  /// Metadata about the trailing glyph in this layout, used for script positioning.
  pub trailing_glyph: Option<MathGlyph>,
}

impl MathLayoutAnnotations {
  fn merge_trailing(&self, other: &MathLayoutAnnotations) -> MathLayoutAnnotations {
    if other.trailing_glyph.is_some() {
      other.clone()
    } else {
      self.clone()
    }
  }
}

#[derive(Debug, Clone)]
pub struct MathGlyph {
  pub font: Arc<LoadedFont>,
  pub glyph_id: u16,
  pub font_size: f32,
  pub italic_correction: f32,
}

/// Final math layout with positioned fragments.
#[derive(Debug, Clone)]
pub struct MathLayout {
  pub width: f32,
  pub height: f32,
  pub baseline: f32,
  pub fragments: Vec<MathFragment>,
  pub annotations: MathLayoutAnnotations,
}

impl MathFragment {
  fn translate(self, offset: Point) -> Self {
    match self {
      MathFragment::Glyph { origin, run } => MathFragment::Glyph {
        origin: Point::new(origin.x + offset.x, origin.y + offset.y),
        run,
      },
      MathFragment::Rule(rect) => MathFragment::Rule(rect.translate(offset)),
      MathFragment::StrokeRect {
        rect,
        radius,
        width,
      } => MathFragment::StrokeRect {
        rect: rect.translate(offset),
        radius,
        width,
      },
    }
  }
}

impl MathLayout {
  pub fn size(&self) -> Size {
    Size::new(self.width, self.height)
  }
}

/// Internal layout style carrying math-specific sizing state.
#[derive(Debug, Clone, Copy)]
struct MathStyle {
  font_size: f32,
  display_style: bool,
  default_variant: Option<MathVariant>,
  script_level: u8,
}

impl MathStyle {
  fn from_computed(style: &ComputedStyle) -> Self {
    Self {
      font_size: style.font_size,
      display_style: style.display.is_block_level(),
      default_variant: None,
      script_level: 0,
    }
  }

  fn script_with_constants(&self, constants: Option<&MathConstants>) -> Self {
    let scale = if self.script_level == 0 {
      constants
        .and_then(|c| c.script_percent_scale_down)
        .unwrap_or(SCRIPT_SCALE)
    } else {
      constants
        .and_then(|c| c.script_script_percent_scale_down)
        .unwrap_or(SCRIPT_SCALE)
    };
    let mut size = self.font_size * scale;
    if size < 6.0 {
      size = 6.0;
    }
    Self {
      font_size: size,
      display_style: false,
      default_variant: self.default_variant,
      script_level: self.script_level.saturating_add(1),
    }
  }
}

fn normalized_text(node: &DomNode, preserve_space: bool) -> Option<String> {
  let mut buf = String::new();
  collect_text(node, &mut buf);
  if preserve_space {
    if buf.is_empty() {
      None
    } else {
      Some(buf)
    }
  } else {
    let trimmed = buf.trim();
    if trimmed.is_empty() {
      None
    } else {
      Some(trimmed.to_string())
    }
  }
}

fn collect_text(node: &DomNode, out: &mut String) {
  match &node.node_type {
    DomNodeType::Text { content } => out.push_str(content),
    DomNodeType::Element { .. }
    | DomNodeType::Slot { .. }
    | DomNodeType::Document { .. }
    | DomNodeType::ShadowRoot { .. } => {
      for child in node.children.iter() {
        collect_text(child, out);
      }
    }
  }
}

fn parse_mathvariant(node: &DomNode) -> Option<MathVariant> {
  let value = node.get_attribute_ref("mathvariant")?;
  match value.to_ascii_lowercase().as_str() {
    "normal" | "upright" => Some(MathVariant::Normal),
    "bold" => Some(MathVariant::Bold),
    "italic" | "oblique" => Some(MathVariant::Italic),
    "bold-italic" | "bold-oblique" => Some(MathVariant::BoldItalic),
    "double-struck" | "doublestruck" => Some(MathVariant::DoubleStruck),
    "script" => Some(MathVariant::Script),
    "bold-script" | "boldscript" => Some(MathVariant::BoldScript),
    "fraktur" => Some(MathVariant::Fraktur),
    "bold-fraktur" | "boldfraktur" => Some(MathVariant::BoldFraktur),
    "sans-serif" | "sansserif" => Some(MathVariant::SansSerif),
    "sans-serif-bold" | "bold-sans-serif" | "boldsansserif" => Some(MathVariant::SansSerifBold),
    "sans-serif-italic" | "sans-serif-oblique" | "sansserifitalic" | "sansserifoblique" => {
      Some(MathVariant::SansSerifItalic)
    }
    "sans-serif-bold-italic"
    | "bold-sans-serif-italic"
    | "sans-serif-bold-oblique"
    | "bold-sans-serif-oblique"
    | "boldsansserifitalic" => Some(MathVariant::SansSerifBoldItalic),
    "monospace" | "typewriter" => Some(MathVariant::Monospace),
    _ => None,
  }
}

fn parse_math_length(raw: Option<&str>) -> Option<MathLength> {
  let value = raw?.trim();
  if value.is_empty() {
    return None;
  }
  if let Some(v) = value.strip_suffix("ex") {
    return v.trim().parse::<f32>().ok().map(MathLength::Ex);
  }
  if let Some(v) = value.strip_suffix("em") {
    return v.trim().parse::<f32>().ok().map(MathLength::Em);
  }
  if let Some(v) = value.strip_suffix("px") {
    return v.trim().parse::<f32>().ok().map(MathLength::Px);
  }
  value.parse::<f32>().ok().map(MathLength::Em)
}

fn parse_math_size(raw: &str) -> Option<MathSize> {
  match raw.trim().to_ascii_lowercase().as_str() {
    "small" => Some(MathSize::Scale(0.8)),
    "normal" => Some(MathSize::Scale(1.0)),
    "big" => Some(MathSize::Scale(1.2)),
    other => {
      if let Some(v) = other.strip_suffix('%') {
        if let Ok(pct) = v.trim().parse::<f32>() {
          return Some(MathSize::Scale(pct / 100.0));
        }
      }
      if let Some(v) = other.strip_suffix("px") {
        return v.trim().parse::<f32>().ok().map(MathSize::Absolute);
      }
      if let Some(v) = other.strip_suffix("em") {
        return v.trim().parse::<f32>().ok().map(|v| MathSize::Scale(v));
      }
      if let Ok(val) = other.parse::<f32>() {
        Some(MathSize::Scale(val))
      } else {
        None
      }
    }
  }
}

fn parse_display_style(value: Option<&str>) -> Option<bool> {
  let raw = value?.trim();
  if raw.is_empty() {
    return None;
  }
  if raw.eq_ignore_ascii_case("true") || raw == "1" {
    Some(true)
  } else if raw.eq_ignore_ascii_case("false") || raw == "0" {
    Some(false)
  } else {
    None
  }
}

fn parse_row_align_list(value: Option<&str>) -> Vec<RowAlign> {
  value
    .map(|v| {
      v.split(|c| c == ' ' || c == ',')
        .filter_map(|item| match item.trim().to_ascii_lowercase().as_str() {
          "axis" => Some(RowAlign::Axis),
          "top" => Some(RowAlign::Top),
          "bottom" => Some(RowAlign::Bottom),
          "center" | "centre" | "middle" => Some(RowAlign::Center),
          "baseline" => Some(RowAlign::Baseline),
          _ => None,
        })
        .collect()
    })
    .unwrap_or_default()
}

fn parse_column_align_list(value: Option<&str>) -> Vec<ColumnAlign> {
  value
    .map(|v| {
      v.split(|c| c == ' ' || c == ',')
        .filter_map(|item| match item.trim().to_ascii_lowercase().as_str() {
          "left" => Some(ColumnAlign::Left),
          "center" | "centre" => Some(ColumnAlign::Center),
          "right" => Some(ColumnAlign::Right),
          _ => None,
        })
        .collect()
    })
    .unwrap_or_default()
}

fn parse_menclose_notation(value: Option<&str>) -> Vec<MencloseNotation> {
  let Some(raw) = value else {
    return vec![MencloseNotation::Box];
  };
  let parsed: Vec<MencloseNotation> = raw
    .split(|c| c == ' ' || c == ',')
    .filter_map(|item| match item.trim().to_ascii_lowercase().as_str() {
      "box" => Some(MencloseNotation::Box),
      "roundedbox" => Some(MencloseNotation::RoundedBox),
      "circle" => Some(MencloseNotation::Circle),
      "top" => Some(MencloseNotation::Top),
      "bottom" => Some(MencloseNotation::Bottom),
      "left" => Some(MencloseNotation::Left),
      "right" => Some(MencloseNotation::Right),
      "horizontalstrike" => Some(MencloseNotation::HorizontalStrike),
      "verticalstrike" => Some(MencloseNotation::VerticalStrike),
      "updiagonalstrike" => Some(MencloseNotation::UpDiagonalStrike),
      "downdiagonalstrike" => Some(MencloseNotation::DownDiagonalStrike),
      "longdiv" => Some(MencloseNotation::LongDiv),
      _ => None,
    })
    .collect();
  if parsed.is_empty() {
    vec![MencloseNotation::Box]
  } else {
    parsed
  }
}

fn parse_mstyle_overrides(node: &DomNode) -> MathStyleOverrides {
  MathStyleOverrides {
    display_style: parse_display_style(node.get_attribute_ref("displaystyle")),
    math_size: node
      .get_attribute_ref("mathsize")
      .and_then(|v| parse_math_size(v)),
    math_variant: parse_mathvariant(node),
  }
}

fn repeating_value<T: Copy>(values: &[T], index: usize) -> Option<T> {
  if values.is_empty() {
    None
  } else {
    Some(*values.get(index).unwrap_or_else(|| values.last().unwrap()))
  }
}

fn wrap_row_or_single(mut children: Vec<MathNode>) -> Option<MathNode> {
  if children.is_empty() {
    None
  } else if children.len() == 1 {
    Some(children.remove(0))
  } else {
    Some(MathNode::Row(children))
  }
}

fn parse_children(node: &DomNode) -> Vec<MathNode> {
  node.children.iter().filter_map(parse_mathml).collect()
}

fn is_annotation_tag(tag: &str) -> bool {
  matches!(tag, "annotation" | "annotation-xml")
}

fn parse_scripts(children: &[DomNode]) -> Vec<(Option<MathNode>, Option<MathNode>)> {
  let mut pairs = Vec::new();
  let mut idx = 0;
  while idx < children.len() {
    if let Some(tag) = children[idx].tag_name() {
      if tag.eq_ignore_ascii_case("mprescripts") {
        idx += 1;
        continue;
      }
    }
    let sub = children.get(idx).and_then(parse_mathml);
    let sup = children.get(idx + 1).and_then(parse_mathml);
    pairs.push((sub, sup));
    idx += 2;
  }
  pairs
}

fn empty_text_node() -> MathNode {
  MathNode::Text {
    text: String::new(),
    variant: None,
  }
}

/// Parse a DomNode subtree into a MathNode tree.
pub fn parse_mathml(node: &DomNode) -> Option<MathNode> {
  match &node.node_type {
    DomNodeType::Text { content } => {
      let trimmed = content.trim();
      if trimmed.is_empty() {
        None
      } else {
        Some(MathNode::Text {
          text: trimmed.to_string(),
          variant: None,
        })
      }
    }
    DomNodeType::Slot { .. } | DomNodeType::ShadowRoot { .. } | DomNodeType::Document { .. } => {
      wrap_row_or_single(parse_children(node))
    }
    DomNodeType::Element {
      tag_name,
      namespace,
      ..
    } => {
      let tag = tag_name.to_ascii_lowercase();
      let in_math_ns = namespace == MATHML_NAMESPACE;
      match tag.as_str() {
        "annotation" | "annotation-xml" => None,
        "semantics" => {
          let mut first_child = None;
          for child in node.children.iter() {
            match &child.node_type {
              DomNodeType::Element { tag_name, .. } => {
                if is_annotation_tag(&tag_name.to_ascii_lowercase()) {
                  continue;
                }
              }
              DomNodeType::Text { content } => {
                if content.trim().is_empty() {
                  continue;
                }
              }
              _ => {}
            }
            first_child = Some(child);
            break;
          }
          first_child.and_then(parse_mathml)
        }
        "math" if in_math_ns || namespace.is_empty() => {
          let display = node
            .get_attribute_ref("display")
            .map(|v| v.eq_ignore_ascii_case("block"))
            .unwrap_or(false);
          let children = parse_children(node);
          Some(MathNode::Math { display, children })
        }
        "none" => None,
        "mrow" => Some(MathNode::Row(parse_children(node))),
        "mi" => normalized_text(node, false).map(|text| MathNode::Identifier {
          text,
          variant: parse_mathvariant(node),
        }),
        "mn" => normalized_text(node, false).map(|text| MathNode::Number {
          text,
          variant: parse_mathvariant(node),
        }),
        "mo" => normalized_text(node, false).map(|text| {
          let stretchy = node
            .get_attribute_ref("stretchy")
            .map(|v| !v.eq_ignore_ascii_case("false"))
            .unwrap_or(true);
          MathNode::Operator {
            text,
            stretchy,
            variant: parse_mathvariant(node),
          }
        }),
        "ms" | "mtext" => normalized_text(node, true).map(|text| MathNode::Text {
          text,
          variant: parse_mathvariant(node),
        }),
        "mspace" => Some(MathNode::Space {
          width: parse_math_length(node.get_attribute_ref("width")).unwrap_or(MathLength::Em(0.0)),
          height: parse_math_length(node.get_attribute_ref("height"))
            .unwrap_or(MathLength::Em(0.0)),
          depth: parse_math_length(node.get_attribute_ref("depth")).unwrap_or(MathLength::Em(0.0)),
        }),
        "mstyle" => Some(MathNode::Style {
          overrides: parse_mstyle_overrides(node),
          children: parse_children(node),
        }),
        "merror" => wrap_row_or_single(parse_children(node)),
        "mfrac" => {
          let mut children = parse_children(node).into_iter();
          let num = children.next().unwrap_or_else(empty_text_node);
          let den = children.next().unwrap_or_else(empty_text_node);
          Some(MathNode::Fraction {
            numerator: Box::new(num),
            denominator: Box::new(den),
          })
        }
        "msqrt" => {
          let mut children = parse_children(node);
          let child = match children.len() {
            0 => empty_text_node(),
            1 => children.remove(0),
            _ => MathNode::Row(children),
          };
          Some(MathNode::Sqrt(Box::new(child)))
        }
        "mroot" => {
          let mut children = parse_children(node).into_iter();
          let radicand = children.next().unwrap_or_else(empty_text_node);
          let index = children.next().unwrap_or_else(empty_text_node);
          Some(MathNode::Root {
            radicand: Box::new(radicand),
            index: Box::new(index),
          })
        }
        "msup" => {
          let mut children = parse_children(node).into_iter();
          let base = children.next().unwrap_or_else(empty_text_node);
          let sup = children.next().unwrap_or_else(empty_text_node);
          Some(MathNode::Superscript {
            base: Box::new(base),
            superscript: Box::new(sup),
          })
        }
        "msub" => {
          let mut children = parse_children(node).into_iter();
          let base = children.next().unwrap_or_else(empty_text_node);
          let sub = children.next().unwrap_or_else(empty_text_node);
          Some(MathNode::Subscript {
            base: Box::new(base),
            subscript: Box::new(sub),
          })
        }
        "msubsup" => {
          let mut children = parse_children(node).into_iter();
          let base = children.next().unwrap_or_else(empty_text_node);
          let sub = children.next().unwrap_or_else(empty_text_node);
          let sup = children.next().unwrap_or_else(empty_text_node);
          Some(MathNode::SubSuperscript {
            base: Box::new(base),
            subscript: Box::new(sub),
            superscript: Box::new(sup),
          })
        }
        "mover" => {
          let mut children = parse_children(node).into_iter();
          let base = children.next().unwrap_or_else(empty_text_node);
          let over = children.next().unwrap_or_else(empty_text_node);
          Some(MathNode::Over {
            base: Box::new(base),
            over: Box::new(over),
          })
        }
        "munder" => {
          let mut children = parse_children(node).into_iter();
          let base = children.next().unwrap_or_else(empty_text_node);
          let under = children.next().unwrap_or_else(empty_text_node);
          Some(MathNode::Under {
            base: Box::new(base),
            under: Box::new(under),
          })
        }
        "munderover" => {
          let mut children = parse_children(node).into_iter();
          let base = children.next().unwrap_or_else(empty_text_node);
          let under = children.next().unwrap_or_else(empty_text_node);
          let over = children.next().unwrap_or_else(empty_text_node);
          Some(MathNode::UnderOver {
            base: Box::new(base),
            under: Box::new(under),
            over: Box::new(over),
          })
        }
        "mmultiscripts" => {
          let mut iter = node.children.iter();
          let base = iter
            .next()
            .and_then(parse_mathml)
            .unwrap_or_else(empty_text_node);
          let mut pre = Vec::new();
          let mut post_nodes = Vec::new();
          let mut in_pre = false;
          for child in node.children.iter().skip(1) {
            if child
              .tag_name()
              .map(|t| t.eq_ignore_ascii_case("mprescripts"))
              .unwrap_or(false)
            {
              in_pre = true;
              continue;
            }
            if in_pre {
              pre.push(child.clone());
            } else {
              post_nodes.push(child.clone());
            }
          }
          let postscripts = parse_scripts(&post_nodes);
          let prescripts = parse_scripts(&pre);
          Some(MathNode::Multiscripts {
            base: Box::new(base),
            prescripts,
            postscripts,
          })
        }
        "mfenced" => {
          let open = node
            .get_attribute_ref("open")
            .map(|s| s.to_string())
            .unwrap_or_else(|| "(".to_string());
          let close = node
            .get_attribute_ref("close")
            .map(|s| s.to_string())
            .unwrap_or_else(|| ")".to_string());
          let separators = node
            .get_attribute_ref("separators")
            .map(|s| {
              s.chars()
                .filter(|c| *c != ' ' && *c != '\t')
                .collect::<Vec<char>>()
            })
            .filter(|v| !v.is_empty())
            .unwrap_or_else(|| vec![',']);
          let inner = parse_children(node);
          if inner.is_empty() {
            return None;
          }
          let mut row = Vec::new();
          row.push(MathNode::Operator {
            text: open,
            stretchy: true,
            variant: Some(MathVariant::Normal),
          });
          for (idx, child) in inner.into_iter().enumerate() {
            if idx > 0 {
              let sep = separators
                .get(idx - 1)
                .or_else(|| separators.last())
                .copied()
                .unwrap_or(',');
              row.push(MathNode::Operator {
                text: sep.to_string(),
                stretchy: false,
                variant: Some(MathVariant::Normal),
              });
            }
            row.push(child);
          }
          row.push(MathNode::Operator {
            text: close,
            stretchy: true,
            variant: Some(MathVariant::Normal),
          });
          Some(MathNode::Row(row))
        }
        "menclose" => {
          let notation = parse_menclose_notation(node.get_attribute_ref("notation"));
          let child = wrap_row_or_single(parse_children(node)).unwrap_or_else(empty_text_node);
          Some(MathNode::Enclose {
            notation,
            child: Box::new(child),
          })
        }
        "mtr" => Some(MathNode::Row(
          node.children.iter().filter_map(parse_mathml).collect(),
        )),
        "mtd" => Some(MathNode::Row(parse_children(node))),
        "mtable" => {
          let table_row_aligns = parse_row_align_list(node.get_attribute_ref("rowalign"));
          let table_col_aligns = parse_column_align_list(node.get_attribute_ref("columnalign"));
          let mut rows = Vec::new();
          for child in node.children.iter() {
            let Some(tag) = child.tag_name() else {
              continue;
            };
            if tag.eq_ignore_ascii_case("mtr") || tag.eq_ignore_ascii_case("mtd") {
              let row_aligns = parse_row_align_list(child.get_attribute_ref("rowalign"));
              let row_col_aligns = parse_column_align_list(child.get_attribute_ref("columnalign"));
              let mut cells = Vec::new();
              let cell_nodes: Vec<&DomNode> = if tag.eq_ignore_ascii_case("mtd") {
                vec![child]
              } else {
                child
                  .children
                  .iter()
                  .filter(|n| {
                    n.tag_name()
                      .map(|t| t.eq_ignore_ascii_case("mtd") || t.eq_ignore_ascii_case("mth"))
                      .unwrap_or(false)
                  })
                  .collect()
              };
              for cell_node in cell_nodes {
                let cell_align = cell_node
                  .get_attribute_ref("columnalign")
                  .and_then(|v| parse_column_align_list(Some(v)).into_iter().next());
                let row_align = cell_node
                  .get_attribute_ref("rowalign")
                  .and_then(|v| parse_row_align_list(Some(v)).into_iter().next());
                let content = parse_mathml(cell_node).unwrap_or_else(empty_text_node);
                cells.push(MathTableCell {
                  content,
                  row_align,
                  column_align: cell_align,
                });
              }
              rows.push(MathTableRow {
                cells,
                row_align: row_aligns.get(0).cloned(),
                column_aligns: row_col_aligns,
              });
            }
          }
          Some(MathNode::Table(MathTable {
            rows,
            column_aligns: table_col_aligns,
            row_aligns: table_row_aligns,
          }))
        }
        _ => Some(MathNode::Row(parse_children(node))),
      }
    }
  }
}

/// Layout engine for math trees.
pub struct MathLayoutContext {
  pipeline: ShapingPipeline,
  font_ctx: FontContext,
}

enum StretchOrientation {
  Vertical { target: f32 },
  Horizontal { target: f32 },
}

impl StretchOrientation {
  fn target(&self) -> f32 {
    match self {
      StretchOrientation::Vertical { target } | StretchOrientation::Horizontal { target } => {
        *target
      }
    }
  }

  fn main_dimension(&self, layout: &MathLayout) -> f32 {
    match self {
      StretchOrientation::Vertical { .. } => layout.height,
      StretchOrientation::Horizontal { .. } => layout.width,
    }
  }
}

impl MathLayoutContext {
  pub fn new(font_ctx: FontContext) -> Self {
    Self {
      pipeline: ShapingPipeline::new(),
      font_ctx,
    }
  }

  fn rule_thickness(style: &MathStyle) -> f32 {
    let base = (style.font_size * 0.06).clamp(1.0, style.font_size * 0.5);
    if style.display_style {
      base * 1.1
    } else {
      base
    }
  }

  fn axis_height(
    metrics: &ScaledMetrics,
    style: &MathStyle,
    constants: Option<&MathConstants>,
  ) -> f32 {
    if let Some(c) = constants.and_then(|c| c.axis_height) {
      return c;
    }
    metrics
      .x_height
      .unwrap_or(style.font_size * 0.5)
      .max(style.font_size * 0.2)
      * 0.5
  }

  fn script_gap(style: &MathStyle) -> f32 {
    style.font_size * if style.display_style { 0.12 } else { 0.1 }
  }

  fn frac_gap(style: &MathStyle) -> f32 {
    style.font_size * if style.display_style { 0.25 } else { 0.18 }
  }

  fn sqrt_padding(style: &MathStyle) -> f32 {
    style.font_size * if style.display_style { 0.14 } else { 0.1 }
  }

  fn table_spacing(style: &MathStyle) -> (f32, f32) {
    (style.font_size * 0.5, style.font_size * 0.25)
  }

  fn resolve_math_font(
    &self,
    base_style: &ComputedStyle,
    math_style: &MathStyle,
    variant: MathVariant,
  ) -> Option<Arc<LoadedFont>> {
    let mut style = base_style.clone();
    style.font_size = math_style.font_size;
    style.font_family = self
      .preferred_math_families_for_variant(base_style, variant)
      .into();
    style.font_style = if variant.is_italic() {
      CssFontStyle::Italic
    } else {
      CssFontStyle::Normal
    };
    if variant.is_bold() {
      style.font_weight = CssFontWeight::Bold;
    }
    let stretch = FontStretch::from_percentage(style.font_stretch.to_percentage());
    self
      .font_ctx
      .get_font_full(
        &style.font_family,
        style.font_weight.to_u16(),
        match style.font_style {
          CssFontStyle::Normal => FontStyle::Normal,
          CssFontStyle::Italic => FontStyle::Italic,
          CssFontStyle::Oblique(_) => FontStyle::Oblique,
        },
        stretch,
      )
      .map(Arc::new)
  }

  fn math_constants_for_layout(
    &self,
    layout: &MathLayout,
    style: &MathStyle,
    base_style: &ComputedStyle,
    fallback_variant: MathVariant,
  ) -> Option<MathConstants> {
    if let Some(glyph) = &layout.annotations.trailing_glyph {
      if let Some(constants) = self.font_ctx.math_constants(&glyph.font, glyph.font_size) {
        return Some(constants);
      }
    }
    self.default_math_constants(style, base_style, fallback_variant)
  }

  fn default_math_constants(
    &self,
    style: &MathStyle,
    base_style: &ComputedStyle,
    variant: MathVariant,
  ) -> Option<MathConstants> {
    let variant = style.default_variant.unwrap_or(variant);
    let font = self.resolve_math_font(base_style, style, variant)?;
    self.font_ctx.math_constants(&font, style.font_size)
  }

  fn layout_glyph_by_id(
    &self,
    font: Arc<LoadedFont>,
    glyph_id: u16,
    font_size: f32,
  ) -> Option<MathLayout> {
    let face = crate::text::face_cache::get_ttf_face(&font)?;
    let face = face.face();
    let glyph = ttf_parser::GlyphId(glyph_id);
    let metrics = font.metrics().ok()?.scale(font_size);
    let bbox = face.glyph_bounding_box(glyph);
    let advance = face
      .glyph_hor_advance(glyph)
      .map(|v| v as f32 * metrics.scale)
      .unwrap_or(0.0);
    let (ascent, descent, width) = if let Some(bbox) = bbox {
      (
        bbox.y_max as f32 * metrics.scale,
        -(bbox.y_min as f32) * metrics.scale,
        advance.max((bbox.x_max - bbox.x_min) as f32 * metrics.scale),
      )
    } else {
      (
        metrics.ascent,
        metrics.descent,
        advance.max(metrics.font_size * 0.5),
      )
    };
    let glyph_pos = crate::text::pipeline::GlyphPosition {
      glyph_id: glyph_id as u32,
      cluster: 0,
      x_offset: 0.0,
      y_offset: 0.0,
      x_advance: advance,
      y_advance: 0.0,
    };
    let run = ShapedRun {
      text: String::new(),
      start: 0,
      end: 0,
      glyphs: vec![glyph_pos],
      direction: TextDirection::LeftToRight,
      level: 0,
      advance,
      font: font.clone(),
      font_size,
      baseline_shift: 0.0,
      language: None,
      synthetic_bold: 0.0,
      synthetic_oblique: 0.0,
      rotation: crate::text::pipeline::RunRotation::None,
      palette_index: 0,
      palette_overrides: Arc::new(Vec::new()),
      palette_override_hash: 0,
      variations: Vec::new(),
      scale: 1.0,
    };
    let mut annotations = MathLayoutAnnotations::default();
    let italic_correction = self
      .font_ctx
      .math_italic_correction(&font, glyph_id, font_size)
      .unwrap_or(0.0);
    annotations.trailing_glyph = Some(MathGlyph {
      font,
      glyph_id,
      font_size,
      italic_correction,
    });
    Some(MathLayout {
      width,
      height: ascent + descent,
      baseline: ascent,
      fragments: vec![MathFragment::Glyph {
        origin: Point::new(0.0, ascent),
        run,
      }],
      annotations,
    })
  }

  fn align_stretch(
    &self,
    layout: MathLayout,
    target_ascent: f32,
    target_descent: f32,
  ) -> MathLayout {
    let desired_height = (target_ascent + target_descent).max(layout.height);
    let offset_y = target_ascent - layout.baseline + (desired_height - layout.height) * 0.5;
    let fragments = layout
      .fragments
      .into_iter()
      .map(|f| f.translate(Point::new(0.0, offset_y)))
      .collect();
    MathLayout {
      baseline: target_ascent,
      height: desired_height,
      width: layout.width,
      fragments,
      annotations: layout.annotations,
    }
  }

  fn build_glyph_construction(
    &self,
    font: Arc<LoadedFont>,
    construction: ttf_parser::math::GlyphConstruction<'static>,
    min_overlap: f32,
    orientation: StretchOrientation,
    font_size: f32,
  ) -> Option<MathLayout> {
    let scale = font.metrics().ok()?.scale(font_size).scale;
    let target_main = orientation.target();
    let mut best_variant: Option<(MathLayout, f32)> = None;
    for idx in 0..(construction.variants.len() as usize) {
      let Some(var) = construction.variants.get(idx as u16) else {
        continue;
      };
      let layout = self.layout_glyph_by_id(font.clone(), var.variant_glyph.0, font_size)?;
      let layout_main = orientation.main_dimension(&layout);
      if layout_main >= target_main
        && best_variant.as_ref().map(|(_, h)| *h).unwrap_or(f32::MAX) > layout_main
      {
        best_variant = Some((layout, layout_main));
      }
    }
    if let Some((layout, _)) = best_variant {
      return Some(layout);
    }
    let Some(assembly) = construction.assembly else {
      return None;
    };
    let parts_len = assembly.parts.len() as usize;
    if parts_len == 0 {
      return None;
    }
    let mut parts: Vec<ttf_parser::math::GlyphPart> = Vec::new();
    for idx in 0..parts_len {
      if let Some(part) = assembly.parts.get(idx as u16) {
        parts.push(part);
      }
    }
    let mut extender: Option<ttf_parser::math::GlyphPart> = None;
    let mut base_advance: f32 = 0.0;
    for part in &parts {
      let advance = part.full_advance as f32 * scale;
      if part.part_flags.extender() && extender.is_none() {
        extender = Some(*part);
      }
      base_advance += advance;
    }
    let overlap = min_overlap;
    let base_height = base_advance - overlap * (parts.len().saturating_sub(1)) as f32;
    let extender_advance = extender
      .as_ref()
      .map(|p| (p.full_advance as f32 * scale - overlap).max(0.0))
      .unwrap_or(0.0);
    let repeat_count = if target_main > 0.0 && extender_advance > 0.0 && base_height < target_main {
      ((target_main - base_height) / extender_advance)
        .ceil()
        .max(0.0) as usize
    } else {
      0
    };
    let mut assembly_parts: Vec<ttf_parser::math::GlyphPart> = Vec::new();
    for part in &parts {
      assembly_parts.push(*part);
      if part.part_flags.extender() && repeat_count > 0 {
        for _ in 0..repeat_count {
          assembly_parts.push(*part);
        }
      }
    }
    let mut fragments = Vec::new();
    let mut max_width: f32 = 0.0;
    let mut annotations = MathLayoutAnnotations::default();
    match orientation {
      StretchOrientation::Vertical { .. } => {
        let mut laid_out_parts = Vec::new();
        for part in &assembly_parts {
          let layout = self.layout_glyph_by_id(font.clone(), part.glyph_id.0, font_size)?;
          max_width = max_width.max(layout.width);
          laid_out_parts.push((layout, *part));
        }
        let mut cursor = 0.0;
        for (idx, (layout, part)) in laid_out_parts.into_iter().enumerate() {
          let x = (max_width - layout.width) * 0.5;
          for frag in layout.fragments {
            fragments.push(frag.translate(Point::new(x, cursor)));
          }
          annotations = annotations.merge_trailing(&layout.annotations);
          if idx + 1 < assembly_parts.len() {
            cursor += (part.full_advance as f32 * scale) - overlap;
          } else {
            cursor += part.full_advance as f32 * scale;
          }
        }
        Some(MathLayout {
          width: max_width,
          height: cursor,
          baseline: cursor / 2.0,
          fragments,
          annotations,
        })
      }
      StretchOrientation::Horizontal { .. } => {
        let mut laid_out_parts = Vec::new();
        let mut baseline: f32 = 0.0;
        let mut max_height: f32 = 0.0;
        for part in &assembly_parts {
          let layout = self.layout_glyph_by_id(font.clone(), part.glyph_id.0, font_size)?;
          max_height = max_height.max(layout.height);
          baseline = baseline.max(layout.baseline);
          laid_out_parts.push((layout, *part));
        }
        let mut cursor = 0.0;
        for (idx, (layout, part)) in laid_out_parts.into_iter().enumerate() {
          let y_offset = baseline - layout.baseline;
          for frag in layout.fragments {
            fragments.push(frag.translate(Point::new(cursor, y_offset)));
          }
          annotations = annotations.merge_trailing(&layout.annotations);
          max_height = max_height.max(y_offset + layout.height);
          let advance = part.full_advance as f32 * scale;
          if idx + 1 < assembly_parts.len() {
            cursor += advance - overlap;
          } else {
            cursor += advance;
          }
        }
        Some(MathLayout {
          width: cursor,
          height: max_height,
          baseline,
          fragments,
          annotations,
        })
      }
    }
  }

  fn stretch_operator_vertical(
    &mut self,
    text: &str,
    variant: MathVariant,
    target_ascent: f32,
    target_descent: f32,
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> Option<MathLayout> {
    let required_height = target_ascent + target_descent;
    let (runs, _base_metrics) = self.shape_text(text, base_style, style, variant);
    let metrics = runs
      .get(0)
      .and_then(|run| self.font_ctx.get_scaled_metrics(&run.font, style.font_size))
      .unwrap_or_else(|| self.base_font_metrics(base_style, style.font_size));
    let Some(first_run) = runs.first() else {
      return None;
    };
    let glyph_id = first_run.glyphs.first().map(|g| g.glyph_id as u16)?;
    let font = first_run.font.clone();
    if let Some((construction, min_overlap)) =
      self
        .font_ctx
        .math_glyph_construction(&font, glyph_id, true, style.font_size)
    {
      let min_height = self
        .font_ctx
        .math_constants(&font, style.font_size)
        .and_then(|c| {
          let mut h = required_height;
          if let Some(min) = c.delimited_sub_formula_min_height {
            h = h.max(min);
          }
          if let Some(min) = c.display_operator_min_height {
            h = h.max(min);
          }
          Some(h)
        })
        .unwrap_or(required_height);
      if let Some(layout) = self.build_glyph_construction(
        font.clone(),
        construction,
        min_overlap,
        StretchOrientation::Vertical { target: min_height },
        style.font_size,
      ) {
        return Some(self.align_stretch(layout, target_ascent, target_descent));
      }
    }
    let current_height = metrics.ascent + metrics.descent;
    let factor = if current_height > 0.0 {
      (required_height / current_height).clamp(1.0, 8.0)
    } else {
      1.0
    };
    if factor > 1.01 {
      let mut stretch_style = *style;
      stretch_style.font_size *= factor;
      let resolved_variant =
        self.resolve_variant(Some(variant), &stretch_style, MathVariant::Normal);
      let layout = self.layout_glyphs(text, base_style, &stretch_style, resolved_variant);
      return Some(self.align_stretch(layout, target_ascent, target_descent));
    }
    None
  }

  fn stretch_operator_horizontal(
    &mut self,
    text: &str,
    variant: MathVariant,
    target_width: f32,
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> Option<MathLayout> {
    if target_width <= 0.0 {
      return None;
    }
    let (runs, _base_metrics) = self.shape_text(text, base_style, style, variant);
    let Some(first_run) = runs.first() else {
      return None;
    };
    let glyph_id = first_run.glyphs.first().map(|g| g.glyph_id as u16)?;
    let font = first_run.font.clone();
    if let Some((construction, min_overlap)) =
      self
        .font_ctx
        .math_glyph_construction(&font, glyph_id, false, style.font_size)
    {
      if let Some(layout) = self.build_glyph_construction(
        font.clone(),
        construction,
        min_overlap,
        StretchOrientation::Horizontal {
          target: target_width,
        },
        style.font_size,
      ) {
        if layout.width >= target_width * 0.99 {
          return Some(layout);
        }
      }
    }
    let current_width: f32 = runs.iter().map(|r| r.advance).sum();
    let factor = if current_width > 0.0 {
      (target_width / current_width).clamp(1.0, 8.0)
    } else {
      1.0
    };
    if factor > 1.01 {
      let mut stretch_style = *style;
      stretch_style.font_size *= factor;
      let resolved_variant =
        self.resolve_variant(Some(variant), &stretch_style, MathVariant::Normal);
      let layout = self.layout_glyphs(text, base_style, &stretch_style, resolved_variant);
      return Some(layout);
    }
    None
  }

  fn resolve_variant(
    &self,
    explicit: Option<MathVariant>,
    style: &MathStyle,
    fallback: MathVariant,
  ) -> MathVariant {
    explicit.or(style.default_variant).unwrap_or(fallback)
  }

  fn resolve_length(&self, len: MathLength, style: &MathStyle, metrics: &ScaledMetrics) -> f32 {
    match len {
      MathLength::Em(v) => v * style.font_size,
      MathLength::Ex(v) => v * metrics.x_height.unwrap_or(style.font_size * 0.5),
      MathLength::Px(v) => v,
    }
  }

  fn apply_style_overrides(&self, style: &MathStyle, overrides: &MathStyleOverrides) -> MathStyle {
    let mut next = *style;
    if let Some(display) = overrides.display_style {
      next.display_style = display;
    }
    if let Some(size) = overrides.math_size {
      next.font_size = match size {
        MathSize::Scale(f) => (style.font_size * f).max(1.0),
        MathSize::Absolute(px) => px.max(1.0),
      };
    }
    if let Some(variant) = overrides.math_variant {
      next.default_variant = Some(variant);
    }
    next
  }

  fn base_font_metrics(&self, style: &ComputedStyle, size: f32) -> ScaledMetrics {
    let mut clone = style.clone();
    clone.font_size = size;
    let italic = matches!(clone.font_style, CssFontStyle::Italic);
    let oblique = matches!(clone.font_style, CssFontStyle::Oblique(_));
    let stretch = FontStretch::from_percentage(clone.font_stretch.to_percentage());
    self
      .font_ctx
      .get_font_full(
        &clone.font_family,
        clone.font_weight.to_u16(),
        if italic {
          FontStyle::Italic
        } else if oblique {
          FontStyle::Oblique
        } else {
          FontStyle::Normal
        },
        stretch,
      )
      .and_then(|font| font.metrics().ok())
      .map(|m| m.scale(size))
      .unwrap_or_else(|| ScaledMetrics {
        font_size: size,
        scale: 1.0,
        ascent: size * 0.8,
        descent: size * 0.2,
        line_gap: 0.0,
        line_height: size,
        x_height: Some(size * 0.5),
        cap_height: Some(size * 0.7),
        underline_position: size * 0.05,
        underline_thickness: size * 0.05,
      })
  }

  fn preferred_math_families(&self, style: &ComputedStyle) -> Vec<String> {
    let mut families: Vec<String> = Vec::new();
    families.push("math".to_string());
    families.extend(self.font_ctx.math_family_names());
    for fam in style.font_family.iter() {
      if !families.iter().any(|f| f.eq_ignore_ascii_case(fam)) {
        families.push(fam.clone());
      }
    }
    families
  }

  fn variant_preferred_families(&self, variant: MathVariant) -> Vec<String> {
    let mut families = Vec::new();
    match variant {
      MathVariant::SansSerif
      | MathVariant::SansSerifBold
      | MathVariant::SansSerifItalic
      | MathVariant::SansSerifBoldItalic => families.push("sans-serif".to_string()),
      MathVariant::Monospace => families.push("monospace".to_string()),
      MathVariant::DoubleStruck => {
        families.push("math-doublestruck".to_string());
        families.push("double-struck".to_string());
      }
      MathVariant::Script | MathVariant::BoldScript => {
        families.push("math-script".to_string());
        families.push("script".to_string());
      }
      MathVariant::Fraktur | MathVariant::BoldFraktur => {
        families.push("math-fraktur".to_string());
        families.push("fraktur".to_string());
      }
      _ => {}
    }
    families
  }

  fn preferred_math_families_for_variant(
    &self,
    style: &ComputedStyle,
    variant: MathVariant,
  ) -> Vec<String> {
    let mut families = self.variant_preferred_families(variant);
    for fam in self.preferred_math_families(style) {
      if !families.iter().any(|f| f.eq_ignore_ascii_case(&fam)) {
        families.push(fam);
      }
    }
    families
  }

  fn shape_text(
    &mut self,
    text: &str,
    base_style: &ComputedStyle,
    math_style: &MathStyle,
    variant: MathVariant,
  ) -> (Vec<ShapedRun>, ScaledMetrics) {
    let mut style = base_style.clone();
    style.font_size = math_style.font_size;
    style.font_family = self
      .preferred_math_families_for_variant(base_style, variant)
      .into();
    style.font_style = if variant.is_italic() {
      CssFontStyle::Italic
    } else {
      CssFontStyle::Normal
    };
    if variant.is_bold() {
      style.font_weight = CssFontWeight::Bold;
    }

    let metrics = self.base_font_metrics(&style, style.font_size);
    let runs = match self.pipeline.shape_with_direction(
      text,
      &style,
      &self.font_ctx,
      TextDirection::LeftToRight,
    ) {
      Ok(mut r) => {
        crate::layout::contexts::inline::line_builder::TextItem::apply_spacing_to_runs(
          &mut r,
          text,
          style.letter_spacing,
          style.word_spacing,
        );
        r
      }
      Err(_) => Vec::new(),
    };
    (runs, metrics)
  }

  fn layout_glyphs(
    &mut self,
    text: &str,
    base_style: &ComputedStyle,
    math_style: &MathStyle,
    variant: MathVariant,
  ) -> MathLayout {
    let (runs, base_metrics) = self.shape_text(text, base_style, math_style, variant);
    if runs.is_empty() {
      let height = math_style.font_size;
      return MathLayout {
        width: math_style.font_size * text.len() as f32 * 0.6,
        height,
        baseline: height * 0.8,
        fragments: vec![],
        annotations: MathLayoutAnnotations::default(),
      };
    }

    let metrics = runs
      .get(0)
      .and_then(|run| {
        self
          .font_ctx
          .get_scaled_metrics(&run.font, math_style.font_size)
      })
      .unwrap_or(base_metrics);
    let ascent = metrics.ascent;
    let descent = metrics.descent;
    let width: f32 = runs.iter().map(|r| r.advance).sum();
    let mut fragments = Vec::new();
    let mut pen_x = 0.0;
    let mut annotations = MathLayoutAnnotations::default();
    if let Some(last_run) = runs.last() {
      if let Some(last_glyph) = last_run.glyphs.last() {
        let italic_correction = self
          .font_ctx
          .math_italic_correction(
            &last_run.font,
            last_glyph.glyph_id as u16,
            math_style.font_size,
          )
          .unwrap_or(0.0);
        annotations.trailing_glyph = Some(MathGlyph {
          font: last_run.font.clone(),
          glyph_id: last_glyph.glyph_id as u16,
          font_size: math_style.font_size,
          italic_correction,
        });
      }
    }
    for run in runs {
      let origin = Point::new(pen_x, ascent);
      pen_x += run.advance;
      fragments.push(MathFragment::Glyph { origin, run });
    }
    MathLayout {
      width,
      height: ascent + descent,
      baseline: ascent,
      fragments,
      annotations,
    }
  }

  fn layout_row(
    &mut self,
    children: &[MathNode],
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    let mut layouts = Vec::with_capacity(children.len());
    for child in children {
      layouts.push(self.layout_node(child, style, base_style));
    }
    // Stretch operators after seeing surrounding content.
    let stretchy_indices: Vec<usize> = children
      .iter()
      .enumerate()
      .filter_map(|(idx, child)| match child {
        MathNode::Operator { stretchy: true, .. } => Some(idx),
        _ => None,
      })
      .collect();
    if !stretchy_indices.is_empty() {
      let mut stretchy_mask = vec![false; layouts.len()];
      for idx in &stretchy_indices {
        if let Some(slot) = stretchy_mask.get_mut(*idx) {
          *slot = true;
        }
      }
      let mut target_ascent: f32 = 0.0;
      let mut target_descent: f32 = 0.0;
      for (idx, layout) in layouts.iter().enumerate() {
        if stretchy_mask.get(idx).copied().unwrap_or(false) {
          continue;
        }
        target_ascent = target_ascent.max(layout.baseline);
        target_descent = target_descent.max(layout.height - layout.baseline);
      }
      if target_ascent == 0.0 && target_descent == 0.0 {
        for layout in &layouts {
          target_ascent = target_ascent.max(layout.baseline);
          target_descent = target_descent.max(layout.height - layout.baseline);
        }
      }
      let pad = Self::rule_thickness(style) * 0.5;
      target_ascent += pad;
      target_descent += pad;
      if target_ascent == 0.0 && target_descent == 0.0 {
        target_ascent = style.font_size * 0.8;
        target_descent = style.font_size * 0.2;
      }

      for idx in stretchy_indices {
        if let MathNode::Operator { text, variant, .. } = &children[idx] {
          let resolved_variant = self.resolve_variant(*variant, style, MathVariant::Normal);
          if let Some(layout) = self.stretch_operator_vertical(
            text,
            resolved_variant,
            target_ascent,
            target_descent,
            style,
            base_style,
          ) {
            layouts[idx] = layout;
          }
        }
      }
    }
    if layouts.is_empty() {
      return self.layout_glyphs("", base_style, style, MathVariant::Normal);
    }

    let mut max_ascent: f32 = 0.0;
    let mut max_descent: f32 = 0.0;
    for layout in &layouts {
      max_ascent = max_ascent.max(layout.baseline);
      max_descent = max_descent.max(layout.height - layout.baseline);
    }
    let baseline = max_ascent;
    let mut x = 0.0;
    let mut fragments = Vec::new();
    let trailing_annotations = layouts
      .last()
      .map(|l| l.annotations.clone())
      .unwrap_or_default();
    for layout in layouts {
      let y = baseline - layout.baseline;
      for frag in layout.fragments {
        fragments.push(frag.translate(Point::new(x, y)));
      }
      x += layout.width;
    }
    MathLayout {
      width: x,
      height: baseline + max_descent,
      baseline,
      fragments,
      annotations: trailing_annotations,
    }
  }

  fn layout_space(
    &mut self,
    width: MathLength,
    height: MathLength,
    depth: MathLength,
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    let metrics = self.base_font_metrics(base_style, style.font_size);
    let w = self.resolve_length(width, style, &metrics);
    let h = self.resolve_length(height, style, &metrics);
    let d = self.resolve_length(depth, style, &metrics);
    let total_h = h + d;
    let (height, baseline) = if total_h > 0.0 {
      (total_h, h)
    } else {
      (metrics.line_height, metrics.ascent)
    };
    MathLayout {
      width: w.max(0.0),
      height: height.max(0.0),
      baseline,
      fragments: Vec::new(),
      annotations: MathLayoutAnnotations::default(),
    }
  }

  fn layout_fraction(
    &mut self,
    num: &MathNode,
    den: &MathNode,
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    let metrics = self.base_font_metrics(base_style, style.font_size);
    let constants = self.default_math_constants(style, base_style, MathVariant::Normal);
    let axis = Self::axis_height(&metrics, style, constants.as_ref());
    let script_style = if style.display_style {
      *style
    } else {
      style.script_with_constants(constants.as_ref())
    };
    let numerator = self.layout_node(num, &script_style, base_style);
    let denominator = self.layout_node(den, &script_style, base_style);
    let annotations = numerator
      .annotations
      .merge_trailing(&denominator.annotations);
    let width = numerator.width.max(denominator.width);
    let rule = constants
      .as_ref()
      .and_then(|c| c.fraction_rule_thickness)
      .unwrap_or_else(|| Self::rule_thickness(style));
    let gap = if style.display_style {
      constants
        .as_ref()
        .and_then(|c| c.fraction_num_display_style_gap_min)
        .unwrap_or_else(|| Self::frac_gap(style))
    } else {
      constants
        .as_ref()
        .and_then(|c| c.fraction_numerator_gap_min)
        .unwrap_or_else(|| Self::frac_gap(style))
    };
    let baseline = (numerator.height + gap + rule * 0.5).max(axis + gap + rule * 0.5);
    let mut fragments = Vec::new();
    let num_x = (width - numerator.width) / 2.0;
    let den_x = (width - denominator.width) / 2.0;
    let num_y = baseline - rule * 0.5 - gap - numerator.baseline;
    let den_y = baseline + rule * 0.5 + gap - denominator.baseline;

    for frag in numerator.fragments {
      fragments.push(frag.translate(Point::new(num_x, num_y)));
    }
    for frag in denominator.fragments {
      fragments.push(frag.translate(Point::new(den_x, den_y)));
    }

    fragments.push(MathFragment::Rule(Rect::from_xywh(
      0.0,
      baseline - rule * 0.5,
      width,
      rule,
    )));

    let height = (den_y + denominator.height)
      .max(num_y + numerator.height)
      .max(baseline + rule * 0.5);
    MathLayout {
      width,
      height,
      baseline,
      fragments,
      annotations,
    }
  }

  fn layout_superscript(
    &mut self,
    base: &MathNode,
    sup: Option<&MathNode>,
    sub: Option<&MathNode>,
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    let base_layout = self.layout_node(base, style, base_style);
    let constants =
      self.math_constants_for_layout(&base_layout, style, base_style, MathVariant::Normal);
    let script_style = style.script_with_constants(constants.as_ref());
    let sup_layout = sup.map(|n| self.layout_node(n, &script_style, base_style));
    let sub_layout = sub.map(|n| self.layout_node(n, &script_style, base_style));
    let base_metrics = self.base_font_metrics(base_style, style.font_size);
    let x_height = base_metrics.x_height.unwrap_or(style.font_size * 0.5);
    let sup_shift = constants
      .as_ref()
      .and_then(|c| {
        if sub.is_some() {
          c.superscript_shift_up_cramped
        } else {
          c.superscript_shift_up
        }
      })
      .unwrap_or_else(|| {
        (base_metrics.ascent * 0.6)
          .max(x_height * 0.65)
          .max(style.font_size * if style.display_style { 0.4 } else { 0.34 })
      });
    let sub_shift = constants
      .as_ref()
      .and_then(|c| c.subscript_shift_down)
      .unwrap_or_else(|| (base_metrics.descent * 0.8 + x_height * 0.2).max(style.font_size * 0.24));
    let min_gap = constants
      .as_ref()
      .and_then(|c| c.sub_superscript_gap_min)
      .unwrap_or_else(|| {
        (Self::script_gap(style) + Self::rule_thickness(style)).max(style.font_size * 0.06)
      });
    let sup_bottom_max_with_sub = constants
      .as_ref()
      .and_then(|c| c.superscript_bottom_max_with_subscript);
    let sub_baseline_drop_min = constants
      .as_ref()
      .and_then(|c| c.subscript_baseline_drop_min);

    let mut width = base_layout.width;
    let mut fragments = Vec::new();
    let mut max_ascent = base_layout.baseline;
    let mut max_descent = base_layout.height - base_layout.baseline;

    let mut script_width: f32 = 0.0;
    if let Some(layout) = &sup_layout {
      script_width = script_width.max(layout.width);
    }
    if let Some(layout) = &sub_layout {
      script_width = script_width.max(layout.width);
    }
    if script_width > 0.0 {
      width += constants
        .as_ref()
        .and_then(|c| c.space_after_script)
        .unwrap_or_else(|| Self::script_gap(style))
        + script_width;
    }
    let mut max_width = width;

    // Base fragments
    for frag in base_layout.fragments {
      fragments.push(frag);
    }

    let gap = if script_width > 0.0 {
      constants
        .as_ref()
        .and_then(|c| c.space_after_script)
        .unwrap_or_else(|| Self::script_gap(style))
    } else {
      0.0
    };
    let x = base_layout.width + gap;
    let base_descent = base_layout.height - base_layout.baseline;
    let italic_correction = base_layout
      .annotations
      .trailing_glyph
      .as_ref()
      .map(|g| g.italic_correction)
      .unwrap_or(0.0);
    let mut trailing_annotations = base_layout.annotations.clone();
    let mut sup_y = None;
    if let Some(layout) = sup_layout {
      let mut y = base_layout.baseline - sup_shift - layout.baseline;
      if let Some(bottom_min) = constants.as_ref().and_then(|c| c.superscript_bottom_min) {
        let sup_bottom = y + layout.height - layout.baseline;
        let allowed = base_layout.baseline - bottom_min;
        if sup_bottom > allowed {
          y -= sup_bottom - allowed;
        }
      }
      if let (Some(limit), true) = (sup_bottom_max_with_sub, sub.is_some()) {
        let sup_bottom = y + layout.height - layout.baseline;
        let allowed = base_layout.baseline - limit;
        if sup_bottom > allowed {
          y -= sup_bottom - allowed;
        }
      }
      let sup_kern = base_layout
        .annotations
        .trailing_glyph
        .as_ref()
        .map(|g| {
          self.font_ctx.math_kern(
            &g.font,
            g.glyph_id,
            layout.baseline,
            g.font_size,
            true,
            MathKernSide::Right,
          )
        })
        .unwrap_or(0.0);
      let sup_x = x + italic_correction + sup_kern;
      for frag in layout.fragments {
        fragments.push(frag.translate(Point::new(sup_x, y)));
      }
      max_width = max_width.max(sup_x + layout.width);
      max_ascent = max_ascent.max(layout.baseline - y);
      max_descent = max_descent.max(layout.height - (layout.baseline - y));
      sup_y = Some((y, layout.height));
      trailing_annotations = trailing_annotations.merge_trailing(&layout.annotations);
    }

    if let Some(layout) = sub_layout {
      let mut y = base_layout.baseline + base_descent + sub_shift - layout.baseline;
      if let Some((sup_y, sup_h)) = sup_y {
        let sup_bottom = sup_y + sup_h;
        let gap = y - sup_bottom;
        if gap < min_gap {
          y += min_gap - gap;
        }
      }
      if let Some(top_max) = constants.as_ref().and_then(|c| c.subscript_top_max) {
        let sub_top = y + layout.baseline;
        let min_top = base_layout.baseline + top_max;
        if sub_top < min_top {
          y += min_top - sub_top;
        }
      }
      if let Some(min_drop) = sub_baseline_drop_min {
        let drop = y + layout.baseline - base_layout.baseline;
        if drop < min_drop {
          y += min_drop - drop;
        }
      }
      let sub_kern = base_layout
        .annotations
        .trailing_glyph
        .as_ref()
        .map(|g| {
          self.font_ctx.math_kern(
            &g.font,
            g.glyph_id,
            layout.height - layout.baseline,
            g.font_size,
            false,
            MathKernSide::Right,
          )
        })
        .unwrap_or(0.0);
      let sub_x = x + sub_kern;
      for frag in layout.fragments {
        fragments.push(frag.translate(Point::new(sub_x, y)));
      }
      max_width = max_width.max(sub_x + layout.width);
      max_ascent = max_ascent.max(layout.baseline - y);
      max_descent = max_descent.max(layout.height - (layout.baseline - y));
      trailing_annotations = trailing_annotations.merge_trailing(&layout.annotations);
    }

    MathLayout {
      width: max_width,
      height: max_ascent + max_descent,
      baseline: max_ascent,
      fragments,
      annotations: trailing_annotations,
    }
  }

  fn layout_under_over(
    &mut self,
    base: &MathNode,
    under: Option<&MathNode>,
    over: Option<&MathNode>,
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    let base_layout = self.layout_node(base, style, base_style);
    let constants =
      self.math_constants_for_layout(&base_layout, style, base_style, MathVariant::Normal);
    let script_style = if style.display_style {
      *style
    } else {
      style.script_with_constants(constants.as_ref())
    };
    let stretch_target = base_layout.width + Self::rule_thickness(style);
    let under_layout = under.map(|n| match n {
      MathNode::Operator {
        text,
        stretchy: true,
        variant,
      } => {
        let resolved = self.resolve_variant(*variant, &script_style, MathVariant::Normal);
        self
          .stretch_operator_horizontal(text, resolved, stretch_target, &script_style, base_style)
          .unwrap_or_else(|| self.layout_node(n, &script_style, base_style))
      }
      _ => self.layout_node(n, &script_style, base_style),
    });
    let over_layout = over.map(|n| match n {
      MathNode::Operator {
        text,
        stretchy: true,
        variant,
      } => {
        let resolved = self.resolve_variant(*variant, &script_style, MathVariant::Normal);
        self
          .stretch_operator_horizontal(text, resolved, stretch_target, &script_style, base_style)
          .unwrap_or_else(|| self.layout_node(n, &script_style, base_style))
      }
      _ => self.layout_node(n, &script_style, base_style),
    });
    let over_gap = constants
      .as_ref()
      .and_then(|c| c.overbar_vertical_gap)
      .unwrap_or_else(|| Self::frac_gap(style));
    let under_gap = constants
      .as_ref()
      .and_then(|c| c.underbar_vertical_gap)
      .unwrap_or_else(|| Self::frac_gap(style));

    let mut width = base_layout.width;
    if let Some(layout) = &under_layout {
      width = width.max(layout.width);
    }
    if let Some(layout) = &over_layout {
      width = width.max(layout.width);
    }

    let mut fragments = Vec::new();
    let mut annotations = base_layout.annotations.clone();
    // Base
    for frag in base_layout.fragments {
      fragments.push(frag);
    }

    let mut ascent = base_layout.baseline;
    let mut descent = base_layout.height - base_layout.baseline;
    if let Some(layout) = over_layout {
      let x = (width - layout.width) / 2.0;
      let y = -(layout.height + over_gap);
      for frag in layout.fragments {
        fragments.push(frag.translate(Point::new(x, y)));
      }
      ascent = ascent.max(base_layout.baseline - y);
      annotations = annotations.merge_trailing(&layout.annotations);
    }
    if let Some(layout) = under_layout {
      let x = (width - layout.width) / 2.0;
      let y = base_layout.baseline + under_gap;
      for frag in layout.fragments {
        fragments.push(frag.translate(Point::new(x, y)));
      }
      descent = descent.max(layout.height + under_gap);
      annotations = annotations.merge_trailing(&layout.annotations);
    }

    MathLayout {
      width,
      height: ascent + descent,
      baseline: ascent,
      fragments,
      annotations,
    }
  }

  fn layout_sqrt(
    &mut self,
    body: &MathNode,
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    let content = self.layout_node(body, style, base_style);
    let constants =
      self.math_constants_for_layout(&content, style, base_style, MathVariant::Normal);
    let padding = Self::sqrt_padding(style);
    let rule = constants
      .as_ref()
      .and_then(|c| c.radical_rule_thickness)
      .unwrap_or_else(|| Self::rule_thickness(style));
    let gap = constants
      .as_ref()
      .map(|c| {
        if style.display_style {
          c.radical_display_style_vertical_gap
        } else {
          c.radical_vertical_gap
        }
      })
      .flatten()
      .unwrap_or_else(|| Self::sqrt_padding(style));
    let extra_ascender = constants
      .as_ref()
      .and_then(|c| c.radical_extra_ascender)
      .unwrap_or(0.0);
    let target_height = content.height + gap + rule + extra_ascender;
    let target_descent = (content.height - content.baseline).max(0.0);
    let target_ascent = target_height - target_descent;
    let radical_variant = self.resolve_variant(None, style, MathVariant::Normal);
    let mut radical = self
      .stretch_operator_vertical(
        "√",
        radical_variant,
        target_ascent,
        target_descent,
        style,
        base_style,
      )
      .unwrap_or_else(|| self.layout_glyphs("√", base_style, style, radical_variant));
    if (radical.height - target_height).abs() > style.font_size * 0.05 {
      radical = self.align_stretch(radical, target_ascent, target_descent);
    }

    let offset_x = radical.width + padding;
    let content_y = gap + rule;
    let baseline = content.baseline + content_y;
    let mut fragments = Vec::new();
    // Radical glyph
    for frag in radical.fragments {
      fragments.push(frag.translate(Point::new(0.0, baseline - radical.baseline)));
    }

    // Content
    for frag in content.fragments {
      fragments.push(frag.translate(Point::new(offset_x, content_y)));
    }

    fragments.push(MathFragment::Rule(Rect::from_xywh(
      offset_x,
      content_y - rule,
      content.width,
      rule,
    )));

    let height = (content_y + content.height).max(radical.height + (baseline - radical.baseline));
    MathLayout {
      width: offset_x + content.width,
      height: height + padding,
      baseline,
      fragments,
      annotations: content.annotations,
    }
  }

  fn layout_root(
    &mut self,
    radicand: &MathNode,
    index: &MathNode,
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    let constants = self.default_math_constants(style, base_style, MathVariant::Normal);
    let index_style = style.script_with_constants(constants.as_ref());
    let index_layout = self.layout_node(index, &index_style, base_style);
    let sqrt_layout = self.layout_sqrt(radicand, style, base_style);
    let base_gap = constants
      .as_ref()
      .and_then(|c| c.radical_kern_before_degree)
      .unwrap_or_else(|| Self::script_gap(style));

    let offset_x = index_layout.width + base_gap;
    let mut fragments = Vec::new();

    let raise_percent = constants
      .as_ref()
      .and_then(|c| c.radical_degree_bottom_raise_percent)
      .unwrap_or(0.0)
      / 100.0;
    let raise = sqrt_layout.baseline * raise_percent;
    let index_y = (sqrt_layout.baseline - sqrt_layout.height * 0.6) - index_layout.baseline - raise;
    for frag in index_layout.fragments {
      fragments.push(frag.translate(Point::new(0.0, index_y)));
    }

    for frag in sqrt_layout.fragments {
      fragments.push(frag.translate(Point::new(offset_x, 0.0)));
    }

    MathLayout {
      width: offset_x + sqrt_layout.width,
      height: sqrt_layout.height.max(index_y + index_layout.height),
      baseline: sqrt_layout.baseline,
      fragments,
      annotations: sqrt_layout.annotations,
    }
  }

  fn layout_enclose(
    &mut self,
    notation: &[MencloseNotation],
    body: &MathNode,
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    let content = self.layout_node(body, style, base_style);
    let stroke = Self::rule_thickness(style);
    let padding = Self::sqrt_padding(style) + stroke * 0.5;
    let width = content.width + padding * 2.0 + stroke;
    let height = content.height + padding * 2.0 + stroke;
    let baseline = content.baseline + padding + stroke * 0.5;
    let content_offset = Point::new(padding + stroke * 0.5, padding + stroke * 0.5);
    let annotations = content.annotations.clone();

    let mut fragments: Vec<MathFragment> = content
      .fragments
      .into_iter()
      .map(|f| f.translate(content_offset))
      .collect();

    let outer_rect = Rect::from_xywh(0.0, 0.0, width.max(0.0), height.max(0.0));
    for note in notation {
      match note {
        MencloseNotation::Box => fragments.push(MathFragment::StrokeRect {
          rect: outer_rect,
          radius: 0.0,
          width: stroke,
        }),
        MencloseNotation::RoundedBox => fragments.push(MathFragment::StrokeRect {
          rect: outer_rect,
          radius: padding,
          width: stroke,
        }),
        MencloseNotation::Circle => {
          let radius = (outer_rect.width().min(outer_rect.height()) / 2.0).max(0.0);
          fragments.push(MathFragment::StrokeRect {
            rect: outer_rect,
            radius,
            width: stroke,
          });
        }
        MencloseNotation::Top => {
          fragments.push(MathFragment::Rule(Rect::from_xywh(0.0, 0.0, width, stroke)))
        }
        MencloseNotation::Bottom => fragments.push(MathFragment::Rule(Rect::from_xywh(
          0.0,
          height - stroke,
          width,
          stroke,
        ))),
        MencloseNotation::Left => fragments.push(MathFragment::Rule(Rect::from_xywh(
          0.0, 0.0, stroke, height,
        ))),
        MencloseNotation::Right => fragments.push(MathFragment::Rule(Rect::from_xywh(
          width - stroke,
          0.0,
          stroke,
          height,
        ))),
        MencloseNotation::HorizontalStrike => fragments.push(MathFragment::Rule(Rect::from_xywh(
          0.0,
          height / 2.0 - stroke * 0.5,
          width,
          stroke,
        ))),
        MencloseNotation::VerticalStrike => fragments.push(MathFragment::Rule(Rect::from_xywh(
          width / 2.0 - stroke * 0.5,
          0.0,
          stroke,
          height,
        ))),
        MencloseNotation::UpDiagonalStrike | MencloseNotation::DownDiagonalStrike => {
          // Approximate diagonal strikes with intersecting horizontal/vertical marks.
          fragments.push(MathFragment::Rule(Rect::from_xywh(
            0.0,
            height / 2.0 - stroke * 0.5,
            width,
            stroke,
          )));
          fragments.push(MathFragment::Rule(Rect::from_xywh(
            width / 2.0 - stroke * 0.5,
            0.0,
            stroke,
            height,
          )));
        }
        MencloseNotation::LongDiv => {
          fragments.push(MathFragment::Rule(Rect::from_xywh(0.0, 0.0, width, stroke)));
          fragments.push(MathFragment::Rule(Rect::from_xywh(
            0.0, 0.0, stroke, height,
          )));
        }
      }
    }

    MathLayout {
      width,
      height,
      baseline,
      fragments,
      annotations,
    }
  }

  fn layout_table(
    &mut self,
    table: &MathTable,
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    if table.rows.is_empty() {
      return self.layout_glyphs("", base_style, style, MathVariant::Normal);
    }
    let (col_spacing, row_spacing) = Self::table_spacing(style);
    let metrics = self.base_font_metrics(base_style, style.font_size);
    let mut cell_layouts: Vec<Vec<MathLayout>> = Vec::new();
    let mut col_widths: Vec<f32> = Vec::new();
    let mut row_baselines: Vec<f32> = Vec::new();
    let mut row_heights: Vec<f32> = Vec::new();

    for row in &table.rows {
      let mut layouts = Vec::new();
      let mut baseline: f32 = 0.0;
      let mut max_descent: f32 = 0.0;
      let mut max_height: f32 = 0.0;
      for (col, cell) in row.cells.iter().enumerate() {
        let layout = self.layout_node(&cell.content, style, base_style);
        if col >= col_widths.len() {
          col_widths.push(layout.width);
        } else {
          col_widths[col] = col_widths[col].max(layout.width);
        }
        baseline = baseline.max(layout.baseline);
        max_descent = max_descent.max(layout.height - layout.baseline);
        max_height = max_height.max(layout.height);
        layouts.push(layout);
      }
      if row.cells.is_empty() {
        baseline = metrics.ascent;
        max_height = metrics.line_height;
      } else {
        max_height = max_height.max(baseline + max_descent);
      }
      row_baselines.push(baseline);
      row_heights.push(max_height);
      cell_layouts.push(layouts);
    }

    let width: f32 = col_widths.iter().copied().sum::<f32>()
      + col_spacing * (col_widths.len().saturating_sub(1)) as f32;
    let mut y = 0.0;
    let mut fragments = Vec::new();
    let mut table_baseline = 0.0;
    let mut trailing_annotations = MathLayoutAnnotations::default();
    for (row_idx, (row, layouts)) in table.rows.iter().zip(cell_layouts.into_iter()).enumerate() {
      let row_height = row_heights[row_idx];
      let row_baseline = row_baselines[row_idx];
      let row_align_pref = repeating_value(&table.row_aligns, row_idx).or(row.row_align);
      if row_idx == 0 {
        table_baseline = row_baseline + y;
      }
      let mut x = 0.0;
      for (col_idx, (cell, layout)) in row.cells.iter().zip(layouts.into_iter()).enumerate() {
        let col_align_default =
          repeating_value(&table.column_aligns, col_idx).unwrap_or(ColumnAlign::Center);
        let col_align = cell
          .column_align
          .or_else(|| repeating_value(&row.column_aligns, col_idx))
          .unwrap_or(col_align_default);
        let cell_row_align = cell
          .row_align
          .or(row_align_pref)
          .unwrap_or(RowAlign::Baseline);
        let baseline_target = match cell_row_align {
          RowAlign::Axis => Self::axis_height(&metrics, style, None) + style.font_size * 0.5,
          _ => row_baseline,
        };
        let offset_y = match cell_row_align {
          RowAlign::Baseline | RowAlign::Axis => y + (baseline_target - layout.baseline),
          RowAlign::Top => y,
          RowAlign::Bottom => y + (row_height - layout.height),
          RowAlign::Center => y + (row_height - layout.height) / 2.0,
        };
        let width_available = col_widths.get(col_idx).copied().unwrap_or(layout.width);
        let offset_x = x
          + match col_align {
            ColumnAlign::Left => 0.0,
            ColumnAlign::Center => (width_available - layout.width) / 2.0,
            ColumnAlign::Right => (width_available - layout.width).max(0.0),
          };
        for frag in layout.fragments {
          fragments.push(frag.translate(Point::new(offset_x, offset_y)));
        }
        x += width_available + col_spacing;
        trailing_annotations = trailing_annotations.merge_trailing(&layout.annotations);
      }
      y += row_height + row_spacing;
    }

    MathLayout {
      width,
      height: y - row_spacing,
      baseline: table_baseline,
      fragments,
      annotations: trailing_annotations,
    }
  }

  fn layout_multiscripts(
    &mut self,
    base: &MathNode,
    pre: &[(Option<MathNode>, Option<MathNode>)],
    post: &[(Option<MathNode>, Option<MathNode>)],
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    let base_layout = self.layout_node(base, style, base_style);
    let constants =
      self.math_constants_for_layout(&base_layout, style, base_style, MathVariant::Normal);
    let script_style = style.script_with_constants(constants.as_ref());
    let mut fragments = Vec::new();
    let mut ascent = base_layout.baseline;
    let mut descent = base_layout.height - base_layout.baseline;

    let script_gap = constants
      .as_ref()
      .and_then(|c| c.space_after_script)
      .unwrap_or_else(|| Self::script_gap(style));
    let base_metrics = self.base_font_metrics(base_style, style.font_size);
    let x_height = base_metrics.x_height.unwrap_or(style.font_size * 0.5);
    let sup_fallback = || {
      (base_metrics.ascent * 0.6)
        .max(x_height * 0.65)
        .max(style.font_size * if style.display_style { 0.4 } else { 0.34 })
    };
    let sup_shift_up = constants
      .as_ref()
      .and_then(|c| c.superscript_shift_up)
      .unwrap_or_else(sup_fallback);
    let sup_shift_up_cramped = constants
      .as_ref()
      .and_then(|c| c.superscript_shift_up_cramped)
      .unwrap_or_else(sup_fallback);
    let sub_shift = constants
      .as_ref()
      .and_then(|c| c.subscript_shift_down)
      .unwrap_or_else(|| (base_metrics.descent * 0.8 + x_height * 0.2).max(style.font_size * 0.24));
    let min_gap = constants
      .as_ref()
      .and_then(|c| c.sub_superscript_gap_min)
      .unwrap_or_else(|| {
        (Self::script_gap(style) + Self::rule_thickness(style)).max(style.font_size * 0.06)
      });
    let sup_bottom_min = constants.as_ref().and_then(|c| c.superscript_bottom_min);
    let sup_bottom_max_with_sub = constants
      .as_ref()
      .and_then(|c| c.superscript_bottom_max_with_subscript);
    let sub_top_max = constants.as_ref().and_then(|c| c.subscript_top_max);
    let sub_baseline_drop_min = constants
      .as_ref()
      .and_then(|c| c.subscript_baseline_drop_min);
    let base_descent = base_layout.height - base_layout.baseline;
    let italic_correction = base_layout
      .annotations
      .trailing_glyph
      .as_ref()
      .map(|g| g.italic_correction)
      .unwrap_or(0.0);

    let build_block = |scripts: &[(Option<MathNode>, Option<MathNode>)],
                       side: MathKernSide,
                       apply_italic: bool,
                       ctx: &mut Self|
     -> (f32, f32, f32, Vec<MathFragment>, MathLayoutAnnotations) {
      let mut block_width: f32 = 0.0;
      let mut block_ascent: f32 = 0.0;
      let mut block_descent: f32 = 0.0;
      let mut frags = Vec::new();
      let mut annotations = MathLayoutAnnotations::default();
      let mut first_pair = true;
      for pair in scripts {
        let sup_layout = pair
          .1
          .as_ref()
          .map(|n| ctx.layout_node(n, &script_style, base_style));
        let sub_layout = pair
          .0
          .as_ref()
          .map(|n| ctx.layout_node(n, &script_style, base_style));
        if sup_layout.is_none() && sub_layout.is_none() {
          continue;
        }
        if !first_pair {
          block_width += script_gap;
        }
        first_pair = false;
        let x_start = block_width;
        let mut pair_end = block_width;
        let mut pair_ascent: f32 = 0.0;
        let mut pair_descent: f32 = 0.0;
        let mut sup_pos: Option<(f32, f32)> = None;
        if let Some(layout) = &sup_layout {
          let has_sub = sub_layout.is_some();
          let mut y = base_layout.baseline
            - if has_sub {
              sup_shift_up_cramped
            } else {
              sup_shift_up
            }
            - layout.baseline;
          if let Some(bottom_min) = sup_bottom_min {
            let sup_bottom = y + layout.height - layout.baseline;
            let allowed = base_layout.baseline - bottom_min;
            if sup_bottom > allowed {
              y -= sup_bottom - allowed;
            }
          }
          if let (Some(limit), true) = (sup_bottom_max_with_sub, has_sub) {
            let sup_bottom = y + layout.height - layout.baseline;
            let allowed = base_layout.baseline - limit;
            if sup_bottom > allowed {
              y -= sup_bottom - allowed;
            }
          }
          let sup_kern = base_layout
            .annotations
            .trailing_glyph
            .as_ref()
            .map(|g| {
              ctx.font_ctx.math_kern(
                &g.font,
                g.glyph_id,
                layout.baseline,
                g.font_size,
                true,
                side,
              )
            })
            .unwrap_or(0.0);
          let sup_x = x_start + if apply_italic { italic_correction } else { 0.0 } + sup_kern;
          for frag in &layout.fragments {
            frags.push(frag.clone().translate(Point::new(sup_x, y)));
          }
          pair_end = pair_end.max(sup_x + layout.width);
          pair_ascent = pair_ascent.max(layout.baseline - y);
          pair_descent = pair_descent.max(layout.height - (layout.baseline - y));
          sup_pos = Some((y, layout.height));
          annotations = annotations.merge_trailing(&layout.annotations);
        }
        if let Some(layout) = &sub_layout {
          let mut y = base_layout.baseline + base_descent + sub_shift - layout.baseline;
          if let Some((sup_y, sup_h)) = sup_pos {
            let sup_bottom = sup_y + sup_h;
            let gap = y - sup_bottom;
            if gap < min_gap {
              y += min_gap - gap;
            }
          }
          if let Some(top_max) = sub_top_max {
            let sub_top = y + layout.baseline;
            let min_top = base_layout.baseline + top_max;
            if sub_top < min_top {
              y += min_top - sub_top;
            }
          }
          if let Some(min_drop) = sub_baseline_drop_min {
            let drop = y + layout.baseline - base_layout.baseline;
            if drop < min_drop {
              y += min_drop - drop;
            }
          }
          let sub_kern = base_layout
            .annotations
            .trailing_glyph
            .as_ref()
            .map(|g| {
              ctx.font_ctx.math_kern(
                &g.font,
                g.glyph_id,
                layout.height - layout.baseline,
                g.font_size,
                false,
                side,
              )
            })
            .unwrap_or(0.0);
          let sub_x = x_start + sub_kern;
          for frag in &layout.fragments {
            frags.push(frag.clone().translate(Point::new(sub_x, y)));
          }
          pair_end = pair_end.max(sub_x + layout.width);
          pair_ascent = pair_ascent.max(layout.baseline - y);
          pair_descent = pair_descent.max(layout.height - (layout.baseline - y));
          annotations = annotations.merge_trailing(&layout.annotations);
        }
        block_ascent = block_ascent.max(pair_ascent);
        block_descent = block_descent.max(pair_descent);
        block_width = pair_end;
      }
      if !first_pair {
        block_width += script_gap;
      }
      (block_width, block_ascent, block_descent, frags, annotations)
    };

    let (pre_width, pre_ascent, pre_descent, pre_frags, pre_annot) =
      build_block(pre, MathKernSide::Left, false, self);
    let (post_width, post_ascent, post_descent, post_frags, post_annot) =
      build_block(post, MathKernSide::Right, true, self);
    let width_left = pre_width;
    let width_right = post_width;
    ascent = ascent.max(pre_ascent).max(post_ascent);
    descent = descent.max(pre_descent).max(post_descent);

    // Position fragments
    for frag in pre_frags {
      fragments.push(frag);
    }
    for frag in base_layout.fragments {
      fragments.push(frag.translate(Point::new(width_left, 0.0)));
    }
    for frag in post_frags {
      fragments.push(frag.translate(Point::new(width_left + base_layout.width, 0.0)));
    }

    MathLayout {
      width: width_left + base_layout.width + width_right,
      height: ascent + descent,
      baseline: ascent,
      fragments,
      annotations: post_annot
        .merge_trailing(&base_layout.annotations)
        .merge_trailing(&pre_annot),
    }
  }

  fn layout_node(
    &mut self,
    node: &MathNode,
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    match node {
      MathNode::Math { display, children } => {
        let mut style = *style;
        style.display_style = style.display_style || *display;
        self.layout_row(children, &style, base_style)
      }
      MathNode::Row(children) => self.layout_row(children, style, base_style),
      MathNode::Identifier { text, variant } => {
        let resolved = self.resolve_variant(*variant, style, MathVariant::Italic);
        self.layout_glyphs(text, base_style, style, resolved)
      }
      MathNode::Number { text, variant } => {
        let resolved = self.resolve_variant(*variant, style, MathVariant::Normal);
        self.layout_glyphs(text, base_style, style, resolved)
      }
      MathNode::Operator {
        text,
        stretchy: _,
        variant,
      } => {
        let resolved = self.resolve_variant(*variant, style, MathVariant::Normal);
        // Stretching handled during row aggregation by scaling font size heuristically.
        self.layout_glyphs(text, base_style, style, resolved)
      }
      MathNode::Text { text, variant } => {
        let resolved = self.resolve_variant(*variant, style, MathVariant::Normal);
        self.layout_glyphs(text, base_style, style, resolved)
      }
      MathNode::Space {
        width,
        height,
        depth,
      } => self.layout_space(*width, *height, *depth, style, base_style),
      MathNode::Fraction {
        numerator,
        denominator,
      } => self.layout_fraction(numerator, denominator, style, base_style),
      MathNode::Sqrt(body) => self.layout_sqrt(body, style, base_style),
      MathNode::Root { radicand, index } => self.layout_root(radicand, index, style, base_style),
      MathNode::Superscript { base, superscript } => {
        self.layout_superscript(base, Some(superscript.as_ref()), None, style, base_style)
      }
      MathNode::Subscript { base, subscript } => {
        self.layout_superscript(base, None, Some(subscript.as_ref()), style, base_style)
      }
      MathNode::SubSuperscript {
        base,
        subscript,
        superscript,
      } => self.layout_superscript(
        base,
        Some(superscript.as_ref()),
        Some(subscript.as_ref()),
        style,
        base_style,
      ),
      MathNode::Over { base, over } => {
        self.layout_under_over(base, None, Some(over.as_ref()), style, base_style)
      }
      MathNode::Under { base, under } => {
        self.layout_under_over(base, Some(under.as_ref()), None, style, base_style)
      }
      MathNode::UnderOver { base, under, over } => self.layout_under_over(
        base,
        Some(under.as_ref()),
        Some(over.as_ref()),
        style,
        base_style,
      ),
      MathNode::Style {
        overrides,
        children,
      } => {
        let next_style = self.apply_style_overrides(style, overrides);
        self.layout_row(children, &next_style, base_style)
      }
      MathNode::Enclose { notation, child } => {
        self.layout_enclose(notation, child, style, base_style)
      }
      MathNode::Table(table) => self.layout_table(table, style, base_style),
      MathNode::Multiscripts {
        base,
        prescripts,
        postscripts,
      } => self.layout_multiscripts(base, prescripts, postscripts, style, base_style),
    }
  }

  /// Public entrypoint: layout a MathNode tree using the provided style.
  pub fn layout(&mut self, node: &MathNode, style: &ComputedStyle) -> MathLayout {
    let math_style = MathStyle::from_computed(style);
    self.layout_node(node, &math_style, style)
  }
}

/// Layout MathML using the provided style and font context.
pub fn layout_mathml(node: &MathNode, style: &ComputedStyle, font_ctx: &FontContext) -> MathLayout {
  let mut ctx = MathLayoutContext::new(font_ctx.clone());
  ctx.layout(node, style)
}

#[cfg(test)]
mod tests {
  use super::*;

  fn find_math_element<'a>(node: &'a crate::dom::DomNode) -> Option<&'a crate::dom::DomNode> {
    if node
      .tag_name()
      .map(|t| t.eq_ignore_ascii_case("math"))
      .unwrap_or(false)
    {
      return Some(node);
    }
    node.children.iter().find_map(find_math_element)
  }

  fn parse_math_from_html(markup: &str) -> MathNode {
    let dom = crate::dom::parse_html(markup).expect("dom");
    let math_node = find_math_element(&dom).expect("math element");
    parse_mathml(math_node).expect("math parsed")
  }

  #[test]
  fn table_layout_completes() {
    let style = ComputedStyle::default();
    let node = MathNode::Table(MathTable {
      rows: vec![
        MathTableRow {
          cells: vec![MathTableCell {
            content: MathNode::Identifier {
              text: "a".into(),
              variant: None,
            },
            row_align: None,
            column_align: None,
          }],
          row_align: None,
          column_aligns: Vec::new(),
        },
        MathTableRow {
          cells: vec![MathTableCell {
            content: MathNode::Identifier {
              text: "b".into(),
              variant: None,
            },
            row_align: None,
            column_align: None,
          }],
          row_align: None,
          column_aligns: Vec::new(),
        },
      ],
      column_aligns: Vec::new(),
      row_aligns: Vec::new(),
    });
    let layout = layout_mathml(&node, &style, &FontContext::empty());
    assert!(layout.width > 0.0);
    assert!(layout.height > 0.0);
    assert!(layout.baseline > 0.0);
  }

  #[test]
  fn table_layout_with_font_db() {
    let style = ComputedStyle::default();
    let node = MathNode::Table(MathTable {
      rows: vec![
        MathTableRow {
          cells: vec![
            MathTableCell {
              content: MathNode::Number {
                text: "1".into(),
                variant: None,
              },
              row_align: None,
              column_align: None,
            },
            MathTableCell {
              content: MathNode::Number {
                text: "2".into(),
                variant: None,
              },
              row_align: None,
              column_align: None,
            },
          ],
          row_align: None,
          column_aligns: Vec::new(),
        },
        MathTableRow {
          cells: vec![
            MathTableCell {
              content: MathNode::Number {
                text: "3".into(),
                variant: None,
              },
              row_align: None,
              column_align: None,
            },
            MathTableCell {
              content: MathNode::Number {
                text: "4".into(),
                variant: None,
              },
              row_align: None,
              column_align: None,
            },
          ],
          row_align: None,
          column_aligns: Vec::new(),
        },
      ],
      column_aligns: Vec::new(),
      row_aligns: Vec::new(),
    });
    let ctx = FontContext::new();
    let layout = layout_mathml(&node, &style, &ctx);
    assert!(layout.width > 0.0);
    assert!(layout.height > 0.0);
  }

  #[test]
  fn mathvariant_controls_token_style() {
    let parsed = parse_math_from_html("<math><mi mathvariant=\"normal\">x</mi></math>");
    let MathNode::Math { children, .. } = parsed else {
      panic!("expected math root");
    };
    let MathNode::Identifier { variant, text } = &children[0] else {
      panic!("expected identifier child");
    };
    assert_eq!(text, "x");
    assert!(matches!(variant, Some(MathVariant::Normal)));
  }

  #[test]
  fn semantics_ignores_annotation_children() {
    let markup = r#"<math>
        <semantics>
          <mrow><mi>x</mi><mo>=</mo><mn>1</mn></mrow>
          <annotation encoding="application/x-tex">x=1</annotation>
          <annotation-xml encoding="application/mathml+xml"><mi>y</mi></annotation-xml>
        </semantics>
      </math>"#;
    let parsed = parse_math_from_html(markup);
    let MathNode::Math { children, .. } = parsed else {
      panic!("expected math root");
    };
    assert_eq!(
      children.len(),
      1,
      "only presentation child should be parsed"
    );
    let row_children = match &children[0] {
      MathNode::Row(children) => children,
      other => panic!("expected row child, got {:?}", other),
    };
    assert_eq!(
      row_children.len(),
      3,
      "annotation content should be skipped"
    );
    assert!(
      !row_children
        .iter()
        .any(|child| { matches!(child, MathNode::Text { text, .. } if text.contains("x=1")) }),
      "annotation text should not appear in parsed output",
    );
    let dom = crate::dom::parse_html(markup).expect("dom");
    let semantics_node = find_math_element(&dom)
      .and_then(|math| {
        math.children.iter().find(|child| {
          child
            .tag_name()
            .map(|t| t.eq_ignore_ascii_case("semantics"))
            .unwrap_or(false)
        })
      })
      .expect("semantics element");
    let annotation_node = semantics_node
      .children
      .iter()
      .find(|child| {
        child
          .tag_name()
          .map(|t| t.eq_ignore_ascii_case("annotation"))
          .unwrap_or(false)
      })
      .expect("annotation child");
    assert!(
      parse_mathml(annotation_node).is_none(),
      "annotation nodes should be ignored entirely"
    );
  }

  #[test]
  fn parses_none_in_multiscripts_as_absent_slot() {
    let parsed = parse_math_from_html(
      "<math><mmultiscripts><mi>x</mi><none/><mi>a</mi></mmultiscripts></math>",
    );
    let MathNode::Math { children, .. } = parsed else {
      panic!("expected math root");
    };
    let MathNode::Multiscripts { postscripts, .. } = &children[0] else {
      panic!("expected multiscripts child");
    };
    assert_eq!(postscripts.len(), 1);
    let (sub, sup) = &postscripts[0];
    assert!(sub.is_none(), "expected omitted subscript to be None");
    let MathNode::Identifier { text, .. } = sup.as_ref().expect("expected superscript") else {
      panic!("expected identifier superscript");
    };
    assert_eq!(text, "a");
  }

  #[test]
  fn none_scripts_do_not_affect_multiscript_width() {
    let style = ComputedStyle::default();
    let ctx = FontContext::empty();
    let with_none =
      parse_math_from_html("<math><mmultiscripts><mi>x</mi><none/><none/></mmultiscripts></math>");
    let without_none =
      parse_math_from_html("<math><mmultiscripts><mi>x</mi></mmultiscripts></math>");
    let with_layout = layout_mathml(&with_none, &style, &ctx);
    let without_layout = layout_mathml(&without_none, &style, &ctx);
    assert!(
      (with_layout.width - without_layout.width).abs() < 0.001,
      "none placeholder should not change width: {} vs {}",
      with_layout.width,
      without_layout.width
    );
  }
}
