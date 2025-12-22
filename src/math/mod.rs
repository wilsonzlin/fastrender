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
use crate::text::font_db::{FontStretch, FontStyle, ScaledMetrics};
use crate::text::font_loader::FontContext;
use crate::text::pipeline::{Direction as TextDirection, ShapedRun, ShapingPipeline};

const SCRIPT_SCALE: f32 = 0.71;

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
    italic: bool,
    bold: bool,
  },
  Number {
    text: String,
    italic: bool,
    bold: bool,
  },
  Operator {
    text: String,
    stretchy: bool,
    italic: bool,
    bold: bool,
  },
  Text {
    text: String,
    italic: bool,
    bold: bool,
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
  Table(Vec<Vec<MathNode>>),
}

/// Renderable fragment produced by math layout.
#[derive(Debug, Clone)]
pub enum MathFragment {
  Glyph { origin: Point, run: ShapedRun },
  Rule(Rect),
}

/// Final math layout with positioned fragments.
#[derive(Debug, Clone)]
pub struct MathLayout {
  pub width: f32,
  pub height: f32,
  pub baseline: f32,
  pub fragments: Vec<MathFragment>,
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
  display: bool,
}

impl MathStyle {
  fn script(&self) -> Self {
    let mut size = self.font_size * SCRIPT_SCALE;
    if size < 6.0 {
      size = 6.0;
    }
    Self {
      font_size: size,
      display: false,
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
    | DomNodeType::Document
    | DomNodeType::ShadowRoot { .. } => {
      for child in &node.children {
        collect_text(child, out);
      }
    }
  }
}

fn mathvariant_flags(node: &DomNode, default_italic: bool, default_bold: bool) -> (bool, bool) {
  let Some(value) = node.get_attribute_ref("mathvariant") else {
    return (default_italic, default_bold);
  };

  match value.to_ascii_lowercase().as_str() {
    "normal" | "upright" => (false, false),
    "bold" => (false, true),
    "italic" => (true, false),
    "bold-italic" => (true, true),
    "double-struck" => (false, true),
    "script" => (true, default_bold),
    "bold-script" => (true, true),
    "fraktur" => (true, default_bold),
    "bold-fraktur" => (true, true),
    "sans-serif" => (false, default_bold),
    "sans-serif-bold" | "bold-sans-serif" => (false, true),
    "sans-serif-italic" | "sans-serif-oblique" => (true, default_bold),
    "sans-serif-bold-italic" | "bold-sans-serif-italic" => (true, true),
    "monospace" => (false, default_bold),
    _ => (default_italic, default_bold),
  }
}

fn parse_children(node: &DomNode) -> Vec<MathNode> {
  node.children.iter().filter_map(parse_mathml).collect()
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
          italic: false,
          bold: false,
        })
      }
    }
    DomNodeType::Element {
      tag_name,
      namespace,
      ..
    } => {
      let tag = tag_name.to_ascii_lowercase();
      let in_math_ns = namespace == MATHML_NAMESPACE;
      match tag.as_str() {
        "math" if in_math_ns || namespace.is_empty() => {
          let display = node
            .get_attribute_ref("display")
            .map(|v| v.eq_ignore_ascii_case("block"))
            .unwrap_or(false);
          let children = parse_children(node);
          Some(MathNode::Math { display, children })
        }
        "mrow" => Some(MathNode::Row(parse_children(node))),
        "mi" => normalized_text(node, false).map(|text| {
          let (italic, bold) = mathvariant_flags(node, true, false);
          MathNode::Identifier { text, italic, bold }
        }),
        "mn" => normalized_text(node, false).map(|text| {
          let (italic, bold) = mathvariant_flags(node, false, false);
          MathNode::Number { text, italic, bold }
        }),
        "mo" => normalized_text(node, false).map(|text| {
          let stretchy = node
            .get_attribute_ref("stretchy")
            .map(|v| !v.eq_ignore_ascii_case("false"))
            .unwrap_or(true);
          let (italic, bold) = mathvariant_flags(node, false, false);
          MathNode::Operator {
            text,
            stretchy,
            italic,
            bold,
          }
        }),
        "ms" | "mtext" => normalized_text(node, true).map(|text| {
          let (italic, bold) = mathvariant_flags(node, false, false);
          MathNode::Text { text, italic, bold }
        }),
        "mfrac" => {
          let mut children = parse_children(node).into_iter();
          let num = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          let den = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          Some(MathNode::Fraction {
            numerator: Box::new(num),
            denominator: Box::new(den),
          })
        }
        "msqrt" => {
          let mut children = parse_children(node);
          let child = match children.len() {
            0 => MathNode::Text {
              text: "".into(),
              italic: false,
              bold: false,
            },
            1 => children.remove(0),
            _ => MathNode::Row(children),
          };
          Some(MathNode::Sqrt(Box::new(child)))
        }
        "mroot" => {
          let mut children = parse_children(node).into_iter();
          let radicand = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          let index = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          Some(MathNode::Root {
            radicand: Box::new(radicand),
            index: Box::new(index),
          })
        }
        "msup" => {
          let mut children = parse_children(node).into_iter();
          let base = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          let sup = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          Some(MathNode::Superscript {
            base: Box::new(base),
            superscript: Box::new(sup),
          })
        }
        "msub" => {
          let mut children = parse_children(node).into_iter();
          let base = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          let sub = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          Some(MathNode::Subscript {
            base: Box::new(base),
            subscript: Box::new(sub),
          })
        }
        "msubsup" => {
          let mut children = parse_children(node).into_iter();
          let base = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          let sub = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          let sup = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          Some(MathNode::SubSuperscript {
            base: Box::new(base),
            subscript: Box::new(sub),
            superscript: Box::new(sup),
          })
        }
        "mover" => {
          let mut children = parse_children(node).into_iter();
          let base = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          let over = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          Some(MathNode::Over {
            base: Box::new(base),
            over: Box::new(over),
          })
        }
        "munder" => {
          let mut children = parse_children(node).into_iter();
          let base = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          let under = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          Some(MathNode::Under {
            base: Box::new(base),
            under: Box::new(under),
          })
        }
        "munderover" => {
          let mut children = parse_children(node).into_iter();
          let base = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          let under = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
          let over = children.next().unwrap_or_else(|| MathNode::Text {
            text: "".into(),
            italic: false,
            bold: false,
          });
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
            .unwrap_or_else(|| MathNode::Text {
              text: "".into(),
              italic: false,
              bold: false,
            });
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
        "mtr" => Some(MathNode::Row(
          node.children.iter().filter_map(parse_mathml).collect(),
        )),
        "mtd" => Some(MathNode::Row(parse_children(node))),
        "mtable" => {
          let rows: Vec<Vec<MathNode>> = node
            .children
            .iter()
            .filter_map(|c| {
              if let Some(tag) = c.tag_name() {
                if tag.eq_ignore_ascii_case("mtr") || tag.eq_ignore_ascii_case("mtd") {
                  return parse_mathml(c).map(|n| match n {
                    MathNode::Row(children) => children,
                    other => vec![other],
                  });
                }
              }
              None
            })
            .collect();
          Some(MathNode::Table(rows))
        }
        _ => Some(MathNode::Row(parse_children(node))),
      }
    }
    DomNodeType::Document => node.children.iter().find_map(parse_mathml),
  }
}

/// Layout engine for math trees.
pub struct MathLayoutContext {
  pipeline: ShapingPipeline,
  font_ctx: FontContext,
}

impl MathLayoutContext {
  pub fn new(font_ctx: FontContext) -> Self {
    Self {
      pipeline: ShapingPipeline::new(),
      font_ctx,
    }
  }

  fn rule_thickness(style: &MathStyle) -> f32 {
    (style.font_size * 0.06).clamp(1.0, style.font_size * 0.5)
  }

  fn axis_height(metrics: &ScaledMetrics, style: &MathStyle) -> f32 {
    metrics
      .x_height
      .unwrap_or(style.font_size * 0.5)
      .max(style.font_size * 0.2)
      * 0.5
  }

  fn script_gap(style: &MathStyle) -> f32 {
    style.font_size * 0.1
  }

  fn frac_gap(style: &MathStyle) -> f32 {
    style.font_size * 0.2
  }

  fn sqrt_padding(style: &MathStyle) -> f32 {
    style.font_size * 0.1
  }

  fn table_spacing(style: &MathStyle) -> (f32, f32) {
    (style.font_size * 0.5, style.font_size * 0.25)
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
    for fam in &style.font_family {
      if !families.iter().any(|f| f.eq_ignore_ascii_case(fam)) {
        families.push(fam.clone());
      }
    }
    families
  }

  fn shape_text(
    &mut self,
    text: &str,
    base_style: &ComputedStyle,
    math_style: &MathStyle,
    italic: bool,
    bold: bool,
  ) -> (Vec<ShapedRun>, ScaledMetrics) {
    let mut style = base_style.clone();
    style.font_size = math_style.font_size;
    style.font_family = self.preferred_math_families(base_style);
    if italic {
      style.font_style = CssFontStyle::Italic;
    }
    if bold {
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
    italic: bool,
    bold: bool,
  ) -> MathLayout {
    let (runs, metrics) = self.shape_text(text, base_style, math_style, italic, bold);
    if runs.is_empty() {
      let height = math_style.font_size;
      return MathLayout {
        width: math_style.font_size * text.len() as f32 * 0.6,
        height,
        baseline: height * 0.8,
        fragments: vec![],
      };
    }

    let ascent = metrics.ascent;
    let descent = metrics.descent;
    let width: f32 = runs.iter().map(|r| r.advance).sum();
    let mut fragments = Vec::new();
    let mut pen_x = 0.0;
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
      let mut max_ascent: f32 = 0.0;
      let mut max_descent: f32 = 0.0;
      for layout in &layouts {
        max_ascent = max_ascent.max(layout.baseline);
        max_descent = max_descent.max(layout.height - layout.baseline);
      }
      let target_height = max_ascent + max_descent;
      for idx in stretchy_indices {
        if let MathNode::Operator {
          text, italic, bold, ..
        } = &children[idx]
        {
          if target_height > layouts[idx].height * 1.05 && layouts[idx].height > 0.0 {
            let factor = (target_height / layouts[idx].height).clamp(1.0, 4.0);
            let mut stretch_style = *style;
            stretch_style.font_size *= factor;
            layouts[idx] = self.layout_glyphs(text, base_style, &stretch_style, *italic, *bold);
          }
        }
      }
    }
    if layouts.is_empty() {
      return self.layout_glyphs("", base_style, style, false, false);
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
    for layout in layouts {
      let y = baseline - layout.baseline;
      for frag in layout.fragments {
        match frag {
          MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
            origin: Point::new(origin.x + x, origin.y + y),
            run,
          }),
          MathFragment::Rule(rect) => {
            fragments.push(MathFragment::Rule(rect.translate(Point::new(x, y))))
          }
        }
      }
      x += layout.width;
    }
    MathLayout {
      width: x,
      height: baseline + max_descent,
      baseline,
      fragments,
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
    let axis = Self::axis_height(&metrics, style);
    let script_style = style.script();
    let numerator = self.layout_node(num, &script_style, base_style);
    let denominator = self.layout_node(den, &script_style, base_style);
    let width = numerator.width.max(denominator.width);
    let rule = Self::rule_thickness(style);
    let gap = Self::frac_gap(style);
    let baseline = (numerator.height + gap + rule * 0.5).max(axis + gap + rule * 0.5);
    let mut fragments = Vec::new();
    let num_x = (width - numerator.width) / 2.0;
    let den_x = (width - denominator.width) / 2.0;
    let num_y = baseline - rule * 0.5 - gap - numerator.baseline;
    let den_y = baseline + rule * 0.5 + gap - denominator.baseline;

    for frag in numerator.fragments {
      match frag {
        MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
          origin: Point::new(origin.x + num_x, origin.y + num_y),
          run,
        }),
        MathFragment::Rule(rect) => {
          fragments.push(MathFragment::Rule(rect.translate(Point::new(num_x, num_y))))
        }
      }
    }
    for frag in denominator.fragments {
      match frag {
        MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
          origin: Point::new(origin.x + den_x, origin.y + den_y),
          run,
        }),
        MathFragment::Rule(rect) => {
          fragments.push(MathFragment::Rule(rect.translate(Point::new(den_x, den_y))))
        }
      }
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
    let script_style = style.script();
    let sup_layout = sup.map(|n| self.layout_node(n, &script_style, base_style));
    let sub_layout = sub.map(|n| self.layout_node(n, &script_style, base_style));
    let script_gap = Self::script_gap(style);

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
      width += script_gap + script_width;
    }

    // Base fragments
    for frag in base_layout.fragments {
      match frag {
        MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph { origin, run }),
        MathFragment::Rule(rect) => fragments.push(MathFragment::Rule(rect)),
      }
    }

    let x = base_layout.width + if script_width > 0.0 { script_gap } else { 0.0 };
    let base_descent = base_layout.height - base_layout.baseline;
    if let Some(layout) = sup_layout {
      let y = base_layout.baseline - layout.baseline - style.font_size * 0.35;
      for frag in layout.fragments {
        match frag {
          MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
            origin: Point::new(origin.x + x, origin.y + y),
            run,
          }),
          MathFragment::Rule(rect) => {
            fragments.push(MathFragment::Rule(rect.translate(Point::new(x, y))))
          }
        }
      }
      max_ascent = max_ascent.max(layout.baseline - y);
      max_descent = max_descent.max(layout.height - (layout.baseline - y));
    }

    if let Some(layout) = sub_layout {
      let y = base_layout.baseline + base_descent + script_gap - layout.baseline;
      for frag in layout.fragments {
        match frag {
          MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
            origin: Point::new(origin.x + x, origin.y + y),
            run,
          }),
          MathFragment::Rule(rect) => {
            fragments.push(MathFragment::Rule(rect.translate(Point::new(x, y))))
          }
        }
      }
      max_ascent = max_ascent.max(layout.baseline - y);
      max_descent = max_descent.max(layout.height - (layout.baseline - y));
    }

    MathLayout {
      width,
      height: max_ascent + max_descent,
      baseline: max_ascent,
      fragments,
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
    let script_style = style.script();
    let under_layout = under.map(|n| self.layout_node(n, &script_style, base_style));
    let over_layout = over.map(|n| self.layout_node(n, &script_style, base_style));
    let gap = Self::frac_gap(style);

    let mut width = base_layout.width;
    if let Some(layout) = &under_layout {
      width = width.max(layout.width);
    }
    if let Some(layout) = &over_layout {
      width = width.max(layout.width);
    }

    let mut fragments = Vec::new();
    // Base
    for frag in base_layout.fragments {
      match frag {
        MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph { origin, run }),
        MathFragment::Rule(rect) => fragments.push(MathFragment::Rule(rect)),
      }
    }

    let mut ascent = base_layout.baseline;
    let mut descent = base_layout.height - base_layout.baseline;
    if let Some(layout) = over_layout {
      let x = (width - layout.width) / 2.0;
      let y = base_layout.baseline - gap - layout.height;
      for frag in layout.fragments {
        match frag {
          MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
            origin: Point::new(origin.x + x, origin.y + y),
            run,
          }),
          MathFragment::Rule(rect) => {
            fragments.push(MathFragment::Rule(rect.translate(Point::new(x, y))))
          }
        }
      }
      ascent = ascent.max(base_layout.baseline - y);
    }
    if let Some(layout) = under_layout {
      let x = (width - layout.width) / 2.0;
      let y = base_layout.baseline + gap;
      for frag in layout.fragments {
        match frag {
          MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
            origin: Point::new(origin.x + x, origin.y + y),
            run,
          }),
          MathFragment::Rule(rect) => {
            fragments.push(MathFragment::Rule(rect.translate(Point::new(x, y))))
          }
        }
      }
      descent = descent.max(layout.height + gap);
    }

    MathLayout {
      width,
      height: ascent + descent,
      baseline: ascent,
      fragments,
    }
  }

  fn layout_sqrt(
    &mut self,
    body: &MathNode,
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    let content = self.layout_node(body, style, base_style);
    let padding = Self::sqrt_padding(style);
    let rule = Self::rule_thickness(style);
    let target_height = content.height + padding + rule;
    let mut radical = self.layout_glyphs("√", base_style, style, false, false);
    if radical.height > 0.0 {
      let scale = (target_height / radical.height).clamp(0.8, 3.0);
      if (scale - 1.0).abs() > 0.05 {
        let scaled_style = MathStyle {
          font_size: style.font_size * scale,
          display: style.display,
        };
        radical = self.layout_glyphs("√", base_style, &scaled_style, false, false);
      }
    }

    let offset_x = radical.width + padding;
    let content_y = padding + rule;
    let baseline = content.baseline + content_y;
    let mut fragments = Vec::new();
    // Radical glyph
    for frag in radical.fragments {
      match frag {
        MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
          origin: Point::new(origin.x, origin.y + (baseline - radical.baseline)),
          run,
        }),
        MathFragment::Rule(rect) => fragments.push(MathFragment::Rule(
          rect.translate(Point::new(0.0, baseline - radical.baseline)),
        )),
      }
    }

    // Content
    for frag in content.fragments {
      match frag {
        MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
          origin: Point::new(origin.x + offset_x, origin.y + content_y),
          run,
        }),
        MathFragment::Rule(rect) => fragments.push(MathFragment::Rule(
          rect.translate(Point::new(offset_x, content_y)),
        )),
      }
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
    }
  }

  fn layout_root(
    &mut self,
    radicand: &MathNode,
    index: &MathNode,
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    let index_style = style.script();
    let index_layout = self.layout_node(index, &index_style, base_style);
    let sqrt_layout = self.layout_sqrt(radicand, style, base_style);
    let gap = Self::script_gap(style);

    let offset_x = index_layout.width + gap;
    let mut fragments = Vec::new();

    let index_y = (sqrt_layout.baseline - sqrt_layout.height * 0.6) - index_layout.baseline;
    for frag in index_layout.fragments {
      match frag {
        MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
          origin: Point::new(origin.x, origin.y + index_y),
          run,
        }),
        MathFragment::Rule(rect) => {
          fragments.push(MathFragment::Rule(rect.translate(Point::new(0.0, index_y))))
        }
      }
    }

    for frag in sqrt_layout.fragments {
      match frag {
        MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
          origin: Point::new(origin.x + offset_x, origin.y),
          run,
        }),
        MathFragment::Rule(rect) => fragments.push(MathFragment::Rule(
          rect.translate(Point::new(offset_x, 0.0)),
        )),
      }
    }

    MathLayout {
      width: offset_x + sqrt_layout.width,
      height: sqrt_layout.height.max(index_y + index_layout.height),
      baseline: sqrt_layout.baseline,
      fragments,
    }
  }

  fn layout_table(
    &mut self,
    rows: &[Vec<MathNode>],
    style: &MathStyle,
    base_style: &ComputedStyle,
  ) -> MathLayout {
    if rows.is_empty() {
      return self.layout_glyphs("", base_style, style, false, false);
    }
    let (col_spacing, row_spacing) = Self::table_spacing(style);
    let mut cell_layouts: Vec<Vec<MathLayout>> = Vec::new();
    let mut col_widths: Vec<f32> = Vec::new();
    let mut row_baselines: Vec<f32> = Vec::new();
    let mut row_descents: Vec<f32> = Vec::new();

    for row in rows {
      let mut layouts = Vec::new();
      let mut baseline: f32 = 0.0;
      let mut descent: f32 = 0.0;
      for (col, cell) in row.iter().enumerate() {
        let layout = self.layout_node(cell, style, base_style);
        if col >= col_widths.len() {
          col_widths.push(layout.width);
        } else {
          col_widths[col] = col_widths[col].max(layout.width);
        }
        baseline = baseline.max(layout.baseline);
        descent = descent.max(layout.height - layout.baseline);
        layouts.push(layout);
      }
      row_baselines.push(baseline);
      row_descents.push(descent);
      cell_layouts.push(layouts);
    }

    let width: f32 = col_widths.iter().copied().sum::<f32>()
      + col_spacing * (col_widths.len().saturating_sub(1)) as f32;
    let mut y = 0.0;
    let mut fragments = Vec::new();
    let mut table_baseline = 0.0;
    for (row_idx, layouts) in cell_layouts.into_iter().enumerate() {
      let baseline = row_baselines[row_idx];
      let descent = row_descents[row_idx];
      let row_height = baseline + descent;
      if row_idx == 0 {
        table_baseline = baseline + y;
      }
      let mut x = 0.0;
      for (col_idx, layout) in layouts.into_iter().enumerate() {
        let offset_y = y + (baseline - layout.baseline);
        let offset_x = x;
        for frag in layout.fragments {
          match frag {
            MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
              origin: Point::new(origin.x + offset_x, origin.y + offset_y),
              run,
            }),
            MathFragment::Rule(rect) => fragments.push(MathFragment::Rule(
              rect.translate(Point::new(offset_x, offset_y)),
            )),
          }
        }
        x += col_widths[col_idx] + col_spacing;
      }
      y += row_height + row_spacing;
    }

    MathLayout {
      width,
      height: y - row_spacing,
      baseline: table_baseline,
      fragments,
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
    let script_style = style.script();
    let mut fragments = Vec::new();
    let mut ascent = base_layout.baseline;
    let mut descent = base_layout.height - base_layout.baseline;

    let build_block =
      |scripts: &[(Option<MathNode>, Option<MathNode>)], _is_left: bool, ctx: &mut Self| {
        let mut block_width: f32 = 0.0;
        let mut block_ascent: f32 = 0.0;
        let mut block_descent: f32 = 0.0;
        let mut frags = Vec::new();
        for pair in scripts {
          let sup_layout = pair
            .1
            .as_ref()
            .map(|n| ctx.layout_node(n, &script_style, base_style));
          let sub_layout = pair
            .0
            .as_ref()
            .map(|n| ctx.layout_node(n, &script_style, base_style));
          let pair_width = sup_layout
            .as_ref()
            .map(|l| l.width)
            .unwrap_or(0.0)
            .max(sub_layout.as_ref().map(|l| l.width).unwrap_or(0.0));
          let x_start = block_width;
          if let Some(layout) = sup_layout {
            let y = (base_layout.baseline - layout.baseline - Self::script_gap(style)).min(0.0);
            for frag in layout.fragments {
              match frag {
                MathFragment::Glyph { origin, run } => frags.push(MathFragment::Glyph {
                  origin: Point::new(origin.x + x_start, origin.y + y),
                  run,
                }),
                MathFragment::Rule(rect) => {
                  frags.push(MathFragment::Rule(rect.translate(Point::new(x_start, y))))
                }
              }
            }
            block_ascent = block_ascent.max(layout.baseline - y);
            block_descent = block_descent.max(layout.height - (layout.baseline - y));
          }
          if let Some(layout) = sub_layout {
            let y = base_layout.baseline + Self::script_gap(style) - layout.baseline;
            for frag in layout.fragments {
              match frag {
                MathFragment::Glyph { origin, run } => frags.push(MathFragment::Glyph {
                  origin: Point::new(origin.x + x_start, origin.y + y),
                  run,
                }),
                MathFragment::Rule(rect) => {
                  frags.push(MathFragment::Rule(rect.translate(Point::new(x_start, y))))
                }
              }
            }
            block_ascent = block_ascent.max(layout.baseline - y);
            block_descent = block_descent.max(layout.height - (layout.baseline - y));
          }
          block_width += pair_width + Self::script_gap(style);
        }
        (block_width, block_ascent, block_descent, frags)
      };

    let (pre_width, pre_ascent, pre_descent, pre_frags) = build_block(pre, true, self);
    let (post_width, post_ascent, post_descent, post_frags) = build_block(post, false, self);
    let width_left = pre_width;
    let width_right = post_width;
    ascent = ascent.max(pre_ascent).max(post_ascent);
    descent = descent.max(pre_descent).max(post_descent);

    // Position fragments
    for frag in pre_frags {
      match frag {
        MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
          origin: Point::new(origin.x, origin.y),
          run,
        }),
        MathFragment::Rule(rect) => fragments.push(MathFragment::Rule(rect)),
      }
    }
    for frag in base_layout.fragments {
      match frag {
        MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
          origin: Point::new(origin.x + width_left, origin.y),
          run,
        }),
        MathFragment::Rule(rect) => fragments.push(MathFragment::Rule(
          rect.translate(Point::new(width_left, 0.0)),
        )),
      }
    }
    for frag in post_frags {
      match frag {
        MathFragment::Glyph { origin, run } => fragments.push(MathFragment::Glyph {
          origin: Point::new(origin.x + width_left + base_layout.width, origin.y),
          run,
        }),
        MathFragment::Rule(rect) => fragments.push(MathFragment::Rule(
          rect.translate(Point::new(width_left + base_layout.width, 0.0)),
        )),
      }
    }

    MathLayout {
      width: width_left + base_layout.width + width_right,
      height: ascent + descent,
      baseline: ascent,
      fragments,
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
        style.display = *display;
        self.layout_row(children, &style, base_style)
      }
      MathNode::Row(children) => self.layout_row(children, style, base_style),
      MathNode::Identifier { text, italic, bold } => {
        self.layout_glyphs(text, base_style, style, *italic, *bold)
      }
      MathNode::Number { text, italic, bold } => {
        self.layout_glyphs(text, base_style, style, *italic, *bold)
      }
      MathNode::Operator {
        text,
        stretchy: _,
        italic,
        bold,
      } => {
        // Stretching handled during row aggregation by scaling font size heuristically.
        self.layout_glyphs(text, base_style, style, *italic, *bold)
      }
      MathNode::Text { text, italic, bold } => {
        self.layout_glyphs(text, base_style, style, *italic, *bold)
      }
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
      MathNode::Table(rows) => self.layout_table(rows, style, base_style),
      MathNode::Multiscripts {
        base,
        prescripts,
        postscripts,
      } => self.layout_multiscripts(base, prescripts, postscripts, style, base_style),
    }
  }

  /// Public entrypoint: layout a MathNode tree using the provided style.
  pub fn layout(&mut self, node: &MathNode, style: &ComputedStyle) -> MathLayout {
    let math_style = MathStyle {
      font_size: style.font_size,
      display: false,
    };
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

  #[test]
  fn table_layout_completes() {
    let style = ComputedStyle::default();
    let node = MathNode::Table(vec![
      vec![MathNode::Identifier {
        text: "a".into(),
        italic: true,
        bold: false,
      }],
      vec![MathNode::Identifier {
        text: "b".into(),
        italic: true,
        bold: false,
      }],
    ]);
    let layout = layout_mathml(&node, &style, &FontContext::empty());
    assert!(layout.width > 0.0);
    assert!(layout.height > 0.0);
    assert!(layout.baseline > 0.0);
  }

  #[test]
  fn table_layout_with_font_db() {
    let style = ComputedStyle::default();
    let node = MathNode::Table(vec![
      vec![
        MathNode::Number {
          text: "1".into(),
          italic: false,
          bold: false,
        },
        MathNode::Number {
          text: "2".into(),
          italic: false,
          bold: false,
        },
      ],
      vec![
        MathNode::Number {
          text: "3".into(),
          italic: false,
          bold: false,
        },
        MathNode::Number {
          text: "4".into(),
          italic: false,
          bold: false,
        },
      ],
    ]);
    let ctx = FontContext::new();
    let layout = layout_mathml(&node, &style, &ctx);
    assert!(layout.width > 0.0);
    assert!(layout.height > 0.0);
  }

  #[test]
  fn mathvariant_controls_token_style() {
    let dom =
      crate::dom::parse_html("<math><mi mathvariant=\"normal\">x</mi></math>").expect("dom");
    fn find_math<'a>(node: &'a crate::dom::DomNode) -> Option<&'a crate::dom::DomNode> {
      if node
        .tag_name()
        .map(|t| t.eq_ignore_ascii_case("math"))
        .unwrap_or(false)
      {
        return Some(node);
      }
      node.children.iter().find_map(find_math)
    }
    let math_node = find_math(&dom).expect("math element");
    let parsed = parse_mathml(math_node).expect("math parsed");
    let MathNode::Math { children, .. } = parsed else {
      panic!("expected math root");
    };
    let MathNode::Identifier { italic, bold, text } = &children[0] else {
      panic!("expected identifier child");
    };
    assert_eq!(text, "x");
    assert!(!italic);
    assert!(!bold);
  }
}
