//! Page rule resolution and page box sizing utilities.

use std::collections::HashMap;

use crate::css::types::PropertyValue;
use crate::css::types::{CollectedPageRule, PageMarginArea, PagePseudoClass, PageSelector};
use crate::geometry::{Point, Size};
use crate::style::display::Display;
use crate::style::properties::{apply_declaration_with_base, resolve_pending_logical_properties};
use crate::style::values::{Length, LengthUnit};
use crate::style::ComputedStyle;

/// Logical side for page selectors (:left/:right).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PageSide {
  Left,
  Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PageOrientation {
  Portrait,
  Landscape,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PageNamedSize {
  A5,
  A4,
  A3,
  Letter,
  Legal,
  Tabloid,
}

#[derive(Debug, Clone, Default)]
struct PageSizeSpec {
  width: Option<Length>,
  height: Option<Length>,
  named: Option<PageNamedSize>,
  orientation: Option<PageOrientation>,
}

#[derive(Debug, Clone, Default)]
struct PageProperties {
  size: Option<PageSizeSpec>,
  margin_top: Option<Length>,
  margin_right: Option<Length>,
  margin_bottom: Option<Length>,
  margin_left: Option<Length>,
  bleed: Option<Length>,
  trim: Option<Length>,
}

/// Resolved page box metrics after applying @page rules.
#[derive(Debug, Clone)]
pub struct ResolvedPageStyle {
  pub page_size: Size,
  pub total_size: Size,
  pub content_size: Size,
  pub content_origin: Point,
  pub margin_top: f32,
  pub margin_right: f32,
  pub margin_bottom: f32,
  pub margin_left: f32,
  pub bleed: f32,
  pub trim: f32,
  pub margin_boxes: HashMap<PageMarginArea, ComputedStyle>,
}

/// Resolve page styles for a specific page index/name/side.
pub fn resolve_page_style(
  rules: &[CollectedPageRule<'_>],
  page_index: usize,
  page_name: Option<&str>,
  side: PageSide,
  fallback_size: Size,
  root_font_size: f32,
) -> ResolvedPageStyle {
  let defaults = ComputedStyle::default();
  let mut props = PageProperties::default();
  let mut margin_styles: HashMap<PageMarginArea, ComputedStyle> = HashMap::new();

  let mut matching: Vec<(&CollectedPageRule<'_>, u8)> = Vec::new();
  for rule in rules {
    let mut matched_spec: Option<u8> = None;
    for selector in &rule.rule.selectors {
      if selector_matches(selector, page_index, page_name, side) {
        let spec = selector_specificity(selector);
        matched_spec = Some(matched_spec.map_or(spec, |s| s.max(spec)));
      }
    }

    if let Some(spec) = matched_spec {
      matching.push((rule, spec));
    }
  }

  matching.sort_by(|a, b| {
    a.0
      .layer_order
      .cmp(&b.0.layer_order)
      .then(a.1.cmp(&b.1))
      .then(a.0.order.cmp(&b.0.order))
  });

  for (rule, _) in &matching {
    for decl in &rule.rule.declarations {
      apply_page_declaration(&mut props, decl);
    }

    for margin_rule in &rule.rule.margin_rules {
      let style = margin_styles
        .entry(margin_rule.area)
        .or_insert_with(|| default_margin_style(root_font_size));
      for decl in &margin_rule.declarations {
        apply_declaration_with_base(
          style,
          decl,
          &defaults,
          &defaults,
          None,
          root_font_size,
          root_font_size,
          fallback_size,
        );
      }
    }
  }

  for style in margin_styles.values_mut() {
    resolve_pending_logical_properties(style);
    if matches!(style.display, Display::Inline) {
      style.display = Display::Block;
    }
  }

  let (page_width, page_height) = resolve_page_size(&props, fallback_size, root_font_size);
  let bleed = props
    .bleed
    .and_then(|l| {
      resolve_length_on_axis(
        &l,
        page_width.max(page_height),
        fallback_size,
        root_font_size,
      )
    })
    .unwrap_or(0.0)
    .max(0.0);
  let trim = props
    .trim
    .and_then(|l| {
      resolve_length_on_axis(
        &l,
        page_width.max(page_height),
        fallback_size,
        root_font_size,
      )
    })
    .unwrap_or(0.0)
    .max(0.0);

  let margin_top = resolve_length_on_axis(
    &props.margin_top.unwrap_or_else(|| Length::px(0.0)),
    page_height,
    fallback_size,
    root_font_size,
  )
  .unwrap_or(0.0)
  .max(0.0);
  let margin_bottom = resolve_length_on_axis(
    &props.margin_bottom.unwrap_or_else(|| Length::px(0.0)),
    page_height,
    fallback_size,
    root_font_size,
  )
  .unwrap_or(0.0)
  .max(0.0);
  let margin_left = resolve_length_on_axis(
    &props.margin_left.unwrap_or_else(|| Length::px(0.0)),
    page_width,
    fallback_size,
    root_font_size,
  )
  .unwrap_or(0.0)
  .max(0.0);
  let margin_right = resolve_length_on_axis(
    &props.margin_right.unwrap_or_else(|| Length::px(0.0)),
    page_width,
    fallback_size,
    root_font_size,
  )
  .unwrap_or(0.0)
  .max(0.0);

  let content_width = (page_width - 2.0 * trim - margin_left - margin_right).max(0.0);
  let content_height = (page_height - 2.0 * trim - margin_top - margin_bottom).max(0.0);

  let content_origin = Point::new(bleed + trim + margin_left, bleed + trim + margin_top);
  let total_size = Size::new(page_width + 2.0 * bleed, page_height + 2.0 * bleed);

  ResolvedPageStyle {
    page_size: Size::new(page_width, page_height),
    total_size,
    content_size: Size::new(content_width, content_height),
    content_origin,
    margin_top,
    margin_right,
    margin_bottom,
    margin_left,
    bleed,
    trim,
    margin_boxes: margin_styles,
  }
}

fn selector_matches(
  selector: &PageSelector,
  page_index: usize,
  page_name: Option<&str>,
  side: PageSide,
) -> bool {
  if let Some(name) = &selector.name {
    if let Some(actual) = page_name {
      if actual != name {
        return false;
      }
    } else {
      return false;
    }
  }

  if let Some(pseudo) = selector.pseudo {
    match pseudo {
      PagePseudoClass::First => {
        if page_index != 0 {
          return false;
        }
      }
      PagePseudoClass::Left => {
        if side != PageSide::Left {
          return false;
        }
      }
      PagePseudoClass::Right => {
        if side != PageSide::Right {
          return false;
        }
      }
      PagePseudoClass::Blank => return false,
    }
  }

  true
}

fn selector_specificity(selector: &crate::css::types::PageSelector) -> u8 {
  let mut spec = 0;
  if selector.name.is_some() {
    spec += 2;
  }
  if selector.pseudo.is_some() {
    spec += 1;
  }
  spec
}

fn apply_page_declaration(props: &mut PageProperties, decl: &crate::css::types::Declaration) {
  match decl.property.as_str() {
    "size" => {
      if let Some(size) = parse_page_size_value(&decl.value) {
        props.size = Some(size);
      }
    }
    "margin" => {
      if let Some(values) = parse_margin_shorthand(&decl.value) {
        props.margin_top = Some(values[0]);
        props.margin_right = Some(values[1]);
        props.margin_bottom = Some(values[2]);
        props.margin_left = Some(values[3]);
      }
    }
    "margin-top" => {
      if let Some(len) = length_from_value(&decl.value) {
        props.margin_top = Some(len);
      }
    }
    "margin-right" => {
      if let Some(len) = length_from_value(&decl.value) {
        props.margin_right = Some(len);
      }
    }
    "margin-bottom" => {
      if let Some(len) = length_from_value(&decl.value) {
        props.margin_bottom = Some(len);
      }
    }
    "margin-left" => {
      if let Some(len) = length_from_value(&decl.value) {
        props.margin_left = Some(len);
      }
    }
    "bleed" => {
      if let Some(len) = length_from_value(&decl.value) {
        props.bleed = Some(len);
      }
    }
    "trim" => {
      if let Some(len) = length_from_value(&decl.value) {
        props.trim = Some(len);
      }
    }
    _ => {}
  }
}

fn parse_page_size_value(value: &PropertyValue) -> Option<PageSizeSpec> {
  let mut spec = PageSizeSpec::default();
  match value {
    PropertyValue::Keyword(kw) => {
      let lower = kw.to_ascii_lowercase();
      if lower == "auto" {
        return None;
      }
      if let Some(named) = named_size(&lower) {
        spec.named = Some(named);
      } else if let Some(orientation) = orientation_from_keyword(&lower) {
        spec.orientation = Some(orientation);
      }
    }
    PropertyValue::Length(len) => {
      spec.width = Some(*len);
    }
    PropertyValue::Multiple(values) => {
      for part in values {
        match part {
          PropertyValue::Length(len) => {
            if spec.width.is_none() {
              spec.width = Some(*len);
            } else if spec.height.is_none() {
              spec.height = Some(*len);
            }
          }
          PropertyValue::Keyword(kw) => {
            let lower = kw.to_ascii_lowercase();
            if let Some(orientation) = orientation_from_keyword(&lower) {
              spec.orientation = Some(orientation);
              continue;
            }
            if let Some(named) = named_size(&lower) {
              spec.named = Some(named);
            }
          }
          _ => {}
        }
      }
    }
    _ => {}
  }

  if spec.width.is_some()
    || spec.height.is_some()
    || spec.named.is_some()
    || spec.orientation.is_some()
  {
    Some(spec)
  } else {
    None
  }
}

fn orientation_from_keyword(value: &str) -> Option<PageOrientation> {
  match value {
    "landscape" => Some(PageOrientation::Landscape),
    "portrait" => Some(PageOrientation::Portrait),
    _ => None,
  }
}

fn named_size(value: &str) -> Option<PageNamedSize> {
  match value {
    "a5" => Some(PageNamedSize::A5),
    "a4" => Some(PageNamedSize::A4),
    "a3" => Some(PageNamedSize::A3),
    "letter" => Some(PageNamedSize::Letter),
    "legal" => Some(PageNamedSize::Legal),
    "tabloid" => Some(PageNamedSize::Tabloid),
    _ => None,
  }
}

fn length_from_value(value: &PropertyValue) -> Option<Length> {
  match value {
    PropertyValue::Length(l) => Some(*l),
    PropertyValue::Number(n) => Some(Length::px(*n)),
    PropertyValue::Percentage(p) => Some(Length::percent(*p)),
    PropertyValue::Keyword(k) if k.eq_ignore_ascii_case("auto") => Some(Length::px(0.0)),
    _ => None,
  }
}

fn parse_margin_shorthand(value: &PropertyValue) -> Option<[Length; 4]> {
  let mut values = Vec::new();
  match value {
    PropertyValue::Multiple(list) => {
      for part in list {
        if let Some(len) = length_from_value(part) {
          values.push(len);
        }
      }
    }
    _ => {
      if let Some(len) = length_from_value(value) {
        values.push(len);
      }
    }
  }

  if values.is_empty() {
    return None;
  }

  let resolved = match values.len() {
    1 => [values[0], values[0], values[0], values[0]],
    2 => [values[0], values[1], values[0], values[1]],
    3 => [values[0], values[1], values[2], values[1]],
    _ => [values[0], values[1], values[2], values[3]],
  };
  Some(resolved)
}

fn resolve_page_size(props: &PageProperties, fallback: Size, root_font_size: f32) -> (f32, f32) {
  let mut width = fallback.width;
  let mut height = fallback.height;

  if let Some(spec) = &props.size {
    if let Some(named) = spec.named {
      let dims = named.dimensions();
      width = dims.width;
      height = dims.height;
    }

    if let Some(w) = spec
      .width
      .and_then(|l| resolve_length_on_axis(&l, fallback.width, fallback, root_font_size))
    {
      width = w;
    }
    if let Some(h) = spec
      .height
      .and_then(|l| resolve_length_on_axis(&l, fallback.height, fallback, root_font_size))
    {
      height = h;
    }

    if let Some(orientation) = spec.orientation {
      match orientation {
        PageOrientation::Landscape if height > width => {
          std::mem::swap(&mut width, &mut height);
        }
        PageOrientation::Portrait if width > height => {
          std::mem::swap(&mut width, &mut height);
        }
        _ => {}
      }
    }
  }

  (width.max(0.0), height.max(0.0))
}

fn resolve_length_on_axis(
  length: &Length,
  axis: f32,
  viewport: Size,
  root_font_size: f32,
) -> Option<f32> {
  let percent_base = if length.unit == LengthUnit::Percent {
    Some(axis)
  } else {
    None
  };
  length.resolve_with_context(
    percent_base,
    viewport.width,
    viewport.height,
    root_font_size,
    root_font_size,
  )
}

fn default_margin_style(root_font_size: f32) -> ComputedStyle {
  let mut style = ComputedStyle::default();
  style.display = Display::Block;
  style.root_font_size = root_font_size;
  style
}

impl PageNamedSize {
  fn dimensions(self) -> Size {
    match self {
      PageNamedSize::A5 => Size::new(mm_to_px(148.0), mm_to_px(210.0)),
      PageNamedSize::A4 => Size::new(mm_to_px(210.0), mm_to_px(297.0)),
      PageNamedSize::A3 => Size::new(mm_to_px(297.0), mm_to_px(420.0)),
      PageNamedSize::Letter => Size::new(in_to_px(8.5), in_to_px(11.0)),
      PageNamedSize::Legal => Size::new(in_to_px(8.5), in_to_px(14.0)),
      PageNamedSize::Tabloid => Size::new(in_to_px(11.0), in_to_px(17.0)),
    }
  }
}

fn mm_to_px(mm: f32) -> f32 {
  mm / 25.4 * 96.0
}

fn in_to_px(inches: f32) -> f32 {
  inches * 96.0
}
