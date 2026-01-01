//! Box generation - transforms styled DOM into BoxTree
//!
//! Implements the CSS box generation algorithm that determines what boxes
//! are created from DOM elements.
//!
//! CSS Specification: CSS 2.1 Section 9.2 - Box Generation
//! <https://www.w3.org/TR/CSS21/visuren.html#box-gen>

use crate::compat::CompatProfile;
use crate::debug::runtime;
use crate::dom::DomNode;
use crate::dom::DomNodeType;
use crate::dom::ElementRef;
use crate::dom::SVG_NAMESPACE;
use crate::error::{RenderStage, Result};
use crate::geometry::Size;
use crate::html::image_attrs;
use crate::render_control::check_active_periodic;
use crate::style::color::Rgba;
use crate::style::computed::Visibility;
use crate::style::content::ContentContext;
use crate::style::content::ContentItem;
use crate::style::content::ContentValue;
use crate::style::content::CounterStyle;
use crate::style::counter_styles::CounterStyleName;
use crate::style::counters::CounterManager;
use crate::style::counters::CounterSet;
use crate::style::defaults::parse_color_attribute;
use crate::style::display::Display;
use crate::style::display::FormattingContextType;
use crate::style::media::MediaQuery;
use crate::style::types::Appearance;
use crate::style::types::FontStyle;
use crate::style::types::ListStyleType;
use crate::style::types::TextTransform;
use crate::style::ComputedStyle;
use crate::svg::parse_svg_length_px;
use crate::svg::svg_intrinsic_dimensions_from_attributes;
use crate::tree::anonymous::AnonymousBoxCreator;
use crate::tree::box_tree::BoxNode;
use crate::tree::box_tree::BoxTree;
use crate::tree::box_tree::BoxType;
use crate::tree::box_tree::ForeignObjectInfo;
use crate::tree::box_tree::FormControl;
use crate::tree::box_tree::FormControlKind;
use crate::tree::box_tree::MarkerContent;
use crate::tree::box_tree::MathReplaced;
use crate::tree::box_tree::PictureSource;
use crate::tree::box_tree::ReplacedBox;
use crate::tree::box_tree::ReplacedType;
use crate::tree::box_tree::SizesList;
use crate::tree::box_tree::SrcsetCandidate;
use crate::tree::box_tree::SvgContent;
use crate::tree::box_tree::SvgDocumentCssInjection;
use crate::tree::box_tree::TextControlKind;
use crate::tree::debug::DebugInfo;
use crate::tree::table_fixup::TableStructureFixer;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::OnceLock;
use std::time::Instant;

#[cfg(any(test, feature = "box_generation_demo"))]
pub use crate::tree::box_generation_demo::{
  BoxGenerationConfig, BoxGenerationError, BoxGenerator, DOMNode,
};
pub(crate) fn parse_srcset(attr: &str) -> Vec<SrcsetCandidate> {
  image_attrs::parse_srcset(attr)
}

pub(crate) fn parse_sizes(attr: &str) -> Option<SizesList> {
  image_attrs::parse_sizes(attr)
}

// ============================================================================
// StyledNode-based Box Generation (for real DOM/style pipeline)
// ============================================================================

use crate::style::cascade::StyledNode;

/// Options that control how the box tree is generated from styled DOM.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BoxGenerationOptions {
  /// Compatibility profile controlling whether site-specific heuristics are enabled.
  pub compat_profile: CompatProfile,
}

impl Default for BoxGenerationOptions {
  fn default() -> Self {
    Self {
      compat_profile: CompatProfile::Standards,
    }
  }
}

impl BoxGenerationOptions {
  /// Creates a new options struct with defaults.
  pub fn new() -> Self {
    Self::default()
  }

  /// Sets the compatibility profile for box generation.
  pub fn with_compat_profile(mut self, profile: CompatProfile) -> Self {
    self.compat_profile = profile;
    self
  }

  fn site_compat_hacks_enabled(&self) -> bool {
    self.compat_profile.site_compat_hacks_enabled()
  }
}

const BOX_GEN_DEADLINE_STRIDE: usize = 256;
const MAX_EMBEDDED_SVG_CSS_BYTES: usize = 64 * 1024;

#[derive(Debug, Default, Clone, Copy)]
struct SvgSerializationProfile {
  calls: usize,
  bytes: usize,
  time_ms: f64,
}

thread_local! {
  static SVG_SERIALIZATION_PROFILE: RefCell<Option<SvgSerializationProfile>> = RefCell::new(None);
}

fn svg_serialization_profile_enabled() -> bool {
  runtime::runtime_toggles().truthy("FASTR_SVG_PROFILE")
}

fn enable_svg_serialization_profile() {
  SVG_SERIALIZATION_PROFILE.with(|cell| {
    *cell.borrow_mut() = Some(SvgSerializationProfile::default());
  });
}

fn take_svg_serialization_profile() -> Option<SvgSerializationProfile> {
  SVG_SERIALIZATION_PROFILE.with(|cell| cell.borrow_mut().take())
}

fn record_svg_serialization(duration: std::time::Duration, bytes: usize) {
  SVG_SERIALIZATION_PROFILE.with(|cell| {
    if let Some(profile) = cell.borrow_mut().as_mut() {
      profile.calls += 1;
      profile.bytes += bytes;
      profile.time_ms += duration.as_secs_f64() * 1000.0;
    }
  });
}

#[derive(Debug, Clone)]
struct SvgDocumentCssPolicy {
  embedded_style_element: Option<Arc<str>>,
  max_embedded_svgs: Option<usize>,
  replaced_svg_count: usize,
  forced: Option<bool>,
}

fn svg_embed_document_css_override() -> Option<bool> {
  let toggles = runtime::runtime_toggles();
  let raw = toggles.get("FASTR_SVG_EMBED_DOCUMENT_CSS")?;
  let trimmed = raw.trim();
  if trimmed.is_empty() {
    return None;
  }
  let lower = trimmed.to_ascii_lowercase();
  Some(!matches!(lower.as_str(), "0" | "false" | "off"))
}

fn svg_embed_document_css_max_svgs() -> Option<usize> {
  runtime::runtime_toggles().usize("FASTR_SVG_EMBED_DOCUMENT_CSS_MAX_SVGS")
}

fn build_svg_cdata_style_element(css: &str) -> String {
  // Ensure embedded document CSS stays XML-safe by wrapping it in CDATA and splitting any
  // terminators that would otherwise close the section.
  let mut out = String::with_capacity(css.len() + 32);
  out.push_str("<style><![CDATA[");
  let mut last = 0;
  for (idx, _) in css.match_indices("]]>") {
    out.push_str(&css[last..idx]);
    out.push_str("]]]]><![CDATA[>");
    last = idx + 3;
  }
  out.push_str(&css[last..]);
  out.push_str("]]></style>");
  out
}

fn clone_starting_style(style: &Option<Arc<ComputedStyle>>) -> Option<Arc<ComputedStyle>> {
  style.as_ref().map(Arc::clone)
}

struct BoxGenerationPrepass<'a> {
  document_css: String,
  svg_document_css: SvgDocumentCssPolicy,
  picture_sources: PictureSourceLookup,
  styled_lookup: StyledLookup<'a>,
}

struct StyledLookup<'a> {
  nodes: Vec<Option<&'a StyledNode>>,
}

impl<'a> StyledLookup<'a> {
  fn new() -> Self {
    Self { nodes: vec![None] }
  }

  fn insert(&mut self, node_id: usize, node: &'a StyledNode) {
    if node_id == self.nodes.len() {
      self.nodes.push(Some(node));
      return;
    }

    if node_id >= self.nodes.len() {
      self.nodes.resize(node_id + 1, None);
    }
    self.nodes[node_id] = Some(node);
  }

  fn get(&self, node_id: usize) -> Option<&'a StyledNode> {
    self.nodes.get(node_id).copied().flatten()
  }
}

struct PictureSourceLookup {
  entries: Vec<Option<Vec<PictureSource>>>,
}

impl PictureSourceLookup {
  fn new() -> Self {
    Self {
      entries: vec![None],
    }
  }

  fn insert(&mut self, node_id: usize, sources: Vec<PictureSource>) {
    if node_id == self.entries.len() {
      self.entries.push(Some(sources));
      return;
    }

    if node_id >= self.entries.len() {
      self.entries.resize(node_id + 1, None);
    }
    self.entries[node_id] = Some(sources);
  }

  fn take(&mut self, node_id: usize) -> Vec<PictureSource> {
    self
      .entries
      .get_mut(node_id)
      .and_then(Option::take)
      .unwrap_or_default()
  }
}

fn collect_box_generation_prepass<'a>(
  styled: &'a StyledNode,
  deadline_counter: &mut usize,
) -> Result<BoxGenerationPrepass<'a>> {
  struct CssState {
    enabled: bool,
  }

  fn walk<'a>(
    node: &'a StyledNode,
    out: &mut BoxGenerationPrepass<'a>,
    deadline_counter: &mut usize,
    max_css_bytes: usize,
    css: &mut CssState,
    css_allowed: bool,
    svg_count_allowed: bool,
  ) -> Result<()> {
    check_active_periodic(
      deadline_counter,
      BOX_GEN_DEADLINE_STRIDE,
      RenderStage::Cascade,
    )?;

    out.styled_lookup.insert(node.node_id, node);
    if let Some((img_id, sources)) = picture_sources_for(node) {
      out.picture_sources.insert(img_id, sources);
    }

    let mut children_css_allowed = css_allowed;
    if css_allowed && css.enabled {
      if let Some(tag) = node.node.tag_name() {
        if tag.eq_ignore_ascii_case("template")
          && node.node.get_attribute_ref("shadowroot").is_none()
          && node.node.get_attribute_ref("shadowrootmode").is_none()
        {
          children_css_allowed = false;
        } else if tag.eq_ignore_ascii_case("style") {
          for child in node.children.iter() {
            if let Some(text) = child.node.text_content() {
              out.document_css.push_str(text);
              out.document_css.push('\n');
              if out.document_css.len() > max_css_bytes {
                out.document_css.clear();
                css.enabled = false;
                break;
              }
            }
          }
        }
      }
    }

    let mut children_svg_count_allowed = svg_count_allowed;
    if svg_count_allowed {
      if node.styles.display == Display::None {
        children_svg_count_allowed = false;
      } else if let Some(tag) = node.node.tag_name() {
        if is_replaced_element(tag) && node.styles.display != Display::Contents {
          let is_object_with_fallback = tag.eq_ignore_ascii_case("object")
            && node
              .node
              .get_attribute_ref("data")
              .map(|d| d.is_empty())
              .unwrap_or(true);
          if !is_object_with_fallback {
            if tag.eq_ignore_ascii_case("svg") {
              out.svg_document_css.replaced_svg_count += 1;
            }
            children_svg_count_allowed = false;
          }
        }
      }
    }

    for child in node.children.iter() {
      walk(
        child,
        out,
        deadline_counter,
        max_css_bytes,
        css,
        children_css_allowed,
        children_svg_count_allowed,
      )?;
    }
    Ok(())
  }

  let max_css_bytes = foreign_object_css_limit_bytes().max(MAX_EMBEDDED_SVG_CSS_BYTES);
  let forced = svg_embed_document_css_override();
  let max_embedded_svgs = svg_embed_document_css_max_svgs();
  let mut out = BoxGenerationPrepass {
    document_css: String::new(),
    svg_document_css: SvgDocumentCssPolicy {
      embedded_style_element: None,
      max_embedded_svgs,
      replaced_svg_count: 0,
      forced,
    },
    picture_sources: PictureSourceLookup::new(),
    styled_lookup: StyledLookup::new(),
  };
  let mut css = CssState { enabled: true };
  walk(
    styled,
    &mut out,
    deadline_counter,
    max_css_bytes,
    &mut css,
    true,
    true,
  )?;

  let css_trimmed = out.document_css.trim();
  let css_size_ok = !css_trimmed.is_empty() && out.document_css.len() <= MAX_EMBEDDED_SVG_CSS_BYTES;
  let allow_embed = if !css_size_ok {
    false
  } else if let Some(forced) = out.svg_document_css.forced {
    forced
  } else if let Some(max) = out.svg_document_css.max_embedded_svgs {
    out.svg_document_css.replaced_svg_count <= max
  } else {
    true
  };
  if allow_embed {
    out.svg_document_css.embedded_style_element = Some(Arc::<str>::from(
      build_svg_cdata_style_element(&out.document_css),
    ));
  }

  Ok(out)
}

fn build_box_tree_root(
  styled: &StyledNode,
  options: &BoxGenerationOptions,
  deadline_counter: &mut usize,
) -> Result<BoxNode> {
  let BoxGenerationPrepass {
    document_css,
    svg_document_css,
    mut picture_sources,
    styled_lookup,
  } = collect_box_generation_prepass(styled, deadline_counter)?;
  let mut counters = CounterManager::new_with_styles(styled.styles.counter_styles.clone());
  counters.enter_scope();
  let mut roots = Vec::new();
  let svg_profile = svg_serialization_profile_enabled();
  if svg_profile {
    enable_svg_serialization_profile();
  }
  let result = generate_boxes_for_styled_into(
    styled,
    &styled_lookup,
    &mut counters,
    true,
    &document_css,
    svg_document_css.embedded_style_element.as_ref(),
    &mut picture_sources,
    options,
    deadline_counter,
    &mut roots,
  );
  counters.leave_scope();
  if svg_profile {
    if let Some(profile) = take_svg_serialization_profile() {
      eprintln!(
        "[svg-serialize] calls={} bytes={} time_ms={:.2} embed_doc_css={} doc_css_bytes={} svgs={} max_svgs={} forced={}",
        profile.calls,
        profile.bytes,
        profile.time_ms,
        svg_document_css.embedded_style_element.is_some(),
        document_css.len(),
        svg_document_css.replaced_svg_count,
        svg_document_css.max_embedded_svgs.unwrap_or(usize::MAX),
        svg_document_css
          .forced
          .map(|v| if v { "on" } else { "off" })
          .unwrap_or("auto")
      );
    }
  }
  result?;
  match roots.len() {
    0 => Ok(BoxNode::new_block(
      Arc::new(ComputedStyle::default()),
      FormattingContextType::Block,
      Vec::new(),
    )),
    1 => Ok(roots.remove(0)),
    _ => Ok(BoxNode::new_anonymous_block(
      Arc::new(ComputedStyle::default()),
      roots,
    )),
  }
}

/// Generates a BoxTree from a StyledNode tree
///
/// This is the main entry point for box generation from styled DOM.
/// It recursively converts each StyledNode into the appropriate BoxNode type.
///
/// # Arguments
///
/// * `styled` - The root of the styled node tree
///
/// # Returns
///
/// A `BoxTree` containing the generated box structure
pub fn generate_box_tree(styled: &StyledNode) -> Result<BoxTree> {
  generate_box_tree_with_options(styled, &BoxGenerationOptions::default())
}

/// Generates a BoxTree from a StyledNode tree with custom options.
pub fn generate_box_tree_with_options(
  styled: &StyledNode,
  options: &BoxGenerationOptions,
) -> Result<BoxTree> {
  let mut deadline_counter = 0usize;
  let root = build_box_tree_root(styled, options, &mut deadline_counter)?;
  Ok(BoxTree::new(root))
}

/// Generates a BoxTree from a StyledNode tree and applies CSS-mandated anonymous box fixup.
///
/// This wraps inline-level runs in anonymous blocks and text in anonymous inline boxes so the
/// resulting tree satisfies the CSS 2.1 box-generation invariants (required for flex/grid/blocks
/// that contain raw text to be layed out correctly).
pub fn generate_box_tree_with_anonymous_fixup(styled: &StyledNode) -> Result<BoxTree> {
  generate_box_tree_with_anonymous_fixup_with_options(styled, &BoxGenerationOptions::default())
}

/// Generates a BoxTree from a StyledNode tree, applies anonymous box fixup, and
/// allows customizing generation behavior via options.
pub fn generate_box_tree_with_anonymous_fixup_with_options(
  styled: &StyledNode,
  options: &BoxGenerationOptions,
) -> Result<BoxTree> {
  let mut deadline_counter = 0usize;
  let root = build_box_tree_root(styled, options, &mut deadline_counter)?;
  let fixed_root = AnonymousBoxCreator::fixup_tree_with_deadline(root, &mut deadline_counter)?;
  let fixed_root =
    TableStructureFixer::fixup_tree_internals_with_deadline(fixed_root, &mut deadline_counter)?;
  Ok(BoxTree::new(fixed_root))
}

fn attach_styled_id(mut node: BoxNode, styled: &StyledNode) -> BoxNode {
  node.styled_node_id = Some(styled.node_id);
  node
}

fn push_escaped_text(out: &mut String, value: &str) {
  let bytes = value.as_bytes();
  let mut last = 0usize;
  while let Some(rel_idx) = memchr::memchr2(b'&', b'<', &bytes[last..]) {
    let idx = last + rel_idx;
    if last < idx {
      out.push_str(&value[last..idx]);
    }
    match bytes[idx] {
      b'&' => out.push_str("&amp;"),
      b'<' => out.push_str("&lt;"),
      _ => unreachable!("memchr2 returned non-matching byte"),
    }
    last = idx + 1;
  }
  if last < value.len() {
    out.push_str(&value[last..]);
  }
}

fn push_escaped_attr(out: &mut String, value: &str) {
  let bytes = value.as_bytes();
  let mut last = 0usize;
  while let Some(rel_idx) = memchr::memchr3(b'&', b'<', b'"', &bytes[last..]) {
    let idx = last + rel_idx;
    if last < idx {
      out.push_str(&value[last..idx]);
    }
    match bytes[idx] {
      b'&' => out.push_str("&amp;"),
      b'<' => out.push_str("&lt;"),
      b'"' => out.push_str("&quot;"),
      _ => unreachable!("memchr3 returned non-matching byte"),
    }
    last = idx + 1;
  }
  if last < value.len() {
    out.push_str(&value[last..]);
  }
}

fn dom_subtree_from_styled(node: &StyledNode) -> DomNode {
  DomNode {
    node_type: node.node.node_type.clone(),
    children: node.children.iter().map(dom_subtree_from_styled).collect(),
  }
}

fn escape_attr(value: &str) -> String {
  let mut out = String::with_capacity(value.len());
  push_escaped_attr(&mut out, value);
  out
}

fn escape_text(value: &str) -> String {
  let mut out = String::with_capacity(value.len());
  push_escaped_text(&mut out, value);
  out
}

enum ComposedChildren<'a> {
  Slice(&'a [StyledNode]),
  Refs(Vec<&'a StyledNode>),
}

impl<'a> ComposedChildren<'a> {
  fn len(&self) -> usize {
    match self {
      Self::Slice(children) => children.len(),
      Self::Refs(children) => children.len(),
    }
  }

  fn get(&self, idx: usize) -> &'a StyledNode {
    match self {
      Self::Slice(children) => &children[idx],
      Self::Refs(children) => children[idx],
    }
  }
}

fn composed_children<'a>(
  styled: &'a StyledNode,
  lookup: &'a StyledLookup<'a>,
) -> ComposedChildren<'a> {
  if let Some(shadow_root) = styled
    .children
    .iter()
    .find(|c| matches!(c.node.node_type, crate::dom::DomNodeType::ShadowRoot { .. }))
  {
    return ComposedChildren::Refs(vec![shadow_root]);
  }

  if matches!(styled.node.node_type, crate::dom::DomNodeType::Slot { .. })
    && !styled.slotted_node_ids.is_empty()
  {
    let mut resolved: Vec<&'a StyledNode> = Vec::with_capacity(styled.slotted_node_ids.len());
    for id in &styled.slotted_node_ids {
      if let Some(node) = lookup.get(*id) {
        resolved.push(node);
      }
    }
    return ComposedChildren::Refs(resolved);
  }

  ComposedChildren::Slice(&styled.children)
}

fn normalize_mime_type(value: &str) -> Option<String> {
  let base = value.split(';').next().unwrap_or("").trim();
  if base.is_empty() {
    None
  } else {
    Some(base.to_ascii_lowercase())
  }
}

fn picture_sources_for(styled: &StyledNode) -> Option<(usize, Vec<PictureSource>)> {
  let tag = styled.node.tag_name()?;
  if !tag.eq_ignore_ascii_case("picture") {
    return None;
  }

  let mut sources: Vec<PictureSource> = Vec::new();
  let mut fallback_img: Option<&StyledNode> = None;

  for child in &styled.children {
    let Some(child_tag) = child.node.tag_name() else {
      continue;
    };

    if child_tag.eq_ignore_ascii_case("source") {
      if fallback_img.is_some() {
        continue;
      }

      let Some(srcset_attr) = child.node.get_attribute_ref("srcset") else {
        continue;
      };
      let parsed_srcset = parse_srcset(srcset_attr);
      if parsed_srcset.is_empty() {
        continue;
      }

      let sizes = child.node.get_attribute_ref("sizes").and_then(parse_sizes);
      let media = child
        .node
        .get_attribute_ref("media")
        .and_then(|m| MediaQuery::parse_list(m).ok());
      let mime_type = child
        .node
        .get_attribute_ref("type")
        .and_then(normalize_mime_type);

      sources.push(PictureSource {
        srcset: parsed_srcset,
        sizes,
        media,
        mime_type,
      });
      continue;
    }

    if child_tag.eq_ignore_ascii_case("img") {
      fallback_img = Some(child);
      break;
    }
  }

  fallback_img.map(|img| (img.node_id, sources))
}

fn parse_svg_number(value: &str) -> Option<f32> {
  parse_svg_length_px(value)
}

#[allow(dead_code)]
fn serialize_dom_subtree(node: &crate::dom::DomNode) -> String {
  match &node.node_type {
    crate::dom::DomNodeType::Text { content } => escape_text(content),
    crate::dom::DomNodeType::ShadowRoot { .. } => {
      node.children.iter().map(serialize_dom_subtree).collect()
    }
    crate::dom::DomNodeType::Slot { attributes, .. } => {
      let mut out = String::new();
      out.push_str("<slot");
      for (name, value) in attributes {
        out.push(' ');
        out.push_str(name);
        out.push('=');
        out.push('"');
        push_escaped_attr(&mut out, value);
        out.push('"');
      }
      out.push('>');
      for child in node.children.iter() {
        out.push_str(&serialize_dom_subtree(child));
      }
      out.push_str("</slot>");
      out
    }
    crate::dom::DomNodeType::Element {
      tag_name,
      attributes,
      ..
    } => {
      let mut out = String::new();
      out.push('<');
      out.push_str(tag_name);
      for (name, value) in attributes {
        out.push(' ');
        out.push_str(name);
        out.push('=');
        out.push('"');
        push_escaped_attr(&mut out, value);
        out.push('"');
      }
      out.push('>');
      for child in node.children.iter() {
        out.push_str(&serialize_dom_subtree(child));
      }
      out.push_str("</");
      out.push_str(tag_name);
      out.push('>');
      out
    }
    crate::dom::DomNodeType::Document { .. } => {
      node.children.iter().map(serialize_dom_subtree).collect()
    }
  }
}

fn serialize_node_with_namespaces(
  styled: &StyledNode,
  inherited_xmlns: &[(String, String)],
  out: &mut String,
) {
  match &styled.node.node_type {
    crate::dom::DomNodeType::Document { .. } | crate::dom::DomNodeType::ShadowRoot { .. } => {
      for child in &styled.children {
        serialize_node_with_namespaces(child, inherited_xmlns, out);
      }
    }
    crate::dom::DomNodeType::Slot {
      namespace,
      attributes,
      ..
    } => {
      let mut attrs = attributes.clone();
      let mut namespaces: Vec<(String, String)> = inherited_xmlns.to_vec();
      for (name, value) in &attrs {
        if name.starts_with("xmlns")
          && !namespaces.iter().any(|(n, _)| n.eq_ignore_ascii_case(name))
        {
          namespaces.push((name.clone(), value.clone()));
        }
      }
      if !namespace.is_empty()
        && !attrs.iter().any(|(n, _)| n.eq_ignore_ascii_case("xmlns"))
        && !namespaces
          .iter()
          .any(|(n, _)| n.eq_ignore_ascii_case("xmlns"))
      {
        namespaces.push(("xmlns".to_string(), namespace.clone()));
      }
      for (name, value) in &namespaces {
        if !attrs.iter().any(|(n, _)| n.eq_ignore_ascii_case(name)) {
          attrs.push((name.clone(), value.clone()));
        }
      }

      out.push_str("<slot");
      for (name, value) in &attrs {
        out.push(' ');
        out.push_str(name);
        out.push('=');
        out.push('"');
        push_escaped_attr(out, value);
        out.push('"');
      }
      out.push('>');
      for child in &styled.children {
        serialize_node_with_namespaces(child, &namespaces, out);
      }
      out.push_str("</slot>");
    }
    crate::dom::DomNodeType::Text { content } => push_escaped_text(out, content),
    crate::dom::DomNodeType::Element {
      tag_name,
      namespace,
      attributes,
    } => {
      let mut attrs = attributes.clone();
      let mut namespaces: Vec<(String, String)> = inherited_xmlns.to_vec();
      for (name, value) in &attrs {
        if name.starts_with("xmlns")
          && !namespaces.iter().any(|(n, _)| n.eq_ignore_ascii_case(name))
        {
          namespaces.push((name.clone(), value.clone()));
        }
      }
      if !namespace.is_empty()
        && !attrs.iter().any(|(n, _)| n.eq_ignore_ascii_case("xmlns"))
        && !namespaces
          .iter()
          .any(|(n, _)| n.eq_ignore_ascii_case("xmlns"))
      {
        namespaces.push(("xmlns".to_string(), namespace.clone()));
      }
      for (name, value) in &namespaces {
        if !attrs.iter().any(|(n, _)| n.eq_ignore_ascii_case(name)) {
          attrs.push((name.clone(), value.clone()));
        }
      }

      out.push('<');
      out.push_str(tag_name);
      for (name, value) in &attrs {
        out.push(' ');
        out.push_str(name);
        out.push('=');
        out.push('"');
        push_escaped_attr(out, value);
        out.push('"');
      }
      out.push('>');
      for child in &styled.children {
        serialize_node_with_namespaces(child, &namespaces, out);
      }
      out.push_str("</");
      out.push_str(tag_name);
      out.push('>');
    }
  }
}

/// Serializes a styled DOM subtree without injecting document CSS or foreignObject placeholders.
///
/// This is intended for defs-only serialization such as collecting SVG filter definitions.
pub fn serialize_styled_subtree_plain(styled: &StyledNode) -> String {
  let mut out = String::new();
  serialize_node_with_namespaces(styled, &[], &mut out);
  out
}

/// Collect all SVG `<filter>` definitions with an `id` attribute from a styled DOM tree.
///
/// The returned map contains serialized filter elements keyed by their id.
/// Namespace declarations from ancestor elements are preserved to keep prefixed attributes valid.
pub fn collect_svg_filter_defs(styled: &StyledNode) -> HashMap<String, String> {
  fn walk(
    styled: &StyledNode,
    inherited_xmlns: &[(String, String)],
    filters: &mut HashMap<String, String>,
  ) {
    let mut owned_namespaces: Option<Vec<(String, String)>> = None;
    let mut namespaces = inherited_xmlns;
    if let crate::dom::DomNodeType::Element {
      tag_name,
      attributes,
      ..
    } = &styled.node.node_type
    {
      if attributes.iter().any(|(name, _)| name.starts_with("xmlns")) {
        let mut updated = inherited_xmlns.to_vec();
        for (name, value) in attributes.iter().filter(|(n, _)| n.starts_with("xmlns")) {
          if !updated.iter().any(|(n, _)| n.eq_ignore_ascii_case(name)) {
            updated.push((name.clone(), value.clone()));
          }
        }
        owned_namespaces = Some(updated);
        namespaces = owned_namespaces.as_deref().unwrap_or(inherited_xmlns);
      }

      if tag_name.eq_ignore_ascii_case("filter") {
        if let Some(id) = styled.node.get_attribute_ref("id") {
          if !id.is_empty() && !filters.contains_key(id) {
            let mut serialized = String::new();
            serialize_node_with_namespaces(styled, namespaces, &mut serialized);
            filters.insert(id.to_string(), serialized);
          }
        }
      }
    }

    for child in &styled.children {
      walk(child, namespaces, filters);
    }
  }

  let mut filters = HashMap::new();
  walk(styled, &[], &mut filters);
  filters
}

fn format_css_color(color: crate::style::color::Rgba) -> String {
  format!(
    "rgba({},{},{},{:.3})",
    color.r,
    color.g,
    color.b,
    color.a.clamp(0.0, 1.0)
  )
}

fn foreign_object_css_limit_bytes() -> usize {
  const DEFAULT_LIMIT: usize = 256 * 1024;
  static LIMIT: OnceLock<usize> = OnceLock::new();

  *LIMIT.get_or_init(|| {
    std::env::var("FASTR_MAX_FOREIGN_OBJECT_CSS_BYTES")
      .ok()
      .and_then(|raw| {
        let trimmed = raw.trim();
        if trimmed.is_empty() {
          return None;
        }
        trimmed.parse::<usize>().ok()
      })
      .unwrap_or(DEFAULT_LIMIT)
  })
}

fn box_debug_info_enabled() -> bool {
  static ENABLED: OnceLock<bool> = OnceLock::new();

  *ENABLED.get_or_init(|| {
    if runtime::runtime_toggles().truthy("FASTR_BOX_DEBUG_INFO") {
      return true;
    }
    cfg!(debug_assertions) || cfg!(test)
  })
}

fn serialize_svg_subtree(
  styled: &StyledNode,
  document_css: &str,
  svg_document_css_style_element: Option<&Arc<str>>,
) -> SvgContent {
  let profile_start = SVG_SERIALIZATION_PROFILE
    .with(|cell| cell.borrow().is_some())
    .then(Instant::now);

  fn merge_style_attribute(attrs: &mut Vec<(String, String)>, extra: &str) {
    if extra.trim().is_empty() {
      return;
    }
    if let Some((_, value)) = attrs
      .iter_mut()
      .find(|(name, _)| name.eq_ignore_ascii_case("style"))
    {
      if !value.trim_end().ends_with(';') && !value.trim().is_empty() {
        value.push(';');
      }
      value.push_str(extra);
    } else {
      attrs.push(("style".to_string(), extra.to_string()));
    }
  }

  fn root_style(style: &ComputedStyle) -> String {
    use std::fmt::Write as _;

    let mut out = String::with_capacity(64);
    out.push_str("color: rgba(");
    let color = style.color;
    let _ = write!(
      &mut out,
      "{},{},{},{:.3}",
      color.r,
      color.g,
      color.b,
      color.a.clamp(0.0, 1.0)
    );
    out.push(')');

    // Make unstyled shapes pick up the computed text color (common for icon SVGs).
    out.push_str("; fill: currentColor");

    if !style.font_family.is_empty() {
      out.push_str("; font-family: ");
      for (idx, family) in style.font_family.iter().enumerate() {
        if idx != 0 {
          out.push_str(", ");
        }
        if family.contains(' ') && !(family.starts_with('"') && family.ends_with('"')) {
          out.push('"');
          out.push_str(family);
          out.push('"');
        } else {
          out.push_str(family);
        }
      }
    }

    let _ = write!(&mut out, "; font-size: {:.2}px", style.font_size);
    let _ = write!(&mut out, "; font-weight: {}", style.font_weight.to_u16());
    match style.font_style {
      FontStyle::Italic => out.push_str("; font-style: italic"),
      FontStyle::Oblique(Some(angle)) => {
        let _ = write!(&mut out, "; font-style: oblique {}deg", angle);
      }
      FontStyle::Oblique(None) => out.push_str("; font-style: oblique"),
      FontStyle::Normal => {}
    }

    out
  }

  fn svg_uses_document_css(node: &StyledNode) -> bool {
    if node.node.get_attribute_ref("class").is_some() || node.node.get_attribute_ref("id").is_some()
    {
      return true;
    }
    if let Some(tag) = node.node.tag_name() {
      if tag.eq_ignore_ascii_case("foreignObject") {
        return true;
      }
    }
    node.children.iter().any(svg_uses_document_css)
  }

  let embed_document_css =
    svg_document_css_style_element.is_some() && svg_uses_document_css(styled);

  fn serialize_foreign_object_placeholder(
    styled: &StyledNode,
    attrs: &[(String, String)],
    out: &mut String,
  ) -> bool {
    let mut x = 0.0f32;
    let mut y = 0.0f32;
    let mut width: Option<f32> = None;
    let mut height: Option<f32> = None;
    for (name, value) in attrs {
      match name.as_str() {
        "x" => x = parse_svg_number(value).unwrap_or(0.0),
        "y" => y = parse_svg_number(value).unwrap_or(0.0),
        "width" => width = parse_svg_number(value),
        "height" => height = parse_svg_number(value),
        _ => {}
      }
    }

    let (width, height) = match (width, height) {
      (Some(w), Some(h)) if w > 0.0 && h > 0.0 => (w, h),
      _ => return false,
    };

    let mut fill = None;
    let mut text_color = None;
    let mut font_size = None;
    let mut text_content: Option<String> = None;

    for child in &styled.children {
      if child.styles.background_color.a > 0.0 {
        fill = Some(child.styles.background_color);
      }
      if text_color.is_none() {
        text_color = Some(child.styles.color);
        font_size = Some(child.styles.font_size);
      }
      if text_content.is_none() {
        if let Some(text) = child.node.text_content() {
          if !text.trim().is_empty() {
            text_content = Some(text.trim().to_string());
          }
        }
      }
      if fill.is_some() && text_content.is_some() {
        break;
      }
    }

    let has_fill = fill.is_some();
    let has_text = text_content.is_some();
    if !has_fill && !has_text {
      return false;
    }

    out.push_str("<g>");
    if let Some(color) = fill {
      out.push_str(&format!(
        "<rect x=\"{:.3}\" y=\"{:.3}\" width=\"{:.3}\" height=\"{:.3}\" fill=\"{}\" />",
        x,
        y,
        width,
        height,
        format_css_color(color)
      ));
    }

    if let (Some(text), Some(color), Some(size)) = (text_content, text_color, font_size) {
      let baseline = y + size;
      out.push_str(&format!(
        "<text x=\"{:.3}\" y=\"{:.3}\" fill=\"{}\" font-size=\"{:.3}px\">{}</text>",
        x,
        baseline,
        format_css_color(color),
        size,
        escape_text(&text)
      ));
    }

    out.push_str("</g>");
    true
  }

  fn serialize_foreign_object(
    styled: &StyledNode,
    attrs: &[(String, String)],
    _document_css: &str,
    out: &mut String,
    fallback_out: &mut Option<String>,
    foreign_objects: &mut Vec<ForeignObjectInfo>,
  ) -> bool {
    // ForeignObject output can diverge between the primary SVG (placeholder comment for later
    // replacement) and the fallback SVG (best-effort placeholder rendering). Only allocate and
    // populate the fallback buffer once we know we need it.
    let fallback_out = fallback_out.get_or_insert_with(|| out.clone());

    let mut x = 0.0f32;
    let mut y = 0.0f32;
    let mut width: Option<f32> = None;
    let mut height: Option<f32> = None;
    for (name, value) in attrs {
      match name.as_str() {
        "x" => x = parse_svg_number(value).unwrap_or(0.0),
        "y" => y = parse_svg_number(value).unwrap_or(0.0),
        "width" => width = parse_svg_number(value),
        "height" => height = parse_svg_number(value),
        _ => {}
      }
    }

    let (width, height) = match (width, height) {
      (Some(w), Some(h)) if w > 0.0 && h > 0.0 => (w, h),
      _ => {
        let placeholder = serialize_foreign_object_placeholder(styled, attrs, out);
        let _ = serialize_foreign_object_placeholder(styled, attrs, fallback_out);
        if placeholder {
          return true;
        }
        out.push_str("<!--FASTRENDER_FOREIGN_OBJECT_UNRESOLVED-->");
        fallback_out.push_str("<!--FASTRENDER_FOREIGN_OBJECT_UNRESOLVED-->");
        return true;
      }
    };

    let placeholder = format!("<!--FASTRENDER_FOREIGN_OBJECT_{}-->", foreign_objects.len());
    out.push_str(&placeholder);
    if !serialize_foreign_object_placeholder(styled, attrs, fallback_out) {
      fallback_out.push_str(&placeholder);
    }

    let mut html = String::new();
    for child in &styled.children {
      html.push_str(&serialize_dom_subtree(&child.node));
    }

    let background = if styled.styles.background_color.a > 0.0 {
      Some(styled.styles.background_color)
    } else {
      None
    };

    foreign_objects.push(ForeignObjectInfo {
      placeholder,
      attributes: attrs.to_vec(),
      x,
      y,
      width,
      height,
      opacity: styled.styles.opacity,
      background,
      html,
      style: Arc::clone(&styled.styles),
      overflow_x: styled.styles.overflow_x,
      overflow_y: styled.styles.overflow_y,
    });

    true
  }

  fn serialize_node(
    styled: &StyledNode,
    document_css: &str,
    parent_ns: Option<&str>,
    is_root: bool,
    out: &mut String,
    fallback_out: &mut Option<String>,
    foreign_objects: &mut Vec<ForeignObjectInfo>,
    record_document_css: bool,
    document_css_insert_pos: &mut Option<usize>,
  ) {
    match &styled.node.node_type {
      crate::dom::DomNodeType::Document { .. } | crate::dom::DomNodeType::ShadowRoot { .. } => {
        for child in &styled.children {
          serialize_node(
            child,
            document_css,
            parent_ns,
            false,
            out,
            fallback_out,
            foreign_objects,
            record_document_css,
            document_css_insert_pos,
          );
        }
      }
      crate::dom::DomNodeType::Slot { .. } => {
        for child in &styled.children {
          serialize_node(
            child,
            document_css,
            parent_ns,
            false,
            out,
            fallback_out,
            foreign_objects,
            record_document_css,
            document_css_insert_pos,
          );
        }
      }
      crate::dom::DomNodeType::Text { content } => {
        push_escaped_text(out, content);
        if let Some(fallback_out) = fallback_out.as_mut() {
          push_escaped_text(fallback_out, content);
        }
      }
      crate::dom::DomNodeType::Element {
        tag_name,
        namespace,
        attributes,
      } => {
        let mut current_ns = namespace.as_str();
        if is_root && current_ns.is_empty() {
          current_ns = SVG_NAMESPACE;
        }

        let mut owned_attrs: Option<Vec<(String, String)>> = None;
        let attrs: &[(String, String)] = if is_root {
          let mut attrs = attributes.clone();
          let has_xmlns = attrs
            .iter()
            .any(|(name, _)| name.eq_ignore_ascii_case("xmlns"));
          if !has_xmlns {
            attrs.push(("xmlns".to_string(), current_ns.to_string()));
          }

          let style_attr = root_style(&styled.styles);
          merge_style_attribute(&mut attrs, &style_attr);
          owned_attrs = Some(attrs);
          owned_attrs.as_deref().unwrap()
        } else if !current_ns.is_empty() && parent_ns != Some(current_ns) {
          let has_xmlns = attributes
            .iter()
            .any(|(name, _)| name.eq_ignore_ascii_case("xmlns"));
          if has_xmlns {
            attributes
          } else {
            let mut attrs = attributes.clone();
            attrs.push(("xmlns".to_string(), current_ns.to_string()));
            owned_attrs = Some(attrs);
            owned_attrs.as_deref().unwrap()
          }
        } else {
          attributes
        };

        if tag_name.eq_ignore_ascii_case("foreignObject") {
          if serialize_foreign_object(
            styled,
            attrs,
            document_css,
            out,
            fallback_out,
            foreign_objects,
          ) {
            return;
          }
        }

        out.push('<');
        if let Some(fallback_out) = fallback_out.as_mut() {
          fallback_out.push('<');
        }
        out.push_str(tag_name);
        if let Some(fallback_out) = fallback_out.as_mut() {
          fallback_out.push_str(tag_name);
        }
        for (name, value) in attrs {
          out.push(' ');
          if let Some(fallback_out) = fallback_out.as_mut() {
            fallback_out.push(' ');
          }
          out.push_str(name);
          if let Some(fallback_out) = fallback_out.as_mut() {
            fallback_out.push_str(name);
          }
          out.push('=');
          if let Some(fallback_out) = fallback_out.as_mut() {
            fallback_out.push('=');
          }
          out.push('"');
          if let Some(fallback_out) = fallback_out.as_mut() {
            fallback_out.push('"');
          }
          push_escaped_attr(out, value);
          if let Some(fallback_out) = fallback_out.as_mut() {
            push_escaped_attr(fallback_out, value);
          }
          out.push('"');
          if let Some(fallback_out) = fallback_out.as_mut() {
            fallback_out.push('"');
          }
        }
        out.push('>');
        if let Some(fallback_out) = fallback_out.as_mut() {
          fallback_out.push('>');
        }

        if is_root {
          if record_document_css && document_css_insert_pos.is_none() {
            *document_css_insert_pos = Some(out.len());
          }
        }

        for child in &styled.children {
          serialize_node(
            child,
            document_css,
            Some(current_ns),
            false,
            out,
            fallback_out,
            foreign_objects,
            record_document_css,
            document_css_insert_pos,
          );
        }

        out.push_str("</");
        if let Some(fallback_out) = fallback_out.as_mut() {
          fallback_out.push_str("</");
        }
        out.push_str(tag_name);
        if let Some(fallback_out) = fallback_out.as_mut() {
          fallback_out.push_str(tag_name);
        }
        out.push('>');
        if let Some(fallback_out) = fallback_out.as_mut() {
          fallback_out.push('>');
        }
      }
    }
  }

  let mut out = String::new();
  let mut fallback_out = None;
  let mut foreign_objects: Vec<ForeignObjectInfo> = Vec::new();
  let mut document_css_insert_pos = None;
  serialize_node(
    styled,
    document_css,
    None,
    true,
    &mut out,
    &mut fallback_out,
    &mut foreign_objects,
    embed_document_css,
    &mut document_css_insert_pos,
  );

  let fallback_svg = if foreign_objects.is_empty() {
    String::new()
  } else {
    fallback_out.unwrap_or_else(|| out.clone())
  };

  let shared_css = if !foreign_objects.is_empty()
    && document_css.as_bytes().len() <= foreign_object_css_limit_bytes()
  {
    document_css.to_string()
  } else {
    String::new()
  };

  let document_css_injection = if embed_document_css {
    match (svg_document_css_style_element, document_css_insert_pos) {
      (Some(style_element), Some(insert_pos)) => Some(SvgDocumentCssInjection {
        style_element: Arc::clone(style_element),
        insert_pos,
      }),
      _ => None,
    }
  } else {
    None
  };

  let content = SvgContent {
    svg: out,
    fallback_svg,
    foreign_objects,
    shared_css,
    document_css_injection,
  };

  if let Some(start) = profile_start {
    record_svg_serialization(
      start.elapsed(),
      content.svg.len() + content.fallback_svg.len(),
    );
  }

  content
}

/// Recursively generates BoxNodes from a StyledNode, honoring display: contents by
/// splicing grandchildren into the parent’s child list rather than creating a box.
fn generate_boxes_for_styled_into(
  styled: &StyledNode,
  styled_lookup: &StyledLookup<'_>,
  counters: &mut CounterManager,
  _is_root: bool,
  document_css: &str,
  svg_document_css_style_element: Option<&Arc<str>>,
  picture_sources: &mut PictureSourceLookup,
  options: &BoxGenerationOptions,
  deadline_counter: &mut usize,
  out: &mut Vec<BoxNode>,
) -> Result<()> {
  check_active_periodic(
    deadline_counter,
    BOX_GEN_DEADLINE_STRIDE,
    RenderStage::Cascade,
  )?;
  if let Some(text) = styled.node.text_content() {
    if !text.is_empty() {
      let style = Arc::clone(&styled.styles);
      if let Some(needle) = runtime::runtime_toggles().get("FASTR_FIND_BOX_TEXT") {
        if text.contains(&needle) {
          eprintln!(
            "[box-gen-text] styled_node_id={} tag={} display={:?} text={:?}",
            styled.node_id,
            styled.node.tag_name().unwrap_or("#text"),
            styled.styles.display,
            text
          );
        }
      }
      let mut box_node = BoxNode::new_text(style, text.to_string());
      box_node.starting_style = clone_starting_style(&styled.starting_styles.base);
      out.push(attach_styled_id(box_node, styled));
      return Ok(());
    }
  }

  counters.enter_scope();
  apply_counter_properties_from_style(styled, counters);

  let site_compat = options.site_compat_hacks_enabled();

  // Common ad placeholders that hold space even when empty: drop when they have no children/content.
  if site_compat {
    if let Some(class_attr) = styled.node.get_attribute_ref("class") {
      if styled.children.is_empty()
        && (class_attr.contains("ad-height-hold")
          || class_attr.contains("ad__slot")
          || class_attr.contains("should-hold-space"))
      {
        counters.leave_scope();
        return Ok(());
      }
    }
  }

  // display:none suppresses box generation entirely.
  if styled.styles.display == Display::None {
    counters.leave_scope();
    return Ok(());
  }

  if let Some(tag) = styled.node.tag_name() {
    if tag.eq_ignore_ascii_case("math") {
      let dom_subtree = dom_subtree_from_styled(styled);
      let math_root = crate::math::parse_mathml(&dom_subtree)
        .unwrap_or_else(|| crate::math::MathNode::Row(Vec::new()));
      counters.leave_scope();
      let box_node = BoxNode::new_replaced(
        Arc::clone(&styled.styles),
        ReplacedType::Math(MathReplaced {
          root: math_root,
          layout: None,
        }),
        None,
        None,
      );
      let mut box_node = box_node;
      box_node.starting_style = clone_starting_style(&styled.starting_styles.base);
      out.push(attach_debug_info(box_node, styled));
      return Ok(());
    }
  }

  // Form controls render as replaced elements with intrinsic sizing and native painting.
  if let Some(form_control) = create_form_control_replaced(styled) {
    counters.leave_scope();
    let box_node = BoxNode::new_replaced(
      Arc::clone(&styled.styles),
      ReplacedType::FormControl(form_control),
      None,
      None,
    );
    out.push(attach_debug_info(box_node, styled));
    return Ok(());
  }

  // Replaced elements short-circuit to a single replaced box unless they're display: contents.
  if let Some(tag) = styled.node.tag_name() {
    // Non-rendered elements: <source>, <track>, <option>, <optgroup> never create boxes.
    if tag.eq_ignore_ascii_case("source")
      || tag.eq_ignore_ascii_case("track")
      || tag.eq_ignore_ascii_case("option")
      || tag.eq_ignore_ascii_case("optgroup")
    {
      counters.leave_scope();
      return Ok(());
    }

    if is_replaced_element(tag) && styled.styles.display != Display::Contents {
      // The <object> element falls back to its nested content when no data URI is provided.
      // In that case we should not generate a replaced box, allowing the children to render normally.
      if !tag.eq_ignore_ascii_case("object")
        || styled
          .node
          .get_attribute_ref("data")
          .is_some_and(|d| !d.is_empty())
      {
        counters.leave_scope();
        let picture_sources_for_img = if tag.eq_ignore_ascii_case("img") {
          picture_sources.take(styled.node_id)
        } else {
          Vec::new()
        };
        let box_node = create_replaced_box_from_styled(
          styled,
          Arc::clone(&styled.styles),
          document_css,
          svg_document_css_style_element,
          picture_sources_for_img,
        );
        let mut box_node = box_node;
        box_node.starting_style = clone_starting_style(&styled.starting_styles.base);
        out.push(attach_debug_info(box_node, styled));
        return Ok(());
      }
    }
  }

  let composed_children = composed_children(styled, styled_lookup);
  let composed_len = composed_children.len();
  let mut children: Vec<BoxNode> = Vec::with_capacity(composed_len);
  let mut idx = 0;
  while idx < composed_len {
    let child = composed_children.get(idx);
    if site_compat {
      if let Some(testid) = child.node.get_attribute_ref("data-testid") {
        if testid == "one-nav-overlay" {
          let overlay_hidden =
            matches!(child.styles.visibility, Visibility::Hidden) || child.styles.opacity == 0.0;
          if overlay_hidden {
            // Skip the overlay and the subsequent focus-trap container when the overlay is hidden (menu closed).
            idx += 1;
            while idx < composed_len {
              let next = composed_children.get(idx);
              // Skip over whitespace/text nodes between the overlay and drawer.
              if let crate::dom::DomNodeType::Text { content } = &next.node.node_type {
                if content.trim().is_empty() {
                  idx += 1;
                  continue;
                }
              }

              if let Some(class_attr) = next.node.get_attribute_ref("class") {
                if class_attr.contains("FocusTrapContainer-") {
                  for grandchild in &next.children {
                    generate_boxes_for_styled_into(
                      grandchild,
                      styled_lookup,
                      counters,
                      false,
                      document_css,
                      svg_document_css_style_element,
                      picture_sources,
                      options,
                      deadline_counter,
                      &mut children,
                    )?;
                  }
                  idx += 1;
                }
              }
              break;
            }
            continue;
          }
        }
      }
    }
    generate_boxes_for_styled_into(
      child,
      styled_lookup,
      counters,
      false,
      document_css,
      svg_document_css_style_element,
      picture_sources,
      options,
      deadline_counter,
      &mut children,
    )?;
    idx += 1;
  }

  // Generate ::before/::after/::marker in a single pass without repeated `insert(0, ...)` shifts.
  let mut before_box: Option<BoxNode> = None;
  let mut marker_box: Option<BoxNode> = None;
  let mut after_box: Option<BoxNode> = None;

  if let Some(before_styles) = &styled.before_styles {
    let before_start = clone_starting_style(&styled.starting_styles.before);
    before_box = create_pseudo_element_box(styled, before_styles, before_start, "before", counters);
  }

  if styled.styles.display == Display::ListItem {
    marker_box = create_marker_box(styled, counters);
  }

  if let Some(after_styles) = &styled.after_styles {
    let after_start = clone_starting_style(&styled.starting_styles.after);
    after_box = create_pseudo_element_box(styled, after_styles, after_start, "after", counters);
  }

  if before_box.is_some() || marker_box.is_some() || after_box.is_some() {
    let extra = usize::from(before_box.is_some())
      + usize::from(marker_box.is_some())
      + usize::from(after_box.is_some());
    let mut combined = Vec::with_capacity(children.len() + extra);
    if let Some(marker_box) = marker_box {
      combined.push(marker_box);
    }
    if let Some(before_box) = before_box {
      combined.push(before_box);
    }
    combined.append(&mut children);
    if let Some(after_box) = after_box {
      combined.push(after_box);
    }
    children = combined;
  }

  // display: contents contributes its children directly.
  if styled.styles.display == Display::Contents {
    counters.leave_scope();
    out.extend(children);
    return Ok(());
  }

  let style = Arc::clone(&styled.styles);
  let fc_type = styled
    .styles
    .display
    .formatting_context_type()
    .unwrap_or(FormattingContextType::Block);

  let mut box_node = match styled.styles.display {
    Display::Block | Display::FlowRoot | Display::ListItem => {
      BoxNode::new_block(style, fc_type, children)
    }
    Display::Inline
    | Display::Ruby
    | Display::RubyBase
    | Display::RubyText
    | Display::RubyBaseContainer
    | Display::RubyTextContainer => BoxNode::new_inline(style, children),
    Display::InlineBlock => BoxNode::new_inline_block(style, fc_type, children),
    Display::Flex | Display::InlineFlex => {
      BoxNode::new_block(style, FormattingContextType::Flex, children)
    }
    Display::Grid | Display::InlineGrid => {
      BoxNode::new_block(style, FormattingContextType::Grid, children)
    }
    Display::Table | Display::InlineTable => {
      BoxNode::new_block(style, FormattingContextType::Table, children)
    }
    // Table-internal boxes (simplified for Wave 2)
    Display::TableRow
    | Display::TableCell
    | Display::TableRowGroup
    | Display::TableHeaderGroup
    | Display::TableFooterGroup
    | Display::TableColumn
    | Display::TableColumnGroup
    | Display::TableCaption => BoxNode::new_block(style, FormattingContextType::Block, children),
    Display::None | Display::Contents => unreachable!("handled above"),
  };

  box_node.starting_style = clone_starting_style(&styled.starting_styles.base);
  box_node.first_line_style = styled.first_line_styles.as_ref().map(Arc::clone);
  box_node.first_letter_style = styled.first_letter_styles.as_ref().map(Arc::clone);

  counters.leave_scope();
  out.push(attach_debug_info(box_node, styled));
  Ok(())
}

fn attach_debug_info(mut box_node: BoxNode, styled: &StyledNode) -> BoxNode {
  box_node.styled_node_id = Some(styled.node_id);
  if !box_debug_info_enabled() {
    return box_node;
  }
  if let Some(tag) = styled.node.tag_name() {
    // Extract colspan/rowspan for table cells and span for columns/colgroups
    let colspan = styled
      .node
      .get_attribute_ref("colspan")
      .and_then(|s| s.parse::<usize>().ok())
      .unwrap_or(1)
      .max(1);
    let rowspan = styled
      .node
      .get_attribute_ref("rowspan")
      .and_then(|s| s.parse::<usize>().ok())
      .unwrap_or(1)
      .max(1);
    let column_span = if matches!(
      styled.styles.display,
      Display::TableColumn | Display::TableColumnGroup
    ) {
      styled
        .node
        .get_attribute_ref("span")
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(1)
        .max(1)
    } else {
      1
    };

    let id = styled.node.get_attribute("id");
    let classes = styled
      .node
      .get_attribute_ref("class")
      .map(|c| c.split_ascii_whitespace().map(str::to_owned).collect())
      .unwrap_or_default();

    let mut dbg = DebugInfo::new(Some(tag.to_string()), id, classes).with_spans(colspan, rowspan);
    dbg.column_span = column_span;
    box_node = box_node.with_debug_info(dbg);
  }
  box_node
}

/// Creates a box for a pseudo-element (::before or ::after)
fn create_pseudo_element_box(
  styled: &StyledNode,
  styles: &Arc<ComputedStyle>,
  starting_style: Option<Arc<ComputedStyle>>,
  pseudo_name: &str,
  counters: &CounterManager,
) -> Option<BoxNode> {
  let content_value = effective_content_value(styles);
  if matches!(content_value, ContentValue::None | ContentValue::Normal) {
    return None;
  }

  let pseudo_style = Arc::clone(styles);

  let mut context = ContentContext::new();
  for (name, value) in styled.node.attributes_iter() {
    context.set_attribute(name, value);
  }
  for (name, stack) in counters.snapshot() {
    context.set_counter_stack(&name, stack);
  }
  context.set_quotes(styles.quotes.clone());

  // Build children based on content items, supporting both text and replaced content.
  let mut children: Vec<BoxNode> = Vec::new();
  let mut text_buf = String::new();

  let flush_text = |buf: &mut String, pseudo_style: &Arc<ComputedStyle>, out: &mut Vec<BoxNode>| {
    if buf.is_empty() {
      return;
    }
    let text = std::mem::take(buf);
    let mut text_box = BoxNode::new_text(pseudo_style.clone(), text);
    text_box.styled_node_id = Some(styled.node_id);
    out.push(text_box);
  };

  let items = match &content_value {
    ContentValue::Items(items) => items,
    _ => return None,
  };

  for item in items {
    match item {
      ContentItem::String(s) => text_buf.push_str(s),
      ContentItem::Attr { name, fallback, .. } => {
        if let Some(val) = context.get_attribute(name) {
          text_buf.push_str(&val);
        } else if let Some(fb) = fallback {
          text_buf.push_str(fb);
        }
      }
      ContentItem::Counter { name, style } => {
        let value = context.get_counter(name);
        let formatted = styles
          .counter_styles
          .format_value(value, style.clone().unwrap_or(CounterStyle::Decimal.into()));
        text_buf.push_str(&formatted);
      }
      ContentItem::Counters {
        name,
        separator,
        style,
      } => {
        let values = context.get_counters(name);
        let style_name = style.clone().unwrap_or(CounterStyle::Decimal.into());
        if values.is_empty() {
          text_buf.push_str(&styles.counter_styles.format_value(0, style_name));
        } else {
          for (idx, value) in values.iter().enumerate() {
            if idx != 0 {
              text_buf.push_str(separator);
            }
            text_buf.push_str(
              &styles
                .counter_styles
                .format_value(*value, style_name.clone()),
            );
          }
        }
      }
      ContentItem::StringReference { name, kind } => {
        if let Some(value) = context.get_running_string(name, *kind) {
          text_buf.push_str(value);
        }
      }
      ContentItem::OpenQuote => {
        text_buf.push_str(context.open_quote());
        context.push_quote();
      }
      ContentItem::CloseQuote => {
        text_buf.push_str(context.close_quote());
        context.pop_quote();
      }
      ContentItem::NoOpenQuote => context.push_quote(),
      ContentItem::NoCloseQuote => context.pop_quote(),
      ContentItem::Element { .. } => {
        // Running elements are not yet resolved into generated content.
      }
      ContentItem::Url(url) => {
        flush_text(&mut text_buf, &pseudo_style, &mut children);
        let replaced = ReplacedBox {
          replaced_type: ReplacedType::Image {
            src: url.clone(),
            alt: None,
            sizes: None,
            srcset: Vec::new(),
            picture_sources: Vec::new(),
          },
          intrinsic_size: None,
          aspect_ratio: None,
        };
        children.push(BoxNode {
          starting_style: starting_style.clone(),
          box_type: BoxType::Replaced(replaced),
          style: pseudo_style.clone(),
          children: vec![],
          id: 0,
          debug_info: None,
          styled_node_id: Some(styled.node_id),
          first_line_style: None,
          first_letter_style: None,
        });
      }
    }
  }

  flush_text(&mut text_buf, &pseudo_style, &mut children);

  if children.is_empty() {
    return None;
  }

  // Determine the box type based on display property
  let fc_type = styles
    .display
    .formatting_context_type()
    .unwrap_or(FormattingContextType::Block);

  // Wrap in appropriate box type based on display
  let mut pseudo_box = match styles.display {
    Display::Block => BoxNode::new_block(pseudo_style.clone(), fc_type, children),
    Display::Inline | Display::None => BoxNode::new_inline(pseudo_style.clone(), children),
    Display::InlineBlock => BoxNode::new_inline_block(pseudo_style.clone(), fc_type, children),
    _ => BoxNode::new_inline(pseudo_style.clone(), children),
  };

  // Add debug info to mark this as a pseudo-element
  pseudo_box.debug_info = Some(DebugInfo::new(
    Some(pseudo_name.to_string()),
    None,
    vec!["pseudo-element".to_string()],
  ));
  pseudo_box.styled_node_id = Some(styled.node_id);
  pseudo_box.starting_style = starting_style;

  Some(pseudo_box)
}

fn create_marker_box(styled: &StyledNode, counters: &CounterManager) -> Option<BoxNode> {
  // Prefer authored ::marker styles; fall back to the originating style when absent.
  let (mut marker_style, has_pseudo_styles) = if let Some(styles) = styled.marker_styles.as_deref()
  {
    (styles.clone(), true)
  } else {
    (styled.styles.as_ref().clone(), false)
  };
  // ::marker boxes are inline and should not carry layout-affecting edges from the list item.
  crate::style::cascade::reset_marker_box_properties(&mut marker_style);
  marker_style.display = Display::Inline;

  let content = marker_content_from_style(styled, &marker_style, counters)?;
  marker_style.list_style_type = ListStyleType::None;
  marker_style.list_style_image = crate::style::types::ListStyleImage::None;
  if !has_pseudo_styles {
    // Ensure list-item text transforms do not alter markers when no ::marker styles are authored.
    marker_style.text_transform = TextTransform::none();
  }

  let mut node = BoxNode::new_marker(Arc::new(marker_style), content);
  node.styled_node_id = Some(styled.node_id);
  node.starting_style = clone_starting_style(&styled.starting_styles.marker);
  Some(node)
}

pub(crate) fn marker_content_from_style(
  styled: &StyledNode,
  marker_style: &ComputedStyle,
  counters: &CounterManager,
) -> Option<MarkerContent> {
  let content_value = effective_content_value(marker_style);
  if matches!(content_value, ContentValue::None) || marker_style.content == "none" {
    return None;
  }

  if !matches!(content_value, ContentValue::Normal | ContentValue::None) {
    let mut context = ContentContext::new();
    for (name, value) in styled.node.attributes_iter() {
      context.set_attribute(name, value);
    }
    for (name, stack) in counters.snapshot() {
      context.set_counter_stack(&name, stack);
    }
    context.set_quotes(marker_style.quotes.clone());

    let mut text = String::new();
    let mut image: Option<String> = None;

    if let ContentValue::Items(items) = &content_value {
      for item in items {
        match item {
          ContentItem::String(s) => text.push_str(s),
          ContentItem::Attr { name, fallback, .. } => {
            if let Some(val) = context.get_attribute(name) {
              text.push_str(&val);
            } else if let Some(fb) = fallback {
              text.push_str(fb);
            }
          }
          ContentItem::Counter { name, style } => {
            let value = context.get_counter(name);
            let formatted = style
              .clone()
              .unwrap_or(CounterStyleName::from(CounterStyle::Decimal));
            let formatted = marker_style.counter_styles.format_value(value, formatted);
            text.push_str(&formatted);
          }
          ContentItem::Counters {
            name,
            separator,
            style,
          } => {
            let values = context.get_counters(name);
            let style = style
              .clone()
              .unwrap_or(CounterStyleName::from(CounterStyle::Decimal));
            if values.is_empty() {
              text.push_str(&marker_style.counter_styles.format_value(0, style));
            } else {
              for (idx, value) in values.iter().enumerate() {
                if idx != 0 {
                  text.push_str(separator);
                }
                text.push_str(
                  &marker_style
                    .counter_styles
                    .format_value(*value, style.clone()),
                );
              }
            }
          }
          ContentItem::StringReference { name, kind } => {
            if let Some(value) = context.get_running_string(name, *kind) {
              text.push_str(value);
            }
          }
          ContentItem::OpenQuote => {
            text.push_str(context.open_quote());
            context.push_quote();
          }
          ContentItem::CloseQuote => {
            text.push_str(context.close_quote());
            context.pop_quote();
          }
          ContentItem::NoOpenQuote => context.push_quote(),
          ContentItem::NoCloseQuote => context.pop_quote(),
          ContentItem::Element { .. } => {
            // Running elements are not supported for list markers yet.
          }
          ContentItem::Url(url) => {
            // If the author supplies multiple URLs we take the last; mixed text+image returns text.
            image = Some(url.clone());
          }
        }
      }
    }

    if !text.is_empty() {
      return Some(MarkerContent::Text(text));
    }
    if let Some(src) = image {
      let replaced = ReplacedBox {
        replaced_type: ReplacedType::Image {
          src,
          alt: None,
          sizes: None,
          srcset: Vec::new(),
          picture_sources: Vec::new(),
        },
        intrinsic_size: None,
        aspect_ratio: None,
      };
      return Some(MarkerContent::Image(replaced));
    }
    return None;
  }

  match &marker_style.list_style_image {
    crate::style::types::ListStyleImage::Url(url) => {
      let replaced = ReplacedBox {
        replaced_type: ReplacedType::Image {
          src: url.clone(),
          alt: None,
          sizes: None,
          srcset: Vec::new(),
          picture_sources: Vec::new(),
        },
        intrinsic_size: None,
        aspect_ratio: None,
      };
      return Some(MarkerContent::Image(replaced));
    }
    crate::style::types::ListStyleImage::None => {}
  }

  let text = list_marker_text(marker_style.list_style_type.clone(), counters);
  if text.is_empty() {
    None
  } else {
    Some(MarkerContent::Text(text))
  }
}

/// Derive an effective `content` value that falls back to the legacy `content`
/// string when structured parsing has not populated `content_value`.
fn effective_content_value(style: &ComputedStyle) -> ContentValue {
  match &style.content_value {
    ContentValue::Normal => {
      let raw = style.content.trim();
      if raw.is_empty() || raw.eq_ignore_ascii_case("normal") {
        ContentValue::Normal
      } else if raw.eq_ignore_ascii_case("none") {
        ContentValue::None
      } else {
        ContentValue::Items(vec![ContentItem::String(style.content.clone())])
      }
    }
    other => other.clone(),
  }
}

fn apply_counter_properties_from_style(styled: &StyledNode, counters: &mut CounterManager) {
  if styled.node.text_content().is_some() {
    return;
  }

  let tag_name = styled.node.tag_name();
  let is_ol = tag_name.is_some_and(|t| t.eq_ignore_ascii_case("ol"));
  let reversed = is_ol && styled.node.get_attribute_ref("reversed").is_some();

  let is_list_container = tag_name.is_some_and(|tag| {
    tag.eq_ignore_ascii_case("ol")
      || tag.eq_ignore_ascii_case("ul")
      || tag.eq_ignore_ascii_case("menu")
      || tag.eq_ignore_ascii_case("dir")
  });

  if is_list_container {
    // Each list establishes its own default step; child lists shouldn't inherit reversed steps.
    counters.set_list_item_increment(1);
    if reversed {
      counters.set_list_item_increment(-1);
    }
  }

  let css_reset = styled.styles.counters.counter_reset.clone();
  let reset_is_ua_default = matches!(
      css_reset.as_ref(),
      Some(reset) if reset.items.len() == 1 && reset.items[0].name == "list-item" && reset.items[0].value == 0
  );
  let mut applied_reset = false;

  if let Some(reset) = css_reset {
    if is_list_container && reset_is_ua_default {
      // Defer to HTML list defaults below so start/reversed can override UA list-item reset.
    } else {
      counters.apply_reset(&reset);
      applied_reset = true;
    }
  }

  if is_list_container && !applied_reset {
    let start = styled
      .node
      .get_attribute_ref("start")
      .and_then(|s| s.parse::<i32>().ok());
    let step = counters.list_item_increment();
    let start_value = if is_ol {
      if reversed {
        // reversed lists count down; default start is the number of list items
        let item_count = list_item_count(styled) as i32;
        start.unwrap_or_else(|| item_count.max(0))
      } else {
        start.unwrap_or(1)
      }
    } else {
      0
    };
    let default_value = start_value.saturating_sub(step);
    let default_reset = CounterSet::single("list-item", default_value);
    counters.apply_reset(&default_reset);
  }

  if let Some(set) = &styled.styles.counters.counter_set {
    counters.apply_set(set);
  }

  // HTML LI value attribute sets the list-item counter for this item.
  if tag_name.as_deref() == Some("li") {
    if let Some(value_attr) = styled
      .node
      .get_attribute("value")
      .and_then(|v| v.parse::<i32>().ok())
    {
      let step = counters.list_item_increment();
      let target = value_attr.saturating_sub(step);
      counters.apply_set(&CounterSet::single("list-item", target));
    }
  }

  let css_increment = styled.styles.counters.counter_increment.as_ref();
  let increment_is_ua_default = matches!(
      css_increment,
      Some(increment) if increment.items.len() == 1 && increment.items[0].name == "list-item" && increment.items[0].value == 1
  );

  if let Some(increment) = css_increment {
    if increment_is_ua_default && styled.styles.display == Display::ListItem {
      let step = counters.list_item_increment();
      counters.apply_increment(&CounterSet::single("list-item", step));
    } else {
      counters.apply_increment(increment);
    }
  } else if styled.styles.display == Display::ListItem {
    let step = counters.list_item_increment();
    counters.apply_increment(&CounterSet::single("list-item", step));
  }
}

/// Count immediate list items belonging to this list, ignoring nested lists.
fn list_item_count(styled: &StyledNode) -> usize {
  fn walk(node: &StyledNode, in_nested_list: bool, acc: &mut usize) {
    for child in node.children.iter() {
      let tag = child.node.tag_name();
      let is_list = tag.is_some_and(|tag| {
        tag.eq_ignore_ascii_case("ol")
          || tag.eq_ignore_ascii_case("ul")
          || tag.eq_ignore_ascii_case("menu")
          || tag.eq_ignore_ascii_case("dir")
      });
      let now_nested = in_nested_list || is_list;
      if !now_nested && tag.is_some_and(|tag| tag.eq_ignore_ascii_case("li")) {
        *acc += 1;
      }
      // Do not count descendants of nested lists toward the ancestor list's item count.
      walk(child, now_nested, acc);
    }
  }

  let mut count = 0;
  walk(styled, false, &mut count);
  count
}

fn collect_text_content(node: &StyledNode) -> String {
  fn walk(node: &StyledNode, out: &mut String) {
    if let DomNodeType::Text { content } = &node.node.node_type {
      out.push_str(content);
    }
    for child in node.children.iter() {
      walk(child, out);
    }
  }

  let mut text = String::new();
  walk(node, &mut text);
  text
}

fn option_label_from_node(node: &StyledNode) -> Option<String> {
  if let Some(label) = node
    .node
    .get_attribute_ref("label")
    .filter(|l| !l.is_empty())
  {
    return Some(label.to_string());
  }
  if let Some(label) = node
    .node
    .get_attribute_ref("value")
    .filter(|v| !v.is_empty())
  {
    return Some(label.to_string());
  }

  let text = collect_text_content(node);
  let trimmed = text.trim();
  if trimmed.is_empty() {
    None
  } else {
    Some(trimmed.to_string())
  }
}

fn find_selected_option_label(node: &StyledNode, optgroup_disabled: bool) -> Option<String> {
  let Some(tag) = node.node.tag_name() else {
    return None;
  };

  let is_option = tag.eq_ignore_ascii_case("option");
  let is_optgroup = tag.eq_ignore_ascii_case("optgroup");
  let this_disabled = optgroup_disabled
    || (is_option && node.node.get_attribute_ref("disabled").is_some())
    || (is_optgroup && node.node.get_attribute_ref("disabled").is_some());

  if is_option && !this_disabled && node.node.get_attribute_ref("selected").is_some() {
    return option_label_from_node(node);
  }

  let next_optgroup_disabled =
    optgroup_disabled || (is_optgroup && node.node.get_attribute_ref("disabled").is_some());
  for child in node.children.iter() {
    if let Some(val) = find_selected_option_label(child, next_optgroup_disabled) {
      return Some(val);
    }
  }

  None
}

fn first_enabled_option_label(node: &StyledNode, optgroup_disabled: bool) -> Option<String> {
  let Some(tag) = node.node.tag_name() else {
    return None;
  };

  let is_option = tag.eq_ignore_ascii_case("option");
  let is_optgroup = tag.eq_ignore_ascii_case("optgroup");
  let this_disabled = optgroup_disabled
    || (is_option && node.node.get_attribute_ref("disabled").is_some())
    || (is_optgroup && node.node.get_attribute_ref("disabled").is_some());

  if is_option && !this_disabled {
    return option_label_from_node(node);
  }

  let next_optgroup_disabled =
    optgroup_disabled || (is_optgroup && node.node.get_attribute_ref("disabled").is_some());
  for child in node.children.iter() {
    if let Some(val) = first_enabled_option_label(child, next_optgroup_disabled) {
      return Some(val);
    }
  }

  None
}

fn select_label(node: &StyledNode) -> Option<String> {
  let explicit = find_selected_option_label(node, false);
  if explicit.is_some() {
    return explicit;
  }

  if node.node.get_attribute_ref("multiple").is_some() {
    return None;
  }

  first_enabled_option_label(node, false)
}

fn option_value_from_node(node: &StyledNode) -> String {
  if let Some(value) = node.node.get_attribute_ref("value") {
    return value.to_string();
  }

  let mut value = String::new();
  for child in node.children.iter() {
    if let DomNodeType::Text { content } = &child.node.node_type {
      value.push_str(content);
    }
  }
  value
}

fn collect_selected_option_values(
  node: &StyledNode,
  optgroup_disabled: bool,
  out: &mut Vec<String>,
) {
  let tag = node.node.tag_name().unwrap_or("");
  let is_option = tag.eq_ignore_ascii_case("option");
  let is_optgroup = tag.eq_ignore_ascii_case("optgroup");

  let option_disabled = node.node.get_attribute_ref("disabled").is_some();
  let next_optgroup_disabled = optgroup_disabled || (is_optgroup && option_disabled);

  if is_option
    && node.node.get_attribute_ref("selected").is_some()
    && !(option_disabled || optgroup_disabled)
  {
    out.push(option_value_from_node(node));
  }

  for child in node.children.iter() {
    collect_selected_option_values(child, next_optgroup_disabled, out);
  }
}

fn find_selected_option_value(node: &StyledNode, optgroup_disabled: bool) -> Option<String> {
  let tag = node.node.tag_name().unwrap_or("");
  let is_option = tag.eq_ignore_ascii_case("option");
  let is_optgroup = tag.eq_ignore_ascii_case("optgroup");

  let option_disabled = node.node.get_attribute_ref("disabled").is_some();
  let next_optgroup_disabled = optgroup_disabled || (is_optgroup && option_disabled);

  if is_option
    && node.node.get_attribute_ref("selected").is_some()
    && !(option_disabled || optgroup_disabled)
  {
    return Some(option_value_from_node(node));
  }

  for child in node.children.iter() {
    if let Some(val) = find_selected_option_value(child, next_optgroup_disabled) {
      return Some(val);
    }
  }
  None
}

fn first_enabled_option_value(node: &StyledNode, optgroup_disabled: bool) -> Option<String> {
  let tag = node.node.tag_name().unwrap_or("");
  let is_option = tag.eq_ignore_ascii_case("option");
  let is_optgroup = tag.eq_ignore_ascii_case("optgroup");

  let option_disabled = node.node.get_attribute_ref("disabled").is_some();
  let next_optgroup_disabled = optgroup_disabled || (is_optgroup && option_disabled);

  if is_option && !(option_disabled || optgroup_disabled) {
    return Some(option_value_from_node(node));
  }

  for child in node.children.iter() {
    if let Some(val) = first_enabled_option_value(child, next_optgroup_disabled) {
      return Some(val);
    }
  }
  None
}

fn select_value(node: &StyledNode) -> Option<String> {
  let multiple = node.node.get_attribute_ref("multiple").is_some();
  if multiple {
    let mut values = Vec::new();
    collect_selected_option_values(node, false, &mut values);
    if values.is_empty() {
      return None;
    }
    return Some(values.join(", "));
  }

  let explicit = find_selected_option_value(node, false);
  if explicit.is_some() {
    return explicit;
  }

  first_enabled_option_value(node, false)
}

fn input_label(node: &DomNode, input_type: &str) -> String {
  if let Some(value) = node.get_attribute_ref("value").filter(|v| !v.is_empty()) {
    return value.to_string();
  }

  if input_type.eq_ignore_ascii_case("submit") {
    "Submit".to_string()
  } else if input_type.eq_ignore_ascii_case("reset") {
    "Reset".to_string()
  } else if input_type.eq_ignore_ascii_case("button") {
    "Button".to_string()
  } else {
    input_type.to_ascii_uppercase()
  }
}

fn button_label(node: &StyledNode) -> String {
  let text = collect_text_content(node);
  let trimmed = text.trim();
  if !trimmed.is_empty() {
    return trimmed.to_string();
  }

  node
    .node
    .get_attribute_ref("value")
    .filter(|v| !v.is_empty())
    .map(|v| v.to_string())
    .unwrap_or_else(|| "Button".to_string())
}

fn parse_f32_attr(node: &DomNode, name: &str) -> Option<f32> {
  node
    .get_attribute_ref(name)
    .and_then(|v| v.trim().parse::<f32>().ok())
}

fn create_form_control_replaced(styled: &StyledNode) -> Option<FormControl> {
  let tag = styled.node.tag_name()?;
  let appearance = styled.styles.appearance.clone();

  if matches!(appearance, Appearance::None) {
    return None;
  }

  if !tag.eq_ignore_ascii_case("input")
    && !tag.eq_ignore_ascii_case("textarea")
    && !tag.eq_ignore_ascii_case("select")
    && !tag.eq_ignore_ascii_case("button")
  {
    return None;
  }

  let disabled = styled.node.get_attribute_ref("disabled").is_some();
  let inert = styled.node.get_attribute_ref("inert").is_some()
    || styled
      .node
      .get_attribute_ref("data-fastr-inert")
      .map(|v| v.eq_ignore_ascii_case("true"))
      .unwrap_or(false);
  let focus_flag = styled
    .node
    .get_attribute_ref("data-fastr-focus")
    .map(|v| v.eq_ignore_ascii_case("true"))
    .unwrap_or(false);
  let focus_visible_flag = styled
    .node
    .get_attribute_ref("data-fastr-focus-visible")
    .map(|v| v.eq_ignore_ascii_case("true"))
    .unwrap_or(false);
  let mut focused = (focus_flag || focus_visible_flag) && !inert;
  let mut focus_visible = focus_visible_flag && !inert;
  if !focused {
    focus_visible = false;
  }
  if disabled {
    focused = false;
    focus_visible = false;
  }
  let textarea_value = tag
    .eq_ignore_ascii_case("textarea")
    .then(|| collect_text_content(styled));
  let element_ref = ElementRef::new(&styled.node);
  let required = element_ref.accessibility_required() && !disabled;
  let mut invalid = element_ref.accessibility_supports_validation() && !disabled;
  if invalid {
    if tag.eq_ignore_ascii_case("textarea") {
      invalid = required
        && textarea_value
          .as_deref()
          .unwrap_or_default()
          .trim()
          .is_empty();
    } else if tag.eq_ignore_ascii_case("select") {
      invalid = required && select_value(styled).unwrap_or_default().trim().is_empty();
    } else {
      invalid = !element_ref.accessibility_is_valid();
    }
  }

  if tag.eq_ignore_ascii_case("input") {
    let input_type = styled.node.get_attribute_ref("type").unwrap_or("text");
    if input_type.eq_ignore_ascii_case("hidden") {
      return None;
    }

    let control = if input_type.eq_ignore_ascii_case("checkbox") {
      FormControlKind::Checkbox {
        is_radio: false,
        checked: styled.node.get_attribute_ref("checked").is_some(),
        indeterminate: styled
          .node
          .get_attribute_ref("indeterminate")
          .map(|v| v.eq_ignore_ascii_case("true"))
          .unwrap_or(false)
          || styled
            .node
            .get_attribute_ref("aria-checked")
            .map(|v| v.eq_ignore_ascii_case("mixed"))
            .unwrap_or(false),
      }
    } else if input_type.eq_ignore_ascii_case("radio") {
      FormControlKind::Checkbox {
        is_radio: true,
        checked: styled.node.get_attribute_ref("checked").is_some(),
        indeterminate: false,
      }
    } else if input_type.eq_ignore_ascii_case("button")
      || input_type.eq_ignore_ascii_case("submit")
      || input_type.eq_ignore_ascii_case("reset")
    {
      FormControlKind::Button {
        label: input_label(&styled.node, input_type),
      }
    } else if input_type.eq_ignore_ascii_case("range") {
      FormControlKind::Range {
        value: parse_f32_attr(&styled.node, "value").unwrap_or(50.0),
        min: parse_f32_attr(&styled.node, "min"),
        max: parse_f32_attr(&styled.node, "max"),
      }
    } else if input_type.eq_ignore_ascii_case("color") {
      let raw_value = styled
        .node
        .get_attribute_ref("value")
        .filter(|v| !v.is_empty());
      let parsed = raw_value.and_then(parse_color_attribute);
      let color_value = parsed.unwrap_or(Rgba {
        r: 0,
        g: 0,
        b: 0,
        a: 1.0,
      });
      if raw_value.is_some() && parsed.is_none() && !disabled {
        invalid = true;
      }
      FormControlKind::Color {
        value: color_value,
        raw: raw_value.map(|v| v.to_string()),
      }
    } else {
      let size_attr = styled
        .node
        .get_attribute_ref("size")
        .and_then(|s| s.parse::<u32>().ok());
      let mut placeholder = styled
        .node
        .get_attribute_ref("placeholder")
        .filter(|p| !p.is_empty())
        .map(|p| p.to_string());
      let value = styled
        .node
        .get_attribute_ref("value")
        .filter(|v| !v.is_empty())
        .map(|v| v.to_string())
        .unwrap_or_default();

      let kind = if input_type.eq_ignore_ascii_case("password") {
        TextControlKind::Password
      } else if input_type.eq_ignore_ascii_case("number") {
        TextControlKind::Number
      } else if input_type.eq_ignore_ascii_case("date") {
        placeholder.get_or_insert_with(|| "yyyy-mm-dd".to_string());
        TextControlKind::Date
      } else if input_type.eq_ignore_ascii_case("datetime-local") {
        placeholder.get_or_insert_with(|| "yyyy-mm-dd hh:mm".to_string());
        TextControlKind::Date
      } else if input_type.eq_ignore_ascii_case("month") {
        placeholder.get_or_insert_with(|| "yyyy-mm".to_string());
        TextControlKind::Date
      } else if input_type.eq_ignore_ascii_case("week") {
        placeholder.get_or_insert_with(|| "yyyy-Www".to_string());
        TextControlKind::Date
      } else if input_type.eq_ignore_ascii_case("time") {
        placeholder.get_or_insert_with(|| "hh:mm".to_string());
        TextControlKind::Date
      } else if input_type.is_empty()
        || input_type.eq_ignore_ascii_case("text")
        || input_type.eq_ignore_ascii_case("search")
        || input_type.eq_ignore_ascii_case("url")
        || input_type.eq_ignore_ascii_case("tel")
        || input_type.eq_ignore_ascii_case("email")
      {
        TextControlKind::Plain
      } else {
        let label = placeholder
          .or_else(|| (!value.is_empty()).then_some(value))
          .or_else(|| Some(input_type.to_ascii_uppercase()));
        return Some(FormControl {
          control: FormControlKind::Unknown { label },
          appearance,
          disabled,
          focused,
          focus_visible,
          required,
          invalid,
        });
      };

      FormControlKind::Text {
        value,
        placeholder,
        size_attr,
        kind,
      }
    };

    Some(FormControl {
      control,
      appearance,
      disabled,
      focused,
      focus_visible,
      required,
      invalid,
    })
  } else if tag.eq_ignore_ascii_case("textarea") {
    Some(FormControl {
      control: FormControlKind::TextArea {
        value: textarea_value.unwrap_or_else(|| collect_text_content(styled)),
        rows: styled
          .node
          .get_attribute_ref("rows")
          .and_then(|r| r.parse::<u32>().ok()),
        cols: styled
          .node
          .get_attribute_ref("cols")
          .and_then(|c| c.parse::<u32>().ok()),
      },
      appearance,
      disabled,
      focused,
      focus_visible,
      required,
      invalid,
    })
  } else if tag.eq_ignore_ascii_case("select") {
    let label = select_label(styled).unwrap_or_else(|| "Select".to_string());
    Some(FormControl {
      control: FormControlKind::Select {
        label,
        multiple: styled.node.get_attribute_ref("multiple").is_some(),
      },
      appearance,
      disabled,
      focused,
      focus_visible,
      required,
      invalid,
    })
  } else if tag.eq_ignore_ascii_case("button") {
    Some(FormControl {
      control: FormControlKind::Button {
        label: button_label(styled),
      },
      appearance,
      disabled,
      focused,
      focus_visible,
      required,
      invalid,
    })
  } else {
    None
  }
}

/// Checks if an element is a replaced element
///
/// Replaced elements are those whose content is replaced by an external resource,
/// such as images, videos, iframes, etc. These elements have intrinsic dimensions.
pub fn is_replaced_element(tag: &str) -> bool {
  tag.eq_ignore_ascii_case("img")
    || tag.eq_ignore_ascii_case("video")
    || tag.eq_ignore_ascii_case("canvas")
    || tag.eq_ignore_ascii_case("svg")
    || tag.eq_ignore_ascii_case("iframe")
    || tag.eq_ignore_ascii_case("embed")
    || tag.eq_ignore_ascii_case("object")
    || tag.eq_ignore_ascii_case("audio")
    || tag.eq_ignore_ascii_case("math")
}

/// Creates a BoxNode for a replaced element from a StyledNode
fn create_replaced_box_from_styled(
  styled: &StyledNode,
  style: Arc<ComputedStyle>,
  document_css: &str,
  svg_document_css_style_element: Option<&Arc<str>>,
  picture_sources: Vec<PictureSource>,
) -> BoxNode {
  let tag = styled.node.tag_name().unwrap_or("img");

  // Determine replaced type
  let replaced_type = if tag.eq_ignore_ascii_case("img") {
    let src = styled.node.get_attribute("src").unwrap_or_default();
    let alt = styled
      .node
      .get_attribute_ref("alt")
      .filter(|s| !s.is_empty())
      .map(|s| s.to_string());
    let srcset = styled
      .node
      .get_attribute_ref("srcset")
      .map(parse_srcset)
      .unwrap_or_default();
    let sizes = styled.node.get_attribute_ref("sizes").and_then(parse_sizes);
    ReplacedType::Image {
      src,
      alt,
      srcset,
      sizes,
      picture_sources,
    }
  } else if tag.eq_ignore_ascii_case("video") {
    let src = styled.node.get_attribute("src").unwrap_or_default();
    let poster = styled
      .node
      .get_attribute_ref("poster")
      .filter(|s| !s.is_empty())
      .map(|s| s.to_string());
    ReplacedType::Video { src, poster }
  } else if tag.eq_ignore_ascii_case("audio") {
    let src = styled.node.get_attribute("src").unwrap_or_default();
    ReplacedType::Audio { src }
  } else if tag.eq_ignore_ascii_case("canvas") {
    ReplacedType::Canvas
  } else if tag.eq_ignore_ascii_case("svg") {
    ReplacedType::Svg {
      content: serialize_svg_subtree(styled, document_css, svg_document_css_style_element),
    }
  } else if tag.eq_ignore_ascii_case("iframe") {
    let src = styled.node.get_attribute("src").unwrap_or_default();
    let srcdoc = styled
      .node
      .get_attribute_ref("srcdoc")
      .filter(|s| !s.is_empty())
      .map(|s| s.to_string());
    ReplacedType::Iframe { src, srcdoc }
  } else if tag.eq_ignore_ascii_case("embed") {
    let src = styled.node.get_attribute("src").unwrap_or_default();
    ReplacedType::Embed { src }
  } else if tag.eq_ignore_ascii_case("object") {
    let data = styled.node.get_attribute("data").unwrap_or_default();
    ReplacedType::Object { data }
  } else {
    let src = styled.node.get_attribute("src").unwrap_or_default();
    let alt = styled
      .node
      .get_attribute_ref("alt")
      .filter(|s| !s.is_empty())
      .map(|s| s.to_string());
    ReplacedType::Image {
      src,
      alt,
      sizes: None,
      srcset: Vec::new(),
      picture_sources: Vec::new(),
    }
  };

  let width_attr = styled.node.get_attribute_ref("width");
  let height_attr = styled.node.get_attribute_ref("height");

  let (mut intrinsic_size, mut aspect_ratio) = match &replaced_type {
    ReplacedType::Svg { .. } => {
      let view_box_attr = styled.node.get_attribute_ref("viewBox");
      let preserve_aspect_ratio_attr = styled.node.get_attribute_ref("preserveAspectRatio");
      let svg_intrinsic = svg_intrinsic_dimensions_from_attributes(
        width_attr,
        height_attr,
        view_box_attr,
        preserve_aspect_ratio_attr,
      );

      let size = match (svg_intrinsic.width, svg_intrinsic.height) {
        (Some(w), Some(h)) => Size::new(w, h),
        (Some(w), None) => Size::new(w, 150.0),
        (None, Some(h)) => Size::new(300.0, h),
        (None, None) => Size::new(300.0, 150.0),
      };
      (Some(size), svg_intrinsic.aspect_ratio)
    }
    _ => {
      let intrinsic_width = width_attr.and_then(|w| w.parse::<f32>().ok());

      let intrinsic_height = height_attr.and_then(|h| h.parse::<f32>().ok());

      let intrinsic_size = match (intrinsic_width, intrinsic_height) {
        (Some(w), Some(h)) => Some(Size::new(w, h)),
        _ => None,
      };

      let aspect_ratio = match (intrinsic_width, intrinsic_height) {
        (Some(w), Some(h)) if h > 0.0 => Some(w / h),
        _ => None,
      };

      (intrinsic_size, aspect_ratio)
    }
  };

  if intrinsic_size.is_none() && aspect_ratio.is_none() {
    match &replaced_type {
      ReplacedType::Canvas
      | ReplacedType::Video { .. }
      | ReplacedType::Iframe { .. }
      | ReplacedType::Embed { .. }
      | ReplacedType::Object { .. } => {
        intrinsic_size = Some(Size::new(300.0, 150.0));
        aspect_ratio = Some(2.0);
      }
      ReplacedType::Audio { .. } => {
        intrinsic_size = Some(Size::new(300.0, 32.0));
        aspect_ratio = Some(300.0 / 32.0);
      }
      _ => {}
    }
  }

  let replaced_box = ReplacedBox {
    replaced_type,
    intrinsic_size,
    aspect_ratio,
  };

  BoxNode {
    box_type: BoxType::Replaced(replaced_box),
    style,
    starting_style: None,
    children: vec![],
    id: 0,
    debug_info: None,
    styled_node_id: None,
    first_line_style: None,
    first_letter_style: None,
  }
}

#[cfg(test)]
mod tests {
  use super::generate_box_tree as generate_box_tree_result;
  use super::generate_box_tree_with_anonymous_fixup as generate_box_tree_with_anonymous_fixup_result;
  use super::*;
  use crate::dom;
  use crate::dom::HTML_NAMESPACE;
  use crate::geometry::Size;
  use crate::style;
  use crate::style::cascade::StartingStyleSet;
  use crate::style::counter_styles::{CounterStyleRegistry, CounterStyleRule, CounterSystem};
  use crate::style::counters::CounterSet;
  use crate::tree::box_tree::FormControl;
  use crate::tree::box_tree::FormControlKind;
  use crate::tree::box_tree::MarkerContent;
  use crate::tree::box_tree::ReplacedType;
  use crate::tree::box_tree::TextControlKind;

  fn default_style() -> Arc<ComputedStyle> {
    Arc::new(ComputedStyle::default())
  }

  fn styled_element(tag: &str) -> StyledNode {
    StyledNode {
      node_id: 0,
      node: DomNode {
        node_type: DomNodeType::Element {
          tag_name: tag.to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      },
      styles: Arc::new(ComputedStyle::default()),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    }
  }

  fn generate_box_tree(styled: &StyledNode) -> BoxTree {
    generate_box_tree_result(styled).expect("box generation failed")
  }

  fn generate_box_tree_with_anonymous_fixup(styled: &StyledNode) -> BoxTree {
    generate_box_tree_with_anonymous_fixup_result(styled).expect("anonymous box generation failed")
  }

  #[test]
  fn box_generation_reuses_computed_style_arcs() {
    let root_style = Arc::new(ComputedStyle::default());
    let text_style = Arc::new(ComputedStyle::default());
    assert!(
      !Arc::ptr_eq(&root_style, &text_style),
      "test setup requires distinct style arcs"
    );

    let text_dom = DomNode {
      node_type: DomNodeType::Text {
        content: "hello".to_string(),
      },
      children: vec![],
    };
    let text_node = StyledNode {
      node_id: 1,
      node: text_dom,
      styles: Arc::clone(&text_style),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    };

    let root_dom = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };
    let root_node = StyledNode {
      node_id: 0,
      node: root_dom,
      styles: Arc::clone(&root_style),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![text_node],
    };

    fn find_text_box<'a>(node: &'a BoxNode) -> Option<&'a BoxNode> {
      if matches!(node.box_type, BoxType::Text(_)) {
        return Some(node);
      }
      node.children.iter().find_map(find_text_box)
    }

    let tree = generate_box_tree(&root_node);

    assert!(
      Arc::ptr_eq(&tree.root.style, &root_style),
      "expected box tree to reuse the computed style Arc for element boxes"
    );

    let text_box = find_text_box(&tree.root).expect("expected a text box");
    assert!(
      Arc::ptr_eq(&text_box.style, &text_style),
      "expected box tree to reuse the computed style Arc for text boxes"
    );
  }

  #[test]
  fn generate_box_tree_includes_marker_from_marker_styles() {
    use style::color::Rgba;
    use style::display::Display;
    use style::types::ListStyleType;

    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_type = ListStyleType::Decimal;

    let mut marker_style = ComputedStyle::default();
    marker_style.display = Display::Inline;
    marker_style.content = "✱".to_string();
    marker_style.color = Rgba::RED;

    let text_dom = dom::DomNode {
      node_type: dom::DomNodeType::Text {
        content: "Item".to_string(),
      },
      children: vec![],
    };
    let text_node = StyledNode {
      node_id: 0,
      node: text_dom.clone(),
      styles: Arc::new(ComputedStyle::default()),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    };

    let li_dom = dom::DomNode {
      node_type: dom::DomNodeType::Element {
        tag_name: "li".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![text_dom],
    };

    let li = StyledNode {
      node_id: 0,
      node: li_dom,
      styles: Arc::new(li_style),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: Some(Arc::new(marker_style)),
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![text_node],
    };

    let tree = generate_box_tree(&li);
    assert_eq!(tree.root.children.len(), 2);
    let marker = tree.root.children.first().expect("marker");
    assert!(matches!(marker.box_type, BoxType::Marker(_)));
    assert_eq!(marker.text(), Some("✱"));
    assert_eq!(marker.style.color, Rgba::RED);
  }

  #[test]
  fn audio_generates_replaced_box_with_fallback_size() {
    let html = "<html><body><audio src=\"sound.mp3\"></audio></body></html>";
    let dom = crate::dom::parse_html(html).expect("parse");
    let styled = crate::style::cascade::apply_styles(&dom, &crate::css::types::StyleSheet::new());
    let box_tree = generate_box_tree(&styled);

    fn find_audio(node: &BoxNode, out: &mut Vec<ReplacedBox>) {
      if let BoxType::Replaced(repl) = &node.box_type {
        if matches!(repl.replaced_type, ReplacedType::Audio { .. }) {
          out.push(repl.clone());
        }
      }
      for child in node.children.iter() {
        find_audio(child, out);
      }
    }

    let mut audios = Vec::new();
    find_audio(&box_tree.root, &mut audios);
    assert_eq!(audios.len(), 1, "expected one audio replaced box");
    let audio = &audios[0];
    assert_eq!(
      audio.intrinsic_size,
      Some(Size::new(300.0, 32.0)),
      "audio should get default UA size when none provided"
    );
  }

  #[test]
  fn object_without_data_renders_children_instead_of_replacement() {
    // When <object> lacks a data attribute, it should fall back to its children.
    let html = "<html><body><object><p id=\"fallback\">hi</p></object></body></html>";
    let dom = crate::dom::parse_html(html).expect("parse");
    let styled = crate::style::cascade::apply_styles(&dom, &crate::css::types::StyleSheet::new());
    let box_tree = generate_box_tree(&styled);

    fn count_object_replacements(node: &BoxNode) -> usize {
      let mut count = 0;
      if let BoxType::Replaced(repl) = &node.box_type {
        if matches!(repl.replaced_type, ReplacedType::Object { .. }) {
          count += 1;
        }
      }
      for child in node.children.iter() {
        count += count_object_replacements(child);
      }
      count
    }

    fn collect_text(node: &BoxNode, out: &mut Vec<String>) {
      if let BoxType::Text(text) = &node.box_type {
        out.push(text.text.clone());
      }
      for child in node.children.iter() {
        collect_text(child, out);
      }
    }

    assert_eq!(
      count_object_replacements(&box_tree.root),
      0,
      "object without data should render fallback content"
    );

    let mut texts = Vec::new();
    collect_text(&box_tree.root, &mut texts);
    assert!(
      texts.iter().any(|t| t.contains("hi")),
      "fallback text from object children should be present"
    );
  }

  #[test]
  fn form_controls_generate_replaced_boxes() {
    let html = "<html><body>
      <input id=\"text\" value=\"hello\">
      <input type=\"checkbox\" checked>
      <input type=\"radio\" checked>
      <select><option selected>One</option><option>Two</option></select>
      <button>Go</button>
      <textarea>note</textarea>
    </body></html>";
    let dom = crate::dom::parse_html(html).expect("parse");
    let styled = crate::style::cascade::apply_styles(&dom, &crate::css::types::StyleSheet::new());
    let box_tree = generate_box_tree(&styled);

    fn collect_controls(node: &BoxNode, out: &mut Vec<ReplacedType>) {
      if let BoxType::Replaced(repl) = &node.box_type {
        if matches!(repl.replaced_type, ReplacedType::FormControl(_)) {
          out.push(repl.replaced_type.clone());
        }
      }
      for child in node.children.iter() {
        collect_controls(child, out);
      }
    }

    let mut controls = Vec::new();
    collect_controls(&box_tree.root, &mut controls);
    assert_eq!(controls.len(), 6, "all form controls should be replaced");

    assert!(controls.iter().any(|c| matches!(
      c,
      ReplacedType::FormControl(FormControl {
        control: FormControlKind::Checkbox {
          is_radio: false,
          checked: true,
          ..
        },
        ..
      })
    )));

    assert!(controls.iter().any(|c| matches!(
      c,
      ReplacedType::FormControl(FormControl {
        control: FormControlKind::Select { label, .. },
        ..
      }) if label == "One"
    )));
  }

  #[test]
  fn new_form_control_input_types_are_identified() {
    let html = "<html><body>
      <input type=\"password\" value=\"abc\">
      <input type=\"number\" value=\"5\" data-fastr-focus=\"true\" data-fastr-focus-visible=\"true\">
      <input type=\"color\" value=\"#00ff00\">
      <input type=\"color\" value=\"not-a-color\" disabled>
      <input type=\"date\" required>
      <input type=\"datetime-local\">
      <input type=\"month\">
      <input type=\"week\">
      <input type=\"time\">
      <input type=\"number\" size=\"7\" placeholder=\"sized number\">
      <input type=\"checkbox\" indeterminate=\"true\">
      <input type=\"foo\" placeholder=\"mystery\" data-fastr-focus-visible=\"true\">
      <input size=\"5\" value=\"sized\">
      <textarea rows=\"4\" cols=\"10\">hi</textarea>
    </body></html>";
    let dom = crate::dom::parse_html(html).expect("parse");
    let styled = crate::style::cascade::apply_styles(&dom, &crate::css::types::StyleSheet::new());
    let box_tree = generate_box_tree(&styled);

    fn collect_controls(node: &BoxNode, out: &mut Vec<FormControl>) {
      if let BoxType::Replaced(repl) = &node.box_type {
        if let ReplacedType::FormControl(control) = &repl.replaced_type {
          out.push(control.clone());
        }
      }
      for child in node.children.iter() {
        collect_controls(child, out);
      }
    }

    let mut controls = Vec::new();
    collect_controls(&box_tree.root, &mut controls);

    assert!(
      controls.iter().any(|c| matches!(
        &c.control,
        FormControlKind::Text {
          kind: TextControlKind::Password,
          ..
        }
      )),
      "password input should map to password text control"
    );
    assert!(
      controls.iter().any(|c| matches!(
        &c.control,
        FormControlKind::Text {
          kind: TextControlKind::Number,
          value,
          ..
        } if value == "5"
      ) && c.focus_visible),
      "number input should be recognized and keep focus-visible hint"
    );
    assert!(
      controls
        .iter()
        .any(|c| matches!(&c.control, FormControlKind::Color { .. })),
      "color input should generate a color control"
    );
    assert!(
      controls.iter().any(|c| {
        matches!(
          &c.control,
          FormControlKind::Color { raw, .. } if raw.as_deref() == Some("not-a-color")
        ) && c.disabled
          && !c.invalid
      }),
      "disabled color inputs with invalid values should stay valid for painting"
    );
    assert!(
      controls.iter().any(|c| matches!(
        &c.control,
        FormControlKind::Text {
          kind: TextControlKind::Date,
          ..
        }
      ) && c.required
        && c.invalid),
      "required date input without value should be marked invalid"
    );
    assert!(
      controls.iter().any(|c| matches!(
        &c.control,
        FormControlKind::Text {
          kind: TextControlKind::Date,
          placeholder,
          ..
        } if placeholder.as_deref() == Some("yyyy-mm-dd hh:mm")
      )),
      "datetime-local inputs should synthesize a datetime placeholder"
    );
    assert!(
      controls.iter().any(|c| matches!(
        &c.control,
        FormControlKind::Text {
          kind: TextControlKind::Date,
          placeholder,
          ..
        } if placeholder.as_deref() == Some("yyyy-mm")
      )),
      "month inputs should synthesize a month placeholder"
    );
    assert!(
      controls.iter().any(|c| matches!(
        &c.control,
        FormControlKind::Text {
          kind: TextControlKind::Date,
          placeholder,
          ..
        } if placeholder.as_deref() == Some("yyyy-Www")
      )),
      "week inputs should synthesize a week placeholder"
    );
    assert!(
      controls.iter().any(|c| matches!(
        &c.control,
        FormControlKind::Text {
          kind: TextControlKind::Date,
          placeholder,
          ..
        } if placeholder.as_deref() == Some("hh:mm")
      )),
      "time inputs should synthesize a time placeholder"
    );
    assert!(
      controls.iter().any(|c| matches!(
        &c.control,
        FormControlKind::Checkbox {
          indeterminate: true,
          ..
        }
      )),
      "indeterminate checkbox should be captured"
    );
    assert!(
      controls
        .iter()
        .any(|c| matches!(&c.control, FormControlKind::Unknown { label }
        if label.as_deref() == Some("mystery"))
          && c.focus_visible
          && c.focused),
      "unknown types should fall back to labeled control and keep focus-visible hint"
    );
    assert!(
      controls.iter().any(|c| matches!(
        &c.control,
        FormControlKind::Text {
          size_attr: Some(5),
          kind: TextControlKind::Plain,
          ..
        }
      )),
      "size attribute should be preserved on text-like inputs"
    );
    assert!(
      controls.iter().any(|c| matches!(
        &c.control,
        FormControlKind::Text {
          size_attr: Some(7),
          kind: TextControlKind::Number,
          placeholder,
          ..
        } if placeholder.as_deref() == Some("sized number")
      )),
      "number inputs should keep size hints and placeholder text"
    );
    assert!(
      controls.iter().any(|c| matches!(
        &c.control,
        FormControlKind::TextArea { rows, cols, .. } if rows == &Some(4) && cols == &Some(10)
      )),
      "rows/cols should be captured on textarea for intrinsic sizing"
    );
  }

  #[test]
  fn appearance_none_avoids_form_control_replacement() {
    let html =
      "<html><body><input id=\"plain\" style=\"appearance: none; border: 0\"></body></html>";
    let dom = crate::dom::parse_html(html).expect("parse");
    let styled = crate::style::cascade::apply_styles(&dom, &crate::css::types::StyleSheet::new());
    let box_tree = generate_box_tree(&styled);

    fn count_replaced(node: &BoxNode) -> usize {
      let mut count = 0;
      if let BoxType::Replaced(repl) = &node.box_type {
        if matches!(repl.replaced_type, ReplacedType::FormControl(_)) {
          count += 1;
        }
      }
      for child in node.children.iter() {
        count += count_replaced(child);
      }
      count
    }

    assert_eq!(
      count_replaced(&box_tree.root),
      0,
      "appearance:none should disable native control replacement"
    );
  }

  #[test]
  fn replaced_media_defaults_to_300_by_150() {
    let style = default_style();

    for tag in ["canvas", "video", "iframe", "embed", "object"] {
      let styled = styled_element(tag);
      let box_node = create_replaced_box_from_styled(&styled, style.clone(), "", None, Vec::new());
      match &box_node.box_type {
        BoxType::Replaced(replaced) => {
          assert_eq!(
            replaced.intrinsic_size,
            Some(Size::new(300.0, 150.0)),
            "{tag} should default to 300x150"
          );
          assert_eq!(
            replaced.aspect_ratio,
            Some(2.0),
            "{tag} should default to 2:1 ratio"
          );
        }
        other => panic!("expected replaced box for {tag}, got {:?}", other),
      }
    }
  }

  #[test]
  fn display_contents_splices_children_into_parent() {
    let mut root = styled_element("div");
    Arc::make_mut(&mut root.styles).display = Display::Block;

    let mut contents = styled_element("section");
    Arc::make_mut(&mut contents.styles).display = Display::Contents;

    let mut child1 = styled_element("p");
    Arc::make_mut(&mut child1.styles).display = Display::Block;
    let mut child2 = styled_element("p");
    Arc::make_mut(&mut child2.styles).display = Display::Block;

    contents.children = vec![child1, child2];
    root.children = vec![contents];

    let tree = generate_box_tree(&root);
    assert_eq!(
      tree.root.children.len(),
      2,
      "contents element should not create a box"
    );
    let tags: Vec<_> = tree
      .root
      .children
      .iter()
      .filter_map(|c| c.debug_info.as_ref().and_then(|d| d.tag_name.clone()))
      .collect();
    assert_eq!(tags, vec!["p".to_string(), "p".to_string()]);
  }

  #[test]
  fn whitespace_text_nodes_are_preserved() {
    let mut root = styled_element("div");
    Arc::make_mut(&mut root.styles).display = Display::Block;
    let mut child = styled_element("span");
    Arc::make_mut(&mut child.styles).display = Display::Inline;
    child.node = dom::DomNode {
      node_type: dom::DomNodeType::Text {
        content: "   ".to_string(),
      },
      children: vec![],
    };
    root.children = vec![child];

    let tree = generate_box_tree(&root);
    assert_eq!(tree.root.children.len(), 1);
    assert!(
      tree.root.children[0].is_text(),
      "whitespace text should produce a text box"
    );
  }

  // =============================================================================
  // Utility Method Tests
  // =============================================================================

  #[cfg(any(test, feature = "box_generation_demo"))]
  #[test]
  fn test_count_boxes_utility() {
    let style = default_style();

    // Single box
    let single = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    assert_eq!(BoxGenerator::count_boxes(&single), 1);

    // Parent with 3 children
    let children = vec![
      BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]),
      BoxNode::new_inline(style.clone(), vec![]),
      BoxNode::new_text(style.clone(), "text".to_string()),
    ];
    let parent = BoxNode::new_block(style, FormattingContextType::Block, children);
    assert_eq!(BoxGenerator::count_boxes(&parent), 4);
  }

  #[cfg(any(test, feature = "box_generation_demo"))]
  #[test]
  fn test_find_boxes_by_predicate() {
    let style = default_style();

    let text1 = BoxNode::new_text(style.clone(), "Hello".to_string());
    let text2 = BoxNode::new_text(style.clone(), "World".to_string());
    let inline = BoxNode::new_inline(style.clone(), vec![text1]);
    let block = BoxNode::new_block(
      style.clone(),
      FormattingContextType::Block,
      vec![inline, text2],
    );

    // Find all text boxes
    let text_boxes = BoxGenerator::find_boxes_by_predicate(&block, |b| b.is_text());
    assert_eq!(text_boxes.len(), 2);

    // Find inline boxes (inline + 2 text boxes since text is inline-level)
    let inline_boxes = BoxGenerator::find_boxes_by_predicate(&block, |b| b.is_inline_level());
    assert_eq!(inline_boxes.len(), 3); // 1 inline + 2 text boxes
  }

  #[test]
  fn test_find_block_boxes() {
    let style = default_style();

    let text = BoxNode::new_text(style.clone(), "text".to_string());
    let inline = BoxNode::new_inline(style.clone(), vec![]);
    let block1 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    let block2 = BoxNode::new_block(style.clone(), FormattingContextType::Flex, vec![]);
    let root = BoxNode::new_block(
      style,
      FormattingContextType::Block,
      vec![text, inline, block1, block2],
    );

    let blocks = BoxGenerator::find_block_boxes(&root);
    assert_eq!(blocks.len(), 3); // root + block1 + block2
  }

  #[test]
  fn test_find_inline_boxes() {
    let style = default_style();

    let text = BoxNode::new_text(style.clone(), "text".to_string());
    let inline1 = BoxNode::new_inline(style.clone(), vec![text]);
    let inline2 = BoxNode::new_inline(style.clone(), vec![]);
    let root = BoxNode::new_block(style, FormattingContextType::Block, vec![inline1, inline2]);

    // inline1 + inline2 + text (text is also inline-level)
    let inlines = BoxGenerator::find_inline_boxes(&root);
    assert_eq!(inlines.len(), 3);
  }

  #[test]
  fn test_find_text_boxes() {
    let style = default_style();

    let text1 = BoxNode::new_text(style.clone(), "Hello".to_string());
    let text2 = BoxNode::new_text(style.clone(), "World".to_string());
    let inline = BoxNode::new_inline(style.clone(), vec![text1]);
    let root = BoxNode::new_block(style, FormattingContextType::Block, vec![inline, text2]);

    let texts = BoxGenerator::find_text_boxes(&root);
    assert_eq!(texts.len(), 2);
  }

  #[test]
  fn test_find_replaced_boxes_utility() {
    let style = default_style();

    let img = BoxNode::new_replaced(
      style.clone(),
      ReplacedType::Image {
        src: "test.png".to_string(),
        alt: None,
        sizes: None,
        srcset: Vec::new(),
        picture_sources: Vec::new(),
      },
      Some(Size::new(100.0, 100.0)),
      Some(1.0),
    );
    let video = BoxNode::new_replaced(
      style.clone(),
      ReplacedType::Video {
        src: "test.mp4".to_string(),
        poster: None,
      },
      None,
      None,
    );
    let text = BoxNode::new_text(style.clone(), "text".to_string());
    let root = BoxNode::new_block(style, FormattingContextType::Block, vec![img, video, text]);

    let replaced = BoxGenerator::find_replaced_boxes(&root);
    assert_eq!(replaced.len(), 2);
  }
  #[test]
  fn fallback_marker_resets_box_model_but_inherits_color() {
    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.color = crate::style::color::Rgba::RED;
    li_style.padding_left = crate::style::values::Length::px(20.0);
    li_style.margin_left = Some(crate::style::values::Length::px(10.0));

    let styled = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      },
      styles: Arc::new(li_style),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    };

    let tree = generate_box_tree(&styled);
    let marker = match tree.root.children.first().expect("marker").box_type {
      BoxType::Marker(_) => tree.root.children.first().unwrap(),
      _ => panic!("expected marker as first child"),
    };
    assert_eq!(marker.style.color, crate::style::color::Rgba::RED);
    assert!(marker.style.padding_left.is_zero());
    assert!(marker.style.margin_left.unwrap().is_zero());
    assert_eq!(
      marker.style.background_color,
      crate::style::color::Rgba::TRANSPARENT
    );
  }

  #[test]
  fn marker_styles_keep_text_decorations_and_shadows() {
    use crate::css::types::TextShadow;
    use crate::style::color::Rgba;
    use crate::style::counters::CounterManager;
    use crate::style::types::ListStyleType;
    use crate::style::types::TextDecorationLine;
    use crate::style::types::TextDecorationStyle;
    use crate::style::types::TextDecorationThickness;
    use crate::style::values::Length;

    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_type = ListStyleType::Decimal;

    let mut marker_styles = ComputedStyle::default();
    marker_styles.display = Display::Inline;
    marker_styles.list_style_type = ListStyleType::Decimal;
    marker_styles.text_decoration.lines = TextDecorationLine::UNDERLINE;
    marker_styles.text_decoration.style = TextDecorationStyle::Wavy;
    marker_styles.text_decoration.color = Some(Rgba::BLUE);
    marker_styles.text_decoration.thickness = TextDecorationThickness::Length(Length::px(2.0));
    marker_styles.text_shadow = vec![TextShadow {
      offset_x: Length::px(1.0),
      offset_y: Length::px(2.0),
      blur_radius: Length::px(3.0),
      color: Some(Rgba::GREEN),
    }]
    .into();
    marker_styles.padding_left = Length::px(8.0);
    marker_styles.margin_left = Some(Length::px(4.0));
    marker_styles.background_color = Rgba::rgb(255, 0, 255);

    let styled = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      },
      styles: Arc::new(li_style),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: Some(Arc::new(marker_styles)),
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    };

    let marker_box =
      create_marker_box(&styled, &CounterManager::default()).expect("marker should be generated");
    let style = marker_box.style.as_ref();
    assert!(style
      .text_decoration
      .lines
      .contains(TextDecorationLine::UNDERLINE));
    assert_eq!(style.text_decoration.style, TextDecorationStyle::Wavy);
    assert_eq!(style.text_decoration.color, Some(Rgba::BLUE));
    assert_eq!(
      style.text_decoration.thickness,
      TextDecorationThickness::Length(Length::px(2.0))
    );
    assert_eq!(style.text_shadow.len(), 1);
    assert_eq!(style.text_shadow[0].offset_x, Length::px(1.0));
    assert_eq!(style.text_shadow[0].offset_y, Length::px(2.0));
    assert_eq!(style.text_shadow[0].blur_radius, Length::px(3.0));
    assert_eq!(style.text_shadow[0].color, Some(Rgba::GREEN));

    // Layout-affecting properties are reset even when authored on ::marker.
    assert!(style.padding_left.is_zero());
    assert!(style.margin_left.unwrap().is_zero());
    assert_eq!(style.background_color, Rgba::TRANSPARENT);
  }

  #[test]
  fn marker_styles_preserve_text_transform() {
    use crate::style::counters::CounterManager;
    use crate::style::types::CaseTransform;
    use crate::style::types::ListStyleType;

    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_type = ListStyleType::String("abc".to_string());

    let mut marker_styles = ComputedStyle::default();
    marker_styles.display = Display::Inline;
    marker_styles.list_style_type = ListStyleType::String("abc".to_string());
    marker_styles.text_transform = TextTransform::with_case(CaseTransform::Uppercase);

    let styled = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      },
      styles: Arc::new(li_style),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: Some(Arc::new(marker_styles)),
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    };

    let marker_box =
      create_marker_box(&styled, &CounterManager::default()).expect("marker should be generated");
    assert!(matches!(marker_box.box_type, BoxType::Marker(_)));
    assert_eq!(
      marker_box.style.text_transform,
      TextTransform::with_case(CaseTransform::Uppercase)
    );
  }

  #[test]
  fn pseudo_content_respects_quotes_property() {
    use crate::dom::DomNodeType;
    use crate::style::content::ContentItem;
    use crate::style::content::ContentValue;

    let mut before_style = ComputedStyle::default();
    before_style.content_value = ContentValue::Items(vec![
      ContentItem::OpenQuote,
      ContentItem::String("hi".to_string()),
    ]);
    before_style.quotes = vec![("«".to_string(), "»".to_string())].into();

    let base_style = ComputedStyle::default();
    let styled = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      },
      styles: Arc::new(base_style),
      starting_styles: StartingStyleSet::default(),
      before_styles: Some(Arc::new(before_style)),
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    };

    let mut counters = CounterManager::new();
    counters.enter_scope();
    let before_box = create_pseudo_element_box(
      &styled,
      styled.before_styles.as_ref().unwrap(),
      clone_starting_style(&styled.starting_styles.before),
      "before",
      &counters,
    )
    .expect("before box");
    counters.leave_scope();

    assert_eq!(before_box.children.len(), 1);
    if let BoxType::Text(text) = &before_box.children[0].box_type {
      assert_eq!(text.text, "«hi");
    } else {
      panic!("expected text child");
    }
  }

  #[test]
  fn pseudo_content_supports_attr_counter_and_image() {
    use crate::dom::DomNodeType;
    use crate::style::content::ContentItem;
    use crate::style::content::ContentValue;

    let mut before_style = ComputedStyle::default();
    before_style.content_value = ContentValue::Items(vec![
      ContentItem::Attr {
        name: "data-label".to_string(),
        type_or_unit: None,
        fallback: Some("fallback".to_string()),
      },
      ContentItem::String(": ".to_string()),
      ContentItem::Counter {
        name: "item".to_string(),
        style: Some(CounterStyle::UpperRoman.into()),
      },
      ContentItem::String(" ".to_string()),
      ContentItem::Url("icon.png".to_string()),
    ]);

    let base_style = ComputedStyle::default();
    let styled = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: DomNodeType::Element {
          tag_name: "div".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("data-label".to_string(), "hello".to_string())],
        },
        children: vec![],
      },
      styles: Arc::new(base_style),
      starting_styles: StartingStyleSet::default(),
      before_styles: Some(Arc::new(before_style)),
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    };

    let mut counters = CounterManager::new();
    counters.enter_scope();
    counters.apply_reset(&CounterSet::single("item", 3));

    let before_box = create_pseudo_element_box(
      &styled,
      styled.before_styles.as_ref().unwrap(),
      clone_starting_style(&styled.starting_styles.before),
      "before",
      &counters,
    )
    .expect("before box");
    counters.leave_scope();

    // Expect inline container with text + image children
    assert_eq!(before_box.children.len(), 2);
    if let BoxType::Text(text) = &before_box.children[0].box_type {
      assert_eq!(text.text, "hello: III ");
    } else {
      panic!("expected text child");
    }
    if let BoxType::Replaced(replaced) = &before_box.children[1].box_type {
      match &replaced.replaced_type {
        ReplacedType::Image { src, .. } => assert_eq!(src, "icon.png"),
        _ => panic!("expected image replaced content"),
      }
    } else {
      panic!("expected replaced child");
    }
  }

  #[test]
  fn marker_content_resolves_structured_content() {
    use crate::dom::DomNodeType;
    use crate::style::content::ContentItem;
    use crate::style::content::ContentValue;

    let mut marker_style = ComputedStyle::default();
    marker_style.content_value = ContentValue::Items(vec![
      ContentItem::String("[".to_string()),
      ContentItem::Counter {
        name: "item".to_string(),
        style: Some(CounterStyle::LowerRoman.into()),
      },
      ContentItem::String("]".to_string()),
    ]);

    let base_style = ComputedStyle::default();
    let styled = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      },
      styles: Arc::new(base_style),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: Some(Arc::new(marker_style)),
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    };

    let mut counters = CounterManager::new();
    counters.enter_scope();
    counters.apply_reset(&CounterSet::single("item", 2));
    counters.apply_increment(&CounterSet::single("item", 1));

    let marker_box = create_marker_box(&styled, &counters).expect("marker");
    counters.leave_scope();

    match marker_box.box_type {
      BoxType::Marker(marker) => match marker.content {
        MarkerContent::Text(t) => assert_eq!(t, "[iii]"),
        MarkerContent::Image(_) => panic!("expected text marker"),
      },
      _ => panic!("expected marker box"),
    }
  }

  #[test]
  fn marker_uses_string_list_style_type() {
    let mut style = ComputedStyle::default();
    style.list_style_type = ListStyleType::String("★".to_string());
    style.display = Display::ListItem;
    let style = Arc::new(style);
    let styled = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      },
      styles: style.clone(),
      marker_styles: Some(style.clone()),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    };

    let mut counters = CounterManager::new();
    counters.enter_scope();
    counters.apply_reset(&CounterSet::single("list-item", 1));

    let marker_box = create_marker_box(&styled, &counters).expect("marker");
    counters.leave_scope();

    match marker_box.box_type {
      BoxType::Marker(marker) => match marker.content {
        MarkerContent::Text(t) => assert_eq!(t, "★ "),
        MarkerContent::Image(_) => panic!("expected text marker from string list-style-type"),
      },
      _ => panic!("expected marker box"),
    }
  }

  #[test]
  fn marker_uses_custom_counter_style_definition() {
    let mut registry = CounterStyleRegistry::with_builtins();
    let mut rule = CounterStyleRule::new("custom-mark");
    rule.system = Some(CounterSystem::Cyclic);
    rule.symbols = Some(vec!["◇".into()]);
    registry.register(rule);
    let registry = Arc::new(registry);

    let mut style = ComputedStyle::default();
    style.counter_styles = registry.clone();
    style.list_style_type = ListStyleType::Custom("custom-mark".to_string());
    style.display = Display::ListItem;
    let style = Arc::new(style);

    let styled = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      },
      styles: style.clone(),
      marker_styles: Some(style.clone()),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    };

    let mut counters = CounterManager::new_with_styles(registry);
    counters.enter_scope();
    counters.apply_reset(&CounterSet::single("list-item", 1));

    let marker_box = create_marker_box(&styled, &counters).expect("marker");
    counters.leave_scope();

    match marker_box.box_type {
      BoxType::Marker(marker) => match marker.content {
        MarkerContent::Text(t) => assert_eq!(t, "◇ "),
        MarkerContent::Image(_) => panic!("expected text marker"),
      },
      _ => panic!("expected marker box"),
    }
  }

  #[test]
  fn ordered_list_start_attribute_sets_initial_counter() {
    let mut ol_style = ComputedStyle::default();
    ol_style.display = Display::Block;
    let ol_style = Arc::new(ol_style);

    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_type = ListStyleType::Decimal;
    let li_style = Arc::new(li_style);

    let ol_dom = dom::DomNode {
      node_type: dom::DomNodeType::Element {
        tag_name: "ol".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("start".to_string(), "5".to_string())],
      },
      children: vec![],
    };

    let mk_li = |text: &str| StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![dom::DomNode {
          node_type: dom::DomNodeType::Text {
            content: text.to_string(),
          },
          children: vec![],
        }],
      },
      styles: li_style.clone(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![StyledNode {
        node_id: 0,
        node: dom::DomNode {
          node_type: dom::DomNodeType::Text {
            content: text.to_string(),
          },
          children: vec![],
        },
        styles: default_style(),
        starting_styles: StartingStyleSet::default(),
        before_styles: None,
        after_styles: None,
        marker_styles: None,
        first_line_styles: None,
        first_letter_styles: None,
        assigned_slot: None,
        slotted_node_ids: Vec::new(),
        children: vec![],
      }],
    };

    let ol = StyledNode {
      node_id: 0,
      node: ol_dom,
      styles: ol_style,
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![mk_li("one"), mk_li("two"), mk_li("three")],
    };

    let tree = generate_box_tree(&ol);
    let markers: Vec<String> = tree
      .root
      .children
      .iter()
      .filter_map(|li| {
        li.children.first().and_then(|child| match &child.box_type {
          BoxType::Marker(m) => match &m.content {
            MarkerContent::Text(t) => Some(t.clone()),
            MarkerContent::Image(_) => None,
          },
          _ => None,
        })
      })
      .collect();

    assert_eq!(markers, vec!["5 ", "6 ", "7 "]);
  }

  #[test]
  fn ordered_list_defaults_start_at_one() {
    let mut ol_style = ComputedStyle::default();
    ol_style.display = Display::Block;
    let ol_style = Arc::new(ol_style);

    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_type = ListStyleType::Decimal;
    let li_style = Arc::new(li_style);

    let ol_dom = dom::DomNode {
      node_type: dom::DomNodeType::Element {
        tag_name: "ol".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let mk_li = |text: &str| StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![dom::DomNode {
          node_type: dom::DomNodeType::Text {
            content: text.to_string(),
          },
          children: vec![],
        }],
      },
      styles: li_style.clone(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![StyledNode {
        node_id: 0,
        node: dom::DomNode {
          node_type: dom::DomNodeType::Text {
            content: text.to_string(),
          },
          children: vec![],
        },
        styles: default_style(),
        starting_styles: StartingStyleSet::default(),
        before_styles: None,
        after_styles: None,
        marker_styles: None,
        first_line_styles: None,
        first_letter_styles: None,
        assigned_slot: None,
        slotted_node_ids: Vec::new(),
        children: vec![],
      }],
    };

    let ol = StyledNode {
      node_id: 0,
      node: ol_dom,
      styles: ol_style,
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![mk_li("one"), mk_li("two"), mk_li("three")],
    };

    let tree = generate_box_tree(&ol);
    let markers: Vec<String> = tree
      .root
      .children
      .iter()
      .filter_map(|li| {
        li.children.first().and_then(|child| match &child.box_type {
          BoxType::Marker(m) => match &m.content {
            MarkerContent::Text(t) => Some(t.clone()),
            MarkerContent::Image(_) => None,
          },
          _ => None,
        })
      })
      .collect();

    assert_eq!(markers, vec!["1 ", "2 ", "3 "]);
  }

  #[test]
  fn reversed_ordered_list_counts_down() {
    let mut ol_style = ComputedStyle::default();
    ol_style.display = Display::Block;
    let ol_style = Arc::new(ol_style);

    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_type = ListStyleType::Decimal;
    let li_style = Arc::new(li_style);

    let ol_dom = dom::DomNode {
      node_type: dom::DomNodeType::Element {
        tag_name: "ol".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("reversed".to_string(), String::new())],
      },
      children: vec![],
    };

    let mk_li = |text: &str| StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![dom::DomNode {
          node_type: dom::DomNodeType::Text {
            content: text.to_string(),
          },
          children: vec![],
        }],
      },
      styles: li_style.clone(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![StyledNode {
        node_id: 0,
        node: dom::DomNode {
          node_type: dom::DomNodeType::Text {
            content: text.to_string(),
          },
          children: vec![],
        },
        styles: default_style(),
        starting_styles: StartingStyleSet::default(),
        before_styles: None,
        after_styles: None,
        marker_styles: None,
        first_line_styles: None,
        first_letter_styles: None,
        assigned_slot: None,
        slotted_node_ids: Vec::new(),
        children: vec![],
      }],
    };

    let ol = StyledNode {
      node_id: 0,
      node: ol_dom,
      styles: ol_style,
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![mk_li("one"), mk_li("two"), mk_li("three")],
    };

    let tree = generate_box_tree(&ol);
    let markers: Vec<String> = tree
      .root
      .children
      .iter()
      .filter_map(|li| {
        li.children.first().and_then(|child| match &child.box_type {
          BoxType::Marker(m) => match &m.content {
            MarkerContent::Text(t) => Some(t.clone()),
            MarkerContent::Image(_) => None,
          },
          _ => None,
        })
      })
      .collect();

    assert_eq!(markers, vec!["3 ", "2 ", "1 "]);
  }

  #[test]
  fn li_value_attribute_sets_counter_for_that_item() {
    let mut ol_style = ComputedStyle::default();
    ol_style.display = Display::Block;
    let ol_style = Arc::new(ol_style);

    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_type = ListStyleType::Decimal;
    let li_style = Arc::new(li_style);

    let ol_dom = dom::DomNode {
      node_type: dom::DomNodeType::Element {
        tag_name: "ol".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![],
      },
      children: vec![],
    };

    let mk_li = |text: &str, value: Option<&str>| StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: value
            .map(|v| vec![("value".to_string(), v.to_string())])
            .unwrap_or_else(Vec::new),
        },
        children: vec![dom::DomNode {
          node_type: dom::DomNodeType::Text {
            content: text.to_string(),
          },
          children: vec![],
        }],
      },
      styles: li_style.clone(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![StyledNode {
        node_id: 0,
        node: dom::DomNode {
          node_type: dom::DomNodeType::Text {
            content: text.to_string(),
          },
          children: vec![],
        },
        styles: default_style(),
        starting_styles: StartingStyleSet::default(),
        before_styles: None,
        after_styles: None,
        marker_styles: None,
        first_line_styles: None,
        first_letter_styles: None,
        assigned_slot: None,
        slotted_node_ids: Vec::new(),
        children: vec![],
      }],
    };

    let ol = StyledNode {
      node_id: 0,
      node: ol_dom,
      styles: ol_style,
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![
        mk_li("one", None),
        mk_li("two", Some("10")),
        mk_li("three", None),
      ],
    };

    let tree = generate_box_tree(&ol);
    let markers: Vec<String> = tree
      .root
      .children
      .iter()
      .filter_map(|li| {
        li.children.first().and_then(|child| match &child.box_type {
          BoxType::Marker(m) => match &m.content {
            MarkerContent::Text(t) => Some(t.clone()),
            MarkerContent::Image(_) => None,
          },
          _ => None,
        })
      })
      .collect();

    assert_eq!(markers, vec!["1 ", "10 ", "11 "]);
  }

  #[test]
  fn reversed_list_value_attribute_counts_down_from_value() {
    let mut ol_style = ComputedStyle::default();
    ol_style.display = Display::Block;
    let ol_style = Arc::new(ol_style);

    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_type = ListStyleType::Decimal;
    let li_style = Arc::new(li_style);

    let ol_dom = dom::DomNode {
      node_type: dom::DomNodeType::Element {
        tag_name: "ol".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("reversed".to_string(), String::new())],
      },
      children: vec![],
    };

    let mk_li = |text: &str, value: Option<&str>| StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: value
            .map(|v| vec![("value".to_string(), v.to_string())])
            .unwrap_or_else(Vec::new),
        },
        children: vec![dom::DomNode {
          node_type: dom::DomNodeType::Text {
            content: text.to_string(),
          },
          children: vec![],
        }],
      },
      styles: li_style.clone(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![StyledNode {
        node_id: 0,
        node: dom::DomNode {
          node_type: dom::DomNodeType::Text {
            content: text.to_string(),
          },
          children: vec![],
        },
        styles: default_style(),
        starting_styles: StartingStyleSet::default(),
        before_styles: None,
        after_styles: None,
        marker_styles: None,
        first_line_styles: None,
        first_letter_styles: None,
        assigned_slot: None,
        slotted_node_ids: Vec::new(),
        children: vec![],
      }],
    };

    let ol = StyledNode {
      node_id: 0,
      node: ol_dom,
      styles: ol_style,
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![
        mk_li("one", None),
        mk_li("two", Some("10")),
        mk_li("three", None),
      ],
    };

    let tree = generate_box_tree(&ol);
    let markers: Vec<String> = tree
      .root
      .children
      .iter()
      .filter_map(|li| {
        li.children.first().and_then(|child| match &child.box_type {
          BoxType::Marker(m) => match &m.content {
            MarkerContent::Text(t) => Some(t.clone()),
            MarkerContent::Image(_) => None,
          },
          _ => None,
        })
      })
      .collect();

    assert_eq!(markers, vec!["3 ", "10 ", "9 "]);
  }

  #[test]
  fn reversed_list_ignores_nested_items_for_default_start() {
    let mut ol_style = ComputedStyle::default();
    ol_style.display = Display::Block;
    let ol_style = Arc::new(ol_style);

    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_type = ListStyleType::Decimal;
    let li_style = Arc::new(li_style);

    let nested_ol = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "ol".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("reversed".to_string(), String::new())],
        },
        children: vec![],
      },
      styles: ol_style.clone(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![StyledNode {
        node_id: 0,
        node: dom::DomNode {
          node_type: dom::DomNodeType::Element {
            tag_name: "li".to_string(),
            namespace: HTML_NAMESPACE.to_string(),
            attributes: vec![],
          },
          children: vec![dom::DomNode {
            node_type: dom::DomNodeType::Text {
              content: "inner".to_string(),
            },
            children: vec![],
          }],
        },
        styles: li_style.clone(),
        starting_styles: StartingStyleSet::default(),
        before_styles: None,
        after_styles: None,
        marker_styles: None,
        first_line_styles: None,
        first_letter_styles: None,
        assigned_slot: None,
        slotted_node_ids: Vec::new(),
        children: vec![StyledNode {
          node_id: 0,
          node: dom::DomNode {
            node_type: dom::DomNodeType::Text {
              content: "inner".to_string(),
            },
            children: vec![],
          },
          styles: default_style(),
          starting_styles: StartingStyleSet::default(),
          before_styles: None,
          after_styles: None,
          marker_styles: None,
          first_line_styles: None,
          first_letter_styles: None,
          assigned_slot: None,
          slotted_node_ids: Vec::new(),
          children: vec![],
        }],
      }],
    };

    let ol_dom = dom::DomNode {
      node_type: dom::DomNodeType::Element {
        tag_name: "ol".to_string(),
        namespace: HTML_NAMESPACE.to_string(),
        attributes: vec![("reversed".to_string(), String::new())],
      },
      children: vec![],
    };

    let outer_li = |child: StyledNode| StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![child.node.clone()],
      },
      styles: li_style.clone(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![child],
    };

    let ol = StyledNode {
      node_id: 0,
      node: ol_dom,
      styles: ol_style.clone(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![
        outer_li(StyledNode {
          node_id: 0,
          node: dom::DomNode {
            node_type: dom::DomNodeType::Text {
              content: "outer1".to_string(),
            },
            children: vec![],
          },
          styles: default_style(),
          starting_styles: StartingStyleSet::default(),
          before_styles: None,
          after_styles: None,
          marker_styles: None,
          first_line_styles: None,
          first_letter_styles: None,
          assigned_slot: None,
          slotted_node_ids: Vec::new(),
          children: vec![],
        }),
        outer_li(nested_ol),
      ],
    };

    let tree = generate_box_tree(&ol);
    let markers: Vec<String> = tree
      .root
      .children
      .iter()
      .filter_map(|li| {
        li.children.first().and_then(|child| match &child.box_type {
          BoxType::Marker(m) => match &m.content {
            MarkerContent::Text(t) => Some(t.clone()),
            MarkerContent::Image(_) => None,
          },
          _ => None,
        })
      })
      .collect();

    assert_eq!(markers, vec!["2 ", "1 "]);
  }

  #[test]
  fn reversed_list_skips_menu_items_for_default_start() {
    let mut ol_style = ComputedStyle::default();
    ol_style.display = Display::Block;
    ol_style.list_style_type = ListStyleType::Decimal;
    let ol_style = Arc::new(ol_style);

    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_type = ListStyleType::Decimal;
    let li_style = Arc::new(li_style);

    let mut menu_style = ComputedStyle::default();
    menu_style.display = Display::Block;
    menu_style.list_style_type = ListStyleType::Disc;
    let menu_style = Arc::new(menu_style);

    let text_node = |content: &str| StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Text {
          content: content.to_string(),
        },
        children: vec![],
      },
      styles: default_style(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    };

    let li = |content: &str| StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      },
      styles: li_style.clone(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![text_node(content)],
    };

    let menu = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "menu".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      },
      styles: menu_style,
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![li("skip-1"), li("skip-2")],
    };

    let ol = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "ol".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("reversed".to_string(), String::new())],
        },
        children: vec![],
      },
      styles: ol_style,
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![li("one"), menu, li("two")],
    };

    let tree = generate_box_tree(&ol);
    let markers: Vec<String> = tree
      .root
      .children
      .iter()
      .filter_map(|child| match &child.box_type {
        BoxType::Block(_) => child.children.first().and_then(|c| match &c.box_type {
          BoxType::Marker(m) => match &m.content {
            MarkerContent::Text(t) => Some(t.clone()),
            MarkerContent::Image(_) => None,
          },
          _ => None,
        }),
        _ => None,
      })
      .collect();

    // Only top-level list items should contribute to the count: markers 2, 1.
    assert_eq!(markers, vec!["2 ", "1 "]);
  }

  #[test]
  fn nested_list_resets_increment_to_default() {
    // Outer reversed list should not force inner list to count down; inner list starts at 1.
    let mut ol_style = ComputedStyle::default();
    ol_style.display = Display::Block;
    ol_style.list_style_type = ListStyleType::Decimal;
    let ol_style = Arc::new(ol_style);

    let mut li_style = ComputedStyle::default();
    li_style.display = Display::ListItem;
    li_style.list_style_type = ListStyleType::Decimal;
    let li_style = Arc::new(li_style);

    let text_node = |content: &str| StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Text {
          content: content.to_string(),
        },
        children: vec![],
      },
      styles: default_style(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![],
    };

    let mk_li = |content: &str| StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      },
      styles: li_style.clone(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![text_node(content)],
    };

    let inner_ol = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "ol".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      },
      styles: ol_style.clone(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![mk_li("inner-one"), mk_li("inner-two")],
    };

    let outer_li = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "li".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![],
        },
        children: vec![],
      },
      styles: li_style.clone(),
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![text_node("outer"), inner_ol],
    };

    let outer_ol = StyledNode {
      node_id: 0,
      node: dom::DomNode {
        node_type: dom::DomNodeType::Element {
          tag_name: "ol".to_string(),
          namespace: HTML_NAMESPACE.to_string(),
          attributes: vec![("reversed".to_string(), String::new())],
        },
        children: vec![],
      },
      styles: ol_style,
      starting_styles: StartingStyleSet::default(),
      before_styles: None,
      after_styles: None,
      marker_styles: None,
      first_line_styles: None,
      first_letter_styles: None,
      assigned_slot: None,
      slotted_node_ids: Vec::new(),
      children: vec![outer_li],
    };

    let tree = generate_box_tree(&outer_ol);
    let inner_box = tree.root.children[0]
      .children
      .iter()
      .find(|child| matches!(child.box_type, BoxType::Block(_)))
      .expect("inner list block");

    let first_inner_marker = match &inner_box.children[0].children[0].box_type {
      BoxType::Marker(marker) => marker.content.clone(),
      other => panic!("expected marker for first inner item, got {:?}", other),
    };
    let second_inner_marker = match &inner_box.children[1].children[0].box_type {
      BoxType::Marker(marker) => marker.content.clone(),
      other => panic!("expected marker for second inner item, got {:?}", other),
    };

    let first_text = match first_inner_marker {
      MarkerContent::Text(t) => t,
      MarkerContent::Image(_) => panic!("expected text marker, got Image"),
    };
    let second_text = match second_inner_marker {
      MarkerContent::Text(t) => t,
      MarkerContent::Image(_) => panic!("expected text marker, got Image"),
    };

    assert_eq!(first_text, "1 ");
    assert_eq!(second_text, "2 ");
  }

  #[test]
  fn inline_svg_carries_serialized_content() {
    let html = r#"<html><body><svg width="10" height="10"><rect width="10" height="10" fill="red"/></svg></body></html>"#;
    let dom = crate::dom::parse_html(html).expect("parse");
    let styled = crate::style::cascade::apply_styles(&dom, &crate::css::types::StyleSheet::new());
    let box_tree = generate_box_tree(&styled);

    fn find_svg(node: &BoxNode) -> Option<&ReplacedBox> {
      if let BoxType::Replaced(repl) = &node.box_type {
        if matches!(repl.replaced_type, ReplacedType::Svg { .. }) {
          return Some(repl);
        }
      }
      for child in node.children.iter() {
        if let Some(found) = find_svg(child) {
          return Some(found);
        }
      }
      None
    }

    let svg = find_svg(&box_tree.root).expect("svg replaced box");
    match &svg.replaced_type {
      ReplacedType::Svg { content } => {
        assert!(
          content.svg.contains("<rect") && content.svg.contains("fill=\"red\""),
          "serialized SVG should include child elements"
        );
      }
      other => panic!("expected svg replaced type, got {:?}", other),
    }
  }
}

fn list_marker_text(list_style: ListStyleType, counters: &CounterManager) -> String {
  let core = match list_style {
    ListStyleType::None => return String::new(),
    ListStyleType::Disc => counters.format("list-item", CounterStyle::Disc),
    ListStyleType::Circle => counters.format("list-item", CounterStyle::Circle),
    ListStyleType::Square => counters.format("list-item", CounterStyle::Square),
    ListStyleType::Decimal => counters.format("list-item", CounterStyle::Decimal),
    ListStyleType::DecimalLeadingZero => {
      counters.format("list-item", CounterStyle::DecimalLeadingZero)
    }
    ListStyleType::LowerRoman => counters.format("list-item", CounterStyle::LowerRoman),
    ListStyleType::UpperRoman => counters.format("list-item", CounterStyle::UpperRoman),
    ListStyleType::LowerAlpha => counters.format("list-item", CounterStyle::LowerAlpha),
    ListStyleType::UpperAlpha => counters.format("list-item", CounterStyle::UpperAlpha),
    ListStyleType::Armenian => counters.format("list-item", CounterStyle::Armenian),
    ListStyleType::LowerArmenian => counters.format("list-item", CounterStyle::LowerArmenian),
    ListStyleType::Georgian => counters.format("list-item", CounterStyle::Georgian),
    ListStyleType::LowerGreek => counters.format("list-item", CounterStyle::LowerGreek),
    ListStyleType::DisclosureOpen => counters.format("list-item", CounterStyle::DisclosureOpen),
    ListStyleType::DisclosureClosed => counters.format("list-item", CounterStyle::DisclosureClosed),
    ListStyleType::String(text) => text,
    ListStyleType::Custom(name) => counters.format("list-item", CounterStyleName::Custom(name)),
  };

  if core.is_empty() {
    core
  } else {
    format!("{} ", core)
  }
}
