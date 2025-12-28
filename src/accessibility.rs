use crate::dom::{DomNode, DomNodeType, ElementRef};
use crate::style::cascade::StyledNode;
use crate::style::computed::Visibility;
use crate::style::display::Display;
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::ptr;

/// Checked state for toggleable controls.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum CheckState {
  True,
  False,
  Mixed,
}

/// Pressed state for buttons/switches.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum PressedState {
  True,
  False,
  Mixed,
}

/// Current state for landmarks and navigation items.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum AriaCurrent {
  Page,
  Step,
  Location,
  Date,
  Time,
  True,
}

/// Accessibility-related states for a node.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct AccessibilityState {
  pub focusable: bool,
  pub disabled: bool,
  pub required: bool,
  pub invalid: bool,
  pub visited: bool,
  #[serde(skip_serializing_if = "is_false")]
  pub busy: bool,
  pub readonly: bool,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub has_popup: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub checked: Option<CheckState>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub selected: Option<bool>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub pressed: Option<PressedState>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub expanded: Option<bool>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub current: Option<AriaCurrent>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub modal: Option<bool>,
}

impl Default for AccessibilityState {
  fn default() -> Self {
    Self {
      focusable: false,
      disabled: false,
      required: false,
      invalid: false,
      visited: false,
      busy: false,
      readonly: false,
      has_popup: None,
      checked: None,
      selected: None,
      pressed: None,
      expanded: None,
      current: None,
      modal: None,
    }
  }
}

/// A node in the exported accessibility tree.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct AccessibilityNode {
  pub role: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub name: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub description: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub value: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub level: Option<u32>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub html_tag: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub id: Option<String>,
  pub states: AccessibilityState,
  pub children: Vec<AccessibilityNode>,
}

/// Build an accessibility tree from a styled DOM.
pub fn build_accessibility_tree(root: &StyledNode) -> AccessibilityNode {
  let mut lookup = HashMap::new();
  build_styled_lookup(root, &mut lookup);
  let mut hidden = HashMap::new();
  let mut aria_hidden = HashMap::new();
  let mut ids = HashMap::new();
  compute_hidden_and_ids(
    root,
    false,
    false,
    &mut hidden,
    &mut aria_hidden,
    &mut ids,
  );

  let labels = collect_labels(root, &hidden, &ids, &lookup);

  let ctx = BuildContext {
    hidden,
    aria_hidden,
    ids,
    labels,
    lookup,
  };

  let mut ancestors: Vec<&DomNode> = Vec::new();
  let mut styled_ancestors: Vec<&StyledNode> = Vec::new();
  let mut children = Vec::new();
  for child in ctx.composed_children(root) {
    children.extend(build_nodes(
      child,
      &ctx,
      &mut ancestors,
      &mut styled_ancestors,
    ));
  }

  AccessibilityNode {
    role: "document".to_string(),
    name: None,
    description: None,
    value: None,
    level: None,
    html_tag: Some("document".to_string()),
    id: None,
    states: AccessibilityState::default(),
    children,
  }
}

/// Serialize the accessibility tree to JSON for snapshot tests.
pub fn accessibility_tree_json(root: &StyledNode) -> serde_json::Value {
  serde_json::to_value(build_accessibility_tree(root)).unwrap_or(serde_json::Value::Null)
}

fn build_styled_lookup<'a>(node: &'a StyledNode, out: &mut HashMap<usize, &'a StyledNode>) {
  out.insert(node.node_id, node);
  for child in &node.children {
    build_styled_lookup(child, out);
  }
}

fn composed_children<'a>(
  styled: &'a StyledNode,
  lookup: &HashMap<usize, &'a StyledNode>,
) -> Vec<&'a StyledNode> {
  if let Some(shadow_root) = styled
    .children
    .iter()
    .find(|c| matches!(c.node.node_type, DomNodeType::ShadowRoot { .. }))
  {
    return vec![shadow_root];
  }

  if matches!(styled.node.node_type, DomNodeType::Slot { .. })
    && !styled.slotted_node_ids.is_empty()
  {
    let mut resolved: Vec<&'a StyledNode> = Vec::new();
    for id in &styled.slotted_node_ids {
      if let Some(node) = lookup.get(id) {
        resolved.push(*node);
      }
    }
    return resolved;
  }

  styled.children.iter().collect()
}

struct BuildContext<'a> {
  hidden: HashMap<usize, bool>,
  aria_hidden: HashMap<usize, bool>,
  ids: HashMap<String, usize>,
  labels: HashMap<usize, Vec<usize>>,
  lookup: HashMap<usize, &'a StyledNode>,
}

#[derive(Clone, Copy)]
enum TextAlternativeMode {
  /// Only include nodes that are visible in the rendered output.
  Visible,
  /// Include nodes that are hidden by CSS/HTML but not explicitly aria-hidden.
  Referenced,
}

impl<'a> BuildContext<'a> {
  fn is_hidden(&self, node: &StyledNode) -> bool {
    *self.hidden.get(&node.node_id).unwrap_or(&false)
  }

  /// Whether the node or any ancestor is hidden from assistive technology via
  /// `aria-hidden` or `inert`.
  fn is_accessibility_hidden(&self, node: &StyledNode) -> bool {
    *self.aria_hidden.get(&node.node_id).unwrap_or(&false)
  }

  fn is_hidden_for_mode(&self, node: &StyledNode, mode: TextAlternativeMode) -> bool {
    match mode {
      TextAlternativeMode::Visible => self.is_hidden(node),
      TextAlternativeMode::Referenced => self.is_accessibility_hidden(node),
    }
  }

  fn node_for_id(&self, id: &str) -> Option<&'a StyledNode> {
    let node_id = self.ids.get(id)?;
    self.lookup.get(node_id).copied()
  }

  fn node_by_id(&self, id: usize) -> Option<&'a StyledNode> {
    self.lookup.get(&id).copied()
  }

  fn composed_children(&self, node: &'a StyledNode) -> Vec<&'a StyledNode> {
    composed_children(node, &self.lookup)
  }

  fn visible_text(&self, node: &'a StyledNode) -> String {
    self.text_content(node, TextAlternativeMode::Visible)
  }

  fn text_content(&self, node: &'a StyledNode, mode: TextAlternativeMode) -> String {
    if self.is_hidden_for_mode(node, mode) {
      return String::new();
    }

    match &node.node.node_type {
      DomNodeType::Text { content } => normalize_whitespace(content),
      DomNodeType::Element { .. } | DomNodeType::Slot { .. } => {
        let tag = node
          .node
          .tag_name()
          .map(|t| t.to_ascii_lowercase())
          .unwrap_or_default();

        if tag.eq_ignore_ascii_case("script") || tag.eq_ignore_ascii_case("style") {
          return String::new();
        }

        if tag == "img" {
          if let Some(alt) = node.node.get_attribute_ref("alt") {
            let norm = normalize_whitespace(alt);
            if !norm.is_empty() {
              return norm;
            }
          }
        }

        let mut parts = Vec::new();
        for child in self.composed_children(node) {
          let text = self.text_content(child, mode);
          if !text.is_empty() {
            parts.push(text);
          }
        }
        normalize_whitespace(&parts.join(" "))
      }
      DomNodeType::Document | DomNodeType::ShadowRoot { .. } => {
        let mut parts = Vec::new();
        for child in self.composed_children(node) {
          let text = self.text_content(child, mode);
          if !text.is_empty() {
            parts.push(text);
          }
        }
        normalize_whitespace(&parts.join(" "))
      }
    }
  }

  fn text_alternative(
    &self,
    node: &'a StyledNode,
    visited: &mut HashSet<usize>,
    mode: TextAlternativeMode,
  ) -> Option<String> {
    if !visited.insert(node.node_id) || self.is_hidden_for_mode(node, mode) {
      return None;
    }

    if let Some(labelled) = referenced_text_attr(node, self, "aria-labelledby", visited, mode) {
      return Some(labelled);
    }

    if let Some(label) = node.node.get_attribute_ref("aria-label") {
      let norm = normalize_whitespace(label);
      if !norm.is_empty() {
        return Some(norm);
      }
    }

    if let Some(label) = label_association_name(node, self, visited, mode) {
      return Some(label);
    }

    if let Some(native) = native_name_from_html(node, self, visited, mode) {
      if !native.is_empty() {
        return Some(native);
      }
    }

    if let Some(alt) = node.node.get_attribute_ref("alt") {
      let norm = normalize_whitespace(alt);
      if !norm.is_empty() {
        return Some(norm);
      }
    }

    if let Some(title) = node.node.get_attribute_ref("title") {
      let norm = normalize_whitespace(title);
      if !norm.is_empty() {
        return Some(norm);
      }
    }

    let (role, _) = compute_role(node, &[], None);
    if allows_visible_text_name(node.node.tag_name(), role.as_deref(), true) {
      let text = self.text_content(node, mode);
      if !text.is_empty() {
        return Some(text);
      }
    }

    None
  }
}

fn build_nodes<'a>(
  node: &'a StyledNode,
  ctx: &BuildContext<'a>,
  ancestors: &mut Vec<&'a DomNode>,
  styled_ancestors: &mut Vec<&'a StyledNode>,
) -> Vec<AccessibilityNode> {
  if ctx.is_hidden(node) {
    return Vec::new();
  }

  match node.node.node_type {
    DomNodeType::Text { .. } => Vec::new(),
    DomNodeType::Document | DomNodeType::ShadowRoot { .. } => {
      let mut children = Vec::new();
      ancestors.push(&node.node);
      styled_ancestors.push(node);
      for child in ctx.composed_children(node) {
        children.extend(build_nodes(child, ctx, ancestors, styled_ancestors));
      }
      styled_ancestors.pop();
      ancestors.pop();
      children
    }
    DomNodeType::Element { .. } | DomNodeType::Slot { .. } => {
      let mut children = Vec::new();
      ancestors.push(&node.node);
      styled_ancestors.push(node);
      for child in ctx.composed_children(node) {
        children.extend(build_nodes(child, ctx, ancestors, styled_ancestors));
      }
      styled_ancestors.pop();
      ancestors.pop();

      let element_ref = ElementRef::with_ancestors(&node.node, ancestors);
      let (mut role, presentational_role) =
        compute_role(node, ancestors, styled_ancestors.last().copied());
      let mut name = compute_name(node, ctx, role.as_deref(), !presentational_role);
      let mut description = compute_description(node, ctx);
      let decorative_image = is_decorative_img(node, ctx);

      if decorative_image {
        role = None;
        name = None;
        description = None;
      }

      // Regions should expose only when labelled.
      if role.as_deref() == Some("region") && name.is_none() {
        role = None;
      }
      if role.as_deref() == Some("form") && name.is_none() {
        role = None;
      }

      let aria_disabled = parse_bool_attr(&node.node, "aria-disabled");
      let disabled = aria_disabled.unwrap_or_else(|| element_ref.accessibility_disabled());
      let required = parse_bool_attr(&node.node, "aria-required")
        .unwrap_or_else(|| element_ref.accessibility_required());
      let invalid = parse_invalid(&node.node, &element_ref);
      let checked = compute_checked(node, role.as_deref(), &element_ref);
      let selected = compute_selected(node, role.as_deref(), &element_ref);
      let pressed = compute_pressed(node, role.as_deref());
      let busy = attr_truthy(&node.node, "aria-busy");
      let modal = compute_modal(&node.node);
      let current = parse_aria_current(&node.node);
      let expanded = compute_expanded(node, role.as_deref(), ancestors);
      let has_popup = parse_has_popup(&node.node);
      let visited =
        role.as_deref() == Some("link") && attr_truthy(&node.node, "data-fastr-visited");
      let focusable = compute_focusable(&node.node, role.as_deref(), disabled);
      let readonly = compute_readonly(&node.node, role.as_deref(), &element_ref);
      let value = compute_value(node, role.as_deref(), &element_ref, ctx);
      let level = compute_level(&node.node, role.as_deref());

      if name.is_none() {
        name = fallback_name_for_role(role.as_deref(), node, ctx, &element_ref);
      }

      let states = AccessibilityState {
        focusable,
        disabled,
        required,
        invalid,
        visited,
        busy,
        readonly,
        has_popup,
        checked,
        selected,
        pressed,
        expanded,
        current,
        modal,
      };

      let should_expose = !decorative_image
        && (role.is_some()
          || name.is_some()
          || description.is_some()
          || value.is_some()
          || focusable);
      if !should_expose {
        return children;
      }

      let role = role.unwrap_or_else(|| "generic".to_string());
      let html_tag = node.node.tag_name().map(|t| t.to_ascii_lowercase());
      let id = node
        .node
        .get_attribute_ref("id")
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string());

      vec![AccessibilityNode {
        role,
        name,
        description,
        value,
        level,
        html_tag,
        id,
        states,
        children,
      }]
    }
  }
}

fn compute_hidden_and_ids(
  node: &StyledNode,
  ancestor_hidden: bool,
  ancestor_aria_hidden: bool,
  hidden: &mut HashMap<usize, bool>,
  aria_hidden: &mut HashMap<usize, bool>,
  ids: &mut HashMap<String, usize>,
) {
  let is_hidden = ancestor_hidden || is_node_hidden(node);
  let is_aria_hidden = ancestor_aria_hidden || is_node_aria_hidden(node);
  hidden.insert(node.node_id, is_hidden);
  aria_hidden.insert(node.node_id, is_aria_hidden);

  if let DomNodeType::Element { .. } = node.node.node_type {
    if let Some(id) = node.node.get_attribute_ref("id") {
      ids.entry(id.to_string()).or_insert(node.node_id);
    }
  }

  for child in &node.children {
    compute_hidden_and_ids(child, is_hidden, is_aria_hidden, hidden, aria_hidden, ids);
  }
}

fn collect_labels(
  root: &StyledNode,
  hidden: &HashMap<usize, bool>,
  ids: &HashMap<String, usize>,
  lookup: &HashMap<usize, &StyledNode>,
) -> HashMap<usize, Vec<usize>> {
  fn is_hidden(node: &StyledNode, hidden: &HashMap<usize, bool>) -> bool {
    *hidden.get(&node.node_id).unwrap_or(&false)
  }

  fn first_labelable_descendant<'a>(
    node: &'a StyledNode,
    hidden: &HashMap<usize, bool>,
    lookup: &'a HashMap<usize, &'a StyledNode>,
  ) -> Option<&'a StyledNode> {
    for child in composed_children(node, lookup) {
      if is_hidden(child, hidden) {
        continue;
      }
      if is_labelable(&child.node) {
        return Some(child);
      }
      if let Some(found) = first_labelable_descendant(child, hidden, lookup) {
        return Some(found);
      }
    }
    None
  }

  fn walk(
    node: &StyledNode,
    hidden: &HashMap<usize, bool>,
    ids: &HashMap<String, usize>,
    lookup: &HashMap<usize, &StyledNode>,
    labels: &mut HashMap<usize, Vec<usize>>,
  ) {
    if is_hidden(node, hidden) {
      return;
    }

    let is_label = node
      .node
      .tag_name()
      .map(|t| t.eq_ignore_ascii_case("label"))
      .unwrap_or(false);

    if is_label {
      if let Some(for_attr) = node.node.get_attribute_ref("for") {
        let target_key = for_attr.trim();
        if !target_key.is_empty() {
          if let Some(target_id) = ids.get(target_key) {
            if let Some(target_node) = lookup.get(target_id) {
              if is_labelable(&target_node.node) {
                labels.entry(*target_id).or_default().push(node.node_id);
              }
            }
          }
        }
      } else if let Some(target) = first_labelable_descendant(node, hidden, lookup) {
        labels.entry(target.node_id).or_default().push(node.node_id);
      }
    }

    for child in &node.children {
      walk(child, hidden, ids, lookup, labels);
    }
  }

  let mut labels: HashMap<usize, Vec<usize>> = HashMap::new();
  // Labels are collected based on the styled tree (not composed), but labelable descendants within
  // a label follow the composed tree to mirror rendered structure.
  walk(root, hidden, ids, lookup, &mut labels);
  labels
}

fn is_decorative_img(node: &StyledNode, ctx: &BuildContext) -> bool {
  let Some(tag) = node.node.tag_name().map(|t| t.to_ascii_lowercase()) else {
    return false;
  };
  if tag != "img" {
    return false;
  }

  let alt_empty = node
    .node
    .get_attribute_ref("alt")
    .map(normalize_whitespace)
    .is_some_and(|alt| alt.is_empty());
  if !alt_empty {
    return false;
  }

  if node.node.get_attribute_ref("aria-label").is_some()
    || node.node.get_attribute_ref("aria-labelledby").is_some()
  {
    return false;
  }

  if ctx.labels.contains_key(&node.node_id) {
    return false;
  }

  let parsed_role = parse_aria_role_attr(&node.node);

  if let Some(ParsedRole::Explicit(_)) = parsed_role {
    return false;
  }

  if node
    .node
    .get_attribute_ref("title")
    .map(normalize_whitespace)
    .is_some_and(|title| !title.is_empty())
    && parsed_role.is_none()
  {
    return false;
  }

  true
}

fn is_node_hidden(node: &StyledNode) -> bool {
  let attr_hidden = match node.node.node_type {
    DomNodeType::Element { .. } => node.node.get_attribute_ref("hidden").is_some(),
    _ => false,
  };

  attr_hidden
    || is_node_aria_hidden(node)
    || matches!(node.styles.display, Display::None)
    || node.styles.visibility != Visibility::Visible
}

fn is_node_aria_hidden(node: &StyledNode) -> bool {
  parse_bool_attr(&node.node, "aria-hidden").unwrap_or(false) || node.styles.inert
}

fn normalize_whitespace(input: &str) -> String {
  let mut out = String::new();
  let mut last_space = false;
  for ch in input.chars() {
    if ch.is_whitespace() {
      if !last_space {
        out.push(' ');
      }
      last_space = true;
    } else {
      out.push(ch);
      last_space = false;
    }
  }
  out.trim().to_string()
}

fn is_sectioning_ancestor(ancestors: &[&DomNode]) -> bool {
  ancestors.iter().any(|ancestor| {
    matches!(ancestor.node_type, DomNodeType::ShadowRoot { .. })
      || ancestor
        .tag_name()
        .map(|t| {
          matches!(
            t.to_ascii_lowercase().as_str(),
            "article" | "aside" | "main" | "nav" | "section" | "header" | "footer"
          )
        })
        .unwrap_or(false)
  })
}

enum ParsedRole {
  Explicit(String),
  Presentational,
}

fn is_supported_role(role: &str) -> bool {
  matches!(
    role,
    "article"
      | "banner"
      | "button"
      | "caption"
      | "cell"
      | "checkbox"
      | "columnheader"
      | "combobox"
      | "complementary"
      | "contentinfo"
      | "dialog"
      | "figure"
      | "form"
      | "generic"
      | "group"
      | "heading"
      | "img"
      | "link"
      | "list"
      | "listbox"
      | "listitem"
      | "main"
      | "meter"
      | "navigation"
      | "option"
      | "progressbar"
      | "radio"
      | "region"
      | "row"
      | "rowgroup"
      | "rowheader"
      | "searchbox"
      | "slider"
      | "spinbutton"
      | "status"
      | "switch"
      | "table"
      | "textbox"
  )
}

fn parse_aria_role_attr(node: &DomNode) -> Option<ParsedRole> {
  let raw_role = node.get_attribute_ref("role")?;

  for token in raw_role.split_ascii_whitespace() {
    let role = token.to_ascii_lowercase();
    if role == "none" || role == "presentation" {
      return Some(ParsedRole::Presentational);
    }

    if is_supported_role(&role) {
      return Some(ParsedRole::Explicit(role));
    }
  }

  None
}

fn compute_role(
  node: &StyledNode,
  ancestors: &[&DomNode],
  styled_parent: Option<&StyledNode>,
) -> (Option<String>, bool) {
  let dom_node = &node.node;

  if let Some(parsed) = parse_aria_role_attr(dom_node) {
    return match parsed {
      ParsedRole::Explicit(role) => (Some(role), false),
      ParsedRole::Presentational => (None, true),
    };
  }

  let Some(tag) = dom_node.tag_name().map(|t| t.to_ascii_lowercase()) else {
    return (None, false);
  };

  let role = match tag.as_str() {
    "a" => dom_node
      .get_attribute_ref("href")
      .map(|_| "link".to_string()),
    "area" => dom_node
      .get_attribute_ref("href")
      .map(|_| "link".to_string()),
    "button" => Some("button".to_string()),
    "summary" => is_details_summary(node, styled_parent).then(|| "button".to_string()),
    "input" => input_role(dom_node),
    "textarea" => Some("textbox".to_string()),
    "select" => Some(select_role(dom_node)),
    "option" => Some("option".to_string()),
    "img" => Some("img".to_string()),
    "figure" => Some("figure".to_string()),
    "ul" | "ol" => Some("list".to_string()),
    "li" => Some("listitem".to_string()),
    "table" => Some("table".to_string()),
    "thead" | "tbody" | "tfoot" => Some("rowgroup".to_string()),
    "tr" => Some("row".to_string()),
    "td" => Some("cell".to_string()),
    "th" => header_role(dom_node),
    "caption" => Some("caption".to_string()),
    "progress" => Some("progressbar".to_string()),
    "meter" => Some("meter".to_string()),
    "output" => Some("status".to_string()),
    "details" => Some("group".to_string()),
    "fieldset" => Some("group".to_string()),
    "main" => {
      if is_sectioning_ancestor(ancestors) {
        None
      } else {
        Some("main".to_string())
      }
    }
    "nav" => Some("navigation".to_string()),
    "header" => {
      if is_sectioning_ancestor(ancestors) {
        None
      } else {
        Some("banner".to_string())
      }
    }
    "footer" => {
      if is_sectioning_ancestor(ancestors) {
        None
      } else {
        Some("contentinfo".to_string())
      }
    }
    "aside" => Some("complementary".to_string()),
    "form" => Some("form".to_string()),
    "article" => Some("article".to_string()),
    "section" => Some("region".to_string()),
    "dialog" => Some("dialog".to_string()),
    "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => Some("heading".to_string()),
    _ => None,
  };

  (role, false)
}

fn is_details_summary(node: &StyledNode, styled_parent: Option<&StyledNode>) -> bool {
  let Some(parent) = styled_parent else {
    return false;
  };

  if !parent
    .node
    .tag_name()
    .map(|t| t.eq_ignore_ascii_case("details"))
    .unwrap_or(false)
  {
    return false;
  }

  for child in &parent.children {
    if child
      .node
      .tag_name()
      .map(|t| t.eq_ignore_ascii_case("summary"))
      .unwrap_or(false)
    {
      return ptr::eq(child, node);
    }
  }

  false
}

fn input_role(node: &DomNode) -> Option<String> {
  let input_type = node
    .get_attribute_ref("type")
    .map(|t| t.to_ascii_lowercase())
    .unwrap_or_else(|| "text".to_string());

  match input_type.as_str() {
    "hidden" => None,
    "checkbox" => Some("checkbox".to_string()),
    "radio" => Some("radio".to_string()),
    "range" => Some("slider".to_string()),
    "number" => Some("spinbutton".to_string()),
    "search" => Some("searchbox".to_string()),
    "button" | "submit" | "reset" | "image" => Some("button".to_string()),
    _ => Some("textbox".to_string()),
  }
}

fn select_role(node: &DomNode) -> String {
  let multiple = node.get_attribute_ref("multiple").is_some();
  if multiple {
    return "listbox".to_string();
  }

  if let Some(size) = node.get_attribute_ref("size") {
    if let Ok(val) = size.parse::<i32>() {
      if val > 1 {
        return "listbox".to_string();
      }
    }
  }

  "combobox".to_string()
}

fn header_role(node: &DomNode) -> Option<String> {
  if !node
    .tag_name()
    .map(|t| t.eq_ignore_ascii_case("th"))
    .unwrap_or(false)
  {
    return None;
  }
  let scope = node
    .get_attribute_ref("scope")
    .map(|s| s.to_ascii_lowercase())
    .unwrap_or_default();

  if scope == "row" || scope == "rowgroup" {
    Some("rowheader".to_string())
  } else {
    Some("columnheader".to_string())
  }
}

fn compute_name(
  node: &StyledNode,
  ctx: &BuildContext,
  role: Option<&str>,
  allow_visible_text: bool,
) -> Option<String> {
  let mut visited = HashSet::new();
  compute_name_internal(node, ctx, role, allow_visible_text, &mut visited)
}

fn compute_name_internal(
  node: &StyledNode,
  ctx: &BuildContext,
  role: Option<&str>,
  allow_visible_text: bool,
  visited: &mut HashSet<usize>,
) -> Option<String> {
  if !visited.insert(node.node_id) {
    return None;
  }

  if let Some(labelled) =
    referenced_text_attr(node, ctx, "aria-labelledby", visited, TextAlternativeMode::Referenced)
  {
    return Some(labelled);
  }

  if let Some(label) = node.node.get_attribute_ref("aria-label") {
    let norm = normalize_whitespace(label);
    if !norm.is_empty() {
      return Some(norm);
    }
  }

  if let Some(label) = label_association_name(
    node,
    ctx,
    visited,
    TextAlternativeMode::Visible,
  ) {
    return Some(label);
  }

  if let Some(placeholder) = placeholder_as_name(node) {
    return Some(placeholder);
  }

  if allow_visible_text {
    if let Some(native) =
      native_name_from_html(node, ctx, visited, TextAlternativeMode::Visible)
    {
      if !native.is_empty() {
        return Some(native);
      }
    }

    if let Some(specific) = role_specific_name(node, ctx, role) {
      if !specific.is_empty() {
        return Some(specific);
      }
    }
  }

  if let Some(title) = node.node.get_attribute_ref("title") {
    let norm = normalize_whitespace(title);
    if !norm.is_empty() {
      return Some(norm);
    }
  }

  if allow_visible_text && allows_visible_text_name(node.node.tag_name(), role, false) {
    let text = ctx.visible_text(node);
    if !text.is_empty() {
      return Some(text);
    }
  }

  None
}

fn native_name_from_html<'a>(
  node: &'a StyledNode,
  ctx: &BuildContext<'a>,
  visited: &mut HashSet<usize>,
  mode: TextAlternativeMode,
) -> Option<String> {
  let tag = node
    .node
    .tag_name()
    .map(|t| t.to_ascii_lowercase())?;

  match tag.as_str() {
    "fieldset" => first_child_with_tag(node, ctx, "legend", false, mode)
      .and_then(|legend| ctx.text_alternative(legend, visited, mode)),
    "figure" => first_child_with_tag(node, ctx, "figcaption", true, mode)
      .and_then(|caption| ctx.text_alternative(caption, visited, mode)),
    "table" => first_child_with_tag(node, ctx, "caption", true, mode)
      .and_then(|caption| ctx.text_alternative(caption, visited, mode)),
    _ => None,
  }
}

fn first_child_with_tag<'a>(
  node: &'a StyledNode,
  ctx: &BuildContext<'a>,
  tag: &str,
  require_visible: bool,
  mode: TextAlternativeMode,
) -> Option<&'a StyledNode> {
  for child in ctx.composed_children(node) {
    if require_visible && ctx.is_hidden_for_mode(child, mode) {
      continue;
    }

    let is_match = child
      .node
      .tag_name()
      .map(|t| t.eq_ignore_ascii_case(tag))
      .unwrap_or(false);
    if is_match {
      return Some(child);
    }

  }

  None
}

fn allows_visible_text_name(tag: Option<&str>, role: Option<&str>, allow_legend: bool) -> bool {
  let tag_blocked = tag
    .map(|t| {
      let lower = t.to_ascii_lowercase();
      matches!(lower.as_str(), "fieldset" | "figure" | "table" | "dialog" | "select")
        || (!allow_legend && lower == "legend")
    })
    .unwrap_or(false);

  if tag_blocked {
    return false;
  }

  if let Some(role) = role {
    if role.eq_ignore_ascii_case("dialog")
      || role.eq_ignore_ascii_case("table")
      || role.eq_ignore_ascii_case("figure")
      || role.eq_ignore_ascii_case("combobox")
      || role.eq_ignore_ascii_case("listbox")
    {
      return false;
    }
  }

  true
}

/// Use placeholder text as a fallback accessible name for text-entry controls.
///
/// HTML-AAM and major browsers expose the `placeholder` attribute as the name
/// when an `<input>`/`<textarea>` has no explicit label. We mirror that behavior
/// (after ARIA and `<label>` sources) so unlabeled text fields remain
/// discoverable to assistive tech.
fn placeholder_as_name(node: &StyledNode) -> Option<String> {
  let tag = node.node.tag_name()?.to_ascii_lowercase();
  let is_textbox_like = match tag.as_str() {
    "textarea" => true,
    "input" => {
      let mut input_type = node
        .node
        .get_attribute_ref("type")
        .map(|t| t.to_ascii_lowercase())
        .unwrap_or_else(|| "text".to_string());
      if input_type.trim().is_empty() {
        input_type = "text".to_string();
      }
      matches!(
        input_type.as_str(),
        "text" | "search" | "email" | "tel" | "url" | "password" | "number"
      )
    }
    _ => false,
  };

  if !is_textbox_like {
    return None;
  }

  let placeholder = node.node.get_attribute_ref("placeholder")?;
  let norm = normalize_whitespace(placeholder);
  if norm.is_empty() {
    None
  } else {
    Some(norm)
  }
}

fn referenced_text_attr(
  node: &StyledNode,
  ctx: &BuildContext,
  attr: &str,
  visited: &mut HashSet<usize>,
  mode: TextAlternativeMode,
) -> Option<String> {
  let Some(attr_value) = node.node.get_attribute_ref(attr) else {
    return None;
  };

  let mut parts = Vec::new();
  for id in attr_value.split_whitespace() {
    if let Some(target) = ctx.node_for_id(id) {
      if let Some(text) = ctx.text_alternative(target, visited, mode) {
        if !text.is_empty() {
          parts.push(text);
        }
      }
    }
  }

  if parts.is_empty() {
    None
  } else {
    Some(normalize_whitespace(&parts.join(" ")))
  }
}

fn label_association_name(
  node: &StyledNode,
  ctx: &BuildContext,
  visited: &mut HashSet<usize>,
  mode: TextAlternativeMode,
) -> Option<String> {
  if !is_labelable(&node.node) {
    return None;
  }

  let Some(label_ids) = ctx.labels.get(&node.node_id) else {
    return None;
  };

  let mut parts = Vec::new();
  for label_id in label_ids {
    if let Some(label_node) = ctx.node_by_id(*label_id) {
      if let Some(text) = ctx.text_alternative(label_node, visited, mode) {
        if !text.is_empty() {
          parts.push(text);
        }
      }
    }
  }

  if parts.is_empty() {
    None
  } else {
    Some(normalize_whitespace(&parts.join(" ")))
  }
}

fn first_visible_child_text(
  node: &StyledNode,
  ctx: &BuildContext,
  tag_name: &str,
) -> Option<String> {
  for child in &node.children {
    if child
      .node
      .tag_name()
      .map(|t| t.eq_ignore_ascii_case(tag_name))
      .unwrap_or(false)
    {
      let text = ctx.visible_text(child);
      if !text.is_empty() {
        return Some(text);
      }
    }
  }

  None
}

fn role_specific_name(node: &StyledNode, ctx: &BuildContext, role: Option<&str>) -> Option<String> {
  let tag = node
    .node
    .tag_name()
    .map(|t| t.to_ascii_lowercase())?
    .to_string();

  match tag.as_str() {
    "img" => node
      .node
      .get_attribute_ref("alt")
      .map(|alt| normalize_whitespace(alt)),
    "input" => {
      let input_type = node
        .node
        .get_attribute_ref("type")
        .map(|t| t.to_ascii_lowercase())
        .unwrap_or_else(|| "text".to_string());

      if matches!(input_type.as_str(), "button" | "submit" | "reset") {
        return node
          .node
          .get_attribute_ref("value")
          .map(normalize_whitespace)
          .or_else(|| default_button_label(&input_type).map(|s| s.to_string()));
      }

      if input_type == "image" {
        return node
          .node
          .get_attribute_ref("alt")
          .map(normalize_whitespace)
          .or_else(|| {
            node
              .node
              .get_attribute_ref("value")
              .map(normalize_whitespace)
          });
      }

      None
    }
    "button" => {
      let text = ctx.visible_text(node);
      if !text.is_empty() {
        Some(text)
      } else {
        node
          .node
          .get_attribute_ref("value")
          .map(normalize_whitespace)
      }
    }
    "option" => {
      let text = ctx.visible_text(node);
      if text.is_empty() {
        None
      } else {
        Some(text)
      }
    }
    "fieldset" => {
      let legend = node.children.iter().find(|child| {
        child
          .node
          .tag_name()
          .map(|t| t.eq_ignore_ascii_case("legend"))
          .unwrap_or(false)
      });

      match legend {
        Some(legend_node) => {
          if ctx.is_hidden(legend_node) {
            None
          } else {
            let text = ctx.visible_text(legend_node);
            if text.is_empty() {
              None
            } else {
              Some(text)
            }
          }
        }
        None => None,
      }
    }
    "table" => first_visible_child_text(node, ctx, "caption"),
    "figure" => first_visible_child_text(node, ctx, "figcaption"),
    _ => {
      if role == Some("heading") {
        let text = ctx.visible_text(node);
        if !text.is_empty() {
          return Some(text);
        }
      }
      None
    }
  }
}

fn fallback_name_for_role(
  role: Option<&str>,
  node: &StyledNode,
  ctx: &BuildContext,
  element_ref: &ElementRef,
) -> Option<String> {
  match role {
    Some("textbox") | Some("searchbox") | Some("combobox") | Some("listbox")
    | Some("spinbutton") | Some("slider") => element_ref
      .accessibility_value()
      .filter(|v| !v.is_empty())
      .map(|v| normalize_whitespace(&v)),
    Some("option") => {
      let text = ctx.visible_text(node);
      if text.is_empty() {
        None
      } else {
        Some(text)
      }
    }
    _ => None,
  }
}

fn default_button_label(input_type: &str) -> Option<&'static str> {
  match input_type {
    "submit" => Some("Submit"),
    "reset" => Some("Reset"),
    "button" => Some("Button"),
    _ => None,
  }
}

fn compute_description(node: &StyledNode, ctx: &BuildContext) -> Option<String> {
  let mut parts = Vec::new();
  let mut visited = HashSet::new();

  if let Some(desc) = referenced_text_attr(
    node,
    ctx,
    "aria-describedby",
    &mut visited,
    TextAlternativeMode::Referenced,
  ) {
    parts.push(desc);
  }

  if let Some(description) = node.node.get_attribute_ref("aria-description") {
    let norm = normalize_whitespace(description);
    if !norm.is_empty() {
      parts.push(norm);
    }
  }

  if parts.is_empty() {
    None
  } else {
    Some(normalize_whitespace(&parts.join(" ")))
  }
}

fn compute_level(node: &DomNode, role: Option<&str>) -> Option<u32> {
  if !matches!(role, Some("heading")) {
    return None;
  }

  if let Some(attr) = node.get_attribute_ref("aria-level") {
    if let Ok(level) = attr.trim().parse::<u32>() {
      return Some(level);
    }
  }

  let tag = node.tag_name().map(|t| t.to_ascii_lowercase());
  match tag.as_deref() {
    Some("h1") => Some(1),
    Some("h2") => Some(2),
    Some("h3") => Some(3),
    Some("h4") => Some(4),
    Some("h5") => Some(5),
    Some("h6") => Some(6),
    _ => Some(2),
  }
}

fn compute_value(
  node: &StyledNode,
  role: Option<&str>,
  element_ref: &ElementRef,
  ctx: &BuildContext,
) -> Option<String> {
  match role {
    Some("textbox") | Some("searchbox") | Some("combobox") | Some("listbox") => element_ref
      .accessibility_value()
      .filter(|v| !v.is_empty())
      .map(|v| normalize_whitespace(&v)),
    Some("spinbutton") | Some("slider") => {
      if let Some(value) = aria_value_attr(&node.node) {
        return Some(value);
      }

      // Fall back to the resolved control value (e.g., range inputs expose their sanitized slider
      // position even when no explicit value is authored).
      element_ref
        .accessibility_value()
        .filter(|v| !v.is_empty())
        .map(|v| normalize_whitespace(&v))
    }
    Some("progressbar") => {
      if let Some(value) = aria_value_attr(&node.node) {
        return Some(value);
      }

      progress_value(&node.node)
    }
    Some("meter") => {
      if let Some(value) = aria_value_attr(&node.node) {
        return Some(value);
      }

      meter_value(&node.node)
    }
    Some("option") => {
      let text = ctx.visible_text(node);
      if text.is_empty() {
        None
      } else {
        Some(text)
      }
    }
    _ => None,
  }
}

fn aria_value_attr(node: &DomNode) -> Option<String> {
  if let Some(text) = node.get_attribute_ref("aria-valuetext") {
    let norm = normalize_whitespace(text);
    if !norm.is_empty() {
      return Some(norm);
    }
  }

  if let Some(text) = node.get_attribute_ref("aria-valuenow") {
    let norm = normalize_whitespace(text);
    if !norm.is_empty() {
      return Some(norm);
    }
  }

  None
}

fn format_number(mut value: f64) -> String {
  if value == -0.0 {
    value = 0.0;
  }
  let mut s = value.to_string();
  if s.contains('.') {
    while s.ends_with('0') {
      s.pop();
    }
    if s.ends_with('.') {
      s.pop();
    }
  }
  s
}

fn progress_value(node: &DomNode) -> Option<String> {
  let raw_value = node.get_attribute_ref("value")?;
  let parsed = raw_value.trim().parse::<f64>().ok()?;
  Some(format_number(parsed))
}

fn meter_value(node: &DomNode) -> Option<String> {
  let raw_value = node.get_attribute_ref("value")?;
  let parsed = raw_value.trim().parse::<f64>().ok()?;
  Some(format_number(parsed))
}

fn compute_invalid(node: &DomNode, element_ref: &ElementRef) -> bool {
  if let Some(value) = parse_aria_invalid(node) {
    return value;
  }

  element_ref.accessibility_supports_validation() && !element_ref.accessibility_is_valid()
}

fn compute_checked(
  node: &StyledNode,
  role: Option<&str>,
  element_ref: &ElementRef,
) -> Option<CheckState> {
  if let Some(state) = parse_check_state(&node.node, "aria-checked") {
    return Some(state);
  }

  if matches!(role, Some("checkbox") | Some("radio") | Some("switch")) {
    if element_ref.accessibility_indeterminate() {
      return Some(CheckState::Mixed);
    }
    if element_ref.accessibility_checked() {
      return Some(CheckState::True);
    }
    return Some(CheckState::False);
  }

  None
}

fn compute_selected(
  node: &StyledNode,
  role: Option<&str>,
  element_ref: &ElementRef,
) -> Option<bool> {
  if let Some(selected) = parse_bool_attr(&node.node, "aria-selected") {
    return Some(selected);
  }

  if role == Some("option") {
    return Some(element_ref.accessibility_selected());
  }

  None
}

fn compute_pressed(node: &StyledNode, role: Option<&str>) -> Option<PressedState> {
  if let Some(state) = parse_pressed_state(&node.node, "aria-pressed") {
    return Some(state);
  }

  if role == Some("button") && attr_truthy(&node.node, "data-fastr-active") {
    return Some(PressedState::True);
  }

  None
}

fn compute_modal(node: &DomNode) -> Option<bool> {
  if let Some(value) = parse_bool_attr(node, "aria-modal") {
    return Some(value);
  }

  let is_dialog = match node.tag_name() {
    Some(tag) => tag.eq_ignore_ascii_case("dialog"),
    None => false,
  };

  if is_dialog && attr_truthy(node, "data-fastr-modal") {
    return Some(true);
  }

  None
}

fn compute_readonly(node: &DomNode, _role: Option<&str>, element_ref: &ElementRef) -> bool {
  if let Some(value) = parse_bool_attr(node, "aria-readonly") {
    return value;
  }

  element_ref.accessibility_readonly()
}

fn compute_expanded(node: &StyledNode, role: Option<&str>, ancestors: &[&DomNode]) -> Option<bool> {
  if let Some(expanded) = parse_expanded(&node.node) {
    return Some(expanded);
  }

  let is_summary = node
    .node
    .tag_name()
    .map(|t| t.eq_ignore_ascii_case("summary"))
    .unwrap_or(false);

  if role == Some("button") && is_summary {
    if let Some(parent) = ancestors.last() {
      if parent
        .tag_name()
        .map(|t| t.eq_ignore_ascii_case("details"))
        .unwrap_or(false)
      {
        if let Some(expanded) = parse_expanded(parent) {
          return Some(expanded);
        }
        return Some(parent.get_attribute_ref("open").is_some());
      }
    }
  }

  if role == Some("combobox")
    && node
      .node
      .get_attribute_ref("data-fastr-open")
      .is_some()
  {
    return Some(attr_truthy(&node.node, "data-fastr-open"));
  }

  None
}

fn compute_focusable(node: &DomNode, role: Option<&str>, disabled: bool) -> bool {
  if disabled {
    return false;
  }

  if let Some(tabindex) = node.get_attribute_ref("tabindex") {
    let trimmed = tabindex.trim();
    if !trimmed.is_empty() && trimmed.parse::<i32>().is_ok() {
      return true;
    }
  }

  let tag = match node.tag_name() {
    Some(t) => t.to_ascii_lowercase(),
    None => return false,
  };

  if tag == "a" && node.get_attribute_ref("href").is_some() {
    return true;
  }

  if matches!(tag.as_str(), "button" | "select" | "textarea") {
    return true;
  }

  if tag == "input" {
    let input_type = node
      .get_attribute_ref("type")
      .map(|t| t.to_ascii_lowercase())
      .unwrap_or_else(|| "text".to_string());
    return input_type != "hidden";
  }

  if tag == "option" {
    return true;
  }

  if let Some(r) = role {
    if matches!(r, "button" | "link" | "checkbox" | "radio" | "switch") {
      return true;
    }
  }

  node
    .get_attribute_ref("contenteditable")
    .map(|v| v.is_empty() || v.eq_ignore_ascii_case("true"))
    .unwrap_or(false)
}

fn parse_invalid(node: &DomNode, element_ref: &ElementRef) -> bool {
  compute_invalid(node, element_ref)
}

fn parse_expanded(node: &DomNode) -> Option<bool> {
  let value = node.get_attribute_ref("aria-expanded")?;
  let lower = value.trim().to_ascii_lowercase();
  if lower.is_empty() {
    return Some(true);
  }
  match lower.as_str() {
    "true" | "1" => Some(true),
    "false" | "0" => Some(false),
    _ => None,
  }
}

fn parse_has_popup(node: &DomNode) -> Option<String> {
  let value = node.get_attribute_ref("aria-haspopup")?;
  let trimmed = value.trim();
  if trimmed.is_empty() {
    return Some("true".to_string());
  }

  let lower = trimmed.to_ascii_lowercase();
  if lower == "false" || lower == "0" {
    return None;
  }

  Some(lower)
}

fn parse_aria_invalid(node: &DomNode) -> Option<bool> {
  let value = node.get_attribute_ref("aria-invalid")?;
  let token = value.trim().to_ascii_lowercase();

  if matches!(token.as_str(), "grammar" | "spelling") {
    return Some(true);
  }

  parse_bool_token(&token)
}

fn parse_bool_attr(node: &DomNode, name: &str) -> Option<bool> {
  let value = node.get_attribute_ref(name)?;
  let token = value.trim().to_ascii_lowercase();
  parse_bool_token(&token)
}

fn parse_bool_token(token: &str) -> Option<bool> {
  match token {
    "" => Some(true),
    "true" | "1" | "yes" => Some(true),
    "false" | "0" | "no" => Some(false),
    _ => None,
  }
}

fn attr_truthy(node: &DomNode, name: &str) -> bool {
  parse_bool_attr(node, name).unwrap_or(false)
}

fn parse_check_state(node: &DomNode, name: &str) -> Option<CheckState> {
  let value = node.get_attribute_ref(name)?;
  match value.to_ascii_lowercase().trim() {
    "true" | "1" => Some(CheckState::True),
    "false" | "0" => Some(CheckState::False),
    "mixed" => Some(CheckState::Mixed),
    _ => None,
  }
}

fn parse_pressed_state(node: &DomNode, name: &str) -> Option<PressedState> {
  let value = node.get_attribute_ref(name)?;
  match value.to_ascii_lowercase().trim() {
    "true" | "1" => Some(PressedState::True),
    "false" | "0" => Some(PressedState::False),
    "mixed" => Some(PressedState::Mixed),
    _ => None,
  }
}

fn parse_aria_current(node: &DomNode) -> Option<AriaCurrent> {
  let value = node.get_attribute_ref("aria-current")?;
  let token = value.trim().to_ascii_lowercase();

  if token.is_empty() || token == "false" {
    return None;
  }

  match token.as_str() {
    "page" => Some(AriaCurrent::Page),
    "step" => Some(AriaCurrent::Step),
    "location" => Some(AriaCurrent::Location),
    "date" => Some(AriaCurrent::Date),
    "time" => Some(AriaCurrent::Time),
    "true" => Some(AriaCurrent::True),
    _ => None,
  }
}

fn is_false(value: &bool) -> bool {
  !*value
}

fn is_labelable(node: &DomNode) -> bool {
  let Some(tag) = node.tag_name() else {
    return false;
  };

  match tag.to_ascii_lowercase().as_str() {
    "button" | "select" | "textarea" | "output" | "progress" | "meter" => true,
    "input" => {
      let input_type = node
        .get_attribute_ref("type")
        .map(|t| t.to_ascii_lowercase())
        .unwrap_or_else(|| "text".to_string());
      input_type != "hidden"
    }
    _ => false,
  }
}
