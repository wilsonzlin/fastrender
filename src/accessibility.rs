use crate::dom::{DomNode, DomNodeType, ElementRef};
use crate::style::cascade::StyledNode;
use crate::style::computed::Visibility;
use crate::style::display::Display;
use serde::Serialize;
use std::collections::{HashMap, HashSet};

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

/// Accessibility-related states for a node.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct AccessibilityState {
  pub focusable: bool,
  pub disabled: bool,
  pub required: bool,
  pub invalid: bool,
  pub visited: bool,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub checked: Option<CheckState>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub selected: Option<bool>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub pressed: Option<PressedState>,
}

impl Default for AccessibilityState {
  fn default() -> Self {
    Self {
      focusable: false,
      disabled: false,
      required: false,
      invalid: false,
      visited: false,
      checked: None,
      selected: None,
      pressed: None,
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
  let mut hidden = HashMap::new();
  let mut ids = HashMap::new();
  compute_hidden_and_ids(root, false, &mut hidden, &mut ids);

  let labels = collect_labels(root, &hidden, &ids);

  let ctx = BuildContext {
    root,
    hidden,
    ids,
    labels,
  };

  let mut ancestors: Vec<&DomNode> = Vec::new();
  let mut children = Vec::new();
  for child in &root.children {
    children.extend(build_nodes(child, &ctx, &mut ancestors));
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

struct BuildContext<'a> {
  root: &'a StyledNode,
  hidden: HashMap<usize, bool>,
  ids: HashMap<String, usize>,
  labels: HashMap<usize, Vec<usize>>,
}

impl<'a> BuildContext<'a> {
  fn is_hidden(&self, node: &StyledNode) -> bool {
    *self.hidden.get(&node.node_id).unwrap_or(&false)
  }

  fn node_for_id(&self, id: &str) -> Option<&'a StyledNode> {
    let node_id = self.ids.get(id)?;
    find_node_by_id(self.root, *node_id)
  }

  fn visible_text(&self, node: &StyledNode) -> String {
    if self.is_hidden(node) {
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
        for child in &node.children {
          let text = self.visible_text(child);
          if !text.is_empty() {
            parts.push(text);
          }
        }
        normalize_whitespace(&parts.join(" "))
      }
      DomNodeType::Document | DomNodeType::ShadowRoot { .. } => {
        let mut parts = Vec::new();
        for child in &node.children {
          let text = self.visible_text(child);
          if !text.is_empty() {
            parts.push(text);
          }
        }
        normalize_whitespace(&parts.join(" "))
      }
    }
  }

  fn text_alternative(&self, node: &StyledNode, visited: &mut HashSet<usize>) -> Option<String> {
    if !visited.insert(node.node_id) || self.is_hidden(node) {
      return None;
    }

    if let Some(labelled) = referenced_text_attr(node, self, "aria-labelledby", visited) {
      return Some(labelled);
    }

    if let Some(label) = node.node.get_attribute_ref("aria-label") {
      let norm = normalize_whitespace(label);
      if !norm.is_empty() {
        return Some(norm);
      }
    }

    if let Some(label) = label_association_name(node, self, visited) {
      return Some(label);
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

    let text = self.visible_text(node);
    if text.is_empty() {
      None
    } else {
      Some(text)
    }
  }
}

fn build_nodes<'a>(
  node: &'a StyledNode,
  ctx: &BuildContext<'a>,
  ancestors: &mut Vec<&'a DomNode>,
) -> Vec<AccessibilityNode> {
  if ctx.is_hidden(node) {
    return Vec::new();
  }

  match node.node.node_type {
    DomNodeType::Text { .. } => Vec::new(),
    DomNodeType::Document | DomNodeType::ShadowRoot { .. } => {
      let mut children = Vec::new();
      ancestors.push(&node.node);
      for child in &node.children {
        children.extend(build_nodes(child, ctx, ancestors));
      }
      ancestors.pop();
      children
    }
    DomNodeType::Element { .. } | DomNodeType::Slot { .. } => {
      let mut children = Vec::new();
      ancestors.push(&node.node);
      for child in &node.children {
        children.extend(build_nodes(child, ctx, ancestors));
      }
      ancestors.pop();

      let element_ref = ElementRef::with_ancestors(&node.node, ancestors);
      let mut role = compute_role(&node.node);
      let mut name = compute_name(node, ctx, role.as_deref());
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

      let aria_disabled = parse_bool_attr(&node.node, "aria-disabled");
      let disabled = aria_disabled.unwrap_or_else(|| element_ref.accessibility_disabled());
      let required = parse_bool_attr(&node.node, "aria-required")
        .unwrap_or_else(|| element_ref.accessibility_required());
      let invalid = parse_invalid(&node.node, &element_ref);
      let checked = compute_checked(node, role.as_deref(), &element_ref);
      let selected = compute_selected(node, role.as_deref(), &element_ref);
      let pressed = compute_pressed(node, role.as_deref());
      let visited =
        role.as_deref() == Some("link") && attr_truthy(&node.node, "data-fastr-visited");
      let focusable = compute_focusable(&node.node, role.as_deref(), disabled);
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
        checked,
        selected,
        pressed,
      };

      let should_expose = !decorative_image
        && (role.is_some() || name.is_some() || description.is_some() || value.is_some() || focusable);
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
  hidden: &mut HashMap<usize, bool>,
  ids: &mut HashMap<String, usize>,
) {
  let is_hidden = ancestor_hidden || is_node_hidden(node);
  hidden.insert(node.node_id, is_hidden);

  if !is_hidden {
    if let DomNodeType::Element { .. } = node.node.node_type {
      if let Some(id) = node.node.get_attribute_ref("id") {
        ids.entry(id.to_string()).or_insert(node.node_id);
      }
    }
  }

  for child in &node.children {
    compute_hidden_and_ids(child, is_hidden, hidden, ids);
  }
}

fn collect_labels(
  root: &StyledNode,
  hidden: &HashMap<usize, bool>,
  ids: &HashMap<String, usize>,
) -> HashMap<usize, Vec<usize>> {
  fn is_hidden(node: &StyledNode, hidden: &HashMap<usize, bool>) -> bool {
    *hidden.get(&node.node_id).unwrap_or(&false)
  }

  fn first_labelable_descendant<'a>(
    node: &'a StyledNode,
    hidden: &HashMap<usize, bool>,
  ) -> Option<&'a StyledNode> {
    for child in &node.children {
      if is_hidden(child, hidden) {
        continue;
      }
      if is_labelable(&child.node) {
        return Some(child);
      }
      if let Some(found) = first_labelable_descendant(child, hidden) {
        return Some(found);
      }
    }
    None
  }

  fn walk(
    node: &StyledNode,
    root: &StyledNode,
    hidden: &HashMap<usize, bool>,
    ids: &HashMap<String, usize>,
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
            if let Some(target_node) = find_node_by_id(root, *target_id) {
              if is_labelable(&target_node.node) {
                labels.entry(*target_id).or_default().push(node.node_id);
              }
            }
          }
        }
      } else if let Some(target) = first_labelable_descendant(node, hidden) {
        labels.entry(target.node_id).or_default().push(node.node_id);
      }
    }

    for child in &node.children {
      walk(child, root, hidden, ids, labels);
    }
  }

  let mut labels: HashMap<usize, Vec<usize>> = HashMap::new();
  walk(root, root, hidden, ids, &mut labels);
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

  let role_attr = node
    .node
    .get_attribute_ref("role")
    .map(|r| r.trim().to_ascii_lowercase())
    .filter(|r| !r.is_empty());

  if let Some(role) = role_attr {
    if role != "none" && role != "presentation" {
      return false;
    }
  }

  true
}

fn is_node_hidden(node: &StyledNode) -> bool {
  let attr_hidden = match node.node.node_type {
    DomNodeType::Element { .. } => {
      node.node.get_attribute_ref("hidden").is_some()
        || parse_bool_attr(&node.node, "aria-hidden").unwrap_or(false)
    }
    _ => false,
  };

  attr_hidden
    || node.styles.inert
    || matches!(node.styles.display, Display::None)
    || node.styles.visibility != Visibility::Visible
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

fn find_node_by_id<'a>(root: &'a StyledNode, id: usize) -> Option<&'a StyledNode> {
  if root.node_id == id {
    return Some(root);
  }
  for child in &root.children {
    if let Some(found) = find_node_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

fn compute_role(node: &DomNode) -> Option<String> {
  if let Some(role) = node
    .get_attribute_ref("role")
    .map(|r| r.trim().to_ascii_lowercase())
  {
    if !role.is_empty() {
      return Some(role);
    }
  }

  let tag = node.tag_name()?.to_ascii_lowercase();

  match tag.as_str() {
    "a" => node.get_attribute_ref("href").map(|_| "link".to_string()),
    "area" => node.get_attribute_ref("href").map(|_| "link".to_string()),
    "button" => Some("button".to_string()),
    "summary" => Some("button".to_string()),
    "input" => input_role(node),
    "textarea" => Some("textbox".to_string()),
    "select" => Some(select_role(node)),
    "option" => Some("option".to_string()),
    "img" => Some("img".to_string()),
    "ul" | "ol" => Some("list".to_string()),
    "li" => Some("listitem".to_string()),
    "table" => Some("table".to_string()),
    "thead" | "tbody" | "tfoot" => Some("rowgroup".to_string()),
    "tr" => Some("row".to_string()),
    "td" => Some("cell".to_string()),
    "th" => header_role(node),
    "caption" => Some("caption".to_string()),
    "progress" => Some("progressbar".to_string()),
    "meter" => Some("meter".to_string()),
    "output" => Some("status".to_string()),
    "fieldset" => Some("group".to_string()),
    "main" => Some("main".to_string()),
    "nav" => Some("navigation".to_string()),
    "header" => Some("banner".to_string()),
    "footer" => Some("contentinfo".to_string()),
    "aside" => Some("complementary".to_string()),
    "form" => Some("form".to_string()),
    "article" => Some("article".to_string()),
    "section" => Some("region".to_string()),
    "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => Some("heading".to_string()),
    _ => None,
  }
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

fn compute_name(node: &StyledNode, ctx: &BuildContext, role: Option<&str>) -> Option<String> {
  let mut visited = HashSet::new();
  compute_name_internal(node, ctx, role, &mut visited)
}

fn compute_name_internal(
  node: &StyledNode,
  ctx: &BuildContext,
  role: Option<&str>,
  visited: &mut HashSet<usize>,
) -> Option<String> {
  if !visited.insert(node.node_id) {
    return None;
  }

  if let Some(labelled) = referenced_text_attr(node, ctx, "aria-labelledby", visited) {
    return Some(labelled);
  }

  if let Some(label) = node.node.get_attribute_ref("aria-label") {
    let norm = normalize_whitespace(label);
    if !norm.is_empty() {
      return Some(norm);
    }
  }

  if let Some(label) = label_association_name(node, ctx, visited) {
    return Some(label);
  }

  if let Some(specific) = role_specific_name(node, ctx, role) {
    if !specific.is_empty() {
      return Some(specific);
    }
  }

  if let Some(title) = node.node.get_attribute_ref("title") {
    let norm = normalize_whitespace(title);
    if !norm.is_empty() {
      return Some(norm);
    }
  }

  let is_fieldset = node
    .node
    .tag_name()
    .map(|t| t.eq_ignore_ascii_case("fieldset"))
    .unwrap_or(false);
  if is_fieldset {
    return None;
  }

  let text = ctx.visible_text(node);
  if text.is_empty() {
    None
  } else {
    Some(text)
  }
}

fn referenced_text_attr(
  node: &StyledNode,
  ctx: &BuildContext,
  attr: &str,
  visited: &mut HashSet<usize>,
) -> Option<String> {
  let Some(attr_value) = node.node.get_attribute_ref(attr) else {
    return None;
  };

  let mut parts = Vec::new();
  for id in attr_value.split_whitespace() {
    if let Some(target) = ctx.node_for_id(id) {
      if let Some(text) = ctx.text_alternative(target, visited) {
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
) -> Option<String> {
  if !is_labelable(&node.node) {
    return None;
  }

  let Some(label_ids) = ctx.labels.get(&node.node_id) else {
    return None;
  };

  let mut parts = Vec::new();
  for label_id in label_ids {
    if let Some(label_node) = find_node_by_id(ctx.root, *label_id) {
      if let Some(text) = ctx.text_alternative(label_node, visited) {
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

  if let Some(desc) = referenced_text_attr(node, ctx, "aria-describedby", &mut visited) {
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

  let tag = node.tag_name()?.to_ascii_lowercase();
  match tag.as_str() {
    "h1" => Some(1),
    "h2" => Some(2),
    "h3" => Some(3),
    "h4" => Some(4),
    "h5" => Some(5),
    "h6" => Some(6),
    _ => None,
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

  if matches!(role, Some("checkbox") | Some("radio")) {
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

fn compute_focusable(node: &DomNode, role: Option<&str>, disabled: bool) -> bool {
  if disabled {
    return false;
  }

  if node.get_attribute_ref("tabindex").is_some() {
    return true;
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
