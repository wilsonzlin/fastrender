use html5ever::parse_document;
use html5ever::tendril::TendrilSink;
use markup5ever_rcdom::{Handle, NodeData, RcDom};
use crate::error::{Error, Result};
use crate::css::{FastRenderSelectorImpl, PseudoClass, PseudoElement, CssString};
use selectors::{Element, OpaqueElement, attr::{CaseSensitivity, AttrSelectorOperation}};
use std::ptr;

#[derive(Debug, Clone)]
pub struct DomNode {
    pub node_type: DomNodeType,
    pub children: Vec<DomNode>,
}

#[derive(Debug, Clone)]
pub enum DomNodeType {
    Document,
    Element {
        tag_name: String,
        attributes: Vec<(String, String)>,
    },
    Text {
        content: String,
    },
}

pub fn parse_html(html: &str) -> Result<DomNode> {
    let dom = parse_document(RcDom::default(), Default::default())
        .from_utf8()
        .read_from(&mut html.as_bytes())
        .map_err(|e| Error::HtmlParse(format!("Failed to parse HTML: {}", e)))?;

    Ok(convert_handle_to_node(&dom.document))
}

fn convert_handle_to_node(handle: &Handle) -> DomNode {
    let node = handle;

    let node_type = match &node.data {
        NodeData::Document => DomNodeType::Document,
        NodeData::Element { name, attrs, .. } => {
            let tag_name = name.local.to_string();
            let attributes = attrs
                .borrow()
                .iter()
                .map(|attr| {
                    (
                        attr.name.local.to_string(),
                        attr.value.to_string(),
                    )
                })
                .collect();

            DomNodeType::Element {
                tag_name,
                attributes,
            }
        }
        NodeData::Text { contents } => {
            let content = contents.borrow().to_string();
            DomNodeType::Text { content }
        }
        _ => {
            // Skip comments, processing instructions, doctypes
            return DomNode {
                node_type: DomNodeType::Document,
                children: node.children.borrow().iter().map(convert_handle_to_node).collect(),
            };
        }
    };

    let children = node
        .children
        .borrow()
        .iter()
        .map(convert_handle_to_node)
        .collect();

    DomNode {
        node_type,
        children,
    }
}

impl DomNode {
    pub fn get_attribute(&self, name: &str) -> Option<String> {
        match &self.node_type {
            DomNodeType::Element { attributes, .. } => {
                attributes
                    .iter()
                    .find(|(k, _)| k == name)
                    .map(|(_, v)| v.clone())
            }
            _ => None,
        }
    }

    pub fn tag_name(&self) -> Option<&str> {
        match &self.node_type {
            DomNodeType::Element { tag_name, .. } => Some(tag_name),
            _ => None,
        }
    }

    pub fn is_element(&self) -> bool {
        matches!(self.node_type, DomNodeType::Element { .. })
    }

    pub fn is_text(&self) -> bool {
        matches!(self.node_type, DomNodeType::Text { .. })
    }

    pub fn text_content(&self) -> Option<&str> {
        match &self.node_type {
            DomNodeType::Text { content } => Some(content),
            _ => None,
        }
    }

    pub fn walk_tree<F>(&self, f: &mut F)
    where
        F: FnMut(&DomNode),
    {
        f(self);
        for child in &self.children {
            child.walk_tree(f);
        }
    }

    /// Get element children (skip text nodes)
    pub fn element_children(&self) -> Vec<&DomNode> {
        self.children.iter().filter(|c| c.is_element()).collect()
    }

    /// Check if this element has a specific class
    pub fn has_class(&self, class: &str) -> bool {
        if let Some(class_attr) = self.get_attribute("class") {
            class_attr.split_whitespace().any(|c| c == class)
        } else {
            false
        }
    }

    /// Check if this element has a specific ID
    pub fn has_id(&self, id: &str) -> bool {
        self.get_attribute("id").as_deref() == Some(id)
    }
}

/// Wrapper for DomNode that implements Element trait for selector matching
/// This wrapper carries context needed for matching (parent, siblings)
#[derive(Debug, Clone, Copy)]
pub struct ElementRef<'a> {
    pub node: &'a DomNode,
    pub parent: Option<&'a DomNode>,
    all_ancestors: &'a [&'a DomNode],
}

impl<'a> ElementRef<'a> {
    pub fn new(node: &'a DomNode) -> Self {
        Self { node, parent: None, all_ancestors: &[] }
    }

    pub fn with_ancestors(node: &'a DomNode, ancestors: &'a [&'a DomNode]) -> Self {
        let parent = ancestors.last().copied();
        Self {
            node,
            parent,
            all_ancestors: ancestors,
        }
    }

    /// Get parent node
    fn parent_node(&self) -> Option<&'a DomNode> {
        self.parent
    }

    /// Get sibling elements from parent
    fn sibling_elements(&self) -> Vec<&'a DomNode> {
        if let Some(parent) = self.parent_node() {
            parent.element_children()
        } else {
            vec![]
        }
    }

    /// Find index of this element among siblings
    fn element_index(&self) -> Option<usize> {
        let siblings = self.sibling_elements();
        siblings.iter().position(|&sibling| ptr::eq(sibling, self.node))
    }
}

impl<'a> Element for ElementRef<'a> {
    type Impl = FastRenderSelectorImpl;

    fn opaque(&self) -> OpaqueElement {
        OpaqueElement::new(self.node)
    }

    fn parent_element(&self) -> Option<Self> {
        self.parent.map(|parent| {
            // Create ElementRef for parent with its ancestors
            if self.all_ancestors.len() > 1 {
                // If we have multiple ancestors, the parent's ancestors are all but the last
                ElementRef::with_ancestors(parent, &self.all_ancestors[..self.all_ancestors.len() - 1])
            } else {
                // Parent is the root
                ElementRef::new(parent)
            }
        })
    }

    fn parent_node_is_shadow_root(&self) -> bool {
        false // We don't support shadow DOM
    }

    fn containing_shadow_host(&self) -> Option<Self> {
        None
    }

    fn is_pseudo_element(&self) -> bool {
        false
    }

    fn prev_sibling_element(&self) -> Option<Self> {
        let siblings = self.sibling_elements();
        let index = self.element_index()?;
        if index > 0 {
            let prev_node = siblings[index - 1];
            Some(ElementRef {
                node: prev_node,
                parent: self.parent,
                all_ancestors: self.all_ancestors,
            })
        } else {
            None
        }
    }

    fn next_sibling_element(&self) -> Option<Self> {
        let siblings = self.sibling_elements();
        let index = self.element_index()?;
        if index + 1 < siblings.len() {
            let next_node = siblings[index + 1];
            Some(ElementRef {
                node: next_node,
                parent: self.parent,
                all_ancestors: self.all_ancestors,
            })
        } else {
            None
        }
    }

    fn is_html_element_in_html_document(&self) -> bool {
        true // Simplification: assume all HTML
    }

    fn has_local_name(&self, local_name: &str) -> bool {
        self.node
            .tag_name()
            .map(|tag| tag.eq_ignore_ascii_case(local_name))
            .unwrap_or(false)
    }

    fn has_namespace(&self, _ns: &str) -> bool {
        true // Simplification: assume HTML namespace
    }

    fn is_same_type(&self, other: &Self) -> bool {
        self.node.tag_name() == other.node.tag_name()
    }

    fn attr_matches(
        &self,
        _ns: &selectors::attr::NamespaceConstraint<&CssString>,
        local_name: &CssString,
        operation: &AttrSelectorOperation<&CssString>,
    ) -> bool {
        let attr_value = match self.node.get_attribute(local_name.as_str()) {
            Some(v) => v,
            None => return false,
        };

        match operation {
            AttrSelectorOperation::Exists => true,
            AttrSelectorOperation::WithValue {
                operator,
                case_sensitivity,
                value,
            } => {
                let value_str: &str = std::borrow::Borrow::borrow(&**value);
                let matches = match operator {
                    selectors::attr::AttrSelectorOperator::Equal => attr_value == value_str,
                    selectors::attr::AttrSelectorOperator::Includes => {
                        attr_value.split_whitespace().any(|v| v == value_str)
                    }
                    selectors::attr::AttrSelectorOperator::DashMatch => {
                        attr_value == value_str
                            || attr_value.starts_with(&format!("{}-", value_str))
                    }
                    selectors::attr::AttrSelectorOperator::Prefix => {
                        attr_value.starts_with(value_str)
                    }
                    selectors::attr::AttrSelectorOperator::Substring => {
                        attr_value.contains(value_str)
                    }
                    selectors::attr::AttrSelectorOperator::Suffix => {
                        attr_value.ends_with(value_str)
                    }
                };

                matches
            }
        }
    }

    fn match_non_ts_pseudo_class(
        &self,
        pseudo: &PseudoClass,
        _context: &mut selectors::matching::MatchingContext<Self::Impl>,
    ) -> bool {
        match pseudo {
            PseudoClass::Root => {
                matches!(self.node.tag_name(), Some("html"))
            }
            PseudoClass::FirstChild => {
                self.element_index() == Some(0)
            }
            PseudoClass::LastChild => {
                let siblings = self.sibling_elements();
                self.element_index() == Some(siblings.len().saturating_sub(1))
            }
            PseudoClass::OnlyChild => {
                self.sibling_elements().len() == 1
            }
            PseudoClass::NthChild(a, b) => {
                // nth-child formula: an + b
                // Index is 1-based in CSS
                if let Some(index) = self.element_index() {
                    let n = (index + 1) as i32;
                    if *a == 0 {
                        n == *b
                    } else {
                        (n - b) % a == 0 && (n - b) / a >= 0
                    }
                } else {
                    false
                }
            }
            PseudoClass::NthLastChild(a, b) => {
                let siblings = self.sibling_elements();
                if let Some(index) = self.element_index() {
                    let n = (siblings.len() - index) as i32;
                    if *a == 0 {
                        n == *b
                    } else {
                        (n - b) % a == 0 && (n - b) / a >= 0
                    }
                } else {
                    false
                }
            }
            // Interactive pseudo-classes (not supported in static rendering)
            PseudoClass::Hover | PseudoClass::Active | PseudoClass::Focus => false,
            PseudoClass::Link => {
                matches!(self.node.tag_name(), Some("a")) && self.node.get_attribute("href").is_some()
            }
            PseudoClass::Visited => false, // Can't determine visited state
        }
    }

    fn match_pseudo_element(
        &self,
        _pseudo: &PseudoElement,
        _context: &mut selectors::matching::MatchingContext<Self::Impl>,
    ) -> bool {
        false // We don't render pseudo-elements yet
    }

    fn is_link(&self) -> bool {
        matches!(self.node.tag_name(), Some("a")) && self.node.get_attribute("href").is_some()
    }

    fn is_html_slot_element(&self) -> bool {
        false // No shadow DOM support
    }

    fn has_id(&self, id: &CssString, _case_sensitivity: CaseSensitivity) -> bool {
        self.node.has_id(id.as_str())
    }

    fn has_class(&self, class: &CssString, _case_sensitivity: CaseSensitivity) -> bool {
        self.node.has_class(class.as_str())
    }

    fn imported_part(&self, _name: &CssString) -> Option<CssString> {
        None
    }

    fn is_part(&self, _name: &CssString) -> bool {
        false
    }

    fn is_empty(&self) -> bool {
        self.node.children.iter().all(|child| {
            child.is_text() && child.text_content().map(|t| t.trim().is_empty()).unwrap_or(true)
        })
    }

    fn is_root(&self) -> bool {
        matches!(self.node.tag_name(), Some("html"))
    }

    fn first_element_child(&self) -> Option<Self> {
        // We don't support this for now - static rendering doesn't need it
        None
    }

    fn apply_selector_flags(&self, _flags: selectors::matching::ElementSelectorFlags) {
        // We don't track selector flags for static rendering
    }

    fn has_custom_state(&self, _name: &CssString) -> bool {
        // We don't support custom states
        false
    }

    fn add_element_unique_hashes(&self, _filter: &mut selectors::bloom::CountingBloomFilter<selectors::bloom::BloomStorageU8>) -> bool {
        // We don't use bloom filters for optimization
        true
    }
}
