use crate::css::selectors::{FastRenderSelectorImpl, PseudoClass, PseudoElement, TextDirection};
use crate::css::types::CssString;
use crate::error::{Error, ParseError, Result};
use html5ever::parse_document;
use html5ever::tendril::TendrilSink;
use markup5ever_rcdom::{Handle, NodeData, RcDom};
use selectors::{
    attr::{AttrSelectorOperation, CaseSensitivity},
    Element, OpaqueElement,
};
use std::cell::RefCell;
use std::ptr;
use std::thread_local;
use unicode_bidi::bidi_class;

const HTML_NAMESPACE: &str = "http://www.w3.org/1999/xhtml";

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

thread_local! {
    static TARGET_FRAGMENT: RefCell<Option<String>> = RefCell::new(None);
}

pub(crate) fn with_target_fragment<R, F: FnOnce() -> R>(target: Option<&str>, f: F) -> R {
    TARGET_FRAGMENT.with(|slot| {
        let previous = slot.borrow_mut().take();
        if let Some(t) = target {
            *slot.borrow_mut() = Some(t.trim_start_matches('#').to_string());
        }
        let result = f();
        *slot.borrow_mut() = previous;
        result
    })
}

fn current_target_fragment() -> Option<String> {
    TARGET_FRAGMENT.with(|slot| slot.borrow().clone())
}

/// Resolve the first-strong direction within this subtree, skipping script/style contents.
pub fn resolve_first_strong_direction(node: &DomNode) -> Option<TextDirection> {
    let mut stack = vec![node];
    while let Some(current) = stack.pop() {
        match &current.node_type {
            DomNodeType::Text { content } => {
                for ch in content.chars() {
                    match bidi_class(ch) {
                        unicode_bidi::BidiClass::L => return Some(TextDirection::Ltr),
                        unicode_bidi::BidiClass::R | unicode_bidi::BidiClass::AL => return Some(TextDirection::Rtl),
                        _ => {}
                    }
                }
            }
            DomNodeType::Element { tag_name, .. } => {
                let skip = tag_name.eq_ignore_ascii_case("script") || tag_name.eq_ignore_ascii_case("style");
                if skip {
                    continue;
                }
                for child in &current.children {
                    stack.push(child);
                }
            }
            DomNodeType::Document => {
                for child in &current.children {
                    stack.push(child);
                }
            }
        }
    }
    None
}

pub fn parse_html(html: &str) -> Result<DomNode> {
    let dom = parse_document(RcDom::default(), Default::default())
        .from_utf8()
        .read_from(&mut html.as_bytes())
        .map_err(|e| {
            Error::Parse(ParseError::InvalidHtml {
                message: format!("Failed to parse HTML: {}", e),
                line: 0,
            })
        })?;

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
                .map(|attr| (attr.name.local.to_string(), attr.value.to_string()))
                .collect();

            DomNodeType::Element { tag_name, attributes }
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

    let mut children = node
        .children
        .borrow()
        .iter()
        .map(convert_handle_to_node)
        .collect::<Vec<_>>();

    // HTML <wbr> elements represent optional break opportunities. Synthesize a zero-width break
    // text node so line breaking can consider the opportunity while still allowing the element to
    // be styled/selected.
    if let DomNodeType::Element { tag_name, .. } = &node_type {
        if tag_name.eq_ignore_ascii_case("wbr") {
            children.push(DomNode {
                node_type: DomNodeType::Text {
                    content: "\u{200B}".to_string(),
                },
                children: Vec::new(),
            });
        }
    }

    DomNode { node_type, children }
}

impl DomNode {
    pub fn get_attribute(&self, name: &str) -> Option<String> {
        match &self.node_type {
            DomNodeType::Element { attributes, .. } => {
                attributes.iter().find(|(k, _)| k == name).map(|(_, v)| v.clone())
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

    pub fn attributes_iter(&self) -> Box<dyn Iterator<Item = (&str, &str)> + '_> {
        match &self.node_type {
            DomNodeType::Element { attributes, .. } => {
                Box::new(attributes.iter().map(|(k, v)| (k.as_str(), v.as_str())))
            }
            _ => Box::new(std::iter::empty()),
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
        Self {
            node,
            parent: None,
            all_ancestors: &[],
        }
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

    /// Position (index, total) among siblings filtered by a predicate.
    fn position_in_siblings<F>(&self, predicate: F) -> Option<(usize, usize)>
    where
        F: Fn(&DomNode) -> bool,
    {
        let siblings: Vec<_> = self.sibling_elements().into_iter().filter(|s| predicate(s)).collect();
        let index = siblings.iter().position(|&sibling| ptr::eq(sibling, self.node))?;
        Some((index, siblings.len()))
    }

    /// Position among siblings of the same element type (case-insensitive).
    fn position_in_type(&self) -> Option<(usize, usize)> {
        let tag = self.node.tag_name()?;
        self.position_in_siblings(|sibling| {
            sibling
                .tag_name()
                .map(|name| name.eq_ignore_ascii_case(tag))
                .unwrap_or(false)
        })
    }

    /// Return the language of this element, inherited from ancestors if absent.
    fn language(&self) -> Option<String> {
        // Walk from self up through ancestors (closest first) for lang/xml:lang
        if let Some(lang) = self.lang_attribute(self.node) {
            return Some(lang);
        }

        for ancestor in self.all_ancestors.iter().rev() {
            if let Some(lang) = self.lang_attribute(ancestor) {
                return Some(lang);
            }
        }
        None
    }

    fn lang_attribute(&self, node: &DomNode) -> Option<String> {
        node.get_attribute("lang")
            .or_else(|| node.get_attribute("xml:lang"))
            .map(|l| l.to_ascii_lowercase())
    }

    /// Direction from dir/xml:dir attributes, inherited; defaults to LTR when none found.
    fn direction(&self) -> TextDirection {
        if let Some(dir) = self.dir_attribute(self.node, self.node) {
            return dir;
        }
        for ancestor in self.all_ancestors.iter().rev() {
            if let Some(dir) = self.dir_attribute(ancestor, ancestor) {
                return dir;
            }
        }
        TextDirection::Ltr
    }

    fn dir_attribute(&self, node: &DomNode, resolve_root: &DomNode) -> Option<TextDirection> {
        node.get_attribute("dir")
            .or_else(|| node.get_attribute("xml:dir"))
            .and_then(|d| match d.to_ascii_lowercase().as_str() {
                "ltr" => Some(TextDirection::Ltr),
                "rtl" => Some(TextDirection::Rtl),
                "auto" => resolve_first_strong_direction(resolve_root),
                _ => None,
            })
    }

    fn is_target(&self) -> bool {
        let Some(target) = current_target_fragment() else {
            return false;
        };
        if self.node.get_attribute("id").as_deref() == Some(target.as_str()) {
            return true;
        }
        if let Some(tag) = self.node.tag_name() {
            if matches!(tag.to_ascii_lowercase().as_str(), "a" | "area") {
                if self.node.get_attribute("name").as_deref() == Some(target.as_str()) {
                    return true;
                }
            }
        }
        false
    }
}

fn matches_an_plus_b(a: i32, b: i32, position: i32) -> bool {
    if a == 0 {
        position == b
    } else {
        let diff = position - b;
        diff % a == 0 && diff / a >= 0
    }
}

fn lang_matches(range: &str, lang: &str) -> bool {
    if range == "*" {
        return true;
    }
    if range.eq_ignore_ascii_case(lang) {
        return true;
    }
    // Prefix match with boundary
    lang.starts_with(range) && lang.as_bytes().get(range.len()) == Some(&b'-')
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

    fn has_namespace(&self, ns: &str) -> bool {
        if self.node.tag_name().is_none() {
            return false;
        }

        ns.is_empty() || ns == HTML_NAMESPACE
    }

    fn is_same_type(&self, other: &Self) -> bool {
        match (self.node.tag_name(), other.node.tag_name()) {
            (Some(a), Some(b)) => a.eq_ignore_ascii_case(b),
            _ => false,
        }
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
                case_sensitivity: _,
                value,
            } => {
                let value_str: &str = std::borrow::Borrow::borrow(&**value);
                let matches = match operator {
                    selectors::attr::AttrSelectorOperator::Equal => attr_value == value_str,
                    selectors::attr::AttrSelectorOperator::Includes => {
                        attr_value.split_whitespace().any(|v| v == value_str)
                    }
                    selectors::attr::AttrSelectorOperator::DashMatch => {
                        attr_value == value_str || attr_value.starts_with(&format!("{}-", value_str))
                    }
                    selectors::attr::AttrSelectorOperator::Prefix => attr_value.starts_with(value_str),
                    selectors::attr::AttrSelectorOperator::Substring => attr_value.contains(value_str),
                    selectors::attr::AttrSelectorOperator::Suffix => attr_value.ends_with(value_str),
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
            PseudoClass::FirstChild => self.element_index() == Some(0),
            PseudoClass::LastChild => {
                let siblings = self.sibling_elements();
                self.element_index() == Some(siblings.len().saturating_sub(1))
            }
            PseudoClass::OnlyChild => self.sibling_elements().len() == 1,
            PseudoClass::NthChild(a, b) => self
                .element_index()
                .map(|index| matches_an_plus_b(*a, *b, (index + 1) as i32))
                .unwrap_or(false),
            PseudoClass::NthLastChild(a, b) => {
                let siblings = self.sibling_elements();
                self.element_index()
                    .map(|index| {
                        let n = (siblings.len() - index) as i32;
                        matches_an_plus_b(*a, *b, n)
                    })
                    .unwrap_or(false)
            }
            PseudoClass::FirstOfType => self.position_in_type().map(|(index, _)| index == 0).unwrap_or(false),
            PseudoClass::LastOfType => self
                .position_in_type()
                .map(|(index, len)| index == len.saturating_sub(1))
                .unwrap_or(false),
            PseudoClass::OnlyOfType => self.position_in_type().map(|(_, len)| len == 1).unwrap_or(false),
            PseudoClass::NthOfType(a, b) => self
                .position_in_type()
                .map(|(index, _)| matches_an_plus_b(*a, *b, (index + 1) as i32))
                .unwrap_or(false),
            PseudoClass::NthLastOfType(a, b) => self
                .position_in_type()
                .map(|(index, len)| {
                    let n = (len - index) as i32;
                    matches_an_plus_b(*a, *b, n)
                })
                .unwrap_or(false),
            PseudoClass::Lang(langs) => {
                if let Some(lang) = self.language() {
                    langs.iter().any(|range| lang_matches(range, &lang))
                } else {
                    false
                }
            }
            PseudoClass::Dir(dir) => self.direction() == *dir,
            PseudoClass::AnyLink => self.is_link(),
            PseudoClass::Target => self.is_target(),
            PseudoClass::Scope => self.all_ancestors.is_empty(),
            PseudoClass::Empty => self.is_empty(),
            // Interactive pseudo-classes (not supported in static rendering)
            PseudoClass::Hover | PseudoClass::Active | PseudoClass::Focus => false,
            PseudoClass::Link => self.is_link(),
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
        let Some(tag) = self.node.tag_name() else {
            return false;
        };
        let has_href = self.node.get_attribute("href").is_some();
        has_href && matches!(tag.to_ascii_lowercase().as_str(), "a" | "area" | "link")
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
        self.node
            .children
            .iter()
            .all(|child| !child.is_element() && !child.is_text())
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

    fn add_element_unique_hashes(
        &self,
        _filter: &mut selectors::bloom::CountingBloomFilter<selectors::bloom::BloomStorageU8>,
    ) -> bool {
        // We don't use bloom filters for optimization
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use selectors::matching::{
        MatchingContext, MatchingForInvalidation, MatchingMode, NeedsSelectorFlags, QuirksMode, SelectorCaches,
    };

    fn element(tag: &str, children: Vec<DomNode>) -> DomNode {
        DomNode {
            node_type: DomNodeType::Element {
                tag_name: tag.to_string(),
                attributes: vec![],
            },
            children,
        }
    }

    fn text(content: &str) -> DomNode {
        DomNode {
            node_type: DomNodeType::Text {
                content: content.to_string(),
            },
            children: vec![],
        }
    }

    fn matches(node: &DomNode, ancestors: &[&DomNode], pseudo: &PseudoClass) -> bool {
        let mut caches = SelectorCaches::default();
        let mut context = MatchingContext::new(
            MatchingMode::Normal,
            None,
            &mut caches,
            QuirksMode::NoQuirks,
            NeedsSelectorFlags::No,
            MatchingForInvalidation::No,
        );
        let element_ref = ElementRef::with_ancestors(node, ancestors);
        element_ref.match_non_ts_pseudo_class(pseudo, &mut context)
    }

    #[test]
    fn namespace_matching_defaults_to_html() {
        let node = element("div", vec![]);
        let element_ref = ElementRef::new(&node);

        assert!(element_ref.has_namespace(""));
        assert!(element_ref.has_namespace(HTML_NAMESPACE));
        assert!(!element_ref.has_namespace("http://www.w3.org/2000/svg"));
    }

    #[test]
    fn is_same_type_ignores_ascii_case() {
        let upper = element("DIV", vec![]);
        let lower = element("div", vec![]);

        let upper_ref = ElementRef::new(&upper);
        let lower_ref = ElementRef::new(&lower);

        assert!(upper_ref.is_same_type(&lower_ref));
    }

    fn collect_wbr_texts(node: &DomNode, out: &mut Vec<String>) {
        if let DomNodeType::Element { tag_name, .. } = &node.node_type {
            if tag_name.eq_ignore_ascii_case("wbr") {
                for child in &node.children {
                    if let DomNodeType::Text { content } = &child.node_type {
                        out.push(content.clone());
                    }
                }
            }
        }
        for child in &node.children {
            collect_wbr_texts(child, out);
        }
    }

    #[test]
    fn empty_pseudo_requires_no_element_or_text_children() {
        let empty = element("div", vec![]);
        let whitespace = element("div", vec![text(" \n")]);
        let child = element("div", vec![element("span", vec![])]);

        assert!(matches(&empty, &[], &PseudoClass::Empty));
        assert!(!matches(&whitespace, &[], &PseudoClass::Empty));
        assert!(!matches(&child, &[], &PseudoClass::Empty));
    }

    #[test]
    fn wbr_inserts_zero_width_break_text_node() {
        let dom = parse_html("<p>Hello<wbr>World</p>").expect("parse html");
        let mut texts = Vec::new();
        collect_wbr_texts(&dom, &mut texts);
        assert!(texts.iter().any(|t| t == "\u{200B}"));
    }

    #[test]
    fn type_position_pseudos_filter_by_tag_name() {
        let parent = element(
            "div",
            vec![element("span", vec![]), element("em", vec![]), element("span", vec![])],
        );
        let ancestors: Vec<&DomNode> = vec![&parent];

        let first_span = &parent.children[0];
        let em = &parent.children[1];
        let second_span = &parent.children[2];

        assert!(matches(first_span, &ancestors, &PseudoClass::FirstOfType));
        assert!(!matches(first_span, &ancestors, &PseudoClass::LastOfType));
        assert!(!matches(first_span, &ancestors, &PseudoClass::OnlyOfType));
        assert!(!matches(first_span, &ancestors, &PseudoClass::NthOfType(2, 0)));
        assert!(matches(first_span, &ancestors, &PseudoClass::NthLastOfType(2, 0)));

        assert!(matches(second_span, &ancestors, &PseudoClass::LastOfType));
        assert!(!matches(second_span, &ancestors, &PseudoClass::OnlyOfType));
        assert!(matches(second_span, &ancestors, &PseudoClass::NthOfType(2, 0)));
        assert!(matches(second_span, &ancestors, &PseudoClass::NthLastOfType(0, 1)));

        // Different element type should be unaffected by span counting.
        assert!(matches(em, &ancestors, &PseudoClass::OnlyOfType));
    }

    #[test]
    fn only_of_type_ignores_unrelated_siblings() {
        let parent = element("div", vec![element("span", vec![]), element("div", vec![])]);
        let ancestors: Vec<&DomNode> = vec![&parent];

        let span = &parent.children[0];
        let div = &parent.children[1];

        assert!(matches(span, &ancestors, &PseudoClass::OnlyOfType));
        assert!(matches(div, &ancestors, &PseudoClass::OnlyOfType));
    }

    #[test]
    fn lang_matches_inherit_and_prefix() {
        let child = element("p", vec![]);
        let root = element(
            "html",
            vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "div".to_string(),
                    attributes: vec![("lang".to_string(), "en-US".to_string())],
                },
                children: vec![child],
            }],
        );

        let ancestors: Vec<&DomNode> = vec![&root, &root.children[0]];
        let node = &root.children[0].children[0];

        assert!(matches(node, &ancestors, &PseudoClass::Lang(vec!["en".into()])));
        assert!(matches(node, &ancestors, &PseudoClass::Lang(vec!["en-us".into()])));
        assert!(matches(node, &ancestors, &PseudoClass::Lang(vec!["*".into()])));
        assert!(!matches(node, &ancestors, &PseudoClass::Lang(vec!["fr".into()])));

        // Multiple ranges OR together
        assert!(matches(
            node,
            &ancestors,
            &PseudoClass::Lang(vec!["fr".into(), "en".into()])
        ));
    }

    #[test]
    fn dir_matches_inherited_direction() {
        let child = element("span", vec![]);
        let root = element(
            "div",
            vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "p".to_string(),
                    attributes: vec![("dir".to_string(), "rtl".to_string())],
                },
                children: vec![child],
            }],
        );
        let ancestors: Vec<&DomNode> = vec![&root, &root.children[0]];
        let node = &root.children[0].children[0];

        assert!(matches(node, &ancestors, &PseudoClass::Dir(TextDirection::Rtl)));
        assert!(!matches(node, &ancestors, &PseudoClass::Dir(TextDirection::Ltr)));
    }

    #[test]
    fn dir_auto_uses_first_strong() {
        let rtl_text = DomNode {
            node_type: DomNodeType::Text {
                content: "שלום".to_string(),
            },
            children: vec![],
        };
        let root = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("dir".to_string(), "auto".to_string())],
            },
            children: vec![rtl_text],
        };
        assert!(matches(&root, &[], &PseudoClass::Dir(TextDirection::Rtl)));
        assert!(!matches(&root, &[], &PseudoClass::Dir(TextDirection::Ltr)));
    }

    #[test]
    fn dir_auto_on_ancestor_inherits_resolved_direction() {
        let rtl_text = DomNode {
            node_type: DomNodeType::Text {
                content: "שלום".to_string(),
            },
            children: vec![],
        };
        let child = element("span", vec![]);
        let container = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "p".to_string(),
                attributes: vec![("dir".to_string(), "auto".to_string())],
            },
            children: vec![rtl_text, child],
        };
        let root = element("div", vec![container]);
        let ancestors: Vec<&DomNode> = vec![&root, &root.children[0]];
        let target = &root.children[0].children[1];
        assert!(matches(target, &ancestors, &PseudoClass::Dir(TextDirection::Rtl)));
    }

    #[test]
    fn any_link_matches_href_anchors() {
        let link = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "a".to_string(),
                attributes: vec![("href".to_string(), "#foo".to_string())],
            },
            children: vec![],
        };
        assert!(matches(&link, &[], &PseudoClass::AnyLink));
        assert!(matches(&link, &[], &PseudoClass::Link));
        assert!(!matches(&link, &[], &PseudoClass::Visited));

        // Area and link elements also qualify
        let area = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "area".to_string(),
                attributes: vec![("href".to_string(), "/foo".to_string())],
            },
            children: vec![],
        };
        let stylesheet_link = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "link".to_string(),
                attributes: vec![("href".to_string(), "style.css".to_string())],
            },
            children: vec![],
        };

        assert!(matches(&area, &[], &PseudoClass::AnyLink));
        assert!(matches(&stylesheet_link, &[], &PseudoClass::AnyLink));
    }

    #[test]
    fn target_matches_id_and_name() {
        let target = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("id".to_string(), "section".to_string())],
            },
            children: vec![],
        };
        with_target_fragment(Some("#section"), || {
            assert!(matches(&target, &[], &PseudoClass::Target));
        });
        with_target_fragment(Some("section"), || {
            assert!(matches(&target, &[], &PseudoClass::Target));
        });
        with_target_fragment(Some("other"), || {
            assert!(!matches(&target, &[], &PseudoClass::Target));
        });

        let anchor = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "a".to_string(),
                attributes: vec![("name".to_string(), "anchor".to_string())],
            },
            children: vec![],
        };
        with_target_fragment(Some("anchor"), || {
            assert!(matches(&anchor, &[], &PseudoClass::Target));
        });
    }

    #[test]
    fn parse_html_preserves_text_content() {
        let html = "<!doctype html><html><body><div><h1>Example Domain</h1><p>This domain is for use in documentation examples without needing permission.</p></div></body></html>";
        let dom = parse_html(html).expect("parse");
        fn contains_text(node: &DomNode, needle: &str) -> bool {
            match &node.node_type {
                DomNodeType::Text { content } => content.contains(needle),
                _ => node.children.iter().any(|c| contains_text(c, needle)),
            }
        }
        assert!(contains_text(&dom, "Example Domain"));
        assert!(contains_text(&dom, "documentation examples"));
    }

    #[test]
    fn scope_matches_document_root_only() {
        let child = element("div", vec![]);
        let root = element("html", vec![child.clone()]);
        let ancestors: Vec<&DomNode> = vec![&root];
        assert!(matches(&root, &[], &PseudoClass::Scope));
        assert!(!matches(&child, &ancestors, &PseudoClass::Scope));
    }
}
