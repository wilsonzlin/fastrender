//! CSS Cascade and Style Application
//!
//! This module handles applying CSS rules to DOM nodes to produce
//! computed styles. It implements the CSS cascade algorithm.
//!
//! Reference: CSS Cascading and Inheritance Level 4
//! <https://www.w3.org/TR/css-cascade-4/>

use crate::css::parser::{parse_declarations, parse_stylesheet};
use crate::css::selectors::PseudoElement;
use crate::css::types::{Declaration, StyleRule, StyleSheet};
use crate::dom::{DomNode, ElementRef};
use crate::style::defaults::{get_default_styles_for_element, parse_color_attribute, parse_dimension_attribute};
use crate::style::display::Display;
use crate::style::grid::finalize_grid_placement;
use crate::style::media::MediaContext;
use crate::style::properties::apply_declaration;
use crate::style::{normalize_language_tag, ComputedStyle};
use selectors::context::{QuirksMode, SelectorCaches};
use selectors::matching::{matches_selector, MatchingContext, MatchingMode};

/// User-agent stylesheet containing default browser styles
const USER_AGENT_STYLESHEET: &str = include_str!("../user_agent.css");

/// A styled DOM node with computed CSS styles
#[derive(Debug, Clone)]
pub struct StyledNode {
    pub node: DomNode,
    pub styles: ComputedStyle,
    /// Styles for ::before pseudo-element (if content is set)
    pub before_styles: Option<ComputedStyle>,
    /// Styles for ::after pseudo-element (if content is set)
    pub after_styles: Option<ComputedStyle>,
    pub children: Vec<StyledNode>,
}

/// Apply styles to a DOM tree with default viewport (desktop)
///
/// This is the main entry point for CSS cascade. It uses a default
/// desktop viewport size for media query evaluation.
pub fn apply_styles(dom: &DomNode, stylesheet: &StyleSheet) -> StyledNode {
    // Use default desktop viewport
    let media_ctx = MediaContext::screen(1200.0, 800.0);
    apply_styles_with_media(dom, stylesheet, &media_ctx)
}

/// Apply styles to a DOM tree with a specific media context
///
/// This allows controlling viewport size and other media features
/// for responsive rendering.
pub fn apply_styles_with_media(dom: &DomNode, stylesheet: &StyleSheet, media_ctx: &MediaContext) -> StyledNode {
    // Parse user-agent stylesheet
    let ua_stylesheet = parse_stylesheet(USER_AGENT_STYLESHEET).unwrap_or_else(|_| StyleSheet::new());

    // Collect applicable rules from both stylesheets
    // User-agent rules come first (lower priority)
    let mut all_rules: Vec<&StyleRule> = ua_stylesheet.collect_style_rules(media_ctx);
    all_rules.extend(stylesheet.collect_style_rules(media_ctx));

    apply_styles_internal(dom, &all_rules, &ComputedStyle::default(), 16.0)
}

fn apply_styles_internal(
    node: &DomNode,
    rules: &[&StyleRule],
    parent_styles: &ComputedStyle,
    root_font_size: f32,
) -> StyledNode {
    let mut styles = get_default_styles_for_element(node);

    // Inherit styles from parent
    inherit_styles(&mut styles, parent_styles);

    // Apply matching CSS rules
    let ancestors: Vec<&DomNode> = vec![];
    let matching_rules = find_matching_rules(node, rules, &ancestors);
    for (_specificity, declarations) in matching_rules {
        for decl in declarations {
            apply_declaration(&mut styles, &decl, parent_styles.font_size, root_font_size);
        }
    }

    // Apply inline styles (highest specificity)
    if let Some(style_attr) = node.get_attribute("style") {
        let inline_decls = parse_declarations(&style_attr);
        for decl in inline_decls {
            apply_declaration(&mut styles, &decl, parent_styles.font_size, root_font_size);
        }
    }

    // Apply language from attributes (inherits by default)
    if let Some(lang) = node
        .get_attribute("lang")
        .or_else(|| node.get_attribute("xml:lang"))
        .filter(|l| !l.is_empty())
    {
        styles.language = normalize_language_tag(&lang);
    }

    // Finalize grid placement - resolve named grid lines
    finalize_grid_placement(&mut styles);
    resolve_match_parent_text_align(&mut styles, parent_styles);

    // Compute pseudo-element styles
    let before_styles =
        compute_pseudo_element_styles(node, rules, &ancestors, &styles, root_font_size, &PseudoElement::Before);
    let after_styles =
        compute_pseudo_element_styles(node, rules, &ancestors, &styles, root_font_size, &PseudoElement::After);

    // Recursively style children (passing current node in ancestors)
    let mut new_ancestors = ancestors.clone();
    new_ancestors.push(node);
    let children = node
        .children
        .iter()
        .map(|child| apply_styles_internal_with_ancestors(child, rules, &styles, root_font_size, &new_ancestors))
        .collect();

    StyledNode {
        node: node.clone(),
        styles,
        before_styles,
        after_styles,
        children,
    }
}

fn apply_styles_internal_with_ancestors(
    node: &DomNode,
    rules: &[&StyleRule],
    parent_styles: &ComputedStyle,
    root_font_size: f32,
    ancestors: &[&DomNode],
) -> StyledNode {
    let mut styles = get_default_styles_for_element(node);

    // Inherit styles from parent
    inherit_styles(&mut styles, parent_styles);

    // Apply matching CSS rules
    let matching_rules = find_matching_rules(node, rules, ancestors);
    for (_specificity, declarations) in matching_rules {
        for decl in declarations {
            apply_declaration(&mut styles, &decl, parent_styles.font_size, root_font_size);
        }
    }

    // Apply inline styles (highest specificity)
    if let Some(style_attr) = node.get_attribute("style") {
        let inline_decls = parse_declarations(&style_attr);
        for decl in inline_decls {
            apply_declaration(&mut styles, &decl, parent_styles.font_size, root_font_size);
        }
    }

    // Parse legacy HTML presentation attributes (bgcolor, width, height, etc.)
    if let Some(bgcolor) = node.get_attribute("bgcolor") {
        if let Some(color) = parse_color_attribute(&bgcolor) {
            styles.background_color = color;
        }
    }

    // Parse width attribute (can be pixels like "18" or percentage like "85%")
    if let Some(width_str) = node.get_attribute("width") {
        if let Some(width) = parse_dimension_attribute(&width_str) {
            styles.width = Some(width);
        }
    }

    // Parse height attribute
    if let Some(height_str) = node.get_attribute("height") {
        if let Some(height) = parse_dimension_attribute(&height_str) {
            styles.height = Some(height);
        }
    }

    // Finalize grid placement - resolve named grid lines
    finalize_grid_placement(&mut styles);
    resolve_match_parent_text_align(&mut styles, parent_styles);

    // Compute pseudo-element styles from CSS rules
    let before_styles =
        compute_pseudo_element_styles(node, rules, ancestors, &styles, root_font_size, &PseudoElement::Before);
    let after_styles =
        compute_pseudo_element_styles(node, rules, ancestors, &styles, root_font_size, &PseudoElement::After);

    // Recursively style children (passing current node in ancestors)
    let mut new_ancestors = ancestors.to_vec();
    new_ancestors.push(node);
    let children: Vec<StyledNode> = node
        .children
        .iter()
        .map(|child| apply_styles_internal_with_ancestors(child, rules, &styles, root_font_size, &new_ancestors))
        .collect();

    StyledNode {
        node: node.clone(),
        styles,
        before_styles,
        after_styles,
        children,
    }
}

fn inherit_styles(styles: &mut ComputedStyle, parent: &ComputedStyle) {
    // Typography properties inherit
    styles.font_family = parent.font_family.clone();
    styles.font_size = parent.font_size;
    styles.font_weight = parent.font_weight;
    styles.font_style = parent.font_style;
    styles.line_height = parent.line_height.clone();
    styles.direction = parent.direction;
    styles.text_align = parent.text_align;
    styles.text_align_last = parent.text_align_last;
    styles.text_justify = parent.text_justify;
    styles.text_indent = parent.text_indent;
    styles.text_transform = parent.text_transform;
    styles.letter_spacing = parent.letter_spacing;
    styles.word_spacing = parent.word_spacing;
    styles.white_space = parent.white_space;
    styles.hyphens = parent.hyphens;
    styles.word_break = parent.word_break;
    styles.overflow_wrap = parent.overflow_wrap;
    styles.language = parent.language.clone();

    // Color inherits
    styles.color = parent.color;

    // CSS Custom Properties inherit
    styles.custom_properties = parent.custom_properties.clone();

    // Grid line names inherit (so children can reference parent's named grid lines)
    styles.grid_column_names = parent.grid_column_names.clone();
    styles.grid_row_names = parent.grid_row_names.clone();
}

fn resolve_match_parent_text_align(styles: &mut ComputedStyle, parent: &ComputedStyle) {
    use crate::style::types::TextAlign;
    if !matches!(styles.text_align, TextAlign::MatchParent) {
        return;
    }
    // Behaves like inherit, but start/end become physical based on the parent's direction.
    let inherited = match parent.text_align {
        TextAlign::MatchParent => {
            if matches!(parent.direction, crate::style::types::Direction::Rtl) {
                TextAlign::End
            } else {
                TextAlign::Start
            }
        }
        other => other,
    };
    styles.text_align = match inherited {
        TextAlign::Start => {
            if matches!(parent.direction, crate::style::types::Direction::Rtl) {
                TextAlign::Right
            } else {
                TextAlign::Left
            }
        }
        TextAlign::End => {
            if matches!(parent.direction, crate::style::types::Direction::Rtl) {
                TextAlign::Left
            } else {
                TextAlign::Right
            }
        }
        other => other,
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::css::types::StyleSheet;
    use crate::dom::DomNodeType;

    fn element_with_style(style: &str) -> DomNode {
        DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), style.to_string())],
            },
            children: vec![],
        }
    }

    #[test]
    fn text_align_shorthand_resets_text_align_last_to_auto() {
        let dom = element_with_style("text-align-last: right; text-align: center;");
        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(styled.styles.text_align, crate::style::types::TextAlign::Center));
        assert!(matches!(
            styled.styles.text_align_last,
            crate::style::types::TextAlignLast::Auto
        ));
    }

    #[test]
    fn text_align_justify_all_sets_last_line_justify() {
        let dom = element_with_style("text-align-last: right; text-align: justify-all;");
        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(styled.styles.text_align, crate::style::types::TextAlign::Justify));
        assert!(matches!(
            styled.styles.text_align_last,
            crate::style::types::TextAlignLast::Justify
        ));
    }

    #[test]
    fn text_align_match_parent_maps_start_end_using_parent_direction() {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "direction: rtl; text-align: start;".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![("style".to_string(), "text-align: match-parent;".to_string())],
                },
                children: vec![],
            }],
        };
        let styled = apply_styles(&parent, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert!(matches!(child.styles.text_align, crate::style::types::TextAlign::Right));
    }

    #[test]
    fn text_align_match_parent_inherits_parent_alignment() {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "text-align: center;".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![("style".to_string(), "text-align: match-parent;".to_string())],
                },
                children: vec![],
            }],
        };
        let styled = apply_styles(&parent, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert!(matches!(child.styles.text_align, crate::style::types::TextAlign::Center));
    }
}

fn find_matching_rules(node: &DomNode, rules: &[&StyleRule], ancestors: &[&DomNode]) -> Vec<(u32, Vec<Declaration>)> {
    let mut matches = Vec::new();

    // Build ElementRef chain with proper parent links
    let element_ref = build_element_ref_chain(node, ancestors);

    // Create selector caches and matching context
    let mut caches = SelectorCaches::default();
    let mut context = MatchingContext::new(
        MatchingMode::Normal,
        None,
        &mut caches,
        QuirksMode::NoQuirks,
        selectors::matching::NeedsSelectorFlags::No,
        selectors::matching::MatchingForInvalidation::No,
    );

    for rule in rules {
        // Check if any selector in the list matches
        let mut matched = false;
        let mut max_specificity = 0u32;

        for selector in rule.selectors.slice().iter() {
            // Skip selectors with pseudo-elements (handled separately)
            if selector.pseudo_element().is_some() {
                continue;
            }
            if matches_selector(selector, 0, None, &element_ref, &mut context) {
                matched = true;
                let spec = selector.specificity();
                if spec > max_specificity {
                    max_specificity = spec;
                }
            }
        }

        if matched {
            matches.push((max_specificity, rule.declarations.clone()));
        }
    }

    // Sort by specificity (lower specificity first, so later rules override)
    matches.sort_by_key(|(spec, _)| *spec);

    matches
}

/// Find rules that match an element with a specific pseudo-element
fn find_pseudo_element_rules(
    node: &DomNode,
    rules: &[&StyleRule],
    ancestors: &[&DomNode],
    pseudo: &PseudoElement,
) -> Vec<(u32, Vec<Declaration>)> {
    let mut matches = Vec::new();

    // Build ElementRef chain with proper parent links
    let element_ref = build_element_ref_chain(node, ancestors);

    // Create selector caches and matching context
    let mut caches = SelectorCaches::default();
    let mut context = MatchingContext::new(
        MatchingMode::Normal,
        None,
        &mut caches,
        QuirksMode::NoQuirks,
        selectors::matching::NeedsSelectorFlags::No,
        selectors::matching::MatchingForInvalidation::No,
    );

    for rule in rules {
        let mut matched = false;
        let mut max_specificity = 0u32;

        for selector in rule.selectors.slice().iter() {
            // Only consider selectors with the matching pseudo-element
            if let Some(selector_pseudo) = selector.pseudo_element() {
                if selector_pseudo == pseudo {
                    // Check if the element part matches
                    if matches_selector(selector, 0, None, &element_ref, &mut context) {
                        matched = true;
                        let spec = selector.specificity();
                        if spec > max_specificity {
                            max_specificity = spec;
                        }
                    }
                }
            }
        }

        if matched {
            matches.push((max_specificity, rule.declarations.clone()));
        }
    }

    // Sort by specificity
    matches.sort_by_key(|(spec, _)| *spec);

    matches
}

/// Build an ElementRef with ancestor context
fn build_element_ref_chain<'a>(node: &'a DomNode, ancestors: &'a [&'a DomNode]) -> ElementRef<'a> {
    if ancestors.is_empty() {
        return ElementRef::new(node);
    }

    // Create ElementRef with all ancestors
    ElementRef::with_ancestors(node, ancestors)
}

/// Compute styles for a pseudo-element (::before or ::after)
///
/// Returns Some(ComputedStyle) if the pseudo-element should be generated
/// (i.e., has content property set to something other than 'none' or 'normal')
fn compute_pseudo_element_styles(
    node: &DomNode,
    rules: &[&StyleRule],
    ancestors: &[&DomNode],
    parent_styles: &ComputedStyle,
    root_font_size: f32,
    pseudo: &PseudoElement,
) -> Option<ComputedStyle> {
    // Find rules matching this pseudo-element
    let matching_rules = find_pseudo_element_rules(node, rules, ancestors, pseudo);

    if matching_rules.is_empty() {
        return None;
    }

    // Start with default inline styles (pseudo-elements default to display: inline)
    let mut styles = ComputedStyle::default();
    styles.display = Display::Inline;

    // Inherit from parent element
    inherit_styles(&mut styles, parent_styles);

    // Apply matching declarations
    for (_specificity, declarations) in matching_rules {
        for decl in declarations {
            apply_declaration(&mut styles, &decl, parent_styles.font_size, root_font_size);
        }
    }

    // Check if content property generates content
    // Per CSS spec, ::before/::after only generate boxes if content is not 'none' or 'normal'
    if styles.content.is_empty() || styles.content == "none" || styles.content == "normal" {
        return None;
    }

    Some(styles)
}
