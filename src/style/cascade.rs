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
    /// Styles for ::marker pseudo-element (list items only)
    pub marker_styles: Option<ComputedStyle>,
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
    let marker_styles = compute_marker_styles(node, rules, &ancestors, &styles, root_font_size);

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
        marker_styles,
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
    let marker_styles = compute_marker_styles(node, rules, ancestors, &styles, root_font_size);

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
        marker_styles,
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
    styles.tab_size = parent.tab_size;
    styles.hyphens = parent.hyphens;
    styles.word_break = parent.word_break;
    styles.overflow_wrap = parent.overflow_wrap;
    styles.language = parent.language.clone();
    styles.list_style_type = parent.list_style_type;
    styles.list_style_position = parent.list_style_position;

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
    use crate::css::parser::parse_stylesheet;
    use crate::css::parser::parse_declarations;
    use crate::css::types::StyleSheet;
    use crate::dom::DomNodeType;
    use crate::style::color::Rgba;
    use crate::style::display::Display;
    use crate::style::types::{ListStylePosition, ListStyleType};

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
        assert!(matches!(
            styled.styles.text_align,
            crate::style::types::TextAlign::Center
        ));
        assert!(matches!(
            styled.styles.text_align_last,
            crate::style::types::TextAlignLast::Auto
        ));
    }

    #[test]
    fn text_align_justify_all_sets_last_line_justify() {
        let dom = element_with_style("text-align-last: right; text-align: justify-all;");
        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align,
            crate::style::types::TextAlign::Justify
        ));
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
        assert!(matches!(
            child.styles.text_align,
            crate::style::types::TextAlign::Center
        ));
    }

    #[test]
    fn list_style_inherits_from_parent() {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "list-style-type: square; list-style-position: inside;".to_string(),
                )],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![("style".to_string(), "color: red;".to_string())],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let li = styled.children.first().expect("li");
        assert!(matches!(li.styles.list_style_type, ListStyleType::Square));
        assert!(matches!(li.styles.list_style_position, ListStylePosition::Inside));
    }

    #[test]
    fn marker_pseudo_resets_box_model_and_forces_inline_display() {
        let lone_li = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "li".to_string(),
                attributes: vec![("style".to_string(), "color: red;".to_string())],
            },
            children: vec![],
        };
        assert_eq!(lone_li.get_attribute("style"), Some("color: red;".to_string()));
        let decls = parse_declarations("color: red;");
        assert_eq!(decls.len(), 1);
        if let crate::css::types::PropertyValue::Color(c) = decls[0].value {
            assert_eq!(c, Rgba::RED);
        } else {
            panic!("color did not parse");
        }
        let mut manual = get_default_styles_for_element(&lone_li);
        inherit_styles(&mut manual, &ComputedStyle::default());
        let fs = manual.font_size;
        for decl in parse_declarations("color: red;") {
            apply_declaration(&mut manual, &decl, fs, fs);
        }
        assert_eq!(manual.color, Rgba::RED);
        let lone_styled = apply_styles(&lone_li, &StyleSheet::new());
        assert_eq!(lone_styled.styles.color, Rgba::RED);

        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![("style".to_string(), "color: red;".to_string())],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                color: red;
                display: block;
                padding: 10px;
                margin-left: 12px;
                background: blue;
            }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        assert_eq!(li.styles.color, Rgba::RED);
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert_eq!(marker.color, Rgba::RED);
        assert!(matches!(marker.display, Display::Inline));
        assert!(marker.padding_left.is_zero());
        assert!(marker.padding_right.is_zero());
        assert!(marker.margin_left.unwrap().is_zero());
        assert_eq!(marker.background_color, Rgba::TRANSPARENT);
        assert!(matches!(marker.text_transform, crate::style::types::TextTransform::None));
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

fn compute_marker_styles(
    node: &DomNode,
    rules: &[&StyleRule],
    ancestors: &[&DomNode],
    parent_styles: &ComputedStyle,
    root_font_size: f32,
) -> Option<ComputedStyle> {
    if parent_styles.display != Display::ListItem {
        return None;
    }

    let matching_rules = find_pseudo_element_rules(node, rules, ancestors, &PseudoElement::Marker);

    let mut styles = ComputedStyle::default();
    styles.display = Display::Inline;
    inherit_styles(&mut styles, parent_styles);

    for (_specificity, declarations) in matching_rules {
        for decl in declarations {
            apply_declaration(&mut styles, &decl, parent_styles.font_size, root_font_size);
        }
    }

    reset_marker_box_properties(&mut styles);
    styles.display = Display::Inline;
    styles.text_transform = crate::style::types::TextTransform::None;
    Some(styles)
}

fn reset_marker_box_properties(styles: &mut ComputedStyle) {
    let defaults = ComputedStyle::default();
    styles.position = defaults.position;
    styles.top = None;
    styles.right = None;
    styles.bottom = None;
    styles.left = None;
    styles.z_index = defaults.z_index;

    styles.width = None;
    styles.height = None;
    styles.min_width = None;
    styles.min_height = None;
    styles.max_width = None;
    styles.max_height = None;

    styles.margin_top = defaults.margin_top;
    styles.margin_right = defaults.margin_right;
    styles.margin_bottom = defaults.margin_bottom;
    styles.margin_left = defaults.margin_left;

    styles.padding_top = defaults.padding_top;
    styles.padding_right = defaults.padding_right;
    styles.padding_bottom = defaults.padding_bottom;
    styles.padding_left = defaults.padding_left;

    styles.border_top_width = defaults.border_top_width;
    styles.border_right_width = defaults.border_right_width;
    styles.border_bottom_width = defaults.border_bottom_width;
    styles.border_left_width = defaults.border_left_width;

    styles.border_top_style = defaults.border_top_style;
    styles.border_right_style = defaults.border_right_style;
    styles.border_bottom_style = defaults.border_bottom_style;
    styles.border_left_style = defaults.border_left_style;

    styles.border_top_color = defaults.border_top_color;
    styles.border_right_color = defaults.border_right_color;
    styles.border_bottom_color = defaults.border_bottom_color;
    styles.border_left_color = defaults.border_left_color;

    styles.border_top_left_radius = defaults.border_top_left_radius;
    styles.border_top_right_radius = defaults.border_top_right_radius;
    styles.border_bottom_left_radius = defaults.border_bottom_left_radius;
    styles.border_bottom_right_radius = defaults.border_bottom_right_radius;

    styles.background_color = defaults.background_color;
    styles.background_image = defaults.background_image.clone();
    styles.background_size = defaults.background_size.clone();
    styles.background_position = defaults.background_position.clone();
    styles.background_repeat = defaults.background_repeat.clone();
    styles.background_origin = defaults.background_origin;
    styles.background_clip = defaults.background_clip;
    styles.object_fit = defaults.object_fit;
    styles.object_position = defaults.object_position.clone();

    styles.box_shadow.clear();
    styles.text_shadow.clear();
    styles.filter.clear();
    styles.backdrop_filter.clear();
    styles.mix_blend_mode = defaults.mix_blend_mode;
    styles.isolation = defaults.isolation;
    styles.transform.clear();
    styles.transform_origin = defaults.transform_origin.clone();
    styles.overflow_x = defaults.overflow_x;
    styles.overflow_y = defaults.overflow_y;
    styles.opacity = defaults.opacity;

    // Markers should not carry table/layout-specific state
    styles.border_spacing_horizontal = defaults.border_spacing_horizontal;
    styles.border_spacing_vertical = defaults.border_spacing_vertical;
    styles.border_collapse = defaults.border_collapse;
    styles.table_layout = defaults.table_layout;
}
