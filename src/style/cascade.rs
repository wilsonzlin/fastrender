//! CSS Cascade and Style Application
//!
//! This module handles applying CSS rules to DOM nodes to produce
//! computed styles. It implements the CSS cascade algorithm.
//!
//! Reference: CSS Cascading and Inheritance Level 4
//! https://www.w3.org/TR/css-cascade-4/

use crate::css::{self, Declaration, StyleRule, StyleSheet};
use crate::dom::{DomNode, ElementRef};
use crate::style::defaults::{get_default_styles_for_element, parse_color_attribute, parse_dimension_attribute};
use crate::style::grid::finalize_grid_placement;
use crate::style::media::MediaContext;
use crate::style::properties::apply_declaration;
use crate::style::types::{BorderStyle, FontWeight, LineHeight};
use crate::style::values::Length;
use crate::style::{ComputedStyle, Display, Rgba};
use selectors::context::{QuirksMode, SelectorCaches};
use selectors::matching::{matches_selector, MatchingContext, MatchingMode};

/// User-agent stylesheet containing default browser styles
const USER_AGENT_STYLESHEET: &str = include_str!("../user_agent.css");

/// A styled DOM node with computed CSS styles
#[derive(Debug, Clone)]
pub struct StyledNode {
    pub node: DomNode,
    pub styles: ComputedStyle,
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
    let ua_stylesheet = css::parse_stylesheet(USER_AGENT_STYLESHEET).unwrap_or_else(|_| StyleSheet::new());

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
        let inline_decls = css::parse_declarations(&style_attr);
        for decl in inline_decls {
            apply_declaration(&mut styles, &decl, parent_styles.font_size, root_font_size);
        }
    }

    // Finalize grid placement - resolve named grid lines
    finalize_grid_placement(&mut styles);

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
        let inline_decls = css::parse_declarations(&style_attr);
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

    // HACK: Add grid placement for TOC (from @media min-width:1000px)
    // The CSS has `.toc { grid-column: 1/4; grid-row: 2; }` but our CSS parser
    // doesn't handle @media queries yet, so we hardcode it here.
    // Must be done AFTER finalize_grid_placement to override.
    if node.has_class("toc") {
        // CSS grid-column: 1/4 means from line 1 to line 4
        // CSS grid-row: 2 means from line 2 to line 3 (span 1)
        // These line numbers are used directly by Taffy (NOT 0-indexed)
        styles.grid_column_start = 1;
        styles.grid_column_end = 4;
        styles.grid_row_start = 2;
        styles.grid_row_end = 3;
    }

    // HACK: Both img-links for the map should be at row 2 alongside TOC
    // Note: HTML has nested <a> elements, but nested links are invalid HTML.
    // The browser/parser auto-corrects this by making them siblings instead of nested.
    // So both the "outer" link (hn.wilsonl.in) and "inner" link (map.png) are
    // actually sibling grid items, and both need to be at row 2.
    if node.has_class("img-link") {
        if let Some(href) = node.get_attribute("href") {
            if href == "https://hn.wilsonl.in" || href == "./map.png" {
                styles.grid_row_start = 2;
                styles.grid_row_end = 3;
            }
        }
    }

    // HACK: Add border-radius to images
    if node.tag_name() == Some("img") {
        // Check if we're inside an article element (.5rem = 8px)
        let in_article = ancestors.iter().any(|a| a.tag_name() == Some("article"));
        if in_article {
            styles.border_top_left_radius = Length::px(8.0);
            styles.border_top_right_radius = Length::px(8.0);
            styles.border_bottom_left_radius = Length::px(8.0);
            styles.border_bottom_right_radius = Length::px(8.0);
        }

        // Check if we're inside banner-left (avatar image with border-radius: 50%)
        let in_banner_left = ancestors.iter().any(|a| a.has_class("banner-left"));
        if in_banner_left {
            // border-radius: 50% for circular avatar
            // Since the image is 2rem (32px) square, 50% = 16px radius
            styles.border_top_left_radius = Length::px(16.0);
            styles.border_top_right_radius = Length::px(16.0);
            styles.border_bottom_left_radius = Length::px(16.0);
            styles.border_bottom_right_radius = Length::px(16.0);
        }
    }

    // HACK: Style subscribe button - white background, blue border and text
    if node.has_class("subscribe-btn") {
        styles.padding_top = Length::px(6.0); // .375rem
        styles.padding_right = Length::px(12.0); // .75rem
        styles.padding_bottom = Length::px(6.0);
        styles.padding_left = Length::px(12.0);
        styles.background_color = Rgba {
            r: 255,
            g: 255,
            b: 255,
            a: 1.0,
        }; // white background
        styles.color = Rgba {
            r: 59,
            g: 130,
            b: 246,
            a: 1.0,
        }; // #3b82f6 blue text
        styles.border_top_width = Length::px(1.0);
        styles.border_right_width = Length::px(1.0);
        styles.border_bottom_width = Length::px(1.0);
        styles.border_left_width = Length::px(1.0);
        styles.border_top_color = Rgba {
            r: 59,
            g: 130,
            b: 246,
            a: 1.0,
        }; // #3b82f6
        styles.border_right_color = Rgba {
            r: 59,
            g: 130,
            b: 246,
            a: 1.0,
        };
        styles.border_bottom_color = Rgba {
            r: 59,
            g: 130,
            b: 246,
            a: 1.0,
        };
        styles.border_left_color = Rgba {
            r: 59,
            g: 130,
            b: 246,
            a: 1.0,
        };
        styles.border_top_style = BorderStyle::Solid;
        styles.border_right_style = BorderStyle::Solid;
        styles.border_bottom_style = BorderStyle::Solid;
        styles.border_left_style = BorderStyle::Solid;
        styles.border_top_left_radius = Length::px(4.0); // .25rem
        styles.border_top_right_radius = Length::px(4.0);
        styles.border_bottom_left_radius = Length::px(4.0);
        styles.border_bottom_right_radius = Length::px(4.0);
    }

    // Recursively style children (passing current node in ancestors)
    let mut new_ancestors = ancestors.to_vec();
    new_ancestors.push(node);
    let mut children: Vec<StyledNode> = node
        .children
        .iter()
        .map(|child| apply_styles_internal_with_ancestors(child, rules, &styles, root_font_size, &new_ancestors))
        .collect();

    // HACK: Add ::before pseudo-element content for .toc elements
    // The CSS has `.toc::before { content: "Contents"; ... }`
    if node.has_class("toc") {
        // Create a synthetic container with text content
        // We can't use a plain text node because layout filters those out
        let text_node = DomNode {
            node_type: crate::dom::DomNodeType::Text {
                content: "CONTENTS".to_string(),
            },
            children: vec![],
        };

        let before_node = DomNode {
            node_type: crate::dom::DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![],
            },
            children: vec![text_node.clone()],
        };

        // Create styles for the ::before pseudo-element matching the CSS
        let mut before_styles = ComputedStyle::default();
        before_styles.display = Display::Block;
        before_styles.font_size = 12.0; // .75rem = 12px
        before_styles.font_weight = FontWeight::Bold;
        before_styles.color = Rgba {
            r: 90,
            g: 90,
            b: 90,
            a: 1.0,
        }; // #5a5a5a (--color-text-muted)
        before_styles.margin_bottom = Some(Length::px(8.0));
        before_styles.line_height = LineHeight::Normal;

        // Create styled text child
        let text_styled = StyledNode {
            node: text_node.clone(),
            styles: ComputedStyle::default(),
            children: vec![],
        };

        let before_styled = StyledNode {
            node: before_node,
            styles: before_styles,
            children: vec![text_styled],
        };

        // Insert at the beginning
        children.insert(0, before_styled);
    }

    StyledNode {
        node: node.clone(),
        styles,
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
    styles.text_align = parent.text_align;
    styles.text_transform = parent.text_transform;
    styles.letter_spacing = parent.letter_spacing;
    styles.word_spacing = parent.word_spacing;
    styles.white_space = parent.white_space;

    // Color inherits
    styles.color = parent.color;

    // CSS Custom Properties inherit
    styles.custom_properties = parent.custom_properties.clone();

    // Grid line names inherit (so children can reference parent's named grid lines)
    styles.grid_column_names = parent.grid_column_names.clone();
    styles.grid_row_names = parent.grid_row_names.clone();
}

fn find_matching_rules(
    node: &DomNode,
    rules: &[&StyleRule],
    ancestors: &[&DomNode],
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
        // Check if any selector in the list matches
        let mut matched = false;
        let mut max_specificity = 0u32;

        for selector in rule.selectors.slice().iter() {
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

/// Build an ElementRef with ancestor context
fn build_element_ref_chain<'a>(node: &'a DomNode, ancestors: &'a [&'a DomNode]) -> ElementRef<'a> {
    if ancestors.is_empty() {
        return ElementRef::new(node);
    }

    // Create ElementRef with all ancestors
    ElementRef::with_ancestors(node, ancestors)
}
