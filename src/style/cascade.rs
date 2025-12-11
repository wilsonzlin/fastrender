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
    resolve_relative_font_weight(&mut styles, parent_styles);
    propagate_text_decorations(&mut styles, parent_styles);

    // Root font size for rem resolution: the document root sets the value for all descendants.
    let is_root = ancestors.is_empty();
    let current_root_font_size = if is_root { styles.font_size } else { root_font_size };
    styles.root_font_size = current_root_font_size;

    // Compute pseudo-element styles
    let before_styles = compute_pseudo_element_styles(
        node,
        rules,
        &ancestors,
        &styles,
        current_root_font_size,
        &PseudoElement::Before,
    );
    let after_styles = compute_pseudo_element_styles(
        node,
        rules,
        &ancestors,
        &styles,
        current_root_font_size,
        &PseudoElement::After,
    );
    let marker_styles = compute_marker_styles(node, rules, &ancestors, &styles, current_root_font_size);

    // Recursively style children (passing current node in ancestors)
    let mut new_ancestors = ancestors.clone();
    new_ancestors.push(node);
    let children = node
        .children
        .iter()
        .map(|child| {
            apply_styles_internal_with_ancestors(child, rules, &styles, current_root_font_size, &new_ancestors)
        })
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
    resolve_relative_font_weight(&mut styles, parent_styles);
    propagate_text_decorations(&mut styles, parent_styles);

    let is_root = ancestors.is_empty();
    let current_root_font_size = if is_root { styles.font_size } else { root_font_size };
    styles.root_font_size = current_root_font_size;

    // Compute pseudo-element styles from CSS rules
    let before_styles = compute_pseudo_element_styles(
        node,
        rules,
        ancestors,
        &styles,
        current_root_font_size,
        &PseudoElement::Before,
    );
    let after_styles = compute_pseudo_element_styles(
        node,
        rules,
        ancestors,
        &styles,
        current_root_font_size,
        &PseudoElement::After,
    );
    let marker_styles = compute_marker_styles(node, rules, ancestors, &styles, current_root_font_size);

    // Recursively style children (passing current node in ancestors)
    let mut new_ancestors = ancestors.to_vec();
    new_ancestors.push(node);
    let children: Vec<StyledNode> = node
        .children
        .iter()
        .map(|child| {
            apply_styles_internal_with_ancestors(child, rules, &styles, current_root_font_size, &new_ancestors)
        })
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
    styles.root_font_size = parent.root_font_size;
    styles.font_weight = parent.font_weight;
    styles.font_style = parent.font_style;
    styles.font_variant = parent.font_variant;
    styles.font_variant_caps = parent.font_variant_caps;
    styles.font_variant_alternates = parent.font_variant_alternates.clone();
    styles.font_variant_numeric = parent.font_variant_numeric;
    styles.font_variant_east_asian = parent.font_variant_east_asian;
    styles.font_variant_ligatures = parent.font_variant_ligatures;
    styles.font_variant_position = parent.font_variant_position;
    styles.font_size_adjust = parent.font_size_adjust;
    styles.font_synthesis = parent.font_synthesis;
    styles.font_feature_settings = parent.font_feature_settings.clone();
    styles.font_stretch = parent.font_stretch;
    styles.font_kerning = parent.font_kerning;
    styles.line_height = parent.line_height.clone();
    styles.direction = parent.direction;
    styles.text_align = parent.text_align;
    styles.text_align_last = parent.text_align_last;
    styles.text_justify = parent.text_justify;
    styles.text_indent = parent.text_indent;
    styles.text_decoration_skip_ink = parent.text_decoration_skip_ink;
    styles.text_underline_offset = parent.text_underline_offset;
    styles.text_underline_position = parent.text_underline_position;
    styles.text_emphasis_style = parent.text_emphasis_style.clone();
    styles.text_emphasis_color = parent.text_emphasis_color;
    styles.text_emphasis_position = parent.text_emphasis_position;
    styles.text_transform = parent.text_transform;
    styles.text_combine_upright = parent.text_combine_upright;
    // text-orientation is non-inherited; leave as initial value
    styles.writing_mode = parent.writing_mode;
    styles.letter_spacing = parent.letter_spacing;
    styles.word_spacing = parent.word_spacing;
    styles.justify_items = parent.justify_items;
    styles.visibility = parent.visibility;
    styles.white_space = parent.white_space;
    styles.line_break = parent.line_break;
    styles.tab_size = parent.tab_size;
    styles.caption_side = parent.caption_side;
    styles.empty_cells = parent.empty_cells;
    styles.hyphens = parent.hyphens;
    styles.word_break = parent.word_break;
    styles.overflow_wrap = parent.overflow_wrap;
    styles.language = parent.language.clone();
    styles.list_style_type = parent.list_style_type;
    styles.list_style_position = parent.list_style_position;
    styles.list_style_image = parent.list_style_image.clone();

    // Color inherits
    styles.color = parent.color;

    // CSS Custom Properties inherit
    styles.custom_properties = parent.custom_properties.clone();
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
    use crate::css::parser::parse_declarations;
    use crate::css::parser::parse_stylesheet;
    use crate::css::types::StyleSheet;
    use crate::dom::DomNodeType;
    use crate::style::color::Rgba;
    use crate::style::computed::Visibility;
    use crate::style::display::Display;
    use crate::style::float::Float;
    use crate::style::types::{
        LineBreak, ListStylePosition, ListStyleType, TextCombineUpright, TextDecorationLine, TextUnderlineOffset,
        TextUnderlinePosition, UnicodeBidi, WhiteSpace,
    };

    fn element_with_style(style: &str) -> DomNode {
        DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), style.to_string())],
            },
            children: vec![],
        }
    }

    fn child_font_weight(parent_style: &str, child_style: &str) -> u16 {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), parent_style.to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![("style".to_string(), child_style.to_string())],
                },
                children: vec![],
            }],
        };
        let styled = apply_styles(&parent, &StyleSheet::new());
        styled.children.first().expect("child").styles.font_weight.to_u16()
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
    fn text_decoration_propagates_to_descendants() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "text-decoration: underline;".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert_eq!(child_styles.applied_text_decorations.len(), 1);
        assert!(child_styles.applied_text_decorations[0]
            .decoration
            .lines
            .contains(TextDecorationLine::UNDERLINE));
    }

    #[test]
    fn underline_offset_and_position_inherit() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "text-underline-offset: 2px; text-underline-position: under right;".to_string(),
                )],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        assert!(
            matches!(styled.styles.text_underline_position, TextUnderlinePosition::UnderRight),
            "parent got {:?}",
            styled.styles.text_underline_position
        );
        let child_styles = &styled.children[0].styles;
        match child_styles.text_underline_offset {
            TextUnderlineOffset::Length(l) => assert!((l.to_px() - 2.0).abs() < 0.01),
            other => panic!("expected underline offset to inherit, got {:?}", other),
        }
        assert!(
            matches!(child_styles.text_underline_position, TextUnderlinePosition::UnderRight),
            "got {:?}",
            child_styles.text_underline_position
        );
    }

    #[test]
    fn text_emphasis_inherits() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "text-emphasis-style: open dot; text-emphasis-color: red; text-emphasis-position: under left;"
                        .to_string(),
                )],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert!(matches!(
            child_styles.text_emphasis_style,
            crate::style::types::TextEmphasisStyle::Mark {
                fill: crate::style::types::TextEmphasisFill::Open,
                shape: crate::style::types::TextEmphasisShape::Dot
            }
        ));
        assert_eq!(child_styles.text_emphasis_color, Some(Rgba::RED));
        assert!(matches!(
            child_styles.text_emphasis_position,
            crate::style::types::TextEmphasisPosition::UnderLeft
        ));
    }

    #[test]
    fn line_break_inherits() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "line-break: anywhere;".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert_eq!(child_styles.line_break, LineBreak::Anywhere);
    }

    #[test]
    fn grid_templates_do_not_inherit() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "display: grid; \
                     grid-template-columns: [a] 50px [b] 1fr [c]; \
                     grid-template-rows: [top] 10px [middle] auto [bottom]; \
                     grid-template-areas: \"hero sidebar\" \"footer footer\";"
                        .to_string(),
                )],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let parent_styles = &styled.styles;
        assert_eq!(parent_styles.grid_template_columns.len(), 2);
        assert_eq!(parent_styles.grid_column_line_names.len(), 3);
        assert_eq!(parent_styles.grid_template_rows.len(), 2);
        assert_eq!(parent_styles.grid_row_line_names.len(), 3);
        assert_eq!(parent_styles.grid_template_areas.len(), 2);

        let child_styles = &styled.children[0].styles;
        assert!(child_styles.grid_template_columns.is_empty());
        assert!(child_styles.grid_template_rows.is_empty());
        assert!(child_styles.grid_template_areas.is_empty());
        assert!(child_styles.grid_column_line_names.is_empty());
        assert!(child_styles.grid_row_line_names.is_empty());
        assert!(child_styles.grid_column_names.is_empty());
        assert!(child_styles.grid_row_names.is_empty());
    }

    #[test]
    fn image_rendering_does_not_inherit() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "image-rendering: pixelated;".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        assert!(matches!(
            styled.styles.image_rendering,
            crate::style::types::ImageRendering::Pixelated
        ));
        assert!(matches!(
            styled.children[0].styles.image_rendering,
            crate::style::types::ImageRendering::Auto
        ));
    }

    #[test]
    fn text_decoration_none_breaks_propagation() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![("style".to_string(), "text-decoration: none;".to_string())],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "text-decoration: underline;".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert!(child_styles.applied_text_decorations.is_empty());
    }

    #[test]
    fn text_decoration_adds_to_parent_decoration() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "span".to_string(),
                attributes: vec![("style".to_string(), "text-decoration: overline;".to_string())],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "text-decoration: underline;".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child_styles = &styled.children[0].styles;
        assert_eq!(child_styles.applied_text_decorations.len(), 2);
        assert!(child_styles.applied_text_decorations[0]
            .decoration
            .lines
            .contains(TextDecorationLine::UNDERLINE));
        assert!(child_styles.applied_text_decorations[1]
            .decoration
            .lines
            .contains(TextDecorationLine::OVERLINE));
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
                text-decoration: underline;
                text-indent: 40px;
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
        assert_eq!(marker.text_transform, crate::style::types::TextTransform::none());
        assert!(marker.text_decoration.lines == crate::style::types::TextDecorationLine::NONE);
        assert!(matches!(marker.text_align, crate::style::types::TextAlign::Start));
        assert_eq!(marker.text_indent, crate::style::types::TextIndent::default());
        assert!(matches!(marker.float, Float::None));
        assert!(marker.transform.is_empty());
        assert_eq!(marker.opacity, 1.0);
    }

    #[test]
    fn marker_style_inherits_from_list_item_not_parent() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "color: blue;".to_string())],
            },
            children: vec![DomNode {
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
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let ul = styled.children.first().expect("ul");
        let li = ul.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");
        assert_eq!(
            marker.color,
            Rgba::RED,
            "marker should inherit color from list item, not its parent"
        );
    }

    #[test]
    fn marker_rule_cannot_override_list_style_type_or_image() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![("style".to_string(), "list-style-type: decimal;".to_string())],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                list-style-type: square;
                list-style-image: url(bullet.png);
            }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");
        // list-style declarations on ::marker are ignored; marker retains the list-item value.
        assert!(
            matches!(marker.list_style_type, ListStyleType::Decimal),
            "marker should ignore list-style-type on ::marker"
        );
        assert!(
            matches!(marker.list_style_image, crate::style::types::ListStyleImage::None),
            "marker should ignore list-style-image on ::marker"
        );
    }

    #[test]
    fn marker_allows_text_combine_and_typography_but_ignores_non_text_box_properties() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![("style".to_string(), "letter-spacing: 0px;".to_string())],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                text-combine-upright: all;
                letter-spacing: 2px;
                visibility: hidden;
            }
        "#,
        )
        .unwrap();

        let media_ctx = MediaContext::screen(1200.0, 800.0);
        let rule_refs: Vec<&StyleRule> = stylesheet.collect_style_rules(&media_ctx);
        assert_eq!(rule_refs.len(), 1, "should collect the authored li::marker rule");
        let ancestors: Vec<&DomNode> = vec![&dom]; // ul ancestor for the li
        let element_ref = build_element_ref_chain(&dom.children[0], &ancestors);
        let mut caches = SelectorCaches::default();
        let mut context = MatchingContext::new(
            MatchingMode::ForStatelessPseudoElement,
            None,
            &mut caches,
            QuirksMode::NoQuirks,
            selectors::matching::NeedsSelectorFlags::No,
            selectors::matching::MatchingForInvalidation::No,
        );
        let selector = rule_refs[0]
            .selectors
            .slice()
            .first()
            .expect("selector in rule");
        assert!(
            matches_selector(selector, 0, None, &element_ref, &mut context),
            "selector should match the originating element"
        );
        let marker_matches =
            find_pseudo_element_rules(&dom.children[0], &rule_refs, &ancestors, &PseudoElement::Marker);
        assert_eq!(marker_matches.len(), 1, "marker rules should match li::marker");

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert!(
            marker_allows_property("text-combine-upright"),
            "filter should allow text-combine-upright"
        );
        assert!(
            (marker.letter_spacing - 2.0).abs() < f32::EPSILON,
            "letter-spacing should apply to marker contents (got {})",
            marker.letter_spacing
        );
        assert_eq!(
            marker.text_combine_upright,
            TextCombineUpright::All,
            "text-combine-upright should be honored on ::marker"
        );
        assert!(
            matches!(marker.visibility, Visibility::Visible),
            "non-text properties like visibility should be ignored for marker boxes"
        );
    }

    #[test]
    fn marker_ua_defaults_match_css_lists() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert!(
            matches!(marker.unicode_bidi, UnicodeBidi::Isolate),
            "marker should default unicode-bidi to isolate"
        );
        assert!(
            matches!(marker.white_space, WhiteSpace::Pre),
            "marker should default white-space to pre"
        );
        assert_eq!(
            marker.font_variant_numeric.spacing,
            crate::style::types::NumericSpacing::Tabular,
            "marker should default to tabular numbers"
        );
        assert_eq!(
            marker.text_transform,
            crate::style::types::TextTransform::none(),
            "marker should default text-transform to none"
        );
    }

    #[test]
    fn marker_rejects_alignment_and_indent_declarations() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                text-align: center;
                text-indent: 40px;
            }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert!(
            matches!(marker.text_align, crate::style::types::TextAlign::Start),
            "text-align should be ignored on ::marker"
        );
        assert_eq!(
            marker.text_indent,
            crate::style::types::TextIndent::default(),
            "text-indent should be ignored on ::marker"
        );
    }

    #[test]
    fn font_weight_relative_keywords_follow_css_fonts_table() {
        assert_eq!(child_font_weight("font-weight: 50;", "font-weight: bolder;"), 400);
        assert_eq!(child_font_weight("font-weight: 50;", "font-weight: lighter;"), 50);

        assert_eq!(child_font_weight("font-weight: 500;", "font-weight: bolder;"), 700);
        assert_eq!(child_font_weight("font-weight: 500;", "font-weight: lighter;"), 100);

        assert_eq!(child_font_weight("font-weight: 650;", "font-weight: bolder;"), 900);
        assert_eq!(child_font_weight("font-weight: 650;", "font-weight: lighter;"), 400);

        assert_eq!(child_font_weight("font-weight: 800;", "font-weight: bolder;"), 900);
        assert_eq!(child_font_weight("font-weight: 800;", "font-weight: lighter;"), 700);

        assert_eq!(child_font_weight("font-weight: 950;", "font-weight: bolder;"), 950);
        assert_eq!(child_font_weight("font-weight: 950;", "font-weight: lighter;"), 700);
    }

    #[test]
    fn out_of_range_font_weight_is_ignored() {
        let dom = element_with_style("font-weight: 1200;");
        let styled = apply_styles(&dom, &StyleSheet::new());
        assert_eq!(styled.styles.font_weight.to_u16(), 400);
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
        MatchingMode::ForStatelessPseudoElement,
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
                    let result = matches_selector(selector, 0, None, &element_ref, &mut context);
                    if result {
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

fn resolve_relative_font_weight(styles: &mut ComputedStyle, parent: &ComputedStyle) {
    let parent_weight = parent.font_weight.to_u16();
    styles.font_weight = styles.font_weight.resolve_relative(parent_weight);
}

fn propagate_text_decorations(styles: &mut ComputedStyle, parent: &ComputedStyle) {
    styles.applied_text_decorations = parent.applied_text_decorations.clone();
    if styles.text_decoration_line_specified && styles.text_decoration.lines.is_empty() {
        styles.applied_text_decorations.clear();
    }
    if !styles.text_decoration.lines.is_empty() {
        styles
            .applied_text_decorations
            .push(crate::style::types::ResolvedTextDecoration {
                decoration: styles.text_decoration.clone(),
                skip_ink: styles.text_decoration_skip_ink,
                underline_offset: styles.text_underline_offset,
                underline_position: styles.text_underline_position,
            });
    }
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
    resolve_relative_font_weight(&mut styles, parent_styles);
    propagate_text_decorations(&mut styles, parent_styles);

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
    list_item_styles: &ComputedStyle,
    root_font_size: f32,
) -> Option<ComputedStyle> {
    if list_item_styles.display != Display::ListItem {
        return None;
    }

    let matching_rules = find_pseudo_element_rules(node, rules, ancestors, &PseudoElement::Marker);

    let mut styles = ComputedStyle::default();
    styles.display = Display::Inline;
    inherit_styles(&mut styles, list_item_styles);

    for (_specificity, declarations) in matching_rules {
        for decl in declarations {
            if marker_allows_property(&decl.property) {
                apply_declaration(&mut styles, &decl, list_item_styles.font_size, root_font_size);
            }
        }
    }
    resolve_relative_font_weight(&mut styles, list_item_styles);
    propagate_text_decorations(&mut styles, list_item_styles);

    reset_marker_box_properties(&mut styles);
    styles.unicode_bidi = crate::style::types::UnicodeBidi::Isolate;
    styles.font_variant_numeric.spacing = crate::style::types::NumericSpacing::Tabular;
    styles.white_space = crate::style::types::WhiteSpace::Pre;
    styles.text_transform = crate::style::types::TextTransform::none();
    styles.display = Display::Inline;
    Some(styles)
}

pub(crate) fn reset_marker_box_properties(styles: &mut ComputedStyle) {
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

fn marker_allows_property(property: &str) -> bool {
    let p = property.to_ascii_lowercase();

    // Marker boxes honor a limited set of box-level properties
    // (CSS Lists 3 § 3.1.1).
    if matches!(p.as_str(), "content" | "direction" | "unicode-bidi" | "text-combine-upright") {
        return true;
    }

    // Animations/transitions are explicitly permitted.
    if p.starts_with("animation") || p.starts_with("transition") {
        return true;
    }

    // Inheritable text-affecting properties apply to the marker's contents.
    if p.starts_with("font-") {
        return true;
    }

    matches!(
        p.as_str(),
        "color"
            | "white-space"
            | "line-height"
            | "letter-spacing"
            | "word-spacing"
            | "text-transform"
            | "text-emphasis"
            | "text-emphasis-style"
            | "text-emphasis-color"
            | "text-emphasis-position"
            | "text-decoration-skip-ink"
            | "text-underline-position"
            | "text-underline-offset"
            | "text-decoration"
            | "text-decoration-line"
            | "text-decoration-style"
            | "text-decoration-color"
            | "text-decoration-thickness"
            | "text-shadow"
            | "line-break"
    )
}
