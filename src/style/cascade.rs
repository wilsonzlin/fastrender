//! CSS Cascade and Style Application
//!
//! This module handles applying CSS rules to DOM nodes to produce
//! computed styles. It implements the CSS cascade algorithm.
//!
//! Reference: CSS Cascading and Inheritance Level 4
//! <https://www.w3.org/TR/css-cascade-4/>

use crate::css::parser::{parse_declarations, parse_stylesheet};
use crate::css::selectors::{PseudoElement, TextDirection};
use crate::css::types::CssImportLoader;
use crate::css::types::{Declaration, StyleRule, StyleSheet};
use crate::dom::{resolve_first_strong_direction, with_target_fragment, DomNode, ElementRef};
use crate::style::defaults::{get_default_styles_for_element, parse_color_attribute, parse_dimension_attribute};
use crate::style::display::Display;
use crate::style::grid::finalize_grid_placement;
use crate::style::media::MediaContext;
use crate::style::properties::{apply_declaration, resolve_pending_logical_properties, with_image_set_dpr};
use crate::style::{normalize_language_tag, ComputedStyle};
use selectors::context::{QuirksMode, SelectorCaches};
use selectors::matching::{matches_selector, MatchingContext, MatchingMode};

/// User-agent stylesheet containing default browser styles
const USER_AGENT_STYLESHEET: &str = include_str!("../user_agent.css");

/// The origin of a style rule for cascade ordering.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum StyleOrigin {
    UserAgent,
    Author,
    Inline,
}

impl StyleOrigin {
    fn rank(self) -> u8 {
        match self {
            StyleOrigin::UserAgent => 0,
            StyleOrigin::Author | StyleOrigin::Inline => 1,
        }
    }
}

/// A style rule annotated with its origin and document order.
#[derive(Copy, Clone)]
struct CascadeRule<'a> {
    origin: StyleOrigin,
    order: usize,
    rule: &'a StyleRule,
}

/// A matched rule with the specificity of the selector that applied.
struct MatchedRule {
    origin: StyleOrigin,
    specificity: u32,
    order: usize,
    declarations: Vec<Declaration>,
}

/// A single declaration annotated with cascade ordering keys.
struct MatchedDeclaration {
    important: bool,
    origin: StyleOrigin,
    specificity: u32,
    rule_order: usize,
    decl_order: usize,
    declaration: Declaration,
}

const INLINE_SPECIFICITY: u32 = 1 << 30;
const INLINE_RULE_ORDER: usize = usize::MAX / 2;

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
    apply_styles_with_target(dom, stylesheet, None)
}

/// Apply styles with an explicit target fragment for :target matching.
pub fn apply_styles_with_target(dom: &DomNode, stylesheet: &StyleSheet, target_fragment: Option<&str>) -> StyledNode {
    // Use default desktop viewport
    let media_ctx = MediaContext::screen(1200.0, 800.0);
    apply_styles_with_media_and_target(dom, stylesheet, &media_ctx, target_fragment)
}

/// Apply styles to a DOM tree with a specific media context
///
/// This allows controlling viewport size and other media features
/// for responsive rendering.
pub fn apply_styles_with_media(dom: &DomNode, stylesheet: &StyleSheet, media_ctx: &MediaContext) -> StyledNode {
    apply_styles_with_media_and_target(dom, stylesheet, media_ctx, None)
}

pub fn apply_styles_with_media_and_target(
    dom: &DomNode,
    stylesheet: &StyleSheet,
    media_ctx: &MediaContext,
    target_fragment: Option<&str>,
) -> StyledNode {
    apply_styles_with_media_target_and_imports(dom, stylesheet, media_ctx, target_fragment, None, None::<&str>)
}

/// Apply styles with media context, optional :target, and optional import loader/base URL.
pub fn apply_styles_with_media_target_and_imports(
    dom: &DomNode,
    stylesheet: &StyleSheet,
    media_ctx: &MediaContext,
    target_fragment: Option<&str>,
    import_loader: Option<&dyn CssImportLoader>,
    base_url: Option<&str>,
) -> StyledNode {
    // Parse user-agent stylesheet
    let ua_stylesheet = parse_stylesheet(USER_AGENT_STYLESHEET).unwrap_or_else(|_| StyleSheet::new());

    // Resolve imports if a loader is provided
    let author_sheet = if let Some(loader) = import_loader {
        stylesheet.resolve_imports(loader, base_url, media_ctx)
    } else {
        stylesheet.clone()
    };

    // Collect applicable rules from both stylesheets
    // User-agent rules come first (lower priority)
    let ua_rules = ua_stylesheet.collect_style_rules(media_ctx);
    let author_rules = author_sheet.collect_style_rules(media_ctx);

    let mut all_rules: Vec<CascadeRule<'_>> = Vec::with_capacity(ua_rules.len() + author_rules.len());
    for (order, rule) in ua_rules.iter().enumerate() {
        all_rules.push(CascadeRule {
            origin: StyleOrigin::UserAgent,
            order,
            rule,
        });
    }
    let offset = all_rules.len();
    for (idx, rule) in author_rules.iter().enumerate() {
        all_rules.push(CascadeRule {
            origin: StyleOrigin::Author,
            order: offset + idx,
            rule,
        });
    }

    with_target_fragment(target_fragment, || {
        with_image_set_dpr(media_ctx.device_pixel_ratio, || {
            apply_styles_internal(dom, &all_rules, &ComputedStyle::default(), 16.0)
        })
    })
}

fn apply_styles_internal(
    node: &DomNode,
    rules: &[CascadeRule<'_>],
    parent_styles: &ComputedStyle,
    root_font_size: f32,
) -> StyledNode {
    let mut styles = get_default_styles_for_element(node);

    // Inherit styles from parent
    inherit_styles(&mut styles, parent_styles);

    // Apply matching CSS rules and inline styles with full cascade ordering
    let ancestors: Vec<&DomNode> = vec![];
    let mut matching_rules = find_matching_rules(node, rules, &ancestors);
    if let Some(presentational_rule) = dir_presentational_hint(node, 0) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = list_type_presentational_hint(node, 1) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = alignment_presentational_hint(node, 2) {
        matching_rules.push(presentational_rule);
    }
    let inline_decls = node.get_attribute("style").as_deref().map(parse_declarations);
    apply_cascaded_declarations(
        &mut styles,
        matching_rules,
        inline_decls,
        parent_styles.font_size,
        root_font_size,
        |_| true,
    );

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
    resolve_match_parent_text_align(&mut styles, parent_styles, ancestors.is_empty());
    resolve_match_parent_text_align_last(&mut styles, parent_styles, ancestors.is_empty());
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
    rules: &[CascadeRule<'_>],
    parent_styles: &ComputedStyle,
    root_font_size: f32,
    ancestors: &[&DomNode],
) -> StyledNode {
    let mut styles = get_default_styles_for_element(node);

    // Inherit styles from parent
    inherit_styles(&mut styles, parent_styles);

    // Apply matching CSS rules and inline styles with full cascade ordering
    let mut matching_rules = find_matching_rules(node, rules, ancestors);
    if let Some(presentational_rule) = dir_presentational_hint(node, 0) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = list_type_presentational_hint(node, 1) {
        matching_rules.push(presentational_rule);
    }
    if let Some(presentational_rule) = alignment_presentational_hint(node, 2) {
        matching_rules.push(presentational_rule);
    }
    let inline_decls = node.get_attribute("style").as_deref().map(parse_declarations);
    apply_cascaded_declarations(
        &mut styles,
        matching_rules,
        inline_decls,
        parent_styles.font_size,
        root_font_size,
        |_| true,
    );

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
    resolve_match_parent_text_align(&mut styles, parent_styles, ancestors.is_empty());
    resolve_match_parent_text_align_last(&mut styles, parent_styles, ancestors.is_empty());
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
    // Reset cascade bookkeeping for the new style; logical pending state should not inherit.
    styles.logical.reset();
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
    styles.font_optical_sizing = parent.font_optical_sizing;
    styles.font_variation_settings = parent.font_variation_settings.clone();
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
    styles.quotes = parent.quotes.clone();
    styles.cursor = parent.cursor;
    styles.cursor_images = parent.cursor_images.clone();

    // Color inherits
    styles.color = parent.color;

    // CSS Custom Properties inherit
    styles.custom_properties = parent.custom_properties.clone();
}

fn resolve_match_parent_text_align(styles: &mut ComputedStyle, parent: &ComputedStyle, is_root: bool) {
    use crate::style::types::TextAlign;
    if !matches!(styles.text_align, TextAlign::MatchParent) {
        return;
    }
    if is_root {
        styles.text_align = TextAlign::Start;
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

fn resolve_match_parent_text_align_last(styles: &mut ComputedStyle, parent: &ComputedStyle, is_root: bool) {
    use crate::style::types::TextAlignLast;
    if !matches!(styles.text_align_last, TextAlignLast::MatchParent) {
        return;
    }
    if is_root {
        styles.text_align_last = TextAlignLast::Start;
        return;
    }
    let inherited = match parent.text_align_last {
        TextAlignLast::MatchParent => {
            if matches!(parent.direction, crate::style::types::Direction::Rtl) {
                TextAlignLast::End
            } else {
                TextAlignLast::Start
            }
        }
        other => other,
    };
    styles.text_align_last = match inherited {
        TextAlignLast::Start => {
            if matches!(parent.direction, crate::style::types::Direction::Rtl) {
                TextAlignLast::Right
            } else {
                TextAlignLast::Left
            }
        }
        TextAlignLast::End => {
            if matches!(parent.direction, crate::style::types::Direction::Rtl) {
                TextAlignLast::Left
            } else {
                TextAlignLast::Right
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
    use crate::css::types::CssImportLoader;
    use crate::css::types::StyleSheet;
    use crate::dom::DomNodeType;
    use crate::style::color::Rgba;
    use crate::style::computed::Visibility;
    use crate::style::display::Display;
    use crate::style::float::Float;
    use crate::style::types::{
        LineBreak, ListStylePosition, ListStyleType, TextCombineUpright, TextDecorationLine, TextUnderlineOffset,
        TextUnderlinePosition, UnicodeBidi, WhiteSpace, WillChange, WillChangeHint,
    };
    use crate::style::CursorKeyword;

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

    fn element_with_id_and_class(id: &str, class: &str, style: Option<&str>) -> DomNode {
        let mut attributes = vec![
            ("id".to_string(), id.to_string()),
            ("class".to_string(), class.to_string()),
        ];
        if let Some(style) = style {
            attributes.push(("style".to_string(), style.to_string()));
        }
        DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes,
            },
            children: vec![],
        }
    }

    #[test]
    fn important_overrides_more_specific_normal_declarations() {
        let dom = element_with_id_and_class("target", "item", None);
        let stylesheet = parse_stylesheet(
            r#"
            #target { color: red; }
            .item { color: blue !important; }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.color, Rgba::rgb(0, 0, 255));
    }

    #[test]
    fn author_important_beats_inline_normal() {
        let dom = element_with_id_and_class("target", "item", Some("color: green;"));
        let stylesheet = parse_stylesheet(".item { color: red !important; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.color, Rgba::rgb(255, 0, 0));
    }

    #[test]
    fn inline_important_outranks_author_important_through_specificity() {
        let dom = element_with_id_and_class("target", "item", Some("color: green !important;"));
        let stylesheet = parse_stylesheet(".item { color: blue !important; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert_eq!(styled.styles.color, Rgba::rgb(0, 128, 0));
    }

    #[test]
    fn font_variation_settings_inherit() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), r#"font-variation-settings: "wght" 600;"#.to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert_eq!(child.styles.font_variation_settings.len(), 1);
        assert_eq!(child.styles.font_variation_settings[0].tag, *b"wght");
        assert!((child.styles.font_variation_settings[0].value - 600.0).abs() < 0.001);
    }

    #[test]
    fn imports_are_resolved_when_loader_present() {
        struct Loader;
        impl CssImportLoader for Loader {
            fn load(&self, _url: &str) -> crate::error::Result<String> {
                Ok("#target { color: rgb(1, 2, 3); }".to_string())
            }
        }

        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("id".to_string(), "target".to_string())],
            },
            children: vec![],
        };
        let stylesheet = parse_stylesheet(r#"@import url("import.css");"#).unwrap();
        let media_ctx = MediaContext::screen(800.0, 600.0);
        let styled = apply_styles_with_media_target_and_imports(
            &dom,
            &stylesheet,
            &media_ctx,
            None,
            Some(&Loader),
            Some("https://example.com/page.css"),
        );
        assert_eq!(styled.styles.color, Rgba::rgb(1, 2, 3));
    }

    #[test]
    fn dir_attribute_sets_direction_and_isolate() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("dir".to_string(), "rtl".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(styled.styles.direction, crate::style::types::Direction::Rtl));
        assert!(matches!(
            styled.styles.unicode_bidi,
            crate::style::types::UnicodeBidi::Isolate
        ));
    }

    #[test]
    fn author_css_overrides_presentational_dir() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("dir".to_string(), "rtl".to_string())],
            },
            children: vec![],
        };

        let stylesheet = parse_stylesheet("div { direction: ltr; unicode-bidi: normal; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert!(matches!(styled.styles.direction, crate::style::types::Direction::Ltr));
        assert!(matches!(
            styled.styles.unicode_bidi,
            crate::style::types::UnicodeBidi::Normal
        ));
    }

    #[test]
    fn image_set_selection_uses_media_context_dpr() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let stylesheet =
            parse_stylesheet("div { background-image: image-set(url(\"lo.png\") 1x, url(\"hi.png\") 2x); }").unwrap();
        let media = MediaContext::screen(800.0, 600.0).with_device_pixel_ratio(2.0);
        let styled = apply_styles_with_media(&dom, &stylesheet, &media);

        assert!(matches!(
            styled.styles.background_layers.get(0).and_then(|l| l.image.as_ref()),
            Some(crate::style::types::BackgroundImage::Url(url)) if url == "hi.png"
        ));
    }

    #[test]
    fn inline_style_overrides_presentational_dir() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![
                    ("dir".to_string(), "rtl".to_string()),
                    ("style".to_string(), "direction: ltr;".to_string()),
                ],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(styled.styles.direction, crate::style::types::Direction::Ltr));
    }

    #[test]
    fn cursor_inherits_and_can_be_overridden() {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "cursor: move;".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert_eq!(child.styles.cursor, CursorKeyword::Move);

        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("style".to_string(), "cursor: move;".to_string())],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![("style".to_string(), "cursor: pointer;".to_string())],
                },
                children: vec![],
            }],
        };
        let styled = apply_styles(&parent, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert_eq!(child.styles.cursor, CursorKeyword::Pointer);
    }

    #[test]
    fn ol_type_attribute_sets_list_style_type() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ol".to_string(),
                attributes: vec![("type".to_string(), "A".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.list_style_type,
            crate::style::types::ListStyleType::UpperAlpha
        ));
    }

    #[test]
    fn li_type_attribute_overrides_parent_style() {
        let child = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "li".to_string(),
                attributes: vec![("type".to_string(), "i".to_string())],
            },
            children: vec![],
        };
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ol".to_string(),
                attributes: vec![("type".to_string(), "A".to_string())],
            },
            children: vec![child],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let li = styled.children.first().expect("li");
        assert!(matches!(
            li.styles.list_style_type,
            crate::style::types::ListStyleType::LowerRoman
        ));
    }

    #[test]
    fn ul_type_attribute_uses_presentational_hint() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![("type".to_string(), "circle".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.list_style_type,
            crate::style::types::ListStyleType::Circle
        ));
    }

    #[test]
    fn author_css_overrides_type_presentational_hint() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ol".to_string(),
                attributes: vec![("type".to_string(), "I".to_string())],
            },
            children: vec![],
        };

        let stylesheet = parse_stylesheet("ol { list-style-type: lower-alpha; }").unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        assert!(matches!(
            styled.styles.list_style_type,
            crate::style::types::ListStyleType::LowerAlpha
        ));
    }

    #[test]
    fn align_attribute_sets_text_align_on_table_cells() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "td".to_string(),
                attributes: vec![("align".to_string(), "right".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align,
            crate::style::types::TextAlign::Right
        ));
    }

    #[test]
    fn valign_attribute_sets_vertical_align_on_cells() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "td".to_string(),
                attributes: vec![("valign".to_string(), "middle".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.vertical_align,
            crate::style::types::VerticalAlign::Middle
        ));
    }

    #[test]
    fn css_overrides_align_presentational_hint() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "td".to_string(),
                attributes: vec![
                    ("align".to_string(), "center".to_string()),
                    ("style".to_string(), "text-align: left;".to_string()),
                ],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(styled.styles.text_align, crate::style::types::TextAlign::Left));
    }

    #[test]
    fn align_attribute_maps_on_block_elements() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![("align".to_string(), "center".to_string())],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align,
            crate::style::types::TextAlign::Center
        ));
    }

    #[test]
    fn center_element_defaults_to_center_alignment() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "center".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align,
            crate::style::types::TextAlign::Center
        ));
    }

    #[test]
    fn table_cells_wrap_by_default() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "td".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.white_space,
            crate::style::types::WhiteSpace::Normal
        ));
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
    fn will_change_parses_hint_list() {
        let dom = element_with_style("will-change: transform, opacity");
        let styled = apply_styles(&dom, &StyleSheet::new());
        match &styled.styles.will_change {
            WillChange::Hints(hints) => {
                assert_eq!(
                    hints,
                    &vec![
                        WillChangeHint::Property("transform".to_string()),
                        WillChangeHint::Property("opacity".to_string())
                    ]
                );
            }
            other => panic!("expected hints, got {other:?}"),
        }
    }

    #[test]
    fn will_change_accepts_auto_and_rejects_invalid() {
        let auto = element_with_style("will-change: auto");
        let styled_auto = apply_styles(&auto, &StyleSheet::new());
        assert!(matches!(styled_auto.styles.will_change, WillChange::Auto));

        let invalid = element_with_style("will-change: auto, transform");
        let styled_invalid = apply_styles(&invalid, &StyleSheet::new());
        assert!(matches!(styled_invalid.styles.will_change, WillChange::Auto));
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
    fn text_align_last_match_parent_maps_start_end_using_parent_direction() {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "direction: rtl; text-align-last: start;".to_string(),
                )],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "span".to_string(),
                    attributes: vec![("style".to_string(), "text-align-last: match-parent;".to_string())],
                },
                children: vec![],
            }],
        };

        let styled = apply_styles(&parent, &StyleSheet::new());
        let child = styled.children.first().expect("child");
        assert!(matches!(
            child.styles.text_align_last,
            crate::style::types::TextAlignLast::Right
        ));
    }

    #[test]
    fn text_align_match_parent_sets_last_to_match_parent_and_resolves() {
        let parent = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "div".to_string(),
                attributes: vec![(
                    "style".to_string(),
                    "direction: rtl; text-align-last: start;".to_string(),
                )],
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
        assert!(matches!(
            child.styles.text_align_last,
            crate::style::types::TextAlignLast::Right
        ));
    }

    #[test]
    fn root_match_parent_text_align_last_computes_to_start() {
        let dom = element_with_style("direction: rtl; text-align-last: match-parent;");
        let styled = apply_styles(&dom, &StyleSheet::new());
        assert!(matches!(
            styled.styles.text_align_last,
            crate::style::types::TextAlignLast::Start
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
        let rule_refs: Vec<CascadeRule<'_>> = stylesheet
            .collect_style_rules(&media_ctx)
            .into_iter()
            .enumerate()
            .map(|(order, rule)| CascadeRule {
                origin: StyleOrigin::Author,
                order,
                rule,
            })
            .collect();
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
        let selector = rule_refs[0].rule.selectors.slice().first().expect("selector in rule");
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

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                text-decoration: underline;
                text-shadow: 1px 1px red;
            }
        "#,
        )
        .unwrap();
        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert!(
            marker.applied_text_decorations.is_empty(),
            "non-inherited text decorations should not be applied to ::marker"
        );
        assert!(
            marker.text_shadow.is_empty(),
            "text-shadow should not apply to ::marker"
        );
    }

    #[test]
    fn marker_author_overrides_ua_defaults() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![("style".to_string(), "color: black;".to_string())],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                white-space: normal;
                unicode-bidi: normal;
                text-transform: uppercase;
            }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert!(
            matches!(marker.white_space, WhiteSpace::Normal),
            "author white-space should override UA default"
        );
        assert!(
            matches!(marker.unicode_bidi, UnicodeBidi::Normal),
            "author unicode-bidi should override UA default"
        );
        assert_eq!(
            marker.text_transform,
            crate::style::types::TextTransform::with_case(crate::style::types::CaseTransform::Uppercase),
            "author text-transform should override UA default"
        );
    }

    #[test]
    fn marker_allows_text_emphasis_properties() {
        let dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "ul".to_string(),
                attributes: vec![],
            },
            children: vec![DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![("style".to_string(), "color: black;".to_string())],
                },
                children: vec![],
            }],
        };

        let stylesheet = parse_stylesheet(
            r#"
            li::marker {
                text-emphasis-style: open dot;
                text-emphasis-color: red;
                text-emphasis-position: under right;
            }
        "#,
        )
        .unwrap();

        let styled = apply_styles(&dom, &stylesheet);
        let li = styled.children.first().expect("li");
        let marker = li.marker_styles.as_ref().expect("marker styles");

        assert_eq!(
            marker.text_emphasis_style,
            crate::style::types::TextEmphasisStyle::Mark {
                fill: crate::style::types::TextEmphasisFill::Open,
                shape: crate::style::types::TextEmphasisShape::Dot
            },
            "text-emphasis-style should apply to ::marker"
        );
        assert_eq!(
            marker.text_emphasis_color,
            Some(Rgba::RED),
            "text-emphasis-color should apply to ::marker"
        );
        assert_eq!(
            marker.text_emphasis_position,
            crate::style::types::TextEmphasisPosition::UnderRight,
            "text-emphasis-position should apply to ::marker"
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

fn find_matching_rules(node: &DomNode, rules: &[CascadeRule<'_>], ancestors: &[&DomNode]) -> Vec<MatchedRule> {
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

    for rule in rules.iter() {
        // Check if any selector in the list matches
        let mut matched = false;
        let mut max_specificity = 0u32;

        for selector in rule.rule.selectors.slice().iter() {
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
            matches.push(MatchedRule {
                origin: rule.origin,
                specificity: max_specificity,
                order: rule.order,
                declarations: rule.rule.declarations.clone(),
            });
        }
    }

    // Sort by specificity (lower specificity first, so later rules override), then document order.
    matches.sort_by(|a, b| a.specificity.cmp(&b.specificity).then(a.order.cmp(&b.order)));

    matches
}

/// Find rules that match an element with a specific pseudo-element
fn find_pseudo_element_rules(
    node: &DomNode,
    rules: &[CascadeRule<'_>],
    ancestors: &[&DomNode],
    pseudo: &PseudoElement,
) -> Vec<MatchedRule> {
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

    for rule in rules.iter() {
        let mut matched = false;
        let mut max_specificity = 0u32;

        for selector in rule.rule.selectors.slice().iter() {
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
            matches.push(MatchedRule {
                origin: rule.origin,
                specificity: max_specificity,
                order: rule.order,
                declarations: rule.rule.declarations.clone(),
            });
        }
    }

    matches.sort_by(|a, b| a.specificity.cmp(&b.specificity).then(a.order.cmp(&b.order)));

    matches
}

fn apply_cascaded_declarations<F>(
    styles: &mut ComputedStyle,
    matched_rules: Vec<MatchedRule>,
    inline_declarations: Option<Vec<Declaration>>,
    parent_font_size: f32,
    root_font_size: f32,
    filter: F,
) where
    F: Fn(&Declaration) -> bool,
{
    let mut flattened: Vec<MatchedDeclaration> = Vec::new();

    for rule in matched_rules {
        for (decl_order, declaration) in rule.declarations.into_iter().enumerate() {
            flattened.push(MatchedDeclaration {
                important: declaration.important,
                origin: rule.origin,
                specificity: rule.specificity,
                rule_order: rule.order,
                decl_order,
                declaration,
            });
        }
    }

    if let Some(inline) = inline_declarations {
        for (decl_order, declaration) in inline.into_iter().enumerate() {
            flattened.push(MatchedDeclaration {
                important: declaration.important,
                origin: StyleOrigin::Inline,
                specificity: INLINE_SPECIFICITY,
                rule_order: INLINE_RULE_ORDER,
                decl_order,
                declaration,
            });
        }
    }

    flattened.sort_by(|a, b| {
        a.important
            .cmp(&b.important)
            .then(a.origin.rank().cmp(&b.origin.rank()))
            .then(a.specificity.cmp(&b.specificity))
            .then(a.rule_order.cmp(&b.rule_order))
            .then(a.decl_order.cmp(&b.decl_order))
    });

    for entry in flattened {
        if filter(&entry.declaration) {
            apply_declaration(styles, &entry.declaration, parent_font_size, root_font_size);
        }
    }
    resolve_pending_logical_properties(styles);
}

fn dir_presentational_hint(node: &DomNode, order: usize) -> Option<MatchedRule> {
    let dir = node.get_attribute("dir")?;
    let dir = dir.trim().to_ascii_lowercase();
    match dir.as_str() {
        "ltr" | "rtl" => {
            let css = format!("direction: {}; unicode-bidi: isolate;", dir);
            let declarations = parse_declarations(&css);
            Some(MatchedRule {
                origin: StyleOrigin::Author,
                specificity: 0,
                order,
                declarations,
            })
        }
        "auto" => {
            let resolved = resolve_first_strong_direction(node).map(|d| match d {
                TextDirection::Ltr => crate::style::types::Direction::Ltr,
                TextDirection::Rtl => crate::style::types::Direction::Rtl,
            });
            let dir_value = match resolved.unwrap_or(crate::style::types::Direction::Ltr) {
                crate::style::types::Direction::Rtl => "rtl",
                crate::style::types::Direction::Ltr => "ltr",
            };
            let declarations = parse_declarations(&format!("direction: {}; unicode-bidi: isolate;", dir_value));
            Some(MatchedRule {
                origin: StyleOrigin::Author,
                specificity: 0,
                order,
                declarations,
            })
        }
        _ => None,
    }
}

fn list_type_presentational_hint(node: &DomNode, order: usize) -> Option<MatchedRule> {
    let tag = node.tag_name()?.to_ascii_lowercase();
    let ty = node.get_attribute("type")?;
    let mapped = match tag.as_str() {
        "ol" | "li" => map_ol_type(&ty),
        "ul" => map_ul_type(&ty),
        _ => None,
    }?;
    let declarations = parse_declarations(&format!("list-style-type: {};", mapped));
    Some(MatchedRule {
        origin: StyleOrigin::Author,
        specificity: 0,
        order,
        declarations,
    })
}

fn map_ol_type(value: &str) -> Option<&'static str> {
    match value.trim() {
        "1" => Some("decimal"),
        "a" => Some("lower-alpha"),
        "A" => Some("upper-alpha"),
        "i" => Some("lower-roman"),
        "I" => Some("upper-roman"),
        _ => None,
    }
}

fn map_ul_type(value: &str) -> Option<&'static str> {
    match value.trim().to_ascii_lowercase().as_str() {
        "disc" => Some("disc"),
        "circle" => Some("circle"),
        "square" => Some("square"),
        _ => None,
    }
}

fn alignment_presentational_hint(node: &DomNode, order: usize) -> Option<MatchedRule> {
    let tag = node.tag_name()?.to_ascii_lowercase();
    let mut declarations = String::new();

    if let Some(align) = node
        .get_attribute("align")
        .or_else(|| (tag == "center").then(|| "center".to_string()))
    {
        if let Some(mapped) = map_align(&align) {
            if matches!(
                tag.as_str(),
                "td" | "th" | "tr" | "table" | "div" | "p" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "center"
            ) {
                declarations.push_str(&format!("text-align: {};", mapped));
            }
        }
    }

    if matches!(tag.as_str(), "td" | "th" | "tr") {
        if let Some(valign) = node.get_attribute("valign") {
            if let Some(mapped) = map_valign(&valign) {
                declarations.push_str(&format!("vertical-align: {};", mapped));
            }
        }
    }

    if declarations.is_empty() {
        return None;
    }

    Some(MatchedRule {
        origin: StyleOrigin::Author,
        specificity: 0,
        order,
        declarations: parse_declarations(&declarations),
    })
}

fn map_align(value: &str) -> Option<&'static str> {
    match value.trim().to_ascii_lowercase().as_str() {
        "left" => Some("left"),
        "center" => Some("center"),
        "right" => Some("right"),
        "justify" => Some("justify"),
        _ => None,
    }
}

fn map_valign(value: &str) -> Option<&'static str> {
    match value.trim().to_ascii_lowercase().as_str() {
        "top" => Some("top"),
        "middle" => Some("middle"),
        "bottom" => Some("bottom"),
        "baseline" => Some("baseline"),
        _ => None,
    }
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
    rules: &[CascadeRule<'_>],
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
    apply_cascaded_declarations(
        &mut styles,
        matching_rules,
        None,
        parent_styles.font_size,
        root_font_size,
        |_| true,
    );
    resolve_match_parent_text_align(&mut styles, parent_styles, false);
    resolve_match_parent_text_align_last(&mut styles, parent_styles, false);
    resolve_relative_font_weight(&mut styles, parent_styles);
    propagate_text_decorations(&mut styles, parent_styles);

    // Check if content property generates content
    // Per CSS spec, ::before/::after only generate boxes if content is not 'none' or 'normal'
    if matches!(
        styles.content_value,
        crate::style::content::ContentValue::None | crate::style::content::ContentValue::Normal
    ) {
        return None;
    }

    Some(styles)
}

fn compute_marker_styles(
    node: &DomNode,
    rules: &[CascadeRule<'_>],
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

    // UA defaults per CSS Lists 3: isolate, tabular numbers, pre white-space, no transforms.
    styles.unicode_bidi = crate::style::types::UnicodeBidi::Isolate;
    styles.font_variant_numeric.spacing = crate::style::types::NumericSpacing::Tabular;
    styles.white_space = crate::style::types::WhiteSpace::Pre;
    styles.text_transform = crate::style::types::TextTransform::none();

    apply_cascaded_declarations(
        &mut styles,
        matching_rules,
        None,
        list_item_styles.font_size,
        root_font_size,
        |decl| marker_allows_property(&decl.property),
    );
    resolve_match_parent_text_align(&mut styles, list_item_styles, false);
    resolve_match_parent_text_align_last(&mut styles, list_item_styles, false);
    resolve_relative_font_weight(&mut styles, list_item_styles);
    propagate_text_decorations(&mut styles, list_item_styles);

    reset_marker_box_properties(&mut styles);
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
    styles.background_images = defaults.background_images.clone();
    styles.background_positions = defaults.background_positions.clone();
    styles.background_sizes = defaults.background_sizes.clone();
    styles.background_repeats = defaults.background_repeats.clone();
    styles.background_attachments = defaults.background_attachments.clone();
    styles.background_origins = defaults.background_origins.clone();
    styles.background_clips = defaults.background_clips.clone();
    styles.rebuild_background_layers();
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
    if matches!(
        p.as_str(),
        "content" | "direction" | "unicode-bidi" | "text-combine-upright"
    ) {
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
            | "line-break"
            | "word-break"
            | "overflow-wrap"
            | "hyphens"
            | "tab-size"
    )
}
