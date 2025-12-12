//! Box generation - transforms styled DOM into BoxTree
//!
//! Implements the CSS box generation algorithm that determines what boxes
//! are created from DOM elements.
//!
//! CSS Specification: CSS 2.1 Section 9.2 - Box Generation
//! <https://www.w3.org/TR/CSS21/visuren.html#box-gen>

use crate::dom::{DomNode, DomNodeType};
use crate::geometry::Size;
use crate::style::content::{ContentContext, ContentItem, ContentValue, CounterStyle};
use crate::style::counters::{CounterManager, CounterSet};
use crate::style::display::{Display, FormattingContextType};
use crate::style::types::{ListStyleType, TextTransform};
use crate::style::values::{Length, LengthUnit};
use crate::style::ComputedStyle;
use crate::tree::anonymous::AnonymousBoxCreator;
use crate::tree::box_tree::{
    BoxNode, BoxTree, BoxType, MarkerContent, ReplacedBox, ReplacedType, SizesEntry, SizesList, SrcsetCandidate,
    SrcsetDescriptor,
};
use crate::tree::debug::DebugInfo;
use cssparser::{Parser, ParserInput, Token};
use std::sync::Arc;

/// Simplified DOM node representation
///
/// This is a placeholder for the real DOM implementation.
/// In a real browser engine, this would be the actual DOM node.
///
/// # Note
///
/// This simplified version is used for:
/// - Testing box generation in isolation
/// - Wave 2 development (before real DOM exists)
/// - Documentation examples
///
/// In production, replace with actual DOM node types.
#[derive(Debug, Clone)]
pub struct DOMNode {
    /// Element tag name (None for text nodes)
    pub tag_name: Option<String>,

    /// Element ID attribute
    pub id: Option<String>,

    /// Element classes
    pub classes: Vec<String>,

    /// Computed style for this element
    pub style: Arc<ComputedStyle>,

    /// Text content (for text nodes)
    pub text: Option<String>,

    /// Child nodes
    pub children: Vec<DOMNode>,

    /// Intrinsic size for replaced elements (images, video, etc.)
    ///
    /// This is set when the element has known dimensions (e.g., from
    /// width/height attributes on <img> or natural dimensions).
    pub intrinsic_size: Option<Size>,

    /// Source URL for replaced elements (img src, video src, etc.)
    pub src: Option<String>,

    /// Alternative text for replaced elements (e.g., <img alt="...">)
    pub alt: Option<String>,

    /// Srcset candidates for replaced elements
    pub srcset: Option<String>,

    /// Poster image for video elements
    pub poster: Option<String>,
}

impl DOMNode {
    /// Creates a new element node
    pub fn new_element(tag_name: impl Into<String>, style: Arc<ComputedStyle>, children: Vec<DOMNode>) -> Self {
        Self {
            tag_name: Some(tag_name.into()),
            id: None,
            classes: Vec::new(),
            style,
            text: None,
            children,
            intrinsic_size: None,
            src: None,
            alt: None,
            srcset: None,
            poster: None,
        }
    }

    /// Creates a new text node
    pub fn new_text(text: impl Into<String>, style: Arc<ComputedStyle>) -> Self {
        Self {
            tag_name: None,
            id: None,
            classes: Vec::new(),
            style,
            text: Some(text.into()),
            children: Vec::new(),
            intrinsic_size: None,
            src: None,
            alt: None,
            srcset: None,
            poster: None,
        }
    }

    /// Creates a new replaced element (img, video, canvas, etc.)
    ///
    /// # Arguments
    ///
    /// * `tag_name` - Element tag (img, video, canvas, svg, iframe)
    /// * `style` - Computed style for the element
    /// * `src` - Source URL or data URI
    /// * `intrinsic_size` - Natural dimensions of the content (if known)
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::tree::box_generation::DOMNode;
    /// use fastrender::ComputedStyle;
    /// use fastrender::Size;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let img = DOMNode::new_replaced("img", style, "image.png", Some(Size::new(100.0, 50.0)));
    ///
    /// assert!(img.is_replaced_element());
    /// assert_eq!(img.intrinsic_size, Some(Size::new(100.0, 50.0)));
    /// ```
    pub fn new_replaced(
        tag_name: impl Into<String>,
        style: Arc<ComputedStyle>,
        src: impl Into<String>,
        intrinsic_size: Option<Size>,
    ) -> Self {
        Self {
            tag_name: Some(tag_name.into()),
            id: None,
            classes: Vec::new(),
            style,
            text: None,
            children: Vec::new(),
            intrinsic_size,
            src: Some(src.into()),
            alt: None,
            srcset: None,
            poster: None,
        }
    }

    /// Sets element ID (builder pattern)
    pub fn with_id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    /// Adds a class (builder pattern)
    pub fn with_class(mut self, class: impl Into<String>) -> Self {
        self.classes.push(class.into());
        self
    }

    /// Sets the alt text (builder pattern)
    pub fn with_alt(mut self, alt: impl Into<String>) -> Self {
        self.alt = Some(alt.into());
        self
    }

    /// Sets poster (builder pattern)
    pub fn with_poster(mut self, poster: impl Into<String>) -> Self {
        self.poster = Some(poster.into());
        self
    }

    /// Sets intrinsic size (builder pattern)
    ///
    /// Used for replaced elements to specify their natural dimensions.
    pub fn with_intrinsic_size(mut self, size: Size) -> Self {
        self.intrinsic_size = Some(size);
        self
    }

    /// Sets source URL (builder pattern)
    ///
    /// Used for replaced elements (img src, video src, etc.)
    pub fn with_src(mut self, src: impl Into<String>) -> Self {
        self.src = Some(src.into());
        self
    }

    /// Returns true if this is a text node
    pub fn is_text(&self) -> bool {
        self.text.is_some()
    }

    /// Returns true if this is an element node
    pub fn is_element(&self) -> bool {
        self.tag_name.is_some()
    }

    /// Returns true if this is a replaced element
    ///
    /// Replaced elements are elements whose content is outside the scope of
    /// the CSS formatting model. Examples include:
    /// - `<img>` - Images
    /// - `<video>` - Video
    /// - `<canvas>` - Canvas drawing surface
    /// - `<svg>` - SVG graphics
    /// - `<iframe>` - Nested browsing contexts
    /// - `<embed>` - External content
    /// - `<object>` - External resources
    ///
    /// # CSS 2.1 Section 3.1
    ///
    /// "An element whose content is outside the scope of the CSS formatting
    /// model, such as an image, embedded document, or applet."
    pub fn is_replaced_element(&self) -> bool {
        if let Some(tag) = &self.tag_name {
            matches!(
                tag.as_str(),
                "img" | "video" | "canvas" | "svg" | "iframe" | "embed" | "object" | "audio"
            )
        } else {
            false
        }
    }

    /// Gets the display value from computed style
    pub fn display(&self) -> Display {
        self.style.display
    }

    /// Computes the aspect ratio from intrinsic size
    ///
    /// Returns width / height, or None if intrinsic size is not set
    /// or height is zero.
    pub fn aspect_ratio(&self) -> Option<f32> {
        self.intrinsic_size.and_then(|size| {
            if size.height > 0.0 {
                Some(size.width / size.height)
            } else {
                None
            }
        })
    }

    /// Gets the replaced element type based on tag name
    ///
    /// Returns None if this is not a replaced element.
    pub fn replaced_type(&self) -> Option<ReplacedType> {
        if !self.is_replaced_element() {
            return None;
        }

        let tag = self.tag_name.as_ref()?;
        let src = self.src.clone().unwrap_or_default();
        let alt = self.alt.clone().filter(|s| !s.is_empty());
        let srcset = self.srcset.as_ref().map(|v| parse_srcset(v)).unwrap_or_default();
        let poster = self.poster.clone().filter(|s| !s.is_empty());

        match tag.as_str() {
            "img" => Some(ReplacedType::Image {
                src,
                alt,
                srcset,
                sizes: None,
            }),
            "video" => Some(ReplacedType::Video { src, poster }),
            "canvas" => Some(ReplacedType::Canvas),
            "svg" => Some(ReplacedType::Svg { content: src }),
            "iframe" => Some(ReplacedType::Iframe { src }),
            _ => None,
        }
    }
}

fn parse_srcset(attr: &str) -> Vec<SrcsetCandidate> {
    attr.split(',')
        .filter_map(|candidate| {
            let trimmed = candidate.trim();
            if trimmed.is_empty() {
                return None;
            }
            let mut parts = trimmed.split_whitespace();
            let url = parts.next()?.to_string();
            let mut descriptor: Option<SrcsetDescriptor> = None;

            for desc in parts {
                if descriptor.is_some() {
                    // Multiple descriptors are invalid; ignore this candidate.
                    return None;
                }
                let d = desc.trim();
                if let Some(raw) = d.strip_suffix('x') {
                    if let Ok(val) = raw.parse::<f32>() {
                        descriptor = Some(SrcsetDescriptor::Density(val));
                    }
                } else if let Some(raw) = d.strip_suffix("dppx") {
                    if let Ok(val) = raw.parse::<f32>() {
                        descriptor = Some(SrcsetDescriptor::Density(val));
                    }
                } else if let Some(raw) = d.strip_suffix('w') {
                    if let Ok(val) = raw.parse::<u32>() {
                        descriptor = Some(SrcsetDescriptor::Width(val));
                    }
                }
            }

            Some(SrcsetCandidate {
                url,
                descriptor: descriptor.unwrap_or(SrcsetDescriptor::Density(1.0)),
            })
        })
        .collect()
}

fn parse_sizes(attr: &str) -> Option<SizesList> {
    use crate::style::media::MediaQuery;

    let mut entries = Vec::new();
    for item in attr.split(',') {
        let trimmed = item.trim();
        if trimmed.is_empty() {
            continue;
        }
        let mut parts = trimmed.rsplitn(2, char::is_whitespace);
        let length_part = parts.next().map(str::trim);
        let media_part = parts.next().map(str::trim);
        let length = match length_part.and_then(parse_sizes_length) {
            Some(l) => l,
            None => continue,
        };

        let media = match media_part {
            Some(cond) if !cond.is_empty() => match MediaQuery::parse_list(cond) {
                Ok(list) => Some(list),
                Err(_) => None,
            },
            _ => None,
        };

        entries.push(SizesEntry { media, length });
    }

    if entries.is_empty() {
        None
    } else {
        Some(SizesList { entries })
    }
}

fn parse_sizes_length(value: &str) -> Option<Length> {
    use crate::css::properties::MathFn;
    use crate::css::properties::{
        parse_calc_function_length, parse_clamp_function_length, parse_min_max_function_length,
    };

    let mut input = ParserInput::new(value);
    let mut parser = Parser::new(&mut input);

    let parsed = match parser.next() {
        Ok(Token::Dimension { value, ref unit, .. }) => {
            let unit = unit.as_ref().to_ascii_lowercase();
            match unit.as_str() {
                "px" => Some(Length::px(*value)),
                "em" => Some(Length::em(*value)),
                "rem" => Some(Length::rem(*value)),
                "ex" => Some(Length::ex(*value)),
                "ch" => Some(Length::ch(*value)),
                "pt" => Some(Length::pt(*value)),
                "pc" => Some(Length::pc(*value)),
                "in" => Some(Length::inches(*value)),
                "cm" => Some(Length::cm(*value)),
                "mm" => Some(Length::mm(*value)),
                "q" => Some(Length::q(*value)),
                "vw" => Some(Length::new(*value, LengthUnit::Vw)),
                "vh" => Some(Length::new(*value, LengthUnit::Vh)),
                "vmin" => Some(Length::new(*value, LengthUnit::Vmin)),
                "vmax" => Some(Length::new(*value, LengthUnit::Vmax)),
                _ => None,
            }
        }
        Ok(Token::Function(ref name)) if name.eq_ignore_ascii_case("calc") => {
            parse_calc_function_length(&mut parser).ok()
        }
        Ok(Token::Function(ref name)) if name.eq_ignore_ascii_case("min") => {
            parse_min_max_function_length(&mut parser, MathFn::Min).ok()
        }
        Ok(Token::Function(ref name)) if name.eq_ignore_ascii_case("max") => {
            parse_min_max_function_length(&mut parser, MathFn::Max).ok()
        }
        Ok(Token::Function(ref name)) if name.eq_ignore_ascii_case("clamp") => {
            parse_clamp_function_length(&mut parser).ok()
        }
        Ok(Token::Percentage { unit_value, .. }) => Some(Length::percent(*unit_value * 100.0)),
        Ok(Token::Number { value, .. }) if *value == 0.0 => Some(Length::px(0.0)),
        Err(_) => None,
        _ => None,
    }?;

    parser.skip_whitespace();
    if parser.is_exhausted() {
        Some(parsed)
    } else {
        None
    }
}

/// Configuration for box generation
///
/// Controls how boxes are generated from DOM.
#[derive(Debug, Clone)]
pub struct BoxGenerationConfig {
    /// Whether to generate debug info for boxes
    pub include_debug_info: bool,

    /// Whether to insert anonymous boxes (Wave 3)
    pub insert_anonymous_boxes: bool,
}

impl BoxGenerationConfig {
    /// Creates default configuration
    pub fn new() -> Self {
        Self {
            include_debug_info: true,
            insert_anonymous_boxes: false,
        }
    }

    /// Configuration for production (minimal debug info)
    pub fn production() -> Self {
        Self {
            include_debug_info: false,
            insert_anonymous_boxes: false,
        }
    }

    /// Configuration for development (full debug info)
    pub fn development() -> Self {
        Self {
            include_debug_info: true,
            insert_anonymous_boxes: false,
        }
    }
}

impl Default for BoxGenerationConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Errors that can occur during box generation
#[derive(Debug)]
pub enum BoxGenerationError {
    /// Root element has display: none
    RootDisplayNone,

    /// Root element has display: contents
    RootDisplayContents,

    /// Invalid display value
    InvalidDisplay(String),

    /// Unsupported feature (for future use)
    Unsupported(String),

    /// Invalid box tree structure
    ///
    /// Examples: text box with children, replaced box with children
    InvalidStructure(String),
}

impl std::fmt::Display for BoxGenerationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RootDisplayNone => write!(f, "Root element has display: none"),
            Self::RootDisplayContents => write!(f, "Root element has display: contents"),
            Self::InvalidDisplay(msg) => write!(f, "Invalid display value: {}", msg),
            Self::Unsupported(msg) => write!(f, "Unsupported feature: {}", msg),
            Self::InvalidStructure(msg) => write!(f, "Invalid structure: {}", msg),
        }
    }
}

impl std::error::Error for BoxGenerationError {}

/// Box generator - transforms DOM tree into Box tree
///
/// Implements the CSS box generation algorithm from CSS 2.1 Section 9.2.
///
/// # Current Implementation (Wave 2)
///
/// - Handles display: none (skip element)
/// - Handles display: contents (skip box, but process children)
/// - Creates boxes for block, inline, flex, grid, table
/// - Creates text boxes
/// - Preserves document order
///
/// # Future Implementation (Wave 3)
///
/// - Anonymous box insertion for mixed inline/block content
/// - Pseudo-element box generation (::before, ::after)
pub struct BoxGenerator {
    config: BoxGenerationConfig,
}

impl BoxGenerator {
    /// Creates a new box generator with default configuration
    pub fn new() -> Self {
        Self {
            config: BoxGenerationConfig::default(),
        }
    }

    /// Creates a box generator with custom configuration
    pub fn with_config(config: BoxGenerationConfig) -> Self {
        Self { config }
    }

    /// Generates a box tree from a DOM tree
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Root element has `display: none`
    /// - Root element has `display: contents`
    pub fn generate(&self, dom_root: &DOMNode) -> Result<BoxTree, BoxGenerationError> {
        // Root must generate a box
        if self.should_skip_element(dom_root) {
            return Err(BoxGenerationError::RootDisplayNone);
        }

        if self.is_display_contents(dom_root) {
            return Err(BoxGenerationError::RootDisplayContents);
        }

        let mut counters = CounterManager::new();
        counters.enter_scope();
        let root_result = self.generate_box_for_element(dom_root, &mut counters);
        counters.leave_scope();
        // Generate box for root
        let root_box = root_result?;

        // Apply anonymous box fixup if enabled
        let final_root = if self.config.insert_anonymous_boxes {
            AnonymousBoxCreator::fixup_tree(root_box)
        } else {
            root_box
        };

        Ok(BoxTree::new(final_root))
    }

    /// Generates a box tree with anonymous box fixup
    ///
    /// This is a convenience method that generates the box tree with
    /// anonymous box insertion enabled, regardless of the config setting.
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Root element has `display: none`
    /// - Root element has `display: contents`
    pub fn generate_with_anonymous_fixup(&self, dom_root: &DOMNode) -> Result<BoxTree, BoxGenerationError> {
        // Root must generate a box
        if self.should_skip_element(dom_root) {
            return Err(BoxGenerationError::RootDisplayNone);
        }

        if self.is_display_contents(dom_root) {
            return Err(BoxGenerationError::RootDisplayContents);
        }

        let mut counters = CounterManager::new();
        counters.enter_scope();
        let root_result = self.generate_box_for_element(dom_root, &mut counters);
        counters.leave_scope();

        // Generate box for root
        let root_box = root_result?;

        // Always apply anonymous box fixup
        let fixed_root = AnonymousBoxCreator::fixup_tree(root_box);

        Ok(BoxTree::new(fixed_root))
    }

    /// Generates a box for a single DOM node (element or text)
    ///
    /// # CSS 2.1 Section 9.2
    ///
    /// Box generation determines what type of box is created based on:
    /// 1. Element type (text node, element, replaced element)
    /// 2. Display property value
    /// 3. Replaced element status (img, video, etc.)
    fn generate_box_for_element(
        &self,
        node: &DOMNode,
        counters: &mut CounterManager,
    ) -> Result<BoxNode, BoxGenerationError> {
        // Handle text nodes
        if node.is_text() {
            return Ok(self.create_text_box(node));
        }

        counters.enter_scope();
        let result = self.generate_box_for_element_in_scope(node, counters);
        counters.leave_scope();
        result
    }

    fn generate_box_for_element_in_scope(
        &self,
        node: &DOMNode,
        counters: &mut CounterManager,
    ) -> Result<BoxNode, BoxGenerationError> {
        self.apply_counter_properties(node, counters);

        // Handle replaced elements (img, video, canvas, etc.)
        // Replaced elements don't have children in the CSS sense
        if node.is_replaced_element() {
            return self.create_replaced_box(node);
        }

        // Generate boxes for children first
        let child_boxes = self.generate_child_boxes(node, counters)?;

        // Determine what kind of box to create
        let display = node.display();

        // Derive formatting context type (if any)
        let fc_type = display.formatting_context_type();

        // Create appropriate box type
        let box_node = match display {
            Display::Block | Display::FlowRoot | Display::ListItem => BoxNode::new_block(
                node.style.clone(),
                fc_type.unwrap_or(FormattingContextType::Block),
                child_boxes,
            ),

            Display::Inline => BoxNode::new_inline(node.style.clone(), child_boxes),

            Display::InlineBlock => {
                BoxNode::new_inline_block(node.style.clone(), FormattingContextType::Block, child_boxes)
            }

            Display::Flex | Display::InlineFlex => {
                BoxNode::new_block(node.style.clone(), FormattingContextType::Flex, child_boxes)
            }

            Display::Grid | Display::InlineGrid => {
                BoxNode::new_block(node.style.clone(), FormattingContextType::Grid, child_boxes)
            }

            Display::Table | Display::InlineTable => {
                BoxNode::new_block(node.style.clone(), FormattingContextType::Table, child_boxes)
            }

            // Table-internal boxes (simplified for Wave 2)
            Display::TableRow
            | Display::TableCell
            | Display::TableRowGroup
            | Display::TableHeaderGroup
            | Display::TableFooterGroup
            | Display::TableColumn
            | Display::TableColumnGroup
            | Display::TableCaption => {
                // For now, treat as block boxes
                BoxNode::new_block(node.style.clone(), FormattingContextType::Block, child_boxes)
            }

            Display::None | Display::Contents => {
                // These should be filtered out before reaching here
                return Err(BoxGenerationError::InvalidDisplay(format!("{:?}", display)));
            }
        };

        let mut box_node = if box_node.style.display == Display::ListItem {
            self.attach_list_marker(box_node, counters)
        } else {
            box_node
        };

        // Add debug info if enabled
        if self.config.include_debug_info {
            let debug_info = self.create_debug_info(node);
            box_node = box_node.with_debug_info(debug_info);
        }

        Ok(box_node)
    }

    /// Creates a replaced box for replaced elements (img, video, canvas, etc.)
    ///
    /// # CSS 2.1 Section 10.3.2
    ///
    /// "Replaced elements are those whose content is outside the scope of
    /// the CSS formatting model, such as an image, embedded document, or applet."
    ///
    /// Replaced elements have intrinsic dimensions and aspect ratios that affect
    /// how they are sized during layout.
    fn create_replaced_box(&self, node: &DOMNode) -> Result<BoxNode, BoxGenerationError> {
        let replaced_type = node
            .replaced_type()
            .ok_or_else(|| BoxGenerationError::Unsupported(format!("Not a replaced element: {:?}", node.tag_name)))?;

        let intrinsic_size = node.intrinsic_size;
        let aspect_ratio = node.aspect_ratio();

        let box_node = BoxNode::new_replaced(node.style.clone(), replaced_type, intrinsic_size, aspect_ratio);

        // Add debug info if enabled
        if self.config.include_debug_info {
            let debug_info = self.create_debug_info(node);
            Ok(box_node.with_debug_info(debug_info))
        } else {
            Ok(box_node)
        }
    }

    /// Generates boxes for all children of a node
    ///
    /// Handles:
    /// - Skipping display: none children
    /// - Adopting display: contents children's children
    /// - Preserving document order
    fn generate_child_boxes(
        &self,
        parent: &DOMNode,
        counters: &mut CounterManager,
    ) -> Result<Vec<BoxNode>, BoxGenerationError> {
        let mut child_boxes = Vec::new();

        for child in &parent.children {
            // Skip display: none
            if self.should_skip_element(child) {
                continue;
            }

            // Handle display: contents - adopt grandchildren
            if self.is_display_contents(child) {
                // For display: contents, skip the element box but still apply counter properties
                counters.enter_scope();
                self.apply_counter_properties(child, counters);
                let grandchild_boxes = self.generate_child_boxes(child, counters)?;
                counters.leave_scope();

                // For display: contents, skip the element but process its children
                child_boxes.extend(grandchild_boxes);
                continue;
            }

            // Generate box for child
            let child_box = self.generate_box_for_element(child, counters)?;

            child_boxes.push(child_box);
        }

        // Note: Anonymous box fixup is now done as a post-processing step
        // after the entire tree is generated. See AnonymousBoxCreator::fixup_tree()

        Ok(child_boxes)
    }

    /// Creates a text box from a text node
    fn create_text_box(&self, node: &DOMNode) -> BoxNode {
        let text = node.text.as_ref().unwrap().clone();
        let box_node = BoxNode::new_text(node.style.clone(), text);

        if self.config.include_debug_info {
            let debug_info = DebugInfo::new(Some("text".to_string()), None, vec![]);
            box_node.with_debug_info(debug_info)
        } else {
            box_node
        }
    }

    /// Creates debug info from a DOM node
    fn create_debug_info(&self, node: &DOMNode) -> DebugInfo {
        DebugInfo::new(node.tag_name.clone(), node.id.clone(), node.classes.clone())
    }

    /// Returns true if element should be skipped (display: none)
    fn should_skip_element(&self, node: &DOMNode) -> bool {
        if node.is_text() {
            return false;
        }

        matches!(node.display(), Display::None)
    }

    /// Returns true if element has display: contents
    fn is_display_contents(&self, node: &DOMNode) -> bool {
        if node.is_text() {
            return false;
        }

        matches!(node.display(), Display::Contents)
    }

    /// Applies CSS counter operations for the current element
    ///
    /// Order follows CSS Lists & Counters: reset → set → increment. If the
    /// element is a list item and no explicit counter-increment is present,
    /// increment the `list-item` counter by 1 per spec defaults.
    fn apply_counter_properties(&self, node: &DOMNode, counters: &mut CounterManager) {
        if node.is_text() {
            return;
        }

        let mut applied_default_reset = false;
        if let Some(reset) = &node.style.counters.counter_reset {
            counters.apply_reset(reset);
            applied_default_reset = true;
        }

        if !applied_default_reset {
            if let Some(tag) = &node.tag_name {
                if tag.eq_ignore_ascii_case("ol") || tag.eq_ignore_ascii_case("ul") {
                    let default_reset = CounterSet::single("list-item", 0);
                    counters.apply_reset(&default_reset);
                }
            }
        }

        if let Some(set) = &node.style.counters.counter_set {
            counters.apply_set(set);
        }

        if let Some(increment) = &node.style.counters.counter_increment {
            counters.apply_increment(increment);
        } else if node.display() == Display::ListItem {
            counters.apply_increment(&CounterSet::single("list-item", 1));
        }
    }

    /// Prepends a marker box for list items based on list-style-* properties
    fn attach_list_marker(&self, mut list_item: BoxNode, counters: &CounterManager) -> BoxNode {
        let style = list_item.style.clone();
        let dummy_dom = DomNode {
            node_type: DomNodeType::Element {
                tag_name: "li".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };
        let dummy_styled = StyledNode {
            node: dummy_dom,
            styles: (*style).clone(),
            before_styles: None,
            after_styles: None,
            marker_styles: None,
            children: vec![],
        };
        let marker_content = marker_content_from_style(&dummy_styled, &style, counters);
        if let Some(content) = marker_content {
            let mut marker_style = (*style).clone();
            marker_style.display = Display::Inline;
            crate::style::cascade::reset_marker_box_properties(&mut marker_style);
            marker_style.list_style_type = ListStyleType::None;
            marker_style.list_style_image = crate::style::types::ListStyleImage::None;
            marker_style.text_transform = TextTransform::none();

            let marker_node = BoxNode::new_marker(Arc::new(marker_style), content);
            list_item.children.insert(0, marker_node);
        }
        list_item
    }
}

// =============================================================================
// Utility Methods for Box Tree Operations
// =============================================================================

impl BoxGenerator {
    /// Counts total boxes in a generated tree (including root)
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::{BoxNode, FormattingContextType};
    /// use fastrender::ComputedStyle;
    /// use fastrender::BoxGenerator;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let child = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    /// let parent = BoxNode::new_block(style, FormattingContextType::Block, vec![child]);
    ///
    /// let count = BoxGenerator::count_boxes(&parent);
    /// assert_eq!(count, 2); // parent + child
    /// ```
    pub fn count_boxes(box_node: &BoxNode) -> usize {
        1 + box_node
            .children
            .iter()
            .map(|child| Self::count_boxes(child))
            .sum::<usize>()
    }

    /// Finds all boxes matching a predicate
    ///
    /// Traverses the box tree and returns references to all boxes
    /// where the predicate returns true.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::{BoxNode, BoxType, FormattingContextType};
    /// use fastrender::ComputedStyle;
    /// use fastrender::BoxGenerator;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let text = BoxNode::new_text(style.clone(), "Hello".to_string());
    /// let inline = BoxNode::new_inline(style.clone(), vec![text]);
    /// let block = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![inline]);
    ///
    /// // Find all text boxes
    /// let text_boxes = BoxGenerator::find_boxes_by_predicate(&block, |b| b.is_text());
    /// assert_eq!(text_boxes.len(), 1);
    /// ```
    pub fn find_boxes_by_predicate<'a, F>(box_node: &'a BoxNode, predicate: F) -> Vec<&'a BoxNode>
    where
        F: Fn(&BoxNode) -> bool + Copy,
    {
        let mut result = Vec::new();

        if predicate(box_node) {
            result.push(box_node);
        }

        for child in &box_node.children {
            result.extend(Self::find_boxes_by_predicate(child, predicate));
        }

        result
    }

    /// Finds all boxes of a specific box type
    ///
    /// Convenience wrapper around find_boxes_by_predicate for type queries.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::{BoxNode, FormattingContextType};
    /// use fastrender::ComputedStyle;
    /// use fastrender::BoxGenerator;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let child1 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    /// let child2 = BoxNode::new_inline(style.clone(), vec![]);
    /// let parent = BoxNode::new_block(style, FormattingContextType::Block, vec![child1, child2]);
    ///
    /// // Find all block boxes
    /// let block_boxes = BoxGenerator::find_block_boxes(&parent);
    /// assert_eq!(block_boxes.len(), 2); // parent + child1
    /// ```
    pub fn find_block_boxes(box_node: &BoxNode) -> Vec<&BoxNode> {
        Self::find_boxes_by_predicate(box_node, |b| b.is_block_level())
    }

    /// Finds all inline boxes
    pub fn find_inline_boxes(box_node: &BoxNode) -> Vec<&BoxNode> {
        Self::find_boxes_by_predicate(box_node, |b| b.is_inline_level())
    }

    /// Finds all text boxes
    pub fn find_text_boxes(box_node: &BoxNode) -> Vec<&BoxNode> {
        Self::find_boxes_by_predicate(box_node, |b| b.is_text())
    }

    /// Finds all replaced boxes
    pub fn find_replaced_boxes(box_node: &BoxNode) -> Vec<&BoxNode> {
        Self::find_boxes_by_predicate(box_node, |b| b.is_replaced())
    }

    /// Validates box tree structure
    ///
    /// Checks for common structural errors:
    /// - Text boxes with children (invalid)
    /// - Replaced boxes with children (invalid)
    ///
    /// Returns Ok(()) if the tree is valid, Err with description if not.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::{BoxNode, FormattingContextType};
    /// use fastrender::ComputedStyle;
    /// use fastrender::BoxGenerator;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let valid_tree = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![
    ///     BoxNode::new_text(style.clone(), "Hello".to_string()),
    /// ]);
    ///
    /// assert!(BoxGenerator::validate_box_tree(&valid_tree).is_ok());
    /// ```
    pub fn validate_box_tree(box_node: &BoxNode) -> Result<(), BoxGenerationError> {
        // Text boxes cannot have children
        if box_node.is_text() && !box_node.children.is_empty() {
            return Err(BoxGenerationError::InvalidStructure(
                "Text box cannot have children".to_string(),
            ));
        }

        // Replaced boxes cannot have children
        if box_node.is_replaced() && !box_node.children.is_empty() {
            return Err(BoxGenerationError::InvalidStructure(
                "Replaced box cannot have children".to_string(),
            ));
        }

        // Recursively validate children
        for child in &box_node.children {
            Self::validate_box_tree(child)?;
        }

        Ok(())
    }

    /// Returns the depth of the box tree
    ///
    /// The depth is the maximum nesting level from root to any leaf.
    pub fn tree_depth(box_node: &BoxNode) -> usize {
        if box_node.children.is_empty() {
            1
        } else {
            1 + box_node.children.iter().map(Self::tree_depth).max().unwrap_or(0)
        }
    }
}

impl Default for BoxGenerator {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// StyledNode-based Box Generation (for real DOM/style pipeline)
// ============================================================================

use crate::style::cascade::StyledNode;

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
pub fn generate_box_tree(styled: &StyledNode) -> BoxTree {
    let mut counters = CounterManager::new();
    counters.enter_scope();
    let mut roots = generate_boxes_for_styled(styled, &mut counters, true);
    counters.leave_scope();
    let root = match roots.len() {
        0 => BoxNode::new_block(
            Arc::new(ComputedStyle::default()),
            FormattingContextType::Block,
            Vec::new(),
        ),
        1 => roots.remove(0),
        _ => BoxNode::new_anonymous_block(Arc::new(ComputedStyle::default()), roots),
    };
    BoxTree { root }
}

fn escape_attr(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

fn escape_text(value: &str) -> String {
    value.replace('&', "&amp;").replace('<', "&lt;")
}

fn serialize_dom_subtree(node: &crate::dom::DomNode) -> String {
    match &node.node_type {
        crate::dom::DomNodeType::Text { content } => escape_text(content),
        crate::dom::DomNodeType::Element { tag_name, attributes } => {
            let mut out = String::new();
            out.push('<');
            out.push_str(tag_name);
            for (name, value) in attributes {
                out.push(' ');
                out.push_str(name);
                out.push('=');
                out.push('"');
                out.push_str(&escape_attr(value));
                out.push('"');
            }
            out.push('>');
            for child in &node.children {
                out.push_str(&serialize_dom_subtree(child));
            }
            out.push_str("</");
            out.push_str(tag_name);
            out.push('>');
            out
        }
        crate::dom::DomNodeType::Document => node.children.iter().map(serialize_dom_subtree).collect(),
    }
}

/// Recursively generates BoxNodes from a StyledNode, honoring display: contents by
/// splicing grandchildren into the parent’s child list rather than creating a box.
fn generate_boxes_for_styled(styled: &StyledNode, counters: &mut CounterManager, _is_root: bool) -> Vec<BoxNode> {
    if let Some(text) = styled.node.text_content() {
        if !text.is_empty() {
            let style = Arc::new(styled.styles.clone());
            return vec![BoxNode::new_text(style, text.to_string())];
        }
    }

    counters.enter_scope();
    apply_counter_properties_from_style(styled, counters);

    // display:none suppresses box generation entirely.
    if styled.styles.display == Display::None {
        counters.leave_scope();
        return Vec::new();
    }

    // Replaced elements short-circuit to a single replaced box unless they're display: contents.
    if let Some(tag) = styled.node.tag_name() {
        // Non-rendered elements: <source>, <track> never create boxes.
        if matches!(tag.to_ascii_lowercase().as_str(), "source" | "track") {
            counters.leave_scope();
            return Vec::new();
        }

        if is_replaced_element(tag) && styled.styles.display != Display::Contents {
            // The <object> element falls back to its nested content when no data URI is provided.
            // In that case we should not generate a replaced box, allowing the children to render normally.
            if !tag.eq_ignore_ascii_case("object")
                || styled
                    .node
                    .get_attribute("data")
                    .map(|d| !d.is_empty())
                    .unwrap_or(false)
            {
                counters.leave_scope();
                let box_node = create_replaced_box_from_styled(styled, Arc::new(styled.styles.clone()));
                return vec![attach_debug_info(box_node, styled)];
            }
        }
    }

    let mut children: Vec<BoxNode> = Vec::new();
    for child in &styled.children {
        children.extend(generate_boxes_for_styled(child, counters, false));
    }

    // Generate ::before pseudo-element box if styles exist
    if let Some(before_styles) = &styled.before_styles {
        if let Some(before_box) = create_pseudo_element_box(styled, before_styles, "before", counters) {
            children.insert(0, before_box);
        }
    }

    if styled.styles.display == Display::ListItem {
        if let Some(marker_box) = create_marker_box(styled, counters) {
            children.insert(0, marker_box);
        }
    }

    // Generate ::after pseudo-element box if styles exist
    if let Some(after_styles) = &styled.after_styles {
        if let Some(after_box) = create_pseudo_element_box(styled, after_styles, "after", counters) {
            children.push(after_box);
        }
    }

    // display: contents contributes its children directly.
    if styled.styles.display == Display::Contents {
        counters.leave_scope();
        return children;
    }

    let style = Arc::new(styled.styles.clone());
    let fc_type = styled
        .styles
        .display
        .formatting_context_type()
        .unwrap_or(FormattingContextType::Block);

    let box_node = match styled.styles.display {
        Display::Block | Display::FlowRoot | Display::ListItem => BoxNode::new_block(style, fc_type, children),
        Display::Inline => BoxNode::new_inline(style, children),
        Display::InlineBlock => BoxNode::new_inline_block(style, fc_type, children),
        Display::Flex | Display::InlineFlex => BoxNode::new_block(style, FormattingContextType::Flex, children),
        Display::Grid | Display::InlineGrid => BoxNode::new_block(style, FormattingContextType::Grid, children),
        Display::Table | Display::InlineTable => BoxNode::new_block(style, FormattingContextType::Table, children),
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

    counters.leave_scope();
    vec![attach_debug_info(box_node, styled)]
}

fn attach_debug_info(mut box_node: BoxNode, styled: &StyledNode) -> BoxNode {
    if let Some(tag) = styled.node.tag_name() {
        // Extract colspan/rowspan for table cells and span for columns/colgroups
        let colspan = styled
            .node
            .get_attribute("colspan")
            .and_then(|s| s.parse::<usize>().ok())
            .unwrap_or(1)
            .max(1);
        let rowspan = styled
            .node
            .get_attribute("rowspan")
            .and_then(|s| s.parse::<usize>().ok())
            .unwrap_or(1)
            .max(1);
        let column_span = if matches!(styled.styles.display, Display::TableColumn | Display::TableColumnGroup) {
            styled
                .node
                .get_attribute("span")
                .and_then(|s| s.parse::<usize>().ok())
                .unwrap_or(1)
                .max(1)
        } else {
            1
        };

        let id = styled.node.get_attribute("id");
        let classes = styled
            .node
            .get_attribute("class")
            .map(|c| c.split_whitespace().map(|s| s.to_string()).collect())
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
    styles: &ComputedStyle,
    pseudo_name: &str,
    counters: &CounterManager,
) -> Option<BoxNode> {
    let content_value = effective_content_value(styles);
    if matches!(content_value, ContentValue::None | ContentValue::Normal) {
        return None;
    }

    let pseudo_style = Arc::new(styles.clone());

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
        if !buf.is_empty() {
            out.push(BoxNode::new_text(pseudo_style.clone(), buf.clone()));
            buf.clear();
        }
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
                let formatted = style.unwrap_or(CounterStyle::Decimal).format(value);
                text_buf.push_str(&formatted);
            }
            ContentItem::Counters { name, separator, style } => {
                let values = context.get_counters(name);
                if values.is_empty() {
                    text_buf.push('0');
                } else {
                    let formatted: Vec<String> = values
                        .iter()
                        .map(|v| style.unwrap_or(CounterStyle::Decimal).format(*v))
                        .collect();
                    text_buf.push_str(&formatted.join(separator));
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
            ContentItem::Url(url) => {
                flush_text(&mut text_buf, &pseudo_style, &mut children);
                let replaced = ReplacedBox {
                    replaced_type: ReplacedType::Image {
                        src: url.clone(),
                        alt: None,
                        sizes: None,
                        srcset: Vec::new(),
                    },
                    intrinsic_size: None,
                    aspect_ratio: None,
                };
                children.push(BoxNode {
                    box_type: BoxType::Replaced(replaced),
                    style: pseudo_style.clone(),
                    children: vec![],
                    debug_info: None,
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

    Some(pseudo_box)
}

fn create_marker_box(styled: &StyledNode, counters: &CounterManager) -> Option<BoxNode> {
    // Prefer authored ::marker styles; fall back to the originating style when absent.
    let mut marker_style = styled.marker_styles.clone().unwrap_or_else(|| styled.styles.clone());
    // ::marker boxes are inline and should not carry layout-affecting edges from the list item.
    crate::style::cascade::reset_marker_box_properties(&mut marker_style);
    marker_style.display = Display::Inline;

    let content = marker_content_from_style(styled, &marker_style, counters)?;
    marker_style.list_style_type = ListStyleType::None;
    marker_style.list_style_image = crate::style::types::ListStyleImage::None;
    marker_style.text_transform = TextTransform::none();

    Some(BoxNode::new_marker(Arc::new(marker_style), content))
}

fn marker_content_from_style(
    styled: &StyledNode,
    style: &ComputedStyle,
    counters: &CounterManager,
) -> Option<MarkerContent> {
    let content_value = effective_content_value(style);
    if matches!(content_value, ContentValue::None) || style.content == "none" {
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
        context.set_quotes(style.quotes.clone());

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
                        let formatted = style.unwrap_or(CounterStyle::Decimal).format(value);
                        text.push_str(&formatted);
                    }
                    ContentItem::Counters { name, separator, style } => {
                        let values = context.get_counters(name);
                        if values.is_empty() {
                            text.push('0');
                        } else {
                            let formatted: Vec<String> = values
                                .iter()
                                .map(|v| style.unwrap_or(CounterStyle::Decimal).format(*v))
                                .collect();
                            text.push_str(&formatted.join(separator));
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
                },
                intrinsic_size: None,
                aspect_ratio: None,
            };
            return Some(MarkerContent::Image(replaced));
        }
        return None;
    }

    match &style.list_style_image {
        crate::style::types::ListStyleImage::Url(url) => {
            let replaced = ReplacedBox {
                replaced_type: ReplacedType::Image {
                    src: url.clone(),
                    alt: None,
                    sizes: None,
                    srcset: Vec::new(),
                },
                intrinsic_size: None,
                aspect_ratio: None,
            };
            return Some(MarkerContent::Image(replaced));
        }
        crate::style::types::ListStyleImage::None => {}
    }

    let text = list_marker_text(style.list_style_type, counters);
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

    let mut applied_default_reset = false;
    let tag_name = styled.node.tag_name().map(|t| t.to_ascii_lowercase());
    let is_ol = tag_name.as_deref() == Some("ol");
    let is_ul = tag_name.as_deref() == Some("ul");
    let reversed = is_ol && styled.node.get_attribute("reversed").is_some();
    if reversed {
        counters.set_list_item_increment(-1);
    }

    if let Some(reset) = &styled.styles.counters.counter_reset {
        counters.apply_reset(reset);
        applied_default_reset = true;
    }

    if !applied_default_reset && (is_ol || is_ul) {
        let start = styled.node.get_attribute("start").and_then(|s| s.parse::<i32>().ok());
        let step = counters.list_item_increment();
        let start_value = if is_ol {
            if reversed {
                // reversed lists count down; default start is the number of list items
                let item_count = list_item_count(styled) as i32;
                start.unwrap_or(item_count.max(0))
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
        if let Some(value_attr) = styled.node.get_attribute("value").and_then(|v| v.parse::<i32>().ok()) {
            let step = counters.list_item_increment();
            let target = value_attr.saturating_sub(step);
            counters.apply_set(&CounterSet::single("list-item", target));
        }
    }

    if let Some(increment) = &styled.styles.counters.counter_increment {
        counters.apply_increment(increment);
    } else if styled.styles.display == Display::ListItem {
        let step = counters.list_item_increment();
        counters.apply_increment(&CounterSet::single("list-item", step));
    }
}

/// Count immediate list items belonging to this list, ignoring nested lists.
fn list_item_count(styled: &StyledNode) -> usize {
    fn walk(node: &StyledNode, in_nested_list: bool, acc: &mut usize) {
        for child in &node.children {
            let tag = child.node.tag_name().map(|t| t.to_ascii_lowercase());
            let is_list = matches!(tag.as_deref(), Some("ol") | Some("ul"));
            let now_nested = in_nested_list || is_list;
            if !now_nested && tag.as_deref() == Some("li") {
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

/// Checks if an element is a replaced element
///
/// Replaced elements are those whose content is replaced by an external resource,
/// such as images, videos, iframes, etc. These elements have intrinsic dimensions.
pub fn is_replaced_element(tag: &str) -> bool {
    matches!(
        tag.to_lowercase().as_str(),
        "img" | "video" | "canvas" | "svg" | "iframe" | "embed" | "object" | "audio"
    )
}

/// Creates a BoxNode for a replaced element from a StyledNode
fn create_replaced_box_from_styled(styled: &StyledNode, style: Arc<ComputedStyle>) -> BoxNode {
    let tag = styled.node.tag_name().unwrap_or("img");

    // Get src attribute if available
    let src = styled.node.get_attribute("src").unwrap_or_default();
    let alt = styled.node.get_attribute("alt").filter(|s| !s.is_empty());
    let poster = styled.node.get_attribute("poster").filter(|s| !s.is_empty());
    let srcset_attr = styled.node.get_attribute("srcset");
    let sizes_attr = styled.node.get_attribute("sizes");
    let srcset = srcset_attr.as_ref().map(|s| parse_srcset(s)).unwrap_or_default();
    let sizes = sizes_attr.as_ref().and_then(|s| parse_sizes(s));
    let data_attr = styled.node.get_attribute("data").unwrap_or_default();

    // Determine replaced type
    let replaced_type = match tag.to_lowercase().as_str() {
        "img" => ReplacedType::Image {
            src,
            alt,
            srcset,
            sizes,
        },
        "video" => ReplacedType::Video { src, poster },
        "audio" => ReplacedType::Audio { src },
        "canvas" => ReplacedType::Canvas,
        "svg" => ReplacedType::Svg {
            content: serialize_dom_subtree(&styled.node),
        },
        "iframe" => ReplacedType::Iframe { src },
        "embed" => ReplacedType::Embed { src },
        "object" => ReplacedType::Object { data: data_attr },
        _ => ReplacedType::Image {
            src,
            alt,
            sizes: None,
            srcset: Vec::new(),
        },
    };

    // Get intrinsic size from attributes when provided
    let intrinsic_width = styled.node.get_attribute("width").and_then(|w| w.parse::<f32>().ok());

    let intrinsic_height = styled.node.get_attribute("height").and_then(|h| h.parse::<f32>().ok());

    let mut intrinsic_size = match (intrinsic_width, intrinsic_height) {
        (Some(w), Some(h)) => Some(Size::new(w, h)),
        _ => None,
    };

    let mut aspect_ratio = match (intrinsic_width, intrinsic_height) {
        (Some(w), Some(h)) if h > 0.0 => Some(w / h),
        _ => None,
    };

    if intrinsic_size.is_none() && aspect_ratio.is_none() {
        match replaced_type {
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
        children: vec![],
        debug_info: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::Size;
    use crate::style::counters::CounterSet;
    use crate::style::types::CaseTransform;
    use crate::style::types::ListStylePosition;
    use crate::tree::box_tree::{MarkerContent, ReplacedType};
    use crate::{dom, style};

    fn default_style() -> Arc<ComputedStyle> {
        Arc::new(ComputedStyle::default())
    }

    fn style_with_display(display: Display) -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.display = display;
        Arc::new(style)
    }

    #[test]
    fn test_config_creation() {
        let config = BoxGenerationConfig::new();
        assert!(config.include_debug_info);
        assert!(!config.insert_anonymous_boxes);
    }

    #[test]
    fn test_config_production() {
        let config = BoxGenerationConfig::production();
        assert!(!config.include_debug_info);
    }

    #[test]
    fn test_config_development() {
        let config = BoxGenerationConfig::development();
        assert!(config.include_debug_info);
    }

    #[test]
    fn test_generator_creation() {
        let generator = BoxGenerator::new();
        assert!(generator.config.include_debug_info);
    }

    #[test]
    fn test_generator_with_config() {
        let config = BoxGenerationConfig::production();
        let generator = BoxGenerator::with_config(config);
        assert!(!generator.config.include_debug_info);
    }

    #[test]
    fn test_dom_node_element() {
        let dom = DOMNode::new_element("div", default_style(), vec![]);
        assert!(dom.is_element());
        assert!(!dom.is_text());
        assert_eq!(dom.tag_name, Some("div".to_string()));
    }

    #[test]
    fn test_dom_node_text() {
        let dom = DOMNode::new_text("Hello", default_style());
        assert!(dom.is_text());
        assert!(!dom.is_element());
        assert_eq!(dom.text, Some("Hello".to_string()));
    }

    #[test]
    fn test_dom_node_with_id() {
        let dom = DOMNode::new_element("div", default_style(), vec![]).with_id("header");
        assert_eq!(dom.id, Some("header".to_string()));
    }

    #[test]
    fn test_dom_node_with_class() {
        let dom = DOMNode::new_element("div", default_style(), vec![]).with_class("navbar");
        assert_eq!(dom.classes.len(), 1);
        assert_eq!(dom.classes[0], "navbar");
    }

    #[test]
    fn test_generate_single_block() {
        let generator = BoxGenerator::new();
        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);

        let box_tree = generator.generate(&dom).unwrap();
        assert!(box_tree.root.is_block_level());
        assert_eq!(box_tree.count_boxes(), 1);
    }

    #[test]
    fn test_generate_block_with_text() {
        let generator = BoxGenerator::new();
        let text = DOMNode::new_text("Hello", default_style());
        let dom = DOMNode::new_element("p", style_with_display(Display::Block), vec![text]);

        let box_tree = generator.generate(&dom).unwrap();
        assert_eq!(box_tree.count_boxes(), 2); // p + text
        assert_eq!(box_tree.count_text_boxes(), 1);
    }

    #[test]
    fn test_generate_nested_blocks() {
        let generator = BoxGenerator::new();

        let inner = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
        let outer = DOMNode::new_element("div", style_with_display(Display::Block), vec![inner]);

        let box_tree = generator.generate(&outer).unwrap();
        assert_eq!(box_tree.count_boxes(), 2); // div + p
        assert_eq!(box_tree.root.children.len(), 1);
    }

    #[test]
    fn test_generate_multiple_children() {
        let generator = BoxGenerator::new();

        let child1 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
        let child2 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
        let child3 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);

        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![child1, child2, child3]);

        let box_tree = generator.generate(&dom).unwrap();
        assert_eq!(box_tree.count_boxes(), 4); // div + 3 p's
        assert_eq!(box_tree.root.children.len(), 3);
    }

    #[test]
    fn test_skip_display_none_child() {
        let generator = BoxGenerator::new();

        let child1 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
        let child2 = DOMNode::new_element("p", style_with_display(Display::None), vec![]);
        let child3 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);

        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![child1, child2, child3]);

        let box_tree = generator.generate(&dom).unwrap();
        // child2 is skipped (display: none)
        assert_eq!(box_tree.count_boxes(), 3); // div + child1 + child3
        assert_eq!(box_tree.root.children.len(), 2);
    }

    #[test]
    fn test_display_contents_adopts_children() {
        let generator = BoxGenerator::new();

        // Create structure: div > (p display:contents > (span, span))
        let grandchild1 = DOMNode::new_element("span", style_with_display(Display::Inline), vec![]);
        let grandchild2 = DOMNode::new_element("span", style_with_display(Display::Inline), vec![]);

        let child = DOMNode::new_element(
            "p",
            style_with_display(Display::Contents),
            vec![grandchild1, grandchild2],
        );

        let root = DOMNode::new_element("div", style_with_display(Display::Block), vec![child]);

        let box_tree = generator.generate(&root).unwrap();

        // p (display:contents) doesn't generate a box, but its children do
        // So we have: div + span + span = 3 boxes
        assert_eq!(box_tree.count_boxes(), 3);
        // div's children should be the two spans
        assert_eq!(box_tree.root.children.len(), 2);
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
            node: text_dom.clone(),
            styles: ComputedStyle::default(),
            before_styles: None,
            after_styles: None,
            marker_styles: None,
            children: vec![],
        };

        let li_dom = dom::DomNode {
            node_type: dom::DomNodeType::Element {
                tag_name: "li".to_string(),
                attributes: vec![],
            },
            children: vec![text_dom],
        };

        let li = StyledNode {
            node: li_dom,
            styles: li_style,
            before_styles: None,
            after_styles: None,
            marker_styles: Some(marker_style),
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
    fn test_text_box_creation() {
        let generator = BoxGenerator::new();
        let text = DOMNode::new_text("Hello, world!", default_style());

        let text_box = generator.create_text_box(&text);

        assert!(text_box.is_text());
        assert_eq!(text_box.text(), Some("Hello, world!"));
    }

    #[test]
    fn test_debug_info_included() {
        let config = BoxGenerationConfig::development();
        let generator = BoxGenerator::with_config(config);

        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![])
            .with_id("header")
            .with_class("navbar");

        let box_tree = generator.generate(&dom).unwrap();
        assert!(box_tree.root.debug_info.is_some());

        let debug_info = box_tree.root.debug_info.as_ref().unwrap();
        assert_eq!(debug_info.tag_name, Some("div".to_string()));
        assert_eq!(debug_info.id, Some("header".to_string()));
        assert_eq!(debug_info.classes.len(), 1);
    }

    #[test]
    fn test_debug_info_excluded() {
        let config = BoxGenerationConfig::production();
        let generator = BoxGenerator::with_config(config);

        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);

        let box_tree = generator.generate(&dom).unwrap();
        assert!(box_tree.root.debug_info.is_none());
    }

    #[test]
    fn test_complex_tree_structure() {
        let generator = BoxGenerator::new();

        // Build: div > (p > text, p > text, div > p > text)
        let text1 = DOMNode::new_text("Text 1", default_style());
        let text2 = DOMNode::new_text("Text 2", default_style());
        let text3 = DOMNode::new_text("Text 3", default_style());

        let p1 = DOMNode::new_element("p", style_with_display(Display::Block), vec![text1]);
        let p2 = DOMNode::new_element("p", style_with_display(Display::Block), vec![text2]);
        let p3 = DOMNode::new_element("p", style_with_display(Display::Block), vec![text3]);

        let inner_div = DOMNode::new_element("div", style_with_display(Display::Block), vec![p3]);
        let outer_div = DOMNode::new_element("div", style_with_display(Display::Block), vec![p1, p2, inner_div]);

        let box_tree = generator.generate(&outer_div).unwrap();

        // div(root) + p + text + p + text + div + p + text = 8 boxes
        assert_eq!(box_tree.count_boxes(), 8);
        assert_eq!(box_tree.count_text_boxes(), 3);
    }

    #[test]
    fn test_inline_box_generation() {
        let generator = BoxGenerator::new();

        let text = DOMNode::new_text("Hello", default_style());
        let span = DOMNode::new_element("span", style_with_display(Display::Inline), vec![text]);
        let p = DOMNode::new_element("p", style_with_display(Display::Block), vec![span]);

        let box_tree = generator.generate(&p).unwrap();

        // p(block) + span(inline) + text = 3 boxes
        assert_eq!(box_tree.count_boxes(), 3);

        // Root is block
        assert!(box_tree.root.is_block_level());

        // First child should be inline
        assert!(box_tree.root.children[0].is_inline_level());
    }

    #[test]
    fn test_document_order_preserved() {
        let generator = BoxGenerator::new();

        let child1 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]).with_id("first");
        let child2 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]).with_id("second");
        let child3 = DOMNode::new_element("p", style_with_display(Display::Block), vec![]).with_id("third");

        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![child1, child2, child3]);

        let box_tree = generator.generate(&dom).unwrap();

        // Check order is preserved
        assert_eq!(box_tree.root.children.len(), 3);
        let ids: Vec<_> = box_tree
            .root
            .children
            .iter()
            .filter_map(|c| c.debug_info.as_ref())
            .filter_map(|d| d.id.as_ref())
            .map(|s| s.as_str())
            .collect();

        assert_eq!(ids, vec!["first", "second", "third"]);
    }

    #[test]
    fn test_mixed_element_and_text_nodes() {
        let generator = BoxGenerator::new();

        let text1 = DOMNode::new_text("Before", default_style());
        let em = DOMNode::new_element("em", style_with_display(Display::Inline), vec![]);
        let text2 = DOMNode::new_text("After", default_style());

        let p = DOMNode::new_element("p", style_with_display(Display::Block), vec![text1, em, text2]);

        let box_tree = generator.generate(&p).unwrap();

        // p + text + em + text = 4 boxes
        assert_eq!(box_tree.count_boxes(), 4);
        assert_eq!(box_tree.count_text_boxes(), 2);
        assert_eq!(box_tree.root.children.len(), 3);
    }

    #[test]
    fn test_generator_reuse() {
        let generator = BoxGenerator::new();

        // Reuse generator for multiple trees
        for i in 0..10 {
            let dom =
                DOMNode::new_element("div", style_with_display(Display::Block), vec![]).with_id(format!("div-{}", i));

            let result = generator.generate(&dom);
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_empty_element() {
        let generator = BoxGenerator::new();
        let dom = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);

        let box_tree = generator.generate(&dom).unwrap();
        assert_eq!(box_tree.count_boxes(), 1);
        assert_eq!(box_tree.root.children.len(), 0);
    }

    #[test]
    fn test_deeply_nested_structure() {
        let generator = BoxGenerator::new();

        // Create deeply nested: div > div > div > div > p
        let mut current = DOMNode::new_element("p", style_with_display(Display::Block), vec![]);
        for _ in 0..4 {
            current = DOMNode::new_element("div", style_with_display(Display::Block), vec![current]);
        }

        let box_tree = generator.generate(&current).unwrap();
        assert_eq!(box_tree.count_boxes(), 5); // 4 divs + 1 p
    }

    #[test]
    fn test_flex_container() {
        let generator = BoxGenerator::new();

        let child = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);
        let flex_container = DOMNode::new_element("div", style_with_display(Display::Flex), vec![child]);

        let box_tree = generator.generate(&flex_container).unwrap();

        assert_eq!(box_tree.count_boxes(), 2);
        assert_eq!(box_tree.root.formatting_context(), Some(FormattingContextType::Flex));
    }

    #[test]
    fn test_grid_container() {
        let generator = BoxGenerator::new();

        let child = DOMNode::new_element("div", style_with_display(Display::Block), vec![]);
        let grid_container = DOMNode::new_element("div", style_with_display(Display::Grid), vec![child]);

        let box_tree = generator.generate(&grid_container).unwrap();

        assert_eq!(box_tree.count_boxes(), 2);
        assert_eq!(box_tree.root.formatting_context(), Some(FormattingContextType::Grid));
    }

    #[test]
    fn test_root_display_none_error() {
        let generator = BoxGenerator::new();
        let dom = DOMNode::new_element("div", style_with_display(Display::None), vec![]);

        let result = generator.generate(&dom);
        assert!(result.is_err());
        assert!(matches!(result, Err(BoxGenerationError::RootDisplayNone)));
    }

    #[test]
    fn test_root_display_contents_error() {
        let generator = BoxGenerator::new();
        let dom = DOMNode::new_element("div", style_with_display(Display::Contents), vec![]);

        let result = generator.generate(&dom);
        assert!(result.is_err());
        assert!(matches!(result, Err(BoxGenerationError::RootDisplayContents)));
    }

    // =============================================================================
    // Replaced Element Tests
    // =============================================================================

    #[test]
    fn test_dom_node_is_replaced_element() {
        let style = default_style();

        // Test replaced elements
        assert!(DOMNode::new_element("img", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("video", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("canvas", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("svg", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("iframe", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("embed", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("object", style.clone(), vec![]).is_replaced_element());
        assert!(DOMNode::new_element("audio", style.clone(), vec![]).is_replaced_element());

        // Test non-replaced elements
        assert!(!DOMNode::new_element("div", style.clone(), vec![]).is_replaced_element());
        assert!(!DOMNode::new_element("span", style.clone(), vec![]).is_replaced_element());
        assert!(!DOMNode::new_element("p", style.clone(), vec![]).is_replaced_element());
        assert!(!DOMNode::new_text("text", style.clone()).is_replaced_element());
    }

    #[test]
    fn test_new_replaced_constructor() {
        let style = default_style();
        let size = Size::new(100.0, 50.0);

        let img = DOMNode::new_replaced("img", style.clone(), "image.png", Some(size));

        assert_eq!(img.tag_name.as_deref(), Some("img"));
        assert_eq!(img.src.as_deref(), Some("image.png"));
        assert_eq!(img.intrinsic_size, Some(size));
        assert!(img.is_replaced_element());
    }

    #[test]
    fn test_replaced_element_aspect_ratio() {
        let style = default_style();
        let size = Size::new(100.0, 50.0);

        let img = DOMNode::new_replaced("img", style.clone(), "test.png", Some(size));
        assert_eq!(img.aspect_ratio(), Some(2.0)); // 100/50 = 2

        // Test with no intrinsic size
        let img_no_size = DOMNode::new_replaced("img", style.clone(), "test.png", None);
        assert_eq!(img_no_size.aspect_ratio(), None);

        // Test with zero height
        let zero_height = DOMNode::new_replaced("img", style, "test.png", Some(Size::new(100.0, 0.0)));
        assert_eq!(zero_height.aspect_ratio(), None);
    }

    #[test]
    fn test_replaced_element_type_detection() {
        let style = default_style();

        let img = DOMNode::new_replaced("img", style.clone(), "test.png", None);
        assert!(matches!(img.replaced_type(), Some(ReplacedType::Image { .. })));

        let video = DOMNode::new_replaced("video", style.clone(), "test.mp4", None);
        assert!(matches!(video.replaced_type(), Some(ReplacedType::Video { .. })));

        let canvas = DOMNode::new_element("canvas", style.clone(), vec![]);
        assert!(matches!(canvas.replaced_type(), Some(ReplacedType::Canvas)));

        let div = DOMNode::new_element("div", style, vec![]);
        assert!(div.replaced_type().is_none());
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
            for child in &node.children {
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
            for child in &node.children {
                count += count_object_replacements(child);
            }
            count
        }

        fn collect_text(node: &BoxNode, out: &mut Vec<String>) {
            if let BoxType::Text(text) = &node.box_type {
                out.push(text.text.clone());
            }
            for child in &node.children {
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
    fn test_generate_replaced_box() {
        let generator = BoxGenerator::new();
        let style = default_style();
        let size = Size::new(200.0, 100.0);

        let img = DOMNode::new_replaced("img", style.clone(), "test.png", Some(size));
        let wrapper = DOMNode::new_element("div", style_with_display(Display::Block), vec![img]);

        let box_tree = generator.generate(&wrapper).unwrap();

        // Root (div) + img
        assert_eq!(box_tree.count_boxes(), 2);

        // Check that the img is a replaced box
        let img_box = &box_tree.root.children[0];
        assert!(img_box.is_replaced());
    }

    #[test]
    fn test_generate_multiple_replaced_elements() {
        let generator = BoxGenerator::new();
        let style = default_style();

        let img1 = DOMNode::new_replaced("img", style.clone(), "img1.png", Some(Size::new(100.0, 100.0)));
        let video = DOMNode::new_replaced("video", style.clone(), "video.mp4", Some(Size::new(640.0, 480.0)));
        let img2 = DOMNode::new_replaced("img", style.clone(), "img2.png", None);

        let container = DOMNode::new_element("div", style_with_display(Display::Block), vec![img1, video, img2]);

        let box_tree = generator.generate(&container).unwrap();

        assert_eq!(box_tree.count_boxes(), 4); // container + 3 replaced
        assert_eq!(BoxGenerator::find_replaced_boxes(&box_tree.root).len(), 3);
    }

    #[test]
    fn test_replaced_element_with_display_none_child() {
        let generator = BoxGenerator::new();
        let style = default_style();

        // Even though img has children (which shouldn't happen in real HTML),
        // the generator should handle replaced elements without processing children
        let img = DOMNode::new_replaced("img", style.clone(), "test.png", None);
        let container = DOMNode::new_element("div", style_with_display(Display::Block), vec![img]);

        let box_tree = generator.generate(&container).unwrap();

        assert_eq!(box_tree.count_boxes(), 2);
        let replaced_boxes = BoxGenerator::find_replaced_boxes(&box_tree.root);
        assert_eq!(replaced_boxes.len(), 1);
        assert!(replaced_boxes[0].children.is_empty());
    }

    #[test]
    fn test_replaced_element_inline_context() {
        let generator = BoxGenerator::new();
        let style = default_style();

        // img in inline context
        let text = DOMNode::new_text("Hello ", style.clone());
        let img = DOMNode::new_replaced("img", style.clone(), "icon.png", Some(Size::new(16.0, 16.0)));
        let text2 = DOMNode::new_text(" World", style.clone());

        let span = DOMNode::new_element("span", style_with_display(Display::Inline), vec![text, img, text2]);
        let p = DOMNode::new_element("p", style_with_display(Display::Block), vec![span]);

        let box_tree = generator.generate(&p).unwrap();

        // p + span + 2 text + 1 img = 5
        assert_eq!(box_tree.count_boxes(), 5);
        assert_eq!(BoxGenerator::find_replaced_boxes(&box_tree.root).len(), 1);
        assert_eq!(BoxGenerator::find_text_boxes(&box_tree.root).len(), 2);
    }

    fn styled_element(tag: &str) -> StyledNode {
        StyledNode {
            node: dom::DomNode {
                node_type: dom::DomNodeType::Element {
                    tag_name: tag.to_string(),
                    attributes: vec![],
                },
                children: vec![],
            },
            styles: ComputedStyle::default(),
            before_styles: None,
            after_styles: None,
            marker_styles: None,
            children: vec![],
        }
    }

    #[test]
    fn replaced_media_defaults_to_300_by_150() {
        let style = default_style();

        for tag in ["canvas", "video", "iframe", "embed", "object"] {
            let styled = styled_element(tag);
            let box_node = create_replaced_box_from_styled(&styled, style.clone());
            match &box_node.box_type {
                BoxType::Replaced(replaced) => {
                    assert_eq!(
                        replaced.intrinsic_size,
                        Some(Size::new(300.0, 150.0)),
                        "{tag} should default to 300x150"
                    );
                    assert_eq!(replaced.aspect_ratio, Some(2.0), "{tag} should default to 2:1 ratio");
                }
                other => panic!("expected replaced box for {tag}, got {:?}", other),
            }
        }
    }

    #[test]
    fn display_contents_splices_children_into_parent() {
        let mut root = styled_element("div");
        root.styles.display = Display::Block;

        let mut contents = styled_element("section");
        contents.styles.display = Display::Contents;

        let mut child1 = styled_element("p");
        child1.styles.display = Display::Block;
        let mut child2 = styled_element("p");
        child2.styles.display = Display::Block;

        contents.children = vec![child1, child2];
        root.children = vec![contents];

        let tree = generate_box_tree(&root);
        assert_eq!(tree.root.children.len(), 2, "contents element should not create a box");
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
        root.styles.display = Display::Block;
        let mut child = styled_element("span");
        child.styles.display = Display::Inline;
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

    #[test]
    fn test_find_boxes_by_predicate() {
        let style = default_style();

        let text1 = BoxNode::new_text(style.clone(), "Hello".to_string());
        let text2 = BoxNode::new_text(style.clone(), "World".to_string());
        let inline = BoxNode::new_inline(style.clone(), vec![text1]);
        let block = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![inline, text2]);

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
        let root = BoxNode::new_block(style, FormattingContextType::Block, vec![text, inline, block1, block2]);

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
            node: dom::DomNode {
                node_type: dom::DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            },
            styles: li_style,
            before_styles: None,
            after_styles: None,
            marker_styles: None,
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
        assert_eq!(marker.style.background_color, crate::style::color::Rgba::TRANSPARENT);
    }

    #[test]
    fn marker_styles_keep_text_decorations_and_shadows() {
        use crate::css::types::TextShadow;
        use crate::style::color::Rgba;
        use crate::style::counters::CounterManager;
        use crate::style::types::{
            ListStyleType, TextDecorationLine, TextDecorationStyle, TextDecorationThickness,
        };
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
        marker_styles.text_shadow.push(TextShadow {
            offset_x: Length::px(1.0),
            offset_y: Length::px(2.0),
            blur_radius: Length::px(3.0),
            color: Some(Rgba::GREEN),
        });
        marker_styles.padding_left = Length::px(8.0);
        marker_styles.margin_left = Some(Length::px(4.0));
        marker_styles.background_color = Rgba::rgb(255, 0, 255);

        let styled = StyledNode {
            node: dom::DomNode {
                node_type: dom::DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            },
            styles: li_style,
            before_styles: None,
            after_styles: None,
            marker_styles: Some(marker_styles),
            children: vec![],
        };

        let marker_box =
            create_marker_box(&styled, &CounterManager::default()).expect("marker should be generated");
        let style = marker_box.style.as_ref();
        assert!(style.text_decoration.lines.contains(TextDecorationLine::UNDERLINE));
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
    fn test_validate_box_tree_valid() {
        let style = default_style();

        let text = BoxNode::new_text(style.clone(), "Hello".to_string());
        let inline = BoxNode::new_inline(style.clone(), vec![text]);
        let root = BoxNode::new_block(style, FormattingContextType::Block, vec![inline]);

        assert!(BoxGenerator::validate_box_tree(&root).is_ok());
    }

    #[test]
    fn test_tree_depth() {
        let style = default_style();

        // Single node = depth 1
        let single = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
        assert_eq!(BoxGenerator::tree_depth(&single), 1);

        // 2 levels
        let child = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
        let root = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![child]);
        assert_eq!(BoxGenerator::tree_depth(&root), 2);

        // 4 levels deep
        let leaf = BoxNode::new_text(style.clone(), "text".to_string());
        let level3 = BoxNode::new_inline(style.clone(), vec![leaf]);
        let level2 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![level3]);
        let level1 = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![level2]);
        assert_eq!(BoxGenerator::tree_depth(&level1), 4);
    }

    #[test]
    fn test_with_builder_methods() {
        let style = default_style();
        let size = Size::new(50.0, 25.0);

        let node = DOMNode::new_element("img", style, vec![])
            .with_id("my-image")
            .with_class("thumbnail")
            .with_class("responsive")
            .with_src("path/to/image.png")
            .with_intrinsic_size(size);

        assert_eq!(node.id.as_deref(), Some("my-image"));
        assert_eq!(node.classes.len(), 2);
        assert_eq!(node.src.as_deref(), Some("path/to/image.png"));
        assert_eq!(node.intrinsic_size, Some(size));
    }

    // =============================================================================
    // Edge Case Tests
    // =============================================================================

    #[test]
    fn test_empty_replaced_src() {
        let style = default_style();

        let img = DOMNode::new_replaced("img", style.clone(), "", None);
        let wrapper = DOMNode::new_element("div", style_with_display(Display::Block), vec![img]);

        let generator = BoxGenerator::new();
        let box_tree = generator.generate(&wrapper).unwrap();

        assert_eq!(box_tree.count_boxes(), 2);
    }

    #[test]
    fn test_mixed_content_with_replaced() {
        let generator = BoxGenerator::new();
        let style = default_style();

        // Create: div > text + img + text + video + div
        let text1 = DOMNode::new_text("Before image ", style.clone());
        let img = DOMNode::new_replaced("img", style.clone(), "test.png", Some(Size::new(100.0, 50.0)));
        let text2 = DOMNode::new_text(" after image ", style.clone());
        let video = DOMNode::new_replaced("video", style.clone(), "test.mp4", None);
        let nested_div = DOMNode::new_element(
            "div",
            style_with_display(Display::Block),
            vec![DOMNode::new_text("Nested", style.clone())],
        );

        let root = DOMNode::new_element(
            "div",
            style_with_display(Display::Block),
            vec![text1, img, text2, video, nested_div],
        );

        let box_tree = generator.generate(&root).unwrap();

        // root + 2 text + img + video + nested_div + nested_text = 7
        assert_eq!(box_tree.count_boxes(), 7);
        assert_eq!(BoxGenerator::find_replaced_boxes(&box_tree.root).len(), 2);
        assert_eq!(BoxGenerator::find_text_boxes(&box_tree.root).len(), 3);
    }

    #[test]
    fn test_svg_replaced_element() {
        let generator = BoxGenerator::new();
        let style = default_style();

        let svg = DOMNode::new_replaced("svg", style.clone(), "<svg>...</svg>", Some(Size::new(100.0, 100.0)));
        let wrapper = DOMNode::new_element("div", style_with_display(Display::Block), vec![svg]);

        let box_tree = generator.generate(&wrapper).unwrap();

        assert_eq!(BoxGenerator::find_replaced_boxes(&box_tree.root).len(), 1);
    }

    #[test]
    fn pseudo_content_respects_quotes_property() {
        use crate::dom::DomNodeType;
        use crate::style::content::{ContentItem, ContentValue};

        let mut before_style = ComputedStyle::default();
        before_style.content_value =
            ContentValue::Items(vec![ContentItem::OpenQuote, ContentItem::String("hi".to_string())]);
        before_style.quotes = vec![("«".to_string(), "»".to_string())];

        let base_style = ComputedStyle::default();
        let styled = StyledNode {
            node: dom::DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "div".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            },
            styles: base_style,
            before_styles: Some(before_style),
            after_styles: None,
            marker_styles: None,
            children: vec![],
        };

        let mut counters = CounterManager::new();
        counters.enter_scope();
        let before_box =
            create_pseudo_element_box(&styled, styled.before_styles.as_ref().unwrap(), "before", &counters)
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
        use crate::style::content::{ContentItem, ContentValue};

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
                style: Some(CounterStyle::UpperRoman),
            },
            ContentItem::String(" ".to_string()),
            ContentItem::Url("icon.png".to_string()),
        ]);

        let base_style = ComputedStyle::default();
        let styled = StyledNode {
            node: dom::DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "div".to_string(),
                    attributes: vec![("data-label".to_string(), "hello".to_string())],
                },
                children: vec![],
            },
            styles: base_style,
            before_styles: Some(before_style),
            after_styles: None,
            marker_styles: None,
            children: vec![],
        };

        let mut counters = CounterManager::new();
        counters.enter_scope();
        counters.apply_reset(&CounterSet::single("item", 3));

        let before_box =
            create_pseudo_element_box(&styled, styled.before_styles.as_ref().unwrap(), "before", &counters)
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
        use crate::style::content::{ContentItem, ContentValue};

        let mut marker_style = ComputedStyle::default();
        marker_style.content_value = ContentValue::Items(vec![
            ContentItem::String("[".to_string()),
            ContentItem::Counter {
                name: "item".to_string(),
                style: Some(CounterStyle::LowerRoman),
            },
            ContentItem::String("]".to_string()),
        ]);

        let base_style = ComputedStyle::default();
        let styled = StyledNode {
            node: dom::DomNode {
                node_type: DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![],
                },
                children: vec![],
            },
            styles: base_style,
            before_styles: None,
            after_styles: None,
            marker_styles: Some(marker_style),
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
                _ => panic!("expected text marker"),
            },
            _ => panic!("expected marker box"),
        }
    }

    #[test]
    fn ordered_list_start_attribute_sets_initial_counter() {
        let mut ol_style = ComputedStyle::default();
        ol_style.display = Display::Block;

        let mut li_style = ComputedStyle::default();
        li_style.display = Display::ListItem;
        li_style.list_style_type = ListStyleType::Decimal;

        let ol_dom = dom::DomNode {
            node_type: dom::DomNodeType::Element {
                tag_name: "ol".to_string(),
                attributes: vec![("start".to_string(), "5".to_string())],
            },
            children: vec![],
        };

        let mk_li = |text: &str| StyledNode {
            node: dom::DomNode {
                node_type: dom::DomNodeType::Element {
                    tag_name: "li".to_string(),
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
            before_styles: None,
            after_styles: None,
            marker_styles: None,
            children: vec![StyledNode {
                node: dom::DomNode {
                    node_type: dom::DomNodeType::Text {
                        content: text.to_string(),
                    },
                    children: vec![],
                },
                styles: ComputedStyle::default(),
                before_styles: None,
                after_styles: None,
                marker_styles: None,
                children: vec![],
            }],
        };

        let ol = StyledNode {
            node: ol_dom,
            styles: ol_style,
            before_styles: None,
            after_styles: None,
            marker_styles: None,
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
                        _ => None,
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

        let mut li_style = ComputedStyle::default();
        li_style.display = Display::ListItem;
        li_style.list_style_type = ListStyleType::Decimal;

        let ol_dom = dom::DomNode {
            node_type: dom::DomNodeType::Element {
                tag_name: "ol".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let mk_li = |text: &str| StyledNode {
            node: dom::DomNode {
                node_type: dom::DomNodeType::Element {
                    tag_name: "li".to_string(),
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
            before_styles: None,
            after_styles: None,
            marker_styles: None,
            children: vec![StyledNode {
                node: dom::DomNode {
                    node_type: dom::DomNodeType::Text {
                        content: text.to_string(),
                    },
                    children: vec![],
                },
                styles: ComputedStyle::default(),
                before_styles: None,
                after_styles: None,
                marker_styles: None,
                children: vec![],
            }],
        };

        let ol = StyledNode {
            node: ol_dom,
            styles: ol_style,
            before_styles: None,
            after_styles: None,
            marker_styles: None,
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
                        _ => None,
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

        let mut li_style = ComputedStyle::default();
        li_style.display = Display::ListItem;
        li_style.list_style_type = ListStyleType::Decimal;

        let ol_dom = dom::DomNode {
            node_type: dom::DomNodeType::Element {
                tag_name: "ol".to_string(),
                attributes: vec![("reversed".to_string(), "".to_string())],
            },
            children: vec![],
        };

        let mk_li = |text: &str| StyledNode {
            node: dom::DomNode {
                node_type: dom::DomNodeType::Element {
                    tag_name: "li".to_string(),
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
            before_styles: None,
            after_styles: None,
            marker_styles: None,
            children: vec![StyledNode {
                node: dom::DomNode {
                    node_type: dom::DomNodeType::Text {
                        content: text.to_string(),
                    },
                    children: vec![],
                },
                styles: ComputedStyle::default(),
                before_styles: None,
                after_styles: None,
                marker_styles: None,
                children: vec![],
            }],
        };

        let ol = StyledNode {
            node: ol_dom,
            styles: ol_style,
            before_styles: None,
            after_styles: None,
            marker_styles: None,
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
                        _ => None,
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

        let mut li_style = ComputedStyle::default();
        li_style.display = Display::ListItem;
        li_style.list_style_type = ListStyleType::Decimal;

        let ol_dom = dom::DomNode {
            node_type: dom::DomNodeType::Element {
                tag_name: "ol".to_string(),
                attributes: vec![],
            },
            children: vec![],
        };

        let mk_li = |text: &str, value: Option<&str>| StyledNode {
            node: dom::DomNode {
                node_type: dom::DomNodeType::Element {
                    tag_name: "li".to_string(),
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
            before_styles: None,
            after_styles: None,
            marker_styles: None,
            children: vec![StyledNode {
                node: dom::DomNode {
                    node_type: dom::DomNodeType::Text {
                        content: text.to_string(),
                    },
                    children: vec![],
                },
                styles: ComputedStyle::default(),
                before_styles: None,
                after_styles: None,
                marker_styles: None,
                children: vec![],
            }],
        };

        let ol = StyledNode {
            node: ol_dom,
            styles: ol_style,
            before_styles: None,
            after_styles: None,
            marker_styles: None,
            children: vec![mk_li("one", None), mk_li("two", Some("10")), mk_li("three", None)],
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
                        _ => None,
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

        let mut li_style = ComputedStyle::default();
        li_style.display = Display::ListItem;
        li_style.list_style_type = ListStyleType::Decimal;

        let ol_dom = dom::DomNode {
            node_type: dom::DomNodeType::Element {
                tag_name: "ol".to_string(),
                attributes: vec![("reversed".to_string(), "".to_string())],
            },
            children: vec![],
        };

        let mk_li = |text: &str, value: Option<&str>| StyledNode {
            node: dom::DomNode {
                node_type: dom::DomNodeType::Element {
                    tag_name: "li".to_string(),
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
            before_styles: None,
            after_styles: None,
            marker_styles: None,
            children: vec![StyledNode {
                node: dom::DomNode {
                    node_type: dom::DomNodeType::Text {
                        content: text.to_string(),
                    },
                    children: vec![],
                },
                styles: ComputedStyle::default(),
                before_styles: None,
                after_styles: None,
                marker_styles: None,
                children: vec![],
            }],
        };

        let ol = StyledNode {
            node: ol_dom,
            styles: ol_style,
            before_styles: None,
            after_styles: None,
            marker_styles: None,
            children: vec![mk_li("one", None), mk_li("two", Some("10")), mk_li("three", None)],
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
                        _ => None,
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

        let mut li_style = ComputedStyle::default();
        li_style.display = Display::ListItem;
        li_style.list_style_type = ListStyleType::Decimal;

        let nested_ol = StyledNode {
            node: dom::DomNode {
                node_type: dom::DomNodeType::Element {
                    tag_name: "ol".to_string(),
                    attributes: vec![("reversed".to_string(), "".to_string())],
                },
                children: vec![],
            },
            styles: ol_style.clone(),
            before_styles: None,
            after_styles: None,
            marker_styles: None,
            children: vec![StyledNode {
                node: dom::DomNode {
                    node_type: dom::DomNodeType::Element {
                        tag_name: "li".to_string(),
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
                before_styles: None,
                after_styles: None,
                marker_styles: None,
                children: vec![StyledNode {
                    node: dom::DomNode {
                        node_type: dom::DomNodeType::Text {
                            content: "inner".to_string(),
                        },
                        children: vec![],
                    },
                    styles: ComputedStyle::default(),
                    before_styles: None,
                    after_styles: None,
                    marker_styles: None,
                    children: vec![],
                }],
            }],
        };

        let ol_dom = dom::DomNode {
            node_type: dom::DomNodeType::Element {
                tag_name: "ol".to_string(),
                attributes: vec![("reversed".to_string(), "".to_string())],
            },
            children: vec![],
        };

        let outer_li = |child: StyledNode| StyledNode {
            node: dom::DomNode {
                node_type: dom::DomNodeType::Element {
                    tag_name: "li".to_string(),
                    attributes: vec![],
                },
                children: vec![child.node.clone()],
            },
            styles: li_style.clone(),
            before_styles: None,
            after_styles: None,
            marker_styles: None,
            children: vec![child],
        };

        let ol = StyledNode {
            node: ol_dom,
            styles: ol_style,
            before_styles: None,
            after_styles: None,
            marker_styles: None,
            children: vec![
                outer_li(StyledNode {
                    node: dom::DomNode {
                        node_type: dom::DomNodeType::Text {
                            content: "outer1".to_string(),
                        },
                        children: vec![],
                    },
                    styles: ComputedStyle::default(),
                    before_styles: None,
                    after_styles: None,
                    marker_styles: None,
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
                        _ => None,
                    },
                    _ => None,
                })
            })
            .collect();

        assert_eq!(markers, vec!["2 ", "1 "]);
    }

    #[test]
    fn test_iframe_replaced_element() {
        let generator = BoxGenerator::new();
        let style = default_style();

        let iframe = DOMNode::new_replaced(
            "iframe",
            style.clone(),
            "https://example.com",
            Some(Size::new(300.0, 200.0)),
        );
        let wrapper = DOMNode::new_element("div", style_with_display(Display::Block), vec![iframe]);

        let box_tree = generator.generate(&wrapper).unwrap();

        let replaced = BoxGenerator::find_replaced_boxes(&box_tree.root);
        assert_eq!(replaced.len(), 1);
    }

    #[test]
    fn test_list_item_generates_marker() {
        let generator = BoxGenerator::new();
        let mut li_style = ComputedStyle::default();
        li_style.display = Display::ListItem;
        li_style.list_style_type = ListStyleType::Decimal;
        li_style.list_style_position = ListStylePosition::Inside;
        let li_style = Arc::new(li_style);

        let li1 = DOMNode::new_element(
            "li",
            li_style.clone(),
            vec![DOMNode::new_text("First", li_style.clone())],
        );
        let li2 = DOMNode::new_element(
            "li",
            li_style.clone(),
            vec![DOMNode::new_text("Second", li_style.clone())],
        );
        let ul = DOMNode::new_element("ul", style_with_display(Display::Block), vec![li1, li2]);

        let tree = generator.generate(&ul).unwrap();
        let first_li = &tree.root.children[0];
        if let BoxType::Marker(marker_box) = &first_li.children[0].box_type {
            if let MarkerContent::Text(text) = &marker_box.content {
                assert_eq!(text, "1 ");
            } else {
                panic!("expected text marker");
            }
        } else {
            panic!("expected marker text box");
        }
        if let BoxType::Marker(marker_box) = &tree.root.children[1].children[0].box_type {
            if let MarkerContent::Text(text) = &marker_box.content {
                assert_eq!(text, "2 ");
            } else {
                panic!("expected text marker");
            }
        } else {
            panic!("expected marker text box");
        }
    }

    #[test]
    fn markers_support_armenian_and_georgian_styles() {
        let generator = BoxGenerator::new();

        let mut armenian_style = ComputedStyle::default();
        armenian_style.display = Display::ListItem;
        armenian_style.list_style_type = ListStyleType::Armenian;
        let armenian_style = Arc::new(armenian_style);

        let armenian_list = DOMNode::new_element(
            "ol",
            style_with_display(Display::Block),
            vec![DOMNode::new_element(
                "li",
                armenian_style.clone(),
                vec![DOMNode::new_text("item", armenian_style.clone())],
            )],
        );
        let armenian_tree = generator.generate(&armenian_list).unwrap();
        if let BoxType::Marker(marker_box) = &armenian_tree.root.children[0].children[0].box_type {
            if let MarkerContent::Text(text) = &marker_box.content {
                assert_eq!(text, "Ա ");
            } else {
                panic!("expected armenian marker text");
            }
        } else {
            panic!("expected armenian marker");
        }

        let mut georgian_style = ComputedStyle::default();
        georgian_style.display = Display::ListItem;
        georgian_style.list_style_type = ListStyleType::Georgian;
        let georgian_style = Arc::new(georgian_style);

        let georgian_list = DOMNode::new_element(
            "ol",
            style_with_display(Display::Block),
            vec![DOMNode::new_element(
                "li",
                georgian_style.clone(),
                vec![DOMNode::new_text("item", georgian_style.clone())],
            )],
        );
        let georgian_tree = generator.generate(&georgian_list).unwrap();
        if let BoxType::Marker(marker_box) = &georgian_tree.root.children[0].children[0].box_type {
            if let MarkerContent::Text(text) = &marker_box.content {
                assert_eq!(text, "ა ");
            } else {
                panic!("expected georgian marker text");
            }
        } else {
            panic!("expected georgian marker");
        }
    }

    #[test]
    fn markers_support_disclosure_styles() {
        let generator = BoxGenerator::new();

        let mut open_style = ComputedStyle::default();
        open_style.display = Display::ListItem;
        open_style.list_style_type = ListStyleType::DisclosureOpen;
        let open_style = Arc::new(open_style);

        let mut closed_style = ComputedStyle::default();
        closed_style.display = Display::ListItem;
        closed_style.list_style_type = ListStyleType::DisclosureClosed;
        let closed_style = Arc::new(closed_style);

        let open = DOMNode::new_element(
            "li",
            open_style.clone(),
            vec![DOMNode::new_text("Open".to_string(), open_style.clone())],
        );
        let closed = DOMNode::new_element(
            "li",
            closed_style.clone(),
            vec![DOMNode::new_text("Closed".to_string(), closed_style.clone())],
        );
        let ul = DOMNode::new_element("ul", style_with_display(Display::Block), vec![open, closed]);

        let tree = generator.generate(&ul).unwrap();
        let open_marker = match &tree.root.children[0].children[0].box_type {
            BoxType::Marker(marker) => match &marker.content {
                MarkerContent::Text(t) => t.clone(),
                other => panic!("expected text marker, got {:?}", other),
            },
            other => panic!("expected marker, got {:?}", other),
        };
        let closed_marker = match &tree.root.children[1].children[0].box_type {
            BoxType::Marker(marker) => match &marker.content {
                MarkerContent::Text(t) => t.clone(),
                other => panic!("expected text marker, got {:?}", other),
            },
            other => panic!("expected marker, got {:?}", other),
        };

        assert_eq!(open_marker, "▾ ");
        assert_eq!(closed_marker, "▸ ");
    }

    #[test]
    fn list_counters_respect_resets_and_custom_increments() {
        let generator = BoxGenerator::new();

        let mut container_style = ComputedStyle::default();
        container_style.display = Display::Block;
        container_style.counters.counter_reset = Some(CounterSet::single("list-item", 4));
        let container_style = Arc::new(container_style);

        let mut li_style = ComputedStyle::default();
        li_style.display = Display::ListItem;
        li_style.list_style_type = ListStyleType::Decimal;
        let li_style = Arc::new(li_style);

        let first = DOMNode::new_element(
            "li",
            li_style.clone(),
            vec![DOMNode::new_text("first", li_style.clone())],
        );

        let mut custom_increment = (*li_style).clone();
        custom_increment.counters.counter_increment = Some(CounterSet::single("list-item", 2));
        let custom_increment = Arc::new(custom_increment);
        let second = DOMNode::new_element(
            "li",
            custom_increment.clone(),
            vec![DOMNode::new_text("second", custom_increment.clone())],
        );

        let list = DOMNode::new_element("ol", container_style, vec![first, second]);
        let tree = generator.generate(&list).unwrap();

        if let BoxType::Marker(text_box) = &tree.root.children[0].children[0].box_type {
            if let MarkerContent::Text(text) = &text_box.content {
                assert_eq!(text, "5 ");
            } else {
                panic!("expected text marker");
            }
        } else {
            panic!("expected first marker text");
        }

        if let BoxType::Marker(text_box) = &tree.root.children[1].children[0].box_type {
            if let MarkerContent::Text(text) = &text_box.content {
                assert_eq!(text, "7 ");
            } else {
                panic!("expected text marker");
            }
        } else {
            panic!("expected second marker text");
        }
    }

    #[test]
    fn nested_lists_reset_counters() {
        let generator = BoxGenerator::new();

        let mut li_style = ComputedStyle::default();
        li_style.display = Display::ListItem;
        li_style.list_style_type = ListStyleType::Decimal;
        let li_style = Arc::new(li_style);

        let inner_li = DOMNode::new_element(
            "li",
            li_style.clone(),
            vec![DOMNode::new_text("inner", li_style.clone())],
        );
        let inner_list = DOMNode::new_element("ul", style_with_display(Display::Block), vec![inner_li]);

        let outer_li = DOMNode::new_element(
            "li",
            li_style.clone(),
            vec![DOMNode::new_text("outer", li_style.clone()), inner_list],
        );
        let outer = DOMNode::new_element("ul", style_with_display(Display::Block), vec![outer_li]);

        let tree = generator.generate(&outer).unwrap();

        // Outer marker should start at 1
        if let BoxType::Marker(text_box) = &tree.root.children[0].children[0].box_type {
            if let MarkerContent::Text(text) = &text_box.content {
                assert_eq!(text, "1 ");
            } else {
                panic!("expected text marker");
            }
        } else {
            panic!("expected outer marker text");
        }

        let inner_list_box = tree.root.children[0]
            .children
            .iter()
            .find(|child| matches!(child.box_type, BoxType::Block(_)))
            .expect("inner list block");
        if let BoxType::Marker(text_box) = &inner_list_box.children[0].children[0].box_type {
            if let MarkerContent::Text(text) = &text_box.content {
                assert_eq!(text, "1 ");
            } else {
                panic!("expected text marker");
            }
        } else {
            panic!("expected inner marker text");
        }
    }

    #[test]
    fn marker_ignores_text_transform() {
        let generator = BoxGenerator::new();
        let mut li_style = ComputedStyle::default();
        li_style.display = Display::ListItem;
        li_style.list_style_type = ListStyleType::LowerAlpha;
        li_style.text_transform = TextTransform::with_case(CaseTransform::Uppercase);
        let li_style = Arc::new(li_style);

        let li = DOMNode::new_element(
            "li",
            li_style.clone(),
            vec![DOMNode::new_text("item", li_style.clone())],
        );
        let ul = DOMNode::new_element("ul", style_with_display(Display::Block), vec![li]);

        let tree = generator.generate(&ul).unwrap();
        if let BoxType::Marker(marker) = &tree.root.children[0].children[0].box_type {
            if let MarkerContent::Text(text) = &marker.content {
                assert_eq!(text, "a ");
            } else {
                panic!("expected text marker");
            }
        } else {
            panic!("expected marker");
        }
    }

    #[test]
    fn list_style_image_produces_image_marker() {
        let generator = BoxGenerator::new();
        let mut li_style = ComputedStyle::default();
        li_style.display = Display::ListItem;
        li_style.list_style_image = crate::style::types::ListStyleImage::Url("bullet.png".to_string());
        let li_style = Arc::new(li_style);

        let li = DOMNode::new_element(
            "li",
            li_style.clone(),
            vec![DOMNode::new_text("item", li_style.clone())],
        );
        let ul = DOMNode::new_element("ul", style_with_display(Display::Block), vec![li]);

        let tree = generator.generate(&ul).unwrap();
        match &tree.root.children[0].children[0].box_type {
            BoxType::Marker(marker) => match &marker.content {
                MarkerContent::Image(replaced) => {
                    if let ReplacedType::Image { src, .. } = &replaced.replaced_type {
                        assert_eq!(src, "bullet.png");
                    } else {
                        panic!("expected image marker type");
                    }
                }
                other => panic!("expected image marker, got {:?}", other),
            },
            other => panic!("expected marker, got {:?}", other),
        }
    }

    #[test]
    fn inline_svg_carries_serialized_content() {
        let html =
            r#"<html><body><svg width="10" height="10"><rect width="10" height="10" fill="red"/></svg></body></html>"#;
        let dom = crate::dom::parse_html(html).expect("parse");
        let styled = crate::style::cascade::apply_styles(&dom, &crate::css::types::StyleSheet::new());
        let box_tree = generate_box_tree(&styled);

        fn find_svg(node: &BoxNode) -> Option<&ReplacedBox> {
            if let BoxType::Replaced(repl) = &node.box_type {
                if matches!(repl.replaced_type, ReplacedType::Svg { .. }) {
                    return Some(repl);
                }
            }
            for child in &node.children {
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
                    content.contains("<rect") && content.contains("fill=\"red\""),
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
        ListStyleType::Disc => "•".to_string(),
        ListStyleType::Circle => "◦".to_string(),
        ListStyleType::Square => "▪".to_string(),
        ListStyleType::Decimal => counters.format("list-item", CounterStyle::Decimal),
        ListStyleType::DecimalLeadingZero => counters.format("list-item", CounterStyle::DecimalLeadingZero),
        ListStyleType::LowerRoman => counters.format("list-item", CounterStyle::LowerRoman),
        ListStyleType::UpperRoman => counters.format("list-item", CounterStyle::UpperRoman),
        ListStyleType::LowerAlpha => counters.format("list-item", CounterStyle::LowerAlpha),
        ListStyleType::UpperAlpha => counters.format("list-item", CounterStyle::UpperAlpha),
        ListStyleType::Armenian => counters.format("list-item", CounterStyle::Armenian),
        ListStyleType::LowerArmenian => counters.format("list-item", CounterStyle::LowerArmenian),
        ListStyleType::Georgian => counters.format("list-item", CounterStyle::Georgian),
        ListStyleType::LowerGreek => counters.format("list-item", CounterStyle::LowerGreek),
        ListStyleType::DisclosureOpen => "▾".to_string(),
        ListStyleType::DisclosureClosed => "▸".to_string(),
    };

    if core.is_empty() {
        core
    } else {
        format!("{} ", core)
    }
}
