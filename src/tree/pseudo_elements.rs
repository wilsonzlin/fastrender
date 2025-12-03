//! Pseudo-element box generation
//!
//! Implements CSS pseudo-element box generation for ::before and ::after.
//!
//! CSS Specification: CSS 2.1 Section 12.1 - The :before and :after pseudo-elements
//! <https://www.w3.org/TR/CSS21/generate.html#before-after-content>
//!
//! # Overview
//!
//! Pseudo-elements create generated content that is inserted into the document
//! tree as if it were a child of the originating element:
//!
//! - `::before` content is inserted as the first child
//! - `::after` content is inserted as the last child
//!
//! # Box Generation Rules
//!
//! Per CSS 2.1 Section 12.1:
//!
//! 1. The `content` property must have a value other than `none` or `normal`
//!    for the pseudo-element to generate a box.
//!
//! 2. The generated box inherits from its parent (the originating element).
//!
//! 3. The `display` property of the pseudo-element determines the box type:
//!    - Default is `inline`
//!    - Can be changed to `block`, `inline-block`, etc.
//!
//! # Example
//!
//! ```css
//! p::before {
//!     content: "» ";
//!     color: red;
//! }
//! ```
//!
//! Generates an inline box with text "» " as the first child of each `<p>`.

use crate::style::display::{Display, FormattingContextType};
use crate::style::ComputedStyle;
use crate::tree::box_tree::BoxNode;
use crate::tree::debug::DebugInfo;
use std::sync::Arc;

// =============================================================================
// Pseudo-Element Types
// =============================================================================

/// Types of CSS pseudo-elements
///
/// CSS 2.1 defines two pseudo-elements that generate content:
/// - `::before` - content inserted before element's children
/// - `::after` - content inserted after element's children
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PseudoElementType {
    /// `::before` pseudo-element
    ///
    /// Content is inserted as the first child of the originating element.
    Before,

    /// `::after` pseudo-element
    ///
    /// Content is inserted as the last child of the originating element.
    After,
}

impl PseudoElementType {
    /// Returns the CSS name of this pseudo-element
    pub fn css_name(&self) -> &'static str {
        match self {
            PseudoElementType::Before => "::before",
            PseudoElementType::After => "::after",
        }
    }

    /// Returns a short name for debugging
    pub fn short_name(&self) -> &'static str {
        match self {
            PseudoElementType::Before => "before",
            PseudoElementType::After => "after",
        }
    }
}

impl std::fmt::Display for PseudoElementType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.css_name())
    }
}

// =============================================================================
// Content Property Values
// =============================================================================

/// Represents the `content` property value for pseudo-elements
///
/// CSS 2.1 Section 12.2: The 'content' property
///
/// # Supported Values
///
/// - `none` - No content generated
/// - `normal` - Computes to `none` for pseudo-elements
/// - String content - Text to be displayed
/// - `attr()` - Attribute value (not yet implemented)
/// - URL - Image content (not yet implemented)
/// - Counters - `counter()` and `counters()` (not yet implemented)
#[derive(Debug, Clone, PartialEq)]
pub enum PseudoContent {
    /// No content generated
    ///
    /// The pseudo-element does not generate a box.
    None,

    /// Normal content
    ///
    /// For `:before` and `:after`, computes to `none`.
    Normal,

    /// String content
    ///
    /// The text to be displayed. May include escape sequences.
    ///
    /// # Example
    ///
    /// ```css
    /// content: "Hello, World!";
    /// content: "» ";
    /// content: "\201C"; /* Opening double quote */
    /// ```
    String(String),

    /// Multiple content items concatenated
    ///
    /// CSS allows multiple content values to be concatenated:
    ///
    /// ```css
    /// content: "(" attr(data-count) ")";
    /// ```
    Multiple(Vec<PseudoContentItem>),

    /// Open quote character
    ///
    /// Inserts the opening quote based on `quotes` property and nesting level.
    OpenQuote,

    /// Close quote character
    ///
    /// Inserts the closing quote based on `quotes` property and nesting level.
    CloseQuote,

    /// No open quote
    ///
    /// Increments quote nesting level without inserting content.
    NoOpenQuote,

    /// No close quote
    ///
    /// Decrements quote nesting level without inserting content.
    NoCloseQuote,
}

impl Default for PseudoContent {
    fn default() -> Self {
        PseudoContent::None
    }
}

impl PseudoContent {
    /// Creates a string content value
    pub fn string(s: impl Into<String>) -> Self {
        PseudoContent::String(s.into())
    }

    /// Returns true if this content value generates a box
    ///
    /// Content values `none` and `normal` do not generate boxes.
    pub fn generates_box(&self) -> bool {
        match self {
            PseudoContent::None | PseudoContent::Normal => false,
            PseudoContent::NoOpenQuote | PseudoContent::NoCloseQuote => false,
            _ => true,
        }
    }

    /// Resolves the content to a string
    ///
    /// Returns `None` if content doesn't generate text.
    pub fn to_text(&self) -> Option<String> {
        match self {
            PseudoContent::None | PseudoContent::Normal => None,
            PseudoContent::String(s) => Some(s.clone()),
            PseudoContent::Multiple(items) => {
                let mut result = String::new();
                for item in items {
                    if let Some(text) = item.to_text() {
                        result.push_str(&text);
                    }
                }
                if result.is_empty() {
                    None
                } else {
                    Some(result)
                }
            }
            PseudoContent::OpenQuote => Some("\u{201C}".to_string()), // Left double quote
            PseudoContent::CloseQuote => Some("\u{201D}".to_string()), // Right double quote
            PseudoContent::NoOpenQuote | PseudoContent::NoCloseQuote => None,
        }
    }
}

/// Individual items that can appear in `content` property
#[derive(Debug, Clone, PartialEq)]
pub enum PseudoContentItem {
    /// String literal
    String(String),

    /// Attribute value: `attr(name)`
    Attr(String),

    /// Counter value: `counter(name)`
    Counter(String),

    /// Counter value with style: `counter(name, style)`
    CounterStyled(String, String),

    /// Counters value: `counters(name, separator)`
    Counters(String, String),

    /// Open quote
    OpenQuote,

    /// Close quote
    CloseQuote,

    /// URL for image content
    Url(String),
}

impl PseudoContentItem {
    /// Resolves this item to text
    pub fn to_text(&self) -> Option<String> {
        match self {
            PseudoContentItem::String(s) => Some(s.clone()),
            PseudoContentItem::OpenQuote => Some("\u{201C}".to_string()),
            PseudoContentItem::CloseQuote => Some("\u{201D}".to_string()),
            // Attr, Counter, Counters need context to resolve
            _ => None,
        }
    }
}

// =============================================================================
// Pseudo-Element Styles
// =============================================================================

/// Computed styles for a pseudo-element
///
/// This structure holds the styling information specific to a pseudo-element,
/// including the `content` property and display mode.
#[derive(Debug, Clone)]
pub struct PseudoElementStyle {
    /// The content to generate
    pub content: PseudoContent,

    /// Display type for the pseudo-element box
    ///
    /// Default is `Display::Inline` per CSS spec.
    pub display: Display,

    /// Full computed styles (inherits from originating element)
    pub computed: Arc<ComputedStyle>,
}

impl PseudoElementStyle {
    /// Creates a new pseudo-element style with string content
    pub fn with_string(content: impl Into<String>, computed: Arc<ComputedStyle>) -> Self {
        Self {
            content: PseudoContent::String(content.into()),
            display: computed.display,
            computed,
        }
    }

    /// Creates a new pseudo-element style with no content (doesn't generate box)
    pub fn none(computed: Arc<ComputedStyle>) -> Self {
        Self {
            content: PseudoContent::None,
            display: Display::Inline,
            computed,
        }
    }

    /// Returns true if this style generates a box
    pub fn generates_box(&self) -> bool {
        // Must have content and not be display: none
        self.content.generates_box() && self.display != Display::None
    }

    /// Sets the display type
    pub fn with_display(mut self, display: Display) -> Self {
        self.display = display;
        self
    }
}

// =============================================================================
// Pseudo-Element Configuration
// =============================================================================

/// Configuration for pseudo-elements on an element
///
/// Holds the styles for `::before` and `::after` pseudo-elements.
#[derive(Debug, Clone, Default)]
pub struct PseudoElementConfig {
    /// Style for `::before` pseudo-element
    pub before: Option<PseudoElementStyle>,

    /// Style for `::after` pseudo-element
    pub after: Option<PseudoElementStyle>,
}

impl PseudoElementConfig {
    /// Creates a new empty configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates configuration with only `::before`
    pub fn with_before(style: PseudoElementStyle) -> Self {
        Self {
            before: Some(style),
            after: None,
        }
    }

    /// Creates configuration with only `::after`
    pub fn with_after(style: PseudoElementStyle) -> Self {
        Self {
            before: None,
            after: Some(style),
        }
    }

    /// Creates configuration with both pseudo-elements
    pub fn with_both(before: PseudoElementStyle, after: PseudoElementStyle) -> Self {
        Self {
            before: Some(before),
            after: Some(after),
        }
    }

    /// Sets the `::before` style
    pub fn set_before(&mut self, style: PseudoElementStyle) {
        self.before = Some(style);
    }

    /// Sets the `::after` style
    pub fn set_after(&mut self, style: PseudoElementStyle) {
        self.after = Some(style);
    }

    /// Gets the style for a specific pseudo-element type
    pub fn get(&self, pseudo_type: PseudoElementType) -> Option<&PseudoElementStyle> {
        match pseudo_type {
            PseudoElementType::Before => self.before.as_ref(),
            PseudoElementType::After => self.after.as_ref(),
        }
    }

    /// Returns true if any pseudo-element generates a box
    pub fn has_generated_content(&self) -> bool {
        self.before.as_ref().is_some_and(|s| s.generates_box())
            || self.after.as_ref().is_some_and(|s| s.generates_box())
    }

    /// Returns true if `::before` generates a box
    pub fn has_before(&self) -> bool {
        self.before.as_ref().is_some_and(|s| s.generates_box())
    }

    /// Returns true if `::after` generates a box
    pub fn has_after(&self) -> bool {
        self.after.as_ref().is_some_and(|s| s.generates_box())
    }
}

// =============================================================================
// Pseudo-Element Box Generator
// =============================================================================

/// Generator for pseudo-element boxes
///
/// Creates boxes for `::before` and `::after` pseudo-elements based on
/// their computed styles.
///
/// # Usage
///
/// ```ignore
/// let generator = PseudoElementGenerator::new();
///
/// // Check if pseudo-element generates a box
/// if config.has_before() {
///     let before_box = generator.generate_box(
///         PseudoElementType::Before,
///         config.before.as_ref().unwrap(),
///     );
/// }
/// ```
#[derive(Debug, Clone)]
pub struct PseudoElementGenerator {
    /// Include debug info in generated boxes
    include_debug_info: bool,
}

impl PseudoElementGenerator {
    /// Creates a new pseudo-element generator
    pub fn new() -> Self {
        Self {
            include_debug_info: true,
        }
    }

    /// Creates a generator without debug info
    pub fn without_debug_info() -> Self {
        Self {
            include_debug_info: false,
        }
    }

    /// Generates a box for a pseudo-element
    ///
    /// Returns `None` if the pseudo-element doesn't generate a box.
    ///
    /// # Arguments
    ///
    /// * `pseudo_type` - Whether this is `::before` or `::after`
    /// * `style` - The computed style for the pseudo-element
    ///
    /// # Returns
    ///
    /// A `BoxNode` representing the pseudo-element, or `None` if no box is generated.
    pub fn generate_box(&self, pseudo_type: PseudoElementType, style: &PseudoElementStyle) -> Option<BoxNode> {
        // Check if content generates a box
        if !style.generates_box() {
            return None;
        }

        // Get the text content
        let text = style.content.to_text()?;

        // Create text box for the content
        let text_box = BoxNode::new_text(style.computed.clone(), text);

        // Create the pseudo-element box based on display type
        let pseudo_box = self.create_container_box(style, vec![text_box]);

        // Add debug info if enabled
        let pseudo_box = if self.include_debug_info {
            let debug_info = DebugInfo::new(
                Some(pseudo_type.short_name().to_string()),
                None,
                vec!["pseudo-element".to_string()],
            );
            pseudo_box.with_debug_info(debug_info)
        } else {
            pseudo_box
        };

        Some(pseudo_box)
    }

    /// Creates the container box for pseudo-element content
    fn create_container_box(&self, style: &PseudoElementStyle, children: Vec<BoxNode>) -> BoxNode {
        match style.display {
            Display::Block | Display::FlowRoot | Display::ListItem => {
                BoxNode::new_block(style.computed.clone(), FormattingContextType::Block, children)
            }

            Display::InlineBlock => {
                BoxNode::new_inline_block(style.computed.clone(), FormattingContextType::Block, children)
            }

            Display::Flex => BoxNode::new_block(style.computed.clone(), FormattingContextType::Flex, children),

            Display::InlineFlex => {
                BoxNode::new_inline_block(style.computed.clone(), FormattingContextType::Flex, children)
            }

            Display::Grid => BoxNode::new_block(style.computed.clone(), FormattingContextType::Grid, children),

            Display::InlineGrid => {
                BoxNode::new_inline_block(style.computed.clone(), FormattingContextType::Grid, children)
            }

            // Default: inline (most common for pseudo-elements)
            // Covers Display::Inline, Display::None, Display::Contents, Display::Table*, Display::Ruby*
            _ => BoxNode::new_inline(style.computed.clone(), children),
        }
    }

    /// Inserts pseudo-element boxes into an element's children
    ///
    /// This method takes the original children and inserts `::before` and
    /// `::after` boxes at the appropriate positions.
    ///
    /// # Arguments
    ///
    /// * `children` - Original child boxes
    /// * `config` - Pseudo-element configuration
    ///
    /// # Returns
    ///
    /// Modified children with pseudo-element boxes inserted.
    pub fn insert_pseudo_boxes(&self, mut children: Vec<BoxNode>, config: &PseudoElementConfig) -> Vec<BoxNode> {
        // Insert ::before at the beginning
        if let Some(style) = &config.before {
            if let Some(before_box) = self.generate_box(PseudoElementType::Before, style) {
                children.insert(0, before_box);
            }
        }

        // Insert ::after at the end
        if let Some(style) = &config.after {
            if let Some(after_box) = self.generate_box(PseudoElementType::After, style) {
                children.push(after_box);
            }
        }

        children
    }
}

impl Default for PseudoElementGenerator {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Counts pseudo-element boxes in a tree
pub fn count_pseudo_boxes(root: &BoxNode) -> usize {
    let mut count = 0;

    // Check if this is a pseudo-element box
    if let Some(debug) = &root.debug_info {
        if debug.classes.contains(&"pseudo-element".to_string()) {
            count += 1;
        }
    }

    // Recursively count in children
    for child in &root.children {
        count += count_pseudo_boxes(child);
    }

    count
}

/// Finds all pseudo-element boxes in a tree
pub fn find_pseudo_boxes(root: &BoxNode) -> Vec<&BoxNode> {
    let mut result = Vec::new();

    if let Some(debug) = &root.debug_info {
        if debug.classes.contains(&"pseudo-element".to_string()) {
            result.push(root);
        }
    }

    for child in &root.children {
        result.extend(find_pseudo_boxes(child));
    }

    result
}

/// Checks if a box is a pseudo-element box
pub fn is_pseudo_box(box_node: &BoxNode) -> bool {
    box_node
        .debug_info
        .as_ref()
        .map(|d| d.classes.contains(&"pseudo-element".to_string()))
        .unwrap_or(false)
}

/// Gets the pseudo-element type from a box (if it is a pseudo-element)
pub fn get_pseudo_type(box_node: &BoxNode) -> Option<PseudoElementType> {
    let debug = box_node.debug_info.as_ref()?;

    if !debug.classes.contains(&"pseudo-element".to_string()) {
        return None;
    }

    match debug.tag_name.as_deref() {
        Some("before") => Some(PseudoElementType::Before),
        Some("after") => Some(PseudoElementType::After),
        _ => None,
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn default_style() -> Arc<ComputedStyle> {
        Arc::new(ComputedStyle::default())
    }

    fn style_with_display(display: Display) -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.display = display;
        Arc::new(style)
    }

    // =============================================================================
    // PseudoElementType Tests
    // =============================================================================

    #[test]
    fn test_pseudo_type_css_name() {
        assert_eq!(PseudoElementType::Before.css_name(), "::before");
        assert_eq!(PseudoElementType::After.css_name(), "::after");
    }

    #[test]
    fn test_pseudo_type_short_name() {
        assert_eq!(PseudoElementType::Before.short_name(), "before");
        assert_eq!(PseudoElementType::After.short_name(), "after");
    }

    #[test]
    fn test_pseudo_type_display() {
        assert_eq!(format!("{}", PseudoElementType::Before), "::before");
        assert_eq!(format!("{}", PseudoElementType::After), "::after");
    }

    // =============================================================================
    // PseudoContent Tests
    // =============================================================================

    #[test]
    fn test_content_none_does_not_generate_box() {
        assert!(!PseudoContent::None.generates_box());
        assert!(!PseudoContent::Normal.generates_box());
    }

    #[test]
    fn test_content_string_generates_box() {
        let content = PseudoContent::String("Hello".to_string());
        assert!(content.generates_box());
    }

    #[test]
    fn test_content_to_text() {
        assert_eq!(PseudoContent::None.to_text(), None);
        assert_eq!(PseudoContent::Normal.to_text(), None);
        assert_eq!(
            PseudoContent::String("Hello".to_string()).to_text(),
            Some("Hello".to_string())
        );
    }

    #[test]
    fn test_content_quotes() {
        assert!(PseudoContent::OpenQuote.generates_box());
        assert!(PseudoContent::CloseQuote.generates_box());
        assert!(!PseudoContent::NoOpenQuote.generates_box());
        assert!(!PseudoContent::NoCloseQuote.generates_box());
    }

    #[test]
    fn test_content_string_helper() {
        let content = PseudoContent::string("Test");
        assert_eq!(content, PseudoContent::String("Test".to_string()));
    }

    #[test]
    fn test_content_multiple() {
        let items = vec![
            PseudoContentItem::String("Hello, ".to_string()),
            PseudoContentItem::String("World!".to_string()),
        ];
        let content = PseudoContent::Multiple(items);
        assert!(content.generates_box());
        assert_eq!(content.to_text(), Some("Hello, World!".to_string()));
    }

    // =============================================================================
    // PseudoElementStyle Tests
    // =============================================================================

    #[test]
    fn test_style_with_string() {
        let style = PseudoElementStyle::with_string("Content", default_style());
        assert!(style.generates_box());
        assert_eq!(style.content.to_text(), Some("Content".to_string()));
    }

    #[test]
    fn test_style_none() {
        let style = PseudoElementStyle::none(default_style());
        assert!(!style.generates_box());
    }

    #[test]
    fn test_style_display_none() {
        let computed = style_with_display(Display::None);
        let style = PseudoElementStyle {
            content: PseudoContent::String("Content".to_string()),
            display: Display::None,
            computed,
        };
        assert!(!style.generates_box());
    }

    #[test]
    fn test_style_with_display() {
        let style = PseudoElementStyle::with_string("Content", default_style()).with_display(Display::Block);
        assert_eq!(style.display, Display::Block);
    }

    // =============================================================================
    // PseudoElementConfig Tests
    // =============================================================================

    #[test]
    fn test_config_new() {
        let config = PseudoElementConfig::new();
        assert!(!config.has_generated_content());
        assert!(!config.has_before());
        assert!(!config.has_after());
    }

    #[test]
    fn test_config_with_before() {
        let style = PseudoElementStyle::with_string("Before", default_style());
        let config = PseudoElementConfig::with_before(style);
        assert!(config.has_before());
        assert!(!config.has_after());
        assert!(config.has_generated_content());
    }

    #[test]
    fn test_config_with_after() {
        let style = PseudoElementStyle::with_string("After", default_style());
        let config = PseudoElementConfig::with_after(style);
        assert!(!config.has_before());
        assert!(config.has_after());
        assert!(config.has_generated_content());
    }

    #[test]
    fn test_config_with_both() {
        let before = PseudoElementStyle::with_string("Before", default_style());
        let after = PseudoElementStyle::with_string("After", default_style());
        let config = PseudoElementConfig::with_both(before, after);
        assert!(config.has_before());
        assert!(config.has_after());
    }

    #[test]
    fn test_config_get() {
        let style = PseudoElementStyle::with_string("Before", default_style());
        let config = PseudoElementConfig::with_before(style);

        assert!(config.get(PseudoElementType::Before).is_some());
        assert!(config.get(PseudoElementType::After).is_none());
    }

    #[test]
    fn test_config_set_methods() {
        let mut config = PseudoElementConfig::new();

        let before = PseudoElementStyle::with_string("Before", default_style());
        config.set_before(before);
        assert!(config.has_before());

        let after = PseudoElementStyle::with_string("After", default_style());
        config.set_after(after);
        assert!(config.has_after());
    }

    // =============================================================================
    // PseudoElementGenerator Tests
    // =============================================================================

    #[test]
    fn test_generator_new() {
        let generator = PseudoElementGenerator::new();
        assert!(generator.include_debug_info);
    }

    #[test]
    fn test_generator_without_debug_info() {
        let generator = PseudoElementGenerator::without_debug_info();
        assert!(!generator.include_debug_info);
    }

    #[test]
    fn test_generate_box_none_content() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::none(default_style());

        let result = generator.generate_box(PseudoElementType::Before, &style);
        assert!(result.is_none());
    }

    #[test]
    fn test_generate_box_string_content() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::with_string("Hello", default_style());

        let result = generator.generate_box(PseudoElementType::Before, &style);
        assert!(result.is_some());

        let box_node = result.unwrap();
        assert!(box_node.is_inline_level()); // Default is inline
        assert_eq!(box_node.children.len(), 1); // Contains text box
        assert!(box_node.children[0].is_text());
    }

    #[test]
    fn test_generate_box_block_display() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::with_string("Block content", default_style()).with_display(Display::Block);

        let result = generator.generate_box(PseudoElementType::Before, &style);
        assert!(result.is_some());

        let box_node = result.unwrap();
        assert!(box_node.is_block_level());
    }

    #[test]
    fn test_generate_box_inline_block_display() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::with_string("Inline-block", default_style()).with_display(Display::InlineBlock);

        let result = generator.generate_box(PseudoElementType::After, &style);
        assert!(result.is_some());

        let box_node = result.unwrap();
        assert!(box_node.is_inline_level());
        assert!(box_node.formatting_context().is_some());
    }

    #[test]
    fn test_generate_box_flex_display() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::with_string("Flex", default_style()).with_display(Display::Flex);

        let result = generator.generate_box(PseudoElementType::Before, &style);
        assert!(result.is_some());

        let box_node = result.unwrap();
        assert!(box_node.is_block_level());
        assert_eq!(box_node.formatting_context(), Some(FormattingContextType::Flex));
    }

    #[test]
    fn test_generate_box_debug_info() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::with_string("Debug", default_style());

        let result = generator.generate_box(PseudoElementType::Before, &style);
        let box_node = result.unwrap();

        assert!(box_node.debug_info.is_some());
        let debug = box_node.debug_info.as_ref().unwrap();
        assert_eq!(debug.tag_name, Some("before".to_string()));
        assert!(debug.classes.contains(&"pseudo-element".to_string()));
    }

    #[test]
    fn test_generate_box_no_debug_info() {
        let generator = PseudoElementGenerator::without_debug_info();
        let style = PseudoElementStyle::with_string("No debug", default_style());

        let result = generator.generate_box(PseudoElementType::Before, &style);
        let box_node = result.unwrap();

        assert!(box_node.debug_info.is_none());
    }

    // =============================================================================
    // Insert Pseudo Boxes Tests
    // =============================================================================

    #[test]
    fn test_insert_pseudo_boxes_empty_config() {
        let generator = PseudoElementGenerator::new();
        let config = PseudoElementConfig::new();

        let children = vec![BoxNode::new_text(default_style(), "Child".to_string())];

        let result = generator.insert_pseudo_boxes(children.clone(), &config);
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn test_insert_pseudo_boxes_before_only() {
        let generator = PseudoElementGenerator::new();
        let before_style = PseudoElementStyle::with_string("Before", default_style());
        let config = PseudoElementConfig::with_before(before_style);

        let children = vec![BoxNode::new_text(default_style(), "Child".to_string())];

        let result = generator.insert_pseudo_boxes(children, &config);
        assert_eq!(result.len(), 2);

        // First is ::before
        assert!(is_pseudo_box(&result[0]));
        assert_eq!(get_pseudo_type(&result[0]), Some(PseudoElementType::Before));

        // Second is original child
        assert!(!is_pseudo_box(&result[1]));
    }

    #[test]
    fn test_insert_pseudo_boxes_after_only() {
        let generator = PseudoElementGenerator::new();
        let after_style = PseudoElementStyle::with_string("After", default_style());
        let config = PseudoElementConfig::with_after(after_style);

        let children = vec![BoxNode::new_text(default_style(), "Child".to_string())];

        let result = generator.insert_pseudo_boxes(children, &config);
        assert_eq!(result.len(), 2);

        // First is original child
        assert!(!is_pseudo_box(&result[0]));

        // Second is ::after
        assert!(is_pseudo_box(&result[1]));
        assert_eq!(get_pseudo_type(&result[1]), Some(PseudoElementType::After));
    }

    #[test]
    fn test_insert_pseudo_boxes_both() {
        let generator = PseudoElementGenerator::new();
        let before = PseudoElementStyle::with_string("Before", default_style());
        let after = PseudoElementStyle::with_string("After", default_style());
        let config = PseudoElementConfig::with_both(before, after);

        let children = vec![BoxNode::new_text(default_style(), "Child".to_string())];

        let result = generator.insert_pseudo_boxes(children, &config);
        assert_eq!(result.len(), 3);

        // Order: ::before, child, ::after
        assert_eq!(get_pseudo_type(&result[0]), Some(PseudoElementType::Before));
        assert!(!is_pseudo_box(&result[1]));
        assert_eq!(get_pseudo_type(&result[2]), Some(PseudoElementType::After));
    }

    #[test]
    fn test_insert_pseudo_boxes_empty_children() {
        let generator = PseudoElementGenerator::new();
        let before = PseudoElementStyle::with_string("Before", default_style());
        let after = PseudoElementStyle::with_string("After", default_style());
        let config = PseudoElementConfig::with_both(before, after);

        let children = vec![];

        let result = generator.insert_pseudo_boxes(children, &config);
        assert_eq!(result.len(), 2);

        // Order: ::before, ::after
        assert_eq!(get_pseudo_type(&result[0]), Some(PseudoElementType::Before));
        assert_eq!(get_pseudo_type(&result[1]), Some(PseudoElementType::After));
    }

    // =============================================================================
    // Helper Function Tests
    // =============================================================================

    #[test]
    fn test_count_pseudo_boxes() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::with_string("Pseudo", default_style());

        let before = generator.generate_box(PseudoElementType::Before, &style).unwrap();
        let after = generator.generate_box(PseudoElementType::After, &style).unwrap();

        let normal = BoxNode::new_text(default_style(), "Normal".to_string());

        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![before, normal, after],
        );

        assert_eq!(count_pseudo_boxes(&root), 2);
    }

    #[test]
    fn test_find_pseudo_boxes() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::with_string("Pseudo", default_style());

        let before = generator.generate_box(PseudoElementType::Before, &style).unwrap();
        let after = generator.generate_box(PseudoElementType::After, &style).unwrap();

        let normal = BoxNode::new_text(default_style(), "Normal".to_string());

        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![before, normal, after],
        );

        let found = find_pseudo_boxes(&root);
        assert_eq!(found.len(), 2);
    }

    #[test]
    fn test_is_pseudo_box() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::with_string("Test", default_style());

        let pseudo = generator.generate_box(PseudoElementType::Before, &style).unwrap();
        let normal = BoxNode::new_text(default_style(), "Normal".to_string());

        assert!(is_pseudo_box(&pseudo));
        assert!(!is_pseudo_box(&normal));
    }

    #[test]
    fn test_get_pseudo_type() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::with_string("Test", default_style());

        let before = generator.generate_box(PseudoElementType::Before, &style).unwrap();
        let after = generator.generate_box(PseudoElementType::After, &style).unwrap();
        let normal = BoxNode::new_text(default_style(), "Normal".to_string());

        assert_eq!(get_pseudo_type(&before), Some(PseudoElementType::Before));
        assert_eq!(get_pseudo_type(&after), Some(PseudoElementType::After));
        assert_eq!(get_pseudo_type(&normal), None);
    }

    // =============================================================================
    // Edge Case Tests
    // =============================================================================

    #[test]
    fn test_empty_string_content() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::with_string("", default_style());

        let result = generator.generate_box(PseudoElementType::Before, &style);
        // Empty string still generates a box (per CSS spec)
        assert!(result.is_some());
    }

    #[test]
    fn test_whitespace_only_content() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::with_string("   ", default_style());

        let result = generator.generate_box(PseudoElementType::Before, &style);
        assert!(result.is_some());
    }

    #[test]
    fn test_unicode_content() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::with_string("« 日本語 »", default_style());

        let result = generator.generate_box(PseudoElementType::Before, &style);
        assert!(result.is_some());

        let box_node = result.unwrap();
        let text_box = &box_node.children[0];
        assert_eq!(text_box.text(), Some("« 日本語 »"));
    }

    #[test]
    fn test_nested_pseudo_in_tree() {
        let generator = PseudoElementGenerator::new();
        let style = PseudoElementStyle::with_string("Nested", default_style());

        // Create nested structure
        let inner_pseudo = generator.generate_box(PseudoElementType::Before, &style).unwrap();
        let inner_block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![inner_pseudo]);

        let outer_pseudo = generator.generate_box(PseudoElementType::After, &style).unwrap();
        let root = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![inner_block, outer_pseudo],
        );

        // Should find both pseudo boxes
        assert_eq!(count_pseudo_boxes(&root), 2);
    }
}
