//! CSS Content Generation
//!
//! This module implements the CSS `content` property and content generation.
//! It handles the generation of text content for `::before` and `::after`
//! pseudo-elements.
//!
//! # CSS Content Property
//!
//! The `content` property generates replacement content for pseudo-elements.
//! It can contain:
//!
//! - **Strings**: `content: "Hello";`
//! - **attr()**: `content: attr(data-label);` - Gets element attribute value
//! - **counter()**: `content: counter(chapter);` - Gets counter value
//! - **counters()**: `content: counters(section, ".");` - Gets nested counter values
//! - **Quotes**: `content: open-quote;` - Gets quote characters
//! - **Keywords**: `none`, `normal`
//!
//! # Examples
//!
//! ```ignore
//! use fastrender::style::content::{ContentValue, ContentItem, ContentGenerator, ContentContext};
//!
//! let content = ContentValue::Items(vec![
//!     ContentItem::String("Chapter ".to_string()),
//!     ContentItem::Counter { name: "chapter".to_string(), style: None },
//!     ContentItem::String(": ".to_string()),
//! ]);
//!
//! let generator = ContentGenerator::new();
//! let context = ContentContext::default();
//! let text = generator.generate(&content, &context);
//! ```
//!
//! Reference: CSS Generated Content Module Level 3
//! <https://www.w3.org/TR/css-content-3/>

use std::collections::HashMap;
use std::fmt;

/// The computed value of the CSS `content` property
///
/// Represents what content should be generated for a pseudo-element.
///
/// # Examples
///
/// ```
/// use fastrender::style::content::{ContentValue, ContentItem};
///
/// // Simple string content
/// let content = ContentValue::Items(vec![
///     ContentItem::String("Hello".to_string()),
/// ]);
///
/// // Empty content (for ::before/::after with no content)
/// let none = ContentValue::None;
///
/// // Normal content (pseudo-element has no generated content)
/// let normal = ContentValue::Normal;
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum ContentValue {
    /// No content generated
    ///
    /// CSS: `content: none;`
    /// The pseudo-element is not generated.
    None,

    /// Normal content generation (pseudo-element default)
    ///
    /// CSS: `content: normal;`
    /// For `::before` and `::after`, this computes to `none`.
    Normal,

    /// One or more content items
    ///
    /// CSS: `content: "text" attr(x) counter(n);`
    /// Multiple items are concatenated in order.
    Items(Vec<ContentItem>),
}

impl ContentValue {
    /// Creates a content value from a single string
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::content::ContentValue;
    ///
    /// let content = ContentValue::from_string("Hello, World!");
    /// ```
    pub fn from_string(s: &str) -> Self {
        ContentValue::Items(vec![ContentItem::String(s.to_string())])
    }

    /// Returns true if this content value generates no content
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::content::ContentValue;
    ///
    /// assert!(ContentValue::None.is_empty());
    /// assert!(ContentValue::Normal.is_empty());
    /// assert!(!ContentValue::from_string("hello").is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        match self {
            ContentValue::None | ContentValue::Normal => true,
            ContentValue::Items(items) => items.is_empty(),
        }
    }

    /// Returns the number of content items
    pub fn len(&self) -> usize {
        match self {
            ContentValue::None | ContentValue::Normal => 0,
            ContentValue::Items(items) => items.len(),
        }
    }
}

impl Default for ContentValue {
    fn default() -> Self {
        ContentValue::Normal
    }
}

impl fmt::Display for ContentValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ContentValue::None => write!(f, "none"),
            ContentValue::Normal => write!(f, "normal"),
            ContentValue::Items(items) => {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", item)?;
                }
                Ok(())
            }
        }
    }
}

/// A single content item within a content value
///
/// Content items are concatenated together to form the final content.
///
/// Reference: CSS Generated Content Module Level 3
/// <https://www.w3.org/TR/css-content-3/#content-values>
#[derive(Debug, Clone, PartialEq)]
pub enum ContentItem {
    /// A string literal
    ///
    /// CSS: `content: "text";`
    /// The string is rendered as-is.
    String(String),

    /// An attribute reference
    ///
    /// CSS: `content: attr(data-label);`
    /// Gets the value of the specified attribute from the element.
    Attr {
        /// The attribute name to read
        name: String,
        /// Optional type (for CSS attr() level 3)
        /// Default is "string"
        type_or_unit: Option<String>,
        /// Fallback value if attribute is not present
        fallback: Option<String>,
    },

    /// A counter value
    ///
    /// CSS: `content: counter(chapter);`
    /// Gets the current value of a CSS counter.
    Counter {
        /// Counter name
        name: String,
        /// Counter style (e.g., "decimal", "lower-roman")
        style: Option<CounterStyle>,
    },

    /// Nested counter values
    ///
    /// CSS: `content: counters(section, ".");`
    /// Gets all values of a counter in nested scopes.
    Counters {
        /// Counter name
        name: String,
        /// Separator between counter values
        separator: String,
        /// Counter style
        style: Option<CounterStyle>,
    },

    /// Opening quote character
    ///
    /// CSS: `content: open-quote;`
    /// Uses the appropriate quote character based on nesting level.
    OpenQuote,

    /// Closing quote character
    ///
    /// CSS: `content: close-quote;`
    CloseQuote,

    /// No opening quote (but increments nesting)
    ///
    /// CSS: `content: no-open-quote;`
    NoOpenQuote,

    /// No closing quote (but decrements nesting)
    ///
    /// CSS: `content: no-close-quote;`
    NoCloseQuote,

    /// A URL reference (for images)
    ///
    /// CSS: `content: url(image.png);`
    /// Note: Image content requires special handling during layout/paint.
    Url(String),
}

impl ContentItem {
    /// Creates a string content item
    pub fn string(s: impl Into<String>) -> Self {
        ContentItem::String(s.into())
    }

    /// Creates an attr() content item
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::content::ContentItem;
    ///
    /// let attr = ContentItem::attr("data-label");
    /// let attr_with_fallback = ContentItem::attr_with_fallback("title", "Untitled");
    /// ```
    pub fn attr(name: impl Into<String>) -> Self {
        ContentItem::Attr {
            name: name.into(),
            type_or_unit: None,
            fallback: None,
        }
    }

    /// Creates an attr() content item with a fallback value
    pub fn attr_with_fallback(name: impl Into<String>, fallback: impl Into<String>) -> Self {
        ContentItem::Attr {
            name: name.into(),
            type_or_unit: None,
            fallback: Some(fallback.into()),
        }
    }

    /// Creates a counter() content item
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::content::{ContentItem, CounterStyle};
    ///
    /// let counter = ContentItem::counter("chapter");
    /// let roman = ContentItem::counter_styled("chapter", CounterStyle::LowerRoman);
    /// ```
    pub fn counter(name: impl Into<String>) -> Self {
        ContentItem::Counter {
            name: name.into(),
            style: None,
        }
    }

    /// Creates a counter() content item with a specific style
    pub fn counter_styled(name: impl Into<String>, style: CounterStyle) -> Self {
        ContentItem::Counter {
            name: name.into(),
            style: Some(style),
        }
    }

    /// Creates a counters() content item
    pub fn counters(name: impl Into<String>, separator: impl Into<String>) -> Self {
        ContentItem::Counters {
            name: name.into(),
            separator: separator.into(),
            style: None,
        }
    }

    /// Creates a counters() content item with a specific style
    pub fn counters_styled(name: impl Into<String>, separator: impl Into<String>, style: CounterStyle) -> Self {
        ContentItem::Counters {
            name: name.into(),
            separator: separator.into(),
            style: Some(style),
        }
    }
}

impl fmt::Display for ContentItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ContentItem::String(s) => write!(f, "\"{}\"", s.replace('"', "\\\"")),
            ContentItem::Attr { name, fallback, .. } => {
                if let Some(fb) = fallback {
                    write!(f, "attr({}, \"{}\")", name, fb)
                } else {
                    write!(f, "attr({})", name)
                }
            }
            ContentItem::Counter { name, style } => {
                if let Some(s) = style {
                    write!(f, "counter({}, {})", name, s)
                } else {
                    write!(f, "counter({})", name)
                }
            }
            ContentItem::Counters { name, separator, style } => {
                if let Some(s) = style {
                    write!(f, "counters({}, \"{}\", {})", name, separator, s)
                } else {
                    write!(f, "counters({}, \"{}\")", name, separator)
                }
            }
            ContentItem::OpenQuote => write!(f, "open-quote"),
            ContentItem::CloseQuote => write!(f, "close-quote"),
            ContentItem::NoOpenQuote => write!(f, "no-open-quote"),
            ContentItem::NoCloseQuote => write!(f, "no-close-quote"),
            ContentItem::Url(url) => write!(f, "url(\"{}\")", url),
        }
    }
}

/// Counter style for counter() and counters() functions
///
/// Defines how counter values are formatted.
///
/// Reference: CSS Counter Styles Level 3
/// <https://www.w3.org/TR/css-counter-styles-3/>
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CounterStyle {
    /// Decimal numbers (1, 2, 3, ...)
    Decimal,

    /// Decimal with leading zeros (01, 02, ...)
    DecimalLeadingZero,

    /// Lowercase Roman numerals (i, ii, iii, ...)
    LowerRoman,

    /// Uppercase Roman numerals (I, II, III, ...)
    UpperRoman,

    /// Lowercase ASCII letters (a, b, c, ...)
    LowerAlpha,

    /// Uppercase ASCII letters (A, B, C, ...)
    UpperAlpha,

    /// Lowercase Greek letters
    LowerGreek,

    /// Disc marker
    Disc,

    /// Circle marker
    Circle,

    /// Square marker
    Square,

    /// No marker
    None,
}

impl CounterStyle {
    /// Formats a counter value using this style
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::content::CounterStyle;
    ///
    /// assert_eq!(CounterStyle::Decimal.format(42), "42");
    /// assert_eq!(CounterStyle::LowerRoman.format(4), "iv");
    /// assert_eq!(CounterStyle::UpperAlpha.format(1), "A");
    /// ```
    pub fn format(&self, value: i32) -> String {
        match self {
            CounterStyle::Decimal => value.to_string(),
            CounterStyle::DecimalLeadingZero => format!("{:02}", value),
            CounterStyle::LowerRoman => to_roman(value).to_lowercase(),
            CounterStyle::UpperRoman => to_roman(value),
            CounterStyle::LowerAlpha => to_alpha(value, false),
            CounterStyle::UpperAlpha => to_alpha(value, true),
            CounterStyle::LowerGreek => to_greek(value),
            CounterStyle::Disc => "•".to_string(),
            CounterStyle::Circle => "◦".to_string(),
            CounterStyle::Square => "▪".to_string(),
            CounterStyle::None => String::new(),
        }
    }

    /// Parses a counter style from a string
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::content::CounterStyle;
    ///
    /// assert_eq!(CounterStyle::parse("decimal"), Some(CounterStyle::Decimal));
    /// assert_eq!(CounterStyle::parse("lower-roman"), Some(CounterStyle::LowerRoman));
    /// assert_eq!(CounterStyle::parse("unknown"), None);
    /// ```
    pub fn parse(s: &str) -> Option<Self> {
        match s.trim().to_lowercase().as_str() {
            "decimal" => Some(CounterStyle::Decimal),
            "decimal-leading-zero" => Some(CounterStyle::DecimalLeadingZero),
            "lower-roman" => Some(CounterStyle::LowerRoman),
            "upper-roman" => Some(CounterStyle::UpperRoman),
            "lower-alpha" | "lower-latin" => Some(CounterStyle::LowerAlpha),
            "upper-alpha" | "upper-latin" => Some(CounterStyle::UpperAlpha),
            "lower-greek" => Some(CounterStyle::LowerGreek),
            "disc" => Some(CounterStyle::Disc),
            "circle" => Some(CounterStyle::Circle),
            "square" => Some(CounterStyle::Square),
            "none" => Some(CounterStyle::None),
            _ => None,
        }
    }
}

impl Default for CounterStyle {
    fn default() -> Self {
        CounterStyle::Decimal
    }
}

impl fmt::Display for CounterStyle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            CounterStyle::Decimal => "decimal",
            CounterStyle::DecimalLeadingZero => "decimal-leading-zero",
            CounterStyle::LowerRoman => "lower-roman",
            CounterStyle::UpperRoman => "upper-roman",
            CounterStyle::LowerAlpha => "lower-alpha",
            CounterStyle::UpperAlpha => "upper-alpha",
            CounterStyle::LowerGreek => "lower-greek",
            CounterStyle::Disc => "disc",
            CounterStyle::Circle => "circle",
            CounterStyle::Square => "square",
            CounterStyle::None => "none",
        };
        write!(f, "{}", s)
    }
}

/// Converts a number to Roman numerals
fn to_roman(mut n: i32) -> String {
    if n <= 0 {
        return n.to_string();
    }

    let mut result = String::new();
    let numerals = [
        (1000, "M"),
        (900, "CM"),
        (500, "D"),
        (400, "CD"),
        (100, "C"),
        (90, "XC"),
        (50, "L"),
        (40, "XL"),
        (10, "X"),
        (9, "IX"),
        (5, "V"),
        (4, "IV"),
        (1, "I"),
    ];

    for (value, numeral) in numerals {
        while n >= value {
            result.push_str(numeral);
            n -= value;
        }
    }

    result
}

/// Converts a number to alphabetic representation (a, b, c, ...)
fn to_alpha(n: i32, uppercase: bool) -> String {
    if n <= 0 {
        return n.to_string();
    }

    let mut result = String::new();
    let mut n = n;
    let base = if uppercase { 'A' } else { 'a' } as u32;

    while n > 0 {
        n -= 1;
        let c = char::from_u32(base + (n % 26) as u32).unwrap_or('?');
        result.insert(0, c);
        n /= 26;
    }

    result
}

/// Converts a number to Greek letters
fn to_greek(n: i32) -> String {
    if n <= 0 || n > 24 {
        return n.to_string();
    }

    const GREEK: &[char] = &[
        'α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ', 'ι', 'κ', 'λ', 'μ', 'ν', 'ξ', 'ο', 'π', 'ρ', 'σ', 'τ', 'υ', 'φ', 'χ',
        'ψ', 'ω',
    ];

    GREEK[(n - 1) as usize].to_string()
}

/// Context for content generation
///
/// Contains all the information needed to resolve content values,
/// including counter values, element attributes, and quote nesting.
///
/// # Examples
///
/// ```
/// use fastrender::style::content::ContentContext;
/// use std::collections::HashMap;
///
/// let mut context = ContentContext::default();
/// context.set_counter("chapter", 5);
/// context.set_attribute("data-label", "Introduction");
/// ```
#[derive(Debug, Clone, Default)]
pub struct ContentContext {
    /// Current counter values
    ///
    /// Key: counter name, Value: current count
    counters: HashMap<String, Vec<i32>>,

    /// Element attributes
    ///
    /// Key: attribute name, Value: attribute value
    attributes: HashMap<String, String>,

    /// Quote nesting level (0 = no open quotes)
    quote_depth: usize,

    /// Quote characters for different nesting levels
    ///
    /// Each pair is (open-quote, close-quote)
    /// Default is `["\u{201C}", "\u{201D}", "\u{2018}", "\u{2019}"]` (curly quotes)
    quotes: Vec<(String, String)>,
}

impl ContentContext {
    /// Creates a new empty content context
    pub fn new() -> Self {
        Self {
            counters: HashMap::new(),
            attributes: HashMap::new(),
            quote_depth: 0,
            quotes: vec![
                ("\u{201C}".to_string(), "\u{201D}".to_string()), // " "
                ("\u{2018}".to_string(), "\u{2019}".to_string()), // ' '
            ],
        }
    }

    /// Creates a content context with element attributes
    pub fn with_attributes(attributes: HashMap<String, String>) -> Self {
        Self {
            attributes,
            ..Default::default()
        }
    }

    /// Sets a counter value
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::content::ContentContext;
    ///
    /// let mut ctx = ContentContext::new();
    /// ctx.set_counter("chapter", 5);
    /// assert_eq!(ctx.get_counter("chapter"), 5);
    /// ```
    pub fn set_counter(&mut self, name: &str, value: i32) {
        let entry = self.counters.entry(name.to_string()).or_insert_with(Vec::new);
        if entry.is_empty() {
            entry.push(value);
        } else {
            *entry.last_mut().unwrap() = value;
        }
    }

    /// Pushes a new counter scope (for nested counters)
    ///
    /// Used when entering a new counter scope (e.g., nested list).
    pub fn push_counter(&mut self, name: &str, initial_value: i32) {
        let entry = self.counters.entry(name.to_string()).or_insert_with(Vec::new);
        entry.push(initial_value);
    }

    /// Pops a counter scope
    ///
    /// Used when leaving a counter scope.
    pub fn pop_counter(&mut self, name: &str) {
        if let Some(stack) = self.counters.get_mut(name) {
            if stack.len() > 1 {
                stack.pop();
            }
        }
    }

    /// Gets the current value of a counter
    ///
    /// Returns 0 if the counter doesn't exist.
    pub fn get_counter(&self, name: &str) -> i32 {
        self.counters
            .get(name)
            .and_then(|stack| stack.last().copied())
            .unwrap_or(0)
    }

    /// Gets all counter values for nested counters
    ///
    /// Returns an empty slice if the counter doesn't exist.
    pub fn get_counters(&self, name: &str) -> &[i32] {
        self.counters.get(name).map(|v| v.as_slice()).unwrap_or(&[])
    }

    /// Increments a counter by 1
    pub fn increment_counter(&mut self, name: &str) {
        self.increment_counter_by(name, 1);
    }

    /// Increments a counter by a specific amount
    pub fn increment_counter_by(&mut self, name: &str, amount: i32) {
        let entry = self.counters.entry(name.to_string()).or_insert_with(|| vec![0]);
        if let Some(value) = entry.last_mut() {
            *value += amount;
        }
    }

    /// Resets a counter to 0 (or initial value)
    pub fn reset_counter(&mut self, name: &str) {
        self.reset_counter_to(name, 0);
    }

    /// Resets a counter to a specific value
    pub fn reset_counter_to(&mut self, name: &str, value: i32) {
        let entry = self.counters.entry(name.to_string()).or_insert_with(Vec::new);
        if entry.is_empty() {
            entry.push(value);
        } else {
            *entry.last_mut().unwrap() = value;
        }
    }

    /// Sets an element attribute
    pub fn set_attribute(&mut self, name: &str, value: &str) {
        self.attributes.insert(name.to_string(), value.to_string());
    }

    /// Gets an element attribute value
    pub fn get_attribute(&self, name: &str) -> Option<&str> {
        self.attributes.get(name).map(|s| s.as_str())
    }

    /// Sets the quote characters
    ///
    /// Each pair is (open-quote, close-quote) for a nesting level.
    /// The first pair is for the outermost level, second for nested, etc.
    pub fn set_quotes(&mut self, quotes: Vec<(String, String)>) {
        self.quotes = quotes;
    }

    /// Gets the current open quote character
    pub fn open_quote(&self) -> &str {
        let index = self.quote_depth.min(self.quotes.len().saturating_sub(1));
        self.quotes.get(index).map(|(open, _)| open.as_str()).unwrap_or("\"")
    }

    /// Gets the current close quote character
    ///
    /// This returns the close quote for the current nesting level.
    /// Should be called BEFORE `pop_quote()` to get the correct character.
    pub fn close_quote(&self) -> &str {
        // Use current depth minus 1 (since open quote incremented it)
        let depth = self.quote_depth.saturating_sub(1);
        let index = depth.min(self.quotes.len().saturating_sub(1));
        self.quotes.get(index).map(|(_, close)| close.as_str()).unwrap_or("\"")
    }

    /// Increments the quote depth (for open-quote)
    pub fn push_quote(&mut self) {
        self.quote_depth += 1;
    }

    /// Decrements the quote depth (for close-quote)
    pub fn pop_quote(&mut self) {
        self.quote_depth = self.quote_depth.saturating_sub(1);
    }

    /// Gets the current quote depth
    pub fn quote_depth(&self) -> usize {
        self.quote_depth
    }
}

/// Content generator
///
/// Generates text content from a ContentValue and a ContentContext.
///
/// # Examples
///
/// ```
/// use fastrender::style::content::{ContentValue, ContentItem, ContentGenerator, ContentContext};
///
/// let content = ContentValue::Items(vec![
///     ContentItem::String("Chapter ".to_string()),
///     ContentItem::Counter { name: "chapter".to_string(), style: None },
/// ]);
///
/// let generator = ContentGenerator::new();
/// let mut context = ContentContext::new();
/// context.set_counter("chapter", 5);
///
/// let text = generator.generate(&content, &mut context);
/// assert_eq!(text, "Chapter 5");
/// ```
#[derive(Debug, Clone, Default)]
pub struct ContentGenerator {
    // Future: could hold configuration for content generation
}

impl ContentGenerator {
    /// Creates a new content generator
    pub fn new() -> Self {
        Self {}
    }

    /// Generates text content from a content value
    ///
    /// # Arguments
    ///
    /// * `content` - The content value to generate
    /// * `context` - The context containing counters, attributes, and quote state
    ///
    /// # Returns
    ///
    /// The generated text content.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::content::{ContentValue, ContentItem, ContentGenerator, ContentContext};
    ///
    /// let content = ContentValue::from_string("Hello, World!");
    /// let generator = ContentGenerator::new();
    /// let mut context = ContentContext::new();
    ///
    /// let text = generator.generate(&content, &mut context);
    /// assert_eq!(text, "Hello, World!");
    /// ```
    pub fn generate(&self, content: &ContentValue, context: &mut ContentContext) -> String {
        match content {
            ContentValue::None | ContentValue::Normal => String::new(),
            ContentValue::Items(items) => {
                let mut result = String::new();
                for item in items {
                    result.push_str(&self.generate_item(item, context));
                }
                result
            }
        }
    }

    /// Generates text for a single content item
    fn generate_item(&self, item: &ContentItem, context: &mut ContentContext) -> String {
        match item {
            ContentItem::String(s) => s.clone(),

            ContentItem::Attr { name, fallback, .. } => context
                .get_attribute(name)
                .map(|s| s.to_string())
                .or_else(|| fallback.clone())
                .unwrap_or_default(),

            ContentItem::Counter { name, style } => {
                let value = context.get_counter(name);
                let style = style.as_ref().copied().unwrap_or_default();
                style.format(value)
            }

            ContentItem::Counters { name, separator, style } => {
                let values = context.get_counters(name);
                let style = style.as_ref().copied().unwrap_or_default();
                values
                    .iter()
                    .map(|&v| style.format(v))
                    .collect::<Vec<_>>()
                    .join(separator)
            }

            ContentItem::OpenQuote => {
                let quote = context.open_quote().to_string();
                context.push_quote();
                quote
            }

            ContentItem::CloseQuote => {
                // Get the close quote BEFORE popping to get the right quote character
                let quote = context.close_quote().to_string();
                context.pop_quote();
                quote
            }

            ContentItem::NoOpenQuote => {
                context.push_quote();
                String::new()
            }

            ContentItem::NoCloseQuote => {
                context.pop_quote();
                String::new()
            }

            ContentItem::Url(_url) => {
                // URLs generate replaced content (images), not text
                // Return empty string - the caller should handle this specially
                String::new()
            }
        }
    }

    /// Checks if a content value contains only text (no images/urls)
    ///
    /// Useful for determining if content can be rendered as a simple text run.
    pub fn is_text_only(content: &ContentValue) -> bool {
        match content {
            ContentValue::None | ContentValue::Normal => true,
            ContentValue::Items(items) => items.iter().all(|item| !matches!(item, ContentItem::Url(_))),
        }
    }
}

/// Parses a CSS content property value
///
/// # Arguments
///
/// * `input` - The CSS content property value string
///
/// # Returns
///
/// A parsed ContentValue, or None if parsing fails.
///
/// # Examples
///
/// ```
/// use fastrender::style::content::{parse_content, ContentValue, ContentItem};
///
/// let content = parse_content("\"Hello\"").unwrap();
/// assert_eq!(content, ContentValue::Items(vec![ContentItem::String("Hello".to_string())]));
///
/// let none = parse_content("none").unwrap();
/// assert_eq!(none, ContentValue::None);
/// ```
pub fn parse_content(input: &str) -> Option<ContentValue> {
    let input = input.trim();

    // Handle keywords
    match input.to_lowercase().as_str() {
        "none" => return Some(ContentValue::None),
        "normal" => return Some(ContentValue::Normal),
        _ => {}
    }

    // Parse content items
    let mut items = Vec::new();
    let mut chars = input.chars().peekable();

    while chars.peek().is_some() {
        // Skip whitespace
        while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
            chars.next();
        }

        if chars.peek().is_none() {
            break;
        }

        // Check for different content types
        if chars.peek() == Some(&'"') || chars.peek() == Some(&'\'') {
            // String literal
            if let Some(s) = parse_string(&mut chars) {
                items.push(ContentItem::String(s));
            } else {
                return None;
            }
        } else {
            // Function or keyword
            let mut name = String::new();
            while let Some(&c) = chars.peek() {
                if c.is_alphanumeric() || c == '-' || c == '_' {
                    name.push(c);
                    chars.next();
                } else {
                    break;
                }
            }

            if name.is_empty() {
                return None;
            }

            // Skip whitespace
            while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                chars.next();
            }

            if chars.peek() == Some(&'(') {
                // Function call
                chars.next(); // consume '('

                // Collect function arguments
                let mut args = String::new();
                let mut depth = 1;
                while let Some(c) = chars.next() {
                    if c == '(' {
                        depth += 1;
                        args.push(c);
                    } else if c == ')' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        args.push(c);
                    } else {
                        args.push(c);
                    }
                }

                let item = parse_function(&name.to_lowercase(), &args)?;
                items.push(item);
            } else {
                // Keyword
                let item = parse_keyword(&name.to_lowercase())?;
                items.push(item);
            }
        }
    }

    if items.is_empty() {
        None
    } else {
        Some(ContentValue::Items(items))
    }
}

/// Parses a quoted string
fn parse_string(chars: &mut std::iter::Peekable<std::str::Chars>) -> Option<String> {
    let quote = chars.next()?;
    if quote != '"' && quote != '\'' {
        return None;
    }

    let mut result = String::new();
    let mut escaped = false;

    while let Some(c) = chars.next() {
        if escaped {
            result.push(match c {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                _ => c,
            });
            escaped = false;
        } else if c == '\\' {
            escaped = true;
        } else if c == quote {
            return Some(result);
        } else {
            result.push(c);
        }
    }

    // Unterminated string
    None
}

/// Parses a content function (attr, counter, counters, url)
fn parse_function(name: &str, args: &str) -> Option<ContentItem> {
    match name {
        "attr" => {
            let args = args.trim();
            // Simple attr(name) or attr(name, fallback)
            if let Some(comma_pos) = args.find(',') {
                let attr_name = args[..comma_pos].trim();
                let fallback = args[comma_pos + 1..].trim();
                // Remove quotes from fallback if present
                let fallback = if (fallback.starts_with('"') && fallback.ends_with('"'))
                    || (fallback.starts_with('\'') && fallback.ends_with('\''))
                {
                    &fallback[1..fallback.len() - 1]
                } else {
                    fallback
                };
                Some(ContentItem::Attr {
                    name: attr_name.to_string(),
                    type_or_unit: None,
                    fallback: Some(fallback.to_string()),
                })
            } else {
                Some(ContentItem::attr(args.to_string()))
            }
        }

        "counter" => {
            let args = args.trim();
            // counter(name) or counter(name, style)
            if let Some(comma_pos) = args.find(',') {
                let name = args[..comma_pos].trim();
                let style_str = args[comma_pos + 1..].trim();
                let style = CounterStyle::parse(style_str);
                Some(ContentItem::Counter {
                    name: name.to_string(),
                    style,
                })
            } else {
                Some(ContentItem::counter(args.to_string()))
            }
        }

        "counters" => {
            let args = args.trim();
            // counters(name, separator) or counters(name, separator, style)
            // Find the separator string (quoted)
            let mut parts = Vec::new();
            let mut current = String::new();
            let mut in_string = false;
            let mut quote_char = '"';

            for c in args.chars() {
                if !in_string && (c == '"' || c == '\'') {
                    in_string = true;
                    quote_char = c;
                } else if in_string && c == quote_char {
                    in_string = false;
                    parts.push(current.clone());
                    current.clear();
                } else if in_string {
                    current.push(c);
                } else if c == ',' && !in_string {
                    if !current.trim().is_empty() {
                        parts.push(current.trim().to_string());
                    }
                    current.clear();
                } else if !c.is_whitespace() || in_string {
                    current.push(c);
                }
            }
            if !current.trim().is_empty() {
                parts.push(current.trim().to_string());
            }

            if parts.len() >= 2 {
                let name = parts[0].clone();
                let separator = parts[1].clone();
                let style = parts.get(2).and_then(|s| CounterStyle::parse(s));
                Some(ContentItem::Counters { name, separator, style })
            } else {
                None
            }
        }

        "url" => {
            let args = args.trim();
            // Remove quotes if present
            let url =
                if (args.starts_with('"') && args.ends_with('"')) || (args.starts_with('\'') && args.ends_with('\'')) {
                    &args[1..args.len() - 1]
                } else {
                    args
                };
            Some(ContentItem::Url(url.to_string()))
        }

        _ => None,
    }
}

/// Parses a content keyword
fn parse_keyword(name: &str) -> Option<ContentItem> {
    match name {
        "open-quote" => Some(ContentItem::OpenQuote),
        "close-quote" => Some(ContentItem::CloseQuote),
        "no-open-quote" => Some(ContentItem::NoOpenQuote),
        "no-close-quote" => Some(ContentItem::NoCloseQuote),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // === ContentValue Tests ===

    #[test]
    fn test_content_value_none() {
        let content = ContentValue::None;
        assert!(content.is_empty());
        assert_eq!(content.len(), 0);
        assert_eq!(format!("{}", content), "none");
    }

    #[test]
    fn test_content_value_normal() {
        let content = ContentValue::Normal;
        assert!(content.is_empty());
        assert_eq!(format!("{}", content), "normal");
    }

    #[test]
    fn test_content_value_from_string() {
        let content = ContentValue::from_string("Hello");
        assert!(!content.is_empty());
        assert_eq!(content.len(), 1);
    }

    #[test]
    fn test_content_value_default() {
        let content = ContentValue::default();
        assert_eq!(content, ContentValue::Normal);
    }

    // === ContentItem Tests ===

    #[test]
    fn test_content_item_string() {
        let item = ContentItem::string("Hello");
        assert_eq!(format!("{}", item), "\"Hello\"");
    }

    #[test]
    fn test_content_item_attr() {
        let item = ContentItem::attr("data-label");
        assert_eq!(format!("{}", item), "attr(data-label)");
    }

    #[test]
    fn test_content_item_attr_with_fallback() {
        let item = ContentItem::attr_with_fallback("title", "Untitled");
        assert_eq!(format!("{}", item), "attr(title, \"Untitled\")");
    }

    #[test]
    fn test_content_item_counter() {
        let item = ContentItem::counter("chapter");
        assert_eq!(format!("{}", item), "counter(chapter)");
    }

    #[test]
    fn test_content_item_counter_styled() {
        let item = ContentItem::counter_styled("chapter", CounterStyle::LowerRoman);
        assert_eq!(format!("{}", item), "counter(chapter, lower-roman)");
    }

    #[test]
    fn test_content_item_counters() {
        let item = ContentItem::counters("section", ".");
        assert_eq!(format!("{}", item), "counters(section, \".\")");
    }

    #[test]
    fn test_content_item_quotes() {
        assert_eq!(format!("{}", ContentItem::OpenQuote), "open-quote");
        assert_eq!(format!("{}", ContentItem::CloseQuote), "close-quote");
        assert_eq!(format!("{}", ContentItem::NoOpenQuote), "no-open-quote");
        assert_eq!(format!("{}", ContentItem::NoCloseQuote), "no-close-quote");
    }

    // === CounterStyle Tests ===

    #[test]
    fn test_counter_style_decimal() {
        assert_eq!(CounterStyle::Decimal.format(42), "42");
        assert_eq!(CounterStyle::Decimal.format(0), "0");
        assert_eq!(CounterStyle::Decimal.format(-5), "-5");
    }

    #[test]
    fn test_counter_style_decimal_leading_zero() {
        assert_eq!(CounterStyle::DecimalLeadingZero.format(5), "05");
        assert_eq!(CounterStyle::DecimalLeadingZero.format(42), "42");
    }

    #[test]
    fn test_counter_style_lower_roman() {
        assert_eq!(CounterStyle::LowerRoman.format(1), "i");
        assert_eq!(CounterStyle::LowerRoman.format(4), "iv");
        assert_eq!(CounterStyle::LowerRoman.format(9), "ix");
        assert_eq!(CounterStyle::LowerRoman.format(14), "xiv");
        assert_eq!(CounterStyle::LowerRoman.format(49), "xlix");
    }

    #[test]
    fn test_counter_style_upper_roman() {
        assert_eq!(CounterStyle::UpperRoman.format(1), "I");
        assert_eq!(CounterStyle::UpperRoman.format(4), "IV");
        assert_eq!(CounterStyle::UpperRoman.format(1999), "MCMXCIX");
    }

    #[test]
    fn test_counter_style_lower_alpha() {
        assert_eq!(CounterStyle::LowerAlpha.format(1), "a");
        assert_eq!(CounterStyle::LowerAlpha.format(26), "z");
        assert_eq!(CounterStyle::LowerAlpha.format(27), "aa");
        assert_eq!(CounterStyle::LowerAlpha.format(28), "ab");
    }

    #[test]
    fn test_counter_style_upper_alpha() {
        assert_eq!(CounterStyle::UpperAlpha.format(1), "A");
        assert_eq!(CounterStyle::UpperAlpha.format(26), "Z");
        assert_eq!(CounterStyle::UpperAlpha.format(27), "AA");
    }

    #[test]
    fn test_counter_style_lower_greek() {
        assert_eq!(CounterStyle::LowerGreek.format(1), "α");
        assert_eq!(CounterStyle::LowerGreek.format(2), "β");
        assert_eq!(CounterStyle::LowerGreek.format(24), "ω");
    }

    #[test]
    fn test_counter_style_markers() {
        assert_eq!(CounterStyle::Disc.format(1), "•");
        assert_eq!(CounterStyle::Circle.format(1), "◦");
        assert_eq!(CounterStyle::Square.format(1), "▪");
        assert_eq!(CounterStyle::None.format(1), "");
    }

    #[test]
    fn test_counter_style_parse() {
        assert_eq!(CounterStyle::parse("decimal"), Some(CounterStyle::Decimal));
        assert_eq!(CounterStyle::parse("lower-roman"), Some(CounterStyle::LowerRoman));
        assert_eq!(CounterStyle::parse("upper-alpha"), Some(CounterStyle::UpperAlpha));
        assert_eq!(CounterStyle::parse("lower-latin"), Some(CounterStyle::LowerAlpha));
        assert_eq!(CounterStyle::parse("unknown"), None);
    }

    // === ContentContext Tests ===

    #[test]
    fn test_context_counters() {
        let mut ctx = ContentContext::new();

        // Initial value is 0
        assert_eq!(ctx.get_counter("chapter"), 0);

        // Set counter
        ctx.set_counter("chapter", 5);
        assert_eq!(ctx.get_counter("chapter"), 5);

        // Increment
        ctx.increment_counter("chapter");
        assert_eq!(ctx.get_counter("chapter"), 6);

        // Reset
        ctx.reset_counter("chapter");
        assert_eq!(ctx.get_counter("chapter"), 0);
    }

    #[test]
    fn test_context_nested_counters() {
        let mut ctx = ContentContext::new();

        ctx.push_counter("section", 1);
        assert_eq!(ctx.get_counters("section"), &[1]);

        ctx.push_counter("section", 1);
        assert_eq!(ctx.get_counters("section"), &[1, 1]);

        ctx.increment_counter("section");
        assert_eq!(ctx.get_counters("section"), &[1, 2]);

        ctx.pop_counter("section");
        assert_eq!(ctx.get_counters("section"), &[1]);
    }

    #[test]
    fn test_context_attributes() {
        let mut ctx = ContentContext::new();

        ctx.set_attribute("data-label", "Hello");
        assert_eq!(ctx.get_attribute("data-label"), Some("Hello"));
        assert_eq!(ctx.get_attribute("nonexistent"), None);
    }

    #[test]
    fn test_context_quotes() {
        let mut ctx = ContentContext::new();

        // Default quotes
        assert_eq!(ctx.open_quote(), "\u{201C}"); // "
        ctx.push_quote();
        assert_eq!(ctx.open_quote(), "\u{2018}"); // '
        ctx.push_quote();
        // When depth exceeds quotes, use last pair
        assert_eq!(ctx.open_quote(), "\u{2018}");

        ctx.pop_quote();
        ctx.pop_quote();
        assert_eq!(ctx.quote_depth(), 0);
    }

    // === ContentGenerator Tests ===

    #[test]
    fn test_generate_string() {
        let gen = ContentGenerator::new();
        let content = ContentValue::from_string("Hello, World!");
        let mut ctx = ContentContext::new();

        let result = gen.generate(&content, &mut ctx);
        assert_eq!(result, "Hello, World!");
    }

    #[test]
    fn test_generate_attr() {
        let gen = ContentGenerator::new();
        let content = ContentValue::Items(vec![ContentItem::attr("data-label")]);
        let mut ctx = ContentContext::new();
        ctx.set_attribute("data-label", "Test Label");

        let result = gen.generate(&content, &mut ctx);
        assert_eq!(result, "Test Label");
    }

    #[test]
    fn test_generate_attr_fallback() {
        let gen = ContentGenerator::new();
        let content = ContentValue::Items(vec![ContentItem::attr_with_fallback("missing", "Default")]);
        let mut ctx = ContentContext::new();

        let result = gen.generate(&content, &mut ctx);
        assert_eq!(result, "Default");
    }

    #[test]
    fn test_generate_counter() {
        let gen = ContentGenerator::new();
        let content = ContentValue::Items(vec![ContentItem::counter("chapter")]);
        let mut ctx = ContentContext::new();
        ctx.set_counter("chapter", 5);

        let result = gen.generate(&content, &mut ctx);
        assert_eq!(result, "5");
    }

    #[test]
    fn test_generate_counter_styled() {
        let gen = ContentGenerator::new();
        let content = ContentValue::Items(vec![ContentItem::counter_styled("chapter", CounterStyle::LowerRoman)]);
        let mut ctx = ContentContext::new();
        ctx.set_counter("chapter", 4);

        let result = gen.generate(&content, &mut ctx);
        assert_eq!(result, "iv");
    }

    #[test]
    fn test_generate_counters() {
        let gen = ContentGenerator::new();
        let content = ContentValue::Items(vec![ContentItem::counters("section", ".")]);
        let mut ctx = ContentContext::new();
        ctx.push_counter("section", 1);
        ctx.push_counter("section", 2);
        ctx.push_counter("section", 3);

        let result = gen.generate(&content, &mut ctx);
        assert_eq!(result, "1.2.3");
    }

    #[test]
    fn test_generate_quotes() {
        let gen = ContentGenerator::new();
        let content = ContentValue::Items(vec![
            ContentItem::OpenQuote,
            ContentItem::String("Hello".to_string()),
            ContentItem::CloseQuote,
        ]);
        let mut ctx = ContentContext::new();

        let result = gen.generate(&content, &mut ctx);
        assert_eq!(result, "\u{201C}Hello\u{201D}"); // "Hello"
    }

    #[test]
    fn test_generate_combined() {
        let gen = ContentGenerator::new();
        let content = ContentValue::Items(vec![
            ContentItem::String("Chapter ".to_string()),
            ContentItem::counter("chapter"),
            ContentItem::String(": ".to_string()),
            ContentItem::attr("title"),
        ]);
        let mut ctx = ContentContext::new();
        ctx.set_counter("chapter", 5);
        ctx.set_attribute("title", "Introduction");

        let result = gen.generate(&content, &mut ctx);
        assert_eq!(result, "Chapter 5: Introduction");
    }

    #[test]
    fn test_generate_none() {
        let gen = ContentGenerator::new();
        let content = ContentValue::None;
        let mut ctx = ContentContext::new();

        let result = gen.generate(&content, &mut ctx);
        assert_eq!(result, "");
    }

    // === Parsing Tests ===

    #[test]
    fn test_parse_none() {
        let content = parse_content("none").unwrap();
        assert_eq!(content, ContentValue::None);
    }

    #[test]
    fn test_parse_normal() {
        let content = parse_content("normal").unwrap();
        assert_eq!(content, ContentValue::Normal);
    }

    #[test]
    fn test_parse_string() {
        let content = parse_content("\"Hello\"").unwrap();
        assert_eq!(
            content,
            ContentValue::Items(vec![ContentItem::String("Hello".to_string())])
        );
    }

    #[test]
    fn test_parse_single_quoted_string() {
        let content = parse_content("'Hello'").unwrap();
        assert_eq!(
            content,
            ContentValue::Items(vec![ContentItem::String("Hello".to_string())])
        );
    }

    #[test]
    fn test_parse_attr() {
        let content = parse_content("attr(data-label)").unwrap();
        assert_eq!(content, ContentValue::Items(vec![ContentItem::attr("data-label")]));
    }

    #[test]
    fn test_parse_attr_with_fallback() {
        let content = parse_content("attr(title, \"Untitled\")").unwrap();
        assert_eq!(
            content,
            ContentValue::Items(vec![ContentItem::attr_with_fallback("title", "Untitled")])
        );
    }

    #[test]
    fn test_parse_counter() {
        let content = parse_content("counter(chapter)").unwrap();
        assert_eq!(content, ContentValue::Items(vec![ContentItem::counter("chapter")]));
    }

    #[test]
    fn test_parse_counter_with_style() {
        let content = parse_content("counter(chapter, upper-roman)").unwrap();
        assert_eq!(
            content,
            ContentValue::Items(vec![ContentItem::counter_styled("chapter", CounterStyle::UpperRoman)])
        );
    }

    #[test]
    fn test_parse_counters() {
        let content = parse_content("counters(section, \".\")").unwrap();
        assert_eq!(
            content,
            ContentValue::Items(vec![ContentItem::counters("section", ".")])
        );
    }

    #[test]
    fn test_parse_open_quote() {
        let content = parse_content("open-quote").unwrap();
        assert_eq!(content, ContentValue::Items(vec![ContentItem::OpenQuote]));
    }

    #[test]
    fn test_parse_url() {
        let content = parse_content("url(\"image.png\")").unwrap();
        assert_eq!(
            content,
            ContentValue::Items(vec![ContentItem::Url("image.png".to_string())])
        );
    }

    #[test]
    fn test_parse_multiple_items() {
        let content = parse_content("\"Chapter \" counter(chapter) \": \"").unwrap();
        if let ContentValue::Items(items) = content {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], ContentItem::String("Chapter ".to_string()));
            assert!(matches!(items[1], ContentItem::Counter { .. }));
            assert_eq!(items[2], ContentItem::String(": ".to_string()));
        } else {
            panic!("Expected Items");
        }
    }

    #[test]
    fn test_parse_escaped_string() {
        let content = parse_content("\"Hello\\nWorld\"").unwrap();
        assert_eq!(
            content,
            ContentValue::Items(vec![ContentItem::String("Hello\nWorld".to_string())])
        );
    }

    // === Integration Tests ===

    #[test]
    fn test_is_text_only() {
        let text = ContentValue::from_string("Hello");
        assert!(ContentGenerator::is_text_only(&text));

        let with_url = ContentValue::Items(vec![ContentItem::Url("image.png".to_string())]);
        assert!(!ContentGenerator::is_text_only(&with_url));
    }

    #[test]
    fn test_full_workflow() {
        // Simulate a list item with counter
        let gen = ContentGenerator::new();
        let mut ctx = ContentContext::new();

        // Reset list counter
        ctx.reset_counter_to("list-item", 0);

        // Generate content for first list item
        ctx.increment_counter("list-item");
        let content = ContentValue::Items(vec![
            ContentItem::counter_styled("list-item", CounterStyle::Decimal),
            ContentItem::String(". ".to_string()),
        ]);
        assert_eq!(gen.generate(&content, &mut ctx), "1. ");

        // Second list item
        ctx.increment_counter("list-item");
        assert_eq!(gen.generate(&content, &mut ctx), "2. ");

        // Third list item
        ctx.increment_counter("list-item");
        assert_eq!(gen.generate(&content, &mut ctx), "3. ");
    }

    #[test]
    fn test_nested_list_counters() {
        let gen = ContentGenerator::new();
        let mut ctx = ContentContext::new();

        let content = ContentValue::Items(vec![ContentItem::counters("item", ".")]);

        // First level
        ctx.push_counter("item", 1);
        assert_eq!(gen.generate(&content, &mut ctx), "1");

        // Second level
        ctx.push_counter("item", 1);
        assert_eq!(gen.generate(&content, &mut ctx), "1.1");

        ctx.increment_counter("item");
        assert_eq!(gen.generate(&content, &mut ctx), "1.2");

        // Third level
        ctx.push_counter("item", 1);
        assert_eq!(gen.generate(&content, &mut ctx), "1.2.1");

        // Pop back
        ctx.pop_counter("item");
        assert_eq!(gen.generate(&content, &mut ctx), "1.2");

        ctx.pop_counter("item");
        ctx.increment_counter("item");
        assert_eq!(gen.generate(&content, &mut ctx), "2");
    }
}
