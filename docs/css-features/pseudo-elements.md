# Phase 5: Pseudo-elements Support

**Duration:** 4-5 days
**Prerequisites:**
- Phase 1 Box Generation complete (01-box-generation.md)
- Style engine complete
- Layout engine complete
**Dependencies:**
- BoxGenerator
- CSS parser (content property)
- ComputedStyle
- Counter system (for counters())
**Output:** Full support for ::before, ::after, ::first-line, ::first-letter

## Objectives

Implement CSS pseudo-elements that generate additional boxes for styling:

1. **::before** - Insert content before an element's content
2. **::after** - Insert content after an element's content
3. **::first-line** - Style the first line of a block
4. **::first-letter** - Style the first letter of a block
5. **content** property - Generate content for ::before and ::after
6. **Counter system** - CSS counters for automatic numbering

## Context

Pseudo-elements are a mechanism for creating elements that don't exist in the DOM but are rendered as if they did. They're essential for:

- Adding decorative content (icons, quotes, etc.)
- Styling specific parts of text (first letter drop caps)
- Automatic numbering (lists, sections)
- Visual effects without extra markup

**From CSS Pseudo-Elements Module Level 4:**
> "A pseudo-element is an abstraction of the document tree beyond those created by the document language. Pseudo-elements can be used to create abstractions about the document tree beyond those specified by the document language."

**Common uses:**
```css
/* Add quotes */
blockquote::before { content: """; }
blockquote::after { content: """; }

/* Drop caps */
p::first-letter {
  font-size: 3em;
  float: left;
}

/* Automatic numbering */
h2 {
  counter-increment: section;
}
h2::before {
  content: "Section " counter(section) ": ";
}
```

## The Problem V1 Has

V1 has partial CSS variable support but no pseudo-elements:
- No ::before or ::after
- No generated content
- No counters
- No ::first-line or ::first-letter styling

This makes it impossible to render many real-world stylesheets correctly.

## The Solution

Implement pseudo-elements as additional boxes generated during box generation:

```
DOM Element
    ↓ (Box Generation)
[::before box] + [Element box] + [::after box]
    ↓ (Layout)
[::before fragment] + [Element fragment] + [::after fragment]
```

## CSS Pseudo-Elements Specification

### Pseudo-element Syntax

Old syntax (CSS 2.1): Single colon `:before`, `:after`
**New syntax (CSS3):** Double colon `::before`, `::after`

We support both for compatibility:
```css
.old:before { content: "old"; }     /* CSS 2.1 */
.new::before { content: "new"; }    /* CSS3 */
```

### ::before and ::after

These pseudo-elements are inserted as **children** of the element:

```html
<div>Content</div>
```

```css
div::before { content: "BEFORE "; }
div::after { content: " AFTER"; }
```

**Renders as if:**
```html
<div>
  <pseudo-before>BEFORE </pseudo-before>
  Content
  <pseudo-after> AFTER</pseudo-after>
</div>
```

**Box tree:**
```
Block <div>
├── Inline ::before (generated content: "BEFORE ")
├── Text "Content"
└── Inline ::after (generated content: " AFTER")
```

### ::first-line

Styles the first formatted line of a block:

```css
p::first-line {
  font-weight: bold;
  color: blue;
}
```

**Special behavior:**
- Only applies to block containers
- Properties limited to: font, color, background, word-spacing, letter-spacing, text-decoration, text-transform, line-height
- Dynamically reflows: the "first line" changes as width changes

### ::first-letter

Styles the first letter (or punctuation + letter) of a block:

```css
p::first-letter {
  font-size: 3em;
  float: left;
  margin-right: 0.1em;
}
```

**Special behavior:**
- Can be floated
- Includes leading punctuation: ("Hello" → " "H )
- Can span multiple Unicode characters (some ligatures)

### content Property

The `content` property specifies what to display in ::before and ::after:

**Values:**
- `none` - No content (default)
- `normal` - Same as none for ::before/::after
- `<string>` - Text content: `content: "Hello"`
- `attr(<attribute>)` - Element's attribute value: `content: attr(data-label)`
- `counter(<counter-name>)` - Counter value: `content: counter(section)`
- `counters(<counter-name>, <separator>)` - Nested counters
- Multiple values: `content: "Section " counter(section) ": "`

**Whitespace:** Content strings preserve whitespace:
```css
content: "Hello    World"; /* Multiple spaces preserved */
```

**Special characters:**
- `\A` - Line feed (newline)
- `\22` - Quote character (")
- `\20` - Space

### Counters

CSS counters enable automatic numbering:

```css
/* Initialize counter */
body {
  counter-reset: section;  /* Creates counter named 'section', value 0 */
}

/* Increment counter */
h2 {
  counter-increment: section;  /* Increments by 1 */
}

/* Display counter */
h2::before {
  content: counter(section) ". ";
}
```

**Output:**
```
1. First Section
2. Second Section
3. Third Section
```

**Nested counters:**
```css
body { counter-reset: chapter; }
h1 { counter-increment: chapter; counter-reset: section; }
h2 { counter-increment: section; }

h1::before { content: counter(chapter) ". "; }
h2::before { content: counter(chapter) "." counter(section) " "; }
```

**Output:**
```
1. Chapter One
1.1 Section
1.2 Section
2. Chapter Two
2.1 Section
```

## Step-by-Step Implementation

### Step 1: Extend CSS Parser for Pseudo-elements (Day 1 Morning)

**File: `src/css/parser.rs`** (additions)

```rust
/// Pseudo-element type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PseudoElement {
    Before,
    After,
    FirstLine,
    FirstLetter,
}

impl PseudoElement {
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "before" => Some(Self::Before),
            "after" => Some(Self::After),
            "first-line" | "first_line" => Some(Self::FirstLine),
            "first-letter" | "first_letter" => Some(Self::FirstLetter),
            _ => None,
        }
    }
}

/// Selector with optional pseudo-element
#[derive(Debug, Clone)]
pub struct Selector {
    pub compound: CompoundSelector,
    pub pseudo_element: Option<PseudoElement>,
}

/// Parse a selector with optional pseudo-element
///
/// Examples:
///   div::before
///   .class:hover::after
///   p::first-line
fn parse_selector(&mut self) -> Result<Selector> {
    let compound = self.parse_compound_selector()?;

    // Check for pseudo-element (::xxx or :xxx)
    let pseudo_element = if self.peek() == Some(':') {
        self.consume(':');

        // CSS3 uses ::, CSS2.1 uses : (support both)
        if self.peek() == Some(':') {
            self.consume(':');
        }

        let name = self.parse_ident()?;
        PseudoElement::from_name(&name)
    } else {
        None
    };

    Ok(Selector {
        compound,
        pseudo_element,
    })
}
```

### Step 2: Parse content Property (Day 1 Afternoon)

**File: `src/css/properties/content.rs`**

```rust
//! Content property parsing and representation
//!
//! CSS Generated Content Module Level 3
//! https://www.w3.org/TR/css-content-3/

/// Content value for ::before and ::after
#[derive(Debug, Clone, PartialEq)]
pub enum ContentValue {
    /// No content
    None,

    /// Normal content (same as None for ::before/::after)
    Normal,

    /// List of content items
    Items(Vec<ContentItem>),
}

/// A single content item
#[derive(Debug, Clone, PartialEq)]
pub enum ContentItem {
    /// String literal
    String(String),

    /// Element attribute value: attr(name)
    Attr(String),

    /// Counter value: counter(name)
    Counter {
        name: String,
        style: CounterStyle,
    },

    /// Nested counter: counters(name, separator)
    Counters {
        name: String,
        separator: String,
        style: CounterStyle,
    },

    /// URL (for images)
    Url(String),
}

/// Counter display style
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CounterStyle {
    Decimal,       // 1, 2, 3, ...
    DecimalLeadingZero, // 01, 02, 03, ...
    LowerAlpha,    // a, b, c, ...
    UpperAlpha,    // A, B, C, ...
    LowerRoman,    // i, ii, iii, iv, ...
    UpperRoman,    // I, II, III, IV, ...
    LowerGreek,    // α, β, γ, ...
    // ... more styles
}

impl ContentValue {
    /// Parses content property value
    ///
    /// Syntax:
    ///   content: none;
    ///   content: "string";
    ///   content: attr(id);
    ///   content: counter(section);
    ///   content: "Chapter " counter(chapter) ".";
    pub fn parse(input: &str) -> Result<Self> {
        let mut parser = Parser::new(input);

        if parser.try_parse_keyword("none") {
            return Ok(Self::None);
        }

        if parser.try_parse_keyword("normal") {
            return Ok(Self::Normal);
        }

        let mut items = Vec::new();

        loop {
            if let Some(item) = parser.parse_content_item()? {
                items.push(item);
            } else {
                break;
            }
        }

        if items.is_empty() {
            return Err(Error::Parse("Expected content value".into()));
        }

        Ok(Self::Items(items))
    }
}

impl Parser {
    /// Parses a single content item
    fn parse_content_item(&mut self) -> Result<Option<ContentItem>> {
        self.skip_whitespace();

        if self.is_eof() {
            return Ok(None);
        }

        // String literal
        if self.peek() == Some('"') || self.peek() == Some('\'') {
            let string = self.parse_string()?;
            return Ok(Some(ContentItem::String(string)));
        }

        // Function: attr(), counter(), counters(), url()
        if self.peek_ident().is_some() {
            let ident = self.parse_ident()?;

            match ident.as_str() {
                "attr" => {
                    self.expect('(')?;
                    let attr_name = self.parse_ident()?;
                    self.expect(')')?;
                    return Ok(Some(ContentItem::Attr(attr_name)));
                }

                "counter" => {
                    self.expect('(')?;
                    let counter_name = self.parse_ident()?;

                    // Optional style: counter(name, style)
                    let style = if self.peek() == Some(',') {
                        self.consume(',');
                        self.skip_whitespace();
                        self.parse_counter_style()?
                    } else {
                        CounterStyle::Decimal
                    };

                    self.expect(')')?;
                    return Ok(Some(ContentItem::Counter {
                        name: counter_name,
                        style,
                    }));
                }

                "counters" => {
                    self.expect('(')?;
                    let counter_name = self.parse_ident()?;
                    self.expect(',')?;
                    self.skip_whitespace();
                    let separator = self.parse_string()?;

                    // Optional style
                    let style = if self.peek() == Some(',') {
                        self.consume(',');
                        self.skip_whitespace();
                        self.parse_counter_style()?
                    } else {
                        CounterStyle::Decimal
                    };

                    self.expect(')')?;
                    return Ok(Some(ContentItem::Counters {
                        name: counter_name,
                        separator,
                        style,
                    }));
                }

                "url" => {
                    self.expect('(')?;
                    let url = self.parse_string_or_url()?;
                    self.expect(')')?;
                    return Ok(Some(ContentItem::Url(url)));
                }

                _ => {
                    return Err(Error::Parse(format!("Unknown content function: {}", ident)));
                }
            }
        }

        Ok(None)
    }

    fn parse_counter_style(&mut self) -> Result<CounterStyle> {
        let ident = self.parse_ident()?;
        match ident.as_str() {
            "decimal" => Ok(CounterStyle::Decimal),
            "decimal-leading-zero" => Ok(CounterStyle::DecimalLeadingZero),
            "lower-alpha" | "lower-latin" => Ok(CounterStyle::LowerAlpha),
            "upper-alpha" | "upper-latin" => Ok(CounterStyle::UpperAlpha),
            "lower-roman" => Ok(CounterStyle::LowerRoman),
            "upper-roman" => Ok(CounterStyle::UpperRoman),
            "lower-greek" => Ok(CounterStyle::LowerGreek),
            _ => Err(Error::Parse(format!("Unknown counter style: {}", ident))),
        }
    }

    fn parse_string(&mut self) -> Result<String> {
        let quote = self.peek().ok_or_else(|| Error::Parse("Expected string".into()))?;

        if quote != '"' && quote != '\'' {
            return Err(Error::Parse("Expected string".into()));
        }

        self.consume(quote);

        let mut result = String::new();
        loop {
            match self.peek() {
                None => return Err(Error::Parse("Unterminated string".into())),
                Some(c) if c == quote => {
                    self.consume(quote);
                    break;
                }
                Some('\\') => {
                    self.consume('\\');
                    // Handle escape sequences
                    match self.peek() {
                        Some('A') => {
                            result.push('\n');
                            self.advance();
                        }
                        Some(c) if c.is_ascii_hexdigit() => {
                            // Unicode escape: \XXXXXX
                            let hex = self.parse_hex_escape()?;
                            if let Some(ch) = char::from_u32(hex) {
                                result.push(ch);
                            }
                        }
                        Some(c) => {
                            result.push(c);
                            self.advance();
                        }
                        None => return Err(Error::Parse("Escape at end of string".into())),
                    }
                }
                Some(c) => {
                    result.push(c);
                    self.advance();
                }
            }
        }

        Ok(result)
    }
}
```

### Step 3: Generate Pseudo-element Boxes (Day 2)

**File: `src/tree/box_generation.rs`** (additions)

```rust
impl BoxGenerator {
    /// Generates boxes for an element, including pseudo-elements
    fn generate_box_with_pseudos(
        &self,
        node: &DomNode,
        parent_box_type: Option<BoxType>,
    ) -> Result<BoxNode> {
        let style = node.computed_style()
            .ok_or_else(|| Error::Layout("Node has no computed style".into()))?;

        if !self.generates_box(&style) {
            return Err(Error::Layout("Node doesn't generate box".into()));
        }

        let box_type = self.compute_box_type(&style.display);

        // Generate ::before pseudo-element
        let before_box = self.generate_pseudo_element(node, PseudoElement::Before)?;

        // Generate children
        let mut children = Vec::new();

        // Add ::before first
        if let Some(before) = before_box {
            children.push(before);
        }

        // Add normal children
        for child_dom in node.children() {
            match self.generate_box_with_pseudos(child_dom, Some(box_type)) {
                Ok(child_box) => children.push(child_box),
                Err(_) => {} // Skip boxes that don't generate
            }
        }

        // Add text content
        if let Some(text) = node.text_content() {
            if !text.trim().is_empty() {
                let text_box = self.create_text_box(text, style.clone());
                children.push(text_box);
            }
        }

        // Generate ::after pseudo-element
        let after_box = self.generate_pseudo_element(node, PseudoElement::After)?;

        // Add ::after last
        if let Some(after) = after_box {
            children.push(after);
        }

        // Create the box node
        Ok(BoxNode::new(
            style.clone(),
            box_type,
            children,
            Some(node.clone()),
        ))
    }

    /// Generates a pseudo-element box if it has content
    fn generate_pseudo_element(
        &self,
        node: &DomNode,
        pseudo: PseudoElement,
    ) -> Result<Option<BoxNode>> {
        // Get style for pseudo-element
        let pseudo_style = node.get_pseudo_element_style(pseudo)
            .ok_or_else(|| Error::Layout("No pseudo-element style".into()))?;

        // Check if content property is set
        let content = &pseudo_style.content;

        match content {
            ContentValue::None | ContentValue::Normal => {
                // No content, no box
                return Ok(None);
            }
            ContentValue::Items(items) => {
                // Generate content
                let content_string = self.generate_content(items, node)?;

                // Create pseudo-element box
                let text_box = BoxNode::new_text(content_string, pseudo_style.clone());

                // Pseudo-elements are inline by default
                let pseudo_box = BoxNode::new_pseudo(
                    pseudo_style.clone(),
                    BoxType::Inline,
                    vec![text_box],
                    pseudo,
                );

                Ok(Some(pseudo_box))
            }
        }
    }

    /// Generates content string from content items
    fn generate_content(
        &self,
        items: &[ContentItem],
        node: &DomNode,
    ) -> Result<String> {
        let mut result = String::new();

        for item in items {
            match item {
                ContentItem::String(s) => {
                    result.push_str(s);
                }

                ContentItem::Attr(attr_name) => {
                    // Get attribute value from element
                    if let Some(value) = node.get_attribute(attr_name) {
                        result.push_str(&value);
                    }
                }

                ContentItem::Counter { name, style } => {
                    // Get counter value
                    let value = self.counter_system.get_counter(name);
                    let formatted = self.format_counter(value, *style);
                    result.push_str(&formatted);
                }

                ContentItem::Counters { name, separator, style } => {
                    // Get nested counter values
                    let values = self.counter_system.get_counters(name);
                    let formatted_values: Vec<String> = values
                        .iter()
                        .map(|&v| self.format_counter(v, *style))
                        .collect();
                    result.push_str(&formatted_values.join(separator));
                }

                ContentItem::Url(url) => {
                    // TODO: Load image and insert
                    // For now, just skip
                }
            }
        }

        Ok(result)
    }

    /// Formats a counter value according to style
    fn format_counter(&self, value: i32, style: CounterStyle) -> String {
        match style {
            CounterStyle::Decimal => value.to_string(),

            CounterStyle::DecimalLeadingZero => {
                if value < 10 {
                    format!("0{}", value)
                } else {
                    value.to_string()
                }
            }

            CounterStyle::LowerAlpha => {
                // a, b, c, ..., z, aa, ab, ...
                self.to_alphabetic(value, false)
            }

            CounterStyle::UpperAlpha => {
                // A, B, C, ..., Z, AA, AB, ...
                self.to_alphabetic(value, true)
            }

            CounterStyle::LowerRoman => {
                // i, ii, iii, iv, v, ...
                self.to_roman(value, false)
            }

            CounterStyle::UpperRoman => {
                // I, II, III, IV, V, ...
                self.to_roman(value, true)
            }

            CounterStyle::LowerGreek => {
                // α, β, γ, ...
                self.to_greek(value)
            }
        }
    }

    fn to_alphabetic(&self, mut value: i32, uppercase: bool) -> String {
        if value <= 0 {
            return value.to_string();
        }

        let base = if uppercase { 'A' } else { 'a' };
        let mut result = String::new();

        while value > 0 {
            value -= 1;
            let digit = (value % 26) as u8;
            result.insert(0, (base as u8 + digit) as char);
            value /= 26;
        }

        result
    }

    fn to_roman(&self, value: i32, uppercase: bool) -> String {
        if value <= 0 || value >= 4000 {
            return value.to_string();
        }

        let values = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
        let symbols_lower = ["m", "cm", "d", "cd", "c", "xc", "l", "xl", "x", "ix", "v", "iv", "i"];
        let symbols_upper = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"];

        let symbols = if uppercase { &symbols_upper } else { &symbols_lower };

        let mut result = String::new();
        let mut remaining = value;

        for (i, &val) in values.iter().enumerate() {
            while remaining >= val {
                result.push_str(symbols[i]);
                remaining -= val;
            }
        }

        result
    }

    fn to_greek(&self, value: i32) -> String {
        if value <= 0 || value > 24 {
            return value.to_string();
        }

        // Greek lowercase: α=1, β=2, ..., ω=24
        let greek_letters = "αβγδεζηθικλμνξοπρστυφχψω";
        greek_letters.chars().nth((value - 1) as usize).unwrap().to_string()
    }
}
```

### Step 4: Implement Counter System (Day 2-3)

**File: `src/tree/counters.rs`**

```rust
//! CSS Counter system
//!
//! CSS Lists Module Level 3
//! https://www.w3.org/TR/css-lists-3/#auto-numbering

use std::collections::HashMap;

/// CSS counter system
///
/// Tracks counter values during box generation.
/// Counters are scoped to elements and inherit down the tree.
#[derive(Debug)]
pub struct CounterSystem {
    /// Stack of counter scopes (one per nesting level)
    scopes: Vec<CounterScope>,
}

/// A single counter scope
#[derive(Debug)]
struct CounterScope {
    /// Counter name → current value
    counters: HashMap<String, i32>,
}

impl CounterSystem {
    pub fn new() -> Self {
        Self {
            scopes: vec![CounterScope::new()],
        }
    }

    /// Pushes a new scope (when entering an element)
    pub fn push_scope(&mut self) {
        self.scopes.push(CounterScope::new());
    }

    /// Pops a scope (when leaving an element)
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Resets a counter to a value (counter-reset property)
    ///
    /// # Arguments
    ///
    /// * `name` - Counter name
    /// * `value` - Initial value (default 0)
    pub fn reset_counter(&mut self, name: &str, value: i32) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.counters.insert(name.to_string(), value);
        }
    }

    /// Increments a counter (counter-increment property)
    ///
    /// # Arguments
    ///
    /// * `name` - Counter name
    /// * `delta` - Amount to increment (default 1)
    pub fn increment_counter(&mut self, name: &str, delta: i32) {
        // Find counter in current scope or ancestor scopes
        for scope in self.scopes.iter_mut().rev() {
            if let Some(value) = scope.counters.get_mut(name) {
                *value += delta;
                return;
            }
        }

        // Counter doesn't exist, create it with increment value
        if let Some(scope) = self.scopes.last_mut() {
            scope.counters.insert(name.to_string(), delta);
        }
    }

    /// Gets the current value of a counter
    ///
    /// # Arguments
    ///
    /// * `name` - Counter name
    ///
    /// # Returns
    ///
    /// Current value, or 0 if counter doesn't exist
    pub fn get_counter(&self, name: &str) -> i32 {
        // Search scopes from innermost to outermost
        for scope in self.scopes.iter().rev() {
            if let Some(&value) = scope.counters.get(name) {
                return value;
            }
        }

        0 // Default value
    }

    /// Gets all values of a counter in nested scopes
    ///
    /// Used for counters() function.
    ///
    /// # Arguments
    ///
    /// * `name` - Counter name
    ///
    /// # Returns
    ///
    /// Vector of values from outermost to innermost scope
    pub fn get_counters(&self, name: &str) -> Vec<i32> {
        let mut values = Vec::new();

        for scope in &self.scopes {
            if let Some(&value) = scope.counters.get(name) {
                values.push(value);
            }
        }

        values
    }
}

impl CounterScope {
    fn new() -> Self {
        Self {
            counters: HashMap::new(),
        }
    }
}

impl Default for CounterSystem {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_counter_reset() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 0);
        assert_eq!(system.get_counter("section"), 0);
    }

    #[test]
    fn test_counter_increment() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 0);
        system.increment_counter("section", 1);
        assert_eq!(system.get_counter("section"), 1);
        system.increment_counter("section", 1);
        assert_eq!(system.get_counter("section"), 2);
    }

    #[test]
    fn test_nested_counters() {
        let mut system = CounterSystem::new();

        // Root scope
        system.reset_counter("chapter", 0);
        system.increment_counter("chapter", 1); // chapter = 1

        // Enter nested scope (new section)
        system.push_scope();
        system.reset_counter("section", 0);
        system.increment_counter("section", 1); // section = 1

        // counters("section") should return [1]
        assert_eq!(system.get_counters("section"), vec![1]);

        // Increment again
        system.increment_counter("section", 1); // section = 2
        assert_eq!(system.get_counter("section"), 2);

        // Pop scope
        system.pop_scope();

        // section counter is gone
        assert_eq!(system.get_counter("section"), 0);
    }

    #[test]
    fn test_multilevel_nesting() {
        let mut system = CounterSystem::new();

        // Level 1
        system.reset_counter("chapter", 0);
        system.increment_counter("chapter", 1); // 1

        system.push_scope();
        system.reset_counter("section", 0);
        system.increment_counter("section", 1); // 1

        system.push_scope();
        system.increment_counter("section", 1); // Creates new section counter = 1

        // counters("chapter") = [1]
        assert_eq!(system.get_counters("chapter"), vec![1]);

        // counters("section") = [1, 1]
        assert_eq!(system.get_counters("section"), vec![1, 1]);
    }
}
```

### Step 5: Handle ::first-line and ::first-letter (Day 3-4)

These are more complex because they're created **during layout**, not during box generation.

**File: `src/layout/contexts/inline/first_line.rs`**

```rust
//! ::first-line pseudo-element handling
//!
//! The ::first-line pseudo-element is special:
//! - It's created during layout, not box generation
//! - It applies to the first formatted line only
//! - Limited set of properties apply

use crate::tree::{BoxNode, FragmentNode};
use crate::style::ComputedStyle;

/// Applies ::first-line styling to the first line of fragments
///
/// This is called after line breaking, when we know which boxes
/// are on the first line.
pub fn apply_first_line_style(
    line_fragments: &mut [FragmentNode],
    first_line_style: &ComputedStyle,
) {
    // Only certain properties apply to ::first-line:
    // - font properties
    // - color
    // - background
    // - word-spacing, letter-spacing
    // - text-decoration
    // - text-transform
    // - line-height

    for fragment in line_fragments {
        fragment.apply_first_line_overrides(first_line_style);
    }
}

/// Checks if an element has ::first-line styling
pub fn has_first_line_style(box_node: &BoxNode) -> bool {
    box_node.pseudo_element_style(PseudoElement::FirstLine).is_some()
}
```

**File: `src/layout/contexts/inline/first_letter.rs`**

```rust
//! ::first-letter pseudo-element handling

use crate::tree::{BoxNode, FragmentNode};
use crate::style::ComputedStyle;

/// Applies ::first-letter styling
///
/// Creates a special fragment for the first letter.
/// The first letter can be floated!
pub fn create_first_letter_fragment(
    text: &str,
    first_letter_style: &ComputedStyle,
    base_style: &ComputedStyle,
) -> (FragmentNode, String) {
    // Extract first letter (including leading punctuation)
    let (first_part, rest) = extract_first_letter(text);

    // Create fragment for first letter
    let first_letter_fragment = FragmentNode::new_text(
        first_part,
        first_letter_style.clone(),
    );

    (first_letter_fragment, rest.to_string())
}

/// Extracts the first letter from text
///
/// Includes leading punctuation and combines certain characters.
///
/// Examples:
///   "Hello" → ("H", "ello")
///   ""Hello" → (""H", "ello")
///   "«Hello»" → ("«H", "ello»")
fn extract_first_letter(text: &str) -> (&str, &str) {
    let mut chars = text.char_indices();

    // Skip leading punctuation and spaces
    let mut start = 0;
    let mut first_letter_end = 0;

    for (i, ch) in &mut chars {
        if ch.is_whitespace() {
            start = i + ch.len_utf8();
            continue;
        }

        if is_punctuation(ch) {
            // Include punctuation before first letter
            continue;
        }

        // Found first letter
        first_letter_end = i + ch.len_utf8();
        break;
    }

    if first_letter_end == 0 {
        // No letter found
        return (text, "");
    }

    text.split_at(first_letter_end)
}

fn is_punctuation(ch: char) -> bool {
    matches!(ch,
        '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/' |
        ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~' |
        '«' | '»' | '"' | '"' | ''' | '''
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_first_letter() {
        assert_eq!(extract_first_letter("Hello"), ("H", "ello"));
        assert_eq!(extract_first_letter("\"Hello\""), ("\"H", "ello\""));
        assert_eq!(extract_first_letter("  Hello"), ("  H", "ello"));
        assert_eq!(extract_first_letter("«Hello»"), ("«H", "ello»"));
    }
}
```

### Step 6: Tests (Day 4-5)

**File: `tests/pseudo_elements_test.rs`**

```rust
//! Tests for pseudo-elements

use fastrender::*;

#[test]
fn test_before_pseudo_element() {
    let html = r#"<div>Content</div>"#;
    let css = r#"
        div::before {
            content: "BEFORE ";
        }
    "#;

    let rendered = render(html, css);

    // Should render "BEFORE Content"
    assert!(rendered.contains_text("BEFORE Content"));
}

#[test]
fn test_after_pseudo_element() {
    let html = r#"<div>Content</div>"#;
    let css = r#"
        div::after {
            content: " AFTER";
        }
    "#;

    let rendered = render(html, css);
    assert!(rendered.contains_text("Content AFTER"));
}

#[test]
fn test_both_before_and_after() {
    let html = r#"<div>Content</div>"#;
    let css = r#"
        div::before { content: "["; }
        div::after { content: "]"; }
    "#;

    let rendered = render(html, css);
    assert!(rendered.contains_text("[Content]"));
}

#[test]
fn test_attr_function() {
    let html = r#"<div data-label="Label: ">Content</div>"#;
    let css = r#"
        div::before {
            content: attr(data-label);
        }
    "#;

    let rendered = render(html, css);
    assert!(rendered.contains_text("Label: Content"));
}

#[test]
fn test_counter() {
    let html = r#"
        <div>
            <h2>First</h2>
            <h2>Second</h2>
            <h2>Third</h2>
        </div>
    "#;
    let css = r#"
        div {
            counter-reset: section;
        }
        h2 {
            counter-increment: section;
        }
        h2::before {
            content: counter(section) ". ";
        }
    "#;

    let rendered = render(html, css);
    assert!(rendered.contains_text("1. First"));
    assert!(rendered.contains_text("2. Second"));
    assert!(rendered.contains_text("3. Third"));
}

#[test]
fn test_nested_counters() {
    let html = r#"
        <div>
            <h1>Chapter 1</h1>
            <h2>Section 1</h2>
            <h2>Section 2</h2>
            <h1>Chapter 2</h1>
            <h2>Section 1</h2>
        </div>
    "#;
    let css = r#"
        div { counter-reset: chapter; }
        h1 { counter-increment: chapter; counter-reset: section; }
        h2 { counter-increment: section; }
        h1::before { content: counter(chapter) ". "; }
        h2::before { content: counter(chapter) "." counter(section) " "; }
    "#;

    let rendered = render(html, css);
    assert!(rendered.contains_text("1. Chapter 1"));
    assert!(rendered.contains_text("1.1 Section 1"));
    assert!(rendered.contains_text("1.2 Section 2"));
    assert!(rendered.contains_text("2. Chapter 2"));
    assert!(rendered.contains_text("2.1 Section 1"));
}

#[test]
fn test_first_letter() {
    let html = r#"<p>Hello world</p>"#;
    let css = r#"
        p::first-letter {
            font-size: 2em;
            color: red;
        }
    "#;

    let rendered = render(html, css);

    // First letter "H" should be larger and red
    let first_letter_fragment = rendered.find_text_fragment("H");
    assert_eq!(first_letter_fragment.font_size, 32.0); // 2em of 16px
    assert_eq!(first_letter_fragment.color, Color::RED);
}

#[test]
fn test_first_line() {
    let html = r#"<p style="width: 100px">This is a long paragraph that will wrap across multiple lines</p>"#;
    let css = r#"
        p::first-line {
            font-weight: bold;
        }
    "#;

    let rendered = render(html, css);

    // Only first line should be bold
    // (Exact text depends on line breaking)
}
```

## Acceptance Criteria

- [ ] ::before pseudo-element generates boxes with content
- [ ] ::after pseudo-element generates boxes with content
- [ ] content property supports strings
- [ ] content property supports attr() function
- [ ] content property supports counter() function
- [ ] content property supports counters() function
- [ ] counter-reset property works
- [ ] counter-increment property works
- [ ] Nested counters work correctly
- [ ] Counter styles work (decimal, roman, alphabetic, etc.)
- [ ] ::first-line applies to first formatted line
- [ ] ::first-letter applies to first letter (including punctuation)
- [ ] All tests pass: `cargo test pseudo_elements`
- [ ] Visual regression tests match browser rendering
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Generating ::before/::after Without content

**Wrong:**
```rust
// Always generate ::before box
let before = generate_pseudo_element(PseudoElement::Before);
```

**Right:**
```rust
// Only generate if content property is set
if pseudo_style.content != ContentValue::None {
    let before = generate_pseudo_element(...);
}
```

### Pitfall 2: Applying ::first-line Too Early

**Wrong:**
```rust
// Apply during box generation
apply_first_line_style(box_node);
```

**Right:**
```rust
// Apply during/after line breaking
// (We don't know what the first line is until layout!)
after_line_break(|first_line_boxes| {
    apply_first_line_style(first_line_boxes);
});
```

### Pitfall 3: Not Handling Counter Scope

**Wrong:**
```rust
// Global counter map
let mut counters = HashMap::new();
```

**Right:**
```rust
// Scoped counters that inherit down the tree
counter_system.push_scope(); // Entering element
// ... process element ...
counter_system.pop_scope();  // Leaving element
```

## Next Steps

After pseudo-elements:
- Test complex generated content scenarios
- Performance optimization for counter lookups
- Add more counter styles (Hebrew, CJK, etc.)

## References

- **CSS Pseudo-Elements Module Level 4:**
  - https://www.w3.org/TR/css-pseudo-4/
- **CSS Generated Content Module Level 3:**
  - https://www.w3.org/TR/css-content-3/
- **CSS Lists Module Level 3:**
  - https://www.w3.org/TR/css-lists-3/
- **CSS 2.1 Section 12: Generated content:**
  - https://www.w3.org/TR/CSS21/generate.html

---

**Last Updated:** 2025-01-19
**Status:** Ready for Implementation
