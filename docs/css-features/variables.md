# Phase 5: CSS Custom Properties (Variables)

**Duration:** 3-4 days
**Prerequisites:**
- Phase 1 Style Engine complete
- Computed values system
- Cascade system
**Dependencies:**
- CSS parser
- Computed style system
- Inheritance system
**Output:** Complete CSS custom properties support (fixing V1 issues)

## Objectives

Implement full CSS Custom Properties (CSS Variables) support:

1. **Custom property declaration** - `--my-color: blue`
2. **var() function** - `color: var(--my-color)`
3. **Inheritance** - Custom properties inherit down the tree
4. **Fallback values** - `var(--undefined, red)`
5. **Invalid values** - Handle guaranteed-invalid values
6. **Cycle detection** - Prevent infinite loops
7. **Integration** - Work with computed values

## Context

CSS Custom Properties (also called CSS Variables) allow authors to define reusable values in CSS. They're essential for:

- **Theming:** Define color schemes, spacing scales
- **Dynamic values:** Change values with JavaScript
- **Reducing repetition:** DRY principle
- **Component APIs:** Configure components via CSS

**From CSS Custom Properties for Cascading Variables Module Level 1:**
> "This module introduces cascading variables as a new primitive value type that is accepted by all CSS properties, and custom properties for defining them."

**Examples:**
```css
/* Define variables on root */
:root {
  --primary-color: #007bff;
  --spacing-unit: 8px;
  --font-stack: -apple-system, sans-serif;
}

/* Use variables */
.button {
  background: var(--primary-color);
  padding: var(--spacing-unit);
  font-family: var(--font-stack);
}

/* Override in scope */
.button.danger {
  --primary-color: #dc3545;
}

/* Fallback values */
.widget {
  color: var(--text-color, black);
}

/* Computed values */
.card {
  --spacing: 16px;
  padding: var(--spacing);
  margin: calc(var(--spacing) * 2); /* 32px */
}
```

## The Problem V1 Has

V1 has **partial** CSS variable support with bugs:

1. **No proper inheritance:** Variables don't inherit correctly
2. **No cycle detection:** Can cause infinite loops
3. **No guaranteed-invalid handling:** Doesn't handle invalid values per spec
4. **No fallback values:** `var(--x, fallback)` doesn't work
5. **No integration with calc():** Can't use variables in calc()

This causes:
- Broken themes
- Inconsistent variable resolution
- Crashes on circular dependencies

## The Solution

Implement CSS variables properly following the spec:

```
Declaration:
  --my-color: blue;
      ↓
  CustomProperty {
    name: "my-color",
    value: DeclaredValue::Color(blue),
  }

Usage:
  color: var(--my-color);
      ↓
  Resolve var() during computed value time
      ↓
  Lookup --my-color in element/ancestors
      ↓
  Found: blue
      ↓
  color: blue (computed value)
```

## CSS Custom Properties Specification

### Custom Property Names

Custom properties start with `--`:

```css
/* Valid */
--color: red;
--primary-color: blue;
--spacing-1: 8px;
--UPPERCASE: allowed;
--dash-case: preferred;
--123: numbers-ok;
--_underscore: valid;

/* Invalid */
-single-dash: error;  /* Must be -- */
color: red;           /* Not a custom property */
```

### Custom Property Values

Custom properties accept **any** value:

```css
--color: red;                    /* Color */
--length: 10px;                  /* Length */
--number: 42;                    /* Number */
--string: "hello";               /* String */
--list: 1px solid red;           /* List */
--complex: calc(100% - 20px);    /* Expression */
--empty:;                        /* Empty (valid!) */
```

**Important:** Values are NOT parsed until use time!

```css
--invalid: 20px 30px;  /* Valid declaration (even though it's not a valid color) */

.foo {
  color: var(--invalid);  /* INVALID at use time, becomes guaranteed-invalid */
}

.bar {
  padding: var(--invalid);  /* VALID at use time (valid padding) */
}
```

### Inheritance

Custom properties **inherit** by default:

```css
.parent {
  --color: blue;
}

.child {
  /* Inherits --color: blue from parent */
  color: var(--color);  /* blue */
}
```

**Override in child:**
```css
.parent {
  --color: blue;
}

.child {
  --color: red;  /* Overrides parent */
  color: var(--color);  /* red */
}
```

### var() Function

The `var()` function substitutes a custom property value:

**Syntax:**
```
var(<custom-property-name>, <fallback>?)
```

**Examples:**
```css
color: var(--text-color);
background: var(--bg-color, white);  /* Fallback */
padding: var(--spacing, var(--default-spacing, 8px));  /* Nested fallback */
```

**Fallback is used when:**
- Property is not defined
- Property value is the guaranteed-invalid value

**Fallback is NOT used when:**
- Property is defined with any value (even empty!)

```css
.element {
  --color:;  /* Empty value */
  color: var(--color, red);  /* Result: empty (invalid), not red */
  /* Because --color IS defined, fallback is not used */
}
```

### Guaranteed-Invalid Value

When a custom property value is invalid for the property it's used in, it becomes the **guaranteed-invalid value**.

**From the spec:**
> "If a property contains a var() function, and that var() function is not valid, the property is instead set to its initial value."

**Example:**
```css
.element {
  --not-a-color: 20px;
  color: blue;                    /* Valid */
  color: var(--not-a-color);      /* Invalid for color, becomes guaranteed-invalid */
  /* Result: color is 'initial' (not blue, not 20px) */
}
```

### Cycles

Circular dependencies are invalid:

```css
/* Cycle! */
.element {
  --a: var(--b);
  --b: var(--a);
  /* Both become guaranteed-invalid */
}

/* Self-reference is also a cycle */
.element {
  --color: var(--color);
  /* Becomes guaranteed-invalid */
}
```

**Detection:** During resolution, track which variables are currently being resolved.

### At-Computed-Value Time

Custom property substitution happens **at computed-value time**, not parse time:

```
Parse time:
  color: var(--color);
  ↓
  Store as: color: VarReference("--color")

Computed value time:
  Resolve var(--color)
  ↓
  Lookup --color in element/ancestors
  ↓
  Substitute value
  ↓
  Parse substituted value for 'color' property
  ↓
  Compute final value
```

This means:
- Variable resolution happens during style computation
- Same variable can resolve to different values in different contexts
- Variables can contain any syntax (validated only when used)

## Step-by-Step Implementation

### Step 1: Parse Custom Property Declarations (Day 1 Morning)

**File: `src/css/parser.rs`** (additions)

```rust
/// Parses a property declaration
///
/// Could be a regular property or a custom property
fn parse_declaration(&mut self) -> Result<Declaration> {
    let property_name = self.parse_ident()?;

    self.skip_whitespace();
    self.expect(':')?;
    self.skip_whitespace();

    // Check if it's a custom property (starts with --)
    if property_name.starts_with("--") {
        // Custom property: store value as-is (don't parse yet!)
        let value_tokens = self.parse_value_tokens_until(';')?;

        return Ok(Declaration::Custom {
            name: property_name,
            value: value_tokens,
        });
    }

    // Regular property: parse value
    let value = self.parse_property_value(&property_name)?;

    Ok(Declaration::Regular {
        property: property_name,
        value,
    })
}

/// Parses value tokens without interpretation
///
/// Used for custom properties, which are validated only when used
fn parse_value_tokens_until(&mut self, terminator: char) -> Result<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut nesting = 0;  // Track (), {}, []

    loop {
        self.skip_whitespace();

        if self.is_eof() {
            break;
        }

        let ch = self.peek().unwrap();

        // Check for terminator (if not nested)
        if nesting == 0 && ch == terminator {
            break;
        }

        // Track nesting
        match ch {
            '(' | '[' | '{' => nesting += 1,
            ')' | ']' | '}' => nesting -= 1,
            _ => {}
        }

        // Parse token
        let token = self.parse_token()?;
        tokens.push(token);
    }

    Ok(tokens)
}

/// CSS token (for custom property values)
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Function(String, Vec<Token>),  // name, args
    Number(f32),
    Dimension(f32, String),  // value, unit
    Percentage(f32),
    String(String),
    Hash(String),  // #xxx
    Delim(char),
    Whitespace,
}
```

### Step 2: Store Custom Properties in ComputedStyle (Day 1 Afternoon)

**File: `src/style/computed.rs`** (additions)

```rust
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ComputedStyle {
    // ... existing properties ...

    /// Custom properties (CSS variables)
    ///
    /// These are inherited by default.
    pub custom_properties: HashMap<String, CustomPropertyValue>,
}

/// A custom property value
#[derive(Debug, Clone, PartialEq)]
pub enum CustomPropertyValue {
    /// Valid value (tokens to be interpreted when used)
    Valid(Vec<Token>),

    /// Invalid value (guaranteed-invalid)
    Invalid,

    /// Initial value (not set)
    Initial,
}

impl ComputedStyle {
    /// Gets a custom property value, checking this element and ancestors
    pub fn get_custom_property(&self, name: &str) -> Option<&CustomPropertyValue> {
        self.custom_properties.get(name)
    }

    /// Sets a custom property
    pub fn set_custom_property(&mut self, name: String, value: CustomPropertyValue) {
        self.custom_properties.insert(name, value);
    }

    /// Inherits custom properties from parent
    pub fn inherit_custom_properties(&mut self, parent: &ComputedStyle) {
        // Custom properties inherit by default
        for (name, value) in &parent.custom_properties {
            // Don't override if already set on this element
            if !self.custom_properties.contains_key(name) {
                self.custom_properties.insert(name.clone(), value.clone());
            }
        }
    }
}
```

### Step 3: Parse var() Function (Day 1 Afternoon)

**File: `src/css/values.rs`** (additions)

```rust
/// A value that may contain var() references
#[derive(Debug, Clone, PartialEq)]
pub enum ValueWithVars {
    /// Simple value (no variables)
    Simple(Value),

    /// Contains var() references
    WithVars(Vec<ValueComponent>),
}

/// Component of a value that may contain variables
#[derive(Debug, Clone, PartialEq)]
pub enum ValueComponent {
    /// Literal token
    Token(Token),

    /// var() reference
    Var {
        name: String,
        fallback: Option<Vec<ValueComponent>>,
    },
}

/// Parses a value that may contain var()
fn parse_value_with_vars(&mut self) -> Result<ValueWithVars> {
    let components = self.parse_value_components()?;

    // Check if any component is a var()
    let has_vars = components.iter().any(|c| matches!(c, ValueComponent::Var { .. }));

    if has_vars {
        Ok(ValueWithVars::WithVars(components))
    } else {
        // No vars, convert to simple value
        let value = self.components_to_value(components)?;
        Ok(ValueWithVars::Simple(value))
    }
}

fn parse_value_components(&mut self) -> Result<Vec<ValueComponent>> {
    let mut components = Vec::new();

    loop {
        self.skip_whitespace();

        if self.is_eof() || self.peek() == Some(';') || self.peek() == Some('}') {
            break;
        }

        // Check for var() function
        if self.peek_ident() == Some("var") {
            let var_component = self.parse_var_function()?;
            components.push(var_component);
        } else {
            // Regular token
            let token = self.parse_token()?;
            components.push(ValueComponent::Token(token));
        }
    }

    Ok(components)
}

fn parse_var_function(&mut self) -> Result<ValueComponent> {
    self.expect_keyword("var")?;
    self.expect('(')?;
    self.skip_whitespace();

    // Parse custom property name
    if !self.peek_ident().map_or(false, |s| s.starts_with("--")) {
        return Err(Error::Parse("var() requires custom property name (--xxx)".into()));
    }

    let name = self.parse_ident()?;
    self.skip_whitespace();

    // Parse optional fallback
    let fallback = if self.peek() == Some(',') {
        self.consume(',');
        self.skip_whitespace();

        // Parse fallback value (can also contain var()!)
        let fallback_components = self.parse_value_components_until(')')?;
        Some(fallback_components)
    } else {
        None
    };

    self.expect(')')?;

    Ok(ValueComponent::Var { name, fallback })
}

fn parse_value_components_until(&mut self, terminator: char) -> Result<Vec<ValueComponent>> {
    let mut components = Vec::new();
    let mut nesting = 0;

    loop {
        self.skip_whitespace();

        if self.is_eof() {
            break;
        }

        let ch = self.peek().unwrap();

        if nesting == 0 && ch == terminator {
            break;
        }

        match ch {
            '(' => nesting += 1,
            ')' => {
                if nesting == 0 {
                    break;
                }
                nesting -= 1;
            }
            _ => {}
        }

        // Parse component
        if self.peek_ident() == Some("var") {
            let var_component = self.parse_var_function()?;
            components.push(var_component);
        } else {
            let token = self.parse_token()?;
            components.push(ValueComponent::Token(token));
        }
    }

    Ok(components)
}
```

### Step 4: Resolve var() References (Day 2)

**File: `src/style/var_resolver.rs`**

```rust
//! CSS variable (custom property) resolution
//!
//! Handles var() substitution with:
//! - Inheritance lookup
//! - Fallback values
//! - Cycle detection
//! - Guaranteed-invalid value handling

use crate::style::{ComputedStyle, CustomPropertyValue};
use crate::css::{ValueComponent, Token};
use std::collections::HashSet;

/// Variable resolver with cycle detection
pub struct VarResolver<'a> {
    /// Current element's style
    style: &'a ComputedStyle,

    /// Stack of variables currently being resolved (for cycle detection)
    resolving: HashSet<String>,
}

impl<'a> VarResolver<'a> {
    pub fn new(style: &'a ComputedStyle) -> Self {
        Self {
            style,
            resolving: HashSet::new(),
        }
    }

    /// Resolves a value containing var() references
    ///
    /// # Arguments
    ///
    /// * `components` - Value components (may contain var())
    ///
    /// # Returns
    ///
    /// Resolved tokens, or guaranteed-invalid
    pub fn resolve(&mut self, components: &[ValueComponent]) -> Result<Vec<Token>> {
        let mut result = Vec::new();

        for component in components {
            match component {
                ValueComponent::Token(token) => {
                    result.push(token.clone());
                }

                ValueComponent::Var { name, fallback } => {
                    // Resolve var() reference
                    let resolved = self.resolve_var(name, fallback)?;
                    result.extend(resolved);
                }
            }
        }

        Ok(result)
    }

    /// Resolves a var() reference
    fn resolve_var(
        &mut self,
        name: &str,
        fallback: &Option<Vec<ValueComponent>>,
    ) -> Result<Vec<Token>> {
        // Check for cycle
        if self.resolving.contains(name) {
            // Cycle detected!
            return Err(Error::CycleDetected(name.to_string()));
        }

        // Mark as currently resolving
        self.resolving.insert(name.to_string());

        // Look up custom property
        let result = match self.style.get_custom_property(name) {
            Some(CustomPropertyValue::Valid(tokens)) => {
                // Found valid value
                Ok(tokens.clone())
            }

            Some(CustomPropertyValue::Invalid) => {
                // Variable is invalid, use fallback
                self.resolve_fallback(fallback)
            }

            Some(CustomPropertyValue::Initial) | None => {
                // Variable not set, use fallback
                self.resolve_fallback(fallback)
            }
        };

        // Unmark
        self.resolving.remove(name);

        result
    }

    /// Resolves fallback value
    fn resolve_fallback(
        &mut self,
        fallback: &Option<Vec<ValueComponent>>,
    ) -> Result<Vec<Token>> {
        match fallback {
            Some(fallback_components) => {
                // Resolve fallback (may also contain var()!)
                self.resolve(fallback_components)
            }
            None => {
                // No fallback, guaranteed-invalid
                Err(Error::InvalidValue("No fallback for undefined variable".into()))
            }
        }
    }
}

/// Resolves var() in a property value
///
/// This is called during computed value calculation.
pub fn resolve_property_value(
    property: &str,
    value_components: &[ValueComponent],
    style: &ComputedStyle,
) -> Result<Value> {
    let mut resolver = VarResolver::new(style);

    // Resolve var() references
    let tokens = resolver.resolve(value_components)?;

    // Parse tokens as the property's value type
    let value = parse_tokens_for_property(property, &tokens)?;

    Ok(value)
}

/// Parses tokens for a specific property
///
/// The same tokens can be valid for one property but invalid for another!
fn parse_tokens_for_property(property: &str, tokens: &[Token]) -> Result<Value> {
    // Create a temporary parser from tokens
    let token_string = tokens_to_string(tokens);
    let mut parser = Parser::new(&token_string);

    // Parse based on property type
    match property {
        "color" => {
            let color = parser.parse_color()?;
            Ok(Value::Color(color))
        }

        "width" | "height" | "padding" | "margin" => {
            let length = parser.parse_length_or_percentage()?;
            Ok(Value::Length(length))
        }

        "background-image" => {
            let url = parser.parse_url()?;
            Ok(Value::Url(url))
        }

        // ... more properties ...

        _ => {
            // Unknown property, try to parse generically
            Err(Error::Parse(format!("Cannot parse value for property: {}", property)))
        }
    }
}

fn tokens_to_string(tokens: &[Token]) -> String {
    // Convert tokens back to string for parsing
    // This is a bit inefficient but keeps parsing logic simple
    tokens.iter().map(|t| token_to_string(t)).collect::<Vec<_>>().join(" ")
}

fn token_to_string(token: &Token) -> String {
    match token {
        Token::Ident(s) => s.clone(),
        Token::Number(n) => n.to_string(),
        Token::Dimension(n, u) => format!("{}{}", n, u),
        Token::Percentage(n) => format!("{}%", n),
        Token::String(s) => format!("\"{}\"", s),
        Token::Hash(s) => format!("#{}", s),
        Token::Delim(c) => c.to_string(),
        Token::Whitespace => " ".to_string(),
        Token::Function(name, args) => {
            format!("{}({})", name, args.iter().map(token_to_string).collect::<Vec<_>>().join(" "))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_var_resolution() {
        let mut style = ComputedStyle::default();
        style.set_custom_property(
            "--color".to_string(),
            CustomPropertyValue::Valid(vec![Token::Ident("blue".to_string())]),
        );

        let components = vec![
            ValueComponent::Var {
                name: "--color".to_string(),
                fallback: None,
            },
        ];

        let mut resolver = VarResolver::new(&style);
        let tokens = resolver.resolve(&components).unwrap();

        assert_eq!(tokens, vec![Token::Ident("blue".to_string())]);
    }

    #[test]
    fn test_fallback_value() {
        let style = ComputedStyle::default();
        // --color not defined

        let components = vec![
            ValueComponent::Var {
                name: "--color".to_string(),
                fallback: Some(vec![
                    ValueComponent::Token(Token::Ident("red".to_string())),
                ]),
            },
        ];

        let mut resolver = VarResolver::new(&style);
        let tokens = resolver.resolve(&components).unwrap();

        assert_eq!(tokens, vec![Token::Ident("red".to_string())]);
    }

    #[test]
    fn test_cycle_detection() {
        let mut style = ComputedStyle::default();

        // --a: var(--b)
        style.set_custom_property(
            "--a".to_string(),
            CustomPropertyValue::Valid(vec![
                Token::Function(
                    "var".to_string(),
                    vec![Token::Ident("--b".to_string())],
                ),
            ]),
        );

        // --b: var(--a)
        style.set_custom_property(
            "--b".to_string(),
            CustomPropertyValue::Valid(vec![
                Token::Function(
                    "var".to_string(),
                    vec![Token::Ident("--a".to_string())],
                ),
            ]),
        );

        let components = vec![
            ValueComponent::Var {
                name: "--a".to_string(),
                fallback: None,
            },
        ];

        let mut resolver = VarResolver::new(&style);
        let result = resolver.resolve(&components);

        assert!(result.is_err()); // Cycle!
    }

    #[test]
    fn test_nested_fallback() {
        let style = ComputedStyle::default();

        // var(--a, var(--b, red))
        let components = vec![
            ValueComponent::Var {
                name: "--a".to_string(),
                fallback: Some(vec![
                    ValueComponent::Var {
                        name: "--b".to_string(),
                        fallback: Some(vec![
                            ValueComponent::Token(Token::Ident("red".to_string())),
                        ]),
                    },
                ]),
            },
        ];

        let mut resolver = VarResolver::new(&style);
        let tokens = resolver.resolve(&components).unwrap();

        assert_eq!(tokens, vec![Token::Ident("red".to_string())]);
    }
}
```

### Step 5: Integrate with Computed Values (Day 3)

**File: `src/style/computed_values.rs`** (modifications)

```rust
impl ComputedStyle {
    /// Computes a property value, resolving var() if present
    pub fn compute_property(
        &mut self,
        property: &str,
        value: &ValueWithVars,
    ) -> Result<()> {
        match value {
            ValueWithVars::Simple(v) => {
                // No variables, use value directly
                self.set_property(property, v.clone());
                Ok(())
            }

            ValueWithVars::WithVars(components) => {
                // Resolve var() references
                match resolve_property_value(property, components, self) {
                    Ok(resolved_value) => {
                        self.set_property(property, resolved_value);
                        Ok(())
                    }

                    Err(_) => {
                        // Invalid value, use initial
                        let initial = Self::initial_value_for_property(property);
                        self.set_property(property, initial);
                        Ok(())
                    }
                }
            }
        }
    }

    /// Gets the initial value for a property
    fn initial_value_for_property(property: &str) -> Value {
        match property {
            "color" => Value::Color(Color::BLACK),
            "background-color" => Value::Color(Color::TRANSPARENT),
            "width" | "height" => Value::Keyword("auto".into()),
            "display" => Value::Keyword("inline".into()),
            // ... more properties ...
            _ => Value::Keyword("initial".into()),
        }
    }
}
```

### Step 6: Handle Inheritance (Day 3)

**File: `src/style/engine.rs`** (modifications)

```rust
impl StyleEngine {
    fn compute_styles_for_element(
        &self,
        element: &DomNode,
        parent_style: Option<&ComputedStyle>,
    ) -> Result<ComputedStyle> {
        let mut style = ComputedStyle::default();

        // 1. Inherit from parent
        if let Some(parent) = parent_style {
            style.inherit_from(parent);

            // Custom properties ALWAYS inherit
            style.inherit_custom_properties(parent);
        }

        // 2. Apply matching rules
        for rule in self.matching_rules(element) {
            for declaration in &rule.declarations {
                match declaration {
                    Declaration::Custom { name, value } => {
                        // Custom property declaration
                        style.set_custom_property(
                            name.clone(),
                            CustomPropertyValue::Valid(value.clone()),
                        );
                    }

                    Declaration::Regular { property, value } => {
                        // Regular property (may contain var())
                        style.compute_property(property, value)?;
                    }
                }
            }
        }

        // 3. Compute final values
        style.finalize()?;

        Ok(style)
    }
}
```

### Step 7: Tests (Day 4)

**File: `tests/css_variables_test.rs`**

```rust
//! Tests for CSS custom properties (variables)

use fastrender::*;

#[test]
fn test_simple_variable() {
    let html = r#"<div class="box">Content</div>"#;
    let css = r#"
        :root {
            --color: blue;
        }

        .box {
            color: var(--color);
        }
    "#;

    let rendered = render(html, css);
    let box_element = rendered.find(".box");
    assert_eq!(box_element.color, Color::BLUE);
}

#[test]
fn test_variable_inheritance() {
    let html = r#"
        <div class="parent">
            <div class="child">Content</div>
        </div>
    "#;
    let css = r#"
        .parent {
            --color: red;
        }

        .child {
            color: var(--color);  /* Inherits from parent */
        }
    "#;

    let rendered = render(html, css);
    let child = rendered.find(".child");
    assert_eq!(child.color, Color::RED);
}

#[test]
fn test_variable_override() {
    let html = r#"
        <div class="parent">
            <div class="child">Content</div>
        </div>
    "#;
    let css = r#"
        .parent {
            --color: blue;
        }

        .child {
            --color: red;  /* Override */
            color: var(--color);
        }
    "#;

    let rendered = render(html, css);
    let child = rendered.find(".child");
    assert_eq!(child.color, Color::RED);  // Not blue!
}

#[test]
fn test_fallback_value() {
    let html = r#"<div class="box">Content</div>"#;
    let css = r#"
        .box {
            color: var(--undefined, green);
        }
    "#;

    let rendered = render(html, css);
    let box_element = rendered.find(".box");
    assert_eq!(box_element.color, Color::GREEN);
}

#[test]
fn test_nested_fallback() {
    let html = r#"<div class="box">Content</div>"#;
    let css = r#"
        .box {
            color: var(--a, var(--b, var(--c, purple)));
        }
    "#;

    let rendered = render(html, css);
    let box_element = rendered.find(".box");
    assert_eq!(box_element.color, Color::from_name("purple"));
}

#[test]
fn test_guaranteed_invalid() {
    let html = r#"<div class="box">Content</div>"#;
    let css = r#"
        .box {
            --not-a-color: 20px;
            color: blue;
            color: var(--not-a-color);  /* Invalid, should become initial (black) */
        }
    "#;

    let rendered = render(html, css);
    let box_element = rendered.find(".box");
    // color is invalid, should be initial value (black), NOT blue
    assert_eq!(box_element.color, Color::BLACK);
}

#[test]
fn test_cycle_detection() {
    let html = r#"<div class="box">Content</div>"#;
    let css = r#"
        .box {
            --a: var(--b);
            --b: var(--a);
            color: var(--a);  /* Cycle! Should be invalid */
        }
    "#;

    let rendered = render(html, css);
    let box_element = rendered.find(".box");
    // Cycle detected, should be initial value
    assert_eq!(box_element.color, Color::BLACK);
}

#[test]
fn test_self_reference() {
    let html = r#"<div class="box">Content</div>"#;
    let css = r#"
        .box {
            --color: var(--color);  /* Self-reference */
            color: var(--color);
        }
    "#;

    let rendered = render(html, css);
    let box_element = rendered.find(".box");
    // Self-reference is a cycle
    assert_eq!(box_element.color, Color::BLACK);
}

#[test]
fn test_empty_value() {
    let html = r#"<div class="box">Content</div>"#;
    let css = r#"
        .box {
            --empty:;  /* Empty value */
            color: var(--empty, red);
        }
    "#;

    let rendered = render(html, css);
    let box_element = rendered.find(".box");
    // --empty IS defined (as empty), so fallback is NOT used
    // Empty value is invalid for color, so becomes initial (black)
    assert_eq!(box_element.color, Color::BLACK);
}

#[test]
fn test_complex_value() {
    let html = r#"<div class="box">Content</div>"#;
    let css = r#"
        :root {
            --border-style: 2px solid red;
        }

        .box {
            border: var(--border-style);
        }
    "#;

    let rendered = render(html, css);
    let box_element = rendered.find(".box");
    assert_eq!(box_element.border_width, 2.0);
    assert_eq!(box_element.border_color, Color::RED);
    assert_eq!(box_element.border_style, BorderStyle::Solid);
}

#[test]
fn test_in_calc() {
    let html = r#"<div class="box">Content</div>"#;
    let css = r#"
        :root {
            --spacing: 16px;
        }

        .box {
            padding: calc(var(--spacing) * 2);  /* 32px */
        }
    "#;

    let rendered = render(html, css);
    let box_element = rendered.find(".box");
    assert_eq!(box_element.padding_top, 32.0);
}

#[test]
fn test_theming() {
    let html = r#"
        <div class="theme-light">
            <div class="card">Light theme</div>
        </div>
        <div class="theme-dark">
            <div class="card">Dark theme</div>
        </div>
    "#;
    let css = r#"
        .theme-light {
            --bg: white;
            --text: black;
        }

        .theme-dark {
            --bg: black;
            --text: white;
        }

        .card {
            background: var(--bg);
            color: var(--text);
        }
    "#;

    let rendered = render(html, css);

    let light_card = rendered.find_in(".theme-light", ".card");
    assert_eq!(light_card.background_color, Color::WHITE);
    assert_eq!(light_card.color, Color::BLACK);

    let dark_card = rendered.find_in(".theme-dark", ".card");
    assert_eq!(dark_card.background_color, Color::BLACK);
    assert_eq!(dark_card.color, Color::WHITE);
}
```

## Acceptance Criteria

- [ ] Custom properties can be declared (--name: value)
- [ ] Custom properties inherit correctly
- [ ] var() function resolves variables
- [ ] Fallback values work (var(--x, fallback))
- [ ] Nested fallbacks work
- [ ] Undefined variables use fallback or initial
- [ ] Empty values don't trigger fallback
- [ ] Invalid values become guaranteed-invalid
- [ ] Cycles are detected and result in invalid
- [ ] Self-references are detected as cycles
- [ ] Variables work in calc() expressions
- [ ] All tests pass: `cargo test css_variables`
- [ ] Real-world themes work correctly
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Parsing Variable Values Too Early

**Wrong:**
```rust
// Parse custom property value immediately
fn parse_custom_property(&mut self, name: String) -> CustomProperty {
    let value = self.parse_color()?;  // Wrong! May not be a color
    CustomProperty { name, value }
}
```

**Right:**
```rust
// Store tokens, parse later when used
fn parse_custom_property(&mut self, name: String) -> CustomProperty {
    let tokens = self.parse_value_tokens()?;  // Just tokens
    CustomProperty { name, tokens }
}
```

**Why:** Custom properties can contain ANY value. Only validate when used.

### Pitfall 2: Not Handling Guaranteed-Invalid Correctly

**Wrong:**
```rust
// Invalid value, keep previous value
if invalid_for_property {
    // Keep old value
}
```

**Right:**
```rust
// Invalid value, use initial value (not previous!)
if invalid_for_property {
    value = initial_value_for_property(property);
}
```

### Pitfall 3: Using Fallback for Empty Values

**Wrong:**
```rust
// --empty:;
// color: var(--empty, red);

if value.is_empty() {
    use_fallback();  // Wrong!
}
```

**Right:**
```rust
// Fallback only if undefined, not if empty
if value.is_undefined() {  // Not is_empty()!
    use_fallback();
}
```

### Pitfall 4: Not Detecting Cycles

**Wrong:**
```rust
// No cycle detection
fn resolve_var(&self, name: &str) -> Value {
    let value = lookup(name);
    resolve(value)  // May recurse infinitely!
}
```

**Right:**
```rust
// Track currently resolving variables
fn resolve_var(&mut self, name: &str) -> Result<Value> {
    if self.resolving.contains(name) {
        return Err(Error::Cycle);
    }
    self.resolving.insert(name);
    // ... resolve ...
    self.resolving.remove(name);
}
```

## Performance Considerations

- **Cache resolved values:** Don't re-resolve on every access
- **Lazy resolution:** Only resolve variables that are actually used
- **Short-circuit cycles:** Detect cycles early to avoid deep recursion

## Next Steps

After CSS variables:
- Test with real-world CSS frameworks
- Optimize variable lookup (currently O(n) up the tree)
- Add CSS.registerProperty() for typed custom properties (future)

## References

- **CSS Custom Properties for Cascading Variables Module Level 1:**
  - https://www.w3.org/TR/css-variables-1/
- **MDN CSS Custom Properties:**
  - https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_custom_properties
- **Houdini Properties and Values API:**
  - https://drafts.css-houdini.org/css-properties-values-api/

---

**Last Updated:** 2025-01-19
**Status:** Ready for Implementation
