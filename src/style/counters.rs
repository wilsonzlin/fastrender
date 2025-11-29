//! CSS Counter System
//!
//! Implements CSS counters for automatic numbering as specified in:
//! - CSS Lists Module Level 3: https://www.w3.org/TR/css-lists-3/#auto-numbering
//! - CSS 2.1 Section 12.4: https://www.w3.org/TR/CSS21/generate.html#counters
//!
//! # Overview
//!
//! CSS counters provide automatic numbering functionality:
//!
//! ```css
//! /* Initialize counter */
//! body { counter-reset: section; }
//!
//! /* Increment counter */
//! h2 { counter-increment: section; }
//!
//! /* Display counter */
//! h2::before { content: counter(section) ". "; }
//! ```
//!
//! # Counter Scoping
//!
//! Counters are scoped to elements and inherit down the tree.
//! A `counter-reset` creates a new counter instance in the current scope.
//! A `counter-increment` affects the nearest counter instance in scope.
//!
//! # Example
//!
//! ```
//! use fastrender::style::counters::{CounterSystem, CounterStyle};
//!
//! let mut system = CounterSystem::new();
//!
//! // Reset counter
//! system.reset_counter("section", 0);
//!
//! // Increment counter
//! system.increment_counter("section", 1);
//! assert_eq!(system.get_counter("section"), 1);
//!
//! // Format counter
//! let formatted = CounterStyle::Decimal.format(1);
//! assert_eq!(formatted, "1");
//! ```

use std::collections::HashMap;
use std::fmt;

/// CSS counter display style
///
/// Defines how counter values are formatted when displayed.
///
/// CSS Lists Module Level 3 defines many counter styles. We implement
/// the most commonly used ones from CSS 2.1 and CSS Lists Level 3.
///
/// # Examples
///
/// ```
/// use fastrender::style::counters::CounterStyle;
///
/// assert_eq!(CounterStyle::Decimal.format(42), "42");
/// assert_eq!(CounterStyle::LowerAlpha.format(1), "a");
/// assert_eq!(CounterStyle::UpperRoman.format(4), "IV");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum CounterStyle {
    /// Decimal numbers: 1, 2, 3, ...
    #[default]
    Decimal,

    /// Decimal with leading zeros: 01, 02, 03, ... (for values < 10)
    DecimalLeadingZero,

    /// Lowercase ASCII letters: a, b, c, ..., z, aa, ab, ...
    LowerAlpha,

    /// Uppercase ASCII letters: A, B, C, ..., Z, AA, AB, ...
    UpperAlpha,

    /// Lowercase Roman numerals: i, ii, iii, iv, v, ...
    LowerRoman,

    /// Uppercase Roman numerals: I, II, III, IV, V, ...
    UpperRoman,

    /// Lowercase Greek letters: α, β, γ, δ, ...
    LowerGreek,

    /// No marker (for list-style-type: none)
    None,

    /// Disc marker (•) for unordered lists
    Disc,

    /// Circle marker (◦) for unordered lists
    Circle,

    /// Square marker (▪) for unordered lists
    Square,
}

impl CounterStyle {
    /// Formats a counter value according to this style
    ///
    /// # Arguments
    ///
    /// * `value` - The counter value to format
    ///
    /// # Returns
    ///
    /// A string representation of the counter value
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterStyle;
    ///
    /// assert_eq!(CounterStyle::Decimal.format(1), "1");
    /// assert_eq!(CounterStyle::DecimalLeadingZero.format(5), "05");
    /// assert_eq!(CounterStyle::LowerAlpha.format(3), "c");
    /// assert_eq!(CounterStyle::UpperRoman.format(9), "IX");
    /// ```
    pub fn format(self, value: i32) -> String {
        match self {
            Self::Decimal => value.to_string(),
            Self::DecimalLeadingZero => Self::format_decimal_leading_zero(value),
            Self::LowerAlpha => Self::format_alphabetic(value, false),
            Self::UpperAlpha => Self::format_alphabetic(value, true),
            Self::LowerRoman => Self::format_roman(value, false),
            Self::UpperRoman => Self::format_roman(value, true),
            Self::LowerGreek => Self::format_greek(value),
            Self::None => String::new(),
            Self::Disc => "•".to_string(),
            Self::Circle => "◦".to_string(),
            Self::Square => "▪".to_string(),
        }
    }

    /// Formats with leading zero for single digits
    fn format_decimal_leading_zero(value: i32) -> String {
        if value >= 0 && value < 10 {
            format!("0{}", value)
        } else if value < 0 && value > -10 {
            format!("-0{}", value.abs())
        } else {
            value.to_string()
        }
    }

    /// Formats as alphabetic (a-z, aa-az, ...)
    fn format_alphabetic(mut value: i32, uppercase: bool) -> String {
        if value <= 0 {
            return value.to_string();
        }

        let base = if uppercase { b'A' } else { b'a' };
        let mut result = String::new();

        while value > 0 {
            value -= 1;
            let digit = (value % 26) as u8;
            result.insert(0, (base + digit) as char);
            value /= 26;
        }

        result
    }

    /// Formats as Roman numerals
    fn format_roman(value: i32, uppercase: bool) -> String {
        if value <= 0 || value >= 4000 {
            return value.to_string();
        }

        const VALUES: [i32; 13] = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
        const SYMBOLS_LOWER: [&str; 13] = [
            "m", "cm", "d", "cd", "c", "xc", "l", "xl", "x", "ix", "v", "iv", "i",
        ];
        const SYMBOLS_UPPER: [&str; 13] = [
            "M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I",
        ];

        let symbols = if uppercase {
            &SYMBOLS_UPPER
        } else {
            &SYMBOLS_LOWER
        };

        let mut result = String::new();
        let mut remaining = value;

        for (i, &val) in VALUES.iter().enumerate() {
            while remaining >= val {
                result.push_str(symbols[i]);
                remaining -= val;
            }
        }

        result
    }

    /// Formats as Greek letters (α, β, γ, ...)
    fn format_greek(value: i32) -> String {
        if value <= 0 || value > 24 {
            return value.to_string();
        }

        // Greek lowercase letters: α=1, β=2, ..., ω=24
        const GREEK_LETTERS: [char; 24] = [
            'α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ', 'ι', 'κ', 'λ', 'μ', 'ν', 'ξ', 'ο', 'π', 'ρ',
            'σ', 'τ', 'υ', 'φ', 'χ', 'ψ', 'ω',
        ];

        GREEK_LETTERS[(value - 1) as usize].to_string()
    }

    /// Parses a counter style from a CSS keyword
    ///
    /// # Arguments
    ///
    /// * `s` - The CSS keyword to parse
    ///
    /// # Returns
    ///
    /// The parsed counter style, or None if unrecognized
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterStyle;
    ///
    /// assert_eq!(CounterStyle::parse("decimal"), Some(CounterStyle::Decimal));
    /// assert_eq!(CounterStyle::parse("lower-roman"), Some(CounterStyle::LowerRoman));
    /// assert_eq!(CounterStyle::parse("invalid"), None);
    /// ```
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "decimal" => Some(Self::Decimal),
            "decimal-leading-zero" => Some(Self::DecimalLeadingZero),
            "lower-alpha" | "lower-latin" => Some(Self::LowerAlpha),
            "upper-alpha" | "upper-latin" => Some(Self::UpperAlpha),
            "lower-roman" => Some(Self::LowerRoman),
            "upper-roman" => Some(Self::UpperRoman),
            "lower-greek" => Some(Self::LowerGreek),
            "none" => Some(Self::None),
            "disc" => Some(Self::Disc),
            "circle" => Some(Self::Circle),
            "square" => Some(Self::Square),
            _ => None,
        }
    }
}

impl fmt::Display for CounterStyle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Decimal => "decimal",
            Self::DecimalLeadingZero => "decimal-leading-zero",
            Self::LowerAlpha => "lower-alpha",
            Self::UpperAlpha => "upper-alpha",
            Self::LowerRoman => "lower-roman",
            Self::UpperRoman => "upper-roman",
            Self::LowerGreek => "lower-greek",
            Self::None => "none",
            Self::Disc => "disc",
            Self::Circle => "circle",
            Self::Square => "square",
        };
        write!(f, "{}", s)
    }
}

/// A single counter scope
///
/// Represents the counter values at a particular level of the element tree.
/// Each scope maintains its own set of counter instances.
#[derive(Debug, Clone)]
struct CounterScope {
    /// Counter name → current value
    counters: HashMap<String, i32>,
}

impl CounterScope {
    /// Creates a new empty counter scope
    fn new() -> Self {
        Self {
            counters: HashMap::new(),
        }
    }

    /// Resets a counter to the specified value in this scope
    fn reset(&mut self, name: &str, value: i32) {
        self.counters.insert(name.to_string(), value);
    }

    /// Gets a counter value if it exists in this scope
    fn get(&self, name: &str) -> Option<i32> {
        self.counters.get(name).copied()
    }

    /// Gets a mutable reference to a counter value if it exists
    fn get_mut(&mut self, name: &str) -> Option<&mut i32> {
        self.counters.get_mut(name)
    }

    /// Checks if this scope has a counter with the given name
    fn has(&self, name: &str) -> bool {
        self.counters.contains_key(name)
    }
}

impl Default for CounterScope {
    fn default() -> Self {
        Self::new()
    }
}

/// CSS counter system
///
/// Tracks counter values during box generation and layout.
/// Counters are scoped to elements and inherit down the tree.
///
/// # Scope Model
///
/// Each call to `push_scope()` creates a new scope level.
/// Counter lookups search from innermost to outermost scope.
/// Counter resets create new instances in the current scope.
/// Counter increments affect the nearest matching counter.
///
/// # Example
///
/// ```
/// use fastrender::style::counters::CounterSystem;
///
/// let mut system = CounterSystem::new();
///
/// // Create a counter at the root scope
/// system.reset_counter("chapter", 0);
/// system.increment_counter("chapter", 1);
/// assert_eq!(system.get_counter("chapter"), 1);
///
/// // Enter a nested scope
/// system.push_scope();
/// system.reset_counter("section", 0);
/// system.increment_counter("section", 1);
///
/// // Both counters are accessible
/// assert_eq!(system.get_counter("chapter"), 1);
/// assert_eq!(system.get_counter("section"), 1);
///
/// // Exit the nested scope
/// system.pop_scope();
///
/// // Section counter is no longer accessible
/// assert_eq!(system.get_counter("section"), 0); // Default value
/// assert_eq!(system.get_counter("chapter"), 1);
/// ```
#[derive(Debug, Clone)]
pub struct CounterSystem {
    /// Stack of counter scopes (one per nesting level)
    scopes: Vec<CounterScope>,
}

impl CounterSystem {
    /// Creates a new counter system with a root scope
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSystem;
    ///
    /// let system = CounterSystem::new();
    /// assert_eq!(system.get_counter("any"), 0); // Default value
    /// ```
    pub fn new() -> Self {
        Self {
            scopes: vec![CounterScope::new()],
        }
    }

    /// Pushes a new scope (when entering an element)
    ///
    /// Should be called when entering an element during tree traversal.
    /// The new scope inherits counter lookups from parent scopes.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSystem;
    ///
    /// let mut system = CounterSystem::new();
    /// system.reset_counter("section", 1);
    ///
    /// system.push_scope();
    /// // Counter is still accessible from parent scope
    /// assert_eq!(system.get_counter("section"), 1);
    /// ```
    pub fn push_scope(&mut self) {
        self.scopes.push(CounterScope::new());
    }

    /// Pops the current scope (when leaving an element)
    ///
    /// Should be called when leaving an element during tree traversal.
    /// Counter instances created in this scope are discarded.
    ///
    /// # Panics
    ///
    /// Does not panic. If only the root scope remains, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSystem;
    ///
    /// let mut system = CounterSystem::new();
    /// system.push_scope();
    /// system.reset_counter("local", 1);
    /// system.pop_scope();
    ///
    /// // Local counter is gone
    /// assert_eq!(system.get_counter("local"), 0);
    /// ```
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Returns the current scope depth
    ///
    /// The root scope has depth 1.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSystem;
    ///
    /// let mut system = CounterSystem::new();
    /// assert_eq!(system.scope_depth(), 1);
    ///
    /// system.push_scope();
    /// assert_eq!(system.scope_depth(), 2);
    /// ```
    pub fn scope_depth(&self) -> usize {
        self.scopes.len()
    }

    /// Resets a counter to a value (counter-reset property)
    ///
    /// Creates a new counter instance in the current scope.
    /// If a counter with this name already exists in the current scope,
    /// it is reset to the new value.
    ///
    /// # Arguments
    ///
    /// * `name` - Counter name
    /// * `value` - Initial value (default in CSS is 0)
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSystem;
    ///
    /// let mut system = CounterSystem::new();
    /// system.reset_counter("section", 0);
    /// assert_eq!(system.get_counter("section"), 0);
    ///
    /// system.reset_counter("section", 5);
    /// assert_eq!(system.get_counter("section"), 5);
    /// ```
    pub fn reset_counter(&mut self, name: &str, value: i32) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.reset(name, value);
        }
    }

    /// Increments a counter (counter-increment property)
    ///
    /// Increments the nearest counter instance with the given name.
    /// If no counter exists, creates one in the current scope with
    /// the increment value.
    ///
    /// # Arguments
    ///
    /// * `name` - Counter name
    /// * `delta` - Amount to increment (default in CSS is 1)
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSystem;
    ///
    /// let mut system = CounterSystem::new();
    /// system.reset_counter("section", 0);
    /// system.increment_counter("section", 1);
    /// assert_eq!(system.get_counter("section"), 1);
    ///
    /// system.increment_counter("section", 2);
    /// assert_eq!(system.get_counter("section"), 3);
    ///
    /// // Incrementing a non-existent counter creates it
    /// system.increment_counter("new", 5);
    /// assert_eq!(system.get_counter("new"), 5);
    /// ```
    pub fn increment_counter(&mut self, name: &str, delta: i32) {
        // Find counter in current scope or ancestor scopes
        for scope in self.scopes.iter_mut().rev() {
            if let Some(value) = scope.get_mut(name) {
                *value += delta;
                return;
            }
        }

        // Counter doesn't exist, create it with increment value
        if let Some(scope) = self.scopes.last_mut() {
            scope.reset(name, delta);
        }
    }

    /// Sets a counter to a specific value (counter-set property from CSS Lists 3)
    ///
    /// Unlike `reset_counter`, this searches for an existing counter first.
    /// If found, it sets that counter to the value.
    /// If not found, it creates a new counter with the value.
    ///
    /// # Arguments
    ///
    /// * `name` - Counter name
    /// * `value` - Value to set
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSystem;
    ///
    /// let mut system = CounterSystem::new();
    /// system.reset_counter("section", 0);
    /// system.increment_counter("section", 5);
    ///
    /// system.push_scope();
    /// // counter-set affects the existing counter in parent scope
    /// system.set_counter("section", 10);
    /// system.pop_scope();
    ///
    /// assert_eq!(system.get_counter("section"), 10);
    /// ```
    pub fn set_counter(&mut self, name: &str, value: i32) {
        // Find counter in current scope or ancestor scopes
        for scope in self.scopes.iter_mut().rev() {
            if let Some(counter_value) = scope.get_mut(name) {
                *counter_value = value;
                return;
            }
        }

        // Counter doesn't exist, create it in current scope
        if let Some(scope) = self.scopes.last_mut() {
            scope.reset(name, value);
        }
    }

    /// Gets the current value of a counter
    ///
    /// Searches from the innermost scope to the outermost for a counter
    /// with the given name.
    ///
    /// # Arguments
    ///
    /// * `name` - Counter name
    ///
    /// # Returns
    ///
    /// Current value, or 0 if counter doesn't exist
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSystem;
    ///
    /// let mut system = CounterSystem::new();
    /// assert_eq!(system.get_counter("undefined"), 0);
    ///
    /// system.reset_counter("defined", 42);
    /// assert_eq!(system.get_counter("defined"), 42);
    /// ```
    pub fn get_counter(&self, name: &str) -> i32 {
        // Search scopes from innermost to outermost
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return value;
            }
        }

        0 // Default value per CSS specification
    }

    /// Gets all values of a counter in nested scopes
    ///
    /// Used for the `counters()` function which displays hierarchical
    /// counter values.
    ///
    /// # Arguments
    ///
    /// * `name` - Counter name
    ///
    /// # Returns
    ///
    /// Vector of values from outermost to innermost scope
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSystem;
    ///
    /// let mut system = CounterSystem::new();
    ///
    /// // Level 1
    /// system.reset_counter("section", 0);
    /// system.increment_counter("section", 1);
    ///
    /// // Level 2
    /// system.push_scope();
    /// system.reset_counter("section", 0);
    /// system.increment_counter("section", 2);
    ///
    /// // Level 3
    /// system.push_scope();
    /// system.reset_counter("section", 0);
    /// system.increment_counter("section", 3);
    ///
    /// // counters("section", ".") would produce "1.2.3"
    /// assert_eq!(system.get_counters("section"), vec![1, 2, 3]);
    /// ```
    pub fn get_counters(&self, name: &str) -> Vec<i32> {
        let mut values = Vec::new();

        for scope in &self.scopes {
            if let Some(value) = scope.get(name) {
                values.push(value);
            }
        }

        values
    }

    /// Formats counter value for content property
    ///
    /// This is a convenience method that combines getting and formatting
    /// a counter value.
    ///
    /// # Arguments
    ///
    /// * `name` - Counter name
    /// * `style` - Counter display style
    ///
    /// # Returns
    ///
    /// Formatted counter value string
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::{CounterSystem, CounterStyle};
    ///
    /// let mut system = CounterSystem::new();
    /// system.reset_counter("section", 0);
    /// system.increment_counter("section", 4);
    ///
    /// assert_eq!(system.format_counter("section", CounterStyle::Decimal), "4");
    /// assert_eq!(system.format_counter("section", CounterStyle::UpperRoman), "IV");
    /// assert_eq!(system.format_counter("section", CounterStyle::LowerAlpha), "d");
    /// ```
    pub fn format_counter(&self, name: &str, style: CounterStyle) -> String {
        let value = self.get_counter(name);
        style.format(value)
    }

    /// Formats nested counters for content property
    ///
    /// This is a convenience method that combines getting and formatting
    /// nested counter values with a separator.
    ///
    /// # Arguments
    ///
    /// * `name` - Counter name
    /// * `separator` - String to join counter values
    /// * `style` - Counter display style
    ///
    /// # Returns
    ///
    /// Formatted counter values joined by separator
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::{CounterSystem, CounterStyle};
    ///
    /// let mut system = CounterSystem::new();
    /// system.reset_counter("section", 0);
    /// system.increment_counter("section", 1);
    ///
    /// system.push_scope();
    /// system.reset_counter("section", 0);
    /// system.increment_counter("section", 2);
    ///
    /// system.push_scope();
    /// system.reset_counter("section", 0);
    /// system.increment_counter("section", 3);
    ///
    /// assert_eq!(
    ///     system.format_counters("section", ".", CounterStyle::Decimal),
    ///     "1.2.3"
    /// );
    /// ```
    pub fn format_counters(&self, name: &str, separator: &str, style: CounterStyle) -> String {
        let values = self.get_counters(name);
        values
            .iter()
            .map(|&v| style.format(v))
            .collect::<Vec<_>>()
            .join(separator)
    }

    /// Checks if a counter exists in any scope
    ///
    /// # Arguments
    ///
    /// * `name` - Counter name
    ///
    /// # Returns
    ///
    /// true if the counter exists in any scope
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSystem;
    ///
    /// let mut system = CounterSystem::new();
    /// assert!(!system.has_counter("section"));
    ///
    /// system.reset_counter("section", 0);
    /// assert!(system.has_counter("section"));
    /// ```
    pub fn has_counter(&self, name: &str) -> bool {
        self.scopes.iter().any(|scope| scope.has(name))
    }

    /// Clears all counters and resets to a single root scope
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSystem;
    ///
    /// let mut system = CounterSystem::new();
    /// system.reset_counter("section", 5);
    /// system.push_scope();
    ///
    /// system.clear();
    ///
    /// assert_eq!(system.scope_depth(), 1);
    /// assert_eq!(system.get_counter("section"), 0);
    /// ```
    pub fn clear(&mut self) {
        self.scopes.clear();
        self.scopes.push(CounterScope::new());
    }
}

impl Default for CounterSystem {
    fn default() -> Self {
        Self::new()
    }
}

/// A parsed counter-reset property value
///
/// CSS syntax: counter-reset: <counter-name> <integer>? [<counter-name> <integer>?]*
///
/// # Examples
///
/// ```
/// use fastrender::style::counters::CounterReset;
///
/// let reset = CounterReset::parse("section 0 chapter 5").unwrap();
/// assert_eq!(reset.counters.len(), 2);
/// assert_eq!(reset.counters[0], ("section".to_string(), 0));
/// assert_eq!(reset.counters[1], ("chapter".to_string(), 5));
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct CounterReset {
    /// List of (counter name, initial value) pairs
    pub counters: Vec<(String, i32)>,
}

impl CounterReset {
    /// Creates a counter-reset with no counters
    pub fn none() -> Self {
        Self {
            counters: Vec::new(),
        }
    }

    /// Parses a counter-reset property value
    ///
    /// # Arguments
    ///
    /// * `value` - The CSS property value to parse
    ///
    /// # Returns
    ///
    /// A CounterReset, or None if parsing fails
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterReset;
    ///
    /// assert!(CounterReset::parse("none").is_some());
    /// assert!(CounterReset::parse("section").is_some());
    /// assert!(CounterReset::parse("section 5").is_some());
    /// assert!(CounterReset::parse("section 5 chapter").is_some());
    /// ```
    pub fn parse(value: &str) -> Option<Self> {
        let value = value.trim();

        if value.eq_ignore_ascii_case("none") {
            return Some(Self::none());
        }

        let mut counters = Vec::new();
        let mut tokens = value.split_whitespace().peekable();

        while let Some(name) = tokens.next() {
            // Validate counter name (must start with letter or underscore)
            if !is_valid_counter_name(name) {
                return None;
            }

            // Check for optional integer value
            let initial_value = if let Some(&next) = tokens.peek() {
                if let Ok(val) = next.parse::<i32>() {
                    tokens.next();
                    val
                } else {
                    0 // Default value per CSS spec
                }
            } else {
                0
            };

            counters.push((name.to_string(), initial_value));
        }

        if counters.is_empty() {
            return None;
        }

        Some(Self { counters })
    }

    /// Applies this counter-reset to a counter system
    pub fn apply(&self, system: &mut CounterSystem) {
        for (name, value) in &self.counters {
            system.reset_counter(name, *value);
        }
    }
}

impl Default for CounterReset {
    fn default() -> Self {
        Self::none()
    }
}

/// A parsed counter-increment property value
///
/// CSS syntax: counter-increment: <counter-name> <integer>? [<counter-name> <integer>?]*
///
/// # Examples
///
/// ```
/// use fastrender::style::counters::CounterIncrement;
///
/// let incr = CounterIncrement::parse("section 2 chapter").unwrap();
/// assert_eq!(incr.counters.len(), 2);
/// assert_eq!(incr.counters[0], ("section".to_string(), 2));
/// assert_eq!(incr.counters[1], ("chapter".to_string(), 1));
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct CounterIncrement {
    /// List of (counter name, increment value) pairs
    pub counters: Vec<(String, i32)>,
}

impl CounterIncrement {
    /// Creates a counter-increment with no counters
    pub fn none() -> Self {
        Self {
            counters: Vec::new(),
        }
    }

    /// Parses a counter-increment property value
    ///
    /// # Arguments
    ///
    /// * `value` - The CSS property value to parse
    ///
    /// # Returns
    ///
    /// A CounterIncrement, or None if parsing fails
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterIncrement;
    ///
    /// assert!(CounterIncrement::parse("none").is_some());
    /// assert!(CounterIncrement::parse("section").is_some());
    /// assert!(CounterIncrement::parse("section 2").is_some());
    /// assert!(CounterIncrement::parse("section -1").is_some());
    /// ```
    pub fn parse(value: &str) -> Option<Self> {
        let value = value.trim();

        if value.eq_ignore_ascii_case("none") {
            return Some(Self::none());
        }

        let mut counters = Vec::new();
        let mut tokens = value.split_whitespace().peekable();

        while let Some(name) = tokens.next() {
            // Validate counter name
            if !is_valid_counter_name(name) {
                return None;
            }

            // Check for optional integer value
            let increment = if let Some(&next) = tokens.peek() {
                if let Ok(val) = next.parse::<i32>() {
                    tokens.next();
                    val
                } else {
                    1 // Default increment per CSS spec
                }
            } else {
                1
            };

            counters.push((name.to_string(), increment));
        }

        if counters.is_empty() {
            return None;
        }

        Some(Self { counters })
    }

    /// Applies this counter-increment to a counter system
    pub fn apply(&self, system: &mut CounterSystem) {
        for (name, delta) in &self.counters {
            system.increment_counter(name, *delta);
        }
    }
}

impl Default for CounterIncrement {
    fn default() -> Self {
        Self::none()
    }
}

/// A parsed counter-set property value (CSS Lists Level 3)
///
/// CSS syntax: counter-set: <counter-name> <integer>? [<counter-name> <integer>?]*
///
/// # Examples
///
/// ```
/// use fastrender::style::counters::CounterSet;
///
/// let set = CounterSet::parse("section 10").unwrap();
/// assert_eq!(set.counters.len(), 1);
/// assert_eq!(set.counters[0], ("section".to_string(), 10));
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct CounterSet {
    /// List of (counter name, value) pairs
    pub counters: Vec<(String, i32)>,
}

impl CounterSet {
    /// Creates a counter-set with no counters
    pub fn none() -> Self {
        Self {
            counters: Vec::new(),
        }
    }

    /// Parses a counter-set property value
    ///
    /// # Arguments
    ///
    /// * `value` - The CSS property value to parse
    ///
    /// # Returns
    ///
    /// A CounterSet, or None if parsing fails
    pub fn parse(value: &str) -> Option<Self> {
        let value = value.trim();

        if value.eq_ignore_ascii_case("none") {
            return Some(Self::none());
        }

        let mut counters = Vec::new();
        let mut tokens = value.split_whitespace().peekable();

        while let Some(name) = tokens.next() {
            // Validate counter name
            if !is_valid_counter_name(name) {
                return None;
            }

            // Check for optional integer value
            let set_value = if let Some(&next) = tokens.peek() {
                if let Ok(val) = next.parse::<i32>() {
                    tokens.next();
                    val
                } else {
                    0 // Default value per CSS spec
                }
            } else {
                0
            };

            counters.push((name.to_string(), set_value));
        }

        if counters.is_empty() {
            return None;
        }

        Some(Self { counters })
    }

    /// Applies this counter-set to a counter system
    pub fn apply(&self, system: &mut CounterSystem) {
        for (name, value) in &self.counters {
            system.set_counter(name, *value);
        }
    }
}

impl Default for CounterSet {
    fn default() -> Self {
        Self::none()
    }
}

/// Validates a counter name
///
/// Per CSS specification, counter names must be valid CSS identifiers:
/// - Start with a letter (a-z, A-Z) or underscore
/// - Followed by letters, digits, underscores, or hyphens
/// - Cannot be a CSS-wide keyword (inherit, initial, unset, revert)
fn is_valid_counter_name(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }

    // Check for CSS-wide keywords
    match name.to_lowercase().as_str() {
        "inherit" | "initial" | "unset" | "revert" | "none" => return false,
        _ => {}
    }

    let mut chars = name.chars();

    // First character must be letter, underscore, or hyphen followed by non-digit
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        Some('-') => {
            // After hyphen, must have letter, underscore, or hyphen
            match chars.next() {
                Some(c) if c.is_ascii_alphabetic() || c == '_' || c == '-' => {}
                _ => return false,
            }
        }
        _ => return false,
    }

    // Remaining characters: letters, digits, underscores, hyphens
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
}

#[cfg(test)]
mod tests {
    use super::*;

    // CounterStyle tests
    #[test]
    fn test_counter_style_decimal() {
        assert_eq!(CounterStyle::Decimal.format(0), "0");
        assert_eq!(CounterStyle::Decimal.format(1), "1");
        assert_eq!(CounterStyle::Decimal.format(42), "42");
        assert_eq!(CounterStyle::Decimal.format(-5), "-5");
    }

    #[test]
    fn test_counter_style_decimal_leading_zero() {
        assert_eq!(CounterStyle::DecimalLeadingZero.format(0), "00");
        assert_eq!(CounterStyle::DecimalLeadingZero.format(5), "05");
        assert_eq!(CounterStyle::DecimalLeadingZero.format(10), "10");
        assert_eq!(CounterStyle::DecimalLeadingZero.format(-3), "-03");
    }

    #[test]
    fn test_counter_style_lower_alpha() {
        assert_eq!(CounterStyle::LowerAlpha.format(1), "a");
        assert_eq!(CounterStyle::LowerAlpha.format(2), "b");
        assert_eq!(CounterStyle::LowerAlpha.format(26), "z");
        assert_eq!(CounterStyle::LowerAlpha.format(27), "aa");
        assert_eq!(CounterStyle::LowerAlpha.format(28), "ab");
        assert_eq!(CounterStyle::LowerAlpha.format(52), "az");
        assert_eq!(CounterStyle::LowerAlpha.format(53), "ba");
        assert_eq!(CounterStyle::LowerAlpha.format(0), "0"); // Out of range
        assert_eq!(CounterStyle::LowerAlpha.format(-1), "-1"); // Out of range
    }

    #[test]
    fn test_counter_style_upper_alpha() {
        assert_eq!(CounterStyle::UpperAlpha.format(1), "A");
        assert_eq!(CounterStyle::UpperAlpha.format(26), "Z");
        assert_eq!(CounterStyle::UpperAlpha.format(27), "AA");
    }

    #[test]
    fn test_counter_style_lower_roman() {
        assert_eq!(CounterStyle::LowerRoman.format(1), "i");
        assert_eq!(CounterStyle::LowerRoman.format(4), "iv");
        assert_eq!(CounterStyle::LowerRoman.format(5), "v");
        assert_eq!(CounterStyle::LowerRoman.format(9), "ix");
        assert_eq!(CounterStyle::LowerRoman.format(10), "x");
        assert_eq!(CounterStyle::LowerRoman.format(40), "xl");
        assert_eq!(CounterStyle::LowerRoman.format(50), "l");
        assert_eq!(CounterStyle::LowerRoman.format(90), "xc");
        assert_eq!(CounterStyle::LowerRoman.format(100), "c");
        assert_eq!(CounterStyle::LowerRoman.format(400), "cd");
        assert_eq!(CounterStyle::LowerRoman.format(500), "d");
        assert_eq!(CounterStyle::LowerRoman.format(900), "cm");
        assert_eq!(CounterStyle::LowerRoman.format(1000), "m");
        assert_eq!(CounterStyle::LowerRoman.format(1994), "mcmxciv");
        assert_eq!(CounterStyle::LowerRoman.format(2024), "mmxxiv");
        assert_eq!(CounterStyle::LowerRoman.format(0), "0"); // Out of range
        assert_eq!(CounterStyle::LowerRoman.format(4000), "4000"); // Out of range
    }

    #[test]
    fn test_counter_style_upper_roman() {
        assert_eq!(CounterStyle::UpperRoman.format(1), "I");
        assert_eq!(CounterStyle::UpperRoman.format(4), "IV");
        assert_eq!(CounterStyle::UpperRoman.format(1994), "MCMXCIV");
    }

    #[test]
    fn test_counter_style_lower_greek() {
        assert_eq!(CounterStyle::LowerGreek.format(1), "α");
        assert_eq!(CounterStyle::LowerGreek.format(2), "β");
        assert_eq!(CounterStyle::LowerGreek.format(3), "γ");
        assert_eq!(CounterStyle::LowerGreek.format(24), "ω");
        assert_eq!(CounterStyle::LowerGreek.format(0), "0"); // Out of range
        assert_eq!(CounterStyle::LowerGreek.format(25), "25"); // Out of range
    }

    #[test]
    fn test_counter_style_none_and_markers() {
        assert_eq!(CounterStyle::None.format(1), "");
        assert_eq!(CounterStyle::Disc.format(1), "•");
        assert_eq!(CounterStyle::Circle.format(1), "◦");
        assert_eq!(CounterStyle::Square.format(1), "▪");
    }

    #[test]
    fn test_counter_style_parse() {
        assert_eq!(CounterStyle::parse("decimal"), Some(CounterStyle::Decimal));
        assert_eq!(
            CounterStyle::parse("DECIMAL"),
            Some(CounterStyle::Decimal)
        );
        assert_eq!(
            CounterStyle::parse("lower-roman"),
            Some(CounterStyle::LowerRoman)
        );
        assert_eq!(
            CounterStyle::parse("lower-alpha"),
            Some(CounterStyle::LowerAlpha)
        );
        assert_eq!(
            CounterStyle::parse("lower-latin"),
            Some(CounterStyle::LowerAlpha)
        );
        assert_eq!(CounterStyle::parse("invalid"), None);
    }

    #[test]
    fn test_counter_style_display() {
        assert_eq!(format!("{}", CounterStyle::Decimal), "decimal");
        assert_eq!(format!("{}", CounterStyle::LowerRoman), "lower-roman");
    }

    // CounterSystem tests
    #[test]
    fn test_counter_system_new() {
        let system = CounterSystem::new();
        assert_eq!(system.scope_depth(), 1);
        assert_eq!(system.get_counter("any"), 0);
    }

    #[test]
    fn test_counter_reset() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 0);
        assert_eq!(system.get_counter("section"), 0);

        system.reset_counter("section", 5);
        assert_eq!(system.get_counter("section"), 5);
    }

    #[test]
    fn test_counter_increment() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 0);

        system.increment_counter("section", 1);
        assert_eq!(system.get_counter("section"), 1);

        system.increment_counter("section", 1);
        assert_eq!(system.get_counter("section"), 2);

        system.increment_counter("section", 5);
        assert_eq!(system.get_counter("section"), 7);

        system.increment_counter("section", -3);
        assert_eq!(system.get_counter("section"), 4);
    }

    #[test]
    fn test_increment_creates_counter() {
        let mut system = CounterSystem::new();

        // Incrementing non-existent counter creates it
        system.increment_counter("new", 3);
        assert_eq!(system.get_counter("new"), 3);
    }

    #[test]
    fn test_counter_set() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 0);
        system.increment_counter("section", 5);

        system.set_counter("section", 10);
        assert_eq!(system.get_counter("section"), 10);
    }

    #[test]
    fn test_scope_push_pop() {
        let mut system = CounterSystem::new();
        assert_eq!(system.scope_depth(), 1);

        system.push_scope();
        assert_eq!(system.scope_depth(), 2);

        system.push_scope();
        assert_eq!(system.scope_depth(), 3);

        system.pop_scope();
        assert_eq!(system.scope_depth(), 2);

        system.pop_scope();
        assert_eq!(system.scope_depth(), 1);

        // Cannot pop root scope
        system.pop_scope();
        assert_eq!(system.scope_depth(), 1);
    }

    #[test]
    fn test_scope_counter_inheritance() {
        let mut system = CounterSystem::new();
        system.reset_counter("parent", 10);

        system.push_scope();
        // Can access parent scope counter
        assert_eq!(system.get_counter("parent"), 10);

        // Increment affects parent scope counter
        system.increment_counter("parent", 5);
        assert_eq!(system.get_counter("parent"), 15);

        system.pop_scope();
        // Change persists
        assert_eq!(system.get_counter("parent"), 15);
    }

    #[test]
    fn test_scope_counter_shadowing() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 1);

        system.push_scope();
        // Create new counter in child scope (shadows parent)
        system.reset_counter("section", 10);
        assert_eq!(system.get_counter("section"), 10);

        system.increment_counter("section", 1);
        assert_eq!(system.get_counter("section"), 11);

        system.pop_scope();
        // Parent counter unchanged
        assert_eq!(system.get_counter("section"), 1);
    }

    #[test]
    fn test_get_counters() {
        let mut system = CounterSystem::new();

        // Level 1
        system.reset_counter("section", 0);
        system.increment_counter("section", 1);

        // Level 2
        system.push_scope();
        system.reset_counter("section", 0);
        system.increment_counter("section", 2);

        // Level 3
        system.push_scope();
        system.reset_counter("section", 0);
        system.increment_counter("section", 3);

        assert_eq!(system.get_counters("section"), vec![1, 2, 3]);
    }

    #[test]
    fn test_format_counter() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 0);
        system.increment_counter("section", 4);

        assert_eq!(
            system.format_counter("section", CounterStyle::Decimal),
            "4"
        );
        assert_eq!(
            system.format_counter("section", CounterStyle::UpperRoman),
            "IV"
        );
        assert_eq!(
            system.format_counter("section", CounterStyle::LowerAlpha),
            "d"
        );
    }

    #[test]
    fn test_format_counters() {
        let mut system = CounterSystem::new();

        system.reset_counter("section", 0);
        system.increment_counter("section", 1);

        system.push_scope();
        system.reset_counter("section", 0);
        system.increment_counter("section", 2);

        system.push_scope();
        system.reset_counter("section", 0);
        system.increment_counter("section", 3);

        assert_eq!(
            system.format_counters("section", ".", CounterStyle::Decimal),
            "1.2.3"
        );
        assert_eq!(
            system.format_counters("section", "-", CounterStyle::Decimal),
            "1-2-3"
        );
    }

    #[test]
    fn test_has_counter() {
        let mut system = CounterSystem::new();
        assert!(!system.has_counter("section"));

        system.reset_counter("section", 0);
        assert!(system.has_counter("section"));

        system.push_scope();
        // Still accessible from parent scope
        assert!(system.has_counter("section"));
    }

    #[test]
    fn test_clear() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 5);
        system.push_scope();
        system.push_scope();

        system.clear();

        assert_eq!(system.scope_depth(), 1);
        assert_eq!(system.get_counter("section"), 0);
    }

    // CounterReset tests
    #[test]
    fn test_counter_reset_parse_none() {
        let reset = CounterReset::parse("none").unwrap();
        assert!(reset.counters.is_empty());
    }

    #[test]
    fn test_counter_reset_parse_single() {
        let reset = CounterReset::parse("section").unwrap();
        assert_eq!(reset.counters, vec![("section".to_string(), 0)]);
    }

    #[test]
    fn test_counter_reset_parse_with_value() {
        let reset = CounterReset::parse("section 5").unwrap();
        assert_eq!(reset.counters, vec![("section".to_string(), 5)]);
    }

    #[test]
    fn test_counter_reset_parse_multiple() {
        let reset = CounterReset::parse("section 0 chapter 1").unwrap();
        assert_eq!(
            reset.counters,
            vec![
                ("section".to_string(), 0),
                ("chapter".to_string(), 1)
            ]
        );
    }

    #[test]
    fn test_counter_reset_parse_negative() {
        let reset = CounterReset::parse("section -5").unwrap();
        assert_eq!(reset.counters, vec![("section".to_string(), -5)]);
    }

    #[test]
    fn test_counter_reset_apply() {
        let reset = CounterReset::parse("section 0 chapter 5").unwrap();
        let mut system = CounterSystem::new();

        reset.apply(&mut system);

        assert_eq!(system.get_counter("section"), 0);
        assert_eq!(system.get_counter("chapter"), 5);
    }

    // CounterIncrement tests
    #[test]
    fn test_counter_increment_parse_none() {
        let incr = CounterIncrement::parse("none").unwrap();
        assert!(incr.counters.is_empty());
    }

    #[test]
    fn test_counter_increment_parse_single() {
        let incr = CounterIncrement::parse("section").unwrap();
        assert_eq!(incr.counters, vec![("section".to_string(), 1)]);
    }

    #[test]
    fn test_counter_increment_parse_with_value() {
        let incr = CounterIncrement::parse("section 2").unwrap();
        assert_eq!(incr.counters, vec![("section".to_string(), 2)]);
    }

    #[test]
    fn test_counter_increment_parse_multiple() {
        let incr = CounterIncrement::parse("section chapter 2").unwrap();
        assert_eq!(
            incr.counters,
            vec![
                ("section".to_string(), 1),
                ("chapter".to_string(), 2)
            ]
        );
    }

    #[test]
    fn test_counter_increment_parse_negative() {
        let incr = CounterIncrement::parse("section -1").unwrap();
        assert_eq!(incr.counters, vec![("section".to_string(), -1)]);
    }

    #[test]
    fn test_counter_increment_apply() {
        let reset = CounterReset::parse("section 0").unwrap();
        let incr = CounterIncrement::parse("section 2").unwrap();

        let mut system = CounterSystem::new();
        reset.apply(&mut system);
        incr.apply(&mut system);

        assert_eq!(system.get_counter("section"), 2);
    }

    // CounterSet tests
    #[test]
    fn test_counter_set_parse() {
        let set = CounterSet::parse("section 10").unwrap();
        assert_eq!(set.counters, vec![("section".to_string(), 10)]);
    }

    #[test]
    fn test_counter_set_apply() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 0);
        system.increment_counter("section", 5);

        let set = CounterSet::parse("section 20").unwrap();
        set.apply(&mut system);

        assert_eq!(system.get_counter("section"), 20);
    }

    // Counter name validation tests
    #[test]
    fn test_valid_counter_names() {
        assert!(is_valid_counter_name("section"));
        assert!(is_valid_counter_name("Section"));
        assert!(is_valid_counter_name("_section"));
        assert!(is_valid_counter_name("section1"));
        assert!(is_valid_counter_name("section-name"));
        assert!(is_valid_counter_name("section_name"));
        assert!(is_valid_counter_name("-section"));
        assert!(is_valid_counter_name("--custom"));
    }

    #[test]
    fn test_invalid_counter_names() {
        assert!(!is_valid_counter_name(""));
        assert!(!is_valid_counter_name("123"));
        assert!(!is_valid_counter_name("-123"));
        assert!(!is_valid_counter_name("none"));
        assert!(!is_valid_counter_name("inherit"));
        assert!(!is_valid_counter_name("initial"));
        assert!(!is_valid_counter_name("unset"));
    }

    // Integration test: typical counter usage
    #[test]
    fn test_typical_document_outline() {
        let mut system = CounterSystem::new();

        // body { counter-reset: chapter; }
        system.reset_counter("chapter", 0);

        // h1 { counter-increment: chapter; counter-reset: section; }
        system.increment_counter("chapter", 1);
        system.reset_counter("section", 0);

        // h1::before { content: counter(chapter) ". "; }
        assert_eq!(
            system.format_counter("chapter", CounterStyle::Decimal),
            "1"
        );

        // h2 { counter-increment: section; }
        system.increment_counter("section", 1);

        // h2::before { content: counter(chapter) "." counter(section) " "; }
        assert_eq!(
            format!(
                "{}.{} ",
                system.format_counter("chapter", CounterStyle::Decimal),
                system.format_counter("section", CounterStyle::Decimal)
            ),
            "1.1 "
        );

        system.increment_counter("section", 1);
        assert_eq!(
            format!(
                "{}.{} ",
                system.format_counter("chapter", CounterStyle::Decimal),
                system.format_counter("section", CounterStyle::Decimal)
            ),
            "1.2 "
        );

        // Next chapter
        system.increment_counter("chapter", 1);
        system.reset_counter("section", 0);
        assert_eq!(
            system.format_counter("chapter", CounterStyle::Decimal),
            "2"
        );

        system.increment_counter("section", 1);
        assert_eq!(
            format!(
                "{}.{} ",
                system.format_counter("chapter", CounterStyle::Decimal),
                system.format_counter("section", CounterStyle::Decimal)
            ),
            "2.1 "
        );
    }

    #[test]
    fn test_nested_list_counters() {
        let mut system = CounterSystem::new();

        // ol { counter-reset: item; }
        system.reset_counter("item", 0);

        // li { counter-increment: item; }
        // li::before { content: counters(item, ".") " "; }

        // First level: 1
        system.increment_counter("item", 1);
        assert_eq!(
            system.format_counters("item", ".", CounterStyle::Decimal),
            "1"
        );

        // Nested ol
        system.push_scope();
        system.reset_counter("item", 0);

        // Nested 1.1
        system.increment_counter("item", 1);
        assert_eq!(
            system.format_counters("item", ".", CounterStyle::Decimal),
            "1.1"
        );

        // Nested 1.2
        system.increment_counter("item", 1);
        assert_eq!(
            system.format_counters("item", ".", CounterStyle::Decimal),
            "1.2"
        );

        // Deeply nested
        system.push_scope();
        system.reset_counter("item", 0);
        system.increment_counter("item", 1);
        assert_eq!(
            system.format_counters("item", ".", CounterStyle::Decimal),
            "1.2.1"
        );

        system.pop_scope();
        system.pop_scope();

        // Back to first level: 2
        system.increment_counter("item", 1);
        assert_eq!(
            system.format_counters("item", ".", CounterStyle::Decimal),
            "2"
        );
    }
}
