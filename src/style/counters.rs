//! CSS Counter System
//!
//! This module implements CSS counters as specified in CSS Lists and Counters Module Level 3.
//! Counters are named numeric values that can be:
//!
//! - Created and initialized with `counter-reset`
//! - Incremented/decremented with `counter-increment`
//! - Displayed using `counter()` and `counters()` functions in the `content` property
//!
//! # CSS Specification
//!
//! Reference: CSS Lists and Counters Module Level 3
//! <https://www.w3.org/TR/css-lists-3/#counters>
//!
//! # Counter Scoping Rules
//!
//! - `counter-reset` creates a new counter instance on the element
//! - The counter is scoped to the element and its descendants
//! - Nested `counter-reset` creates a new scope (stacking)
//! - `counter-increment` affects the innermost counter with that name
//! - If no counter exists when incrementing, one is implicitly created with value 0
//!
//! # Examples
//!
//! ```
//! use fastrender::style::counters::{CounterSet, CounterSetItem, CounterManager};
//!
//! // Parse counter-reset: chapter 0 section 0
//! let reset = CounterSet::parse("chapter 0 section 0").unwrap();
//! assert_eq!(reset.items.len(), 2);
//!
//! // Use CounterManager during tree traversal
//! let mut manager = CounterManager::new();
//!
//! // Enter element with counter-reset
//! manager.enter_scope();
//! manager.apply_reset(&reset);
//! assert_eq!(manager.get("chapter"), Some(0));
//!
//! // Increment the counter (use parse_increment for default increment of 1)
//! let increment = CounterSet::parse_increment("chapter").unwrap();
//! manager.apply_increment(&increment);
//! assert_eq!(manager.get("chapter"), Some(1));
//!
//! // Leave the scope
//! manager.leave_scope();
//! ```

use std::collections::HashMap;
use std::fmt;

use super::content::CounterStyle;

/// A set of counter specifications for `counter-reset` or `counter-increment`
///
/// CSS syntax: `<counter-name> <integer>?`+
///
/// Examples:
/// - `counter-reset: chapter` → CounterSet with [("chapter", 0)]
/// - `counter-reset: chapter 5` → CounterSet with [("chapter", 5)]
/// - `counter-reset: chapter 0 section 0` → CounterSet with [("chapter", 0), ("section", 0)]
/// - `counter-increment: chapter` → CounterSet with [("chapter", 1)]
/// - `counter-increment: chapter 2` → CounterSet with [("chapter", 2)]
#[derive(Debug, Clone, PartialEq, Default)]
pub struct CounterSet {
    /// The list of counter items
    pub items: Vec<CounterSetItem>,
}

/// A single counter specification within a CounterSet
#[derive(Debug, Clone, PartialEq)]
pub struct CounterSetItem {
    /// The counter name (e.g., "chapter", "section", "list-item")
    pub name: String,
    /// The value for reset or the increment amount
    pub value: i32,
}

impl CounterSet {
    /// Creates a new empty CounterSet
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    /// Creates a CounterSet with a single counter
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSet;
    ///
    /// let set = CounterSet::single("chapter", 0);
    /// assert_eq!(set.items.len(), 1);
    /// assert_eq!(set.items[0].name, "chapter");
    /// assert_eq!(set.items[0].value, 0);
    /// ```
    pub fn single(name: impl Into<String>, value: i32) -> Self {
        Self {
            items: vec![CounterSetItem {
                name: name.into(),
                value,
            }],
        }
    }

    /// Parses a counter-reset value string
    ///
    /// Default value for missing integers is 0.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSet;
    ///
    /// let set = CounterSet::parse_reset("chapter").unwrap();
    /// assert_eq!(set.items[0].value, 0); // default reset value
    ///
    /// let set = CounterSet::parse_reset("chapter 5").unwrap();
    /// assert_eq!(set.items[0].value, 5);
    ///
    /// let set = CounterSet::parse_reset("chapter section").unwrap();
    /// assert_eq!(set.items.len(), 2);
    /// ```
    pub fn parse_reset(input: &str) -> Option<Self> {
        Self::parse_with_default(input, 0)
    }

    /// Parses a counter-increment value string
    ///
    /// Default value for missing integers is 1.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::CounterSet;
    ///
    /// let set = CounterSet::parse_increment("chapter").unwrap();
    /// assert_eq!(set.items[0].value, 1); // default increment value
    ///
    /// let set = CounterSet::parse_increment("chapter 2").unwrap();
    /// assert_eq!(set.items[0].value, 2);
    /// ```
    pub fn parse_increment(input: &str) -> Option<Self> {
        Self::parse_with_default(input, 1)
    }

    /// Parses a counter-set value string (CSS counter-set property)
    ///
    /// Default value for missing integers is 0.
    pub fn parse_set(input: &str) -> Option<Self> {
        Self::parse_with_default(input, 0)
    }

    /// Parses a counter specification string with a default value
    ///
    /// # Arguments
    ///
    /// * `input` - The CSS value string
    /// * `default_value` - Default value if integer is omitted
    ///
    /// # Returns
    ///
    /// Some(CounterSet) on success, None if parsing fails
    pub fn parse_with_default(input: &str, default_value: i32) -> Option<Self> {
        let input = input.trim();

        // Handle "none" keyword
        if input.eq_ignore_ascii_case("none") {
            return Some(Self::new());
        }

        let mut items = Vec::new();
        let mut tokens = input.split_whitespace().peekable();

        while let Some(token) = tokens.next() {
            // Skip keywords we don't understand
            if token.eq_ignore_ascii_case("none") {
                continue;
            }

            // Token should be a counter name (identifier)
            // CSS identifiers can't start with a digit or hyphen followed by digit
            if token.starts_with(|c: char| c.is_ascii_digit())
                || (token.starts_with('-') && token.chars().nth(1).map(|c| c.is_ascii_digit()).unwrap_or(false))
            {
                // This looks like a number without a preceding name - invalid
                return None;
            }

            let name = token.to_string();

            // Check if next token is a number
            let value = if let Some(&next) = tokens.peek() {
                if let Ok(num) = next.parse::<i32>() {
                    tokens.next(); // consume the number
                    num
                } else {
                    default_value
                }
            } else {
                default_value
            };

            items.push(CounterSetItem { name, value });
        }

        if items.is_empty() && !input.is_empty() && !input.eq_ignore_ascii_case("none") {
            // Input wasn't empty but we got no items - parsing failed
            return None;
        }

        Some(Self { items })
    }

    /// Generic parse method (uses 0 as default)
    ///
    /// For reset-style parsing, use `parse_reset()`.
    /// For increment-style parsing, use `parse_increment()`.
    pub fn parse(input: &str) -> Option<Self> {
        Self::parse_with_default(input, 0)
    }

    /// Returns true if the set is empty (no counters)
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Returns the number of counter items
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Adds a counter to the set
    pub fn add(&mut self, name: impl Into<String>, value: i32) {
        self.items.push(CounterSetItem {
            name: name.into(),
            value,
        });
    }

    /// Gets a counter value by name (returns the first match)
    pub fn get(&self, name: &str) -> Option<i32> {
        self.items.iter().find(|item| item.name == name).map(|item| item.value)
    }
}

impl fmt::Display for CounterSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.items.is_empty() {
            return write!(f, "none");
        }

        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", item)?;
        }
        Ok(())
    }
}

impl CounterSetItem {
    /// Creates a new counter item
    pub fn new(name: impl Into<String>, value: i32) -> Self {
        Self {
            name: name.into(),
            value,
        }
    }
}

impl fmt::Display for CounterSetItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.name, self.value)
    }
}

/// A scope containing counter values
///
/// Each scope tracks counters that were reset at that level.
#[derive(Debug, Clone, Default)]
struct CounterScope {
    /// Counters reset at this scope level
    /// Key: counter name, Value: current value
    counters: HashMap<String, i32>,
}

impl CounterScope {
    fn new() -> Self {
        Self {
            counters: HashMap::new(),
        }
    }
}

/// Manages CSS counters during document tree traversal
///
/// The CounterManager maintains a stack of counter scopes that mirror
/// the document tree structure. Each scope contains counters that were
/// reset at that level.
///
/// # Usage Pattern
///
/// During tree traversal:
/// 1. Call `enter_scope()` when entering an element
/// 2. Call `apply_reset()` if element has `counter-reset`
/// 3. Call `apply_increment()` if element has `counter-increment`
/// 4. Use `get()` or `get_all()` to read counter values for content generation
/// 5. Call `leave_scope()` when leaving the element
///
/// # Examples
///
/// ```
/// use fastrender::style::counters::{CounterSet, CounterManager};
///
/// let mut manager = CounterManager::new();
///
/// // Simulate: <body> with counter-reset: chapter
/// manager.enter_scope();
/// manager.apply_reset(&CounterSet::single("chapter", 0));
///
/// // Simulate: <h1> with counter-increment: chapter
/// manager.enter_scope();
/// manager.apply_increment(&CounterSet::single("chapter", 1));
/// assert_eq!(manager.get("chapter"), Some(1));
/// manager.leave_scope();
///
/// // Second h1
/// manager.enter_scope();
/// manager.apply_increment(&CounterSet::single("chapter", 1));
/// assert_eq!(manager.get("chapter"), Some(2));
/// manager.leave_scope();
///
/// manager.leave_scope(); // body
/// ```
#[derive(Debug, Clone)]
pub struct CounterManager {
    /// Stack of counter scopes
    scopes: Vec<CounterScope>,
}

impl CounterManager {
    /// Creates a new CounterManager with an empty root scope
    pub fn new() -> Self {
        Self {
            // Start with a root scope for implicit counters
            scopes: vec![CounterScope::new()],
        }
    }

    /// Enters a new scope (when entering an element)
    ///
    /// This creates a new scope that inherits counter visibility from parent scopes.
    /// Counters reset in this scope will shadow any counters with the same name
    /// in parent scopes.
    pub fn enter_scope(&mut self) {
        self.scopes.push(CounterScope::new());
    }

    /// Leaves the current scope (when leaving an element)
    ///
    /// This removes the current scope and all counters reset within it.
    /// Always maintains at least the root scope.
    pub fn leave_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Applies a counter-reset specification
    ///
    /// Creates new counter instances in the current scope, shadowing any
    /// counters with the same names in parent scopes.
    ///
    /// # Arguments
    ///
    /// * `counter_set` - The parsed counter-reset value
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::{CounterSet, CounterManager};
    ///
    /// let mut manager = CounterManager::new();
    /// manager.enter_scope();
    ///
    /// // counter-reset: chapter 0 section 0
    /// let reset = CounterSet::parse_reset("chapter 0 section 0").unwrap();
    /// manager.apply_reset(&reset);
    ///
    /// assert_eq!(manager.get("chapter"), Some(0));
    /// assert_eq!(manager.get("section"), Some(0));
    /// ```
    pub fn apply_reset(&mut self, counter_set: &CounterSet) {
        if let Some(scope) = self.scopes.last_mut() {
            for item in &counter_set.items {
                scope.counters.insert(item.name.clone(), item.value);
            }
        }
    }

    /// Applies a counter-increment specification
    ///
    /// Increments the innermost counter with each name. If no counter exists,
    /// one is implicitly created with value 0 (in the current scope) and then incremented.
    ///
    /// # Arguments
    ///
    /// * `counter_set` - The parsed counter-increment value
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::{CounterSet, CounterManager};
    ///
    /// let mut manager = CounterManager::new();
    /// manager.enter_scope();
    /// manager.apply_reset(&CounterSet::single("chapter", 0));
    ///
    /// manager.enter_scope();
    /// manager.apply_increment(&CounterSet::single("chapter", 1));
    /// assert_eq!(manager.get("chapter"), Some(1));
    ///
    /// manager.apply_increment(&CounterSet::single("chapter", 1));
    /// assert_eq!(manager.get("chapter"), Some(2));
    /// ```
    pub fn apply_increment(&mut self, counter_set: &CounterSet) {
        for item in &counter_set.items {
            // Find the innermost scope that has this counter
            let mut found = false;
            for scope in self.scopes.iter_mut().rev() {
                if let Some(value) = scope.counters.get_mut(&item.name) {
                    *value += item.value;
                    found = true;
                    break;
                }
            }

            // If counter doesn't exist, create it implicitly with value 0 + increment
            // Per CSS spec: "If there is no counter of the given name on the element,
            // the element instantiates a new counter of the given name with a starting
            // value of 0 before incrementing"
            if !found {
                if let Some(scope) = self.scopes.last_mut() {
                    scope.counters.insert(item.name.clone(), item.value);
                }
            }
        }
    }

    /// Applies a counter-set specification (CSS counter-set property)
    ///
    /// Sets the value of the innermost counter. If no counter exists,
    /// one is implicitly created in the current scope.
    ///
    /// Note: This is different from counter-reset which always creates
    /// a new counter in the current scope. counter-set modifies an existing counter.
    pub fn apply_set(&mut self, counter_set: &CounterSet) {
        for item in &counter_set.items {
            // Find the innermost scope that has this counter
            let mut found = false;
            for scope in self.scopes.iter_mut().rev() {
                if let Some(value) = scope.counters.get_mut(&item.name) {
                    *value = item.value;
                    found = true;
                    break;
                }
            }

            // If counter doesn't exist, create it in current scope
            if !found {
                if let Some(scope) = self.scopes.last_mut() {
                    scope.counters.insert(item.name.clone(), item.value);
                }
            }
        }
    }

    /// Gets the current value of a counter
    ///
    /// Returns the value of the innermost counter with the given name,
    /// or None if no such counter exists.
    ///
    /// # Arguments
    ///
    /// * `name` - The counter name
    ///
    /// # Returns
    ///
    /// Some(value) if the counter exists, None otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::{CounterSet, CounterManager};
    ///
    /// let mut manager = CounterManager::new();
    /// manager.enter_scope();
    /// manager.apply_reset(&CounterSet::single("chapter", 5));
    ///
    /// assert_eq!(manager.get("chapter"), Some(5));
    /// assert_eq!(manager.get("nonexistent"), None);
    /// ```
    pub fn get(&self, name: &str) -> Option<i32> {
        // Search from innermost to outermost scope
        for scope in self.scopes.iter().rev() {
            if let Some(&value) = scope.counters.get(name) {
                return Some(value);
            }
        }
        None
    }

    /// Gets the value of a counter, returning 0 if it doesn't exist
    ///
    /// This is useful for content generation where missing counters should
    /// render as 0.
    pub fn get_or_zero(&self, name: &str) -> i32 {
        self.get(name).unwrap_or(0)
    }

    /// Gets all values of a counter from all scopes
    ///
    /// Returns values from outermost to innermost scope, suitable for
    /// the `counters()` CSS function which displays nested counter values.
    ///
    /// # Arguments
    ///
    /// * `name` - The counter name
    ///
    /// # Returns
    ///
    /// A vector of counter values from outer to inner scope.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::{CounterSet, CounterManager};
    ///
    /// let mut manager = CounterManager::new();
    ///
    /// // Outer list
    /// manager.enter_scope();
    /// manager.apply_reset(&CounterSet::single("item", 0));
    /// manager.apply_increment(&CounterSet::single("item", 1));
    ///
    /// // Nested list
    /// manager.enter_scope();
    /// manager.apply_reset(&CounterSet::single("item", 0));
    /// manager.apply_increment(&CounterSet::single("item", 1));
    ///
    /// // Doubly nested
    /// manager.enter_scope();
    /// manager.apply_reset(&CounterSet::single("item", 0));
    /// manager.apply_increment(&CounterSet::single("item", 1));
    ///
    /// let all = manager.get_all("item");
    /// assert_eq!(all, vec![1, 1, 1]); // "1.1.1"
    /// ```
    pub fn get_all(&self, name: &str) -> Vec<i32> {
        let mut values = Vec::new();

        // Collect from outermost to innermost
        for scope in &self.scopes {
            if let Some(&value) = scope.counters.get(name) {
                values.push(value);
            }
        }

        values
    }

    /// Formats a counter value using the specified style
    ///
    /// # Arguments
    ///
    /// * `name` - The counter name
    /// * `style` - The counter style for formatting
    ///
    /// # Returns
    ///
    /// The formatted counter value, or "0" if the counter doesn't exist.
    pub fn format(&self, name: &str, style: CounterStyle) -> String {
        let value = self.get_or_zero(name);
        style.format(value)
    }

    /// Formats all counter values with a separator (for counters() function)
    ///
    /// # Arguments
    ///
    /// * `name` - The counter name
    /// * `separator` - The string to insert between values
    /// * `style` - The counter style for formatting
    ///
    /// # Returns
    ///
    /// The formatted string, e.g., "1.2.3" for separator "."
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::style::counters::{CounterSet, CounterManager};
    /// use fastrender::style::content::CounterStyle;
    ///
    /// let mut manager = CounterManager::new();
    ///
    /// manager.enter_scope();
    /// manager.apply_reset(&CounterSet::single("section", 1));
    ///
    /// manager.enter_scope();
    /// manager.apply_reset(&CounterSet::single("section", 2));
    ///
    /// manager.enter_scope();
    /// manager.apply_reset(&CounterSet::single("section", 3));
    ///
    /// assert_eq!(manager.format_all("section", ".", CounterStyle::Decimal), "1.2.3");
    /// ```
    pub fn format_all(&self, name: &str, separator: &str, style: CounterStyle) -> String {
        let values = self.get_all(name);
        if values.is_empty() {
            // Per CSS spec, if no counter exists, return "0"
            return style.format(0);
        }

        values
            .iter()
            .map(|&v| style.format(v))
            .collect::<Vec<_>>()
            .join(separator)
    }

    /// Returns the current scope depth (for debugging)
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    /// Checks if a counter exists in any scope
    pub fn has(&self, name: &str) -> bool {
        self.scopes.iter().any(|scope| scope.counters.contains_key(name))
    }

    /// Resets the manager to initial state
    pub fn reset(&mut self) {
        self.scopes.clear();
        self.scopes.push(CounterScope::new());
    }
}

impl Default for CounterManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Computed values for counter-related CSS properties
#[derive(Debug, Clone, PartialEq, Default)]
pub struct CounterProperties {
    /// The counter-reset property value
    /// None means no counter-reset specified
    pub counter_reset: Option<CounterSet>,

    /// The counter-increment property value
    /// None means no counter-increment specified
    pub counter_increment: Option<CounterSet>,

    /// The counter-set property value (CSS counter-set property)
    /// None means no counter-set specified
    pub counter_set: Option<CounterSet>,
}

impl CounterProperties {
    /// Creates new counter properties with no values set
    pub fn new() -> Self {
        Self::default()
    }

    /// Checks if any counter property is set
    pub fn has_any(&self) -> bool {
        self.counter_reset.is_some() || self.counter_increment.is_some() || self.counter_set.is_some()
    }

    /// Applies these counter properties to a CounterManager
    ///
    /// The order is: reset, set, increment (per CSS spec)
    pub fn apply_to(&self, manager: &mut CounterManager) {
        if let Some(ref reset) = self.counter_reset {
            manager.apply_reset(reset);
        }
        if let Some(ref set) = self.counter_set {
            manager.apply_set(set);
        }
        if let Some(ref increment) = self.counter_increment {
            manager.apply_increment(increment);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // === CounterSet Parsing Tests ===

    #[test]
    fn test_parse_single_counter_no_value() {
        let set = CounterSet::parse_reset("chapter").unwrap();
        assert_eq!(set.items.len(), 1);
        assert_eq!(set.items[0].name, "chapter");
        assert_eq!(set.items[0].value, 0); // default for reset
    }

    #[test]
    fn test_parse_single_counter_with_value() {
        let set = CounterSet::parse_reset("chapter 5").unwrap();
        assert_eq!(set.items.len(), 1);
        assert_eq!(set.items[0].name, "chapter");
        assert_eq!(set.items[0].value, 5);
    }

    #[test]
    fn test_parse_multiple_counters() {
        let set = CounterSet::parse_reset("chapter 0 section 0").unwrap();
        assert_eq!(set.items.len(), 2);
        assert_eq!(set.items[0].name, "chapter");
        assert_eq!(set.items[0].value, 0);
        assert_eq!(set.items[1].name, "section");
        assert_eq!(set.items[1].value, 0);
    }

    #[test]
    fn test_parse_multiple_counters_mixed_values() {
        let set = CounterSet::parse_reset("chapter section 5 subsection").unwrap();
        assert_eq!(set.items.len(), 3);
        assert_eq!(set.items[0].name, "chapter");
        assert_eq!(set.items[0].value, 0); // default
        assert_eq!(set.items[1].name, "section");
        assert_eq!(set.items[1].value, 5);
        assert_eq!(set.items[2].name, "subsection");
        assert_eq!(set.items[2].value, 0); // default
    }

    #[test]
    fn test_parse_increment_default() {
        let set = CounterSet::parse_increment("chapter").unwrap();
        assert_eq!(set.items[0].value, 1); // default for increment
    }

    #[test]
    fn test_parse_increment_with_value() {
        let set = CounterSet::parse_increment("chapter 2").unwrap();
        assert_eq!(set.items[0].value, 2);
    }

    #[test]
    fn test_parse_negative_value() {
        let set = CounterSet::parse_increment("chapter -1").unwrap();
        assert_eq!(set.items[0].value, -1);
    }

    #[test]
    fn test_parse_none() {
        let set = CounterSet::parse_reset("none").unwrap();
        assert!(set.is_empty());
    }

    #[test]
    fn test_parse_empty() {
        let set = CounterSet::parse_reset("").unwrap();
        assert!(set.is_empty());
    }

    #[test]
    fn test_parse_whitespace() {
        let set = CounterSet::parse_reset("  chapter   5  ").unwrap();
        assert_eq!(set.items[0].name, "chapter");
        assert_eq!(set.items[0].value, 5);
    }

    #[test]
    fn test_counter_set_display() {
        let set = CounterSet::parse_reset("chapter 0 section 5").unwrap();
        assert_eq!(format!("{}", set), "chapter 0 section 5");
    }

    #[test]
    fn test_counter_set_display_none() {
        let set = CounterSet::new();
        assert_eq!(format!("{}", set), "none");
    }

    // === CounterManager Basic Tests ===

    #[test]
    fn test_manager_new() {
        let manager = CounterManager::new();
        assert_eq!(manager.depth(), 1); // root scope
        assert_eq!(manager.get("any"), None);
    }

    #[test]
    fn test_manager_reset_single() {
        let mut manager = CounterManager::new();
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("chapter", 0));

        assert_eq!(manager.get("chapter"), Some(0));
    }

    #[test]
    fn test_manager_reset_with_value() {
        let mut manager = CounterManager::new();
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("chapter", 10));

        assert_eq!(manager.get("chapter"), Some(10));
    }

    #[test]
    fn test_manager_increment() {
        let mut manager = CounterManager::new();
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("chapter", 0));
        manager.apply_increment(&CounterSet::single("chapter", 1));

        assert_eq!(manager.get("chapter"), Some(1));
    }

    #[test]
    fn test_manager_increment_multiple_times() {
        let mut manager = CounterManager::new();
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("chapter", 0));

        for _ in 0..5 {
            manager.apply_increment(&CounterSet::single("chapter", 1));
        }

        assert_eq!(manager.get("chapter"), Some(5));
    }

    #[test]
    fn test_manager_increment_by_amount() {
        let mut manager = CounterManager::new();
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("chapter", 0));
        manager.apply_increment(&CounterSet::single("chapter", 5));

        assert_eq!(manager.get("chapter"), Some(5));
    }

    #[test]
    fn test_manager_increment_nonexistent_creates() {
        let mut manager = CounterManager::new();
        manager.enter_scope();
        // Increment without prior reset should create counter at 0 + increment
        manager.apply_increment(&CounterSet::single("chapter", 1));

        assert_eq!(manager.get("chapter"), Some(1));
    }

    // === Scope Tests ===

    #[test]
    fn test_manager_scope_shadowing() {
        let mut manager = CounterManager::new();

        // Outer scope
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("item", 5));
        assert_eq!(manager.get("item"), Some(5));

        // Inner scope shadows outer
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("item", 0));
        assert_eq!(manager.get("item"), Some(0));

        // Leave inner scope - outer is visible again
        manager.leave_scope();
        assert_eq!(manager.get("item"), Some(5));
    }

    #[test]
    fn test_manager_scope_increment_inner() {
        let mut manager = CounterManager::new();

        // Outer scope
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("item", 0));

        // Inner scope (no reset - inherits outer counter)
        manager.enter_scope();
        manager.apply_increment(&CounterSet::single("item", 1));
        assert_eq!(manager.get("item"), Some(1));

        manager.leave_scope();
        // Outer counter was modified by inner increment
        assert_eq!(manager.get("item"), Some(1));
    }

    #[test]
    fn test_manager_scope_nested_reset() {
        let mut manager = CounterManager::new();

        // Simulate nested lists
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("item", 0));
        manager.apply_increment(&CounterSet::single("item", 1));
        assert_eq!(manager.get("item"), Some(1));

        // Nested list
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("item", 0));
        manager.apply_increment(&CounterSet::single("item", 1));
        assert_eq!(manager.get("item"), Some(1));

        manager.leave_scope();
        assert_eq!(manager.get("item"), Some(1)); // back to outer
    }

    // === get_all Tests ===

    #[test]
    fn test_manager_get_all_empty() {
        let manager = CounterManager::new();
        assert_eq!(manager.get_all("item"), Vec::<i32>::new());
    }

    #[test]
    fn test_manager_get_all_single() {
        let mut manager = CounterManager::new();
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("item", 5));

        assert_eq!(manager.get_all("item"), vec![5]);
    }

    #[test]
    fn test_manager_get_all_nested() {
        let mut manager = CounterManager::new();

        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("item", 1));

        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("item", 2));

        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("item", 3));

        assert_eq!(manager.get_all("item"), vec![1, 2, 3]);
    }

    #[test]
    fn test_manager_get_all_nested_incremented() {
        let mut manager = CounterManager::new();

        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("section", 0));
        manager.apply_increment(&CounterSet::single("section", 1));

        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("section", 0));
        manager.apply_increment(&CounterSet::single("section", 1));
        manager.apply_increment(&CounterSet::single("section", 1));

        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("section", 0));
        manager.apply_increment(&CounterSet::single("section", 1));

        // Should be [1, 2, 1] representing "1.2.1"
        assert_eq!(manager.get_all("section"), vec![1, 2, 1]);
    }

    // === Formatting Tests ===

    #[test]
    fn test_manager_format() {
        let mut manager = CounterManager::new();
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("chapter", 0));
        manager.apply_increment(&CounterSet::single("chapter", 4));

        assert_eq!(manager.format("chapter", CounterStyle::Decimal), "4");
        assert_eq!(manager.format("chapter", CounterStyle::LowerRoman), "iv");
        assert_eq!(manager.format("chapter", CounterStyle::UpperAlpha), "D");
    }

    #[test]
    fn test_manager_format_nonexistent() {
        let manager = CounterManager::new();
        assert_eq!(manager.format("chapter", CounterStyle::Decimal), "0");
    }

    #[test]
    fn test_manager_format_all() {
        let mut manager = CounterManager::new();

        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("section", 1));

        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("section", 2));

        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("section", 3));

        assert_eq!(manager.format_all("section", ".", CounterStyle::Decimal), "1.2.3");
        assert_eq!(manager.format_all("section", "-", CounterStyle::LowerAlpha), "a-b-c");
    }

    #[test]
    fn test_manager_format_all_nonexistent() {
        let manager = CounterManager::new();
        assert_eq!(manager.format_all("section", ".", CounterStyle::Decimal), "0");
    }

    // === counter-set Property Tests ===

    #[test]
    fn test_manager_set_existing() {
        let mut manager = CounterManager::new();
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("chapter", 0));
        manager.apply_set(&CounterSet::single("chapter", 10));

        assert_eq!(manager.get("chapter"), Some(10));
    }

    #[test]
    fn test_manager_set_nonexistent() {
        let mut manager = CounterManager::new();
        manager.enter_scope();
        manager.apply_set(&CounterSet::single("chapter", 5));

        assert_eq!(manager.get("chapter"), Some(5));
    }

    #[test]
    fn test_manager_set_in_parent_scope() {
        let mut manager = CounterManager::new();

        // Parent scope with counter
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("chapter", 0));

        // Child scope - set should modify parent's counter
        manager.enter_scope();
        manager.apply_set(&CounterSet::single("chapter", 10));

        assert_eq!(manager.get("chapter"), Some(10));

        // Verify it modified parent
        manager.leave_scope();
        assert_eq!(manager.get("chapter"), Some(10));
    }

    // === CounterProperties Tests ===

    #[test]
    fn test_counter_properties_default() {
        let props = CounterProperties::new();
        assert!(!props.has_any());
    }

    #[test]
    fn test_counter_properties_apply_to() {
        let mut props = CounterProperties::new();
        props.counter_reset = Some(CounterSet::single("chapter", 0));
        props.counter_increment = Some(CounterSet::single("chapter", 1));

        let mut manager = CounterManager::new();
        manager.enter_scope();
        props.apply_to(&mut manager);

        assert_eq!(manager.get("chapter"), Some(1));
    }

    // === Edge Case Tests ===

    #[test]
    fn test_manager_multiple_counters() {
        let mut manager = CounterManager::new();
        manager.enter_scope();

        let reset = CounterSet::parse_reset("chapter 0 section 0 subsection 0").unwrap();
        manager.apply_reset(&reset);

        assert_eq!(manager.get("chapter"), Some(0));
        assert_eq!(manager.get("section"), Some(0));
        assert_eq!(manager.get("subsection"), Some(0));

        let inc = CounterSet::parse_increment("chapter section 2").unwrap();
        manager.apply_increment(&inc);

        assert_eq!(manager.get("chapter"), Some(1));
        assert_eq!(manager.get("section"), Some(2));
        assert_eq!(manager.get("subsection"), Some(0));
    }

    #[test]
    fn test_manager_leave_scope_safety() {
        let mut manager = CounterManager::new();
        // Trying to leave root scope should be safe
        manager.leave_scope();
        manager.leave_scope();
        manager.leave_scope();
        assert_eq!(manager.depth(), 1); // Still have root scope
    }

    #[test]
    fn test_manager_reset() {
        let mut manager = CounterManager::new();
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("chapter", 10));
        manager.enter_scope();

        manager.reset();

        assert_eq!(manager.depth(), 1);
        assert_eq!(manager.get("chapter"), None);
    }

    #[test]
    fn test_counter_hyphenated_name() {
        let set = CounterSet::parse_reset("list-item 0").unwrap();
        assert_eq!(set.items[0].name, "list-item");
    }

    #[test]
    fn test_counter_name_with_underscore() {
        let set = CounterSet::parse_reset("my_counter 0").unwrap();
        assert_eq!(set.items[0].name, "my_counter");
    }

    // === Real World Usage Simulation ===

    #[test]
    fn test_chapter_section_numbering() {
        // Simulating a typical document with chapters and sections where sections
        // are nested inside chapter containers:
        //
        // body { counter-reset: chapter; }
        // .chapter { counter-increment: chapter; counter-reset: section; }
        // .section { counter-increment: section; }
        //
        // <body>
        //   <div class="chapter">
        //     <h1>Chapter 1</h1>
        //     <div class="section"><h2>1.1</h2></div>
        //     <div class="section"><h2>1.2</h2></div>
        //   </div>
        //   <div class="chapter">
        //     <h1>Chapter 2</h1>
        //     <div class="section"><h2>2.1</h2></div>
        //   </div>
        // </body>

        let mut manager = CounterManager::new();

        // Enter body
        manager.enter_scope();
        manager.apply_reset(&CounterSet::parse_reset("chapter").unwrap());

        // Chapter 1 container
        manager.enter_scope();
        manager.apply_increment(&CounterSet::parse_increment("chapter").unwrap());
        manager.apply_reset(&CounterSet::parse_reset("section").unwrap());
        assert_eq!(manager.format("chapter", CounterStyle::Decimal), "1");

        // Section 1.1
        manager.enter_scope();
        manager.apply_increment(&CounterSet::parse_increment("section").unwrap());
        assert_eq!(
            format!(
                "{}.{}",
                manager.format("chapter", CounterStyle::Decimal),
                manager.format("section", CounterStyle::Decimal)
            ),
            "1.1"
        );
        manager.leave_scope();

        // Section 1.2
        manager.enter_scope();
        manager.apply_increment(&CounterSet::parse_increment("section").unwrap());
        assert_eq!(
            format!(
                "{}.{}",
                manager.format("chapter", CounterStyle::Decimal),
                manager.format("section", CounterStyle::Decimal)
            ),
            "1.2"
        );
        manager.leave_scope();

        manager.leave_scope(); // Chapter 1 container

        // Chapter 2 container
        manager.enter_scope();
        manager.apply_increment(&CounterSet::parse_increment("chapter").unwrap());
        manager.apply_reset(&CounterSet::parse_reset("section").unwrap());
        assert_eq!(manager.format("chapter", CounterStyle::Decimal), "2");

        // Section 2.1
        manager.enter_scope();
        manager.apply_increment(&CounterSet::parse_increment("section").unwrap());
        assert_eq!(
            format!(
                "{}.{}",
                manager.format("chapter", CounterStyle::Decimal),
                manager.format("section", CounterStyle::Decimal)
            ),
            "2.1"
        );
        manager.leave_scope();

        manager.leave_scope(); // Chapter 2 container
        manager.leave_scope(); // body
    }

    #[test]
    fn test_nested_list_numbering() {
        // Simulating:
        // ol { counter-reset: item; }
        // li { counter-increment: item; }
        // li::before { content: counters(item, "."); }

        let mut manager = CounterManager::new();

        // First ol
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("item", 0));

        // First li
        manager.enter_scope();
        manager.apply_increment(&CounterSet::single("item", 1));
        assert_eq!(manager.format_all("item", ".", CounterStyle::Decimal), "1");

        // Nested ol
        manager.enter_scope();
        manager.apply_reset(&CounterSet::single("item", 0));

        // Nested li 1
        manager.enter_scope();
        manager.apply_increment(&CounterSet::single("item", 1));
        assert_eq!(manager.format_all("item", ".", CounterStyle::Decimal), "1.1");
        manager.leave_scope();

        // Nested li 2
        manager.enter_scope();
        manager.apply_increment(&CounterSet::single("item", 1));
        assert_eq!(manager.format_all("item", ".", CounterStyle::Decimal), "1.2");
        manager.leave_scope();

        manager.leave_scope(); // nested ol
        manager.leave_scope(); // first li

        // Second li
        manager.enter_scope();
        manager.apply_increment(&CounterSet::single("item", 1));
        assert_eq!(manager.format_all("item", ".", CounterStyle::Decimal), "2");
        manager.leave_scope();

        manager.leave_scope(); // first ol
    }
}
