//! CSS Custom Properties (CSS Variables) Storage
//!
//! This module provides storage and resolution for CSS custom properties
//! (also known as CSS variables). Custom properties are defined with `--`
//! prefix and can be referenced using `var()` function.
//!
//! # Overview
//!
//! CSS custom properties support:
//! - Definition: `--primary-color: #007bff;`
//! - Usage: `color: var(--primary-color);`
//! - Fallback: `color: var(--primary-color, blue);`
//! - Inheritance: Custom properties cascade down the DOM tree
//!
//! # Example
//!
//! ```
//! use fastrender::style::variables::CssVariables;
//!
//! // Create a new variables store
//! let mut vars = CssVariables::new();
//!
//! // Set custom properties
//! vars.set("--primary-color", "#007bff");
//! vars.set("--font-size", "16px");
//!
//! // Get a property value
//! assert_eq!(vars.get("--primary-color"), Some(&"#007bff".to_string()));
//!
//! // Resolve var() references
//! assert_eq!(vars.resolve("var(--primary-color)"), Some("#007bff".to_string()));
//!
//! // Use fallback when variable is not defined
//! assert_eq!(vars.resolve("var(--undefined, red)"), Some("red".to_string()));
//! ```
//!
//! # Inheritance
//!
//! CSS custom properties are inherited by default. Child elements receive
//! all custom properties from their parent:
//!
//! ```
//! use fastrender::style::variables::CssVariables;
//!
//! // Parent defines variables
//! let mut parent = CssVariables::new();
//! parent.set("--color", "blue");
//!
//! // Child inherits from parent
//! let child = CssVariables::inherit_from(&parent);
//! assert_eq!(child.get("--color"), Some(&"blue".to_string()));
//! ```
//!
//! # References
//!
//! - CSS Custom Properties Level 1: <https://www.w3.org/TR/css-variables-1/>

use std::collections::HashMap;

/// Maximum recursion depth for resolving nested var() references.
/// This prevents infinite loops from circular variable references.
const MAX_VAR_RESOLUTION_DEPTH: usize = 10;

/// Storage for CSS custom properties (CSS variables).
///
/// This struct manages CSS custom properties defined with `--` prefix.
/// It supports setting, getting, resolving var() references, and
/// inheritance from parent elements.
///
/// # Performance Considerations
///
/// - Properties are stored in a `HashMap<String, String>` for O(1) lookup
/// - Inheritance creates a clone of the parent's properties
/// - Resolution involves string parsing and may be called frequently;
///   consider caching resolved values at the call site if needed
#[derive(Debug, Clone, PartialEq, Default)]
pub struct CssVariables {
    /// Map of custom property names to their values.
    /// Names include the `--` prefix (e.g., "--primary-color").
    properties: HashMap<String, String>,
}

impl CssVariables {
    /// Creates a new empty CSS variables store.
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let vars = CssVariables::new();
    /// assert!(vars.is_empty());
    /// ```
    pub fn new() -> Self {
        Self {
            properties: HashMap::new(),
        }
    }

    /// Creates a new CSS variables store with the given capacity.
    ///
    /// This is useful when you know approximately how many variables
    /// will be stored, to avoid reallocations.
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let vars = CssVariables::with_capacity(10);
    /// assert!(vars.is_empty());
    /// ```
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            properties: HashMap::with_capacity(capacity),
        }
    }

    /// Creates a new CSS variables store by inheriting from a parent.
    ///
    /// All custom properties from the parent are cloned into the new store.
    /// This follows CSS inheritance rules where custom properties cascade
    /// down the DOM tree.
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut parent = CssVariables::new();
    /// parent.set("--color", "blue");
    /// parent.set("--size", "16px");
    ///
    /// let child = CssVariables::inherit_from(&parent);
    /// assert_eq!(child.get("--color"), Some(&"blue".to_string()));
    /// assert_eq!(child.get("--size"), Some(&"16px".to_string()));
    /// ```
    pub fn inherit_from(parent: &CssVariables) -> Self {
        Self {
            properties: parent.properties.clone(),
        }
    }

    /// Inherits all custom properties from a parent store.
    ///
    /// This merges the parent's properties into this store, but existing
    /// properties in this store are NOT overwritten. This follows CSS
    /// cascade rules where child declarations override parent declarations.
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut parent = CssVariables::new();
    /// parent.set("--color", "blue");
    /// parent.set("--size", "16px");
    ///
    /// let mut child = CssVariables::new();
    /// child.set("--color", "red"); // Override parent
    ///
    /// child.inherit(&parent);
    /// assert_eq!(child.get("--color"), Some(&"red".to_string())); // Child wins
    /// assert_eq!(child.get("--size"), Some(&"16px".to_string())); // From parent
    /// ```
    pub fn inherit(&mut self, parent: &CssVariables) {
        for (name, value) in &parent.properties {
            // Don't overwrite existing properties (child takes precedence)
            self.properties.entry(name.clone()).or_insert_with(|| value.clone());
        }
    }

    /// Replaces all properties with those from a parent (full inheritance).
    ///
    /// This is useful at the start of style computation when you want to
    /// start fresh with inherited properties before applying local declarations.
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut parent = CssVariables::new();
    /// parent.set("--color", "blue");
    ///
    /// let mut child = CssVariables::new();
    /// child.inherit_all(&parent);
    /// assert_eq!(child.get("--color"), Some(&"blue".to_string()));
    /// ```
    pub fn inherit_all(&mut self, parent: &CssVariables) {
        self.properties = parent.properties.clone();
    }

    /// Sets a custom property value.
    ///
    /// The name should include the `--` prefix. If the property already
    /// exists, its value is replaced.
    ///
    /// # Arguments
    ///
    /// * `name` - The property name (e.g., "--primary-color")
    /// * `value` - The property value (e.g., "#007bff")
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut vars = CssVariables::new();
    /// vars.set("--color", "blue");
    /// assert_eq!(vars.get("--color"), Some(&"blue".to_string()));
    ///
    /// // Overwrite existing value
    /// vars.set("--color", "red");
    /// assert_eq!(vars.get("--color"), Some(&"red".to_string()));
    /// ```
    pub fn set(&mut self, name: &str, value: &str) {
        self.properties.insert(name.to_string(), value.to_string());
    }

    /// Sets a custom property value with owned strings.
    ///
    /// This is more efficient when you already have owned strings.
    ///
    /// # Arguments
    ///
    /// * `name` - The property name (e.g., "--primary-color")
    /// * `value` - The property value (e.g., "#007bff")
    pub fn set_owned(&mut self, name: String, value: String) {
        self.properties.insert(name, value);
    }

    /// Gets a custom property value.
    ///
    /// Returns `None` if the property is not defined.
    ///
    /// # Arguments
    ///
    /// * `name` - The property name (e.g., "--primary-color")
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut vars = CssVariables::new();
    /// vars.set("--color", "blue");
    ///
    /// assert_eq!(vars.get("--color"), Some(&"blue".to_string()));
    /// assert_eq!(vars.get("--undefined"), None);
    /// ```
    pub fn get(&self, name: &str) -> Option<&String> {
        self.properties.get(name)
    }

    /// Removes a custom property.
    ///
    /// Returns the value if it existed.
    ///
    /// # Arguments
    ///
    /// * `name` - The property name to remove
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut vars = CssVariables::new();
    /// vars.set("--color", "blue");
    /// assert_eq!(vars.remove("--color"), Some("blue".to_string()));
    /// assert_eq!(vars.get("--color"), None);
    /// ```
    pub fn remove(&mut self, name: &str) -> Option<String> {
        self.properties.remove(name)
    }

    /// Checks if a custom property is defined.
    ///
    /// # Arguments
    ///
    /// * `name` - The property name to check
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut vars = CssVariables::new();
    /// vars.set("--color", "blue");
    ///
    /// assert!(vars.contains("--color"));
    /// assert!(!vars.contains("--undefined"));
    /// ```
    pub fn contains(&self, name: &str) -> bool {
        self.properties.contains_key(name)
    }

    /// Returns the number of custom properties defined.
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut vars = CssVariables::new();
    /// assert_eq!(vars.len(), 0);
    ///
    /// vars.set("--color", "blue");
    /// assert_eq!(vars.len(), 1);
    /// ```
    pub fn len(&self) -> usize {
        self.properties.len()
    }

    /// Returns `true` if no custom properties are defined.
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut vars = CssVariables::new();
    /// assert!(vars.is_empty());
    ///
    /// vars.set("--color", "blue");
    /// assert!(!vars.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.properties.is_empty()
    }

    /// Clears all custom properties.
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut vars = CssVariables::new();
    /// vars.set("--color", "blue");
    /// vars.set("--size", "16px");
    ///
    /// vars.clear();
    /// assert!(vars.is_empty());
    /// ```
    pub fn clear(&mut self) {
        self.properties.clear();
    }

    /// Returns an iterator over all custom properties.
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut vars = CssVariables::new();
    /// vars.set("--color", "blue");
    /// vars.set("--size", "16px");
    ///
    /// for (name, value) in vars.iter() {
    ///     println!("{}: {}", name, value);
    /// }
    /// ```
    pub fn iter(&self) -> impl Iterator<Item = (&String, &String)> {
        self.properties.iter()
    }

    /// Returns an iterator over all property names.
    pub fn names(&self) -> impl Iterator<Item = &String> {
        self.properties.keys()
    }

    /// Returns an iterator over all property values.
    pub fn values(&self) -> impl Iterator<Item = &String> {
        self.properties.values()
    }

    /// Resolves a `var()` reference to its value.
    ///
    /// Handles:
    /// - Simple references: `var(--color)` -> property value
    /// - Fallback values: `var(--undefined, blue)` -> "blue"
    /// - Nested var(): `var(--primary, var(--fallback, red))`
    /// - Values containing var(): `10px var(--size) 20px`
    ///
    /// Returns `None` if the variable is not defined and no fallback is provided.
    ///
    /// # Arguments
    ///
    /// * `value` - The value potentially containing var() references
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut vars = CssVariables::new();
    /// vars.set("--color", "blue");
    /// vars.set("--size", "16px");
    ///
    /// // Simple resolution
    /// assert_eq!(vars.resolve("var(--color)"), Some("blue".to_string()));
    ///
    /// // With fallback
    /// assert_eq!(vars.resolve("var(--undefined, red)"), Some("red".to_string()));
    ///
    /// // Values with var() embedded
    /// assert_eq!(
    ///     vars.resolve("color: var(--color)"),
    ///     Some("color: blue".to_string())
    /// );
    ///
    /// // No var() - returns as-is
    /// assert_eq!(vars.resolve("blue"), Some("blue".to_string()));
    /// ```
    pub fn resolve(&self, value: &str) -> Option<String> {
        self.resolve_with_depth(value, 0)
    }

    /// Internal resolution with depth tracking to prevent infinite loops.
    fn resolve_with_depth(&self, value: &str, depth: usize) -> Option<String> {
        // Prevent infinite recursion from circular references
        if depth >= MAX_VAR_RESOLUTION_DEPTH {
            return None;
        }

        // Check if value contains var()
        if !value.contains("var(") {
            return Some(value.to_string());
        }

        let mut result = value.to_string();
        let mut iterations = 0;
        const MAX_ITERATIONS: usize = 100;

        // Keep resolving until no more var() references
        while result.contains("var(") {
            iterations += 1;
            if iterations > MAX_ITERATIONS {
                // Safety limit to prevent infinite loops
                return None;
            }

            if let Some(start) = result.find("var(") {
                // Find the matching closing paren
                if let Some(end) = self.find_matching_paren(&result[start..]) {
                    let var_expr = &result[start..=start + end];
                    let resolved = self.resolve_single_var(var_expr, depth)?;
                    result = format!("{}{}{}", &result[..start], resolved, &result[start + end + 1..]);
                } else {
                    // Malformed var() - missing closing paren
                    return None;
                }
            } else {
                break;
            }
        }

        // Recursively resolve in case the resolved value contains var()
        if result.contains("var(") {
            self.resolve_with_depth(&result, depth + 1)
        } else {
            Some(result)
        }
    }

    /// Finds the position of the matching closing paren for a var() expression.
    /// Returns the byte position (not character position) of the closing paren.
    fn find_matching_paren(&self, s: &str) -> Option<usize> {
        let mut depth = 0;
        let mut started = false;

        for (byte_pos, ch) in s.char_indices() {
            match ch {
                '(' => {
                    started = true;
                    depth += 1;
                }
                ')' => {
                    depth -= 1;
                    if depth == 0 && started {
                        return Some(byte_pos);
                    }
                }
                _ => {}
            }
        }

        None
    }

    /// Resolves a single var() expression (e.g., "var(--color)" or "var(--color, blue)").
    fn resolve_single_var(&self, var_expr: &str, depth: usize) -> Option<String> {
        // Extract content between "var(" and ")"
        if !var_expr.starts_with("var(") || !var_expr.ends_with(')') {
            return None;
        }

        let inner = &var_expr[4..var_expr.len() - 1];

        // Parse variable name and optional fallback
        // Handle nested var() in fallback by counting parens
        let (var_name, fallback) = self.parse_var_inner(inner);
        let var_name = var_name.trim();

        // Look up the variable
        if let Some(value) = self.properties.get(var_name) {
            // Recursively resolve in case value contains var()
            self.resolve_with_depth(value, depth + 1)
        } else if let Some(fb) = fallback {
            // Use fallback and resolve it
            self.resolve_with_depth(fb.trim(), depth + 1)
        } else {
            // No value and no fallback
            None
        }
    }

    /// Parses the inner content of a var() expression.
    ///
    /// Returns (variable_name, fallback_option).
    fn parse_var_inner<'a>(&self, inner: &'a str) -> (&'a str, Option<&'a str>) {
        // Find the first comma that's not inside nested parens
        let mut depth = 0;

        for (byte_pos, ch) in inner.char_indices() {
            match ch {
                '(' => depth += 1,
                ')' => depth -= 1,
                ',' if depth == 0 => {
                    let var_name = &inner[..byte_pos];
                    let fallback = &inner[byte_pos + 1..];
                    return (var_name, Some(fallback));
                }
                _ => {}
            }
        }

        // No comma found - just a variable name
        (inner, None)
    }

    /// Checks if a value contains any var() references.
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// assert!(CssVariables::has_var_reference("color: var(--primary)"));
    /// assert!(!CssVariables::has_var_reference("color: blue"));
    /// ```
    pub fn has_var_reference(value: &str) -> bool {
        value.contains("var(")
    }

    /// Validates a custom property name.
    ///
    /// A valid custom property name must:
    /// - Start with two dashes (`--`)
    /// - Contain at least one character after the dashes
    /// - Not contain invalid characters
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// assert!(CssVariables::is_valid_name("--color"));
    /// assert!(CssVariables::is_valid_name("--primary-color"));
    /// assert!(CssVariables::is_valid_name("--my_var_123"));
    ///
    /// assert!(!CssVariables::is_valid_name("-color")); // Only one dash
    /// assert!(!CssVariables::is_valid_name("color")); // No dashes
    /// assert!(!CssVariables::is_valid_name("--")); // No name after dashes
    /// ```
    pub fn is_valid_name(name: &str) -> bool {
        if !name.starts_with("--") {
            return false;
        }

        let rest = &name[2..];
        if rest.is_empty() {
            return false;
        }

        // CSS custom property names can contain most characters
        // But we'll be permissive here and just check for basic validity
        true
    }

    /// Extracts the variable name from a var() expression.
    ///
    /// Returns `None` if the expression is not a valid var() reference.
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// assert_eq!(
    ///     CssVariables::extract_var_name("var(--color)"),
    ///     Some("--color".to_string())
    /// );
    /// assert_eq!(
    ///     CssVariables::extract_var_name("var(--color, blue)"),
    ///     Some("--color".to_string())
    /// );
    /// assert_eq!(CssVariables::extract_var_name("blue"), None);
    /// ```
    pub fn extract_var_name(value: &str) -> Option<String> {
        if !value.starts_with("var(") || !value.ends_with(')') {
            return None;
        }

        let inner = &value[4..value.len() - 1];

        // Find the first comma (fallback separator)
        let var_name = if let Some(comma_pos) = inner.find(',') {
            &inner[..comma_pos]
        } else {
            inner
        };

        Some(var_name.trim().to_string())
    }

    /// Merges another CssVariables store into this one.
    ///
    /// Properties from `other` overwrite properties in `self`.
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut vars1 = CssVariables::new();
    /// vars1.set("--color", "blue");
    /// vars1.set("--size", "16px");
    ///
    /// let mut vars2 = CssVariables::new();
    /// vars2.set("--color", "red"); // Will overwrite
    /// vars2.set("--font", "Arial");
    ///
    /// vars1.merge(&vars2);
    ///
    /// assert_eq!(vars1.get("--color"), Some(&"red".to_string()));
    /// assert_eq!(vars1.get("--size"), Some(&"16px".to_string()));
    /// assert_eq!(vars1.get("--font"), Some(&"Arial".to_string()));
    /// ```
    pub fn merge(&mut self, other: &CssVariables) {
        for (name, value) in &other.properties {
            self.properties.insert(name.clone(), value.clone());
        }
    }

    /// Creates a new CssVariables with properties from both stores.
    ///
    /// Properties from `other` take precedence over `self`.
    ///
    /// # Example
    ///
    /// ```
    /// use fastrender::style::variables::CssVariables;
    ///
    /// let mut parent = CssVariables::new();
    /// parent.set("--color", "blue");
    ///
    /// let mut child = CssVariables::new();
    /// child.set("--color", "red");
    /// child.set("--size", "16px");
    ///
    /// let combined = parent.combined_with(&child);
    /// assert_eq!(combined.get("--color"), Some(&"red".to_string()));
    /// assert_eq!(combined.get("--size"), Some(&"16px".to_string()));
    /// ```
    pub fn combined_with(&self, other: &CssVariables) -> CssVariables {
        let mut result = self.clone();
        result.merge(other);
        result
    }

    /// Provides direct access to the underlying HashMap.
    ///
    /// This is useful for advanced use cases or integration with existing code.
    pub fn as_map(&self) -> &HashMap<String, String> {
        &self.properties
    }

    /// Consumes self and returns the underlying HashMap.
    pub fn into_map(self) -> HashMap<String, String> {
        self.properties
    }

    /// Creates from an existing HashMap.
    pub fn from_map(properties: HashMap<String, String>) -> Self {
        Self { properties }
    }
}

impl From<HashMap<String, String>> for CssVariables {
    fn from(properties: HashMap<String, String>) -> Self {
        Self { properties }
    }
}

impl IntoIterator for CssVariables {
    type Item = (String, String);
    type IntoIter = std::collections::hash_map::IntoIter<String, String>;

    fn into_iter(self) -> Self::IntoIter {
        self.properties.into_iter()
    }
}

impl<'a> IntoIterator for &'a CssVariables {
    type Item = (&'a String, &'a String);
    type IntoIter = std::collections::hash_map::Iter<'a, String, String>;

    fn into_iter(self) -> Self::IntoIter {
        self.properties.iter()
    }
}

impl Extend<(String, String)> for CssVariables {
    fn extend<T: IntoIterator<Item = (String, String)>>(&mut self, iter: T) {
        self.properties.extend(iter);
    }
}

impl FromIterator<(String, String)> for CssVariables {
    fn from_iter<T: IntoIterator<Item = (String, String)>>(iter: T) -> Self {
        Self {
            properties: iter.into_iter().collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_and_empty() {
        let vars = CssVariables::new();
        assert!(vars.is_empty());
        assert_eq!(vars.len(), 0);
    }

    #[test]
    fn test_set_and_get() {
        let mut vars = CssVariables::new();
        vars.set("--color", "blue");
        assert_eq!(vars.get("--color"), Some(&"blue".to_string()));
        assert_eq!(vars.get("--undefined"), None);
    }

    #[test]
    fn test_set_overwrites() {
        let mut vars = CssVariables::new();
        vars.set("--color", "blue");
        vars.set("--color", "red");
        assert_eq!(vars.get("--color"), Some(&"red".to_string()));
    }

    #[test]
    fn test_remove() {
        let mut vars = CssVariables::new();
        vars.set("--color", "blue");
        assert_eq!(vars.remove("--color"), Some("blue".to_string()));
        assert_eq!(vars.get("--color"), None);
    }

    #[test]
    fn test_contains() {
        let mut vars = CssVariables::new();
        vars.set("--color", "blue");
        assert!(vars.contains("--color"));
        assert!(!vars.contains("--undefined"));
    }

    #[test]
    fn test_clear() {
        let mut vars = CssVariables::new();
        vars.set("--color", "blue");
        vars.set("--size", "16px");
        vars.clear();
        assert!(vars.is_empty());
    }

    #[test]
    fn test_inherit_from() {
        let mut parent = CssVariables::new();
        parent.set("--color", "blue");
        parent.set("--size", "16px");

        let child = CssVariables::inherit_from(&parent);
        assert_eq!(child.get("--color"), Some(&"blue".to_string()));
        assert_eq!(child.get("--size"), Some(&"16px".to_string()));
    }

    #[test]
    fn test_inherit() {
        let mut parent = CssVariables::new();
        parent.set("--color", "blue");
        parent.set("--size", "16px");

        let mut child = CssVariables::new();
        child.set("--color", "red"); // Child overrides

        child.inherit(&parent);
        assert_eq!(child.get("--color"), Some(&"red".to_string())); // Child wins
        assert_eq!(child.get("--size"), Some(&"16px".to_string())); // From parent
    }

    #[test]
    fn test_inherit_all() {
        let mut parent = CssVariables::new();
        parent.set("--color", "blue");

        let mut child = CssVariables::new();
        child.set("--old", "value");
        child.inherit_all(&parent);

        assert_eq!(child.get("--color"), Some(&"blue".to_string()));
        assert_eq!(child.get("--old"), None); // Replaced
    }

    #[test]
    fn test_resolve_simple() {
        let mut vars = CssVariables::new();
        vars.set("--color", "blue");
        assert_eq!(vars.resolve("var(--color)"), Some("blue".to_string()));
    }

    #[test]
    fn test_resolve_with_fallback() {
        let vars = CssVariables::new();
        assert_eq!(vars.resolve("var(--undefined, red)"), Some("red".to_string()));
    }

    #[test]
    fn test_resolve_undefined_no_fallback() {
        let vars = CssVariables::new();
        assert_eq!(vars.resolve("var(--undefined)"), None);
    }

    #[test]
    fn test_resolve_embedded() {
        let mut vars = CssVariables::new();
        vars.set("--color", "blue");
        assert_eq!(vars.resolve("color: var(--color)"), Some("color: blue".to_string()));
    }

    #[test]
    fn test_resolve_multiple_vars() {
        let mut vars = CssVariables::new();
        vars.set("--x", "10px");
        vars.set("--y", "20px");
        assert_eq!(
            vars.resolve("translate(var(--x), var(--y))"),
            Some("translate(10px, 20px)".to_string())
        );
    }

    #[test]
    fn test_resolve_nested_var_in_fallback() {
        let mut vars = CssVariables::new();
        vars.set("--fallback", "green");
        assert_eq!(
            vars.resolve("var(--undefined, var(--fallback))"),
            Some("green".to_string())
        );
    }

    #[test]
    fn test_resolve_nested_var_value() {
        let mut vars = CssVariables::new();
        vars.set("--primary", "var(--secondary)");
        vars.set("--secondary", "blue");
        assert_eq!(vars.resolve("var(--primary)"), Some("blue".to_string()));
    }

    #[test]
    fn test_resolve_no_var() {
        let vars = CssVariables::new();
        assert_eq!(vars.resolve("blue"), Some("blue".to_string()));
    }

    #[test]
    fn test_resolve_circular_reference() {
        let mut vars = CssVariables::new();
        vars.set("--a", "var(--b)");
        vars.set("--b", "var(--a)");
        // Should return None due to max depth limit
        assert_eq!(vars.resolve("var(--a)"), None);
    }

    #[test]
    fn test_has_var_reference() {
        assert!(CssVariables::has_var_reference("color: var(--primary)"));
        assert!(!CssVariables::has_var_reference("color: blue"));
    }

    #[test]
    fn test_is_valid_name() {
        assert!(CssVariables::is_valid_name("--color"));
        assert!(CssVariables::is_valid_name("--primary-color"));
        assert!(CssVariables::is_valid_name("--my_var_123"));
        assert!(!CssVariables::is_valid_name("-color"));
        assert!(!CssVariables::is_valid_name("color"));
        assert!(!CssVariables::is_valid_name("--"));
    }

    #[test]
    fn test_extract_var_name() {
        assert_eq!(
            CssVariables::extract_var_name("var(--color)"),
            Some("--color".to_string())
        );
        assert_eq!(
            CssVariables::extract_var_name("var(--color, blue)"),
            Some("--color".to_string())
        );
        assert_eq!(CssVariables::extract_var_name("blue"), None);
    }

    #[test]
    fn test_merge() {
        let mut vars1 = CssVariables::new();
        vars1.set("--color", "blue");
        vars1.set("--size", "16px");

        let mut vars2 = CssVariables::new();
        vars2.set("--color", "red");
        vars2.set("--font", "Arial");

        vars1.merge(&vars2);

        assert_eq!(vars1.get("--color"), Some(&"red".to_string()));
        assert_eq!(vars1.get("--size"), Some(&"16px".to_string()));
        assert_eq!(vars1.get("--font"), Some(&"Arial".to_string()));
    }

    #[test]
    fn test_combined_with() {
        let mut parent = CssVariables::new();
        parent.set("--color", "blue");

        let mut child = CssVariables::new();
        child.set("--color", "red");
        child.set("--size", "16px");

        let combined = parent.combined_with(&child);
        assert_eq!(combined.get("--color"), Some(&"red".to_string()));
        assert_eq!(combined.get("--size"), Some(&"16px".to_string()));
    }

    #[test]
    fn test_from_map() {
        let mut map = HashMap::new();
        map.insert("--color".to_string(), "blue".to_string());

        let vars = CssVariables::from_map(map);
        assert_eq!(vars.get("--color"), Some(&"blue".to_string()));
    }

    #[test]
    fn test_into_map() {
        let mut vars = CssVariables::new();
        vars.set("--color", "blue");

        let map = vars.into_map();
        assert_eq!(map.get("--color"), Some(&"blue".to_string()));
    }

    #[test]
    fn test_iter() {
        let mut vars = CssVariables::new();
        vars.set("--color", "blue");
        vars.set("--size", "16px");

        let collected: HashMap<_, _> = vars.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
        assert_eq!(collected.len(), 2);
        assert_eq!(collected.get("--color"), Some(&"blue".to_string()));
    }

    #[test]
    fn test_from_iterator() {
        let items = vec![
            ("--color".to_string(), "blue".to_string()),
            ("--size".to_string(), "16px".to_string()),
        ];

        let vars: CssVariables = items.into_iter().collect();
        assert_eq!(vars.len(), 2);
        assert_eq!(vars.get("--color"), Some(&"blue".to_string()));
    }

    #[test]
    fn test_extend() {
        let mut vars = CssVariables::new();
        vars.set("--color", "blue");

        vars.extend(vec![("--size".to_string(), "16px".to_string())]);

        assert_eq!(vars.len(), 2);
        assert_eq!(vars.get("--size"), Some(&"16px".to_string()));
    }

    #[test]
    fn test_default() {
        let vars = CssVariables::default();
        assert!(vars.is_empty());
    }

    #[test]
    fn test_clone() {
        let mut vars = CssVariables::new();
        vars.set("--color", "blue");

        let cloned = vars.clone();
        assert_eq!(cloned.get("--color"), Some(&"blue".to_string()));
    }

    #[test]
    fn test_eq() {
        let mut vars1 = CssVariables::new();
        vars1.set("--color", "blue");

        let mut vars2 = CssVariables::new();
        vars2.set("--color", "blue");

        assert_eq!(vars1, vars2);
    }

    #[test]
    fn test_with_capacity() {
        let vars = CssVariables::with_capacity(10);
        assert!(vars.is_empty());
    }

    #[test]
    fn test_set_owned() {
        let mut vars = CssVariables::new();
        vars.set_owned("--color".to_string(), "blue".to_string());
        assert_eq!(vars.get("--color"), Some(&"blue".to_string()));
    }

    #[test]
    fn test_resolve_fallback_with_spaces() {
        let vars = CssVariables::new();
        assert_eq!(vars.resolve("var(--undefined,   red   )"), Some("red".to_string()));
    }

    #[test]
    fn test_resolve_complex_fallback() {
        let vars = CssVariables::new();
        assert_eq!(
            vars.resolve("var(--undefined, 10px 20px 30px)"),
            Some("10px 20px 30px".to_string())
        );
    }

    #[test]
    fn test_deep_nesting() {
        let mut vars = CssVariables::new();
        vars.set("--a", "var(--b)");
        vars.set("--b", "var(--c)");
        vars.set("--c", "var(--d)");
        vars.set("--d", "final");
        assert_eq!(vars.resolve("var(--a)"), Some("final".to_string()));
    }
}
