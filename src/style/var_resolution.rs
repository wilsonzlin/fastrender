//! CSS Custom Property (var()) Resolution
//!
//! This module implements the resolution of CSS `var()` function references
//! as specified in CSS Custom Properties for Cascading Variables Module Level 1.
//!
//! The `var()` function is used to substitute the value of a custom property
//! (also known as a CSS variable) into another property value.
//!
//! # Syntax
//!
//! ```text
//! var( <custom-property-name> , <declaration-value>? )
//! ```
//!
//! Where:
//! - `<custom-property-name>` is the name of the custom property (e.g., `--color-primary`)
//! - `<declaration-value>` is an optional fallback value used when the custom property is not defined
//!
//! # Examples
//!
//! ```text
//! color: var(--text-color);
//! padding: var(--spacing, 10px);
//! background: var(--theme-bg, var(--fallback-bg, white));
//! ```
//!
//! # Reference
//!
//! CSS Custom Properties for Cascading Variables Module Level 1
//! <https://www.w3.org/TR/css-variables-1/>

use crate::css::properties::parse_property_value;
use crate::css::types::PropertyValue;
use std::collections::HashMap;

/// Maximum depth for recursive var() resolution to prevent infinite loops
const MAX_RECURSION_DEPTH: usize = 10;

/// Result of a var() resolution attempt
#[derive(Debug, Clone)]
pub enum VarResolutionResult {
    /// Successfully resolved to a value
    Resolved(Box<PropertyValue>),
    /// The variable was not found and no fallback was provided
    NotFound(String),
    /// Recursion depth exceeded (possible circular reference)
    RecursionLimitExceeded,
    /// Invalid var() syntax
    InvalidSyntax(String),
}

impl VarResolutionResult {
    /// Returns the resolved value if successful, otherwise returns the original value
    pub fn unwrap_or(self, default: PropertyValue) -> PropertyValue {
        match self {
            VarResolutionResult::Resolved(value) => *value,
            _ => default,
        }
    }

    /// Returns true if the resolution was successful
    pub fn is_resolved(&self) -> bool {
        matches!(self, VarResolutionResult::Resolved(_))
    }
}

/// Resolves CSS `var()` function references in a property value
///
/// This is the main entry point for var() resolution. It handles:
/// - Simple var(--name) references
/// - Fallback values: var(--name, fallback)
/// - Nested var() references in fallback values
/// - var() embedded within other CSS functions (e.g., calc())
///
/// # Arguments
///
/// * `value` - The PropertyValue that may contain var() references
/// * `custom_properties` - Map of custom property names to their string values
///
/// # Returns
///
/// The resolved PropertyValue with all var() references substituted
///
/// # Examples
///
/// ```ignore
/// use std::collections::HashMap;
/// use fastrender::css::PropertyValue;
/// use fastrender::style::var_resolution::resolve_var;
///
/// let mut props = HashMap::new();
/// props.insert("--color".to_string(), "#ff0000".to_string());
///
/// let value = PropertyValue::Keyword("var(--color)".to_string());
/// let resolved = resolve_var(&value, &props);
/// // resolved is now PropertyValue::Color(...)
/// ```
/// Resolves CSS `var()` references using the provided custom properties.
///
/// This helper performs property-agnostic resolution (parses fallback/results without knowing
/// the destination property). For property-aware parsing, use `resolve_var_for_property`.
pub fn resolve_var(value: &PropertyValue, custom_properties: &HashMap<String, String>) -> PropertyValue {
    match resolve_var_recursive(value, custom_properties, 0, "") {
        VarResolutionResult::Resolved(v) => *v,
        other => other.unwrap_or(value.clone()),
    }
}

/// Resolves CSS `var()` references with knowledge of the target property.
///
/// Passing the property name allows the resolver to parse the substituted value using the
/// appropriate grammar (e.g., background layers with commas), rather than the generic parser.
pub fn resolve_var_for_property(
    value: &PropertyValue,
    custom_properties: &HashMap<String, String>,
    property_name: &str,
) -> VarResolutionResult {
    resolve_var_recursive(value, custom_properties, 0, property_name)
}

/// Resolves var() references with explicit depth tracking
///
/// This function is useful when you need to track the recursion depth,
/// for example when implementing custom resolution strategies.
///
/// # Arguments
///
/// * `value` - The PropertyValue to resolve
/// * `custom_properties` - Map of custom property names to values
/// * `depth` - Current recursion depth (starts at 0)
///
/// # Returns
///
/// The resolved PropertyValue, or the original value if resolution fails
pub fn resolve_var_with_depth(
    value: &PropertyValue,
    custom_properties: &HashMap<String, String>,
    depth: usize,
) -> PropertyValue {
    match resolve_var_recursive(value, custom_properties, depth, "") {
        VarResolutionResult::Resolved(v) => *v,
        other => other.unwrap_or(value.clone()),
    }
}

/// Internal recursive implementation of var() resolution
fn resolve_var_recursive(
    value: &PropertyValue,
    custom_properties: &HashMap<String, String>,
    depth: usize,
    property_name: &str,
) -> VarResolutionResult {
    // Check recursion limit to prevent infinite loops
    if depth >= MAX_RECURSION_DEPTH {
        return VarResolutionResult::RecursionLimitExceeded;
    }

    match value {
        PropertyValue::Keyword(kw) => resolve_keyword_var(kw, custom_properties, depth, property_name),
        _ => VarResolutionResult::Resolved(Box::new(value.clone())),
    }
}

/// Resolves var() references within a keyword/string value
fn resolve_keyword_var(
    kw: &str,
    custom_properties: &HashMap<String, String>,
    depth: usize,
    property_name: &str,
) -> VarResolutionResult {
    // Check if it's a simple var() reference (starts with "var(" and ends with ")")
    if kw.starts_with("var(") && kw.ends_with(')') && !kw[4..kw.len() - 1].contains("var(") {
        let inner = &kw[4..kw.len() - 1];
        return resolve_simple_var(inner, custom_properties, depth, property_name);
    }

    // Check if the string contains any var() references
    if kw.contains("var(") {
        let resolved_str = resolve_embedded_vars(kw, custom_properties, depth);
        // Try to parse the resolved string as a proper CSS value
        if let Some(parsed) = parse_resolved_value(&resolved_str, property_name) {
            // Recursively resolve in case the result contains more var() references
            return resolve_var_recursive(&parsed, custom_properties, depth + 1, property_name);
        }
        return VarResolutionResult::Resolved(Box::new(PropertyValue::Keyword(resolved_str)));
    }

    VarResolutionResult::Resolved(Box::new(PropertyValue::Keyword(kw.to_string())))
}

/// Resolves a simple var() reference (content inside var(...))
///
/// Handles: var(--name) and var(--name, fallback)
fn resolve_simple_var(
    inner: &str,
    custom_properties: &HashMap<String, String>,
    depth: usize,
    property_name: &str,
) -> VarResolutionResult {
    // Split by first comma to separate variable name from fallback
    let (var_name, fallback) = split_var_args(inner);
    let var_name = var_name.trim();

    // Look up the variable
    if let Some(resolved_value) = custom_properties.get(var_name) {
        // Try to parse the resolved value as a CSS property value
        if let Some(mut parsed) = parse_resolved_value(resolved_value, property_name) {
            // Recursively resolve in case the value contains another var()
            parsed = resolve_var_recursive(&parsed, custom_properties, depth + 1, property_name).unwrap_or(parsed);
            return VarResolutionResult::Resolved(Box::new(parsed));
        }
        // If parsing fails, return as keyword and try to resolve recursively
        let as_keyword = PropertyValue::Keyword(resolved_value.clone());
        return resolve_var_recursive(&as_keyword, custom_properties, depth + 1, property_name);
    }

    // Variable not found - try fallback
    if let Some(fallback_str) = fallback {
        // The fallback may itself contain var() references
        let resolved_fallback = resolve_embedded_vars(fallback_str, custom_properties, depth + 1);

        // If the fallback still contains unresolved var() calls, the declaration is invalid.
        if resolved_fallback.contains("var(") {
            return VarResolutionResult::NotFound(resolved_fallback);
        }

        if let Some(mut parsed) = parse_resolved_value(&resolved_fallback, property_name) {
            parsed = resolve_var_recursive(&parsed, custom_properties, depth + 1, property_name).unwrap_or(parsed);
            return VarResolutionResult::Resolved(Box::new(parsed));
        }
        let as_keyword = PropertyValue::Keyword(resolved_fallback);
        return resolve_var_recursive(&as_keyword, custom_properties, depth + 1, property_name);
    }

    // No variable found and no fallback - return original
    VarResolutionResult::NotFound(inner.to_string())
}

/// Splits var() arguments into variable name and optional fallback
///
/// Handles nested parentheses properly so that fallbacks containing
/// functions like calc() or nested var() are handled correctly.
fn split_var_args(inner: &str) -> (&str, Option<&str>) {
    let mut paren_depth = 0;

    for (i, ch) in inner.char_indices() {
        match ch {
            '(' => paren_depth += 1,
            ')' => paren_depth -= 1,
            ',' if paren_depth == 0 => {
                let var_name = &inner[..i];
                let fallback = &inner[i + 1..];
                return (var_name, Some(fallback.trim()));
            }
            _ => {}
        }
    }

    (inner, None)
}

/// Resolves var() references embedded within a larger string
///
/// For example: "calc(var(--size) + 10px)" or "0 0 var(--shadow-blur) var(--shadow-color)"
fn resolve_embedded_vars(input: &str, custom_properties: &HashMap<String, String>, depth: usize) -> String {
    // Check recursion limit to prevent infinite loops
    if depth >= MAX_RECURSION_DEPTH {
        return input.to_string();
    }

    let mut result = input.to_string();
    let mut safety_limit = 100; // Additional safety limit for while loop

    while result.contains("var(") && safety_limit > 0 {
        safety_limit -= 1;

        // Find the next var() occurrence
        if let Some(start) = result.find("var(") {
            // Find the matching closing paren
            if let Some(end) = find_matching_paren(&result[start + 4..]) {
                let end_index = start + 4 + end;
                let var_call = &result[start..=end_index].to_string();
                let var_inner = &result[start + 4..end_index];

                // Resolve this var() call
                let (var_name, fallback) = split_var_args(var_inner);
                let var_name = var_name.trim();

                let replacement = if let Some(resolved) = custom_properties.get(var_name) {
                    resolved.clone()
                } else if let Some(fb) = fallback {
                    // Recursively resolve the fallback in case it contains var()
                    resolve_embedded_vars(fb, custom_properties, depth + 1)
                } else {
                    // Can't resolve - keep the original var() call
                    break;
                };

                result = result.replace(var_call, &replacement);
            } else {
                // Malformed var() - can't find closing paren
                break;
            }
        } else {
            break;
        }
    }

    result
}

/// Finds the index of the matching closing parenthesis
///
/// Returns the index relative to the start of the input string
fn find_matching_paren(s: &str) -> Option<usize> {
    let mut depth = 0;

    for (i, ch) in s.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => {
                if depth == 0 {
                    return Some(i);
                }
                depth -= 1;
            }
            _ => {}
        }
    }

    None
}

/// Parses a resolved CSS variable value into a PropertyValue
///
/// This function attempts to parse the string representation of a CSS value
/// (as stored in custom properties) into a typed PropertyValue.
fn parse_resolved_value(value: &str, property_name: &str) -> Option<PropertyValue> {
    // Property-agnostic resolution keeps the value mostly untyped, but still recognizes numeric lengths.
    if property_name.is_empty() {
        let trimmed = value.trim();
        if let Some(len) = crate::css::properties::parse_length(trimmed) {
            return Some(PropertyValue::Length(len));
        }
        if let Ok(num) = trimmed.parse::<f32>() {
            return Some(PropertyValue::Number(num));
        }
        if trimmed.ends_with('%') {
            if let Ok(num) = trimmed[..trimmed.len() - 1].parse::<f32>() {
                return Some(PropertyValue::Percentage(num));
            }
        }
        return Some(PropertyValue::Keyword(trimmed.to_string()));
    }
    // Use the CSS parser to interpret the value
    parse_property_value(property_name, value)
}

/// Checks if a string contains any var() references
///
/// # Examples
///
/// ```ignore
/// assert!(contains_var("var(--color)"));
/// assert!(contains_var("calc(var(--size) + 10px)"));
/// assert!(!contains_var("10px"));
/// ```
pub fn contains_var(value: &str) -> bool {
    value.contains("var(")
}

/// Extracts all custom property names referenced in a value
///
/// This is useful for dependency tracking and determining which
/// properties need to be resolved before others.
///
/// # Examples
///
/// ```ignore
/// let refs = extract_var_references("calc(var(--size) + var(--margin))");
/// assert_eq!(refs, vec!["--size", "--margin"]);
/// ```
pub fn extract_var_references(value: &str) -> Vec<String> {
    let mut refs = Vec::new();
    let mut remaining = value;

    while let Some(start) = remaining.find("var(") {
        let after_var = &remaining[start + 4..];
        if let Some(end) = find_matching_paren(after_var) {
            let inner = &after_var[..end];
            let (var_name, fallback) = split_var_args(inner);
            let var_name = var_name.trim();

            if var_name.starts_with("--") {
                refs.push(var_name.to_string());
            }

            // Also check the fallback for nested var() references
            if let Some(fb) = fallback {
                refs.extend(extract_var_references(fb));
            }

            remaining = &after_var[end..];
        } else {
            break;
        }
    }

    refs
}

/// Validates that a custom property name follows CSS naming rules
///
/// Custom property names must start with two dashes (--) and can contain
/// letters, digits, underscores, and hyphens.
///
/// # Examples
///
/// ```ignore
/// assert!(is_valid_custom_property_name("--color-primary"));
/// assert!(is_valid_custom_property_name("--_internal"));
/// assert!(!is_valid_custom_property_name("color")); // Missing --
/// ```
pub fn is_valid_custom_property_name(name: &str) -> bool {
    if !name.starts_with("--") {
        return false;
    }

    if name.len() <= 2 {
        return false; // Just "--" is not valid
    }

    // The rest can be any character except whitespace
    // (CSS spec allows almost any character in custom property names)
    !name[2..].chars().any(char::is_whitespace)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::values::Length;
    use crate::style::values::LengthUnit;

    fn make_props(pairs: &[(&str, &str)]) -> HashMap<String, String> {
        pairs
            .iter()
            .map(|(k, v)| ((*k).to_string(), (*v).to_string()))
            .collect()
    }

    // Basic var() resolution tests
    #[test]
    fn test_resolve_simple_var() {
        let props = make_props(&[("--color", "#ff0000")]);
        let value = PropertyValue::Keyword("var(--color)".to_string());
        let resolved = resolve_var(&value, &props);

        // Should resolve to a color
        matches!(resolved, PropertyValue::Color(_));
    }

    #[test]
    fn test_resolve_var_with_length() {
        let props = make_props(&[("--size", "16px")]);
        let value = PropertyValue::Keyword("var(--size)".to_string());
        let resolved = resolve_var(&value, &props);

        if let PropertyValue::Length(len) = resolved {
            assert_eq!(len.value, 16.0);
        } else {
            panic!("Expected Length, got {:?}", resolved);
        }
    }

    #[test]
    fn test_resolve_var_not_found() {
        let props = HashMap::new();
        let value = PropertyValue::Keyword("var(--missing)".to_string());
        let resolved = resolve_var(&value, &props);

        // Should return the original var() call
        if let PropertyValue::Keyword(kw) = resolved {
            assert!(kw.contains("var(--missing)"));
        } else {
            panic!("Expected Keyword, got {:?}", resolved);
        }
    }

    // Fallback value tests
    #[test]
    fn test_resolve_var_with_fallback_not_needed() {
        let props = make_props(&[("--color", "blue")]);
        let value = PropertyValue::Keyword("var(--color, red)".to_string());
        let resolved = resolve_var(&value, &props);

        if let PropertyValue::Keyword(kw) = resolved {
            assert_eq!(kw, "blue");
        } else {
            panic!("Expected Keyword, got {:?}", resolved);
        }
    }

    #[test]
    fn test_resolve_var_with_fallback_used() {
        let props = HashMap::new();
        let value = PropertyValue::Keyword("var(--missing, red)".to_string());
        let resolved = resolve_var(&value, &props);

        if let PropertyValue::Keyword(kw) = resolved {
            assert_eq!(kw, "red");
        } else {
            panic!("Expected Keyword 'red', got {:?}", resolved);
        }
    }

    #[test]
    fn test_resolve_var_with_fallback_length() {
        let props = HashMap::new();
        let value = PropertyValue::Keyword("var(--spacing, 10px)".to_string());
        let resolved = resolve_var(&value, &props);

        if let PropertyValue::Length(len) = resolved {
            assert_eq!(len.value, 10.0);
        } else {
            panic!("Expected Length, got {:?}", resolved);
        }
    }

    // Nested var() tests
    #[test]
    fn test_resolve_nested_var_in_fallback() {
        let props = make_props(&[("--fallback-color", "green")]);
        let value = PropertyValue::Keyword("var(--color, var(--fallback-color))".to_string());
        let resolved = resolve_var(&value, &props);

        if let PropertyValue::Keyword(kw) = resolved {
            assert_eq!(kw, "green");
        } else {
            panic!("Expected Keyword 'green', got {:?}", resolved);
        }
    }

    #[test]
    fn test_resolve_chained_vars() {
        let props = make_props(&[("--primary", "var(--base)"), ("--base", "#0000ff")]);
        let value = PropertyValue::Keyword("var(--primary)".to_string());
        let resolved = resolve_var(&value, &props);

        // Should resolve through the chain
        matches!(resolved, PropertyValue::Color(_));
    }

    // Embedded var() tests
    #[test]
    fn test_resolve_embedded_var_in_calc() {
        let props = make_props(&[("--size", "10px")]);
        let value = PropertyValue::Keyword("calc(var(--size) + 5px)".to_string());
        let resolved = resolve_var(&value, &props);

        assert!(
            matches!(resolved, PropertyValue::Length(len) if (len.value - 15.0).abs() < f32::EPSILON && len.unit == LengthUnit::Px),
            "Expected resolved calc length, got {:?}",
            resolved
        );
    }

    #[test]
    fn test_resolve_multiple_embedded_vars() {
        let props = make_props(&[("--x", "10px"), ("--y", "20px")]);
        let value = PropertyValue::Keyword("var(--x) var(--y)".to_string());
        let resolved = resolve_var(&value, &props);

        if let PropertyValue::Keyword(kw) = resolved {
            assert!(kw.contains("10px"));
            assert!(kw.contains("20px"));
        } else {
            panic!("Expected Keyword, got {:?}", resolved);
        }
    }

    #[test]
    fn test_resolve_var_uses_property_specific_parser() {
        let props = make_props(&[("--bg", "url(image.png), linear-gradient(red, blue)")]);
        let value = PropertyValue::Keyword("var(--bg)".to_string());
        let resolved = resolve_var_for_property(&value, &props, "background-image");

        if let VarResolutionResult::Resolved(boxed) = resolved {
            let list = match *boxed {
                PropertyValue::Multiple(list) => list,
                _ => panic!("Expected Multiple for background layers, got {:?}", boxed),
            };
            assert_eq!(list.len(), 3); // url, comma token, gradient
            assert!(matches!(list[0], PropertyValue::Url(ref u) if u == "image.png"));
            assert!(matches!(
                list[2],
                PropertyValue::LinearGradient { .. } | PropertyValue::RepeatingLinearGradient { .. }
            ));
        } else {
            panic!("Expected Multiple for background layers, got {:?}", resolved);
        }
    }

    #[test]
    fn unresolved_var_marks_declaration_invalid() {
        let props = HashMap::new();
        let value = PropertyValue::Keyword("var(--missing)".to_string());
        let resolved = resolve_var_for_property(&value, &props, "color");
        assert!(matches!(resolved, VarResolutionResult::NotFound(_)));
    }

    #[test]
    fn unresolved_fallback_var_marks_declaration_invalid() {
        let props = make_props(&[("--fallback", "var(--still-missing)")]);
        let value = PropertyValue::Keyword("var(--missing, var(--fallback))".to_string());
        let resolved = resolve_var_for_property(&value, &props, "color");
        assert!(matches!(resolved, VarResolutionResult::NotFound(_)));
    }

    // Recursion limit tests
    #[test]
    fn test_recursion_limit() {
        // Create a circular reference
        let props = make_props(&[
            ("--a", "var(--b)"),
            ("--b", "var(--c)"),
            ("--c", "var(--a)"), // Circular!
        ]);
        let value = PropertyValue::Keyword("var(--a)".to_string());

        // Should not stack overflow - recursion limit should kick in
        let _resolved = resolve_var(&value, &props);
        // If we get here without panicking, the test passes
    }

    // Utility function tests
    #[test]
    fn test_contains_var() {
        assert!(contains_var("var(--color)"));
        assert!(contains_var("calc(var(--size) + 10px)"));
        assert!(contains_var("0 0 var(--blur) black"));
        assert!(!contains_var("10px"));
        assert!(!contains_var("red"));
        assert!(!contains_var(""));
    }

    #[test]
    fn test_extract_var_references() {
        let refs = extract_var_references("var(--color)");
        assert_eq!(refs, vec!["--color"]);

        let refs = extract_var_references("calc(var(--size) + var(--margin))");
        assert_eq!(refs, vec!["--size", "--margin"]);

        let refs = extract_var_references("var(--x, var(--y))");
        assert_eq!(refs, vec!["--x", "--y"]);

        let refs = extract_var_references("10px");
        assert!(refs.is_empty());
    }

    #[test]
    fn test_is_valid_custom_property_name() {
        assert!(is_valid_custom_property_name("--color"));
        assert!(is_valid_custom_property_name("--color-primary"));
        assert!(is_valid_custom_property_name("--_internal"));
        assert!(is_valid_custom_property_name("--123"));
        assert!(is_valid_custom_property_name("--myVar"));

        assert!(!is_valid_custom_property_name("color"));
        assert!(!is_valid_custom_property_name("-color"));
        assert!(!is_valid_custom_property_name("--"));
        assert!(!is_valid_custom_property_name("--has space"));
    }

    #[test]
    fn test_split_var_args_simple() {
        let (name, fallback) = split_var_args("--color");
        assert_eq!(name, "--color");
        assert_eq!(fallback, None);
    }

    #[test]
    fn test_split_var_args_with_fallback() {
        let (name, fallback) = split_var_args("--color, red");
        assert_eq!(name, "--color");
        assert_eq!(fallback, Some("red"));
    }

    #[test]
    fn test_split_var_args_with_nested_parens() {
        let (name, fallback) = split_var_args("--color, calc(10px + 5px)");
        assert_eq!(name, "--color");
        assert_eq!(fallback, Some("calc(10px + 5px)"));
    }

    #[test]
    fn test_split_var_args_with_nested_var() {
        let (name, fallback) = split_var_args("--color, var(--fallback)");
        assert_eq!(name, "--color");
        assert_eq!(fallback, Some("var(--fallback)"));
    }

    // Edge cases
    #[test]
    fn test_empty_var() {
        let props = HashMap::new();
        let value = PropertyValue::Keyword("var()".to_string());
        let resolved = resolve_var(&value, &props);

        // Should return the original malformed var()
        if let PropertyValue::Keyword(kw) = resolved {
            assert_eq!(kw, "var()");
        }
    }

    #[test]
    fn test_var_with_whitespace() {
        let props = make_props(&[("--color", "blue")]);
        let value = PropertyValue::Keyword("var(  --color  )".to_string());
        let resolved = resolve_var(&value, &props);

        if let PropertyValue::Keyword(kw) = resolved {
            assert_eq!(kw, "blue");
        } else {
            panic!("Expected Keyword 'blue', got {:?}", resolved);
        }
    }

    #[test]
    fn test_non_var_value_unchanged() {
        let props = make_props(&[("--color", "blue")]);
        let value = PropertyValue::Length(Length::px(10.0));
        let resolved = resolve_var(&value, &props);

        if let PropertyValue::Length(len) = resolved {
            assert_eq!(len.value, 10.0);
        } else {
            panic!("Expected Length, got {:?}", resolved);
        }
    }

    #[test]
    fn test_resolve_var_result_methods() {
        let resolved = VarResolutionResult::Resolved(PropertyValue::Keyword("blue".to_string()));
        assert!(resolved.is_resolved());

        let default = PropertyValue::Keyword("red".to_string());
        let result = VarResolutionResult::NotFound("--missing".to_string());
        let value = result.unwrap_or(default.clone());
        if let PropertyValue::Keyword(kw) = value {
            assert_eq!(kw, "red");
        }
    }
}
