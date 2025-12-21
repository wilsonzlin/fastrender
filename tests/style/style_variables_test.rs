//! Integration tests for CSS Variables Storage
//!
//! These tests verify the CSS custom properties implementation works correctly
//! in various scenarios that mirror real-world usage.

use fastrender::style::variables::CssVariables;
use std::collections::HashMap;

// =============================================================================
// Basic Operations Tests
// =============================================================================

#[test]
fn test_basic_variable_storage() {
    let mut vars = CssVariables::new();

    // Set multiple variables
    vars.set("--primary-color", "#007bff");
    vars.set("--secondary-color", "#6c757d");
    vars.set("--font-size-base", "16px");
    vars.set("--line-height", "1.5");
    vars.set("--border-radius", "4px");

    assert_eq!(vars.len(), 5);
    assert_eq!(vars.get("--primary-color"), Some(&"#007bff".to_string()));
    assert_eq!(vars.get("--font-size-base"), Some(&"16px".to_string()));
}

#[test]
fn test_variable_overwrite() {
    let mut vars = CssVariables::new();

    vars.set("--color", "blue");
    assert_eq!(vars.get("--color"), Some(&"blue".to_string()));

    vars.set("--color", "red");
    assert_eq!(vars.get("--color"), Some(&"red".to_string()));

    vars.set("--color", "green");
    assert_eq!(vars.get("--color"), Some(&"green".to_string()));
}

#[test]
fn test_variable_removal() {
    let mut vars = CssVariables::new();

    vars.set("--color", "blue");
    vars.set("--size", "16px");

    let removed = vars.remove("--color");
    assert_eq!(removed, Some("blue".to_string()));
    assert!(!vars.contains("--color"));
    assert!(vars.contains("--size"));
}

// =============================================================================
// Inheritance Tests
// =============================================================================

#[test]
fn test_inheritance_chain() {
    // Simulate a DOM hierarchy: html > body > main > article
    let mut html_vars = CssVariables::new();
    html_vars.set("--font-family", "Arial");
    html_vars.set("--color", "black");

    let mut body_vars = CssVariables::inherit_from(&html_vars);
    body_vars.set("--background", "white");

    let mut main_vars = CssVariables::inherit_from(&body_vars);
    main_vars.set("--max-width", "1200px");

    let mut article_vars = CssVariables::inherit_from(&main_vars);
    article_vars.set("--color", "gray"); // Override

    // Verify inheritance chain
    assert_eq!(article_vars.get("--font-family"), Some(&"Arial".to_string()));
    assert_eq!(article_vars.get("--background"), Some(&"white".to_string()));
    assert_eq!(article_vars.get("--max-width"), Some(&"1200px".to_string()));
    assert_eq!(article_vars.get("--color"), Some(&"gray".to_string())); // Overridden
}

#[test]
fn test_inheritance_preserves_child_values() {
    let mut parent = CssVariables::new();
    parent.set("--color", "blue");
    parent.set("--size", "16px");

    let mut child = CssVariables::new();
    child.set("--color", "red");
    child.set("--font", "Arial");

    child.inherit(&parent);

    // Child's --color should be preserved
    assert_eq!(child.get("--color"), Some(&"red".to_string()));
    // Parent's --size should be inherited
    assert_eq!(child.get("--size"), Some(&"16px".to_string()));
    // Child's --font should be preserved
    assert_eq!(child.get("--font"), Some(&"Arial".to_string()));
}

#[test]
fn test_inherit_all_replaces() {
    let mut parent = CssVariables::new();
    parent.set("--color", "blue");

    let mut child = CssVariables::new();
    child.set("--old-var", "old-value");

    child.inherit_all(&parent);

    assert_eq!(child.get("--color"), Some(&"blue".to_string()));
    assert_eq!(child.get("--old-var"), None); // Gone after inherit_all
}

// =============================================================================
// Resolution Tests
// =============================================================================

#[test]
fn test_resolution_simple() {
    let mut vars = CssVariables::new();
    vars.set("--primary", "#007bff");
    vars.set("--danger", "#dc3545");

    assert_eq!(vars.resolve("var(--primary)"), Some("#007bff".to_string()));
    assert_eq!(vars.resolve("var(--danger)"), Some("#dc3545".to_string()));
}

#[test]
fn test_resolution_with_fallback() {
    let mut vars = CssVariables::new();
    vars.set("--defined", "value");

    // Undefined with fallback
    assert_eq!(vars.resolve("var(--undefined, fallback)"), Some("fallback".to_string()));

    // Defined ignores fallback
    assert_eq!(vars.resolve("var(--defined, fallback)"), Some("value".to_string()));
}

#[test]
fn test_resolution_in_property_value() {
    let mut vars = CssVariables::new();
    vars.set("--size", "10px");
    vars.set("--color", "#333");

    // Variable in the middle of a value
    assert_eq!(
        vars.resolve("border: 1px solid var(--color)"),
        Some("border: 1px solid #333".to_string())
    );

    // Multiple variables in one value
    assert_eq!(
        vars.resolve("padding: var(--size) var(--size)"),
        Some("padding: 10px 10px".to_string())
    );
}

#[test]
fn test_resolution_nested_var_in_value() {
    let mut vars = CssVariables::new();
    vars.set("--primary", "var(--base-color)");
    vars.set("--base-color", "blue");

    assert_eq!(vars.resolve("var(--primary)"), Some("blue".to_string()));
}

#[test]
fn test_resolution_nested_fallback() {
    let mut vars = CssVariables::new();
    vars.set("--fallback-color", "red");

    // First var undefined, fallback is another var
    assert_eq!(
        vars.resolve("var(--undefined, var(--fallback-color))"),
        Some("red".to_string())
    );

    // Both undefined, innermost fallback used
    assert_eq!(
        vars.resolve("var(--undef1, var(--undef2, blue))"),
        Some("blue".to_string())
    );
}

#[test]
fn test_resolution_complex_fallback() {
    let vars = CssVariables::new();

    // Fallback with spaces and multiple parts
    assert_eq!(
        vars.resolve("var(--shadow, 0 2px 4px rgba(0,0,0,0.1))"),
        Some("0 2px 4px rgba(0,0,0,0.1)".to_string())
    );

    // Fallback with function
    assert_eq!(
        vars.resolve("var(--gradient, linear-gradient(to right, red, blue))"),
        Some("linear-gradient(to right, red, blue)".to_string())
    );
}

#[test]
fn test_resolution_no_var() {
    let vars = CssVariables::new();

    // Plain values should pass through unchanged
    assert_eq!(vars.resolve("blue"), Some("blue".to_string()));
    assert_eq!(vars.resolve("10px"), Some("10px".to_string()));
    assert_eq!(vars.resolve("1.5"), Some("1.5".to_string()));
}

#[test]
fn test_resolution_circular_reference_protected() {
    let mut vars = CssVariables::new();
    vars.set("--a", "var(--b)");
    vars.set("--b", "var(--c)");
    vars.set("--c", "var(--a)"); // Circular!

    // Should not hang, should return None
    assert_eq!(vars.resolve("var(--a)"), None);
}

#[test]
fn test_resolution_deep_nesting_protected() {
    let mut vars = CssVariables::new();

    // Create deep nesting (11 levels, exceeds max depth of 10)
    vars.set("--v1", "var(--v2)");
    vars.set("--v2", "var(--v3)");
    vars.set("--v3", "var(--v4)");
    vars.set("--v4", "var(--v5)");
    vars.set("--v5", "var(--v6)");
    vars.set("--v6", "var(--v7)");
    vars.set("--v7", "var(--v8)");
    vars.set("--v8", "var(--v9)");
    vars.set("--v9", "var(--v10)");
    vars.set("--v10", "var(--v11)");
    vars.set("--v11", "final");

    // Max depth is 10, so this should fail
    assert_eq!(vars.resolve("var(--v1)"), None);
}

#[test]
fn test_resolution_just_within_depth_limit() {
    let mut vars = CssVariables::new();

    // Create nesting within limit (10 levels)
    vars.set("--v1", "var(--v2)");
    vars.set("--v2", "var(--v3)");
    vars.set("--v3", "var(--v4)");
    vars.set("--v4", "var(--v5)");
    vars.set("--v5", "var(--v6)");
    vars.set("--v6", "var(--v7)");
    vars.set("--v7", "var(--v8)");
    vars.set("--v8", "var(--v9)");
    vars.set("--v9", "final");

    // This should work (9 levels of nesting)
    assert_eq!(vars.resolve("var(--v1)"), Some("final".to_string()));
}

// =============================================================================
// Validation Tests
// =============================================================================

#[test]
fn test_valid_property_names() {
    assert!(CssVariables::is_valid_name("--color"));
    assert!(CssVariables::is_valid_name("--primary-color"));
    assert!(CssVariables::is_valid_name("--my_var_123"));
    assert!(CssVariables::is_valid_name("--UPPERCASE"));
    assert!(CssVariables::is_valid_name("--MixedCase"));
    assert!(CssVariables::is_valid_name("--a"));
}

#[test]
fn test_invalid_property_names() {
    assert!(!CssVariables::is_valid_name("-color")); // Only one dash
    assert!(!CssVariables::is_valid_name("color")); // No dashes
    assert!(!CssVariables::is_valid_name("--")); // No name after dashes
}

#[test]
fn test_has_var_reference() {
    assert!(CssVariables::has_var_reference("var(--color)"));
    assert!(CssVariables::has_var_reference("color: var(--primary)"));
    assert!(CssVariables::has_var_reference("var(--x) var(--y)"));

    assert!(!CssVariables::has_var_reference("color: blue"));
    assert!(!CssVariables::has_var_reference("10px"));
    assert!(!CssVariables::has_var_reference(""));
}

#[test]
fn test_extract_var_name() {
    assert_eq!(
        CssVariables::extract_var_name("var(--color)"),
        Some("--color".to_string())
    );
    assert_eq!(
        CssVariables::extract_var_name("var(--primary-color)"),
        Some("--primary-color".to_string())
    );
    assert_eq!(
        CssVariables::extract_var_name("var(--color, blue)"),
        Some("--color".to_string())
    );
    assert_eq!(
        CssVariables::extract_var_name("var(  --spaced  , fallback)"),
        Some("--spaced".to_string())
    );

    assert_eq!(CssVariables::extract_var_name("blue"), None);
    assert_eq!(CssVariables::extract_var_name("var("), None);
}

// =============================================================================
// Merge and Combine Tests
// =============================================================================

#[test]
fn test_merge() {
    let mut base = CssVariables::new();
    base.set("--color", "blue");
    base.set("--size", "16px");

    let mut overlay = CssVariables::new();
    overlay.set("--color", "red"); // Override
    overlay.set("--font", "Arial"); // New

    base.merge(&overlay);

    assert_eq!(base.get("--color"), Some(&"red".to_string()));
    assert_eq!(base.get("--size"), Some(&"16px".to_string()));
    assert_eq!(base.get("--font"), Some(&"Arial".to_string()));
}

#[test]
fn test_combined_with() {
    let mut base = CssVariables::new();
    base.set("--color", "blue");

    let mut overlay = CssVariables::new();
    overlay.set("--color", "red");
    overlay.set("--size", "16px");

    let combined = base.combined_with(&overlay);

    // Original unchanged
    assert_eq!(base.get("--color"), Some(&"blue".to_string()));

    // Combined has overlay values
    assert_eq!(combined.get("--color"), Some(&"red".to_string()));
    assert_eq!(combined.get("--size"), Some(&"16px".to_string()));
}

// =============================================================================
// Iterator and Collection Tests
// =============================================================================

#[test]
fn test_iter() {
    let mut vars = CssVariables::new();
    vars.set("--a", "1");
    vars.set("--b", "2");
    vars.set("--c", "3");

    let collected: HashMap<_, _> = vars.iter().map(|(k, v)| (k.clone(), v.clone())).collect();

    assert_eq!(collected.len(), 3);
    assert_eq!(collected.get("--a"), Some(&"1".to_string()));
    assert_eq!(collected.get("--b"), Some(&"2".to_string()));
    assert_eq!(collected.get("--c"), Some(&"3".to_string()));
}

#[test]
fn test_names_and_values() {
    let mut vars = CssVariables::new();
    vars.set("--x", "10");
    vars.set("--y", "20");

    let names: Vec<_> = vars.names().collect();
    assert_eq!(names.len(), 2);

    let values: Vec<_> = vars.values().collect();
    assert_eq!(values.len(), 2);
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
    vars.set("--existing", "value");

    let new_items = vec![
        ("--new1".to_string(), "value1".to_string()),
        ("--new2".to_string(), "value2".to_string()),
    ];

    vars.extend(new_items);

    assert_eq!(vars.len(), 3);
    assert!(vars.contains("--new1"));
    assert!(vars.contains("--new2"));
}

#[test]
fn test_into_iterator() {
    let mut vars = CssVariables::new();
    vars.set("--a", "1");
    vars.set("--b", "2");

    let map: HashMap<_, _> = vars.into_iter().collect();

    assert_eq!(map.len(), 2);
    assert_eq!(map.get("--a"), Some(&"1".to_string()));
}

// =============================================================================
// HashMap Conversion Tests
// =============================================================================

#[test]
fn test_from_map() {
    let mut map = HashMap::new();
    map.insert("--color".to_string(), "blue".to_string());
    map.insert("--size".to_string(), "16px".to_string());

    let vars = CssVariables::from_map(map);

    assert_eq!(vars.len(), 2);
    assert_eq!(vars.get("--color"), Some(&"blue".to_string()));
}

#[test]
fn test_into_map() {
    let mut vars = CssVariables::new();
    vars.set("--color", "blue");
    vars.set("--size", "16px");

    let map = vars.into_map();

    assert_eq!(map.len(), 2);
    assert_eq!(map.get("--color"), Some(&"blue".to_string()));
}

#[test]
fn test_as_map() {
    let mut vars = CssVariables::new();
    vars.set("--color", "blue");

    let map_ref = vars.as_map();
    assert_eq!(map_ref.get("--color"), Some(&"blue".to_string()));
}

#[test]
fn test_from_into_hashmap() {
    let mut map = HashMap::new();
    map.insert("--key".to_string(), "value".to_string());

    let vars: CssVariables = map.into();
    let map_back = vars.into_map();

    assert_eq!(map_back.get("--key"), Some(&"value".to_string()));
}

// =============================================================================
// Edge Cases and Real-World Scenarios
// =============================================================================

#[test]
fn test_empty_value() {
    let mut vars = CssVariables::new();
    vars.set("--empty", "");

    assert_eq!(vars.get("--empty"), Some(&"".to_string()));
    assert_eq!(vars.resolve("var(--empty)"), Some("".to_string()));
}

#[test]
fn test_value_with_special_characters() {
    let mut vars = CssVariables::new();
    vars.set("--gradient", "linear-gradient(45deg, #f00, #00f)");
    vars.set("--shadow", "0 2px 4px rgba(0, 0, 0, 0.1)");
    vars.set("--content", "'Hello, World!'");

    assert_eq!(
        vars.resolve("var(--gradient)"),
        Some("linear-gradient(45deg, #f00, #00f)".to_string())
    );
    assert_eq!(
        vars.resolve("var(--shadow)"),
        Some("0 2px 4px rgba(0, 0, 0, 0.1)".to_string())
    );
    assert_eq!(vars.resolve("var(--content)"), Some("'Hello, World!'".to_string()));
}

#[test]
fn test_theme_variables_scenario() {
    // Simulate a theming scenario
    let mut light_theme = CssVariables::new();
    light_theme.set("--bg-color", "#ffffff");
    light_theme.set("--text-color", "#333333");
    light_theme.set("--accent-color", "#007bff");
    light_theme.set("--border-color", "#dee2e6");

    let mut dark_theme = CssVariables::new();
    dark_theme.set("--bg-color", "#1a1a1a");
    dark_theme.set("--text-color", "#f0f0f0");
    dark_theme.set("--accent-color", "#4dabf7");
    dark_theme.set("--border-color", "#444444");

    // Use light theme
    let theme = &light_theme;
    assert_eq!(theme.resolve("var(--bg-color)"), Some("#ffffff".to_string()));

    // Switch to dark theme
    let theme = &dark_theme;
    assert_eq!(theme.resolve("var(--bg-color)"), Some("#1a1a1a".to_string()));
}

#[test]
fn test_design_token_hierarchy() {
    // Base tokens
    let mut base = CssVariables::new();
    base.set("--spacing-unit", "4px");
    base.set("--color-primary-500", "#007bff");

    // Semantic tokens (reference base)
    let mut semantic = CssVariables::inherit_from(&base);
    semantic.set("--spacing-sm", "var(--spacing-unit)");
    semantic.set("--spacing-md", "calc(var(--spacing-unit) * 2)");
    semantic.set("--color-action", "var(--color-primary-500)");

    // Component tokens (reference semantic)
    let component = CssVariables::inherit_from(&semantic);

    // Resolution chain works
    assert_eq!(component.resolve("var(--spacing-sm)"), Some("4px".to_string()));
    assert_eq!(component.resolve("var(--color-action)"), Some("#007bff".to_string()));
}

#[test]
fn test_css_variable_in_calc() {
    let mut vars = CssVariables::new();
    vars.set("--base-size", "16px");
    vars.set("--multiplier", "1.5");

    // calc() expression with var()
    let result = vars.resolve("calc(var(--base-size) * var(--multiplier))");
    assert_eq!(result, Some("calc(16px * 1.5)".to_string()));
}

#[test]
fn test_whitespace_handling() {
    let mut vars = CssVariables::new();
    vars.set("--color", "blue");

    // Extra whitespace in var() should work
    assert_eq!(vars.resolve("var(  --color  )"), Some("blue".to_string()));
    assert_eq!(vars.resolve("var(--color )"), Some("blue".to_string()));
    assert_eq!(vars.resolve("var( --color)"), Some("blue".to_string()));
}

#[test]
fn test_fallback_with_whitespace() {
    let vars = CssVariables::new();

    // Fallback with leading/trailing whitespace
    assert_eq!(vars.resolve("var(--undefined,   blue   )"), Some("blue".to_string()));
}

#[test]
fn test_partial_resolution_failure() {
    let mut vars = CssVariables::new();
    vars.set("--known", "value");

    // One var resolves, one doesn't
    // The undefined one should make the whole thing fail
    let result = vars.resolve("var(--known) var(--unknown)");
    assert_eq!(result, None);
}

#[test]
fn test_clone_and_modify() {
    let mut original = CssVariables::new();
    original.set("--color", "blue");

    let mut cloned = original.clone();
    cloned.set("--color", "red");
    cloned.set("--new", "value");

    // Original unchanged
    assert_eq!(original.get("--color"), Some(&"blue".to_string()));
    assert_eq!(original.get("--new"), None);

    // Clone modified
    assert_eq!(cloned.get("--color"), Some(&"red".to_string()));
    assert_eq!(cloned.get("--new"), Some(&"value".to_string()));
}

#[test]
fn test_equality() {
    let mut vars1 = CssVariables::new();
    vars1.set("--a", "1");
    vars1.set("--b", "2");

    let mut vars2 = CssVariables::new();
    vars2.set("--a", "1");
    vars2.set("--b", "2");

    let mut vars3 = CssVariables::new();
    vars3.set("--a", "1");
    vars3.set("--b", "3");

    assert_eq!(vars1, vars2);
    assert_ne!(vars1, vars3);
}
