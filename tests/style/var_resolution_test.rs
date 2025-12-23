//! Integration tests for CSS var() resolution
//!
//! These tests verify the var() resolution functionality in the style system,
//! including:
//! - Basic variable substitution
//! - Fallback values
//! - Nested var() references
//! - Embedded var() in other CSS functions
//! - Edge cases and error handling

use fastrender::style::values::LengthUnit;
use fastrender::style::var_resolution::contains_var;
use fastrender::style::var_resolution::extract_var_references;
use fastrender::style::var_resolution::is_valid_custom_property_name;
use fastrender::style::var_resolution::resolve_var_for_property;
use fastrender::style::var_resolution::resolve_var;
use fastrender::style::var_resolution::resolve_var_with_depth;
use fastrender::style::var_resolution::VarResolutionResult;
use fastrender::Length;
use fastrender::PropertyValue;
use std::collections::HashMap;

/// Helper function to create a custom properties map from pairs
fn make_props(pairs: &[(&str, &str)]) -> HashMap<String, String> {
  pairs
    .iter()
    .map(|(k, v)| (k.to_string(), v.to_string()))
    .collect()
}

// ============================================================================
// Basic var() Resolution Tests
// ============================================================================

#[test]
fn test_resolve_var_with_color_value() {
  // Note: The CSS parser requires property context to parse colors.
  // Without a property name like "color" or "background-color", hex colors
  // are returned as Keywords. This is correct CSS behavior - the property
  // determines value interpretation.
  let props = make_props(&[("--primary-color", "#3b82f6")]);
  let value = PropertyValue::Keyword("var(--primary-color)".to_string());
  let resolved = resolve_var(&value, &props);

  // Hex colors are kept as keywords when no property context is available
  match resolved {
    PropertyValue::Keyword(kw) => {
      assert_eq!(kw, "#3b82f6");
    }
    _ => panic!("Expected Keyword with hex color, got {:?}", resolved),
  }
}

#[test]
fn test_resolve_var_with_px_length() {
  let props = make_props(&[("--spacing", "16px")]);
  let value = PropertyValue::Keyword("var(--spacing)".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Length(len) => {
      assert_eq!(len.value, 16.0);
      assert_eq!(len.unit, fastrender::LengthUnit::Px);
    }
    _ => panic!("Expected Length, got {:?}", resolved),
  }
}

#[test]
fn test_resolve_var_with_em_length() {
  let props = make_props(&[("--font-size", "1.5em")]);
  let value = PropertyValue::Keyword("var(--font-size)".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Length(len) => {
      assert_eq!(len.value, 1.5);
      assert_eq!(len.unit, fastrender::LengthUnit::Em);
    }
    _ => panic!("Expected Length, got {:?}", resolved),
  }
}

#[test]
fn test_resolve_var_with_keyword_value() {
  let props = make_props(&[("--display", "flex")]);
  let value = PropertyValue::Keyword("var(--display)".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Keyword(kw) => {
      assert_eq!(kw, "flex");
    }
    _ => panic!("Expected Keyword, got {:?}", resolved),
  }
}

#[test]
fn test_resolve_var_case_insensitive_function_name() {
  let props = make_props(&[("--color", "green")]);
  let value = PropertyValue::Keyword("VAR(--color)".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Keyword(kw) => assert_eq!(kw, "green"),
    _ => panic!("Expected Keyword 'green', got {:?}", resolved),
  }
}

#[test]
fn test_resolve_var_not_found_no_fallback() {
  let props = HashMap::new();
  let value = PropertyValue::Keyword("var(--undefined)".to_string());
  let resolved = resolve_var(&value, &props);

  // Should return the original var() reference since it can't be resolved
  match resolved {
    PropertyValue::Keyword(kw) => {
      assert!(kw.contains("var(--undefined)"));
    }
    _ => panic!("Expected Keyword, got {:?}", resolved),
  }
}

// ============================================================================
// Fallback Value Tests
// ============================================================================

#[test]
fn test_resolve_var_with_fallback_used() {
  let props = HashMap::new();
  let value = PropertyValue::Keyword("var(--missing, 20px)".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Length(len) => {
      assert_eq!(len.value, 20.0);
    }
    _ => panic!("Expected Length (fallback), got {:?}", resolved),
  }
}

#[test]
fn test_resolve_var_fallback_with_commas_and_functions() {
  let props = HashMap::new();
  let value = PropertyValue::Keyword("var(--missing, rgb(1, 2, 3))".to_string());
  let resolved = resolve_var_for_property(&value, &props, "color");

  match resolved {
    VarResolutionResult::Resolved(v) => match *v {
      PropertyValue::Color(_) => {}
      other => panic!("Expected color value, got {:?}", other),
    },
    other => panic!("Expected resolved fallback color, got {:?}", other),
  }
}

#[test]
fn test_resolve_var_fallback_with_commas_inside_url() {
  let props = HashMap::new();
  let value = PropertyValue::Keyword("var(--missing, url(a,b))".to_string());
  let resolved = resolve_var_for_property(&value, &props, "background-image");

  match resolved {
    VarResolutionResult::Resolved(v) => match *v {
      PropertyValue::Multiple(values) => {
        assert_eq!(values.len(), 1);
        assert!(matches!(values[0], PropertyValue::Url(ref url) if url == "a,b"));
      }
      PropertyValue::Url(ref url) => assert_eq!(url, "a,b"),
      other => panic!("Expected Url value, got {:?}", other),
    },
    other => panic!("Expected resolved url fallback, got {:?}", other),
  }
}

#[test]
fn test_resolve_var_with_fallback_not_used() {
  let props = make_props(&[("--size", "10px")]);
  let value = PropertyValue::Keyword("var(--size, 20px)".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Length(len) => {
      // Should use the defined value, not the fallback
      assert_eq!(len.value, 10.0);
    }
    _ => panic!("Expected Length, got {:?}", resolved),
  }
}

#[test]
fn test_resolve_var_with_color_fallback() {
  let props = HashMap::new();
  let value = PropertyValue::Keyword("var(--missing, red)".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Keyword(kw) => {
      assert_eq!(kw, "red");
    }
    _ => panic!("Expected Keyword 'red', got {:?}", resolved),
  }
}

#[test]
fn test_resolve_var_with_complex_fallback() {
  let props = HashMap::new();
  let value = PropertyValue::Keyword("var(--shadow, 0 2px 4px rgba(0,0,0,0.1))".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Keyword(kw) => {
      assert!(kw.contains("0 2px 4px"));
    }
    _ => panic!("Expected Keyword with shadow value, got {:?}", resolved),
  }
}

// ============================================================================
// Nested var() Tests
// ============================================================================

#[test]
fn test_resolve_nested_var_in_fallback() {
  let props = make_props(&[("--fallback-color", "blue")]);
  let value = PropertyValue::Keyword("var(--primary, var(--fallback-color))".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Keyword(kw) => {
      assert_eq!(kw, "blue");
    }
    _ => panic!("Expected Keyword 'blue', got {:?}", resolved),
  }
}

#[test]
fn test_resolve_chained_variables() {
  // --primary points to another variable
  let props = make_props(&[("--primary", "var(--secondary)"), ("--secondary", "green")]);
  let value = PropertyValue::Keyword("var(--primary)".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Keyword(kw) => {
      assert_eq!(kw, "green");
    }
    _ => panic!("Expected Keyword 'green', got {:?}", resolved),
  }
}

#[test]
fn test_replacement_containing_var_continues_resolving() {
  let props = make_props(&[("--a", "calc(var(--b) * 2)"), ("--b", "5px")]);
  let value = PropertyValue::Keyword("var(--a)".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Length(len) => {
      assert!((len.value - 10.0).abs() < f32::EPSILON);
      assert_eq!(len.unit, fastrender::LengthUnit::Px);
    }
    _ => panic!("Expected Length from nested var replacement, got {:?}", resolved),
  }
}

#[test]
fn test_resolve_deeply_nested_fallback() {
  let props = make_props(&[("--level3", "purple")]);
  let value = PropertyValue::Keyword("var(--level1, var(--level2, var(--level3)))".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Keyword(kw) => {
      assert_eq!(kw, "purple");
    }
    _ => panic!("Expected Keyword 'purple', got {:?}", resolved),
  }
}

// ============================================================================
// Embedded var() Tests
// ============================================================================

#[test]
fn test_resolve_var_in_calc() {
  let props = make_props(&[("--base-size", "16px")]);
  let value = PropertyValue::Keyword("calc(var(--base-size) * 2)".to_string());
  let resolved = resolve_var(&value, &props);

  assert!(
    matches!(resolved, PropertyValue::Length(len) if (len.value - 32.0).abs() < f32::EPSILON && len.unit == LengthUnit::Px),
    "Expected resolved calc length, got {:?}",
    resolved
  );
}

#[test]
fn test_resolve_multiple_vars_in_value() {
  let props = make_props(&[("--x-offset", "10px"), ("--y-offset", "20px")]);
  let value = PropertyValue::Keyword("var(--x-offset) var(--y-offset)".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Keyword(kw) => {
      assert!(kw.contains("10px") && kw.contains("20px"));
    }
    _ => panic!("Expected Keyword with both values, got {:?}", resolved),
  }
}

#[test]
fn test_resolve_repeated_var_occurrences() {
  let props = make_props(&[("--space", "8px")]);
  let value = PropertyValue::Keyword("var(--space) var(--space)".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Keyword(kw) => assert_eq!(kw, "8px 8px"),
    _ => panic!("Expected Keyword with repeated substitution, got {:?}", resolved),
  }
}

#[test]
fn test_resolve_var_in_box_shadow() {
  let props = make_props(&[("--blur", "4px"), ("--color", "rgba(0,0,0,0.2)")]);
  let value = PropertyValue::Keyword("0 2px var(--blur) var(--color)".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Keyword(kw) => {
      assert!(kw.contains("0 2px 4px"));
    }
    _ => panic!("Expected Keyword with shadow, got {:?}", resolved),
  }
}

// ============================================================================
// Recursion Protection Tests
// ============================================================================

#[test]
fn test_circular_reference_protection() {
  // Create a circular reference
  let props = make_props(&[
    ("--a", "var(--b)"),
    ("--b", "var(--c)"),
    ("--c", "var(--a)"),
  ]);
  let value = PropertyValue::Keyword("var(--a)".to_string());

  // Should not cause a stack overflow
  let _resolved = resolve_var(&value, &props);
  // Test passes if we get here without panicking
}

#[test]
fn test_two_variable_cycle_reports_limit() {
  let props = make_props(&[("--a", "var(--b)"), ("--b", "var(--a)")]);
  let value = PropertyValue::Keyword("var(--a)".to_string());
  let resolved = resolve_var_for_property(&value, &props, "color");
  assert!(matches!(resolved, VarResolutionResult::RecursionLimitExceeded));
}

#[test]
fn test_explicit_depth_limit() {
  let props = make_props(&[("--deep", "value")]);
  let value = PropertyValue::Keyword("var(--deep)".to_string());

  // At depth 0, should resolve
  let resolved = resolve_var_with_depth(&value, &props, 0);
  match resolved {
    PropertyValue::Keyword(kw) => assert_eq!(kw, "value"),
    _ => panic!("Expected Keyword 'value', got {:?}", resolved),
  }

  // At max depth, should return original
  let resolved_max = resolve_var_with_depth(&value, &props, 10);
  match resolved_max {
    PropertyValue::Keyword(kw) => assert!(kw.contains("var(--deep)")),
    _ => panic!("Expected unresolved var(), got {:?}", resolved_max),
  }
}

// ============================================================================
// Utility Function Tests
// ============================================================================

#[test]
fn test_contains_var_positive() {
  assert!(contains_var("var(--color)"));
  assert!(contains_var("color: var(--primary)"));
  assert!(contains_var("calc(var(--size) + 10px)"));
  assert!(contains_var("var(--x) var(--y)"));
  assert!(contains_var("VAR(--x)"));
}

#[test]
fn test_contains_var_negative() {
  assert!(!contains_var("10px"));
  assert!(!contains_var("red"));
  assert!(!contains_var("#ff0000"));
  assert!(!contains_var(""));
  assert!(!contains_var("variant")); // Contains "var" but not "var("
}

#[test]
fn test_extract_var_references_single() {
  let refs = extract_var_references("var(--color)");
  assert_eq!(refs, vec!["--color"]);
}

#[test]
fn test_extract_var_references_multiple() {
  let refs = extract_var_references("calc(var(--x) + var(--y))");
  assert_eq!(refs.len(), 2);
  assert!(refs.contains(&"--x".to_string()));
  assert!(refs.contains(&"--y".to_string()));
}

#[test]
fn test_extract_var_references_nested() {
  let refs = extract_var_references("var(--a, var(--b))");
  assert_eq!(refs.len(), 2);
  assert!(refs.contains(&"--a".to_string()));
  assert!(refs.contains(&"--b".to_string()));
}

#[test]
fn test_extract_var_references_none() {
  let refs = extract_var_references("10px solid black");
  assert!(refs.is_empty());
}

#[test]
fn test_is_valid_custom_property_name_valid() {
  assert!(is_valid_custom_property_name("--color"));
  assert!(is_valid_custom_property_name("--primary-color"));
  assert!(is_valid_custom_property_name("--_private"));
  assert!(is_valid_custom_property_name("--camelCase"));
  assert!(is_valid_custom_property_name("--color123"));
  assert!(is_valid_custom_property_name("---triple-dash"));
}

#[test]
fn test_is_valid_custom_property_name_invalid() {
  assert!(!is_valid_custom_property_name("color"));
  assert!(!is_valid_custom_property_name("-color"));
  assert!(!is_valid_custom_property_name("--")); // Just dashes
  assert!(!is_valid_custom_property_name("")); // Empty
  assert!(!is_valid_custom_property_name("--has space"));
  assert!(!is_valid_custom_property_name("--has\ttab"));
}

// ============================================================================
// Edge Case Tests
// ============================================================================

#[test]
fn test_empty_var() {
  let props = HashMap::new();
  let value = PropertyValue::Keyword("var()".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Keyword(kw) => {
      assert_eq!(kw, "var()");
    }
    _ => panic!("Expected Keyword, got {:?}", resolved),
  }
}

#[test]
fn test_var_with_whitespace() {
  let props = make_props(&[("--color", "red")]);
  let value = PropertyValue::Keyword("var(  --color  )".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Keyword(kw) => {
      assert_eq!(kw, "red");
    }
    _ => panic!("Expected Keyword 'red', got {:?}", resolved),
  }
}

#[test]
fn test_non_var_value_unchanged() {
  let props = make_props(&[("--unused", "value")]);
  let value = PropertyValue::Length(Length::px(42.0));
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Length(len) => {
      assert_eq!(len.value, 42.0);
    }
    _ => panic!("Expected Length, got {:?}", resolved),
  }
}

#[test]
fn test_non_keyword_value_unchanged() {
  let props = make_props(&[("--unused", "value")]);

  // Number should pass through unchanged
  let value = PropertyValue::Number(42.5);
  let resolved = resolve_var(&value, &props);
  match resolved {
    PropertyValue::Number(n) => assert!((n - 42.5).abs() < 0.001),
    _ => panic!("Expected Number, got {:?}", resolved),
  }

  // Percentage should pass through unchanged
  let value = PropertyValue::Percentage(50.0);
  let resolved = resolve_var(&value, &props);
  match resolved {
    PropertyValue::Percentage(p) => assert_eq!(p, 50.0),
    _ => panic!("Expected Percentage, got {:?}", resolved),
  }
}

#[test]
fn test_var_with_numeric_fallback() {
  let props = HashMap::new();
  let value = PropertyValue::Keyword("var(--opacity, 0.5)".to_string());
  let resolved = resolve_var(&value, &props);

  match resolved {
    PropertyValue::Number(n) => {
      assert!((n - 0.5).abs() < 0.001);
    }
    _ => panic!("Expected Number, got {:?}", resolved),
  }
}

// ============================================================================
// Integration with Style System Tests
// ============================================================================

#[test]
fn test_resolve_var_preserves_type_for_colors() {
  // Note: CSS color parsing requires property context. Without specifying
  // that this is a "color" or "background-color" property, the parser
  // returns hex colors as Keywords. This is by design - the property
  // determines the value type interpretation.
  let props = make_props(&[("--bg", "#ffffff")]);
  let value = PropertyValue::Keyword("var(--bg)".to_string());
  let resolved = resolve_var(&value, &props);

  // Hex colors are kept as keywords when no property context is available
  // The actual color parsing happens at property application time
  match resolved {
    PropertyValue::Keyword(kw) => {
      assert_eq!(kw, "#ffffff");
    }
    _ => panic!("Expected Keyword with hex color, got {:?}", resolved),
  }
}

#[test]
fn test_resolve_var_preserves_type_for_lengths() {
  let props = make_props(&[("--small", "8px"), ("--medium", "1rem"), ("--large", "2em")]);

  for (name, expected_value) in [("--small", 8.0), ("--medium", 1.0), ("--large", 2.0)] {
    let value = PropertyValue::Keyword(format!("var({})", name));
    let resolved = resolve_var(&value, &props);

    match resolved {
      PropertyValue::Length(len) => {
        assert!(
          (len.value - expected_value).abs() < 0.001,
          "Expected {} for {}, got {}",
          expected_value,
          name,
          len.value
        );
      }
      _ => panic!("Expected Length for {}, got {:?}", name, resolved),
    }
  }
}
