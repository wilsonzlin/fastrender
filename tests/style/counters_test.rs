//! Integration tests for the CSS Counter System
//!
//! These tests verify the counter system works correctly in various real-world
//! scenarios and edge cases.

use fastrender::style::content::CounterStyle;
use fastrender::style::counters::CounterManager;
use fastrender::style::counters::CounterProperties;
use fastrender::style::counters::CounterSet;
use fastrender::style::counters::CounterSetItem;

// ============================================================================
// CounterSet Parsing Tests
// ============================================================================

mod counter_set_parsing {
  use super::*;

  #[test]
  fn parse_single_counter_default_reset() {
    let set = CounterSet::parse_reset("chapter").unwrap();
    assert_eq!(set.len(), 1);
    assert_eq!(set.get("chapter"), Some(0));
  }

  #[test]
  fn parse_single_counter_explicit_value() {
    let set = CounterSet::parse_reset("chapter 5").unwrap();
    assert_eq!(set.get("chapter"), Some(5));
  }

  #[test]
  fn parse_single_counter_negative_value() {
    let set = CounterSet::parse_reset("chapter -3").unwrap();
    assert_eq!(set.get("chapter"), Some(-3));
  }

  #[test]
  fn parse_multiple_counters_all_defaults() {
    let set = CounterSet::parse_reset("chapter section subsection").unwrap();
    assert_eq!(set.len(), 3);
    assert_eq!(set.get("chapter"), Some(0));
    assert_eq!(set.get("section"), Some(0));
    assert_eq!(set.get("subsection"), Some(0));
  }

  #[test]
  fn parse_multiple_counters_mixed_values() {
    let set = CounterSet::parse_reset("chapter 1 section section-part 0").unwrap();
    assert_eq!(set.len(), 3);
    assert_eq!(set.get("chapter"), Some(1));
    assert_eq!(set.get("section"), Some(0)); // Default when followed by identifier
    assert_eq!(set.get("section-part"), Some(0));
  }

  #[test]
  fn parse_increment_default_value() {
    let set = CounterSet::parse_increment("chapter").unwrap();
    assert_eq!(set.get("chapter"), Some(1)); // Default increment is 1
  }

  #[test]
  fn parse_increment_explicit_value() {
    let set = CounterSet::parse_increment("chapter 2").unwrap();
    assert_eq!(set.get("chapter"), Some(2));
  }

  #[test]
  fn parse_increment_negative() {
    let set = CounterSet::parse_increment("chapter -1").unwrap();
    assert_eq!(set.get("chapter"), Some(-1));
  }

  #[test]
  fn parse_none_keyword() {
    let set = CounterSet::parse_reset("none").unwrap();
    assert!(set.is_empty());
  }

  #[test]
  fn parse_empty_string() {
    let set = CounterSet::parse_reset("").unwrap();
    assert!(set.is_empty());
  }

  #[test]
  fn parse_whitespace_handling() {
    let set = CounterSet::parse_reset("  chapter   5   section   10  ").unwrap();
    assert_eq!(set.get("chapter"), Some(5));
    assert_eq!(set.get("section"), Some(10));
  }

  #[test]
  fn parse_hyphenated_counter_name() {
    let set = CounterSet::parse_reset("list-item my-counter").unwrap();
    assert_eq!(set.get("list-item"), Some(0));
    assert_eq!(set.get("my-counter"), Some(0));
  }

  #[test]
  fn parse_underscore_counter_name() {
    let set = CounterSet::parse_reset("my_counter").unwrap();
    assert_eq!(set.get("my_counter"), Some(0));
  }

  #[test]
  fn counter_set_display_format() {
    let set = CounterSet::parse_reset("chapter 1 section 2").unwrap();
    let formatted = format!("{}", set);
    assert!(formatted.contains("chapter 1"));
    assert!(formatted.contains("section 2"));
  }

  #[test]
  fn counter_set_display_none() {
    let set = CounterSet::new();
    assert_eq!(format!("{}", set), "none");
  }

  #[test]
  fn counter_set_item_construction() {
    let item = CounterSetItem::new("test", 42);
    assert_eq!(item.name, "test");
    assert_eq!(item.value, 42);
  }

  #[test]
  fn counter_set_single_constructor() {
    let set = CounterSet::single("chapter", 10);
    assert_eq!(set.len(), 1);
    assert_eq!(set.get("chapter"), Some(10));
  }

  #[test]
  fn counter_set_add_method() {
    let mut set = CounterSet::new();
    set.add("chapter", 0);
    set.add("section", 5);
    assert_eq!(set.len(), 2);
    assert_eq!(set.get("chapter"), Some(0));
    assert_eq!(set.get("section"), Some(5));
  }
}

// ============================================================================
// CounterManager Basic Operations Tests
// ============================================================================

mod counter_manager_basic {
  use super::*;

  #[test]
  fn new_manager_has_root_scope() {
    let manager = CounterManager::new();
    assert_eq!(manager.depth(), 1);
  }

  #[test]
  fn get_nonexistent_counter() {
    let manager = CounterManager::new();
    assert_eq!(manager.get("nonexistent"), None);
  }

  #[test]
  fn get_or_zero_nonexistent() {
    let manager = CounterManager::new();
    assert_eq!(manager.get_or_zero("nonexistent"), 0);
  }

  #[test]
  fn has_counter() {
    let mut manager = CounterManager::new();
    assert!(!manager.has("chapter"));

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 0));
    assert!(manager.has("chapter"));
  }

  #[test]
  fn reset_manager() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 10));
    manager.enter_scope();
    manager.enter_scope();

    manager.reset();

    assert_eq!(manager.depth(), 1);
    assert!(!manager.has("chapter"));
  }
}

// ============================================================================
// Counter Reset and Increment Tests
// ============================================================================

mod counter_reset_increment {
  use super::*;

  #[test]
  fn reset_creates_counter() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 5));

    assert_eq!(manager.get("chapter"), Some(5));
  }

  #[test]
  fn reset_multiple_counters() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::parse_reset("chapter 0 section 0").unwrap());

    assert_eq!(manager.get("chapter"), Some(0));
    assert_eq!(manager.get("section"), Some(0));
  }

  #[test]
  fn increment_existing_counter() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 0));
    manager.apply_increment(&CounterSet::single("chapter", 1));

    assert_eq!(manager.get("chapter"), Some(1));
  }

  #[test]
  fn increment_by_custom_amount() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 0));
    manager.apply_increment(&CounterSet::single("chapter", 5));

    assert_eq!(manager.get("chapter"), Some(5));
  }

  #[test]
  fn increment_negative() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 10));
    manager.apply_increment(&CounterSet::single("chapter", -1));

    assert_eq!(manager.get("chapter"), Some(9));
  }

  #[test]
  fn increment_nonexistent_creates_counter() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    // Per CSS spec, incrementing non-existent counter creates it at 0 + increment
    manager.apply_increment(&CounterSet::single("chapter", 1));

    assert_eq!(manager.get("chapter"), Some(1));
  }

  #[test]
  fn increment_multiple_times() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 0));

    for _ in 0..10 {
      manager.apply_increment(&CounterSet::single("chapter", 1));
    }

    assert_eq!(manager.get("chapter"), Some(10));
  }

  #[test]
  fn set_existing_counter() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 0));
    manager.apply_set(&CounterSet::single("chapter", 100));

    assert_eq!(manager.get("chapter"), Some(100));
  }

  #[test]
  fn set_nonexistent_creates_counter() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_set(&CounterSet::single("chapter", 50));

    assert_eq!(manager.get("chapter"), Some(50));
  }
}

// ============================================================================
// Counter Scope Tests
// ============================================================================

mod counter_scopes {
  use super::*;

  #[test]
  fn scope_entry_and_exit() {
    let mut manager = CounterManager::new();
    assert_eq!(manager.depth(), 1);

    manager.enter_scope();
    assert_eq!(manager.depth(), 2);

    manager.enter_scope();
    assert_eq!(manager.depth(), 3);

    manager.leave_scope();
    assert_eq!(manager.depth(), 2);

    manager.leave_scope();
    assert_eq!(manager.depth(), 1);
  }

  #[test]
  fn scope_maintains_root_scope() {
    let mut manager = CounterManager::new();

    // Try to leave root scope multiple times
    for _ in 0..10 {
      manager.leave_scope();
    }

    assert_eq!(manager.depth(), 1); // Still have root scope
  }

  #[test]
  fn counter_visibility_from_parent_scope() {
    let mut manager = CounterManager::new();

    // Parent scope
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 5));

    // Child scope can see parent's counter
    manager.enter_scope();
    assert_eq!(manager.get("chapter"), Some(5));
  }

  #[test]
  fn counter_shadowing() {
    let mut manager = CounterManager::new();

    // Parent scope
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("item", 10));

    // Child scope shadows with new counter
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("item", 0));
    assert_eq!(manager.get("item"), Some(0));

    // Leave child - parent's counter visible again
    manager.leave_scope();
    assert_eq!(manager.get("item"), Some(10));
  }

  #[test]
  fn increment_modifies_innermost_counter() {
    let mut manager = CounterManager::new();

    // Outer scope
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("item", 0));

    // Inner scope without reset - increments parent
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("item", 1));
    assert_eq!(manager.get("item"), Some(1));

    // Leave inner - parent was modified
    manager.leave_scope();
    assert_eq!(manager.get("item"), Some(1));
  }

  #[test]
  fn nested_reset_creates_new_scope() {
    let mut manager = CounterManager::new();

    // Outer ol
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("item", 0));
    manager.apply_increment(&CounterSet::single("item", 1));
    assert_eq!(manager.get("item"), Some(1));

    // Nested ol - resets to 0
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("item", 0));
    manager.apply_increment(&CounterSet::single("item", 1));
    assert_eq!(manager.get("item"), Some(1)); // Inner scope's counter

    // Leave nested ol
    manager.leave_scope();
    assert_eq!(manager.get("item"), Some(1)); // Back to outer's counter
  }

  #[test]
  fn counter_set_modifies_parent() {
    let mut manager = CounterManager::new();

    // Parent scope
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 0));

    // Child scope uses counter-set
    manager.enter_scope();
    manager.apply_set(&CounterSet::single("chapter", 100));

    assert_eq!(manager.get("chapter"), Some(100));

    // Leave child - parent was modified
    manager.leave_scope();
    assert_eq!(manager.get("chapter"), Some(100));
  }
}

// ============================================================================
// get_all Tests (for counters() function)
// ============================================================================

mod get_all_tests {
  use super::*;

  #[test]
  fn get_all_empty() {
    let manager = CounterManager::new();
    assert!(manager.get_all("item").is_empty());
  }

  #[test]
  fn get_all_single_scope() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("item", 5));

    assert_eq!(manager.get_all("item"), vec![5]);
  }

  #[test]
  fn get_all_nested_scopes() {
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
  fn get_all_with_increments() {
    let mut manager = CounterManager::new();

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("section", 0));
    manager.apply_increment(&CounterSet::single("section", 1));
    manager.apply_increment(&CounterSet::single("section", 1));

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("section", 0));
    manager.apply_increment(&CounterSet::single("section", 1));

    assert_eq!(manager.get_all("section"), vec![2, 1]);
  }

  #[test]
  fn get_all_different_counters() {
    let mut manager = CounterManager::new();

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 1));
    manager.apply_reset(&CounterSet::single("section", 10));

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("section", 20));

    assert_eq!(manager.get_all("chapter"), vec![1]);
    assert_eq!(manager.get_all("section"), vec![10, 20]);
  }
}

// ============================================================================
// Formatting Tests
// ============================================================================

mod formatting_tests {
  use super::*;

  #[test]
  fn format_decimal() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 42));

    assert_eq!(manager.format("chapter", CounterStyle::Decimal), "42");
  }

  #[test]
  fn format_lower_roman() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 4));

    assert_eq!(manager.format("chapter", CounterStyle::LowerRoman), "iv");
  }

  #[test]
  fn format_upper_roman() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 1999));

    assert_eq!(
      manager.format("chapter", CounterStyle::UpperRoman),
      "MCMXCIX"
    );
  }

  #[test]
  fn format_lower_alpha() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 27));

    assert_eq!(manager.format("chapter", CounterStyle::LowerAlpha), "aa");
  }

  #[test]
  fn format_upper_alpha() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", 26));

    assert_eq!(manager.format("chapter", CounterStyle::UpperAlpha), "Z");
  }

  #[test]
  fn format_nonexistent_counter() {
    let manager = CounterManager::new();
    assert_eq!(manager.format("chapter", CounterStyle::Decimal), "0");
  }

  #[test]
  fn format_all_decimal() {
    let mut manager = CounterManager::new();

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("section", 1));

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("section", 2));

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("section", 3));

    assert_eq!(
      manager.format_all("section", ".", CounterStyle::Decimal),
      "1.2.3"
    );
  }

  #[test]
  fn format_all_with_custom_separator() {
    let mut manager = CounterManager::new();

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("section", 1));

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("section", 2));

    assert_eq!(
      manager.format_all("section", " - ", CounterStyle::Decimal),
      "1 - 2"
    );
  }

  #[test]
  fn format_all_lower_alpha() {
    let mut manager = CounterManager::new();

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("item", 1));

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("item", 2));

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("item", 3));

    assert_eq!(
      manager.format_all("item", ".", CounterStyle::LowerAlpha),
      "a.b.c"
    );
  }

  #[test]
  fn format_all_nonexistent() {
    let manager = CounterManager::new();
    assert_eq!(
      manager.format_all("nonexistent", ".", CounterStyle::Decimal),
      "0"
    );
  }
}

// ============================================================================
// CounterProperties Tests
// ============================================================================

mod counter_properties_tests {
  use super::*;

  #[test]
  fn default_has_no_properties() {
    let props = CounterProperties::new();
    assert!(!props.has_any());
  }

  #[test]
  fn has_any_with_reset() {
    let mut props = CounterProperties::new();
    props.counter_reset = Some(CounterSet::single("chapter", 0));
    assert!(props.has_any());
  }

  #[test]
  fn has_any_with_increment() {
    let mut props = CounterProperties::new();
    props.counter_increment = Some(CounterSet::single("chapter", 1));
    assert!(props.has_any());
  }

  #[test]
  fn has_any_with_set() {
    let mut props = CounterProperties::new();
    props.counter_set = Some(CounterSet::single("chapter", 5));
    assert!(props.has_any());
  }

  #[test]
  fn apply_to_reset_only() {
    let mut props = CounterProperties::new();
    props.counter_reset = Some(CounterSet::single("chapter", 10));

    let mut manager = CounterManager::new();
    manager.enter_scope();
    props.apply_to(&mut manager);

    assert_eq!(manager.get("chapter"), Some(10));
  }

  #[test]
  fn apply_to_increment_only() {
    let mut props = CounterProperties::new();
    props.counter_increment = Some(CounterSet::single("chapter", 5));

    let mut manager = CounterManager::new();
    manager.enter_scope();
    props.apply_to(&mut manager);

    // Increment on non-existent creates with 0 + increment
    assert_eq!(manager.get("chapter"), Some(5));
  }

  #[test]
  fn apply_to_order_reset_then_increment() {
    // CSS spec says reset happens before increment
    let mut props = CounterProperties::new();
    props.counter_reset = Some(CounterSet::single("chapter", 0));
    props.counter_increment = Some(CounterSet::single("chapter", 1));

    let mut manager = CounterManager::new();
    manager.enter_scope();
    props.apply_to(&mut manager);

    assert_eq!(manager.get("chapter"), Some(1));
  }

  #[test]
  fn apply_to_order_reset_set_increment() {
    let mut props = CounterProperties::new();
    props.counter_reset = Some(CounterSet::single("chapter", 0));
    props.counter_set = Some(CounterSet::single("chapter", 10));
    props.counter_increment = Some(CounterSet::single("chapter", 1));

    let mut manager = CounterManager::new();
    manager.enter_scope();
    props.apply_to(&mut manager);

    assert_eq!(manager.get("chapter"), Some(11));
  }
}

// ============================================================================
// Real-World Usage Scenarios
// ============================================================================

mod real_world_scenarios {
  use super::*;

  /// Simulates a typical document with chapters and sections where sections
  /// are nested inside chapter containers:
  ///
  /// body { counter-reset: chapter; }
  /// .chapter { counter-increment: chapter; counter-reset: section; }
  /// .section { counter-increment: section; }
  ///
  /// <body>
  ///   <div class="chapter">
  ///     <h1>Chapter 1</h1>
  ///     <div class="section"><h2>1.1</h2></div>
  ///     <div class="section"><h2>1.2</h2></div>
  ///   </div>
  ///   <div class="chapter">
  ///     <h1>Chapter 2</h1>
  ///     <div class="section"><h2>2.1</h2></div>
  ///   </div>
  /// </body>
  #[test]
  fn document_chapter_section_numbering() {
    let mut manager = CounterManager::new();

    // Enter body - reset counters
    manager.enter_scope();
    manager.apply_reset(&CounterSet::parse_reset("chapter").unwrap());

    // Chapter 1 container
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("chapter", 1));
    manager.apply_reset(&CounterSet::single("section", 0));
    assert_eq!(manager.format("chapter", CounterStyle::Decimal), "1");

    // Section 1.1
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("section", 1));
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
    manager.apply_increment(&CounterSet::single("section", 1));
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
    manager.apply_increment(&CounterSet::single("chapter", 1));
    manager.apply_reset(&CounterSet::single("section", 0));
    assert_eq!(manager.format("chapter", CounterStyle::Decimal), "2");

    // Section 2.1
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("section", 1));
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

  /// Simulates nested ordered list numbering:
  /// ol { counter-reset: item; }
  /// li { counter-increment: item; }
  /// li::before { content: counters(item, "."); }
  #[test]
  fn nested_ordered_list_numbering() {
    let mut manager = CounterManager::new();

    // First ol
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("item", 0));

    // li 1
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("item", 1));
    assert_eq!(manager.format_all("item", ".", CounterStyle::Decimal), "1");

    // Nested ol inside li 1
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("item", 0));

    // Nested li 1.1
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("item", 1));
    assert_eq!(
      manager.format_all("item", ".", CounterStyle::Decimal),
      "1.1"
    );
    manager.leave_scope();

    // Nested li 1.2
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("item", 1));
    assert_eq!(
      manager.format_all("item", ".", CounterStyle::Decimal),
      "1.2"
    );

    // Doubly nested ol
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("item", 0));

    // Nested li 1.2.1
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("item", 1));
    assert_eq!(
      manager.format_all("item", ".", CounterStyle::Decimal),
      "1.2.1"
    );
    manager.leave_scope();

    // Nested li 1.2.2
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("item", 1));
    assert_eq!(
      manager.format_all("item", ".", CounterStyle::Decimal),
      "1.2.2"
    );
    manager.leave_scope();

    manager.leave_scope(); // doubly nested ol
    manager.leave_scope(); // nested li 1.2
    manager.leave_scope(); // nested ol
    manager.leave_scope(); // li 1

    // li 2
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("item", 1));
    assert_eq!(manager.format_all("item", ".", CounterStyle::Decimal), "2");
    manager.leave_scope();

    // li 3
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("item", 1));
    assert_eq!(manager.format_all("item", ".", CounterStyle::Decimal), "3");
    manager.leave_scope();

    manager.leave_scope(); // first ol
  }

  /// Simulates figure numbering per section
  #[test]
  fn figure_numbering_per_section() {
    let mut manager = CounterManager::new();

    // Document
    manager.enter_scope();
    manager.apply_reset(&CounterSet::parse_reset("section figure").unwrap());

    // Section 1
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("section", 1));
    manager.apply_reset(&CounterSet::single("figure", 0));

    // Figure 1.1
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("figure", 1));
    assert_eq!(
      format!(
        "Figure {}.{}",
        manager.format("section", CounterStyle::Decimal),
        manager.format("figure", CounterStyle::Decimal)
      ),
      "Figure 1.1"
    );
    manager.leave_scope();

    // Figure 1.2
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("figure", 1));
    assert_eq!(
      format!(
        "Figure {}.{}",
        manager.format("section", CounterStyle::Decimal),
        manager.format("figure", CounterStyle::Decimal)
      ),
      "Figure 1.2"
    );
    manager.leave_scope();

    manager.leave_scope(); // Section 1

    // Section 2
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("section", 1));
    manager.apply_reset(&CounterSet::single("figure", 0));

    // Figure 2.1
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("figure", 1));
    assert_eq!(
      format!(
        "Figure {}.{}",
        manager.format("section", CounterStyle::Decimal),
        manager.format("figure", CounterStyle::Decimal)
      ),
      "Figure 2.1"
    );
    manager.leave_scope();

    manager.leave_scope(); // Section 2
    manager.leave_scope(); // Document
  }

  /// Simulates list-item counter (HTML built-in)
  #[test]
  fn list_item_counter() {
    let mut manager = CounterManager::new();

    // ol start="5"
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("list-item", 4)); // start-1 because increment happens first

    // li
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("list-item", 1));
    assert_eq!(manager.format("list-item", CounterStyle::Decimal), "5");
    manager.leave_scope();

    // li
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("list-item", 1));
    assert_eq!(manager.format("list-item", CounterStyle::Decimal), "6");
    manager.leave_scope();

    // li value="10"
    manager.enter_scope();
    manager.apply_set(&CounterSet::single("list-item", 10));
    assert_eq!(manager.format("list-item", CounterStyle::Decimal), "10");
    manager.leave_scope();

    // li (continues from 10)
    manager.enter_scope();
    manager.apply_increment(&CounterSet::single("list-item", 1));
    assert_eq!(manager.format("list-item", CounterStyle::Decimal), "11");
    manager.leave_scope();

    manager.leave_scope();
  }

  /// Simulates footnote numbering with different styles
  #[test]
  fn footnote_numbering() {
    let mut manager = CounterManager::new();

    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("footnote", 0));

    // Footnote markers using different styles
    for i in 1..=5 {
      manager.enter_scope();
      manager.apply_increment(&CounterSet::single("footnote", 1));

      // Different styles
      let decimal = manager.format("footnote", CounterStyle::Decimal);
      let lower_alpha = manager.format("footnote", CounterStyle::LowerAlpha);
      let lower_roman = manager.format("footnote", CounterStyle::LowerRoman);

      match i {
        1 => {
          assert_eq!(decimal, "1");
          assert_eq!(lower_alpha, "a");
          assert_eq!(lower_roman, "i");
        }
        2 => {
          assert_eq!(decimal, "2");
          assert_eq!(lower_alpha, "b");
          assert_eq!(lower_roman, "ii");
        }
        3 => {
          assert_eq!(decimal, "3");
          assert_eq!(lower_alpha, "c");
          assert_eq!(lower_roman, "iii");
        }
        4 => {
          assert_eq!(decimal, "4");
          assert_eq!(lower_alpha, "d");
          assert_eq!(lower_roman, "iv");
        }
        5 => {
          assert_eq!(decimal, "5");
          assert_eq!(lower_alpha, "e");
          assert_eq!(lower_roman, "v");
        }
        _ => {}
      }

      manager.leave_scope();
    }

    manager.leave_scope();
  }

  /// Simulates reversed counter (countdown)
  #[test]
  fn reversed_countdown() {
    let mut manager = CounterManager::new();

    // Start at 10
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("countdown", 10));

    for expected in (1..=10).rev() {
      manager.enter_scope();
      // Decrement (negative increment)
      // No decrement needed for first read since we start at 10
      if expected < 10 {
        // The counter already shows correct value, we decrement after reading
      }
      assert_eq!(manager.get("countdown"), Some(expected));
      manager.apply_increment(&CounterSet::single("countdown", -1));
      manager.leave_scope();
    }

    assert_eq!(manager.get("countdown"), Some(0));
    manager.leave_scope();
  }
}

// ============================================================================
// Edge Cases and Error Handling
// ============================================================================

mod edge_cases {
  use super::*;

  #[test]
  fn large_counter_values() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", i32::MAX - 1));
    manager.apply_increment(&CounterSet::single("chapter", 1));

    assert_eq!(manager.get("chapter"), Some(i32::MAX));
  }

  #[test]
  fn negative_counter_values() {
    let mut manager = CounterManager::new();
    manager.enter_scope();
    manager.apply_reset(&CounterSet::single("chapter", -100));
    manager.apply_increment(&CounterSet::single("chapter", -1));

    assert_eq!(manager.get("chapter"), Some(-101));
  }

  #[test]
  fn many_nested_scopes() {
    let mut manager = CounterManager::new();

    // Create deeply nested structure
    for i in 1..=100 {
      manager.enter_scope();
      manager.apply_reset(&CounterSet::single("depth", i));
    }

    assert_eq!(manager.depth(), 101); // root + 100
    assert_eq!(manager.get_all("depth").len(), 100);

    // Verify deepest value
    assert_eq!(manager.get("depth"), Some(100));

    // Unwind
    for i in (1..=100).rev() {
      assert_eq!(manager.get("depth"), Some(i));
      manager.leave_scope();
    }

    assert_eq!(manager.depth(), 1);
  }

  #[test]
  fn counter_name_edge_cases() {
    let mut manager = CounterManager::new();
    manager.enter_scope();

    // Very long name
    let long_name = "a".repeat(1000);
    manager.apply_reset(&CounterSet::single(&long_name, 1));
    assert_eq!(manager.get(&long_name), Some(1));

    // Unicode name (though not valid CSS, test doesn't crash)
    manager.apply_reset(&CounterSet::single("counter_日本語", 2));
    assert_eq!(manager.get("counter_日本語"), Some(2));
  }

  #[test]
  fn empty_counter_set_operations() {
    let mut manager = CounterManager::new();
    manager.enter_scope();

    // Empty operations should not crash
    manager.apply_reset(&CounterSet::new());
    manager.apply_increment(&CounterSet::new());
    manager.apply_set(&CounterSet::new());

    assert!(!manager.has("anything"));
  }

  #[test]
  fn default_trait() {
    let manager = CounterManager::default();
    assert_eq!(manager.depth(), 1);

    let set = CounterSet::default();
    assert!(set.is_empty());

    let props = CounterProperties::default();
    assert!(!props.has_any());
  }
}
