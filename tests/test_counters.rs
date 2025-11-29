//! Integration tests for CSS counter system
//!
//! These tests verify the counter system implementation against
//! CSS specifications and real-world usage patterns.

use fastrender::style::counters::{
    CounterIncrement, CounterReset, CounterSet, CounterStyle, CounterSystem,
};

// ============================================================================
// CounterStyle Format Tests
// ============================================================================

mod counter_style {
    use super::*;

    #[test]
    fn decimal_format() {
        let style = CounterStyle::Decimal;
        assert_eq!(style.format(0), "0");
        assert_eq!(style.format(1), "1");
        assert_eq!(style.format(10), "10");
        assert_eq!(style.format(100), "100");
        assert_eq!(style.format(999), "999");
        assert_eq!(style.format(-1), "-1");
        assert_eq!(style.format(-999), "-999");
    }

    #[test]
    fn decimal_leading_zero_format() {
        let style = CounterStyle::DecimalLeadingZero;
        assert_eq!(style.format(0), "00");
        assert_eq!(style.format(1), "01");
        assert_eq!(style.format(9), "09");
        assert_eq!(style.format(10), "10");
        assert_eq!(style.format(99), "99");
        assert_eq!(style.format(-1), "-01");
        assert_eq!(style.format(-9), "-09");
        assert_eq!(style.format(-10), "-10");
    }

    #[test]
    fn lower_alpha_format() {
        let style = CounterStyle::LowerAlpha;

        // Basic letters
        assert_eq!(style.format(1), "a");
        assert_eq!(style.format(2), "b");
        assert_eq!(style.format(3), "c");
        assert_eq!(style.format(26), "z");

        // Double letters
        assert_eq!(style.format(27), "aa");
        assert_eq!(style.format(28), "ab");
        assert_eq!(style.format(52), "az");
        assert_eq!(style.format(53), "ba");

        // Triple letters
        assert_eq!(style.format(702), "zz");
        assert_eq!(style.format(703), "aaa");

        // Out of range fallback
        assert_eq!(style.format(0), "0");
        assert_eq!(style.format(-1), "-1");
    }

    #[test]
    fn upper_alpha_format() {
        let style = CounterStyle::UpperAlpha;
        assert_eq!(style.format(1), "A");
        assert_eq!(style.format(26), "Z");
        assert_eq!(style.format(27), "AA");
        assert_eq!(style.format(52), "AZ");
    }

    #[test]
    fn lower_roman_format() {
        let style = CounterStyle::LowerRoman;

        // Basic numerals
        assert_eq!(style.format(1), "i");
        assert_eq!(style.format(2), "ii");
        assert_eq!(style.format(3), "iii");
        assert_eq!(style.format(4), "iv");
        assert_eq!(style.format(5), "v");
        assert_eq!(style.format(6), "vi");
        assert_eq!(style.format(7), "vii");
        assert_eq!(style.format(8), "viii");
        assert_eq!(style.format(9), "ix");
        assert_eq!(style.format(10), "x");

        // Larger numbers
        assert_eq!(style.format(40), "xl");
        assert_eq!(style.format(50), "l");
        assert_eq!(style.format(90), "xc");
        assert_eq!(style.format(100), "c");
        assert_eq!(style.format(400), "cd");
        assert_eq!(style.format(500), "d");
        assert_eq!(style.format(900), "cm");
        assert_eq!(style.format(1000), "m");

        // Complex numbers
        assert_eq!(style.format(1994), "mcmxciv");
        assert_eq!(style.format(2024), "mmxxiv");
        assert_eq!(style.format(3999), "mmmcmxcix");

        // Out of range fallback
        assert_eq!(style.format(0), "0");
        assert_eq!(style.format(-1), "-1");
        assert_eq!(style.format(4000), "4000");
    }

    #[test]
    fn upper_roman_format() {
        let style = CounterStyle::UpperRoman;
        assert_eq!(style.format(1), "I");
        assert_eq!(style.format(4), "IV");
        assert_eq!(style.format(9), "IX");
        assert_eq!(style.format(1994), "MCMXCIV");
    }

    #[test]
    fn lower_greek_format() {
        let style = CounterStyle::LowerGreek;
        assert_eq!(style.format(1), "α");
        assert_eq!(style.format(2), "β");
        assert_eq!(style.format(3), "γ");
        assert_eq!(style.format(4), "δ");
        assert_eq!(style.format(5), "ε");
        assert_eq!(style.format(24), "ω");

        // Out of range fallback
        assert_eq!(style.format(0), "0");
        assert_eq!(style.format(25), "25");
        assert_eq!(style.format(-1), "-1");
    }

    #[test]
    fn none_and_markers() {
        assert_eq!(CounterStyle::None.format(1), "");
        assert_eq!(CounterStyle::None.format(100), "");
        assert_eq!(CounterStyle::Disc.format(1), "•");
        assert_eq!(CounterStyle::Circle.format(1), "◦");
        assert_eq!(CounterStyle::Square.format(1), "▪");
    }

    #[test]
    fn parse_style_keywords() {
        assert_eq!(CounterStyle::parse("decimal"), Some(CounterStyle::Decimal));
        assert_eq!(
            CounterStyle::parse("decimal-leading-zero"),
            Some(CounterStyle::DecimalLeadingZero)
        );
        assert_eq!(
            CounterStyle::parse("lower-alpha"),
            Some(CounterStyle::LowerAlpha)
        );
        assert_eq!(
            CounterStyle::parse("lower-latin"),
            Some(CounterStyle::LowerAlpha)
        );
        assert_eq!(
            CounterStyle::parse("upper-alpha"),
            Some(CounterStyle::UpperAlpha)
        );
        assert_eq!(
            CounterStyle::parse("upper-latin"),
            Some(CounterStyle::UpperAlpha)
        );
        assert_eq!(
            CounterStyle::parse("lower-roman"),
            Some(CounterStyle::LowerRoman)
        );
        assert_eq!(
            CounterStyle::parse("upper-roman"),
            Some(CounterStyle::UpperRoman)
        );
        assert_eq!(
            CounterStyle::parse("lower-greek"),
            Some(CounterStyle::LowerGreek)
        );
        assert_eq!(CounterStyle::parse("none"), Some(CounterStyle::None));
        assert_eq!(CounterStyle::parse("disc"), Some(CounterStyle::Disc));
        assert_eq!(CounterStyle::parse("circle"), Some(CounterStyle::Circle));
        assert_eq!(CounterStyle::parse("square"), Some(CounterStyle::Square));
    }

    #[test]
    fn parse_case_insensitive() {
        assert_eq!(
            CounterStyle::parse("DECIMAL"),
            Some(CounterStyle::Decimal)
        );
        assert_eq!(
            CounterStyle::parse("Lower-Roman"),
            Some(CounterStyle::LowerRoman)
        );
        assert_eq!(
            CounterStyle::parse("UPPER-ALPHA"),
            Some(CounterStyle::UpperAlpha)
        );
    }

    #[test]
    fn parse_invalid() {
        assert_eq!(CounterStyle::parse("invalid"), None);
        assert_eq!(CounterStyle::parse("roman"), None);
        assert_eq!(CounterStyle::parse("alpha"), None);
        assert_eq!(CounterStyle::parse(""), None);
    }

    #[test]
    fn display_format() {
        assert_eq!(format!("{}", CounterStyle::Decimal), "decimal");
        assert_eq!(
            format!("{}", CounterStyle::DecimalLeadingZero),
            "decimal-leading-zero"
        );
        assert_eq!(format!("{}", CounterStyle::LowerAlpha), "lower-alpha");
        assert_eq!(format!("{}", CounterStyle::UpperAlpha), "upper-alpha");
        assert_eq!(format!("{}", CounterStyle::LowerRoman), "lower-roman");
        assert_eq!(format!("{}", CounterStyle::UpperRoman), "upper-roman");
        assert_eq!(format!("{}", CounterStyle::LowerGreek), "lower-greek");
        assert_eq!(format!("{}", CounterStyle::None), "none");
    }

    #[test]
    fn default_is_decimal() {
        assert_eq!(CounterStyle::default(), CounterStyle::Decimal);
    }
}

// ============================================================================
// CounterSystem Tests
// ============================================================================

mod counter_system {
    use super::*;

    #[test]
    fn new_system_has_root_scope() {
        let system = CounterSystem::new();
        assert_eq!(system.scope_depth(), 1);
    }

    #[test]
    fn undefined_counter_returns_zero() {
        let system = CounterSystem::new();
        assert_eq!(system.get_counter("undefined"), 0);
        assert_eq!(system.get_counter("any_name"), 0);
    }

    #[test]
    fn reset_creates_counter() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 0);
        assert_eq!(system.get_counter("section"), 0);
        assert!(system.has_counter("section"));
    }

    #[test]
    fn reset_with_initial_value() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 10);
        assert_eq!(system.get_counter("section"), 10);
    }

    #[test]
    fn reset_overwrites_existing() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 5);
        system.reset_counter("section", 10);
        assert_eq!(system.get_counter("section"), 10);
    }

    #[test]
    fn increment_adds_to_counter() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 0);

        system.increment_counter("section", 1);
        assert_eq!(system.get_counter("section"), 1);

        system.increment_counter("section", 1);
        assert_eq!(system.get_counter("section"), 2);

        system.increment_counter("section", 5);
        assert_eq!(system.get_counter("section"), 7);
    }

    #[test]
    fn increment_with_negative() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 10);

        system.increment_counter("section", -3);
        assert_eq!(system.get_counter("section"), 7);

        system.increment_counter("section", -10);
        assert_eq!(system.get_counter("section"), -3);
    }

    #[test]
    fn increment_creates_counter_if_missing() {
        let mut system = CounterSystem::new();
        system.increment_counter("section", 5);
        assert_eq!(system.get_counter("section"), 5);
    }

    #[test]
    fn set_counter_updates_value() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 0);
        system.increment_counter("section", 5);

        system.set_counter("section", 100);
        assert_eq!(system.get_counter("section"), 100);
    }

    #[test]
    fn set_counter_creates_if_missing() {
        let mut system = CounterSystem::new();
        system.set_counter("section", 42);
        assert_eq!(system.get_counter("section"), 42);
    }

    #[test]
    fn push_pop_scope() {
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
    }

    #[test]
    fn cannot_pop_root_scope() {
        let mut system = CounterSystem::new();
        system.pop_scope();
        system.pop_scope();
        system.pop_scope();
        assert_eq!(system.scope_depth(), 1);
    }

    #[test]
    fn child_scope_inherits_counter_lookup() {
        let mut system = CounterSystem::new();
        system.reset_counter("parent", 10);

        system.push_scope();
        // Can read parent counter
        assert_eq!(system.get_counter("parent"), 10);
    }

    #[test]
    fn child_scope_increment_affects_parent_counter() {
        let mut system = CounterSystem::new();
        system.reset_counter("parent", 0);

        system.push_scope();
        system.increment_counter("parent", 5);

        system.pop_scope();
        // Increment persisted to parent scope
        assert_eq!(system.get_counter("parent"), 5);
    }

    #[test]
    fn child_scope_reset_shadows_parent() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 100);

        system.push_scope();
        system.reset_counter("section", 1); // Shadows parent
        assert_eq!(system.get_counter("section"), 1);

        system.pop_scope();
        // Parent counter unchanged
        assert_eq!(system.get_counter("section"), 100);
    }

    #[test]
    fn child_scope_counter_discarded_on_pop() {
        let mut system = CounterSystem::new();

        system.push_scope();
        system.reset_counter("local", 42);
        assert_eq!(system.get_counter("local"), 42);

        system.pop_scope();
        assert_eq!(system.get_counter("local"), 0); // No longer exists
    }

    #[test]
    fn get_counters_returns_all_scope_values() {
        let mut system = CounterSystem::new();

        system.reset_counter("section", 0);
        system.increment_counter("section", 1);

        system.push_scope();
        system.reset_counter("section", 0);
        system.increment_counter("section", 2);

        system.push_scope();
        system.reset_counter("section", 0);
        system.increment_counter("section", 3);

        assert_eq!(system.get_counters("section"), vec![1, 2, 3]);
    }

    #[test]
    fn get_counters_empty_if_no_counter() {
        let system = CounterSystem::new();
        assert_eq!(system.get_counters("undefined"), Vec::<i32>::new());
    }

    #[test]
    fn format_counter() {
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
    fn format_counters_with_separator() {
        let mut system = CounterSystem::new();

        system.reset_counter("item", 0);
        system.increment_counter("item", 1);

        system.push_scope();
        system.reset_counter("item", 0);
        system.increment_counter("item", 2);

        system.push_scope();
        system.reset_counter("item", 0);
        system.increment_counter("item", 3);

        assert_eq!(
            system.format_counters("item", ".", CounterStyle::Decimal),
            "1.2.3"
        );
        assert_eq!(
            system.format_counters("item", "-", CounterStyle::Decimal),
            "1-2-3"
        );
        assert_eq!(
            system.format_counters("item", " > ", CounterStyle::Decimal),
            "1 > 2 > 3"
        );
    }

    #[test]
    fn format_counters_single_level() {
        let mut system = CounterSystem::new();
        system.reset_counter("item", 0);
        system.increment_counter("item", 5);

        assert_eq!(
            system.format_counters("item", ".", CounterStyle::Decimal),
            "5"
        );
    }

    #[test]
    fn has_counter() {
        let mut system = CounterSystem::new();
        assert!(!system.has_counter("section"));

        system.reset_counter("section", 0);
        assert!(system.has_counter("section"));

        system.push_scope();
        assert!(system.has_counter("section")); // Inherited

        system.reset_counter("local", 1);
        assert!(system.has_counter("local"));

        system.pop_scope();
        assert!(!system.has_counter("local")); // Gone
    }

    #[test]
    fn clear_resets_system() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 5);
        system.push_scope();
        system.push_scope();

        system.clear();

        assert_eq!(system.scope_depth(), 1);
        assert_eq!(system.get_counter("section"), 0);
        assert!(!system.has_counter("section"));
    }

    #[test]
    fn default_trait() {
        let system = CounterSystem::default();
        assert_eq!(system.scope_depth(), 1);
    }

    #[test]
    fn clone_trait() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 5);
        system.push_scope();
        system.reset_counter("local", 10);

        let cloned = system.clone();
        assert_eq!(cloned.get_counter("section"), 5);
        assert_eq!(cloned.get_counter("local"), 10);
        assert_eq!(cloned.scope_depth(), 2);
    }
}

// ============================================================================
// Property Parsing Tests
// ============================================================================

mod counter_reset_parsing {
    use super::*;

    #[test]
    fn parse_none() {
        let reset = CounterReset::parse("none").unwrap();
        assert!(reset.counters.is_empty());
    }

    #[test]
    fn parse_single_counter_default_value() {
        let reset = CounterReset::parse("section").unwrap();
        assert_eq!(reset.counters.len(), 1);
        assert_eq!(reset.counters[0], ("section".to_string(), 0));
    }

    #[test]
    fn parse_single_counter_with_value() {
        let reset = CounterReset::parse("section 5").unwrap();
        assert_eq!(reset.counters, vec![("section".to_string(), 5)]);
    }

    #[test]
    fn parse_multiple_counters() {
        let reset = CounterReset::parse("section 0 chapter 1 item").unwrap();
        assert_eq!(reset.counters.len(), 3);
        assert_eq!(reset.counters[0], ("section".to_string(), 0));
        assert_eq!(reset.counters[1], ("chapter".to_string(), 1));
        assert_eq!(reset.counters[2], ("item".to_string(), 0));
    }

    #[test]
    fn parse_negative_value() {
        let reset = CounterReset::parse("section -5").unwrap();
        assert_eq!(reset.counters, vec![("section".to_string(), -5)]);
    }

    #[test]
    fn parse_whitespace_handling() {
        let reset = CounterReset::parse("  section   5   chapter   ").unwrap();
        assert_eq!(reset.counters.len(), 2);
        assert_eq!(reset.counters[0], ("section".to_string(), 5));
        assert_eq!(reset.counters[1], ("chapter".to_string(), 0));
    }

    #[test]
    fn parse_invalid_counter_name() {
        // Names can't start with digit
        assert!(CounterReset::parse("123section").is_none());
        // Can't use CSS keywords
        assert!(CounterReset::parse("inherit").is_none());
        assert!(CounterReset::parse("initial").is_none());
    }

    #[test]
    fn parse_empty_returns_none() {
        assert!(CounterReset::parse("").is_none());
        assert!(CounterReset::parse("   ").is_none());
    }

    #[test]
    fn apply_to_system() {
        let reset = CounterReset::parse("section 0 chapter 5").unwrap();
        let mut system = CounterSystem::new();

        reset.apply(&mut system);

        assert_eq!(system.get_counter("section"), 0);
        assert_eq!(system.get_counter("chapter"), 5);
    }

    #[test]
    fn default_is_none() {
        let reset = CounterReset::default();
        assert!(reset.counters.is_empty());
    }
}

mod counter_increment_parsing {
    use super::*;

    #[test]
    fn parse_none() {
        let incr = CounterIncrement::parse("none").unwrap();
        assert!(incr.counters.is_empty());
    }

    #[test]
    fn parse_single_counter_default_increment() {
        let incr = CounterIncrement::parse("section").unwrap();
        assert_eq!(incr.counters, vec![("section".to_string(), 1)]);
    }

    #[test]
    fn parse_single_counter_with_increment() {
        let incr = CounterIncrement::parse("section 2").unwrap();
        assert_eq!(incr.counters, vec![("section".to_string(), 2)]);
    }

    #[test]
    fn parse_multiple_counters() {
        let incr = CounterIncrement::parse("section chapter 2").unwrap();
        assert_eq!(incr.counters.len(), 2);
        assert_eq!(incr.counters[0], ("section".to_string(), 1));
        assert_eq!(incr.counters[1], ("chapter".to_string(), 2));
    }

    #[test]
    fn parse_negative_increment() {
        let incr = CounterIncrement::parse("section -1").unwrap();
        assert_eq!(incr.counters, vec![("section".to_string(), -1)]);
    }

    #[test]
    fn apply_to_system() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 0);

        let incr = CounterIncrement::parse("section 3").unwrap();
        incr.apply(&mut system);

        assert_eq!(system.get_counter("section"), 3);

        incr.apply(&mut system);
        assert_eq!(system.get_counter("section"), 6);
    }
}

mod counter_set_parsing {
    use super::*;

    #[test]
    fn parse_none() {
        let set = CounterSet::parse("none").unwrap();
        assert!(set.counters.is_empty());
    }

    #[test]
    fn parse_single_counter() {
        let set = CounterSet::parse("section 10").unwrap();
        assert_eq!(set.counters, vec![("section".to_string(), 10)]);
    }

    #[test]
    fn parse_without_value() {
        let set = CounterSet::parse("section").unwrap();
        assert_eq!(set.counters, vec![("section".to_string(), 0)]);
    }

    #[test]
    fn apply_to_system() {
        let mut system = CounterSystem::new();
        system.reset_counter("section", 0);
        system.increment_counter("section", 5);

        let set = CounterSet::parse("section 100").unwrap();
        set.apply(&mut system);

        assert_eq!(system.get_counter("section"), 100);
    }
}

// ============================================================================
// Real-World Usage Patterns
// ============================================================================

mod integration {
    use super::*;

    /// Test typical document outline numbering
    /// body { counter-reset: chapter; }
    /// h1 { counter-increment: chapter; counter-reset: section; }
    /// h2 { counter-increment: section; }
    /// h1::before { content: counter(chapter) ". "; }
    /// h2::before { content: counter(chapter) "." counter(section) " "; }
    #[test]
    fn document_outline_numbering() {
        let mut system = CounterSystem::new();

        // body { counter-reset: chapter; }
        system.reset_counter("chapter", 0);

        // First h1
        system.increment_counter("chapter", 1);
        system.reset_counter("section", 0);
        assert_eq!(system.format_counter("chapter", CounterStyle::Decimal), "1");

        // First h2 under h1
        system.increment_counter("section", 1);
        assert_eq!(
            format!(
                "{}.{}",
                system.format_counter("chapter", CounterStyle::Decimal),
                system.format_counter("section", CounterStyle::Decimal)
            ),
            "1.1"
        );

        // Second h2
        system.increment_counter("section", 1);
        assert_eq!(
            format!(
                "{}.{}",
                system.format_counter("chapter", CounterStyle::Decimal),
                system.format_counter("section", CounterStyle::Decimal)
            ),
            "1.2"
        );

        // Second h1
        system.increment_counter("chapter", 1);
        system.reset_counter("section", 0);
        assert_eq!(system.format_counter("chapter", CounterStyle::Decimal), "2");

        // h2 under second h1
        system.increment_counter("section", 1);
        assert_eq!(
            format!(
                "{}.{}",
                system.format_counter("chapter", CounterStyle::Decimal),
                system.format_counter("section", CounterStyle::Decimal)
            ),
            "2.1"
        );
    }

    /// Test nested list numbering using counters()
    /// ol { counter-reset: item; }
    /// li { counter-increment: item; }
    /// li::before { content: counters(item, ".") " "; }
    #[test]
    fn nested_list_numbering() {
        let mut system = CounterSystem::new();

        // First ol
        system.reset_counter("item", 0);

        // li: 1
        system.increment_counter("item", 1);
        assert_eq!(system.format_counters("item", ".", CounterStyle::Decimal), "1");

        // Nested ol
        system.push_scope();
        system.reset_counter("item", 0);

        // Nested li: 1.1
        system.increment_counter("item", 1);
        assert_eq!(system.format_counters("item", ".", CounterStyle::Decimal), "1.1");

        // Nested li: 1.2
        system.increment_counter("item", 1);
        assert_eq!(system.format_counters("item", ".", CounterStyle::Decimal), "1.2");

        // Deeply nested ol
        system.push_scope();
        system.reset_counter("item", 0);

        // li: 1.2.1
        system.increment_counter("item", 1);
        assert_eq!(system.format_counters("item", ".", CounterStyle::Decimal), "1.2.1");

        // li: 1.2.2
        system.increment_counter("item", 1);
        assert_eq!(system.format_counters("item", ".", CounterStyle::Decimal), "1.2.2");

        // Pop deep nesting
        system.pop_scope();

        // li: 1.3
        system.increment_counter("item", 1);
        assert_eq!(system.format_counters("item", ".", CounterStyle::Decimal), "1.3");

        // Pop to first level
        system.pop_scope();

        // li: 2
        system.increment_counter("item", 1);
        assert_eq!(system.format_counters("item", ".", CounterStyle::Decimal), "2");
    }

    /// Test Roman numeral appendix numbering
    /// appendix { counter-reset: appendix; }
    /// appendix > h1 { counter-increment: appendix; }
    /// appendix > h1::before { content: "Appendix " counter(appendix, upper-roman) ": "; }
    #[test]
    fn roman_numeral_appendix() {
        let mut system = CounterSystem::new();
        system.reset_counter("appendix", 0);

        for i in 1..=5 {
            system.increment_counter("appendix", 1);
            let expected = match i {
                1 => "I",
                2 => "II",
                3 => "III",
                4 => "IV",
                5 => "V",
                _ => unreachable!(),
            };
            assert_eq!(
                system.format_counter("appendix", CounterStyle::UpperRoman),
                expected
            );
        }
    }

    /// Test alphabetic footnote markers
    /// .footnote { counter-increment: footnote; }
    /// .footnote::before { content: counter(footnote, lower-alpha) ") "; }
    #[test]
    fn alphabetic_footnotes() {
        let mut system = CounterSystem::new();
        system.reset_counter("footnote", 0);

        let expected = ["a", "b", "c", "d", "e"];
        for exp in expected {
            system.increment_counter("footnote", 1);
            assert_eq!(
                system.format_counter("footnote", CounterStyle::LowerAlpha),
                exp
            );
        }
    }

    /// Test multiple independent counters
    /// figure { counter-increment: figure; }
    /// table { counter-increment: table; }
    /// figure::before { content: "Figure " counter(figure); }
    /// table::before { content: "Table " counter(table); }
    #[test]
    fn multiple_independent_counters() {
        let mut system = CounterSystem::new();
        system.reset_counter("figure", 0);
        system.reset_counter("table", 0);

        // Figure 1
        system.increment_counter("figure", 1);
        assert_eq!(system.format_counter("figure", CounterStyle::Decimal), "1");
        assert_eq!(system.format_counter("table", CounterStyle::Decimal), "0");

        // Table 1
        system.increment_counter("table", 1);
        assert_eq!(system.format_counter("figure", CounterStyle::Decimal), "1");
        assert_eq!(system.format_counter("table", CounterStyle::Decimal), "1");

        // Figure 2
        system.increment_counter("figure", 1);
        assert_eq!(system.format_counter("figure", CounterStyle::Decimal), "2");
        assert_eq!(system.format_counter("table", CounterStyle::Decimal), "1");

        // Table 2
        system.increment_counter("table", 1);
        assert_eq!(system.format_counter("figure", CounterStyle::Decimal), "2");
        assert_eq!(system.format_counter("table", CounterStyle::Decimal), "2");
    }

    /// Test counter with custom increment value
    /// .step { counter-increment: step 10; }
    #[test]
    fn custom_increment_value() {
        let mut system = CounterSystem::new();
        system.reset_counter("step", 0);

        system.increment_counter("step", 10);
        assert_eq!(system.get_counter("step"), 10);

        system.increment_counter("step", 10);
        assert_eq!(system.get_counter("step"), 20);

        system.increment_counter("step", 10);
        assert_eq!(system.get_counter("step"), 30);
    }

    /// Test counter decrement (negative increment)
    /// .countdown { counter-increment: countdown -1; }
    #[test]
    fn counter_decrement() {
        let mut system = CounterSystem::new();
        system.reset_counter("countdown", 10);

        system.increment_counter("countdown", -1);
        assert_eq!(system.get_counter("countdown"), 9);

        system.increment_counter("countdown", -1);
        assert_eq!(system.get_counter("countdown"), 8);

        system.increment_counter("countdown", -5);
        assert_eq!(system.get_counter("countdown"), 3);
    }

    /// Test that counter-reset in child creates new instance
    /// This is important for nested lists to work correctly
    #[test]
    fn nested_counter_reset_creates_instance() {
        let mut system = CounterSystem::new();

        // Parent ol
        system.reset_counter("item", 0);
        system.increment_counter("item", 1);
        system.increment_counter("item", 1);
        assert_eq!(system.get_counter("item"), 2);

        // Child ol - creates new instance
        system.push_scope();
        system.reset_counter("item", 0);
        system.increment_counter("item", 1);
        assert_eq!(system.get_counter("item"), 1);
        assert_eq!(system.get_counters("item"), vec![2, 1]);

        system.pop_scope();

        // Parent resumes
        system.increment_counter("item", 1);
        assert_eq!(system.get_counter("item"), 3);
    }
}
