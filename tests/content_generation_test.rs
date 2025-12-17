//! Integration tests for CSS content generation
//!
//! These tests verify the content generation system works correctly
//! for generating text content for pseudo-elements.

use fastrender::style::content::{
    parse_content, ContentContext, ContentGenerator, ContentItem, ContentValue, CounterStyle,
};
use std::collections::HashMap;

// ============================================================================
// ContentValue Construction Tests
// ============================================================================

#[test]
fn test_content_value_from_string() {
    let content = ContentValue::from_string("Hello, World!");
    assert!(!content.is_empty());
    assert_eq!(content.len(), 1);

    if let ContentValue::Items(items) = content {
        assert_eq!(items.len(), 1);
        assert!(matches!(&items[0], ContentItem::String(s) if s == "Hello, World!"));
    } else {
        panic!("Expected Items variant");
    }
}

#[test]
fn test_content_value_none_is_empty() {
    assert!(ContentValue::None.is_empty());
    assert_eq!(ContentValue::None.len(), 0);
}

#[test]
fn test_content_value_normal_is_empty() {
    assert!(ContentValue::Normal.is_empty());
    assert_eq!(ContentValue::Normal.len(), 0);
}

#[test]
fn test_content_value_empty_items_is_empty() {
    let content = ContentValue::Items(vec![]);
    assert!(content.is_empty());
}

// ============================================================================
// ContentItem Construction Tests
// ============================================================================

#[test]
fn test_content_item_string_construction() {
    let item = ContentItem::string("Hello");
    assert!(matches!(item, ContentItem::String(s) if s == "Hello"));
}

#[test]
fn test_content_item_attr_construction() {
    let item = ContentItem::attr("data-label");
    if let ContentItem::Attr { name, fallback, .. } = item {
        assert_eq!(name, "data-label");
        assert!(fallback.is_none());
    } else {
        panic!("Expected Attr variant");
    }
}

#[test]
fn test_content_item_attr_with_fallback_construction() {
    let item = ContentItem::attr_with_fallback("title", "Default Title");
    if let ContentItem::Attr { name, fallback, .. } = item {
        assert_eq!(name, "title");
        assert_eq!(fallback, Some("Default Title".to_string()));
    } else {
        panic!("Expected Attr variant");
    }
}

#[test]
fn test_content_item_counter_construction() {
    let item = ContentItem::counter("chapter");
    if let ContentItem::Counter { name, style } = item {
        assert_eq!(name, "chapter");
        assert!(style.is_none());
    } else {
        panic!("Expected Counter variant");
    }
}

#[test]
fn test_content_item_counter_styled_construction() {
    let item = ContentItem::counter_styled("chapter", CounterStyle::LowerRoman);
    if let ContentItem::Counter { name, style } = item {
        assert_eq!(name, "chapter");
        assert_eq!(style, Some(CounterStyle::LowerRoman));
    } else {
        panic!("Expected Counter variant");
    }
}

#[test]
fn test_content_item_counters_construction() {
    let item = ContentItem::counters("section", ".");
    if let ContentItem::Counters { name, separator, style } = item {
        assert_eq!(name, "section");
        assert_eq!(separator, ".");
        assert!(style.is_none());
    } else {
        panic!("Expected Counters variant");
    }
}

#[test]
fn test_content_item_counters_styled_construction() {
    let item = ContentItem::counters_styled("section", ".", CounterStyle::UpperAlpha);
    if let ContentItem::Counters { name, separator, style } = item {
        assert_eq!(name, "section");
        assert_eq!(separator, ".");
        assert_eq!(style, Some(CounterStyle::UpperAlpha));
    } else {
        panic!("Expected Counters variant");
    }
}

// ============================================================================
// CounterStyle Format Tests
// ============================================================================

#[test]
fn test_counter_style_decimal_formatting() {
    let style = CounterStyle::Decimal;
    assert_eq!(style.format(1), "1");
    assert_eq!(style.format(10), "10");
    assert_eq!(style.format(100), "100");
    assert_eq!(style.format(0), "0");
    assert_eq!(style.format(-5), "-5");
}

#[test]
fn test_counter_style_decimal_leading_zero_formatting() {
    let style = CounterStyle::DecimalLeadingZero;
    assert_eq!(style.format(1), "01");
    assert_eq!(style.format(5), "05");
    assert_eq!(style.format(10), "10");
    assert_eq!(style.format(99), "99");
}

#[test]
fn test_counter_style_lower_roman_formatting() {
    let style = CounterStyle::LowerRoman;
    assert_eq!(style.format(1), "i");
    assert_eq!(style.format(2), "ii");
    assert_eq!(style.format(3), "iii");
    assert_eq!(style.format(4), "iv");
    assert_eq!(style.format(5), "v");
    assert_eq!(style.format(6), "vi");
    assert_eq!(style.format(9), "ix");
    assert_eq!(style.format(10), "x");
    assert_eq!(style.format(50), "l");
    assert_eq!(style.format(100), "c");
    assert_eq!(style.format(500), "d");
    assert_eq!(style.format(1000), "m");
}

#[test]
fn test_counter_style_upper_roman_formatting() {
    let style = CounterStyle::UpperRoman;
    assert_eq!(style.format(1), "I");
    assert_eq!(style.format(4), "IV");
    assert_eq!(style.format(9), "IX");
    assert_eq!(style.format(49), "XLIX");
    assert_eq!(style.format(99), "XCIX");
    assert_eq!(style.format(1999), "MCMXCIX");
    assert_eq!(style.format(2024), "MMXXIV");
}

#[test]
fn test_counter_style_lower_alpha_formatting() {
    let style = CounterStyle::LowerAlpha;
    assert_eq!(style.format(1), "a");
    assert_eq!(style.format(2), "b");
    assert_eq!(style.format(26), "z");
    assert_eq!(style.format(27), "aa");
    assert_eq!(style.format(28), "ab");
    assert_eq!(style.format(52), "az");
    assert_eq!(style.format(53), "ba");
}

#[test]
fn test_counter_style_upper_alpha_formatting() {
    let style = CounterStyle::UpperAlpha;
    assert_eq!(style.format(1), "A");
    assert_eq!(style.format(2), "B");
    assert_eq!(style.format(26), "Z");
    assert_eq!(style.format(27), "AA");
}

#[test]
fn test_counter_style_lower_greek_formatting() {
    let style = CounterStyle::LowerGreek;
    assert_eq!(style.format(1), "α");
    assert_eq!(style.format(2), "β");
    assert_eq!(style.format(3), "γ");
    assert_eq!(style.format(24), "ω");
    // Out of range returns the number
    assert_eq!(style.format(25), "25");
    assert_eq!(style.format(0), "0");
}

#[test]
fn test_counter_style_markers() {
    assert_eq!(CounterStyle::Disc.format(1), "•");
    assert_eq!(CounterStyle::Disc.format(100), "•");
    assert_eq!(CounterStyle::Circle.format(1), "◦");
    assert_eq!(CounterStyle::Square.format(1), "▪");
    assert_eq!(CounterStyle::None.format(1), "");
}

#[test]
fn additive_counter_styles_fall_back_out_of_range() {
    // Armenian/Georgian additive styles should fall back to decimal when outside their range
    assert_eq!(CounterStyle::Armenian.format(-3), "-3");
    assert_eq!(CounterStyle::LowerArmenian.format(10_000), "10000");
    assert_eq!(CounterStyle::LowerArmenian.format(-1), "-1");
    assert_eq!(CounterStyle::Georgian.format(20_000), "20000");
    assert_eq!(CounterStyle::Georgian.format(-5), "-5");
}

#[test]
fn test_counter_style_parse() {
    assert_eq!(CounterStyle::parse("decimal"), Some(CounterStyle::Decimal));
    assert_eq!(
        CounterStyle::parse("decimal-leading-zero"),
        Some(CounterStyle::DecimalLeadingZero)
    );
    assert_eq!(CounterStyle::parse("lower-roman"), Some(CounterStyle::LowerRoman));
    assert_eq!(CounterStyle::parse("upper-roman"), Some(CounterStyle::UpperRoman));
    assert_eq!(CounterStyle::parse("lower-alpha"), Some(CounterStyle::LowerAlpha));
    assert_eq!(CounterStyle::parse("lower-latin"), Some(CounterStyle::LowerAlpha));
    assert_eq!(CounterStyle::parse("upper-alpha"), Some(CounterStyle::UpperAlpha));
    assert_eq!(CounterStyle::parse("upper-latin"), Some(CounterStyle::UpperAlpha));
    assert_eq!(CounterStyle::parse("lower-greek"), Some(CounterStyle::LowerGreek));
    assert_eq!(CounterStyle::parse("disc"), Some(CounterStyle::Disc));
    assert_eq!(CounterStyle::parse("circle"), Some(CounterStyle::Circle));
    assert_eq!(CounterStyle::parse("square"), Some(CounterStyle::Square));
    assert_eq!(CounterStyle::parse("none"), Some(CounterStyle::None));
    assert_eq!(CounterStyle::parse("unknown-style"), None);
    assert_eq!(CounterStyle::parse(""), None);
}

// ============================================================================
// ContentContext Counter Tests
// ============================================================================

#[test]
fn test_context_counter_basic_operations() {
    let mut ctx = ContentContext::new();

    // Default counter value is 0
    assert_eq!(ctx.get_counter("test"), 0);

    // Set counter
    ctx.set_counter("test", 5);
    assert_eq!(ctx.get_counter("test"), 5);

    // Increment
    ctx.increment_counter("test");
    assert_eq!(ctx.get_counter("test"), 6);

    // Increment by amount
    ctx.increment_counter_by("test", 10);
    assert_eq!(ctx.get_counter("test"), 16);

    // Reset
    ctx.reset_counter("test");
    assert_eq!(ctx.get_counter("test"), 0);

    // Reset to value
    ctx.reset_counter_to("test", 100);
    assert_eq!(ctx.get_counter("test"), 100);
}

#[test]
fn test_context_counter_nested_scopes() {
    let mut ctx = ContentContext::new();

    // Push nested counters
    ctx.push_counter("list", 1);
    assert_eq!(ctx.get_counters("list"), &[1]);

    ctx.push_counter("list", 1);
    assert_eq!(ctx.get_counters("list"), &[1, 1]);

    ctx.increment_counter("list");
    assert_eq!(ctx.get_counters("list"), &[1, 2]);

    ctx.push_counter("list", 1);
    ctx.increment_counter("list");
    ctx.increment_counter("list");
    assert_eq!(ctx.get_counters("list"), &[1, 2, 3]);

    // Pop back
    ctx.pop_counter("list");
    assert_eq!(ctx.get_counters("list"), &[1, 2]);

    ctx.pop_counter("list");
    assert_eq!(ctx.get_counters("list"), &[1]);

    // Pop on single element shouldn't remove it
    ctx.pop_counter("list");
    assert_eq!(ctx.get_counters("list"), &[1]);
}

#[test]
fn test_context_multiple_counters() {
    let mut ctx = ContentContext::new();

    ctx.set_counter("chapter", 1);
    ctx.set_counter("section", 1);
    ctx.set_counter("paragraph", 1);

    ctx.increment_counter("section");
    ctx.increment_counter("section");
    ctx.increment_counter("paragraph");

    assert_eq!(ctx.get_counter("chapter"), 1);
    assert_eq!(ctx.get_counter("section"), 3);
    assert_eq!(ctx.get_counter("paragraph"), 2);
}

// ============================================================================
// ContentContext Attribute Tests
// ============================================================================

#[test]
fn test_context_attributes_basic() {
    let mut ctx = ContentContext::new();

    ctx.set_attribute("data-label", "Test Label");
    ctx.set_attribute("title", "My Title");

    assert_eq!(ctx.get_attribute("data-label"), Some("Test Label"));
    assert_eq!(ctx.get_attribute("title"), Some("My Title"));
    assert_eq!(ctx.get_attribute("nonexistent"), None);
}

#[test]
fn test_context_with_attributes_constructor() {
    let mut attrs = HashMap::new();
    attrs.insert("id".to_string(), "my-id".to_string());
    attrs.insert("class".to_string(), "my-class".to_string());

    let ctx = ContentContext::with_attributes(attrs);

    assert_eq!(ctx.get_attribute("id"), Some("my-id"));
    assert_eq!(ctx.get_attribute("class"), Some("my-class"));
}

// ============================================================================
// ContentContext Quote Tests
// ============================================================================

#[test]
fn test_context_quotes_basic() {
    let mut ctx = ContentContext::new();

    // Default quotes are curly double quotes
    assert_eq!(ctx.open_quote(), "\u{201C}"); // "
    assert_eq!(ctx.quote_depth(), 0);

    // First level
    ctx.push_quote();
    assert_eq!(ctx.quote_depth(), 1);
    assert_eq!(ctx.open_quote(), "\u{2018}"); // '

    // Close
    ctx.pop_quote();
    assert_eq!(ctx.quote_depth(), 0);
    assert_eq!(ctx.close_quote(), "\u{201D}"); // "
}

#[test]
fn test_context_quotes_custom() {
    let mut ctx = ContentContext::new();
    ctx.set_quotes(vec![
        ("<<".to_string(), ">>".to_string()),
        ("<".to_string(), ">".to_string()),
    ]);

    assert_eq!(ctx.open_quote(), "<<");
    ctx.push_quote();
    assert_eq!(ctx.open_quote(), "<");
    ctx.pop_quote();
    assert_eq!(ctx.close_quote(), ">>");
}

#[test]
fn test_context_quotes_deep_nesting() {
    let mut ctx = ContentContext::new();

    // Nest beyond the number of quote pairs
    ctx.push_quote();
    ctx.push_quote();
    ctx.push_quote();
    ctx.push_quote();
    ctx.push_quote();

    // Should still work (using last pair)
    assert_eq!(ctx.open_quote(), "\u{2018}");
    assert_eq!(ctx.quote_depth(), 5);
}

// ============================================================================
// ContentGenerator Tests
// ============================================================================

#[test]
fn test_generator_string_content() {
    let gen = ContentGenerator::new();
    let content = ContentValue::from_string("Hello, World!");
    let mut ctx = ContentContext::new();

    assert_eq!(gen.generate(&content, &mut ctx), "Hello, World!");
}

#[test]
fn test_generator_none_content() {
    let gen = ContentGenerator::new();
    let mut ctx = ContentContext::new();

    assert_eq!(gen.generate(&ContentValue::None, &mut ctx), "");
}

#[test]
fn test_generator_normal_content() {
    let gen = ContentGenerator::new();
    let mut ctx = ContentContext::new();

    assert_eq!(gen.generate(&ContentValue::Normal, &mut ctx), "");
}

#[test]
fn test_generator_attr_content() {
    let gen = ContentGenerator::new();
    let content = ContentValue::Items(vec![ContentItem::attr("data-label")]);
    let mut ctx = ContentContext::new();

    // Without attribute set
    assert_eq!(gen.generate(&content, &mut ctx), "");

    // With attribute set
    ctx.set_attribute("data-label", "Label Value");
    assert_eq!(gen.generate(&content, &mut ctx), "Label Value");
}

#[test]
fn test_generator_attr_with_fallback() {
    let gen = ContentGenerator::new();
    let content = ContentValue::Items(vec![ContentItem::attr_with_fallback("missing-attr", "Fallback Value")]);
    let mut ctx = ContentContext::new();

    assert_eq!(gen.generate(&content, &mut ctx), "Fallback Value");
}

#[test]
fn test_generator_counter_content() {
    let gen = ContentGenerator::new();
    let content = ContentValue::Items(vec![ContentItem::counter("chapter")]);
    let mut ctx = ContentContext::new();

    // Default is 0
    assert_eq!(gen.generate(&content, &mut ctx), "0");

    // Set counter
    ctx.set_counter("chapter", 5);
    assert_eq!(gen.generate(&content, &mut ctx), "5");
}

#[test]
fn test_generator_counter_styled_content() {
    let gen = ContentGenerator::new();
    let content = ContentValue::Items(vec![ContentItem::counter_styled("chapter", CounterStyle::UpperRoman)]);
    let mut ctx = ContentContext::new();
    ctx.set_counter("chapter", 4);

    assert_eq!(gen.generate(&content, &mut ctx), "IV");
}

#[test]
fn test_generator_counters_content() {
    let gen = ContentGenerator::new();
    let content = ContentValue::Items(vec![ContentItem::counters("section", ".")]);
    let mut ctx = ContentContext::new();

    ctx.push_counter("section", 1);
    ctx.push_counter("section", 2);
    ctx.push_counter("section", 3);

    assert_eq!(gen.generate(&content, &mut ctx), "1.2.3");
}

#[test]
fn test_generator_counters_styled_content() {
    let gen = ContentGenerator::new();
    let content = ContentValue::Items(vec![ContentItem::counters_styled(
        "section",
        "-",
        CounterStyle::LowerAlpha,
    )]);
    let mut ctx = ContentContext::new();

    ctx.push_counter("section", 1);
    ctx.push_counter("section", 2);
    ctx.push_counter("section", 3);

    assert_eq!(gen.generate(&content, &mut ctx), "a-b-c");
}

#[test]
fn test_generator_quote_content() {
    let gen = ContentGenerator::new();
    let content = ContentValue::Items(vec![
        ContentItem::OpenQuote,
        ContentItem::String("Hello".to_string()),
        ContentItem::CloseQuote,
    ]);
    let mut ctx = ContentContext::new();

    // Should produce "Hello" with curly quotes
    let result = gen.generate(&content, &mut ctx);
    assert!(result.contains("Hello"));
    assert!(result.starts_with('\u{201C}')); // "
    assert!(result.ends_with('\u{201D}')); // "
}

#[test]
fn test_generator_nested_quotes() {
    let gen = ContentGenerator::new();
    let content = ContentValue::Items(vec![
        ContentItem::OpenQuote,
        ContentItem::String("He said ".to_string()),
        ContentItem::OpenQuote,
        ContentItem::String("Hello".to_string()),
        ContentItem::CloseQuote,
        ContentItem::CloseQuote,
    ]);
    let mut ctx = ContentContext::new();

    let result = gen.generate(&content, &mut ctx);
    // Should have outer double quotes and inner single quotes
    assert!(result.contains("\u{2018}")); // '
    assert!(result.contains("\u{2019}")); // '
}

#[test]
fn test_generator_no_quote_content() {
    let gen = ContentGenerator::new();
    let content = ContentValue::Items(vec![ContentItem::NoOpenQuote, ContentItem::NoCloseQuote]);
    let mut ctx = ContentContext::new();

    // Should produce empty string but affect quote depth
    let result = gen.generate(&content, &mut ctx);
    assert_eq!(result, "");
}

#[test]
fn test_generator_combined_content() {
    let gen = ContentGenerator::new();
    let content = ContentValue::Items(vec![
        ContentItem::String("Chapter ".to_string()),
        ContentItem::counter("chapter"),
        ContentItem::String(": ".to_string()),
        ContentItem::attr("title"),
    ]);
    let mut ctx = ContentContext::new();
    ctx.set_counter("chapter", 5);
    ctx.set_attribute("title", "Introduction");

    assert_eq!(gen.generate(&content, &mut ctx), "Chapter 5: Introduction");
}

#[test]
fn test_generator_url_content() {
    let gen = ContentGenerator::new();
    let content = ContentValue::Items(vec![ContentItem::Url("image.png".to_string())]);
    let mut ctx = ContentContext::new();

    // URL content generates empty string (should be handled as image by caller)
    assert_eq!(gen.generate(&content, &mut ctx), "");
}

#[test]
fn test_is_text_only() {
    assert!(ContentGenerator::is_text_only(&ContentValue::None));
    assert!(ContentGenerator::is_text_only(&ContentValue::Normal));
    assert!(ContentGenerator::is_text_only(&ContentValue::from_string("Hello")));
    assert!(ContentGenerator::is_text_only(&ContentValue::Items(vec![
        ContentItem::counter("x"),
        ContentItem::String("test".to_string()),
    ])));

    // URL makes it not text-only
    assert!(!ContentGenerator::is_text_only(&ContentValue::Items(vec![
        ContentItem::Url("image.png".to_string())
    ])));
}

// ============================================================================
// Parse Content Tests
// ============================================================================

#[test]
fn test_parse_none() {
    assert_eq!(parse_content("none"), Some(ContentValue::None));
    assert_eq!(parse_content("NONE"), Some(ContentValue::None));
    assert_eq!(parse_content("  none  "), Some(ContentValue::None));
}

#[test]
fn test_parse_normal() {
    assert_eq!(parse_content("normal"), Some(ContentValue::Normal));
    assert_eq!(parse_content("NORMAL"), Some(ContentValue::Normal));
}

#[test]
fn test_parse_double_quoted_string() {
    let content = parse_content("\"Hello, World!\"").unwrap();
    if let ContentValue::Items(items) = content {
        assert_eq!(items.len(), 1);
        assert_eq!(items[0], ContentItem::String("Hello, World!".to_string()));
    } else {
        panic!("Expected Items");
    }
}

#[test]
fn test_parse_single_quoted_string() {
    let content = parse_content("'Hello'").unwrap();
    if let ContentValue::Items(items) = content {
        assert_eq!(items.len(), 1);
        assert_eq!(items[0], ContentItem::String("Hello".to_string()));
    } else {
        panic!("Expected Items");
    }
}

#[test]
fn test_parse_escaped_string() {
    let content = parse_content("\"Hello\\nWorld\"").unwrap();
    if let ContentValue::Items(items) = content {
        assert_eq!(items[0], ContentItem::String("Hello\nWorld".to_string()));
    }
}

#[test]
fn test_parse_attr_simple() {
    let content = parse_content("attr(data-label)").unwrap();
    if let ContentValue::Items(items) = content {
        assert_eq!(items.len(), 1);
        if let ContentItem::Attr { name, fallback, .. } = &items[0] {
            assert_eq!(name, "data-label");
            assert!(fallback.is_none());
        } else {
            panic!("Expected Attr");
        }
    }
}

#[test]
fn test_parse_attr_with_fallback() {
    let content = parse_content("attr(title, \"Default Title\")").unwrap();
    if let ContentValue::Items(items) = content {
        if let ContentItem::Attr { name, fallback, .. } = &items[0] {
            assert_eq!(name, "title");
            assert_eq!(fallback.as_deref(), Some("Default Title"));
        }
    }
}

#[test]
fn test_parse_counter_simple() {
    let content = parse_content("counter(chapter)").unwrap();
    if let ContentValue::Items(items) = content {
        if let ContentItem::Counter { name, style } = &items[0] {
            assert_eq!(name, "chapter");
            assert!(style.is_none());
        }
    }
}

#[test]
fn test_parse_counter_with_style() {
    let content = parse_content("counter(chapter, upper-roman)").unwrap();
    if let ContentValue::Items(items) = content {
        if let ContentItem::Counter { name, style } = &items[0] {
            assert_eq!(name, "chapter");
            assert_eq!(*style, Some(CounterStyle::UpperRoman));
        }
    }
}

#[test]
fn test_parse_counters_simple() {
    let content = parse_content("counters(section, \".\")").unwrap();
    if let ContentValue::Items(items) = content {
        if let ContentItem::Counters { name, separator, style } = &items[0] {
            assert_eq!(name, "section");
            assert_eq!(separator, ".");
            assert!(style.is_none());
        }
    }
}

#[test]
fn test_parse_counters_with_style() {
    let content = parse_content("counters(section, \" > \", lower-alpha)").unwrap();
    if let ContentValue::Items(items) = content {
        if let ContentItem::Counters { name, separator, style } = &items[0] {
            assert_eq!(name, "section");
            assert_eq!(separator, " > ");
            assert_eq!(*style, Some(CounterStyle::LowerAlpha));
        }
    }
}

#[test]
fn test_parse_open_quote() {
    let content = parse_content("open-quote").unwrap();
    if let ContentValue::Items(items) = content {
        assert_eq!(items[0], ContentItem::OpenQuote);
    }
}

#[test]
fn test_parse_close_quote() {
    let content = parse_content("close-quote").unwrap();
    if let ContentValue::Items(items) = content {
        assert_eq!(items[0], ContentItem::CloseQuote);
    }
}

#[test]
fn test_parse_no_open_quote() {
    let content = parse_content("no-open-quote").unwrap();
    if let ContentValue::Items(items) = content {
        assert_eq!(items[0], ContentItem::NoOpenQuote);
    }
}

#[test]
fn test_parse_no_close_quote() {
    let content = parse_content("no-close-quote").unwrap();
    if let ContentValue::Items(items) = content {
        assert_eq!(items[0], ContentItem::NoCloseQuote);
    }
}

#[test]
fn test_parse_url_double_quoted() {
    let content = parse_content("url(\"image.png\")").unwrap();
    if let ContentValue::Items(items) = content {
        assert_eq!(items[0], ContentItem::Url("image.png".to_string()));
    }
}

#[test]
fn test_parse_url_unquoted() {
    let content = parse_content("url(image.png)").unwrap();
    if let ContentValue::Items(items) = content {
        assert_eq!(items[0], ContentItem::Url("image.png".to_string()));
    }
}

#[test]
fn test_parse_multiple_items() {
    let content = parse_content("\"Chapter \" counter(chapter) \": \" attr(title)").unwrap();
    if let ContentValue::Items(items) = content {
        assert_eq!(items.len(), 4);
        assert_eq!(items[0], ContentItem::String("Chapter ".to_string()));
        assert!(matches!(items[1], ContentItem::Counter { .. }));
        assert_eq!(items[2], ContentItem::String(": ".to_string()));
        assert!(matches!(items[3], ContentItem::Attr { .. }));
    }
}

#[test]
fn test_parse_empty_string_returns_none() {
    assert_eq!(parse_content(""), None);
    assert_eq!(parse_content("   "), None);
}

#[test]
fn test_parse_invalid_function_returns_none() {
    assert_eq!(parse_content("invalid-function()"), None);
}

// ============================================================================
// Display Formatting Tests
// ============================================================================

#[test]
fn test_content_value_display() {
    assert_eq!(format!("{}", ContentValue::None), "none");
    assert_eq!(format!("{}", ContentValue::Normal), "normal");
    assert_eq!(format!("{}", ContentValue::from_string("Hello")), "\"Hello\"");
}

#[test]
fn test_content_item_display() {
    assert_eq!(format!("{}", ContentItem::string("Hello")), "\"Hello\"");
    assert_eq!(format!("{}", ContentItem::attr("data-x")), "attr(data-x)");
    assert_eq!(format!("{}", ContentItem::counter("n")), "counter(n)");
    assert_eq!(
        format!("{}", ContentItem::counter_styled("n", CounterStyle::LowerRoman)),
        "counter(n, lower-roman)"
    );
    assert_eq!(format!("{}", ContentItem::counters("n", ".")), "counters(n, \".\")");
    assert_eq!(format!("{}", ContentItem::OpenQuote), "open-quote");
    assert_eq!(format!("{}", ContentItem::CloseQuote), "close-quote");
}

#[test]
fn test_counter_style_display() {
    assert_eq!(format!("{}", CounterStyle::Decimal), "decimal");
    assert_eq!(format!("{}", CounterStyle::LowerRoman), "lower-roman");
    assert_eq!(format!("{}", CounterStyle::UpperAlpha), "upper-alpha");
}

// ============================================================================
// Integration / Workflow Tests
// ============================================================================

#[test]
fn test_list_item_workflow() {
    let gen = ContentGenerator::new();
    let mut ctx = ContentContext::new();

    // Content for ordered list items
    let content = ContentValue::Items(vec![
        ContentItem::counter("list-item"),
        ContentItem::String(". ".to_string()),
    ]);

    // Simulate rendering list items
    ctx.reset_counter("list-item");

    ctx.increment_counter("list-item");
    assert_eq!(gen.generate(&content, &mut ctx), "1. ");

    ctx.increment_counter("list-item");
    assert_eq!(gen.generate(&content, &mut ctx), "2. ");

    ctx.increment_counter("list-item");
    assert_eq!(gen.generate(&content, &mut ctx), "3. ");
}

#[test]
fn test_nested_list_workflow() {
    let gen = ContentGenerator::new();
    let mut ctx = ContentContext::new();

    let content = ContentValue::Items(vec![ContentItem::counters("item", ".")]);

    // First level
    ctx.push_counter("item", 1);
    assert_eq!(gen.generate(&content, &mut ctx), "1");

    // Nested level
    ctx.push_counter("item", 1);
    assert_eq!(gen.generate(&content, &mut ctx), "1.1");

    ctx.increment_counter("item");
    assert_eq!(gen.generate(&content, &mut ctx), "1.2");

    // Deeper nesting
    ctx.push_counter("item", 1);
    assert_eq!(gen.generate(&content, &mut ctx), "1.2.1");

    ctx.increment_counter("item");
    assert_eq!(gen.generate(&content, &mut ctx), "1.2.2");

    // Pop back up
    ctx.pop_counter("item");
    assert_eq!(gen.generate(&content, &mut ctx), "1.2");

    ctx.increment_counter("item");
    assert_eq!(gen.generate(&content, &mut ctx), "1.3");

    ctx.pop_counter("item");
    ctx.increment_counter("item");
    assert_eq!(gen.generate(&content, &mut ctx), "2");
}

#[test]
fn test_chapter_section_workflow() {
    let gen = ContentGenerator::new();
    let mut ctx = ContentContext::new();

    // Chapter heading content
    let chapter_content = ContentValue::Items(vec![
        ContentItem::String("Chapter ".to_string()),
        ContentItem::counter_styled("chapter", CounterStyle::Decimal),
        ContentItem::String(": ".to_string()),
        ContentItem::attr("title"),
    ]);

    // Section heading content
    let section_content = ContentValue::Items(vec![
        ContentItem::counter("chapter"),
        ContentItem::String(".".to_string()),
        ContentItem::counter("section"),
        ContentItem::String(" ".to_string()),
        ContentItem::attr("title"),
    ]);

    // Set up chapter 1
    ctx.reset_counter_to("chapter", 1);
    ctx.reset_counter("section");
    ctx.set_attribute("title", "Introduction");
    assert_eq!(gen.generate(&chapter_content, &mut ctx), "Chapter 1: Introduction");

    // Section 1.1
    ctx.increment_counter("section");
    ctx.set_attribute("title", "Overview");
    assert_eq!(gen.generate(&section_content, &mut ctx), "1.1 Overview");

    // Section 1.2
    ctx.increment_counter("section");
    ctx.set_attribute("title", "Background");
    assert_eq!(gen.generate(&section_content, &mut ctx), "1.2 Background");

    // Chapter 2
    ctx.increment_counter("chapter");
    ctx.reset_counter("section");
    ctx.set_attribute("title", "Methods");
    assert_eq!(gen.generate(&chapter_content, &mut ctx), "Chapter 2: Methods");

    // Section 2.1
    ctx.increment_counter("section");
    ctx.set_attribute("title", "Approach");
    assert_eq!(gen.generate(&section_content, &mut ctx), "2.1 Approach");
}

#[test]
fn test_quoted_text_workflow() {
    let gen = ContentGenerator::new();
    let mut ctx = ContentContext::new();

    // Set custom quotes (like French guillemets)
    ctx.set_quotes(vec![
        ("«\u{00A0}".to_string(), "\u{00A0}»".to_string()),
        ("‹\u{00A0}".to_string(), "\u{00A0}›".to_string()),
    ]);

    let content = ContentValue::Items(vec![
        ContentItem::OpenQuote,
        ContentItem::String("Hello".to_string()),
        ContentItem::CloseQuote,
    ]);

    let result = gen.generate(&content, &mut ctx);
    assert!(result.starts_with("«"));
    assert!(result.ends_with("»"));
    assert!(result.contains("Hello"));
}
