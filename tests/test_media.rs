//! Integration tests for CSS Media Query Parser
//!
//! These tests verify the media query parser and evaluator against
//! real-world use cases and CSS specification requirements.

use fastrender::css::parser::parse_stylesheet;
use fastrender::style::media::{ColorScheme, ContrastPreference, MediaContext, MediaQuery, MediaQueryCache};
use std::env;

struct EnvGuard {
    key: &'static str,
    prev: Option<String>,
}

impl EnvGuard {
    fn new(key: &'static str, value: Option<&str>) -> Self {
        let prev = env::var(key).ok();
        if let Some(v) = value {
            env::set_var(key, v);
        } else {
            env::remove_var(key);
        }
        EnvGuard { key, prev }
    }
}

impl Drop for EnvGuard {
    fn drop(&mut self) {
        if let Some(v) = &self.prev {
            env::set_var(self.key, v);
        } else {
            env::remove_var(self.key);
        }
    }
}

// ============================================================================
// Responsive Web Design Tests
// ============================================================================

/// Tests Bootstrap-style breakpoints
#[test]
fn test_bootstrap_breakpoints() {
    // xs: < 576px
    // sm: >= 576px
    // md: >= 768px
    // lg: >= 992px
    // xl: >= 1200px
    // xxl: >= 1400px

    let breakpoints = [
        (
            "(max-width: 575.98px)",
            vec![320.0, 480.0, 575.0],
            vec![576.0, 768.0, 1024.0],
        ),
        (
            "(min-width: 576px)",
            vec![576.0, 768.0, 1024.0],
            vec![320.0, 480.0, 575.0],
        ),
        (
            "(min-width: 768px)",
            vec![768.0, 992.0, 1200.0],
            vec![320.0, 576.0, 767.0],
        ),
        (
            "(min-width: 992px)",
            vec![992.0, 1024.0, 1200.0],
            vec![320.0, 768.0, 991.0],
        ),
        (
            "(min-width: 1200px)",
            vec![1200.0, 1400.0, 1920.0],
            vec![320.0, 992.0, 1199.0],
        ),
        (
            "(min-width: 1400px)",
            vec![1400.0, 1920.0, 2560.0],
            vec![320.0, 1200.0, 1399.0],
        ),
    ];

    for (query_str, should_match, should_not_match) in breakpoints {
        let query = MediaQuery::parse(query_str).unwrap_or_else(|_| panic!("Failed to parse: {}", query_str));

        for width in should_match {
            let ctx = MediaContext::screen(width, 768.0);
            assert!(
                ctx.evaluate(&query),
                "Expected {} to match at width {}px",
                query_str,
                width
            );
        }

        for width in should_not_match {
            let ctx = MediaContext::screen(width, 768.0);
            assert!(
                !ctx.evaluate(&query),
                "Expected {} NOT to match at width {}px",
                query_str,
                width
            );
        }
    }
}

/// Tests Tailwind CSS breakpoints
#[test]
fn test_tailwind_breakpoints() {
    // sm: 640px
    // md: 768px
    // lg: 1024px
    // xl: 1280px
    // 2xl: 1536px

    let test_cases = [
        (640.0, "(min-width: 640px)", true),
        (639.0, "(min-width: 640px)", false),
        (768.0, "(min-width: 768px)", true),
        (767.0, "(min-width: 768px)", false),
        (1024.0, "(min-width: 1024px)", true),
        (1023.0, "(min-width: 1024px)", false),
        (1280.0, "(min-width: 1280px)", true),
        (1279.0, "(min-width: 1280px)", false),
        (1536.0, "(min-width: 1536px)", true),
        (1535.0, "(min-width: 1536px)", false),
    ];

    for (width, query_str, expected) in test_cases {
        let query = MediaQuery::parse(query_str).unwrap();
        let ctx = MediaContext::screen(width, 768.0);
        assert_eq!(
            ctx.evaluate(&query),
            expected,
            "{} at {}px should be {}",
            query_str,
            width,
            expected
        );
    }
}

/// Tests common mobile-first responsive patterns
#[test]
fn test_mobile_first_pattern() {
    // Mobile-first: styles apply to all, then override for larger screens

    let queries = [
        "(min-width: 480px)",  // Larger phones
        "(min-width: 768px)",  // Tablets
        "(min-width: 1024px)", // Small desktops
        "(min-width: 1280px)", // Large desktops
    ];

    let parsed_queries: Vec<MediaQuery> = queries.iter().map(|q| MediaQuery::parse(q).unwrap()).collect();

    // Mobile (375px) - no queries match
    let mobile = MediaContext::screen(375.0, 667.0);
    for query in &parsed_queries {
        assert!(!mobile.evaluate(query));
    }

    // Tablet (768px) - first two queries match
    let tablet = MediaContext::screen(768.0, 1024.0);
    assert!(tablet.evaluate(&parsed_queries[0])); // 480px
    assert!(tablet.evaluate(&parsed_queries[1])); // 768px
    assert!(!tablet.evaluate(&parsed_queries[2])); // 1024px
    assert!(!tablet.evaluate(&parsed_queries[3])); // 1280px

    // Desktop (1440px) - all queries match
    let desktop = MediaContext::screen(1440.0, 900.0);
    for query in &parsed_queries {
        assert!(desktop.evaluate(query));
    }
}

// ============================================================================
// Device-Specific Tests
// ============================================================================

/// Tests common device viewport sizes
#[test]
fn test_device_viewports() {
    let devices = [
        // (name, width, height, is_mobile)
        ("iPhone SE", 375.0, 667.0, true),
        ("iPhone 12 Pro", 390.0, 844.0, true),
        ("iPhone 12 Pro Max", 428.0, 926.0, true),
        ("Pixel 5", 393.0, 851.0, true),
        ("Samsung Galaxy S21", 360.0, 800.0, true),
        ("iPad Mini", 768.0, 1024.0, true),
        ("iPad Pro 11\"", 834.0, 1194.0, true),
        ("MacBook Air 13\"", 1440.0, 900.0, false),
        ("MacBook Pro 16\"", 1792.0, 1120.0, false),
        ("27\" iMac", 2560.0, 1440.0, false),
        ("1080p Monitor", 1920.0, 1080.0, false),
        ("4K Monitor", 3840.0, 2160.0, false),
    ];

    let mobile_query = MediaQuery::parse("(max-width: 767px)").unwrap();
    let tablet_query = MediaQuery::parse("(min-width: 768px) and (max-width: 1023px)").unwrap();
    let desktop_query = MediaQuery::parse("(min-width: 1024px)").unwrap();

    for (name, width, height, _is_mobile) in devices {
        let ctx = MediaContext::screen(width, height);

        // Verify at least one query matches
        let matches_mobile = ctx.evaluate(&mobile_query);
        let matches_tablet = ctx.evaluate(&tablet_query);
        let matches_desktop = ctx.evaluate(&desktop_query);

        assert!(
            matches_mobile || matches_tablet || matches_desktop,
            "Device {} ({}x{}) should match at least one category",
            name,
            width,
            height
        );
    }
}

/// Tests orientation detection for common devices
#[test]
fn test_orientation_detection() {
    let portrait_query = MediaQuery::parse("(orientation: portrait)").unwrap();
    let landscape_query = MediaQuery::parse("(orientation: landscape)").unwrap();

    // Portrait devices
    let portrait_cases = [
        ("iPhone portrait", 375.0, 667.0),
        ("iPad portrait", 768.0, 1024.0),
        ("Square (considered portrait)", 500.0, 500.0),
    ];

    for (name, width, height) in portrait_cases {
        let ctx = MediaContext::screen(width, height);
        assert!(ctx.evaluate(&portrait_query), "{} should be portrait", name);
        assert!(!ctx.evaluate(&landscape_query), "{} should not be landscape", name);
    }

    // Landscape devices
    let landscape_cases = [
        ("iPhone landscape", 667.0, 375.0),
        ("iPad landscape", 1024.0, 768.0),
        ("Desktop monitor", 1920.0, 1080.0),
        ("Ultrawide monitor", 3440.0, 1440.0),
    ];

    for (name, width, height) in landscape_cases {
        let ctx = MediaContext::screen(width, height);
        assert!(ctx.evaluate(&landscape_query), "{} should be landscape", name);
        assert!(!ctx.evaluate(&portrait_query), "{} should not be portrait", name);
    }
}

/// Tests legacy device-width/-height media features
#[test]
fn test_device_dimension_queries_use_device_metrics() {
    // Viewport is narrow, device is wide (common on mobile where device-width stays large)
    let ctx = MediaContext::screen(375.0, 667.0).with_device_size(1080.0, 1920.0);

    let min_device = MediaQuery::parse("(min-device-width: 800px)").unwrap();
    let max_device = MediaQuery::parse("(max-device-width: 800px)").unwrap();
    let exact_device = MediaQuery::parse("(device-width: 1080px)").unwrap();
    let max_device_height = MediaQuery::parse("(max-device-height: 2000px)").unwrap();

    // Device metrics drive these queries, not the viewport
    assert!(ctx.evaluate(&min_device));
    assert!(!ctx.evaluate(&max_device));
    assert!(ctx.evaluate(&exact_device));
    assert!(ctx.evaluate(&max_device_height));

    // Viewport-sized queries still reflect the smaller viewport
    let wide_viewport = MediaQuery::parse("(min-width: 800px)").unwrap();
    assert!(!ctx.evaluate(&wide_viewport));
}

/// Tests device-aspect-ratio media features and range syntax
#[test]
fn test_device_aspect_ratio_queries() {
    let ctx = MediaContext::screen(375.0, 667.0).with_device_size(1080.0, 1920.0);

    let exact_ratio = MediaQuery::parse("(device-aspect-ratio: 9/16)").unwrap();
    let min_ratio = MediaQuery::parse("(min-device-aspect-ratio: 9/16)").unwrap();
    let max_ratio = MediaQuery::parse("(max-device-aspect-ratio: 9/16)").unwrap();
    let mismatch = MediaQuery::parse("(device-aspect-ratio: 16/9)").unwrap();
    let range = MediaQuery::parse("(600px < device-width <= 1200px)").unwrap();

    assert!(ctx.evaluate(&exact_ratio));
    assert!(ctx.evaluate(&min_ratio));
    assert!(ctx.evaluate(&max_ratio));
    assert!(ctx.evaluate(&range));
    assert!(!ctx.evaluate(&mismatch));
}

// ============================================================================
// Print Media Tests
// ============================================================================

/// Tests print-specific media queries
#[test]
fn test_print_media() {
    let screen_ctx = MediaContext::screen(1024.0, 768.0);
    let print_ctx = MediaContext::print(816.0, 1056.0); // US Letter at 96 DPI

    // Test basic media type
    let screen_query = MediaQuery::parse("screen").unwrap();
    let print_query = MediaQuery::parse("print").unwrap();
    let all_query = MediaQuery::parse("all").unwrap();

    assert!(screen_ctx.evaluate(&screen_query));
    assert!(!screen_ctx.evaluate(&print_query));
    assert!(screen_ctx.evaluate(&all_query));

    assert!(!print_ctx.evaluate(&screen_query));
    assert!(print_ctx.evaluate(&print_query));
    assert!(print_ctx.evaluate(&all_query));

    // Test combined queries
    let screen_large = MediaQuery::parse("screen and (min-width: 1024px)").unwrap();
    assert!(screen_ctx.evaluate(&screen_large));
    assert!(!print_ctx.evaluate(&screen_large));

    let print_portrait = MediaQuery::parse("print and (orientation: portrait)").unwrap();
    assert!(!screen_ctx.evaluate(&print_portrait));
    assert!(print_ctx.evaluate(&print_portrait));
}

/// Tests "hide from print" pattern
#[test]
fn test_hide_from_print_pattern() {
    let screen_ctx = MediaContext::screen(1024.0, 768.0);
    let print_ctx = MediaContext::print(816.0, 1056.0);

    // Common pattern: @media print { .no-print { display: none; } }
    let print_query = MediaQuery::parse("print").unwrap();

    // Should match only in print context
    assert!(!screen_ctx.evaluate(&print_query));
    assert!(print_ctx.evaluate(&print_query));

    // Alternative: @media not print { ... }
    let not_print = MediaQuery::parse("not print").unwrap();
    assert!(screen_ctx.evaluate(&not_print));
    assert!(!print_ctx.evaluate(&not_print));
}

// ============================================================================
// High-DPI / Retina Tests
// ============================================================================

/// Tests resolution-based media queries
#[test]
fn test_retina_detection() {
    let standard_dpi = MediaContext::screen(1024.0, 768.0).with_dpr(1.0);
    let retina_2x = MediaContext::screen(1024.0, 768.0).with_dpr(2.0);
    let retina_3x = MediaContext::screen(1024.0, 768.0).with_dpr(3.0);

    // Test various resolution queries
    let min_2x = MediaQuery::parse("(min-resolution: 2dppx)").unwrap();
    let min_192dpi = MediaQuery::parse("(min-resolution: 192dpi)").unwrap(); // 2x in dpi

    assert!(!standard_dpi.evaluate(&min_2x));
    assert!(retina_2x.evaluate(&min_2x));
    assert!(retina_3x.evaluate(&min_2x));

    // 192dpi = 2dppx
    assert!(!standard_dpi.evaluate(&min_192dpi));
    assert!(retina_2x.evaluate(&min_192dpi));

    // Max resolution
    let max_1x = MediaQuery::parse("(max-resolution: 1dppx)").unwrap();
    assert!(standard_dpi.evaluate(&max_1x));
    assert!(!retina_2x.evaluate(&max_1x));
}

/// Tests common retina image patterns
#[test]
fn test_retina_image_patterns() {
    // Common pattern: serve @2x images on high-DPI displays
    let query_2x = MediaQuery::parse("(min-resolution: 2dppx)").unwrap();
    let query_2x_webkit = MediaQuery::parse("(min-resolution: 192dpi)").unwrap();

    let ctx_1x = MediaContext::screen(1024.0, 768.0).with_dpr(1.0);
    let ctx_2x = MediaContext::screen(1024.0, 768.0).with_dpr(2.0);
    let ctx_3x = MediaContext::screen(1024.0, 768.0).with_dpr(3.0);

    // @1x device - serve regular images
    assert!(!ctx_1x.evaluate(&query_2x));
    assert!(!ctx_1x.evaluate(&query_2x_webkit));

    // @2x device - serve @2x images
    assert!(ctx_2x.evaluate(&query_2x));
    assert!(ctx_2x.evaluate(&query_2x_webkit));

    // @3x device - also gets @2x images (unless we have @3x query)
    assert!(ctx_3x.evaluate(&query_2x));
}

// ============================================================================
// User Preferences Tests
// ============================================================================

/// Tests prefers-color-scheme media feature
#[test]
fn test_prefers_color_scheme() {
    let light_ctx = MediaContext::screen(1024.0, 768.0).with_color_scheme(ColorScheme::Light);
    let dark_ctx = MediaContext::screen(1024.0, 768.0).with_color_scheme(ColorScheme::Dark);
    let no_pref_ctx = MediaContext::screen(1024.0, 768.0); // Defaults to no-preference

    let dark_query = MediaQuery::parse("(prefers-color-scheme: dark)").unwrap();
    let light_query = MediaQuery::parse("(prefers-color-scheme: light)").unwrap();
    let no_pref_query = MediaQuery::parse("(prefers-color-scheme: no-preference)").unwrap();

    // Dark mode
    assert!(dark_ctx.evaluate(&dark_query));
    assert!(!dark_ctx.evaluate(&light_query));

    // Light mode
    assert!(!light_ctx.evaluate(&dark_query));
    assert!(light_ctx.evaluate(&light_query));
    assert!(!light_ctx.evaluate(&no_pref_query));

    // No preference - matches only no-preference queries
    assert!(!no_pref_ctx.evaluate(&dark_query));
    assert!(!no_pref_ctx.evaluate(&light_query));
    assert!(no_pref_ctx.evaluate(&no_pref_query));
}

/// Tests prefers-reduced-motion media feature
#[test]
fn test_prefers_reduced_motion() {
    let normal_ctx = MediaContext::screen(1024.0, 768.0);
    let reduced_ctx = MediaContext::screen(1024.0, 768.0).with_reduced_motion(true);

    let reduce_query = MediaQuery::parse("(prefers-reduced-motion: reduce)").unwrap();
    let no_pref_query = MediaQuery::parse("(prefers-reduced-motion: no-preference)").unwrap();

    // Normal user
    assert!(!normal_ctx.evaluate(&reduce_query));
    assert!(normal_ctx.evaluate(&no_pref_query));

    // User prefers reduced motion
    assert!(reduced_ctx.evaluate(&reduce_query));
    assert!(!reduced_ctx.evaluate(&no_pref_query));
}

/// Tests prefers-reduced-transparency media feature
#[test]
fn test_prefers_reduced_transparency() {
    let normal_ctx = MediaContext::screen(1024.0, 768.0);
    let reduced_ctx = MediaContext::screen(1024.0, 768.0).with_reduced_transparency(true);

    let reduce_query = MediaQuery::parse("(prefers-reduced-transparency: reduce)").unwrap();
    let no_pref_query = MediaQuery::parse("(prefers-reduced-transparency: no-preference)").unwrap();

    assert!(!normal_ctx.evaluate(&reduce_query));
    assert!(normal_ctx.evaluate(&no_pref_query));

    assert!(reduced_ctx.evaluate(&reduce_query));
    assert!(!reduced_ctx.evaluate(&no_pref_query));
}

/// Tests prefers-contrast media feature
#[test]
fn test_prefers_contrast() {
    let normal_ctx = MediaContext::screen(1024.0, 768.0);
    let more_ctx = MediaContext::screen(1024.0, 768.0).with_prefers_contrast(ContrastPreference::More);
    let less_ctx = MediaContext::screen(1024.0, 768.0).with_prefers_contrast(ContrastPreference::Less);
    let custom_ctx = MediaContext::screen(1024.0, 768.0).with_prefers_contrast(ContrastPreference::Custom);

    let more_query = MediaQuery::parse("(prefers-contrast: more)").unwrap();
    let less_query = MediaQuery::parse("(prefers-contrast: less)").unwrap();
    let custom_query = MediaQuery::parse("(prefers-contrast: custom)").unwrap();
    let no_pref_query = MediaQuery::parse("(prefers-contrast: no-preference)").unwrap();

    // Defaults to no-preference
    assert!(!normal_ctx.evaluate(&more_query));
    assert!(!normal_ctx.evaluate(&less_query));
    assert!(!normal_ctx.evaluate(&custom_query));
    assert!(normal_ctx.evaluate(&no_pref_query));

    assert!(more_ctx.evaluate(&more_query));
    assert!(!more_ctx.evaluate(&less_query));
    assert!(!more_ctx.evaluate(&custom_query));
    assert!(!more_ctx.evaluate(&no_pref_query));

    assert!(less_ctx.evaluate(&less_query));
    assert!(!less_ctx.evaluate(&more_query));
    assert!(!less_ctx.evaluate(&custom_query));
    assert!(!less_ctx.evaluate(&no_pref_query));

    assert!(custom_ctx.evaluate(&custom_query));
    assert!(!custom_ctx.evaluate(&more_query));
    assert!(!custom_ctx.evaluate(&less_query));
    assert!(!custom_ctx.evaluate(&no_pref_query));
}

/// Tests env override for prefers-contrast
#[test]
fn env_override_prefers_contrast() {
    let guard = EnvGuard::new("FASTR_PREFERS_CONTRAST", Some("high"));
    let ctx = MediaContext::screen(800.0, 600.0).with_env_overrides();
    assert!(matches!(ctx.prefers_contrast, ContrastPreference::More));
    drop(guard);

    let guard_low = EnvGuard::new("FASTR_PREFERS_CONTRAST", Some("low"));
    let ctx = MediaContext::screen(800.0, 600.0).with_env_overrides();
    assert!(matches!(ctx.prefers_contrast, ContrastPreference::Less));
    drop(guard_low);

    let guard_custom = EnvGuard::new("FASTR_PREFERS_CONTRAST", Some("forced"));
    let ctx = MediaContext::screen(800.0, 600.0).with_env_overrides();
    assert!(matches!(ctx.prefers_contrast, ContrastPreference::Custom));
    drop(guard_custom);

    let guard_invalid = EnvGuard::new("FASTR_PREFERS_CONTRAST", Some("unknown"));
    let ctx = MediaContext::screen(800.0, 600.0)
        .with_prefers_contrast(ContrastPreference::More)
        .with_env_overrides();
    assert!(matches!(ctx.prefers_contrast, ContrastPreference::More));
    drop(guard_invalid);
}

/// Tests env override for prefers-reduced-transparency
#[test]
fn env_override_prefers_reduced_transparency() {
    let guard = EnvGuard::new("FASTR_PREFERS_REDUCED_TRANSPARENCY", Some("reduce"));
    let ctx = MediaContext::screen(800.0, 600.0).with_env_overrides();
    assert!(ctx.prefers_reduced_transparency);
    drop(guard);

    let guard_no_pref = EnvGuard::new("FASTR_PREFERS_REDUCED_TRANSPARENCY", Some("no-preference"));
    let ctx = MediaContext::screen(800.0, 600.0).with_env_overrides();
    assert!(!ctx.prefers_reduced_transparency);
    drop(guard_no_pref);

    let guard_invalid = EnvGuard::new("FASTR_PREFERS_REDUCED_TRANSPARENCY", Some("invalid"));
    let ctx = MediaContext::screen(800.0, 600.0).with_env_overrides();
    assert!(!ctx.prefers_reduced_transparency);
    drop(guard_invalid);
}

#[test]
fn media_query_cache_reuses_existing_entry() {
    let query = MediaQuery::parse("(min-width: 10px)").unwrap();
    let ctx = MediaContext::screen(100.0, 100.0);
    let mut cache = MediaQueryCache::default();

    assert!(ctx.evaluate_with_cache(&query, Some(&mut cache)));
    let first_len = cache.len();
    assert!(ctx.evaluate_with_cache(&query, Some(&mut cache)));
    assert_eq!(cache.len(), first_len, "cache should not grow on repeat evaluation");
}

/// Tests env override for prefers-reduced-data
#[test]
fn env_override_prefers_reduced_data() {
    let guard = EnvGuard::new("FASTR_PREFERS_REDUCED_DATA", Some("reduce"));
    let ctx = MediaContext::screen(800.0, 600.0).with_env_overrides();
    assert!(ctx.prefers_reduced_data);
    drop(guard);

    let guard_invalid = EnvGuard::new("FASTR_PREFERS_REDUCED_DATA", Some("invalid"));
    let ctx = MediaContext::screen(800.0, 600.0).with_env_overrides();
    assert!(!ctx.prefers_reduced_data);
    drop(guard_invalid);
}

/// Tests prefers-reduced-data media feature evaluation
#[test]
fn test_prefers_reduced_data() {
    let normal_ctx = MediaContext::screen(1024.0, 768.0);
    let reduced_ctx = MediaContext::screen(1024.0, 768.0).with_reduced_data(true);

    let reduce_query = MediaQuery::parse("(prefers-reduced-data: reduce)").unwrap();
    let no_pref_query = MediaQuery::parse("(prefers-reduced-data: no-preference)").unwrap();

    assert!(!normal_ctx.evaluate(&reduce_query));
    assert!(normal_ctx.evaluate(&no_pref_query));

    assert!(reduced_ctx.evaluate(&reduce_query));
    assert!(!reduced_ctx.evaluate(&no_pref_query));
}

// ============================================================================
// Pointer and Hover Tests
// ============================================================================

/// Tests pointer and hover media features
#[test]
fn test_pointer_and_hover() {
    let desktop = MediaContext::screen(1024.0, 768.0);
    let mobile = MediaContext::mobile(375.0, 667.0);

    // Pointer tests
    let fine_pointer = MediaQuery::parse("(pointer: fine)").unwrap();
    let coarse_pointer = MediaQuery::parse("(pointer: coarse)").unwrap();
    let no_pointer = MediaQuery::parse("(pointer: none)").unwrap();

    assert!(desktop.evaluate(&fine_pointer));
    assert!(!desktop.evaluate(&coarse_pointer));
    assert!(!desktop.evaluate(&no_pointer));

    assert!(!mobile.evaluate(&fine_pointer));
    assert!(mobile.evaluate(&coarse_pointer));
    assert!(!mobile.evaluate(&no_pointer));

    // Hover tests
    let can_hover = MediaQuery::parse("(hover: hover)").unwrap();
    let no_hover = MediaQuery::parse("(hover: none)").unwrap();

    assert!(desktop.evaluate(&can_hover));
    assert!(!desktop.evaluate(&no_hover));

    assert!(!mobile.evaluate(&can_hover));
    assert!(mobile.evaluate(&no_hover));
}

// ============================================================================
// Complex Query Tests
// ============================================================================

/// Tests complex combined media queries
#[test]
fn test_complex_queries() {
    let ctx = MediaContext::screen(1024.0, 768.0)
        .with_dpr(2.0)
        .with_color_scheme(ColorScheme::Dark);

    // Multiple conditions
    let query = MediaQuery::parse("screen and (min-width: 768px) and (orientation: landscape)").unwrap();
    assert!(ctx.evaluate(&query));

    // Width range
    let range_query = MediaQuery::parse("(min-width: 768px) and (max-width: 1200px)").unwrap();
    assert!(ctx.evaluate(&range_query));

    // Out of range
    let out_of_range = MediaQuery::parse("(min-width: 1200px) and (max-width: 1400px)").unwrap();
    assert!(!ctx.evaluate(&out_of_range));
}

/// Tests query list OR logic
#[test]
fn test_query_list_or_logic() {
    let ctx = MediaContext::screen(1024.0, 768.0);

    // Either condition matches
    let queries = MediaQuery::parse_list("(min-width: 1200px), (orientation: landscape)").unwrap();
    assert!(ctx.evaluate_list(&queries)); // orientation matches

    // First matches
    let queries = MediaQuery::parse_list("(min-width: 768px), (max-width: 400px)").unwrap();
    assert!(ctx.evaluate_list(&queries));

    // Neither matches
    let queries = MediaQuery::parse_list("(min-width: 1200px), (max-width: 400px)").unwrap();
    assert!(!ctx.evaluate_list(&queries));

    // Multiple media types
    let queries = MediaQuery::parse_list("print, screen and (min-width: 768px)").unwrap();
    assert!(ctx.evaluate_list(&queries)); // screen matches
}

/// Tests NOT modifier edge cases
#[test]
fn test_not_modifier() {
    let screen_ctx = MediaContext::screen(1024.0, 768.0);
    let print_ctx = MediaContext::print(816.0, 1056.0);

    // NOT media type
    let not_print = MediaQuery::parse("not print").unwrap();
    assert!(screen_ctx.evaluate(&not_print));
    assert!(!print_ctx.evaluate(&not_print));

    // NOT with features
    let not_screen_color = MediaQuery::parse("not screen and (color)").unwrap();
    // "not screen and (color)" negates the entire query
    // For screen with color: screen AND color = true, NOT true = false
    assert!(!screen_ctx.evaluate(&not_screen_color));

    // NOT in query list (each query is independent)
    let queries = MediaQuery::parse_list("not print, (min-width: 1200px)").unwrap();
    // "not print" matches for screen, so whole list matches
    assert!(screen_ctx.evaluate_list(&queries));
}

// ============================================================================
// Edge Cases and Error Handling
// ============================================================================

/// Tests zero and boundary values
#[test]
fn test_boundary_values() {
    // Zero width
    let zero_ctx = MediaContext::screen(0.0, 768.0);
    let min_width = MediaQuery::parse("(min-width: 0px)").unwrap();
    assert!(zero_ctx.evaluate(&min_width));

    // Exact match
    let ctx_768 = MediaContext::screen(768.0, 1024.0);
    let exact_768 = MediaQuery::parse("(width: 768px)").unwrap();
    assert!(ctx_768.evaluate(&exact_768));

    // Just below threshold
    let ctx_767 = MediaContext::screen(767.0, 1024.0);
    let min_768 = MediaQuery::parse("(min-width: 768px)").unwrap();
    assert!(!ctx_767.evaluate(&min_768));

    // Just above threshold
    let ctx_769 = MediaContext::screen(769.0, 1024.0);
    assert!(ctx_769.evaluate(&min_768));
}

/// Tests aspect ratio calculations
#[test]
fn test_aspect_ratio_edge_cases() {
    // 16:9 exactly
    let ctx_16_9 = MediaContext::screen(1920.0, 1080.0);
    let query_16_9 = MediaQuery::parse("(aspect-ratio: 16/9)").unwrap();
    assert!(ctx_16_9.evaluate(&query_16_9));

    // 4:3
    let ctx_4_3 = MediaContext::screen(1024.0, 768.0);
    let query_4_3 = MediaQuery::parse("(aspect-ratio: 4/3)").unwrap();
    assert!(ctx_4_3.evaluate(&query_4_3));

    // Min aspect ratio
    let min_16_9 = MediaQuery::parse("(min-aspect-ratio: 16/9)").unwrap();
    assert!(ctx_16_9.evaluate(&min_16_9));
    assert!(!ctx_4_3.evaluate(&min_16_9)); // 4:3 is less than 16:9

    // Max aspect ratio
    let max_4_3 = MediaQuery::parse("(max-aspect-ratio: 4/3)").unwrap();
    assert!(ctx_4_3.evaluate(&max_4_3));
    assert!(!ctx_16_9.evaluate(&max_4_3)); // 16:9 is greater than 4:3
}

/// Tests parsing error cases
#[test]
fn test_parse_errors() {
    // Empty query should fail
    assert!(MediaQuery::parse("").is_err());

    // Unknown feature
    assert!(MediaQuery::parse("(unknown-feature: value)").is_err());

    // Missing value for feature that requires it
    assert!(MediaQuery::parse("(min-width)").is_err());

    // Invalid orientation value
    assert!(MediaQuery::parse("(orientation: diagonal)").is_err());

    // Invalid resolution format
    assert!(MediaQuery::parse("(min-resolution: 2invalid)").is_err());

    // Missing closing paren
    assert!(MediaQuery::parse("(min-width: 768px").is_err());
}

/// Tests whitespace handling
#[test]
fn test_whitespace_handling() {
    let ctx = MediaContext::screen(1024.0, 768.0);

    // Various whitespace patterns that should all parse identically
    let queries = [
        "(min-width: 768px)",
        "( min-width: 768px )",
        "(min-width:768px)",
        "  (min-width: 768px)  ",
        "(min-width:  768px)",
    ];

    for query_str in queries {
        let query = MediaQuery::parse(query_str).unwrap_or_else(|_| panic!("Failed to parse: '{}'", query_str));
        assert!(ctx.evaluate(&query), "Query '{}' should match", query_str);
    }
}

/// Tests case insensitivity
#[test]
fn test_case_insensitivity() {
    let ctx = MediaContext::screen(1024.0, 768.0);

    let queries = [
        "SCREEN",
        "Screen",
        "screen",
        "(MIN-WIDTH: 768px)",
        "(Min-Width: 768px)",
        "(ORIENTATION: LANDSCAPE)",
    ];

    for query_str in queries {
        let query = MediaQuery::parse(query_str).unwrap_or_else(|_| panic!("Failed to parse: '{}'", query_str));
        assert!(ctx.evaluate(&query), "Query '{}' should match", query_str);
    }
}

// ============================================================================
// Length Unit Tests
// ============================================================================

/// Tests various CSS length units in media queries
#[test]
fn test_length_units() {
    let ctx = MediaContext::screen(1024.0, 768.0);

    // Pixel values
    let px_query = MediaQuery::parse("(min-width: 1024px)").unwrap();
    assert!(ctx.evaluate(&px_query));

    // Em values (assuming 16px base)
    let em_query = MediaQuery::parse("(min-width: 64em)").unwrap(); // 64 * 16 = 1024
    assert!(ctx.evaluate(&em_query));

    // Rem values
    let rem_query = MediaQuery::parse("(min-width: 64rem)").unwrap();
    assert!(ctx.evaluate(&rem_query));

    // Zero without unit
    let zero_query = MediaQuery::parse("(min-width: 0)").unwrap();
    assert!(ctx.evaluate(&zero_query));
}

// ============================================================================
// Real-World Pattern Tests
// ============================================================================

/// Tests common CSS framework patterns
#[test]
fn test_real_world_patterns() {
    // Container width pattern (Bootstrap-style)
    // These are typical viewport widths and the corresponding container max-widths
    let container_queries = [
        ("(min-width: 576px)", 576.0),   // sm viewport triggers 540px container
        ("(min-width: 768px)", 768.0),   // md viewport triggers 720px container
        ("(min-width: 992px)", 992.0),   // lg viewport triggers 960px container
        ("(min-width: 1200px)", 1200.0), // xl viewport triggers 1140px container
        ("(min-width: 1400px)", 1400.0), // xxl viewport triggers 1320px container
    ];

    for (query_str, viewport_width) in container_queries {
        let query = MediaQuery::parse(query_str).unwrap();
        let ctx = MediaContext::screen(viewport_width, 768.0);
        assert!(
            ctx.evaluate(&query),
            "Viewport {} should trigger {}",
            viewport_width,
            query_str
        );
    }
}

/// Tests mobile navigation hide/show pattern
#[test]
fn test_mobile_nav_pattern() {
    // Common pattern: hide mobile nav on large screens
    let hide_mobile_nav = MediaQuery::parse("(min-width: 992px)").unwrap();
    let show_mobile_nav = MediaQuery::parse("(max-width: 991px)").unwrap();

    let mobile = MediaContext::mobile(375.0, 667.0);
    let tablet = MediaContext::screen(768.0, 1024.0);
    let desktop = MediaContext::screen(1280.0, 800.0);

    // Mobile: show mobile nav
    assert!(!mobile.evaluate(&hide_mobile_nav));
    assert!(mobile.evaluate(&show_mobile_nav));

    // Tablet: show mobile nav
    assert!(!tablet.evaluate(&hide_mobile_nav));
    assert!(tablet.evaluate(&show_mobile_nav));

    // Desktop: hide mobile nav
    assert!(desktop.evaluate(&hide_mobile_nav));
    assert!(!desktop.evaluate(&show_mobile_nav));
}

/// Tests dark mode toggle pattern
#[test]
fn test_dark_mode_pattern() {
    let light_ctx = MediaContext::screen(1024.0, 768.0).with_color_scheme(ColorScheme::Light);
    let dark_ctx = MediaContext::screen(1024.0, 768.0).with_color_scheme(ColorScheme::Dark);

    let dark_mode = MediaQuery::parse("(prefers-color-scheme: dark)").unwrap();

    // Light mode user sees light theme
    assert!(!light_ctx.evaluate(&dark_mode));

    // Dark mode user sees dark theme
    assert!(dark_ctx.evaluate(&dark_mode));
}

#[test]
fn media_query_cache_reused_between_collections() {
    let sheet = parse_stylesheet(
        "@media (min-width: 10px) { @font-face { font-family: cache; src: url(cache.woff); } .foo { color: red; } }",
    )
    .unwrap();
    let ctx = MediaContext::screen(100.0, 100.0);
    let mut cache = MediaQueryCache::default();

    let faces = sheet.collect_font_face_rules_with_cache(&ctx, Some(&mut cache));
    assert_eq!(faces.len(), 1);
    let cached_len = cache.len();

    let rules = sheet.collect_style_rules_with_cache(&ctx, Some(&mut cache));
    assert_eq!(rules.len(), 1);
    assert_eq!(cache.len(), cached_len);
}
