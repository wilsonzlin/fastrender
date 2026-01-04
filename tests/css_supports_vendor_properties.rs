use fastrender::css::parser::parse_stylesheet_with_media;
use fastrender::css::supports::supports_declaration;
use fastrender::css::types::CssRule;
use fastrender::style::media::{MediaContext, MediaQueryCache};

#[test]
fn supports_declaration_accepts_targeted_vendor_properties() {
  assert!(supports_declaration("-webkit-hyphens", "none"));
  assert!(supports_declaration("-WEBKIT-HYPHENS", "none"));
  assert!(supports_declaration("-moz-orient", "inline"));
  assert!(supports_declaration("-MoZ-OrIeNt", "inline"));
  assert!(!supports_declaration("-webkit-not-a-real-prop", "none"));
}

#[test]
fn supports_vendor_properties_prevent_pruning_tailwind_reset_blocks() {
  let css = r#"
    @supports (-webkit-hyphens:none) or (-moz-orient:inline) {
      @layer properties {
        :root { --tw-test: 1; }
      }
    }
  "#;

  let media_ctx = MediaContext::screen(800.0, 600.0);
  let mut cache = MediaQueryCache::default();
  let sheet =
    parse_stylesheet_with_media(css, &media_ctx, Some(&mut cache)).expect("parse stylesheet");

  let supports_rule = sheet
    .rules
    .iter()
    .find_map(|rule| match rule {
      CssRule::Supports(rule) => Some(rule),
      _ => None,
    })
    .expect("@supports block should not be pruned");

  let layer_rule = supports_rule
    .rules
    .iter()
    .find_map(|rule| match rule {
      CssRule::Layer(rule) => Some(rule),
      _ => None,
    })
    .expect("@layer rule should survive inside @supports block");

  assert!(
    layer_rule.rules.iter().any(|rule| matches!(rule, CssRule::Style(_))),
    "expected style rule inside @layer block"
  );
}
