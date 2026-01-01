use fastrender::css::parser::{parse_stylesheet_with_media, parse_stylesheet_with_media_cached};
use fastrender::style::media::{MediaContext, MediaQueryCache};

#[test]
fn memoized_stylesheet_parse_matches_uncached_output() {
  let css = r#"
p { color: red; }

@supports (opacity: 0.5) {
  .supported { opacity: 0.5; }
}

@supports (opacity: foo) {
  .unsupported { opacity: 1; }
}

@media (max-width: 500px) {
  .small { color: blue; }
}

@media (min-width: 700px) {
  .large { color: green; }
}
"#;

  let large_ctx = MediaContext::screen(800.0, 600.0);
  let small_ctx = MediaContext::screen(400.0, 600.0);

  for (label, ctx) in [("large", &large_ctx), ("small", &small_ctx)] {
    let mut baseline_cache = MediaQueryCache::default();
    let mut memo_cache = MediaQueryCache::default();

    let baseline =
      parse_stylesheet_with_media(css, ctx, Some(&mut baseline_cache)).expect("baseline parse");
    let memoized = parse_stylesheet_with_media_cached(
      css,
      Some("https://example.test/style.css"),
      ctx,
      Some(&mut memo_cache),
    )
    .expect("memoized parse");

    assert_eq!(
      format!("{baseline:?}"),
      format!("{memoized:?}"),
      "memoized parse output should match baseline for {label} media context"
    );
  }

  let mut cache_large = MediaQueryCache::default();
  let mut cache_small = MediaQueryCache::default();
  let large = parse_stylesheet_with_media_cached(
    css,
    Some("https://example.test/style.css"),
    &large_ctx,
    Some(&mut cache_large),
  )
  .expect("large ctx parse");
  let small = parse_stylesheet_with_media_cached(
    css,
    Some("https://example.test/style.css"),
    &small_ctx,
    Some(&mut cache_small),
  )
  .expect("small ctx parse");

  assert_ne!(
    format!("{large:?}"),
    format!("{small:?}"),
    "different media contexts should produce different pruned stylesheets"
  );
}

