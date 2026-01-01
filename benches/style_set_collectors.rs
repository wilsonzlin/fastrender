use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::css::parser::parse_stylesheet;
use fastrender::style::media::{MediaContext, MediaQueryCache};
use fastrender::style::style_set::StyleSet;

fn generate_large_css(rule_count: usize, decls_per_rule: usize) -> String {
  let mut css = String::new();
  css.push_str(
    r#"
@font-face {
  font-family: "BenchFamily";
  src: url("bench.woff2");
}

@keyframes benchFade {
  from { opacity: 0; }
  to { opacity: 1; }
}
"#,
  );

  for rule_idx in 0..rule_count {
    css.push_str(&format!(".rule-{rule_idx} {{"));
    for decl_idx in 0..decls_per_rule {
      css.push_str(&format!("  --var-{decl_idx}: {rule_idx};"));
    }
    css.push_str("}\n");
  }
  css
}

fn bench_style_set_collectors(c: &mut Criterion) {
  let css = generate_large_css(2000, 8);
  let document = parse_stylesheet(&css).expect("benchmark stylesheet parses");
  let style_set = StyleSet::from_document(document);
  let media_ctx = MediaContext::screen(1024.0, 768.0);

  c.bench_function("css_collect_old_clone_extend_and_collect", |b| {
    b.iter(|| {
      let mut cache = MediaQueryCache::default();
      let mut stylesheet = style_set.document.clone();
      for sheet in style_set.shadows.values() {
        stylesheet.rules.extend(sheet.rules.clone());
      }

      let font_faces = stylesheet.collect_font_face_rules_with_cache(&media_ctx, Some(&mut cache));
      let keyframes = stylesheet.collect_keyframes_with_cache(&media_ctx, Some(&mut cache));
      let has_container_queries = stylesheet.has_container_rules();
      let has_starting_style_rules = stylesheet.has_starting_style_rules();

      black_box((
        font_faces.len(),
        keyframes.len(),
        has_container_queries,
        has_starting_style_rules,
      ));
    })
  });

  c.bench_function("css_collect_new_style_set_iter_and_collect", |b| {
    b.iter(|| {
      let mut cache = MediaQueryCache::default();
      let font_faces =
        style_set.collect_font_face_rules_all_scopes_with_cache(&media_ctx, Some(&mut cache));
      let keyframes =
        style_set.collect_keyframes_all_scopes_with_cache(&media_ctx, Some(&mut cache));
      let has_container_queries = style_set.has_container_rules_any_scope();
      let has_starting_style_rules = style_set.has_starting_style_rules_any_scope();

      black_box((
        font_faces.len(),
        keyframes.len(),
        has_container_queries,
        has_starting_style_rules,
      ));
    })
  });
}

criterion_group!(benches, bench_style_set_collectors);
criterion_main!(benches);
