use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
use fastrender::css::parser::parse_stylesheet;
use fastrender::dom::parse_html;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;

fn generate_cascade_html(nodes: usize, class_variants: usize) -> String {
  let mut html = String::from("<html><head><style>body{margin:0;padding:0;}</style></head><body>");
  html.push_str("<div id=\"root\" class=\"container\">");
  for i in 0..nodes {
    html.push_str("<div class=\"");
    html.push_str(&format!(
      "item c{} c{}\" id=\"item{}\">",
      i % class_variants,
      (i * 3) % class_variants,
      i
    ));
    html.push_str("<span class=\"label\">");
    html.push_str(&format!("Item {}", i));
    html.push_str("</span><p class=\"copy\">Lorem ipsum dolor sit amet</p></div>");
  }
  html.push_str("</div></body></html>");
  html
}

fn generate_cascade_css(class_variants: usize) -> String {
  let mut css = String::from(
    r#"
        body { font-family: sans-serif; line-height: 1.4; }
        #root.container { display: block; padding: 8px; }
        .container > .item { margin: 4px 0; }
        .item .label { font-weight: 600; }
        .item .copy { color: #444; }
    "#,
  );

  for i in 0..class_variants {
    css.push_str(&format!(
            ".c{0} {{ padding: {1}px; border: {2}px solid hsl({3}, 50%, 60%); color: rgb({4}, {5}, {6}); }}\n",
            i,
            (i % 4) + 1,
            (i % 3) + 1,
            (i * 23) % 360,
            (i * 31) % 255,
            (i * 47) % 255,
            (i * 59) % 255
        ));
    css.push_str(&format!(
      ".c{0} .copy {{ font-size: {1}px; }}\n",
      i,
      12 + (i % 6)
    ));
    css.push_str(&format!(
      ".container .c{0} .label {{ text-transform: uppercase; letter-spacing: {1}px; }}\n",
      i,
      (i % 3) as f32 * 0.25
    ));
  }

  // A few compound selectors to exercise matching work
  css.push_str(
    r#"
        .container .item:hover .label { color: #222; }
        .container .item:nth-child(2n) .copy { text-decoration: underline; }
        .container .item:nth-child(3n) .copy { font-style: italic; }
        .container .item:nth-child(5n) { background: linear-gradient(#fafafa, #f0f0f0); }
    "#,
  );

  css
}

fn generate_not_heavy_html(nodes: usize, class_variants: usize) -> String {
  let mut html = String::from("<html><body><div id=\"root\" class=\"container\">");
  for i in 0..nodes {
    html.push_str("<div class=\"");
    html.push_str(&format!(
      "item c{} c{} block{}\" id=\"item{}\">",
      i % class_variants,
      (i * 3) % class_variants,
      (i * 5) % class_variants,
      i
    ));
    html.push_str("<span class=\"label\">");
    html.push_str(&format!("Item {}", i));
    html.push_str("</span></div>");
  }
  html.push_str("</div></body></html>");
  html
}

fn generate_not_heavy_css(class_variants: usize) -> String {
  let mut css = String::new();

  for i in 0..class_variants {
    css.push_str(&format!(
      ":not(:not(.c{0})) {{ border-width: {1}px; }}\n",
      i,
      (i % 3) + 1
    ));
    css.push_str(&format!(
      ":not(:not(.block{0}), :not(.c{0})) {{ padding: {1}px; }}\n",
      i,
      (i % 5) + 1
    ));
    css.push_str(&format!(
      ":not(:not(.c{0})):not(.block{1}) {{ margin: {2}px; }}\n",
      i,
      (i + 1) % class_variants,
      (i % 7) + 1
    ));
  }

  css
}

fn cascade_benchmark(c: &mut Criterion) {
  let node_count = 400;
  let class_variants = 24;
  let html = generate_cascade_html(node_count, class_variants);
  let css = generate_cascade_css(class_variants);

  let dom = parse_html(&html).expect("parse html");
  let stylesheet = parse_stylesheet(&css).expect("parse stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);

  c.bench_function("cascade apply_styles 400 nodes/24 classes", |b| {
    b.iter(|| {
      let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
      black_box(styled);
    });
  });
}

fn cascade_not_benchmark(c: &mut Criterion) {
  let node_count = 300;
  let class_variants = 24;
  let html = generate_not_heavy_html(node_count, class_variants);
  let css = generate_not_heavy_css(class_variants);

  let dom = parse_html(&html).expect("parse html");
  let stylesheet = parse_stylesheet(&css).expect("parse stylesheet");
  let media = MediaContext::screen(1280.0, 720.0);

  c.bench_function("cascade apply_styles not-heavy :not selectors", |b| {
    b.iter(|| {
      let styled = apply_styles_with_media(black_box(&dom), black_box(&stylesheet), &media);
      black_box(styled);
    });
  });
}

criterion_group!(benches, cascade_benchmark, cascade_not_benchmark);
criterion_main!(benches);
