use fastrender::image_loader::ImageCache;
use fastrender::paint::svg_filter::SvgFilterResolver;
use fastrender::FastRender;
use fastrender::Pixmap;
use std::thread;

fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> [u8; 4] {
  let idx = (y as usize * pixmap.width() as usize + x as usize) * 4;
  let data = pixmap.data();
  [data[idx], data[idx + 1], data[idx + 2], data[idx + 3]]
}

fn run_display_none_filter_test(filter_value: &str) {
  let filter_value = filter_value.to_string();
  thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(move || {
      let mut renderer = FastRender::new().expect("renderer");
      let html = format!(
        r#"
        <style>
          body {{ margin: 0; }}
          .box {{
            width: 10px;
            height: 10px;
            background: rgb(0, 0, 255);
            filter: {filter_value};
          }}
        </style>
        <svg style="display: none">
          <defs>
            <filter id="recolor" color-interpolation-filters="sRGB">
              <feColorMatrix type="matrix"
                values="0 0 0 0 1  0 0 0 0 0  0 0 0 0 0  0 0 0 1 0" />
            </filter>
          </defs>
        </svg>
        <div class="box"></div>
        "#,
        filter_value = filter_value
      );

      let dom = renderer.parse_html(html).expect("parse html");
      let fragments = renderer
        .layout_document(&dom, 20, 20)
        .expect("layout document");
      let defs = fragments
        .svg_filter_defs
        .as_ref()
        .map(|defs| defs.contains_key("recolor"))
        .unwrap_or(false);
      assert!(
        defs,
        "SVG filter defs from the DOM should be captured even when not rendered"
      );
      let has_filter = fragments.iter_fragments().any(|frag| {
        frag
          .style
          .as_ref()
          .is_some_and(|style| !style.filter.is_empty())
      });
      assert!(
        has_filter,
        "filter:url(#recolor) should be parsed onto the box"
      );

      let cache = ImageCache::new();
      let mut resolver = SvgFilterResolver::new(
        fragments.svg_filter_defs.clone(),
        vec![&fragments.root],
        Some(&cache),
      );
      assert!(
        resolver.resolve(&filter_value).is_some(),
        "svg filter resolver should find recolor filter in defs"
      );

      let pixmap = renderer.paint(&fragments, 20, 20).expect("paint");
      let sample = pixel(&pixmap, 5, 5);

      assert!(
        sample[0] > sample[2],
        "SVG filter defined in display:none defs should recolor output (got {:?})",
        sample
      );
    })
    .expect("spawn test thread")
    .join()
    .expect("test thread panicked");
}

#[test]
fn display_none_svg_defs_apply_filters() {
  run_display_none_filter_test("url(#recolor)");
}

#[test]
fn display_none_svg_defs_apply_filters_with_quoted_url() {
  run_display_none_filter_test("url(\"#recolor\")");
}
