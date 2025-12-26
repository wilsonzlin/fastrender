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

#[test]
fn display_none_svg_defs_apply_filters() {
  thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(|| {
      let mut renderer = FastRender::new().expect("renderer");
      let html = r#"
      <style>
        body { margin: 0; }
        .box {
          width: 10px;
          height: 10px;
          background: rgb(0, 0, 255);
          filter: url(#recolor);
        }
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
      "#;

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
        resolver.resolve("#recolor").is_some(),
        "svg filter resolver should find #recolor in defs"
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
