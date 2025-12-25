use fastrender::FastRender;
use fastrender::Pixmap;

fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> [u8; 4] {
  let idx = (y as usize * pixmap.width() as usize + x as usize) * 4;
  let data = pixmap.data();
  [data[idx], data[idx + 1], data[idx + 2], data[idx + 3]]
}

#[test]
fn display_none_svg_defs_apply_filters() {
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

  let pixmap = renderer.render_html(html, 20, 20).expect("render html");
  let sample = pixel(&pixmap, 5, 5);

  assert!(
    sample[0] > sample[2],
    "SVG filter defined in display:none defs should recolor output (got {:?})",
    sample
  );
}
