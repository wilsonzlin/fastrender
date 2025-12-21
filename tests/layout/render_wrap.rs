use fastrender::api::FastRender;

fn ink_bounds(pixmap: &tiny_skia::Pixmap) -> Option<(usize, usize)> {
  let data = pixmap.data();
  let width = pixmap.width() as usize;
  let height = pixmap.height() as usize;
  let mut min_y = height;
  let mut max_y = 0;

  for y in 0..height {
    let row_start = y * width * 4;
    for x in 0..width {
      let idx = row_start + x * 4;
      let r = data[idx];
      let g = data[idx + 1];
      let b = data[idx + 2];
      let a = data[idx + 3];
      if a > 0 && (r < 250 || g < 250 || b < 250) {
        min_y = min_y.min(y);
        max_y = max_y.max(y);
      }
    }
  }

  (min_y <= max_y).then_some((min_y, max_y))
}

#[test]
fn long_paragraph_wraps_within_viewport() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r#"
        <p>
            Availing himself of the mild, summer-cool weather that now reigned in these latitudes, and in preparation
            for the peculiarly active pursuits shortly to be anticipated, Perth, the begrimed, blistered old blacksmith,
            had not removed his portable forge to the hold again, after concluding his contributory work for Ahab's leg,
            but still retained it on deck, fast lashed to ringbolts by the foremast; being now almost incessantly invoked
            by the headsmen, and harpooneers, and bowsmen to do some little job for them.
        </p>
    "#;

  let pixmap = renderer
    .render_html(html, 400, 200)
    .expect("render long paragraph");

  let (min_y, max_y) = ink_bounds(&pixmap).expect("text should render");
  let ink_height = max_y - min_y + 1;

  assert!(
    ink_height > 50,
    "text should wrap to multiple lines (ink height {}px)",
    ink_height
  );
}
