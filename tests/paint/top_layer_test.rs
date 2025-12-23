use fastrender::FastRender;

fn pixel_rgba(pixmap: &tiny_skia::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let p = pixmap.pixel(x, y).unwrap();
  (p.red(), p.green(), p.blue(), p.alpha())
}

#[test]
fn modal_dialog_adds_backdrop_and_inert() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r#"
    <style>
      body { margin: 0; }
      button { width: 40px; height: 40px; background: rgb(0, 255, 0); border: none; }
      button:focus { background: rgb(255, 0, 0); }
      dialog { width: 60px; height: 60px; padding: 0; }
    </style>
    <button data-fastr-focus="true"></button>
    <dialog open data-fastr-modal="true"></dialog>
  "#;

  let pixmap = renderer.render_html(html, 120, 120).expect("paint dialog");
  let (r, g, b, _) = pixel_rgba(&pixmap, 5, 5);

  assert!(
    g > r + 80,
    "inert background should keep focus state off (r={r}, g={g}, b={b})"
  );
  assert!(g > 120, "backdrop-tinted green should remain visible");
}

#[test]
fn non_modal_dialog_allows_focus() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r#"
    <style>
      body { margin: 0; }
      button { width: 40px; height: 40px; background: rgb(0, 255, 0); border: none; }
      button:focus { background: rgb(255, 0, 0); }
      dialog { width: 60px; height: 60px; padding: 0; }
    </style>
    <button data-fastr-focus="true"></button>
    <dialog open></dialog>
  "#;

  let pixmap = renderer.render_html(html, 120, 120).expect("paint dialog");
  let (r, g, b, _) = pixel_rgba(&pixmap, 5, 5);

  assert!(
    r > 200 && g < 80 && b < 80,
    "focus should remain active without modal inertness"
  );
}

#[test]
fn popovers_stack_in_dom_order() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r#"
    <style>
      body { margin: 0; }
      .base { position: fixed; inset: 0; background: rgb(0, 128, 0); }
      [popover] { width: 60px; height: 60px; top: 10px; left: 10px; }
      #first { background: rgb(0, 0, 255); }
      #second { background: rgb(255, 255, 0); top: 20px; left: 20px; }
    </style>
    <div class="base"></div>
    <div id="first" popover open></div>
    <div id="second" popover open></div>
  "#;

  let pixmap = renderer
    .render_html(html, 120, 120)
    .expect("paint popovers");
  let (r, g, b, _) = pixel_rgba(&pixmap, 30, 30);
  let (sr, sg, sb, _) = pixel_rgba(&pixmap, 75, 75);

  assert!(
    sr > 200 && sg > 200,
    "second popover should paint its own area, got ({sr},{sg},{sb})"
  );

  assert!(
    b > 200 && r < 80 && g < 80,
    "DOM order should stack popovers, got ({r},{g},{b})"
  );
}

#[test]
fn dialog_backdrop_respects_author_styles() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r#"
    <style>
      body { margin: 0; }
      dialog { width: 80px; height: 80px; padding: 0; }
      dialog::backdrop { background: rgba(255, 0, 0, 0.5); }
    </style>
    <dialog open data-fastr-modal="true"></dialog>
  "#;

  let pixmap = renderer.render_html(html, 200, 200).expect("paint dialog");
  let (r, g, b, _) = pixel_rgba(&pixmap, 0, 0);

  assert!(
    r > 200 && g > 100 && b > 100,
    "custom backdrop should tint the viewport"
  );
}
