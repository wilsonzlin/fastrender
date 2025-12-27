use fastrender::api::FastRender;
use fastrender::style::color::Rgba;

#[test]
fn slotted_background_paints_through_shadow() {
  let html = r#"
    <style>body { margin: 0; }</style>
    <div>
      <template shadowroot="open">
        <style>
          ::slotted(span) {
            display: block;
            width: 40px;
            height: 40px;
            background: rgb(220, 0, 0);
          }
        </style>
        <slot></slot>
      </template>
      <span>slot me</span>
    </div>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let pixmap = renderer
    .render_html(html, 60, 60)
    .expect("render slotted element");
  let data = pixmap.data();

  // Sample a pixel inside the expected slotted rectangle.
  let x = 5usize;
  let y = 5usize;
  let idx = (y * 60 + x) * 4;
  let sample = (data[idx], data[idx + 1], data[idx + 2], data[idx + 3]);

  assert_eq!(sample, (220, 0, 0, 255), "slotted content should paint red");

  // Ensure background remains white outside the slotted box.
  let outside_idx = (50 * 60 + 50) * 4;
  let outside = (
    data[outside_idx],
    data[outside_idx + 1],
    data[outside_idx + 2],
    data[outside_idx + 3],
  );
  assert_eq!(
    outside,
    (255, 255, 255, 255),
    "outside region should remain untouched"
  );
}
