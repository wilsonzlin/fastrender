use fastrender::api::FastRender;
use fastrender::style::color::Rgba;
use fastrender::tree::fragment_tree::FragmentNode;

fn find_fragment_with_background<'a>(
  node: &'a FragmentNode,
  color: Rgba,
) -> Option<&'a FragmentNode> {
  let mut stack = vec![node];
  while let Some(fragment) = stack.pop() {
    if fragment
      .style
      .as_ref()
      .is_some_and(|style| style.background_color == color)
    {
      return Some(fragment);
    }
    stack.extend(fragment.children.iter());
  }
  None
}

#[test]
fn break_before_column_forces_new_manual_column() {
  let mut renderer = FastRender::builder()
    .columns(2, 20.0, 200.0)
    .build()
    .unwrap();

  let html = r#"
    <html>
      <head>
        <style>
          body { margin: 0; column-count: 2; column-gap: 20px; }
          #first { height: 60px; background: rgb(200, 40, 40); }
          #second { height: 40px; background: rgb(40, 180, 90); break-before: column; }
        </style>
      </head>
      <body>
        <div id="first"></div>
        <div id="second"></div>
      </body>
    </html>
  "#;

  let dom = renderer.parse_html(html).expect("parse HTML");
  let fragments = renderer
    .layout_document(&dom, 400, 200)
    .expect("layout with manual columns");

  let first_color = Rgba::rgb(200, 40, 40);
  let second_color = Rgba::rgb(40, 180, 90);

  assert_eq!(
    fragments.additional_fragments.len(),
    1,
    "break-before: column should create a second column fragment even when content fits"
  );

  assert!(
    find_fragment_with_background(&fragments.root, first_color).is_some(),
    "first block should remain in the first column fragment"
  );
  assert!(
    find_fragment_with_background(&fragments.root, second_color).is_none(),
    "second block should not remain in the first column when a column break is requested"
  );

  let second_column = &fragments.additional_fragments[0];
  let second_fragment = find_fragment_with_background(second_column, second_color)
    .expect("second block should start the second column fragment");
  assert!(
    second_fragment.bounds.y().abs() < 0.1,
    "second block should start at the top of the second column"
  );
}

#[test]
fn vertical_writing_mode_break_before_column_forces_new_manual_column() {
  let mut renderer = FastRender::builder()
    .columns(2, 16.0, 200.0)
    .build()
    .unwrap();

  let html = r#"
    <html>
      <head>
        <style>
          body { margin: 0; writing-mode: vertical-rl; column-count: 2; column-gap: 16px; }
          #first { height: 50px; background: rgb(120, 120, 200); }
          #second { height: 50px; background: rgb(20, 160, 140); break-before: column; }
        </style>
      </head>
      <body>
        <div id="first"></div>
        <div id="second"></div>
      </body>
    </html>
  "#;

  let dom = renderer.parse_html(html).expect("parse HTML");
  let fragments = renderer
    .layout_document(&dom, 300, 300)
    .expect("layout vertical writing mode with manual columns");

  let first_color = Rgba::rgb(120, 120, 200);
  let second_color = Rgba::rgb(20, 160, 140);

  assert_eq!(
    fragments.additional_fragments.len(),
    1,
    "column break should yield a second fragment when using manual columns"
  );

  assert!(
    find_fragment_with_background(&fragments.root, first_color).is_some(),
    "first block should stay in the first column fragment"
  );
  assert!(
    find_fragment_with_background(&fragments.root, second_color).is_none(),
    "second block should start a new column instead of sharing the first"
  );

  let second_column = &fragments.additional_fragments[0];
  let second_fragment = find_fragment_with_background(second_column, second_color)
    .expect("second block should appear in the second column fragment");
  assert!(
    second_fragment.bounds.y().abs() < 0.1,
    "second block should start at the column origin in vertical writing mode"
  );
}
