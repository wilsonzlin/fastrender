use fastrender::tree::fragment_tree::FragmentContent;
use fastrender::FastRender;

fn collect_text_spans<'a>(
  fragment: &'a fastrender::FragmentNode,
  out: &mut Vec<(&'a str, f32, f32)>,
  offset_y: f32,
) {
  let current_y = offset_y + fragment.bounds.y();
  if let FragmentContent::Text { text, .. } = &fragment.content {
    out.push((
      text.as_str(),
      current_y,
      current_y + fragment.bounds.height(),
    ));
  }
  for child in &fragment.children {
    collect_text_spans(child, out, current_y);
  }
}

fn baseline_for_text(fragment: &fastrender::FragmentNode, needle: &str) -> Option<f32> {
  let mut best: Option<f32> = None;
  fn walk<'a>(
    node: &'a fastrender::FragmentNode,
    needle: &str,
    best: &mut Option<f32>,
    offset_y: f32,
  ) {
    let current_y = offset_y + node.bounds.y();
    if let FragmentContent::Text {
      text,
      baseline_offset,
      ..
    } = &node.content
    {
      if text.contains(needle) {
        let baseline = current_y + *baseline_offset;
        *best = Some(best.map_or(baseline, |b| b.min(baseline)));
      }
    }
    for child in &node.children {
      walk(child, needle, best, current_y);
    }
  }

  walk(fragment, needle, &mut best, 0.0);
  best
}

fn span_for_text(root: &fastrender::FragmentNode, needle: &str) -> Option<(f32, f32)> {
  let mut spans = Vec::new();
  collect_text_spans(root, &mut spans, 0.0);
  spans
    .iter()
    .filter(|(text, _, _)| text.contains(needle))
    .fold(None, |acc, (_, y1, y2)| match acc {
      Some((min, max)) => Some((min.min(*y1), max.max(*y2))),
      None => Some((*y1, *y2)),
    })
}

fn span_for_text_x(root: &fastrender::FragmentNode, needle: &str) -> Option<(f32, f32)> {
  fn collect<'a>(
    fragment: &'a fastrender::FragmentNode,
    out: &mut Vec<(f32, f32)>,
    offset_x: f32,
    needle: &str,
  ) {
    let current_x = offset_x + fragment.bounds.x();
    if let FragmentContent::Text { text, .. } = &fragment.content {
      if text.contains(needle) {
        out.push((current_x, current_x + fragment.bounds.width()));
      }
    }
    for child in &fragment.children {
      collect(child, out, current_x, needle);
    }
  }

  let mut spans = Vec::new();
  collect(root, &mut spans, 0.0, needle);
  spans.iter().fold(None, |acc, (x1, x2)| match acc {
    Some((min, max)) => Some((min.min(*x1), max.max(*x2))),
    None => Some((*x1, *x2)),
  })
}

fn count_lines(fragment: &fastrender::FragmentNode) -> usize {
  let mut total = matches!(fragment.content, FragmentContent::Line { .. }) as usize;
  for child in &fragment.children {
    total += count_lines(child);
  }
  total
}

#[test]
fn ruby_annotations_position_above_base() {
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer
    .parse_html("<html><body><ruby>漢<rt>かん</rt>字<rt>じ</rt></ruby></body></html>")
    .expect("parse");

  let tree = renderer
    .layout_document(&dom, 200, 200)
    .expect("layout ruby");

  let base = baseline_for_text(&tree.root, "漢").or_else(|| baseline_for_text(&tree.root, "漢字"));
  let annotation = baseline_for_text(&tree.root, "かん");

  let (base_baseline, anno_baseline) = (base.expect("base text"), annotation.expect("annotation"));
  assert!(
    anno_baseline < base_baseline,
    "annotation should sit above base text"
  );
}

#[test]
fn ruby_rtc_places_annotations_on_both_sides() {
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer
    .parse_html("<ruby><rb>漢字</rb><rtc><rt>かんじ</rt></rtc><rtc><rt>kanji</rt></rtc></ruby>")
    .expect("parse");

  let tree = renderer
    .layout_document(&dom, 200, 200)
    .expect("layout ruby");

  let base = baseline_for_text(&tree.root, "漢字").expect("base baseline");
  let top = baseline_for_text(&tree.root, "かんじ").expect("top annotation");
  let bottom = baseline_for_text(&tree.root, "kanji").expect("bottom annotation");
  assert!(top < base, "top annotation should be above base");
  assert!(bottom > base, "bottom annotation should be below base");
}

#[test]
fn ruby_inter_character_mode_still_renders_annotations() {
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer
    .parse_html(
      "<ruby style=\"ruby-position: inter-character\">文<rt>ぶん</rt>字<rt>じ</rt></ruby>",
    )
    .expect("parse");

  let tree = renderer
    .layout_document(&dom, 200, 200)
    .expect("layout ruby");

  let base = baseline_for_text(&tree.root, "文").expect("base span");
  let annotation = baseline_for_text(&tree.root, "ぶん").expect("annotation span");

  let base_span = span_for_text_x(&tree.root, "文").expect("base bounds");
  let annotation_span = span_for_text_x(&tree.root, "ぶん").expect("annotation bounds");

  assert!(
    annotation < base,
    "annotation should be positioned against the base"
  );
  assert!(
    annotation_span.1 - annotation_span.0 >= base_span.1 - base_span.0 - 0.5,
    "inter-character ruby should distribute annotation across the base"
  );
}

#[test]
fn nested_ruby_annotations_render() {
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer
    .parse_html("<ruby>外<rt>そと</rt><ruby>内<rt>うち</rt></ruby></ruby>")
    .expect("parse");

  let tree = renderer
    .layout_document(&dom, 200, 200)
    .expect("layout ruby");

  assert!(span_for_text(&tree.root, "そと").is_some());
  assert!(span_for_text(&tree.root, "うち").is_some());
}

#[test]
fn ruby_participates_in_line_wrapping() {
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer
    .parse_html("<div>前の文 <ruby>漢字<rt>かんじ</rt></ruby> さらに続く文章で改行を確認する</div>")
    .expect("parse");

  let tree = renderer
    .layout_document(&dom, 120, 200)
    .expect("layout ruby");

  let lines = count_lines(&tree.root);
  assert!(lines > 1, "expected wrapped lines with ruby present");
}
