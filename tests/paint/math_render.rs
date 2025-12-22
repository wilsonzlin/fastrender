use fastrender::math::{layout_mathml, MathNode};
use fastrender::paint::display_list::DisplayItem;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::text::font_loader::FontContext;
use fastrender::tree::box_tree::ReplacedType;
use fastrender::{FastRender, FragmentContent, FragmentNode};
use std::thread;

fn find_math_fragment<'a>(fragment: &'a FragmentNode) -> Option<&'a ReplacedType> {
  match &fragment.content {
    FragmentContent::Replaced { replaced_type, .. } => {
      if matches!(replaced_type, ReplacedType::Math(_)) {
        Some(replaced_type)
      } else {
        None
      }
    }
    _ => fragment.children.iter().find_map(find_math_fragment),
  }
}

fn with_stack<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> T {
  thread::Builder::new()
    .stack_size(8 * 1024 * 1024)
    .spawn(f)
    .expect("spawn math thread")
    .join()
    .expect("join math thread")
}

#[test]
fn fraction_mathml_layouts_and_paints() {
  with_stack(|| {
    let mut renderer = FastRender::new().expect("renderer");
    let dom = renderer
      .parse_html("<math><mfrac><mi>a</mi><mi>b</mi></mfrac></math>")
      .expect("dom");
    let fragments = renderer
      .layout_document(&dom, 200, 200)
      .expect("layout document");

    let replaced = find_math_fragment(&fragments.root).expect("math fragment");
    let ReplacedType::Math(math) = replaced else {
      panic!("expected math replaced type");
    };
    let layout = math.layout.as_ref().expect("math layout");
    assert!(layout.width > 0.0 && layout.height > 0.0);
    assert!(layout.baseline > 0.0 && layout.baseline < layout.height);

    let builder = DisplayListBuilder::new();
    let list = builder.build_tree(&fragments);
    assert!(
      list
        .items()
        .iter()
        .any(|item| matches!(item, DisplayItem::FillRect(_))),
      "fraction bar should emit a rule",
    );
    assert!(
      list
        .items()
        .iter()
        .any(|item| matches!(item, DisplayItem::Text(_))),
      "math text should emit glyphs",
    );
  });
}

#[test]
fn sqrt_and_scripts_produce_nonzero_layout() {
  with_stack(|| {
    let mut renderer = FastRender::new().expect("renderer");
    let dom = renderer
      .parse_html("<math><msqrt><msubsup><mi>x</mi><mi>i</mi><mi>2</mi></msubsup></msqrt></math>")
      .expect("dom");
    let fragments = renderer
      .layout_document(&dom, 300, 200)
      .expect("layout document");

    let replaced = find_math_fragment(&fragments.root).expect("math fragment");
    let ReplacedType::Math(math) = replaced else {
      panic!("expected math replaced type");
    };
    let layout = math.layout.as_ref().expect("math layout");
    assert!(layout.height > 0.0, "height should be positive");
    assert!(layout.width > 0.0);
  });
}

#[test]
fn math_layout_falls_back_without_fonts() {
  let style = fastrender::ComputedStyle::default();
  let node = MathNode::Fraction {
    numerator: Box::new(MathNode::Identifier {
      text: "x".into(),
      italic: true,
      bold: false,
    }),
    denominator: Box::new(MathNode::Number {
      text: "2".into(),
      italic: false,
      bold: false,
    }),
  };
  let layout = layout_mathml(&node, &style, &FontContext::empty());
  assert!(layout.width > 0.0);
  assert!(layout.height > 0.0);
  assert!(layout.baseline > 0.0 && layout.baseline < layout.height);
}

#[test]
fn matrix_table_aligns_cells() {
  with_stack(|| {
    let mut renderer = FastRender::new().expect("renderer");
    let dom = renderer
      .parse_html(
        "<math><mtable><mtr><mtd><mi>a</mi></mtd><mtd><mi>b</mi></mtd></mtr><mtr><mtd><mi>c</mi></mtd><mtd><mi>d</mi></mtd></mtr></mtable></math>",
      )
      .expect("dom");
    let fragments = renderer
      .layout_document(&dom, 400, 200)
      .expect("layout document");

    let replaced = find_math_fragment(&fragments.root).expect("math fragment");
    let ReplacedType::Math(math) = replaced else {
      panic!("expected math replaced type");
    };
    let layout = math.layout.as_ref().expect("math layout");
    assert!(layout.width > 0.0 && layout.height > 0.0);
    assert!(
      layout.fragments.len() >= 4,
      "matrix should have at least four glyph fragments"
    );
  });
}

#[test]
fn munderover_stacks_limits() {
  with_stack(|| {
    let mut renderer = FastRender::new().expect("renderer");
    let base_dom = renderer
      .parse_html("<math><mo>&#8721;</mo></math>")
      .expect("base dom");
    let base_layout = {
      let base_fragments = renderer
        .layout_document(&base_dom, 200, 200)
        .expect("layout base");
      let replaced = find_math_fragment(&base_fragments.root).expect("math fragment");
      let ReplacedType::Math(math) = replaced else {
        panic!("expected math replaced type");
      };
      math.layout.as_ref().expect("layout").as_ref().clone()
    };

    let dom = renderer
      .parse_html("<math><munderover><mo>&#8721;</mo><mi>i</mi><mi>n</mi></munderover></math>")
      .expect("dom");
    let fragments = renderer
      .layout_document(&dom, 240, 240)
      .expect("layout document");

    let replaced = find_math_fragment(&fragments.root).expect("math fragment");
    let ReplacedType::Math(math) = replaced else {
      panic!("expected math replaced type");
    };
    let layout = math.layout.as_ref().expect("math layout");
    assert!(layout.height > base_layout.height);
    assert!(layout.baseline > base_layout.baseline);
    assert!(layout.fragments.len() >= 3);
  });
}
