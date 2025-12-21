use fastrender::style::display::Display;
use fastrender::tree::box_tree::BoxNode;
use fastrender::ComputedStyle;
use fastrender::FormattingContextFactory;
use fastrender::FormattingContextType;
use fastrender::LayoutConstraints;
use std::sync::Arc;

fn default_style() -> Arc<ComputedStyle> {
  Arc::new(ComputedStyle::default())
}

#[test]
fn legend_shrinks_to_content_width() {
  // Legend with auto width should shrink to its content, not fill the fieldset width.
  let mut legend_style = ComputedStyle::default();
  legend_style.display = Display::Block;
  legend_style.width = None;
  legend_style.shrink_to_fit_inline_size = true;
  let legend = BoxNode::new_block(Arc::new(legend_style), FormattingContextType::Block, vec![
    BoxNode::new_text(default_style(), "Legend".into()),
  ]);

  let mut fieldset_style = ComputedStyle::default();
  fieldset_style.display = Display::Block;
  let root = BoxNode::new_block(
    Arc::new(fieldset_style),
    FormattingContextType::Block,
    vec![legend],
  );

  let factory = FormattingContextFactory::new();
  let ctx = factory.create(FormattingContextType::Block);
  let constraints = LayoutConstraints::definite(200.0, 200.0);
  let fragment = ctx.layout(&root, &constraints).expect("layout");

  // The legend text is 6 characters at default 16px font; width should be close to text width, not 200px.
  let legend_frag = &fragment.children[0];
  assert!(
    legend_frag.bounds.width() < 100.0,
    "legend width {:?}",
    legend_frag.bounds.width()
  );
}
