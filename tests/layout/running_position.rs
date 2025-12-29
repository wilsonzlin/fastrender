use std::sync::Arc;

use fastrender::geometry::Size;
use fastrender::layout::constraints::{AvailableSpace, LayoutConstraints};
use fastrender::layout::contexts::factory::FormattingContextFactory;
use fastrender::layout::contexts::positioned::ContainingBlock;
use fastrender::style::display::Display;
use fastrender::style::display::FormattingContextType;
use fastrender::text::font_loader::FontContext;
use fastrender::tree::box_tree::BoxNode;
use fastrender::tree::fragment_tree::FragmentContent;
use fastrender::ComputedStyle;

#[test]
fn running_position_creates_anchor_snapshot() {
  let mut running_style = ComputedStyle::default();
  running_style.display = Display::Block;
  running_style.running_position = Some("banner".to_string());
  let running_text = BoxNode::new_text(Arc::new(ComputedStyle::default()), "HEADER".into());
  let running = BoxNode::new_block(
    Arc::new(running_style),
    FormattingContextType::Block,
    vec![running_text],
  );

  let mut body_style = ComputedStyle::default();
  body_style.display = Display::Block;
  let body_text = BoxNode::new_text(Arc::new(ComputedStyle::default()), "Body".into());
  let body = BoxNode::new_block(
    Arc::new(body_style),
    FormattingContextType::Block,
    vec![body_text],
  );

  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;
  let root = BoxNode::new_block(
    Arc::new(root_style),
    FormattingContextType::Block,
    vec![running, body],
  );

  let viewport = Size::new(200.0, 200.0);
  let factory = FormattingContextFactory::with_font_context_viewport_and_cb(
    FontContext::new(),
    viewport,
    ContainingBlock::viewport(viewport),
  );
  let fc = factory.create(FormattingContextType::Block);
  let constraints =
    LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite);
  let fragment = fc.layout(&root, &constraints).expect("layout");

  let mut flow_texts = Vec::new();
  for frag in fragment.iter_fragments() {
    if let FragmentContent::Text { text, .. } = &frag.content {
      flow_texts.push(text.to_string());
    }
  }

  assert_eq!(flow_texts, vec!["Body".to_string()]);

  let anchor = fragment
    .iter_fragments()
    .find(|frag| matches!(frag.content, FragmentContent::RunningAnchor { .. }))
    .expect("running anchor fragment");

  if let FragmentContent::RunningAnchor { name, snapshot } = &anchor.content {
    assert_eq!(name.as_ref(), "banner");
    let mut snapshot_texts = Vec::new();
    for frag in snapshot.iter_fragments() {
      if let FragmentContent::Text { text, .. } = &frag.content {
        snapshot_texts.push(text.to_string());
      }
    }
    assert!(
      snapshot_texts.contains(&"HEADER".to_string()),
      "snapshot should contain running content"
    );
  } else {
    panic!("expected running anchor");
  }
}

#[test]
fn inline_running_position_does_not_flow_text() {
  let mut running_style = ComputedStyle::default();
  running_style.display = Display::Inline;
  running_style.running_position = Some("note".to_string());
  let running_text = BoxNode::new_text(Arc::new(ComputedStyle::default()), "INLINE".into());
  let mut running_inline = BoxNode::new_inline(Arc::new(running_style), vec![running_text]);
  running_inline.id = 10;
  assert_eq!(
    running_inline
      .style
      .running_position
      .as_deref()
      .unwrap_or_default(),
    "note"
  );

  let before = BoxNode::new_text(Arc::new(ComputedStyle::default()), "Before ".into());
  let after = BoxNode::new_text(Arc::new(ComputedStyle::default()), "After".into());
  let mut inline_parent = BoxNode::new_inline(
    Arc::new(ComputedStyle::default()),
    vec![before, running_inline, after],
  );
  inline_parent.id = 5;

  let mut body_style = ComputedStyle::default();
  body_style.display = Display::Block;
  let body = BoxNode::new_block(
    Arc::new(body_style),
    FormattingContextType::Block,
    vec![inline_parent],
  );

  let mut root_style = ComputedStyle::default();
  root_style.display = Display::Block;
  let root = BoxNode::new_block(
    Arc::new(root_style),
    FormattingContextType::Block,
    vec![body],
  );

  let viewport = Size::new(200.0, 200.0);
  let factory = FormattingContextFactory::with_font_context_viewport_and_cb(
    FontContext::new(),
    viewport,
    ContainingBlock::viewport(viewport),
  );
  let fc = factory.create(FormattingContextType::Block);
  let constraints =
    LayoutConstraints::new(AvailableSpace::Definite(200.0), AvailableSpace::Indefinite);
  let fragment = fc.layout(&root, &constraints).expect("layout");

  let anchor_count = fragment
    .iter_fragments()
    .filter(|frag| matches!(frag.content, FragmentContent::RunningAnchor { .. }))
    .count();
  assert_eq!(
    anchor_count, 1,
    "expected one running anchor in the fragment tree"
  );
  let flow_texts = fragment
    .iter_fragments()
    .filter_map(|frag| {
      if let FragmentContent::Text { text, .. } = &frag.content {
        Some(text.as_ref())
      } else {
        None
      }
    })
    .collect::<Vec<_>>()
    .join("");

  assert!(flow_texts.contains("Before"));
  assert!(flow_texts.contains("After"));
  assert!(
    !flow_texts.contains("INLINE"),
    "running content should not appear in normal flow text"
  );

  let anchor = fragment
    .iter_fragments()
    .find(|frag| matches!(frag.content, FragmentContent::RunningAnchor { .. }))
    .expect("running anchor fragment");

  if let FragmentContent::RunningAnchor { name, snapshot } = &anchor.content {
    assert_eq!(name.as_ref(), "note");
    let mut snapshot_texts = Vec::new();
    for frag in snapshot.iter_fragments() {
      if let FragmentContent::Text { text, .. } = &frag.content {
        snapshot_texts.push(text.to_string());
      }
    }
    assert!(
      snapshot_texts.iter().any(|t| t.contains("INLINE")),
      "snapshot should contain running inline content"
    );
  } else {
    panic!("expected running anchor");
  }
}
