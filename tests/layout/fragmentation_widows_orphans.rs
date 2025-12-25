use std::sync::Arc;

use fastrender::layout::fragmentation::{fragment_tree, FragmentationOptions};
use fastrender::style::types::BreakInside;
use fastrender::{ComputedStyle, FragmentContent, FragmentNode, Rect};

fn labeled_line(y: f32, label: &str) -> FragmentNode {
  let text = FragmentNode::new_text(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), label.to_string(), 8.0);
  FragmentNode::new_line(Rect::from_xywh(0.0, y, 100.0, 15.0), 12.0, vec![text])
}

#[test]
fn widows_and_orphans_respected_across_multiple_fragmentainers() {
  let mut style = ComputedStyle::default();
  style.break_inside = BreakInside::Auto;
  style.widows = 2;
  style.orphans = 2;
  let lines: Vec<_> = (0..6)
    .map(|idx| labeled_line(idx as f32 * 15.0, &format!("line-{idx}")))
    .collect();
  let paragraph = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 120.0, 90.0),
    lines,
    Arc::new(style),
  );

  let fragments = fragment_tree(&paragraph, &FragmentationOptions::new(30.0));

  assert_eq!(fragments.len(), 3);
  let mut seen = Vec::new();
  for fragment in &fragments {
    let lines: Vec<_> = fragment
      .children
      .iter()
      .filter(|child| matches!(child.content, FragmentContent::Line { .. }))
      .collect();
    assert_eq!(lines.len(), 2, "each fragment should hold two lines");
    assert!(lines
      .iter()
      .all(|line| (line.bounds.height() - 15.0).abs() < 0.1));
    for line in lines {
      let label = line
        .children
        .iter()
        .find_map(|child| match &child.content {
          FragmentContent::Text { text, .. } => Some(text.clone()),
          _ => None,
        })
        .expect("line label");
      seen.push(label);
    }
  }

  assert_eq!(
    seen,
    vec![
      "line-0".to_string(),
      "line-1".to_string(),
      "line-2".to_string(),
      "line-3".to_string(),
      "line-4".to_string(),
      "line-5".to_string()
    ]
  );
}

#[test]
fn widows_and_orphans_are_soft_constraints() {
  let mut style = ComputedStyle::default();
  style.break_inside = BreakInside::Auto;
  style.orphans = 3;
  style.widows = 2;
  let lines: Vec<_> = (0..4)
    .map(|idx| labeled_line(idx as f32 * 10.0, &format!("line-{idx}")))
    .collect();
  let paragraph = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 120.0, 40.0),
    lines,
    Arc::new(style),
  );

  let fragments = fragment_tree(&paragraph, &FragmentationOptions::new(20.0));

  assert_eq!(
    fragments.len(),
    2,
    "widows/orphans should not prevent fragmentation progress"
  );
  let first_count = fragments[0]
    .children
    .iter()
    .filter(|child| matches!(child.content, FragmentContent::Line { .. }))
    .count();
  let second_count = fragments[1]
    .children
    .iter()
    .filter(|child| matches!(child.content, FragmentContent::Line { .. }))
    .count();
  assert_eq!(first_count + second_count, 4);
  assert_eq!(
    first_count, 2,
    "when constraints cannot be satisfied, fallback should still break at the fragment limit"
  );
  assert!(
    fragments[0].bounds.height() <= 20.1,
    "fragmentation should not defer everything into one oversize fragment"
  );
}
