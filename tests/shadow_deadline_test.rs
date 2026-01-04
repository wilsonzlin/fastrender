use std::time::Duration;

use fastrender::dom::{
  compute_part_export_map_with_ids, compute_slot_assignment_with_ids, enumerate_dom_ids, DomNode,
  DomNodeType, ShadowRootMode,
};
use fastrender::error::{RenderError, RenderStage};
use fastrender::render_control::{with_deadline, RenderDeadline};
use fastrender::Error;

fn build_deep_shadow_host(depth: usize) -> DomNode {
  let mut child = DomNode {
    node_type: DomNodeType::Slot {
      namespace: String::new(),
      attributes: Vec::new(),
      assigned: false,
    },
    children: Vec::new(),
  };

  for _ in 0..depth {
    child = DomNode {
      node_type: DomNodeType::Element {
        tag_name: "div".to_string(),
        namespace: String::new(),
        attributes: Vec::new(),
      },
      children: vec![child],
    };
  }

  let shadow_root = DomNode {
    node_type: DomNodeType::ShadowRoot {
      mode: ShadowRootMode::Open,
      delegates_focus: false,
    },
    children: vec![child],
  };

  DomNode {
    node_type: DomNodeType::Element {
      tag_name: "div".to_string(),
      namespace: String::new(),
      attributes: Vec::new(),
    },
    children: vec![shadow_root],
  }
}

#[test]
fn shadow_slot_assignment_respects_deadline() {
  let dom = build_deep_shadow_host(4096);
  let ids = enumerate_dom_ids(&dom);
  let deadline = RenderDeadline::new(Some(Duration::from_millis(0)), None);

  let result = with_deadline(Some(&deadline), || {
    compute_slot_assignment_with_ids(&dom, &ids)
  });

  assert!(
    matches!(
      result,
      Err(Error::Render(RenderError::Timeout {
        stage: RenderStage::Cascade,
        ..
      }))
    ),
    "expected cascade timeout, got {result:?}"
  );
}

#[test]
fn shadow_part_export_map_respects_deadline() {
  let dom = build_deep_shadow_host(4096);
  let ids = enumerate_dom_ids(&dom);
  let deadline = RenderDeadline::new(Some(Duration::from_millis(0)), None);

  let result = with_deadline(Some(&deadline), || {
    compute_part_export_map_with_ids(&dom, &ids)
  });

  assert!(
    matches!(
      result,
      Err(Error::Render(RenderError::Timeout {
        stage: RenderStage::Cascade,
        ..
      }))
    ),
    "expected cascade timeout, got {result:?}"
  );
}
