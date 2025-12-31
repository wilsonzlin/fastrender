//! DOM clone + top-layer traversal benchmark
//!
//! This benchmark targets a `dom_parse`-adjacent hot path in the render pipeline:
//! cloning the parsed `DomNode` tree and then traversing it to compute/apply top-layer state
//! (`<dialog>` + popover, plus modal inert propagation).
//!
//! Running:
//! ```bash
//! cargo bench --bench dom_clone_bench -- --noplot
//! ```
//!
//! Notes:
//! - The DOM tree is generated programmatically (no committed real-site HTML).
//! - Benchmarks are split to keep DOM clone vs traversal costs separate.

use std::cell::RefCell;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::dom::{self, DomNode, DomNodeType};
use selectors::context::QuirksMode;

mod common;

fn text(content: impl Into<String>) -> DomNode {
  DomNode {
    node_type: DomNodeType::Text {
      content: content.into(),
    },
    children: Vec::new(),
  }
}

fn element(tag_name: &str, attributes: Vec<(String, String)>, children: Vec<DomNode>) -> DomNode {
  DomNode {
    node_type: DomNodeType::Element {
      tag_name: tag_name.to_string(),
      namespace: String::new(),
      attributes,
    },
    children,
  }
}

fn card(idx: usize) -> DomNode {
  let mut root_attrs = vec![
    (
      "class".to_string(),
      format!("card item-{} theme-{}", idx % 24, idx % 3),
    ),
    ("data-id".to_string(), idx.to_string()),
    ("data-kind".to_string(), "card".to_string()),
  ];
  if idx % 128 == 0 {
    root_attrs.push(("id".to_string(), format!("card-{idx}")));
  }

  let header = element(
    "div",
    vec![("class".to_string(), "card-header".to_string())],
    vec![
      element("h2", Vec::new(), vec![text(format!("Item {idx}"))]),
      button_with_optional_popover(idx),
    ],
  );

  let content = element(
    "div",
    vec![("class".to_string(), "card-content".to_string())],
    vec![
      element(
        "p",
        Vec::new(),
        vec![text(
          "FastRender synthetic DOM benchmark paragraph.".to_string(),
        )],
      ),
      element(
        "div",
        vec![("class".to_string(), "media".to_string())],
        vec![
          element(
            "img",
            vec![
              ("src".to_string(), format!("image-{idx}.png")),
              ("alt".to_string(), "thumbnail".to_string()),
            ],
            Vec::new(),
          ),
          element(
            "span",
            vec![("class".to_string(), "caption".to_string())],
            vec![text("caption".to_string())],
          ),
        ],
      ),
    ],
  );

  let list = element(
    "ul",
    vec![("class".to_string(), "items".to_string())],
    (0..3)
      .map(|j| element("li", Vec::new(), vec![text(format!("row {j}"))]))
      .collect(),
  );

  element("div", root_attrs, vec![header, content, list])
}

fn button_with_optional_popover(idx: usize) -> DomNode {
  let mut attrs = vec![
    ("class".to_string(), "card-action".to_string()),
    ("type".to_string(), "button".to_string()),
    ("data-action".to_string(), format!("action-{}", idx % 8)),
  ];

  // Sprinkle popovers deterministically so the traversal hits both the popover and non-popover
  // paths without turning the benchmark into a popover-only stress test.
  if idx % 64 == 0 {
    attrs.push(("popover".to_string(), "auto".to_string()));
    if idx % 256 == 0 {
      // No `open` attribute, but force open to exercise `set_attr`.
      attrs.push(("data-fastr-open".to_string(), "true".to_string()));
    } else if idx % 256 == 128 {
      // `open` present, but force closed to exercise `remove_attr`.
      attrs.push(("open".to_string(), String::new()));
      attrs.push(("data-fastr-open".to_string(), "false".to_string()));
    }
  }

  element("button", attrs, vec![text("…".to_string())])
}

fn top_layer_dialog_nodes() -> Vec<DomNode> {
  vec![
    // Open, non-modal dialog (should remain open).
    element(
      "dialog",
      vec![("open".to_string(), String::new())],
      vec![element(
        "p",
        Vec::new(),
        vec![text("non-modal".to_string())],
      )],
    ),
    // Dialog with open attr, but forced closed (exercises open removal).
    element(
      "dialog",
      vec![
        ("open".to_string(), String::new()),
        ("data-fastr-open".to_string(), "false".to_string()),
      ],
      Vec::new(),
    ),
  ]
}

fn modal_dialog_tail() -> DomNode {
  // Place the modal dialog at the end of the tree so any modal detection must traverse a large
  // portion of the DOM in the worst case.
  element(
    "dialog",
    vec![("data-fastr-open".to_string(), "modal".to_string())],
    vec![element(
      "div",
      vec![("class".to_string(), "modal-body".to_string())],
      vec![text("modal contents".to_string())],
    )],
  )
}

fn build_dom(card_count: usize) -> DomNode {
  let body_children: Vec<DomNode> = (0..card_count)
    .map(card)
    .chain(top_layer_dialog_nodes())
    .chain(std::iter::once(modal_dialog_tail()))
    .collect();

  let html = element(
    "html",
    Vec::new(),
    vec![
      element("head", Vec::new(), Vec::new()),
      element("body", Vec::new(), body_children),
    ],
  );

  DomNode {
    node_type: DomNodeType::Document {
      quirks_mode: QuirksMode::NoQuirks,
    },
    children: vec![html],
  }
}

fn dom_clone_benchmarks(c: &mut Criterion) {
  // 4k cards × 13 elements/card ~= 52k element nodes, plus dialogs/popovers/text nodes.
  let dom = build_dom(4_000);

  let mut group = c.benchmark_group("dom_clone_hot_path");

  group.bench_function("dom_clone", |b| b.iter(|| black_box(dom.clone())));

  // Run the traversal benchmark on a single, already-initialized DOM to keep clone and traversal
  // costs separate.
  let mut dom_for_traversal = dom.clone();
  let _modal_open = dom::apply_top_layer_state_auto(&mut dom_for_traversal);
  let traversal_dom = RefCell::new(dom_for_traversal);

  group.bench_function("top_layer_traversal", |b| {
    b.iter(|| {
      let mut node = traversal_dom.borrow_mut();
      let modal_open = dom::apply_top_layer_state_auto(black_box(&mut *node));
      black_box(modal_open);
    })
  });

  group.finish();
}

criterion_group!(
  name = dom_clone_benches;
  config = common::perf_criterion();
  targets = dom_clone_benchmarks
);
criterion_main!(dom_clone_benches);
