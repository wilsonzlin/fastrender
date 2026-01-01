use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::tree::table_fixup::TableStructureFixer;
use fastrender::{BoxNode, ComputedStyle, FormattingContextType};
use std::sync::Arc;

fn build_block_tree(depth: usize, branching: usize, style: &Arc<ComputedStyle>) -> BoxNode {
  if depth == 0 {
    return BoxNode::new_block(style.clone(), FormattingContextType::Block, Vec::new());
  }

  let mut children = Vec::with_capacity(branching);
  for _ in 0..branching {
    children.push(build_block_tree(depth - 1, branching, style));
  }
  BoxNode::new_block(style.clone(), FormattingContextType::Block, children)
}

fn bench_table_fixup_traversal(c: &mut Criterion) {
  let mut group = c.benchmark_group("table_fixup_traversal");

  // Large tree with no table formatting contexts. This isolates the traversal overhead of
  // `TableStructureFixer::fixup_tree_internals_with_deadline` on pages that contain few or no
  // tables.
  let style = Arc::new(ComputedStyle::default());
  let non_table_tree = build_block_tree(7, 5, &style); // ~97k nodes.

  group.bench_function("non_table_97k_nodes", |b| {
    let mut tree = Some(non_table_tree.clone());
    let mut deadline_counter = 0usize;
    b.iter(|| {
      let input = tree.take().expect("tree consumed");
      let fixed = TableStructureFixer::fixup_tree_internals_with_deadline(
        black_box(input),
        &mut deadline_counter,
      )
      .expect("table fixup traversal succeeds");
      tree = Some(fixed);
    });
  });

  group.finish();
}

fn perf_criterion() -> Criterion {
  Criterion::default().configure_from_args()
}

criterion_group!(
  name = benches;
  config = perf_criterion();
  targets = bench_table_fixup_traversal
);
criterion_main!(benches);
