use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::tree::fragment_tree::{
  enable_fragment_instrumentation, fragment_instrumentation_counters,
  reset_fragment_instrumentation_counters,
};
use fastrender::{FragmentNode, Rect};

struct CountingAllocator;

static ALLOC_CALLS: AtomicUsize = AtomicUsize::new(0);
static ALLOC_BYTES: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for CountingAllocator {
  unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
    ALLOC_CALLS.fetch_add(1, Ordering::Relaxed);
    ALLOC_BYTES.fetch_add(layout.size(), Ordering::Relaxed);
    System.alloc(layout)
  }

  unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
    ALLOC_CALLS.fetch_add(1, Ordering::Relaxed);
    ALLOC_BYTES.fetch_add(layout.size(), Ordering::Relaxed);
    System.alloc_zeroed(layout)
  }

  unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
    ALLOC_CALLS.fetch_add(1, Ordering::Relaxed);
    ALLOC_BYTES.fetch_add(new_size, Ordering::Relaxed);
    System.realloc(ptr, layout, new_size)
  }

  unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
    System.dealloc(ptr, layout)
  }
}

#[global_allocator]
static GLOBAL: CountingAllocator = CountingAllocator;

fn allocation_counts() -> (usize, usize) {
  (
    ALLOC_CALLS.load(Ordering::Relaxed),
    ALLOC_BYTES.load(Ordering::Relaxed),
  )
}

fn allocation_delta<R, F: FnOnce() -> R>(f: F) -> (usize, usize, R) {
  let (start_calls, start_bytes) = allocation_counts();
  let value = f();
  let (end_calls, end_bytes) = allocation_counts();
  (
    end_calls.saturating_sub(start_calls),
    end_bytes.saturating_sub(start_bytes),
    value,
  )
}

fn text_fragment(label: &str) -> FragmentNode {
  FragmentNode::new_text(
    Rect::from_xywh(0.0, 0.0, 4.0 * label.len() as f32, 12.0),
    label,
    10.0,
  )
}

fn build_fragment_subtree(depth: usize, breadth: usize, seed: usize) -> FragmentNode {
  if depth == 0 {
    let children: Vec<_> = (0..breadth)
      .map(|idx| text_fragment(&format!("leaf-{seed}-{idx}")))
      .collect();
    return FragmentNode::new_block(
      Rect::from_xywh(0.0, 0.0, (breadth as f32) * 16.0, 16.0),
      children,
    );
  }

  let children: Vec<_> = (0..breadth)
    .map(|idx| build_fragment_subtree(depth - 1, breadth, seed * 17 + idx))
    .collect();
  FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 200.0, 200.0), children)
}

fn assert_children_shared(a: &FragmentNode, b: &FragmentNode) {
  assert_eq!(a.children.len(), b.children.len());
  assert!(a.children.ptr_eq(&b.children));
  for (left, right) in a.children.iter().zip(b.children.iter()) {
    assert_children_shared(left, right);
  }
}

fn assert_children_unshared(a: &FragmentNode, b: &FragmentNode) {
  assert_eq!(a.children.len(), b.children.len());
  assert!(
    !a.children.ptr_eq(&b.children),
    "expected new child allocation for {:?}",
    a.bounds
  );
  for (left, right) in a.children.iter().zip(b.children.iter()) {
    assert_children_unshared(left, right);
  }
}

fn fragment_clone_benchmarks(c: &mut Criterion) {
  // Moderately deep tree with many text leaves to exercise Arc sharing.
  let fragment = build_fragment_subtree(4, 6, 1);

  {
    let _guard = enable_fragment_instrumentation(true);
    reset_fragment_instrumentation_counters();
    let _ = fragment.clone();
    let shallow_metrics = fragment_instrumentation_counters();
    assert_eq!(
      shallow_metrics.deep_clones, 0,
      "shallow clones should not trigger deep clone instrumentation"
    );
    let _ = fragment.deep_clone();
    let deep_metrics = fragment_instrumentation_counters();
    assert!(
      deep_metrics.deep_clones >= fragment.node_count(),
      "deep_clone should record traversal of the full subtree"
    );
  }
  reset_fragment_instrumentation_counters();

  // Sanity check: shallow clones reuse children, deep clones rebuild them.
  let (clone_allocs, clone_bytes, shallow) = allocation_delta(|| fragment.clone());
  assert_eq!(
    clone_allocs, 0,
    "structural sharing should keep clone() allocation-free"
  );
  assert_eq!(
    clone_bytes, 0,
    "Arc cloning should not request heap bytes for shallow clones"
  );
  assert_eq!(fragment.node_count(), shallow.node_count());
  assert_children_shared(&fragment, &shallow);
  assert!(fragment.children.strong_count() > 1);

  let (deep_allocs, deep_bytes, deep) = allocation_delta(|| fragment.deep_clone());
  assert!(
    deep_allocs > 0 && deep_bytes > 0,
    "deep_clone must allocate new child storage"
  );
  assert_eq!(fragment.node_count(), deep.node_count());
  assert_children_unshared(&fragment, &deep);

  let mut group = c.benchmark_group("fragment_clone");
  group.bench_function("clone_shared", |b| {
    b.iter(|| {
      // cloning should be O(1) thanks to structural sharing
      black_box(fragment.clone())
    })
  });
  group.bench_function("deep_clone", |b| {
    b.iter(|| black_box(fragment.deep_clone()))
  });
  group.finish();
}

criterion_group!(fragment_clone, fragment_clone_benchmarks);
criterion_main!(fragment_clone);
