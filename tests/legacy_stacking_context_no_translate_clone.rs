use fastrender::paint::painter::Painter;
use fastrender::{ComputedStyle, Display, FragmentNode, FragmentTree, Rect, Rgba};
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

struct CountingAllocator;

static ALLOC_CALLS: AtomicUsize = AtomicUsize::new(0);

#[global_allocator]
static GLOBAL: CountingAllocator = CountingAllocator;

unsafe impl GlobalAlloc for CountingAllocator {
  unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
    let ptr = System.alloc(layout);
    if !ptr.is_null() {
      ALLOC_CALLS.fetch_add(1, Ordering::Relaxed);
    }
    ptr
  }

  unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
    let ptr = System.alloc_zeroed(layout);
    if !ptr.is_null() {
      ALLOC_CALLS.fetch_add(1, Ordering::Relaxed);
    }
    ptr
  }

  unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
    let new_ptr = System.realloc(ptr, layout, new_size);
    if !new_ptr.is_null() {
      ALLOC_CALLS.fetch_add(1, Ordering::Relaxed);
    }
    new_ptr
  }

  unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
    System.dealloc(ptr, layout);
  }
}

fn reset_alloc_calls() {
  ALLOC_CALLS.store(0, Ordering::Relaxed);
}

fn alloc_calls() -> usize {
  ALLOC_CALLS.load(Ordering::Relaxed)
}

fn build_nested_stacking_context_tree(depth: usize) -> FragmentTree {
  let mut style = ComputedStyle::default();
  style.display = Display::Block;
  style.opacity = 0.5;
  // Keep backgrounds/borders empty to avoid unrelated work; we want to exercise the stacking
  // context execution path and ensure it doesn't allocate quadratically due to command translation.
  style.background_color = Rgba::TRANSPARENT;
  let style = Arc::new(style);

  let mut current = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 1.0, 1.0),
    Vec::new(),
    style.clone(),
  );

  for level in 1..depth {
    // Offset only the outermost box so that all descendants share the same absolute bounds. This
    // forces each stacking context to establish a non-zero `origin_offset_css` while keeping the
    // content inside the small viewport.
    let bounds = if level + 1 == depth {
      Rect::from_xywh(10.0, 10.0, 1.0, 1.0)
    } else {
      Rect::from_xywh(0.0, 0.0, 1.0, 1.0)
    };
    current = FragmentNode::new_block_styled(bounds, vec![current], style.clone());
  }

  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 16.0, 16.0), vec![current]);
  FragmentTree::new(root)
}

fn paint_allocations_for_depth(depth: usize) -> usize {
  let tree = build_nested_stacking_context_tree(depth);
  let painter = Painter::new(16, 16, Rgba::WHITE).expect("painter");

  reset_alloc_calls();
  let _ = painter.paint(&tree).expect("paint should succeed");
  alloc_calls()
}

#[test]
fn legacy_stacking_context_paint_allocations_scale_linearly() {
  // Warm up one-time allocations so the scaling check is stable.
  let _ = paint_allocations_for_depth(1);

  // Keep nesting depth moderate to avoid stack overflows in the test harness while still
  // exercising the old quadratic `translate_commands` behavior (when present).
  let small_depth = 32;
  let large_depth = 64;
  let small = paint_allocations_for_depth(small_depth);
  let large = paint_allocations_for_depth(large_depth);

  assert!(
    large < small.saturating_mul(3),
    "expected paint allocations to scale ~linearly, got depth {small_depth} -> {small} allocs, depth {large_depth} -> {large} allocs"
  );
}
