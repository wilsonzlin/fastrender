use crate::tree::box_tree::BoxNode;
use std::hash::Hash;

/// Lightweight, hashable reference to a `BoxNode`.
///
/// Taffy allows associating arbitrary context with nodes. We store raw pointers
/// to box nodes so we can recover the DOM node for layout conversion without
/// cloning it into the tree. The pointers are only used during layout and rely
/// on the box tree outliving the formatting context invocation.
#[derive(Copy, Clone)]
pub struct BoxNodeRef(*const BoxNode);

impl BoxNodeRef {
  pub fn new(node: &BoxNode) -> Self {
    Self(node as *const BoxNode)
  }

  /// # Safety
  ///
  /// The caller must ensure the referenced `BoxNode` is still alive. Layout
  /// holds on to box trees for the duration of a layout pass, so this is safe
  /// within a single layout invocation.
  pub unsafe fn get(&self) -> &BoxNode {
    &*self.0
  }

  /// Returns the raw pointer backing this reference for hashing/diagnostics.
  pub fn as_ptr(self) -> *const BoxNode {
    self.0
  }
}

impl std::fmt::Debug for BoxNodeRef {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_tuple("BoxNodeRef")
      .field(&(self.0 as usize as u64))
      .finish()
  }
}

impl PartialEq for BoxNodeRef {
  fn eq(&self, other: &Self) -> bool {
    self.0 == other.0
  }
}

impl Eq for BoxNodeRef {}

impl Hash for BoxNodeRef {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    (self.0 as usize).hash(state);
  }
}

// Layout fan-out runs formatting contexts across rayon workers, so allow these
// raw references to cross threads. Safety is upheld by the immutable box tree
// lifetime during a single layout invocation.
unsafe impl Send for BoxNodeRef {}
unsafe impl Sync for BoxNodeRef {}
