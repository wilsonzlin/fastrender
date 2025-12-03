# Vendor Patches

This document tracks modifications made to vendored dependencies.

## Taffy (v0.9.1)

**Location:** `vendor/taffy/`

### Why Vendored

Taffy is vendored rather than used as a Cargo dependency because we needed to modify the grid auto-placement algorithm to properly respect source order when placing items.

### Modifications

#### 1. Grid Auto-Placement Source Order Fix

**File:** `vendor/taffy/src/compute/grid/placement.rs`

**Change:** Modified the `place_grid_items` function to use single-pass placement that respects source order.

**Reason:** The original Taffy implementation had a multi-pass approach that could place items out of source order in certain edge cases. Our modification collects all children first and places them in a single pass while respecting the DOM source order.

**Marker:** Look for `// MODIFIED: Single-pass placement respecting source order` (line ~47)

#### 2. Removed Unnecessary Clone Calls

**File:** `vendor/taffy/src/compute/grid/placement.rs`

**Change:** Removed `.clone()` calls on references to generic type `S` that doesn't implement `Clone`.

**Reason:** Clippy warning - calling `.clone()` on a reference where the type doesn't implement Clone just copies the reference, which is a no-op.

**Lines:** ~77, ~134, ~168 (removed `.clone()` from `style.clone()` → `style`)

### Maintenance Notes

#### Checking for Upstream Fixes

Before updating Taffy, check if the upstream has addressed:
1. Grid auto-placement source order issues
2. Any related bugs in the grid placement algorithm

#### Re-applying Patches After Update

If updating Taffy:
1. Back up the current `vendor/taffy/` directory
2. Replace with new version
3. Search for `// MODIFIED:` comments in the old version
4. Manually re-apply modifications or check if they're still needed
5. Run the full test suite
6. Update this document with any changes

#### Testing Patches

After any modification to vendored code:
```bash
cargo test
cargo clippy --all-targets --all-features
```

### Version History

| Date | Taffy Version | Patches Applied |
|------|--------------|-----------------|
| Initial | 0.9.1 | Grid auto-placement source order fix |
