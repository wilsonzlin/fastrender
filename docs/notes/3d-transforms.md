# 3D transforms and perspective

FastRender resolves CSS 3D transforms into column-major 4×4 matrices. Stacking
contexts and transform display items now carry the 3D matrix; the renderer
projects it to a 2D affine approximation for tiny-skia while keeping the full
matrix for backface checks and bounds inflation.

Key points:

- The `perspective` property (with `perspective-origin`) is parsed and included
  when building the element's transform matrix.
- `matrix3d()` is preserved verbatim; 2D-compatible matrices still round-trip
  through `to_2d()` for affine consumers.
- `backface-visibility: hidden` culls display list items using the transformed
  surface normal, evaluated in the renderer with the accumulated transform
  stack.
- Elements with transforms or perspective establish containing blocks for
  positioned descendants (including `position: fixed`).

## Preserve-3D fallback

The display-list renderer has a runtime preserve-3d path:

- When it encounters a `transform-style: preserve-3d` stacking context, it
  builds a small "scene" from the subtree.
- The scene is split into planes (flattened item segments + non-preserve-3d
  subtrees), each carrying an accumulated 3D transform.
- Planes are culled when `backface-visibility: hidden` and the surface normal
  faces away from the camera.
- Remaining planes are depth-sorted back-to-front and then rasterized into
  temporary pixmaps which are composited onto the destination:
  - If the transform is affine (or projective warping is disabled), the pixmap
    is drawn using a 2D affine approximation.
  - Otherwise a homography is computed from the source quad to the projected
    destination quad and the pixmap is projectively warped.
  - If the projective warp is unstable (degenerate `w`, non-finite math, or
    otherwise invalid), the renderer falls back to the affine approximation.

Projective warping is enabled by default (via the `preserve3d_warp` feature). It
can be disabled at runtime with `FASTR_PRESERVE3D_DISABLE_WARP=1`, which forces
all projective warps to use the affine approximation instead. If you build
without default features, set `FASTR_PRESERVE3D_WARP=1` to opt back into the
projective warp path.

Set `FASTR_PRESERVE3D_DEBUG=1` to log preserve-3d scene sorting + warp fallback
decisions while debugging real pages.
