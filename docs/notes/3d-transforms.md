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

Projective warping is enabled by default (via the `preserve3d_warp` feature) and
can be disabled with `FASTR_PRESERVE3D_DISABLE_WARP=1`. When the warp path is
unavailable or unstable, the compositor falls back deterministically:

- Stable perspective transforms are warped; otherwise they are approximated to
  2D.
- Depth sorting still runs for `transform-style: preserve-3d` when all child
  depths are stable; otherwise it falls back to the authored paint order
  (equivalent to `transform-style: flat`).
- Degenerate projections (non-finite, `w≈0`, or behind the camera) force flat
  ordering but still honour backface-visibility where possible.

Set `FASTR_PRESERVE3D_DEBUG=1` to log the chosen path per item while debugging
real pages. If you build without default features, set `FASTR_PRESERVE3D_WARP=1`
to opt back into projective warping.
