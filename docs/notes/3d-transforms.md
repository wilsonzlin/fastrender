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
  surface normal.
- Elements with transforms or perspective establish containing blocks for
  positioned descendants (including `position: fixed`).
