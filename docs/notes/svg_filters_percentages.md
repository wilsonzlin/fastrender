# SVG filter percentage resolution

When `filterUnits` or `primitiveUnits` are `userSpaceOnUse`, percentage values on
`x/y/width/height` are resolved against the filtered element's bounding box
(width for `x`/`width`, height for `y`/`height`). In CSS `filter:url(...)`
contexts there is no SVG viewport to anchor user-space percentages, so the
element bbox is the only consistent base.

This rule is shared across filter regions and primitive subregions via
`SvgFilterRegion::resolve` and the `SvgFilter::resolve_primitive_*` helpers in
`svg_filter.rs` to avoid diverging behavior. Light source coordinates for
lighting primitives are parsed as lengths and resolved through the same paths
(`resolve_light_point` -> `resolve_primitive_pos_*`) so percentage inputs
continue to track the filtered element's bounding box even when `primitiveUnits`
is `userSpaceOnUse`. Paired values that come from a single input (e.g.
`stdDeviation="2"` or `kernelUnitLength="1"`) are still resolved per-axis so
`objectBoundingBox` units respect the bbox width for X and height for Y rather
than averaging the two dimensions.
