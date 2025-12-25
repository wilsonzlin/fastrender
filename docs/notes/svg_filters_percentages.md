# SVG filter percentage resolution

When `filterUnits` or `primitiveUnits` are `userSpaceOnUse`, percentage values on
`x/y/width/height` are resolved against the filtered element's bounding box
(width for `x`/`width`, height for `y`/`height`). In CSS `filter:url(...)`
contexts there is no SVG viewport to anchor user-space percentages, so the
element bbox is the only consistent base.

This rule is shared across filter regions and primitive subregions via
`resolve_user_space_percentage` in `svg_filter.rs` to avoid diverging behavior.
