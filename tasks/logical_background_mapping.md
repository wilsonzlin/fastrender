## Logical background-position/size mapping (TODO)

- Currently `background-position-inline/block` and `background-size-inline/block` are ignored. In `writing-mode: sideways-lr`, authored logical positions and sizes leave a single default layer (offsets 0%/0%, size auto/auto).
- Expected: map inline components to the vertical axis and block components to the horizontal axis (similarly for sideways-rl and vertical/horizontal modes) when resolving logical backgrounds.
- Suspected area: logical property resolution in `src/style/properties.rs` (logical mapping for backgrounds missing).
- Repro: set `writing-mode: sideways-lr; background-position-inline: 10px 20px; background-position-block: 3px 7px; background-size-inline: 30px 40px; background-size-block: 10px 20px;` and inspect computed background_positions/background_sizes – remains at defaults.
- References: findings/queue note about logical background properties being ignored.
