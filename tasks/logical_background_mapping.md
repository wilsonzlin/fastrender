## Logical background-position/size mapping

- Current state: logical background properties (`background-position-inline/block`, `background-size-inline/block`) are ignored. In `writing-mode: sideways-lr`, authored logical offsets/sizes collapse to a single default layer (offsets 0%/0%, size auto/auto).
- Expected: map inline components to the vertical axis and block components to the horizontal axis (and vice-versa per writing-mode), emitting layers that reflect authored values.
- Suspected area: logical property resolution in `src/style/properties.rs` for backgrounds.
- Repro: set `writing-mode: sideways-lr; background-position-inline: 10px 20px; background-position-block: 3px 7px; background-size-inline: 30px 40px; background-size-block: 10px 20px;` and inspect computed background_positions/sizes—only defaults remain.
- Status: TODO (not implemented).
