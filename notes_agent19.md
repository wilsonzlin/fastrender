Quick notes (Agent19)
----------------------
- CNN: top-strip render only; inline CSS ~1.7MB, heavy :has/:not/attrs, many ad/hidden classes. Cascade ~4s after selector dedup (~720k candidates), layout dominates (~78s). Flex hotspots: product-zone__inner, vertical-shelf carousels (hundreds). Content likely JS-hidden; layout perf bottleneck.
- MSNBC: solid-color render (48,97,255) at 1200x800 (~18s, ~4,138 boxes); scripts 53 (35 external), style tags 28, inline CSS ~349KB; JS-dependent content.
- Renders cached: cnn (bbox top 275px), msnbc (solid), latimes (full frame bbox ~8..1199 x 8..799), cloudflare (bbox to y~728), wired (full frame), github (full frame).

Status: repo clean, origin/main @5befaf3.
