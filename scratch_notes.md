Quick notes (Agent19)
----------------------
- CNN: render shows only top ~275px; inline CSS only (~1.7MB), heavy :has/:not/attrs, many ad/hidden classes (ad-slot/layout-homepage-mobile/adSlotLoaded). Cascade ~4s after dedup (~720k candidates); layout dominates (~78s). Flex hotspots: product-zone__inner, vertical-shelf carousels (hundreds present). Likely JS-driven content hidden; layout perf remains bottleneck.
- MSNBC: render is solid color (48,97,255) at 1200x800 (~18s, ~4,138 boxes); many scripts/styles, no visible content (JS-dependent). Inline CSS ~349KB; scripts 53 (35 external), style tags 28.
- Renders: cnn (top strip), msnbc (solid), latimes (full frame, bbox 8..1199 x 8..799), cloudflare (bbox to y~728), wired (full frame), github (full frame).

Status: repo clean on origin/main (cf652b3). No code changes.
