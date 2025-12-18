Notes (Agent19)
----------------
- Grid/hero fix on main: grid items are laid out via their own formatting contexts using Taffy-resolved sizes, preserving child fragments/box_ids. GitHub hero text renders.
- Shaping cache in `text/shaper.rs` (4k entries keyed by font ptr/index/size/script/direction/text) to reuse shaping work.
- `inspect_frag` supports `--timeout SECONDS` to exit after the given duration.
CNN: render still slow (~82s) and shows only the top ~275px strip; likely JS-driven/hidden content. Wired.com renders full frame.
CNN profiling notes: cascade ~4s after selector dedup (720k candidates); layout dominates (~78s). Flex profiling shows product-zone__inner and vertical-shelf carousels repeatedly laid out; cache histogram shows heavy reuse of a few keys. Flex cache tolerances relaxed (min epsilon 1px) and cache key now includes debug selector to merge identical components; flex measure cache quantizes definite available sizes.

Working tree clean, up to date (targets added: vox.com, nationalgeographic.com, hbr.org). Idle/available for new assignment.
Repo still clean and up to date; idle/available for new assignment.

Status: synced to origin/main; workspace clean.
