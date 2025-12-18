Notes (Agent19)
----------------
- Grid/hero fix on main: grid items are laid out via their own formatting contexts using Taffy-resolved sizes, preserving child fragments/box_ids. GitHub hero text renders.
- Shaping cache in `text/shaper.rs` (4k entries keyed by font ptr/index/size/script/direction/text) to reuse shaping work.
- `inspect_frag` supports `--timeout SECONDS` to exit after the given duration.
- CNN: render still slow (~82s) and shows only the top ~275px strip; likely JS-driven/hidden content. Wired.com renders full frame.

Status: synced to origin/main; workspace clean.
