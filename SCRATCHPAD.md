Notes (Agent19)
----------------
- CNN: JS-driven; render shows only the top ~275px. Inline CSS only (3 blocks ~199k/1.49M/433 bytes), ~680 `display:none`, 206 media queries, heavy :has/:not/attribute selectors; body HTML mostly header/footer/survey content.
- msnbc.com: render at 1200x800 (~18s, ~4,138 boxes) produced an all-white PNG (single color), likely JS-dependent content.
- Prior fixes on main: grid/hero layout preserved via formatting contexts; shaping cache; `inspect_frag` supports `--timeout`.

Status: clean on origin/main (`90504ca`).
