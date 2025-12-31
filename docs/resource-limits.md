# Resource limits (RAM / CPU / time) for agents

FastRender work involves hostile inputs (real pages) and complex algorithms. **Any run can go pathological**.
We want safe defaults so no benchmark or CLI can eat the machine.

This doc describes a two-layer strategy:

1. **OS-level hard caps** (always available, works for *any* command)
2. **In-process guardrails** (for our own binaries, better error messages + staged attribution)

## 1) OS-level caps (recommended default for agents)

### Convenience wrapper (recommended encouraging default)

Use the repo helper which prefers `prlimit` and falls back to `ulimit`:

```bash
scripts/run_limited.sh --as 8G --cpu 60 -- \
  cargo bench --bench selector_bloom_bench
```

You can also set defaults via environment variables:

```bash
LIMIT_AS=8G LIMIT_CPU=60 scripts/run_limited.sh -- cargo run --release --bin pageset_progress -- run --timeout 5
```

### A. `prlimit` (best general-purpose tool)

If `prlimit` is available (usually via `util-linux`), it can cap address-space and CPU:

```bash
prlimit --as=8G --rss=8G --cpu=30 -- \
  cargo run --release --bin pageset_progress -- run --timeout 5
```

Notes:
- `--as` (virtual address space) is the most reliable “hard memory ceiling”.
- `--rss` is not reliably enforced on all kernels; treat it as advisory.
- Cap `cargo` itself if you are running “cargo run”; `cargo` spawns child processes and inherits limits.

### B. `ulimit` (portable shell-level fallback)

In bash/zsh you can cap virtual memory and stack:

```bash
ulimit -v $((8 * 1024 * 1024))   # KiB
ulimit -s $((64 * 1024))         # KiB
```

Then run your command in the same shell.

### C. cgroups / systemd (best isolation)

If systemd is available:

```bash
systemd-run --user -p MemoryMax=8G -p CPUQuota=200% -- \
  cargo run --release --bin pageset_progress -- run --timeout 5
```

This is the most robust approach for multi-agent environments.

## 2) In-process caps (for FastRender binaries)

OS caps are blunt: they stop the process, but don’t tell us *why*. For FastRender CLIs, prefer:

- **Hard wall-clock timeout** (already exists in `pageset_progress` and `render_pages`)
- **Cooperative per-stage deadlines** (so timeouts are attributed to a stage)
- **Bounded caches** (LRU with explicit byte/item caps)

### What to implement next (repo plan)

- **Add a `--mem-limit-mb` flag** to the render CLIs (at least `pageset_progress`, `render_pages`, `fetch_and_render`).
  - Implementation: call `setrlimit(RLIMIT_AS, …)` at process start (Linux).
  - This enforces a hard ceiling even when the command is invoked through other tooling.
- **Add per-stage “allocation budget” counters** for known hotspots (images, CSS parse, display list build).
  - Goal: fail with a diagnostic like “paint rasterize exceeded 512MB budget” instead of OOM.
- **Make every unbounded cache bounded** (items and/or bytes) with explicit configuration knobs.
- **Bench safety**: benches must never allocate unboundedly by default.
  - Example: `benches/selector_bloom_bench.rs` now has a guardrail (`FASTR_BLOOM_BENCH_MAX_ELEMS`).

## Operational guidance

- For pageset runs, set an OS memory cap by default (cgroups or prlimit).
- When a cap is hit, treat it as a **bug**: either an algorithmic explosion or an unbounded cache.
- The “correct fix” is almost always: reduce asymptotic work, add early exits, and bound caches—**not** “skip rendering”.

