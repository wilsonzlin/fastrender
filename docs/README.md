# FastRender internal docs (wiki)

This `docs/` directory is the canonical “wiki” for this repository. It is internal-only and should reflect **repo reality** (current code, current tools, current behavior), not historical plans or abandoned file layouts.

If a document can’t be kept accurate, delete it and replace it with something smaller and true.

## Start here

- Running the renderer: [running.md](running.md)
- CLI tools & workflows: [cli.md](cli.md)
- Library API: [api.md](api.md)
- Architecture overview: [architecture.md](architecture.md)
- Debugging renders: [debugging.md](debugging.md)
- Profiling & perf logging: [perf_logging.md](perf_logging.md)
- Runtime environment variables: [env-vars.md](env-vars.md)
- Testing: [testing.md](testing.md)
- Vendoring / dependency patches: [vendoring.md](vendoring.md)
- CSS loading & URL resolution: [css-loading.md](css-loading.md)

## Research & notes

- Research notes: `research/` (spec deep dives and implementation references)
- Durable internal notes: `notes/` (small “why/how” writeups that are worth keeping)

## Conventions

- Prefer linking to actual repo paths (e.g. `src/api.rs`) over describing hypothetical files.
- Avoid “phase/wave/task” planning docs in the wiki; keep planning outside the repo.
- Keep docs scoped: a small accurate doc beats a large drifting one.
