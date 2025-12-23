# Dependency locking

We **commit** the workspace `Cargo.lock` to keep dependency resolution deterministic across CI and local runs.

Why this matters:

- Reproducible rendering: small dependency shifts can change pixel output and benchmark numbers.
- CI cache keys hash `Cargo.lock`, so commits without it would thrash caches and mask dependency drift.
- Path/vendored dependencies (e.g. Taffy) rely on the lockfile to stay pinned alongside our code.

Policy:

- Do not add `Cargo.lock` to `.gitignore` or delete it.
- When changing dependencies on purpose, run `cargo update` (optionally `-p <crate>`), then commit the resulting lockfile diff.
- CI runs a lockfile check; it will fail if the file is missing or out of sync.
