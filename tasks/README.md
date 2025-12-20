### Guard Rails

- Guard tests: run `just guard-tests` before pushing to catch accidental deletions of critical files (README presence + CLI example, style regression fixture presence, fetch_and_render exit regression).
  - Quick sanity: `just guard-tests-quick` prints the guard list.

