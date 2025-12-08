#!/bin/bash
# grind-spec.sh - Continuously run codex to grind towards spec-compliant rendering

set -e

cd "$(dirname "$0")"

SESSION_LIMIT=50

INITIAL_PROMPT='You are building fastrender - a from-scratch HTML/CSS rendering engine in Rust.

## Mission
Create a correct, spec-faithful renderer. Every layout, every paint, every CSS property per W3C specifications. No hacks, no shortcuts, no laziness, no impropriety, no workarounds, no fallbacks, no deviation from spec.

## You Are Empowered To
- Research specs, read browser source code, study how Servo/WebKit/Blink solve problems
- Redesign and rearchitect - if the current approach is wrong, fix it properly
- Make major changes - delete code, rewrite modules, change data structures
- Be bold and creative - find elegant solutions, think outside the box
- Take a step back when stuck - understand the problem deeply before coding

## Key Files
- `scratchpad.md` - Session notes, current state, discoveries (keep this updated)
- `AGENTS.md` - Architecture overview
- `src/` - Implementation

## Approach
1. Understand current state
2. Identify what needs work
3. Research if needed (specs, browser implementations, papers)
4. Implement correctly with tests
5. Update scratchpad.md with findings
6. `cargo test` to verify
7. Commit and continue

## Git
- Commit and push regularly to save progress
- Stay on the current branch - no branch switching
- No destructive operations (force push, reset --hard, rebase, etc.)

## Note
Taffy is vendored in vendor/taffy/ - modify there directly if needed, not via Cargo.

Read scratchpad.md, then push this renderer towards spec correctness.'

RESUME_PROMPT='Continue building the spec-faithful renderer.

Check scratchpad.md for context. Identify what needs work. Research if needed. Be bold - redesign if the approach is wrong. Implement properly. Update scratchpad.md. Test. Commit and push. Continue.'

spawn_agent() {
    local prompt="$1"
    local resume="$2"

    if [ "$resume" = "true" ]; then
        echo "$prompt" | codex exec --skip-git-repo-check resume --last -
    else
        echo "$prompt" | codex exec -m gpt-5.1-codex-max --skip-git-repo-check -
    fi
}

backoff=0

while true; do
    turn=0

    while [ $turn -lt $SESSION_LIMIT ]; do
        if [ $turn -eq 0 ]; then
            prompt="$INITIAL_PROMPT"
            resume="false"
        else
            prompt="$RESUME_PROMPT"
            resume="true"
        fi

        if spawn_agent "$prompt" "$resume"; then
            backoff=0
            turn=$((turn + 1))
        else
            if [ $backoff -eq 0 ]; then
                backoff=1
            else
                backoff=$((backoff * 2))
                [ $backoff -gt 300 ] && backoff=300
            fi
            sleep $backoff
            break
        fi
    done

    sleep 2
done
