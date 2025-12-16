#!/bin/bash
# grind-spec.sh - Continuously run codex to grind towards working, spec-faithful rendering

set -e

cd "$(dirname "$0")"

SESSION_LIMIT=50

INITIAL_PROMPT='You are building fastrender - a spec-faithful HTML/CSS rendering engine in Rust.

## Mission
Build a CORRECT renderer that works on REAL pages. Both matter equally:
- Spec-faithful: implementations must match CSS/HTML specifications
- Practical: real websites must actually render

Do NOT hack, ignore specs, or implement incompatibly. Incomplete is OK, incorrect is not.
Example: Table layout must follow CSS 2.1 Section 17, not fake it with flexbox.

## Development Approach
1. Test against real pages:
   - cargo run --release --bin fetch_pages  (fetches HTML, caches in fetches/html/)
   - cargo run --release --bin render_pages (renders cached HTML to fetches/renders/)
2. Inspect the PNG outputs visually
3. When something breaks, understand WHY per the spec
4. Implement the correct fix per spec
5. Verify fix doesnt break other pages
6. Commit and continue

## Priority Order
1. NO CRASHES - panics are never acceptable
2. CORRECT LAYOUT - positions/sizes match spec algorithms  
3. VISIBLE CONTENT - text and images render
4. VISUAL FIDELITY - colors, fonts, decorations

## Testing
- fetch_pages: fetches 50+ target pages in parallel, caches HTML to fetches/html/
- render_pages: renders all cached pages in parallel, outputs PNGs to fetches/renders/
- Each page gets a .log file with timing, errors, crash info
- Summary in fetches/renders/_summary.log
- Visually inspect fetches/renders/*.png
- Fix regressions before adding features

## Scratchpad
scratchpad.md is your persistent memory across sessions. Read it at the start. Update it often.

## You Are Empowered To
- Research specs deeply - read CSS 2.1, CSS3 modules, HTML5
- Study browser source (Servo, WebKit, Blink) for implementation guidance
- Refactor/redesign when the current approach is architecturally wrong
- Delete code that is hacky or spec-incompatible
- Be thorough - understand problems fully before fixing

## Constraints
- Implementations must be spec-faithful (incomplete OK, incorrect NOT OK)
- No element-specific hacks in layout/paint
- Taffy is for flex/grid only - tables must use CSS table algorithm
- Changes must not regress other pages

## Key Files
- scratchpad.md - Your notes, read and update often
- AGENTS.md - Architecture overview  
- src/bin/fetch_pages.rs - Fetches target pages in parallel
- src/bin/render_pages.rs - Renders cached pages in parallel
- fetches/renders/ - PNG renders + per-page logs
- fetches/renders/_summary.log - Overall results
- fetches/html/ - Cached HTML
- fetches/assets/ - Cached images/CSS

## Inspecting Renders
After running render_pages, LOOK at the output PNGs in fetches/renders/:
- Do pages render without crashing?
- Is text content visible and readable?
- Is layout roughly correct (boxes in right places)?
- Are there obvious visual bugs?
- Check _summary.log for crash/error counts

Use: open fetches/renders/*.png

## Git
- Commit after meaningful progress
- Stay on current branch
- No destructive operations (force push, reset --hard, rebase)

Start by reading scratchpad.md for context, then continue from there.'

RESUME_PROMPT='Continue building the spec-faithful renderer.

Read scratchpad.md for context. Update it as you work.

Priority: Fix crashes > Fix layout > Fix content > Visual polish

Spec-faithful AND working on real pages. Both matter.
Incomplete OK, incorrect NOT OK.'

spawn_agent() {
    local prompt="$1"
    local resume="$2"

    local args=(
        --sandbox danger-full-access
        --skip-git-repo-check
        -c 'model_reasoning_effort="xhigh"'
    )

    if [ "$resume" = "true" ]; then
        echo "$prompt" | codex exec "${args[@]}" resume --last -
    else
        echo "$prompt" | codex exec "${args[@]}" -m gpt-5.1-codex-max -
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
