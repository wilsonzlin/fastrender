# FastRender V2 Task-Based Rebuild System

This directory contains a **task-based parallel execution system** for rebuilding FastRender V2. The system is designed to enable **maximum parallelism** through independent, isolated tasks that can be executed by autonomous agents.

## Quick Start

1. **Read this README** - Understand the system
2. **Read TASK_GRAPH.md** - See the full dependency graph (80 tasks)
3. **Read TASK_TEMPLATE.md** - Understand task file structure
4. **Review example tasks** in `wave-1/` through `wave-6/`
5. **Start executing Wave 1 tasks** (all 13 can run in parallel)

## System Overview

### Key Principles

1. **Complete Independence**: Each task file is 100% self-contained
2. **Maximum Parallelism**: Tasks organized in waves; all tasks in a wave can run simultaneously
3. **Clear Dependencies**: Each task declares exactly what it needs from prior tasks
4. **Structured Communication**: Tasks communicate via notes files with standard schema
5. **Verifiable Completion**: Each task has clear success criteria and tests
6. **Stability of Records**: Once a task is completed, keep a single stable record (or remove it once) and avoid repeatedly adding/removing the same task stub. If a task is already tracked via outputs/notes, don’t reintroduce or toggle placeholder files for bookkeeping.

### Architecture

```
┌─────────────┐
│   Task File │ (Self-contained instructions)
│   (Input)   │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Agent     │ (Reads task, reads dependency notes)
│  Executes   │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  Outputs:   │
│  - Code     │ (Implementation)
│  - Tests    │ (Verification)
│  - Notes    │ (For downstream tasks)
└─────────────┘
```

## File Structure

```
docs/tasks/
├── README.md                    # This file
├── TASK_GRAPH.md                # Complete 80-task dependency graph
├── TASK_TEMPLATE.md             # Template for creating new tasks
│
├── wave-1/                      # 13 tasks (no dependencies)
│   ├── (see notes/W1.T01-notes.md for completed geometry types)
│   ├── W1.R01-css-visual-formatting.md
│   └── ...
│
├── wave-2/                      # 12 tasks (depend on Wave 1)
│   ├── W2.T01-boxnode-type.md
│   └── ...
│
├── wave-3/                      # 18 tasks (depend on Wave 2)
│   ├── W3.T04-block-layout.md
│   └── ...
│
├── wave-4/                      # 15 tasks (depend on Wave 3)
│   └── ...
│
├── wave-5/                      # 14 tasks (depend on Wave 4)
│   └── ...
│
├── wave-6/                      # 8 tasks (depend on Wave 5)
│   └── ...
│
└── outputs/
    └── notes/                   # Notes from completed tasks
        ├── W1.T01-notes.md
        ├── W1.T02-notes.md
        └── ...
```

## Task File Format

Each task file contains:

### 1. YAML Frontmatter

```yaml
---
task_id: "W1.T01"
title: "Implement Geometry Types"
wave: 1
estimated_hours: 4-6
depends_on: []
inputs: []
outputs:
  - "src/geometry.rs"
  - "tests/geometry_test.rs"
  - "docs/tasks/notes/W1.T01-notes.md"
skills_required: ["Rust"]
context_files: ["docs/core/type-system.md"]
verification:
  - "cargo test geometry"
---
```

### 2. Complete Task Description

- **Context**: What, why, and how this fits in
- **Prerequisites**: Required knowledge and reading
- **Inputs**: What to read from dependency tasks
- **Objectives**: Clear, measurable goals
- **Implementation Guide**: Step-by-step with complete code
- **Testing Requirements**: What must be tested
- **Output Artifacts**: What to produce

### 3. Notes File Template

Each task produces a notes file with:
- Implementation summary
- API contracts (exact signatures)
- Decisions made (with rationale)
- Spec interpretations
- Discoveries & gotchas
- Performance notes
- Recommendations for downstream tasks
- Open questions

## Execution Model

### Sequential Waves

Tasks are organized in 6 waves. **You must complete all tasks in a wave before starting the next wave.**

```
Wave 1 (Foundation)
  ↓ All Wave 1 tasks complete
Wave 2 (Core Architecture)
  ↓ All Wave 2 tasks complete
Wave 3 (Layout & Fonts)
  ↓ All Wave 3 tasks complete
Wave 4 (Text & Inline)
  ↓ All Wave 4 tasks complete
Wave 5 (Paint & CSS Features)
  ↓ All Wave 5 tasks complete
Wave 6 (Integration & Testing)
  ↓ Complete!
```

### Parallel Execution Within Waves

All tasks within a wave can execute **completely in parallel**:

- **Wave 1**: 13 tasks in parallel
- **Wave 2**: 12 tasks in parallel
- **Wave 3**: 18 tasks in parallel (highest parallelism!)
- **Wave 4**: 15 tasks in parallel
- **Wave 5**: 14 tasks in parallel
- **Wave 6**: 8 tasks in parallel

### Agent Execution

Each agent executing a task should:

1. **Read the task file** (completely self-contained)
2. **Read context files** (linked in frontmatter)
3. **Read dependency notes** (from inputs list)
4. **Implement the task** (following step-by-step guide)
5. **Write tests** (verification)
6. **Create notes file** (for downstream tasks)
7. **Verify completion** (run tests, check criteria)

## Notes File Schema

**Critical**: Notes files are how tasks communicate!

Every completed task MUST create: `docs/tasks/notes/{task_id}-notes.md`

### Required Sections

```markdown
# Task {task_id} Output Notes

## Implementation Summary
[What was built, 2-3 paragraphs]

## API Contracts
[Exact type signatures, trait definitions]

## Decisions Made
[Architectural choices with rationale]

## Spec Interpretations
[How ambiguous specs were resolved]

## Discoveries & Gotchas
[Non-obvious behaviors, pitfalls to avoid]

## Performance Notes
[Benchmarks, optimization opportunities]

## Recommendations for Downstream Tasks
[Specific guidance for dependent tasks]

## Open Questions
[Unresolved issues for future tasks]

## Test Coverage
[What's tested, what gaps remain]
```

**Example of good recommendations section:**

```markdown
## Recommendations for Downstream Tasks

### For W2.T01 (BoxNode Type):

- **Use `Point::zero()` for default position** instead of `Point::new(0.0, 0.0)`
- **Store positions as Point, not (f32, f32)** for type safety
- **Use `Rect::contains_point()` for hit testing** - it handles edge cases correctly
- **Gotcha**: `Rect::intersect()` returns `None` for zero-area rects, handle that case

### For W3.T04 (Block Layout):

- **Use `Rect::from_origin_size()` when constructing** from separate origin and size
- **Performance**: Avoid cloning Rect in hot paths, use references
- **The `EdgeOffsets` type is useful for padding/margin** calculations
```

## Dependency Management

### Reading Dependency Notes

When your task depends on other tasks, you MUST read their notes files:

```yaml
# In your task frontmatter:
depends_on:
  - "W1.T01"
  - "W1.T03"
inputs:
  - "docs/tasks/notes/W1.T01-notes.md"
  - "docs/tasks/notes/W1.T03-notes.md"
```

**What to look for in dependency notes:**

1. **API Contracts**: What types/functions are available?
2. **Decisions Made**: What patterns should you follow?
3. **Recommendations**: Specific guidance for your task
4. **Discoveries**: What gotchas should you avoid?
5. **Open Questions**: What did they leave for you to resolve?

### Writing Good Notes

Your notes will be read by 5-10 downstream tasks. Make them **detailed and actionable**:

❌ **Bad**: "Implemented geometry types. All tests pass."

✅ **Good**:
```markdown
## Recommendations for W2.T01 (BoxNode)

I implemented Point, Size, Rect with these key decisions:

1. **All coordinates are f32** (not f64) for performance and GPU compatibility
2. **Rect uses origin + size**, not (x1, y1, x2, y2) - matches CSS box model
3. **Use `Rect::contains_point()` for hit testing** - handles edges correctly per CSS spec
4. **EdgeOffsets is separate** for margin/padding - don't embed in Rect

Key gotchas:
- Rect::intersect() returns None for zero-area - check before using
- Point arithmetic is implemented via ops traits - you can do `point1 + point2`
- Rect::union() is inclusive of both rects, even if they don't overlap

Performance notes:
- All types are Copy - don't clone, just pass by value
- Rect::contains_point() is inlined and fast for hot paths
- If doing many rect operations, consider batch allocations

For your task specifically:
- Store box positions as Point, not separate x/y fields
- Use Rect for box bounds - it matches CSS containing block model
- The debug formatting is good for logging: format!("{:?}", rect)
```

## Task Examples by Pattern

We've created 10 example tasks showing different patterns:

### 1. Simple Implementation: W1.T01 (Geometry Types)
- Basic types with no dependencies
- Shows standard struct implementation
- Comprehensive testing pattern

### 2. Research Task: W1.R01 (CSS Spec Research)
- Different format - output is research notes, not code
- Shows how to extract knowledge from specifications
- Template for spec interpretation

### 3. Dependency Consumption: W2.T01 (BoxNode Type)
- Shows how to read and use dependency notes
- Builds on W1.T01 (geometry) and W1.T03 (lengths)
- Integration pattern

### 4. Trait Design: W2.T07 (FormattingContext Trait)
- Shows trait abstraction pattern
- Critical architectural decision
- Template for trait design

### 5. Complex Algorithm: W3.T04 (Block Layout)
- Multi-step implementation
- CSS spec translation to code
- Algorithm verification pattern

### 6. External Library: W3.T14 (Font Database)
- Wrapping third-party libraries (fontdb)
- Library integration pattern
- Facade pattern

### 7. Very Complex: W4.T12 (Inline Layout)
- Depends on many upstream tasks
- Shows coordinating multiple systems
- Most complex algorithm pattern

### 8. Tree Transformation: W5.T02 (Display List)
- Converting one tree to another
- Visitor pattern
- Mid-pipeline integration

### 9. Final Integration: W6.T02 (Rendering Pipeline)
- Wiring everything together
- Public API design
- End-to-end verification

### 10. Testing Infrastructure: W6.T04 (WPT Runner)
- Building test harnesses
- External test suite integration
- Verification tooling

## Verification & Quality

### Task Completion Checklist

Before marking a task complete:

- [ ] All objectives achieved
- [ ] All success criteria met
- [ ] Code compiles: `cargo build`
- [ ] Tests pass: `cargo test`
- [ ] Clippy passes: `cargo clippy -- -D warnings`
- [ ] Formatted: `cargo fmt --check`
- [ ] All public APIs documented (rustdoc)
- [ ] Notes file created with all sections filled
- [ ] Output files in correct locations
- [ ] Dependencies' recommendations followed

### Testing Standards

Every task must include tests:

- **Unit tests**: Test all public functions
- **Edge cases**: Empty, null, boundary conditions
- **Error paths**: Test error handling
- **Integration tests**: If applicable
- **Performance tests**: If applicable (benchmarks)

### Documentation Standards

Every public item must have rustdoc:

```rust
/// Brief one-line summary
///
/// Longer explanation of what this does.
///
/// # Examples
///
/// ```
/// use crate::example;
/// let result = example::function();
/// ```
///
/// # Errors
///
/// Returns `Error::Type` when...
///
/// # Panics
///
/// Panics if...
pub fn function() -> Result<T> {
    // implementation
}
```

## Time Estimates

### Per-Task Estimates

Based on example tasks:

- **Simple implementation** (W1.T01): 4-6 hours
- **Research task** (W1.R01): 6-8 hours
- **Complex algorithm** (W3.T04): 10-14 hours
- **Very complex** (W4.T12): 12-16 hours
- **Integration** (W6.T02): 8-12 hours

### Total Project Estimates

**Serial execution** (one task at a time):
- Total: 600-800 hours
- Calendar time: 6-12 months

**Parallel execution** (10 agents):
- Total: Same work hours, but distributed
- Calendar time: 2-3 weeks (critical path ~80-100 hours)

### Wave Estimates

| Wave | Tasks | Total Hours | Parallel Hours |
|------|-------|-------------|----------------|
| 1 | 13 | 60-80 | 6-8 (max of any single task) |
| 2 | 12 | 45-65 | 6-8 |
| 3 | 18 | 120-160 | 12-16 (table layout) |
| 4 | 15 | 100-130 | 12-16 (inline layout) |
| 5 | 14 | 95-120 | 10-12 |
| 6 | 8 | 60-80 | 10-14 (WPT runner) |

**Critical path**: ~80-100 hours minimum wall-clock time

## Workflow

### For Coordinators

1. **Setup Phase**:
   - Create all 80 task files (or generate from templates)
   - Set up outputs directory structure
   - Prepare agent pool

2. **Wave 1 Execution**:
   - Distribute all 13 Wave 1 tasks to agents
   - Monitor completion
   - Collect all outputs and notes

3. **Wave 1 Verification**:
   - Run all tests: `cargo test`
   - Verify all notes files exist
   - Check quality of notes (are recommendations clear?)

4. **Wave 2 Execution**:
   - Distribute all 12 Wave 2 tasks
   - Each agent reads relevant Wave 1 notes
   - Monitor and collect outputs

5. **Repeat** for Waves 3-6

6. **Final Verification**:
   - Full integration test
   - WPT test run
   - Performance benchmarks
   - Code quality review

### For Agents

1. **Receive task assignment**: "Execute W1.T01"

2. **Read task file**: `notes/W1.T01-notes.md` (completed reference)

3. **Read context**:
   - `docs/core/type-system.md` (if referenced)
   - Any other context files

4. **Read dependency notes**: (if any)
   - Check `inputs` in frontmatter
   - Read all dependency notes files
   - Extract recommendations for your task

5. **Execute implementation**:
   - Follow step-by-step guide
   - Write code
   - Write tests
   - Document APIs

6. **Create notes file**:
   - `docs/tasks/notes/W1.T01-notes.md`
   - Fill all required sections
   - Be specific and actionable

7. **Verify completion**:
   - Run verification commands from frontmatter
   - Check completion checklist
   - Ensure all outputs created

8. **Report completion**: Task done, outputs available

## Common Pitfalls

### 1. Not Reading Dependency Notes

❌ **Bad**: Start coding immediately without reading notes from dependencies

✅ **Good**: Carefully read ALL dependency notes, extract:
- API contracts to use
- Patterns to follow
- Recommendations for your specific task

### 2. Vague Notes

❌ **Bad**: "Implemented block layout. It works."

✅ **Good**: "Implemented block layout with these key decisions:
- Margin collapsing uses max(positive) - max(abs(negative)) algorithm
- Width computation follows CSS 2.1 Section 10.3.3 exactly
- Auto margins in BFC are treated as 0 for vertical margins
- For W4.T12: You'll need to call compute_intrinsic_width() when laying out inline-blocks"

### 3. Skipping Tests

❌ **Bad**: Write implementation, skip tests

✅ **Good**: Write comprehensive tests including:
- Happy path
- Edge cases (empty, null, boundary)
- Error conditions
- Integration scenarios

### 4. Deviating from Established Patterns

❌ **Bad**: Create your own error handling approach

✅ **Good**: Follow patterns from dependencies:
- Use the same Error type
- Follow the same Result<T> patterns
- Match the coding style

### 5. Incomplete Documentation

❌ **Bad**: Skip rustdoc, "code is self-documenting"

✅ **Good**: Document every public item:
- What it does
- When to use it
- Example usage
- Possible errors
- Edge cases to watch for

## Advanced Topics

### Creating New Tasks

If you need to create additional tasks:

1. Copy `TASK_TEMPLATE.md`
2. Fill in all sections
3. Assign a task ID: `WX.TYY` where X is wave, YY is number
4. Add to appropriate wave directory
5. Update `TASK_GRAPH.md` dependency graph
6. Ensure task is completely self-contained

### Splitting Tasks

If a task is too large (>16 hours):

1. Split into subtasks: W3.T04a, W3.T04b, etc.
2. Define dependencies between subtasks
3. Each subtask should still be 4-16 hours
4. Ensure each subtask has clear completion criteria

### Handling Blockers

If a task discovers a blocker:

1. **Document in notes**: Clearly describe the issue
2. **Create open question**: Specify what needs resolution
3. **Suggest workaround**: Temporary solution if possible
4. **Flag for coordinator**: Alert that follow-up task may need adjustment

### Integration Points

Some tasks are **integration points** that verify multiple systems work together:

- W6.T01: Integrate all FCs into factory
- W6.T02: Wire complete rendering pipeline
- W6.T04: WPT test runner (verifies whole system)

These tasks are critical for catching integration issues early.

## FAQ

### Q: Can I start Wave 2 before Wave 1 is complete?

**A: No.** Wave 2 tasks depend on Wave 1 outputs (code and notes). You must complete ALL Wave 1 tasks first.

### Q: Can I work on W3.T04 and W3.T06 simultaneously?

**A: Yes!** Within a wave, all tasks can run in parallel. W3.T04 (block layout) and W3.T06 (table layout) are independent.

### Q: What if my task discovers that a dependency's API is insufficient?

**A: Document it.** In your notes, create an "Open Questions" entry describing what's needed. If critical, it may require going back to the dependency task.

### Q: How detailed should notes be?

**A: Very detailed.** Assume downstream tasks have zero context. Your notes are their only source of information about your decisions and discoveries.

### Q: Can I modify code from a dependency task?

**A: Generally no.** Treat dependency outputs as immutable. If you find a bug, document it in your notes and work around it. Critical bugs can be fixed in later waves.

### Q: What if specs are ambiguous?

**A: Make a decision and document it.** In your notes, quote the spec, explain the ambiguity, state your interpretation, and justify why. This becomes the project's canonical interpretation.

## Success Metrics

### Task-Level Success

Each task is successful when:

- ✅ All objectives achieved
- ✅ All tests pass
- ✅ Code quality checks pass (clippy, fmt)
- ✅ Complete notes file created
- ✅ Verified by coordinator

### Wave-Level Success

A wave is complete when:

- ✅ All tasks in wave complete
- ✅ All notes files exist and are comprehensive
- ✅ Full test suite passes: `cargo test`
- ✅ No blockers for next wave

### Project-Level Success

The project is complete when:

- ✅ All 80 tasks complete
- ✅ WPT pass rate >80% for implemented features
- ✅ Reference tests pass >95%
- ✅ Performance within 2x of browser baseline
- ✅ Complete documentation
- ✅ Public API stable and documented

## Getting Started

**Ready to begin?**

1. ✅ Read this README
2. ✅ Read TASK_GRAPH.md to understand dependencies
3. ✅ Read TASK_TEMPLATE.md to understand structure
4. ✅ Review 2-3 example tasks in wave-1/
5. ➡️ **Start executing Wave 1 tasks!**

**First tasks to try:**

- **W1.T01**: Geometry types (simplest, good starting point)
- **W1.R01**: CSS research (different pattern, good for spec study)
- **W1.T03**: Length types (slightly more complex, CSS parsing)

Good luck! Remember: your notes are how you help the next agent. Be thorough! 🚀

---

**Last Updated:** 2025-01-20
**System Version:** 1.0
**Total Tasks:** 80
**Estimated Timeline:** 2-3 weeks with ~10 parallel agents
