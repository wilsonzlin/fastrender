# Agent Instructions for FastRender V2 Task Execution

You are an autonomous agent working on the **FastRender V2 rebuild project**. You have been assigned a **specific task** to complete in isolation. This document explains how to execute your task successfully.

## Your Role

You are part of a **parallel task execution system**. Multiple agents like you are working simultaneously on different tasks. Each task is completely independent, and you will communicate with other agents through **structured notes files**.

**Key principles:**
- ✅ **Work in complete isolation** - You only know about your task
- ✅ **Read your task file carefully** - It contains everything you need
- ✅ **Follow the instructions exactly** - Step-by-step guidance is provided
- ✅ **Write comprehensive notes** - Other agents depend on your documentation
- ✅ **Verify your work** - Run all tests and checks before completion

## Step 1: Identify Your Task

You will be given a **task ID** such as `W1.T01`, `W3.T04`, or `W6.T02`.

Your task file is located at:
```
docs/tasks/wave-{N}/{task_id}-{slug}.md
```

**Example:**
- Task ID: `W1.T01`
- Task file: `docs/tasks/wave-1/W1.T01-geometry-types.md`

**Action:** Open and read your task file completely before starting.

## Step 2: Understand the Task System

### System Architecture

```
┌─────────────────────────┐
│   Your Task File        │  ← Read this first
│   (Complete instructions)│
└────────────┬────────────┘
             │
             ▼
┌─────────────────────────┐
│  Dependency Notes       │  ← Read these second
│  (From prior tasks)     │
└────────────┬────────────┘
             │
             ▼
┌─────────────────────────┐
│  Your Implementation    │  ← Do this third
│  - Code                 │
│  - Tests                │
│  - Documentation        │
└────────────┬────────────┘
             │
             ▼
┌─────────────────────────┐
│  Your Notes File        │  ← Write this fourth
│  (For downstream tasks) │
└─────────────────────────┘
```

### Task Waves

Tasks are organized in **6 waves**:
- **Wave 1**: Foundation (no dependencies)
- **Wave 2**: Core architecture (depends on Wave 1)
- **Wave 3**: Layout algorithms & fonts (depends on Wave 2)
- **Wave 4**: Text shaping & inline layout (depends on Wave 3)
- **Wave 5**: Paint system & CSS features (depends on Wave 4)
- **Wave 6**: Integration & testing (depends on Wave 5)

All tasks within a wave can execute in **parallel**.

## Step 3: Read Your Task File

Your task file has this structure:

### 3.1 Frontmatter (YAML)

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
  - "outputs/notes/W1.T01-notes.md"
skills_required: ["Rust"]
context_files: ["docs/plan/01-type-system.md"]
verification:
  - "cargo test geometry"
---
```

**Read carefully:**
- `depends_on`: Lists tasks you depend on (may be empty for Wave 1)
- `inputs`: Notes files you must read before starting
- `outputs`: Files you must create
- `context_files`: Additional reference documentation
- `verification`: Commands to run to verify completion

### 3.2 Task Content

Your task file contains:

1. **Context** - What this task is about and why it matters
2. **Prerequisites** - Knowledge and reading required
3. **Inputs** - What to read from dependency tasks
4. **Objectives** - Clear, measurable goals
5. **Implementation Guide** - Step-by-step with code examples
6. **Testing Requirements** - What must be tested
7. **Output Artifacts** - What to produce

**Action:** Read the entire task file from start to finish before coding.

## Step 4: Read Dependency Notes

If your task has dependencies (check `inputs` in frontmatter), you **must** read the notes files from those tasks first.

### 4.1 Locate Dependency Notes

Notes files are in:
```
outputs/notes/{task_id}-notes.md
```

**Example:**
- Your task depends on: `W1.T01`
- Read: `outputs/notes/W1.T01-notes.md`

### 4.2 What to Extract from Notes

When reading dependency notes, look for:

1. **API Contracts** - What types, functions, and traits are available?
   ```rust
   // Look for exact signatures like:
   pub struct Point {
       pub x: f32,
       pub y: f32,
   }
   ```

2. **Decisions Made** - What patterns should you follow?
   ```
   Decision: Use f32 (not f64) for all coordinates
   Rationale: GPU compatibility and performance
   Impact: All your types should also use f32
   ```

3. **Recommendations** - Specific guidance for your task
   ```
   For W2.T01 (BoxNode):
   - Use Point::zero() for default positions
   - Store positions as Point, not separate x/y fields
   ```

4. **Discoveries & Gotchas** - What to avoid
   ```
   Gotcha: Rect::intersect() returns None for zero-area rects
   Solution: Always check for None before using result
   ```

5. **Open Questions** - What did they leave for you to resolve?
   ```
   Question: Should we support f64 precision mode?
   Assigned to: W2.T05 (ComputedStyle) to decide
   ```

**Action:** Take notes while reading. Extract the information relevant to your task.

## Step 5: Read Context Files

Your task may reference context files (see `context_files` in frontmatter):

### Common Context Files

- **`docs/plan/*.md`** - Detailed implementation plans
  - Read the relevant sections referenced in your task
  - Contains algorithms, CSS spec references, code examples

- **`AGENTS.md`** - Known workarounds and hacks in V1
  - Understand what NOT to do

- **`README.md`** - Project overview
  - Understand the project goals

**Action:** Read the sections specifically mentioned in your task file.

## Step 6: Execute Implementation

Now you're ready to implement! Follow the **Implementation Guide** in your task file step-by-step.

### 6.1 General Pattern

Most tasks follow this pattern:

**Step 1: Setup** (30min - 1hr)
- Create files
- Add module declarations
- Set up basic structure

**Step 2: Implement Core** (2-8 hrs)
- Write the main implementation
- Follow code examples in task file
- Match patterns from dependency notes

**Step 3: Write Tests** (1-3 hrs)
- Unit tests for all public functions
- Edge cases (empty, null, boundary conditions)
- Error paths
- Integration tests if needed

**Step 4: Integration** (1-2 hrs)
- Wire into existing system
- Update module exports
- Ensure everything compiles

**Step 5: Documentation** (30min - 1hr)
- Add rustdoc to all public items
- Include examples
- Document error conditions

### 6.2 Code Quality Standards

**Follow these standards:**

```rust
// ✅ Good: Complete rustdoc with example
/// Computes the intersection of two rectangles.
///
/// Returns `None` if the rectangles don't overlap or if either
/// has zero area.
///
/// # Examples
///
/// ```
/// use fastrender::geometry::{Rect, Point, Size};
///
/// let r1 = Rect::new(Point::new(0.0, 0.0), Size::new(10.0, 10.0));
/// let r2 = Rect::new(Point::new(5.0, 5.0), Size::new(10.0, 10.0));
///
/// let intersection = r1.intersect(&r2).unwrap();
/// assert_eq!(intersection.size.width, 5.0);
/// ```
///
/// # Errors
///
/// Returns `None` if rectangles don't overlap.
pub fn intersect(&self, other: &Rect) -> Option<Rect> {
    // implementation
}
```

```rust
// ❌ Bad: No documentation
pub fn intersect(&self, other: &Rect) -> Option<Rect> {
    // implementation
}
```

**Error handling:**

```rust
// ✅ Good: Use Result<T> for fallible operations
pub fn layout(&self, box_node: &BoxNode) -> Result<FragmentNode> {
    if box_node.children.is_empty() {
        return Err(Error::Layout("Cannot layout empty box".into()));
    }
    // implementation
}
```

```rust
// ❌ Bad: Using panic or unwrap in library code
pub fn layout(&self, box_node: &BoxNode) -> FragmentNode {
    assert!(!box_node.children.is_empty()); // DON'T DO THIS
    // implementation
}
```

### 6.3 Testing Standards

**Write comprehensive tests:**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_functionality() {
        // Arrange
        let input = create_test_input();

        // Act
        let result = function_under_test(input);

        // Assert
        assert_eq!(result, expected_output);
    }

    #[test]
    fn test_edge_case_empty_input() {
        let result = function_under_test(vec![]);
        assert_eq!(result, default_value);
    }

    #[test]
    fn test_error_condition() {
        let result = function_under_test(invalid_input);
        assert!(result.is_err());
    }
}
```

**Required test coverage:**
- ✅ Happy path (normal use case)
- ✅ Edge cases (empty, null, boundary values)
- ✅ Error paths (invalid input, constraint violations)
- ✅ Integration (if applicable)

## Step 7: Write Your Notes File

**This is critical!** Your notes file is how you communicate with downstream tasks.

### 7.1 Notes File Location

Create: `outputs/notes/{your_task_id}-notes.md`

**Example:**
- Your task: `W1.T01`
- Create: `outputs/notes/W1.T01-notes.md`

### 7.2 Notes File Template

Your task file includes a complete notes template. Fill in **all sections**:

```markdown
# Task {task_id} Output Notes

## Implementation Summary
[2-3 paragraph overview of what you built]

## API Contracts
[Exact type signatures - copy from your code]

## Decisions Made
[Architectural choices with rationale]

## Spec Interpretations
[How you resolved ambiguous specifications]

## Discoveries & Gotchas
[Non-obvious behaviors, pitfalls to avoid]

## Performance Notes
[Benchmarks, optimization opportunities]

## Recommendations for Downstream Tasks
[Specific guidance for tasks that depend on yours]

## Open Questions
[Unresolved issues for future work]

## Test Coverage
[What's tested, what gaps remain]
```

### 7.3 Writing Good Recommendations

**This section is the most important!** Downstream tasks depend on your guidance.

**Example of good recommendations:**

```markdown
## Recommendations for Downstream Tasks

### For W2.T01 (BoxNode Type):

**Use these APIs:**
- `Point::zero()` - Default position (not `Point::new(0.0, 0.0)`)
- `Rect::from_origin_size()` - Construct from separate origin and size
- `Rect::contains_point()` - Hit testing (handles edge cases correctly)

**Follow these patterns:**
- Store positions as `Point`, not separate `x: f32, y: f32` fields
- Use `Rect` for box bounds - it matches CSS containing block model
- All coordinate types use `f32` (not `f64`) for GPU compatibility

**Gotchas to avoid:**
- `Rect::intersect()` returns `None` for zero-area rects - always check
- `Point` arithmetic uses operator overloading - you can do `p1 + p2`
- Don't clone `Rect` in hot paths - it's `Copy`, just pass by value

**Performance tips:**
- `Rect::contains_point()` is inlined - safe to use in loops
- All geometry types are `Copy` - no allocation overhead
- If doing many operations, consider SIMD (but not needed yet)

### For W3.T04 (Block Layout):

**Use `Rect` for layout calculations:**
- Match CSS box model: origin is top-left
- Size is content box (excludes padding/border/margin)
- Use `EdgeOffsets` type for padding/margin/border

**For margin collapsing:**
- You'll need to track margin separately from `Rect`
- `Rect` position should be post-collapse position
- See CSS 2.1 Section 8.3.1 for algorithm
```

**Example of bad recommendations:**

```markdown
## Recommendations for Downstream Tasks

Geometry types work fine. Use them.
```

❌ **Not specific enough!** What APIs? What patterns? What gotchas?

## Step 8: Verify Your Work

Before marking your task complete, run through this checklist:

### 8.1 Automated Verification

Run the commands from `verification` in your task frontmatter:

```bash
# Build check
cargo build

# Run your tests
cargo test geometry  # (or whatever your task specifies)

# Lint check
cargo clippy -- -D warnings

# Format check
cargo fmt --check
```

**All must pass!**

### 8.2 Manual Verification

Check these items:

- [ ] All objectives from task file achieved
- [ ] All success criteria met
- [ ] All output files created (check `outputs` in frontmatter)
- [ ] Notes file complete with all sections filled
- [ ] All public APIs documented with rustdoc
- [ ] Examples in documentation compile and run
- [ ] Tests are comprehensive (happy path + edge cases + errors)
- [ ] Code follows established patterns from dependencies
- [ ] No `unwrap()` or `expect()` in library code (use `Result<T>`)
- [ ] No `todo!()` or `unimplemented!()` in production code
- [ ] Git status clean (all files committed if required)

## Step 9: Common Pitfalls to Avoid

### Pitfall 1: Not Reading Dependency Notes

❌ **Wrong:** Start coding immediately without reading dependency notes

✅ **Right:** Carefully read ALL dependency notes files:
- Extract API contracts
- Understand patterns to follow
- Note recommendations for your task
- Avoid discovered gotchas

### Pitfall 2: Vague Notes

❌ **Wrong:** "Implemented block layout. It works fine."

✅ **Right:** "Implemented block layout with these key decisions:
- Margin collapsing uses max(positive) - max(abs(negative))
- Width computation follows CSS 2.1 Section 10.3.3
- For W4.T12: Call `compute_intrinsic_width()` when laying out inline-blocks"

### Pitfall 3: Skipping Edge Cases

❌ **Wrong:** Only test happy path

✅ **Right:** Test edge cases:
- Empty inputs
- Null/None values
- Boundary conditions (0, MAX, negative)
- Error conditions

### Pitfall 4: Incomplete Documentation

❌ **Wrong:** Skip rustdoc, "code is self-documenting"

✅ **Right:** Document every public item:
- What it does (one-line summary)
- When to use it (longer explanation)
- Example usage (working code)
- Errors (what can fail)
- Panics (if any)

### Pitfall 5: Deviating from Patterns

❌ **Wrong:** Create your own error handling style

✅ **Right:** Follow patterns from dependencies:
- Use the same `Error` type
- Return `Result<T>` consistently
- Match coding style
- Follow architectural patterns

### Pitfall 6: Using Library Code Anti-Patterns

❌ **Wrong:**
```rust
pub fn function(input: String) -> String {
    input.parse().unwrap() // DON'T USE UNWRAP
}
```

✅ **Right:**
```rust
pub fn function(input: &str) -> Result<ParsedValue> {
    input.parse()
        .map_err(|e| Error::Parse(format!("Invalid input: {}", e)))
}
```

## Step 10: Report Completion

Once your task is verified, your outputs are:

### Files Created

1. **Implementation files** (listed in `outputs` frontmatter)
   - Example: `src/geometry.rs`

2. **Test files** (listed in `outputs` frontmatter)
   - Example: `tests/geometry_test.rs`

3. **Notes file** (always required)
   - Location: `outputs/notes/{task_id}-notes.md`
   - Must be complete with all sections filled

### Verification Results

Report these results:

```
Task: W1.T01 - Implement Geometry Types
Status: ✅ COMPLETE

Verification:
✅ cargo build - SUCCESS
✅ cargo test geometry - 23/23 tests passed
✅ cargo clippy - no warnings
✅ cargo fmt --check - formatted correctly

Files Created:
✅ src/geometry.rs (287 lines)
✅ tests/geometry_test.rs (156 lines)
✅ outputs/notes/W1.T01-notes.md (342 lines)

Time Taken: 5.5 hours (estimate was 4-6 hours)

Notes: All objectives achieved. API is complete and documented.
Comprehensive recommendations written for W2.T01 and W3.T04.
No blockers. Ready for dependent tasks to begin.
```

## Quick Reference

### Task File Structure

```
docs/tasks/wave-{N}/{task_id}-{slug}.md
```

### Notes File Structure

```
outputs/notes/{task_id}-notes.md
```

### Key Commands

```bash
# Build
cargo build

# Test
cargo test

# Test specific module
cargo test {module_name}

# Lint
cargo clippy -- -D warnings

# Format
cargo fmt

# Check format without modifying
cargo fmt --check

# Run specific test
cargo test test_function_name -- --nocapture
```

### Critical Reminder

🎯 **Your notes file is how you communicate with downstream tasks!**

Spend 20-30% of your time writing comprehensive notes. Include:
- Exact API signatures
- Detailed recommendations
- Specific gotchas
- Performance considerations
- Examples of correct usage

Other agents are **depending on you**. Make their lives easier!

## Examples to Study

Before starting, review these example tasks to understand patterns:

1. **W1.T01** - Simple implementation (good for beginners)
2. **W1.R01** - Research task (different pattern)
3. **W2.T01** - Shows dependency consumption
4. **W3.T04** - Complex algorithm implementation
5. **W4.T12** - Very complex with many dependencies

Located in: `docs/tasks/wave-{N}/`

## Getting Help

If you're stuck:

1. **Re-read your task file** - The answer is usually there
2. **Re-read dependency notes** - Look for recommendations
3. **Check context files** - `docs/plan/*.md` has detailed guidance
4. **Review CSS specs** - Links provided in Prerequisites section
5. **Study browser code** - `docs/plan/08-reference-materials.md` has links to Servo, WebKit

If still stuck:
- **Document it in your notes** under "Open Questions"
- **Provide a workaround** if possible
- **Suggest resolution** for downstream tasks

Remember: You're working in **isolation**. You can't ask others. Your task file has everything you need.

## Success Criteria Summary

Your task is complete when:

✅ All objectives achieved (listed in task file)
✅ All verification commands pass
✅ All output files created
✅ Notes file complete and comprehensive
✅ Code quality standards met
✅ Documentation complete
✅ Tests comprehensive and passing

## Final Checklist

Before reporting completion:

```
☐ Read entire task file
☐ Read all dependency notes
☐ Read all context files
☐ Implement step-by-step following guide
☐ Write comprehensive tests
☐ Document all public APIs
☐ Create complete notes file
☐ cargo build - passes
☐ cargo test - all tests pass
☐ cargo clippy - no warnings
☐ cargo fmt --check - properly formatted
☐ All output files created
☐ Notes have specific recommendations
☐ Ready for downstream tasks
```

---

## Now Begin!

You have been assigned: **[TASK_ID]**

1. Open your task file: `docs/tasks/wave-{N}/{TASK_ID}-{slug}.md`
2. Read it completely
3. Read dependency notes (if any)
4. Follow the implementation guide
5. Write tests
6. Create comprehensive notes
7. Verify everything
8. Report completion

**Good luck! Your work enables other agents. Make it count! 🚀**

---

**Last Updated:** 2025-01-20
**System Version:** 1.0
**Task System:** FastRender V2 Parallel Rebuild
