# Task Template

This is the template for creating new task files. Each task is a completely self-contained unit of work.

## File Naming

`wave-{N}/{task_id}-{slug}.md`

Examples:
- `notes/W1.T01-notes.md`
- `wave-3/W3.T04-block-layout.md`

## Template Structure

```markdown
---
task_id: "WX.TYY"
title: "Clear, Actionable Task Title"
wave: X
estimated_hours: N-M
depends_on:
  - "WA.TBB"
  - "WA.TCC"
inputs:
  - "docs/tasks/notes/WA.TBB-notes.md"
  - "docs/tasks/notes/WA.TCC-notes.md"
  - "src/some/existing/file.rs" (if applicable)
outputs:
  - "src/path/to/new/file.rs"
  - "tests/path/to/test_file.rs"
  - "docs/tasks/notes/WX.TYY-notes.md"
skills_required:
  - "Rust"
  - "Specific domain knowledge"
context_files:
  - "docs/*/relevant-topic.md"
  - "AGENTS.md" (if relevant)
  - "README.md" (if relevant)
verification:
  - "cargo test {specific_test}"
  - "cargo build --release"
  - "Manual check: {specific thing}"
---

# {Task Title}

## Context

[2-3 paragraphs explaining what this task is about, why it's needed, and how it fits into the bigger picture. This should be enough context that an agent with NO prior knowledge can understand the task.]

### Background

[Explain relevant concepts, terminology, and domain knowledge needed. Link to specs/references.]

### Why This Matters

[Explain the importance of this task and what would break without it.]

## Prerequisites

### Required Knowledge
- Knowledge area 1 (e.g., "Understanding of CSS box model")
- Knowledge area 2 (e.g., "Rust trait system")
- Knowledge area 3 (e.g., "Basic graph algorithms")

### Required Reading
1. **File/Resource 1**: What to learn from it
2. **File/Resource 2**: What to learn from it
3. **Specification Section X.Y**: Specific concepts to understand

## Inputs

### From Dependencies

Read these notes files from prerequisite tasks:

1. **{dependency-task-id}-notes.md**
   - Look for: API contracts for Type X
   - Look for: Decisions about Y
   - Look for: Recommendations about Z

2. **{dependency-task-id}-notes.md**
   - Look for: [specific information needed]

### Existing Code

Read these existing files:
- `src/path/file.rs` - Understand API X
- `docs/*/relevant.md` - Reference implementation guide (Section Y is most relevant)

## Objectives

### Primary Goals

1. **Goal 1**: Specific, measurable objective
2. **Goal 2**: Specific, measurable objective
3. **Goal 3**: Specific, measurable objective

### Success Criteria

- [ ] Criterion 1 (testable)
- [ ] Criterion 2 (testable)
- [ ] Criterion 3 (testable)
- [ ] All tests pass
- [ ] Code follows style guide (run `cargo clippy`)
- [ ] Public APIs documented with rustdoc
- [ ] Notes file created with all sections filled

## Implementation Guide

### Step 1: Setup (Estimated: 30min - 1hr)

**What to do:**
1. Create file: `src/path/to/file.rs`
2. Create test file: `tests/path/to/test.rs`
3. Add module declaration in parent

**Code template:**
```rust
// Starter code showing module structure
```

### Step 2: Implement Core Type/Function (Estimated: X-Y hrs)

**What to do:**
1. Define the main struct/enum/trait
2. Implement core methods
3. Add documentation

**Implementation details:**
[Specific guidance on how to implement, with code examples]

**Reference:**
- See `docs/*/relevant.md` Section Y for algorithm details
- CSS Spec Section A.B: [link]

**Example code:**
```rust
// Complete example showing the pattern
pub struct TheType {
    // fields
}

impl TheType {
    pub fn method(&self) -> Result<Output> {
        // implementation pattern
    }
}
```

### Step 3: Write Tests (Estimated: X-Y hrs)

**Required tests:**

1. **Test 1: [Description]**
   ```rust
   #[test]
   fn test_specific_behavior() {
       // test code
   }
   ```

2. **Test 2: [Description]**
   [Test code example]

3. **Test 3: Edge cases**
   - Empty input
   - Null/None values
   - Boundary conditions

### Step 4: Integration (Estimated: X-Y hrs)

**What to do:**
1. Wire into existing system
2. Update factory/registry if needed
3. Add to module exports

**Code changes needed:**
- Update `src/path/mod.rs` to export new type
- Update `src/other/file.rs` to use new type

### Step 5: Documentation (Estimated: 30min - 1hr)

**What to document:**
1. Public struct/enum with `/// Documentation`
2. All public methods
3. Module-level documentation
4. Examples in doc comments

**Example:**
```rust
/// Brief one-line summary
///
/// Longer explanation of what this does and when to use it.
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
pub fn function() -> Result<Output> {
    // implementation
}
```

## Testing Requirements

### Unit Tests

**Required coverage:**
- All public functions
- Edge cases (empty, null, boundary values)
- Error paths
- Common use cases

**Test file:** `tests/path/to/test.rs`

### Integration Tests

**If applicable:**
- Test interaction with other components
- End-to-end scenarios

### Manual Verification

Run these commands:
```bash
cargo test {specific_test_name}
cargo clippy -- -D warnings
cargo fmt --check
```

## Output Artifacts

### Code Files

1. **`src/path/to/file.rs`**
   - Primary implementation
   - All public APIs documented
   - Passes clippy with no warnings

2. **`tests/path/to/test.rs`**
   - Comprehensive test coverage
   - At least N tests covering M scenarios

### Notes File

Create: **`docs/tasks/notes/{task_id}-notes.md`**

Use this template:

```markdown
# Task {task_id} Output Notes

## Implementation Summary

[2-3 paragraph overview of what was built]

## API Contracts

### Public Types

```rust
// Exact type definitions with full signatures
pub struct TypeName {
    pub field: Type,
}
```

### Public Functions

```rust
// Full function signatures
pub fn function_name(args: Types) -> Result<Output>;
```

### Trait Definitions

[If you defined any traits, include full signatures]

## Decisions Made

### Decision 1: [Title]

**Choice:** What was decided
**Rationale:** Why this choice was made
**Alternatives Considered:** What else was considered and why it was rejected
**Impact:** How this affects downstream tasks

### Decision 2: [Title]

[Same structure]

## Spec Interpretations

### Ambiguity 1: [Description]

**Spec says:** [Quote from spec]
**Interpretation:** How we interpreted it
**Reasoning:** Why this interpretation
**Test coverage:** How we verified this works

## Discoveries & Gotchas

### Discovery 1: [Title]

**What:** What was discovered
**Why it matters:** Impact on implementation
**Recommendation:** What downstream tasks should know

### Gotcha 1: [Title]

**Problem:** What doesn't work as expected
**Solution:** How we worked around it
**Warning:** What to watch out for

## Performance Notes

### Benchmark Results

[If applicable, include benchmark numbers]

### Optimization Opportunities

- **Opportunity 1:** [Description and potential impact]
- **Opportunity 2:** [Description and potential impact]

### Performance Considerations

[Any performance implications of design choices]

## Recommendations for Downstream Tasks

### For Task {dependent-task-id}:

- **Recommendation 1:** Specific guidance
- **Recommendation 2:** What to watch for
- **API to use:** `function_name()` for X purpose

### For Task {dependent-task-id}:

[Same structure]

## Open Questions

### Question 1: [Title]

**Question:** What is unresolved
**Context:** Why this matters
**Suggested resolution:** Possible approaches
**Assigned to:** Which downstream task should resolve this

## Test Coverage

### What's Tested

- ✅ Scenario 1
- ✅ Scenario 2
- ✅ Edge case A
- ✅ Edge case B

### What's Not Tested (Gaps)

- ❌ Scenario X (reason: needs integration with Y)
- ❌ Edge case Z (reason: will be covered in task ABC)

### Test Statistics

- Unit tests: N tests, M assertions
- Coverage: X% (if measured)
- All tests pass: ✅

## Code Quality

- ✅ Clippy passes with no warnings
- ✅ Rustfmt applied
- ✅ All public APIs documented
- ✅ Examples in documentation
- ✅ Error handling complete

## References Used

1. CSS Spec Section X.Y - [link]
2. Servo implementation - [link to file]
3. MDN Documentation - [link]
4. Prior art - [link]

---

**Task completed:** YYYY-MM-DD
**Time taken:** X hours
**Tests passing:** N/N
```

## Common Pitfalls to Avoid

### Pitfall 1: Not Reading Dependency Notes

**Wrong:** Start implementing without reading notes from dependencies
**Right:** Carefully read ALL dependency notes files to understand:
- API contracts you must use
- Decisions that affect your implementation
- Recommendations specifically for your task

### Pitfall 2: Deviating from Established Patterns

**Wrong:** Create your own style/patterns
**Right:** Follow patterns established in dependency tasks:
- Use the same error handling approach
- Follow the same module structure
- Match the coding style

### Pitfall 3: Incomplete Notes File

**Wrong:** Write minimal notes like "implemented X"
**Right:** Fill out EVERY section of the notes template:
- Future tasks depend on your notes
- Your decisions affect downstream work
- Your discoveries save others time

### Pitfall 4: Not Testing Edge Cases

**Wrong:** Only test happy path
**Right:** Test edge cases:
- Empty inputs
- Null/None values
- Boundary conditions
- Error conditions

### Pitfall 5: Missing Documentation

**Wrong:** Skip rustdoc comments
**Right:** Document ALL public APIs:
- What it does
- When to use it
- Example usage
- Possible errors
- Edge cases

## Verification Checklist

Before marking task complete, verify:

- [ ] All objectives met
- [ ] All success criteria satisfied
- [ ] Code compiles: `cargo build`
- [ ] Tests pass: `cargo test`
- [ ] Clippy happy: `cargo clippy -- -D warnings`
- [ ] Formatted: `cargo fmt --check`
- [ ] All public APIs have rustdoc
- [ ] Notes file complete with all sections
- [ ] Output files in correct locations
- [ ] Dependencies' recommendations followed

## Time Tracking

Track your time to improve estimates:

- **Planning & reading:** X hours
- **Implementation:** Y hours
- **Testing:** Z hours
- **Documentation:** W hours
- **Total:** N hours (compare to estimate)

## Getting Help

If stuck:

1. **Re-read context files**: `docs/*/relevant.md` usually has answers
2. **Check dependency notes**: Look for recommendations
3. **Review CSS specs**: Link in "Prerequisites" section
4. **Study browser code**: Servo/WebKit examples in `docs/guides/reference-materials.md`
5. **Ask specific questions**: Document in "Open Questions" section

Remember: You're working in isolation. Your notes file is how you communicate with downstream tasks. Be thorough!
```

## Example Tasks

See these example task files:
- `notes/W1.T01-notes.md` - Simple type implementation (completed reference)
- `wave-3/W3.T04-block-layout.md` - Complex algorithm implementation
- `wave-6/W6.T04-wpt-runner.md` - Testing infrastructure

---

**Last Updated:** 2025-01-20
