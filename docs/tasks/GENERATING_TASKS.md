# Generating All 80 Task Files

This guide explains how to generate the complete set of 80 task files for FastRender V2.

## Overview

We have created:
- ✅ **TASK_GRAPH.md** - Complete dependency graph defining all 80 tasks
- ✅ **TASK_TEMPLATE.md** - Standard template for task files
- ✅ **10 example tasks** - Showing different patterns
- ✅ **README.md** - Complete system documentation

**Still needed**: Generate the remaining 70 task files.

## Generation Approaches

### Approach 1: Manual Creation (Recommended for Quality)

Create each task file manually using the template, referencing:

1. **TASK_GRAPH.md** for task definition
2. **docs/*/*.md** for implementation details
3. **Example tasks** for patterns

**Pros:**
- Highest quality
- Each task is carefully thought through
- Domain knowledge embedded in task

**Cons:**
- Time-intensive (10-15 minutes per task)
- Requires deep understanding

**Estimate:** 70 tasks × 15 min = ~18 hours

### Approach 2: AI Agent Generation (Faster, Good Quality)

Use an AI agent to generate task files based on:

1. **TASK_TEMPLATE.md** as structure
2. **TASK_GRAPH.md** for task metadata
3. **docs/*/*.md** for implementation details
4. **Example tasks** as reference

**Prompt template:**

```
Create a comprehensive task file for FastRender V2 following this specification:

TASK DEFINITION (from TASK_GRAPH.md):
- Task ID: {task_id}
- Title: {title}
- Wave: {wave}
- Hours: {hours}
- Depends on: {dependencies}
- Context: {description}

TEMPLATE:
[Include TASK_TEMPLATE.md]

REFERENCE IMPLEMENTATION:
[Include relevant section from docs/*/*.md]

EXAMPLE TASKS:
[Include 1-2 similar example tasks]

OUTPUT:
A complete, self-contained task file that an isolated agent can execute.
Must include:
- Full YAML frontmatter
- Complete context and prerequisites
- Step-by-step implementation with code examples
- Comprehensive test cases
- Notes file template
- Verification checklist

Make it 3000-5000 words with detailed code examples.
```

**Pros:**
- Fast (can generate 10-20 per hour with agent)
- Consistent quality
- Based on detailed plan docs

**Cons:**
- Requires review
- May miss domain nuances

**Estimate:** 70 tasks × 3 min = ~3.5 hours (plus review time)

### Approach 3: Hybrid (Best Balance)

1. **Generate skeleton** with AI agent (1-2 hours)
2. **Review and enhance** manually (5-10 hours)
3. **Validate** against plan docs (2-3 hours)

**Total estimate:** 8-15 hours for all 70 tasks

## Task Generation Priority

Generate tasks in this order:

### Priority 1: Critical Path (12 tasks)

These tasks form the critical path and block the most other work:

1. **W1.T01** - Geometry types ✅ (already created)
2. **W2.T01** - BoxNode type ✅ (already created)
3. **W2.T07** - FormattingContext trait ✅ (already created)
4. **W3.T04** - Block layout ✅ (already created)
5. **W3.T14** - Font database ✅ (already created)
6. **W4.T12** - Inline layout ✅ (already created)
7. **W5.T02** - Display list builder ✅ (already created)
8. **W6.T02** - Rendering pipeline ✅ (already created)
9. **W3.T06** - Table layout (CRITICAL - fixes V1 flaw)
10. **W4.T05** - Text shaping pipeline
11. **W5.T04** - Paint order sorter
12. **W6.T01** - FC factory integration

### Priority 2: Wave 1 Foundation (10 remaining tasks)

Complete Wave 1 so Wave 2 can start:

1. **W1.R02** - Table layout spec research
2. **W1.R03** - Unicode Bidi & Line Breaking research
3. **W1.R04** - Servo layout study
4. **W1.R05** - Stacking context research
5. **W1.T02** - Error types
6. **W1.T04** - Color types
7. **W1.T05** - Display enum
8. **W1.T06** - Position enum
9. **W1.T07** - Project structure
10. **W1.T08** - Development tools

### Priority 3: Wave 2 Core Architecture (10 remaining tasks)

1. **W2.T02** - BoxType enum
2. **W2.T03** - FragmentNode type
3. **W2.T04** - LayoutConstraints
4. **W2.T05** - ComputedStyle struct
5. **W2.T06** - DebugInfo types
6. **W2.T08** - FormattingContextType enum
7. **W2.T09** - FC Factory structure
8. **W2.T10** - LayoutEngine struct
9. **W2.T11** - Box generator structure
10. **W2.T12** - Debug visualization tools

### Priority 4: Remaining Tasks (48 tasks)

Generate in wave order (Wave 3 → Wave 4 → Wave 5 → Wave 6)

## Task File Checklist

For each generated task file, verify:

### Structure
- [ ] Valid YAML frontmatter
- [ ] All required fields present
- [ ] task_id follows format (WX.TYY)
- [ ] depends_on lists are correct
- [ ] inputs/outputs are complete

### Content
- [ ] Context section explains what/why/how
- [ ] Prerequisites list required knowledge
- [ ] Implementation guide has 4-6 steps
- [ ] Code examples are complete and correct
- [ ] Test cases are comprehensive (10-20 tests)
- [ ] Notes template is included
- [ ] Verification checklist is present

### Quality
- [ ] Self-contained (no external context needed)
- [ ] References correct plan docs
- [ ] Code compiles (syntax correct)
- [ ] Matches established patterns
- [ ] 3000-5000 words in length
- [ ] Actionable and clear

## Batch Generation Script

Here's a skeleton script for batch generation:

```python
#!/usr/bin/env python3
"""
Generate FastRender V2 task files from TASK_GRAPH.md
"""

import yaml
from pathlib import Path

TASK_DEFINITIONS = [
    # Wave 1
    {
        "task_id": "W1.T02",
        "title": "Implement Error Types",
        "wave": 1,
        "hours": "3-4",
        "depends_on": [],
        "plan_doc": "docs/core/type-system.md",
        "plan_section": "Error Types",
        "description": "Create comprehensive error type hierarchy...",
    },
    # ... more tasks
]

TEMPLATE = Path("TASK_TEMPLATE.md").read_text()

def generate_task_file(task_def):
    """Generate a single task file"""

    # Read relevant plan doc section
    plan_content = extract_plan_section(
        task_def["plan_doc"],
        task_def["plan_section"]
    )

    # Generate using AI or template
    content = generate_content(task_def, plan_content)

    # Write to file
    wave_dir = Path(f"wave-{task_def['wave']}")
    wave_dir.mkdir(exist_ok=True)

    file_path = wave_dir / f"{task_def['task_id']}-{slugify(task_def['title'])}.md"
    file_path.write_text(content)

    print(f"✅ Created {file_path}")

def generate_content(task_def, plan_content):
    """Generate task file content"""

    # This would call AI agent or use template substitution
    # For now, return template with placeholders filled

    return f"""---
task_id: "{task_def['task_id']}"
title: "{task_def['title']}"
wave: {task_def['wave']}
estimated_hours: {task_def['hours']}
depends_on: {task_def['depends_on']}
...
---

# {task_def['title']}

## Context

{task_def['description']}

[Rest of template filled in...]

## Implementation Guide

Based on {task_def['plan_doc']}:

{plan_content}

[Step-by-step guide...]
"""

if __name__ == "__main__":
    for task_def in TASK_DEFINITIONS:
        generate_task_file(task_def)

    print(f"\n✅ Generated {len(TASK_DEFINITIONS)} task files")
```

## Quality Assurance

After generation, perform QA:

### Automated Checks

```bash
#!/bin/bash
# validate_tasks.sh

echo "Validating task files..."

# Check YAML frontmatter
for file in wave-*/*.md; do
    echo "Checking $file..."

    # Extract frontmatter
    frontmatter=$(sed -n '/^---$/,/^---$/p' "$file")

    # Validate YAML
    echo "$frontmatter" | python3 -c "import yaml, sys; yaml.safe_load(sys.stdin)" || {
        echo "❌ Invalid YAML in $file"
        exit 1
    }
done

# Check for required sections
required_sections=(
    "## Context"
    "## Prerequisites"
    "## Objectives"
    "## Implementation Guide"
    "## Testing Requirements"
    "## Output Artifacts"
)

for file in wave-*/*.md; do
    for section in "${required_sections[@]}"; do
        grep -q "$section" "$file" || {
            echo "❌ Missing $section in $file"
            exit 1
        }
    done
done

echo "✅ All tasks validated"
```

### Manual Review

For each task, review:

1. **Accuracy**: Does it match TASK_GRAPH.md definition?
2. **Completeness**: Are all sections filled?
3. **Clarity**: Can an isolated agent understand this?
4. **Code quality**: Are examples correct and compilable?
5. **Dependencies**: Are inputs correctly listed?

## Current Status

**Created:** 10 example tasks (Wave 1-6 samples)

**Remaining:** 70 tasks to generate

**Next Steps:**

1. Choose generation approach (hybrid recommended)
2. Generate Priority 1 tasks first (critical path)
3. Complete Wave 1 (enable Wave 2 to start)
4. Generate remaining waves sequentially
5. QA and validate all tasks
6. Begin execution!

## Recommended Timeline

**For full generation:**

| Activity | Time | Output |
|----------|------|--------|
| Generate Priority 1 (4 tasks) | 1 hour | Critical path unblocked |
| Generate Wave 1 (10 tasks) | 2-3 hours | Wave 1 complete |
| Generate Wave 2 (10 tasks) | 2-3 hours | Wave 2 complete |
| Generate Waves 3-6 (46 tasks) | 8-10 hours | All tasks generated |
| QA and validation | 3-4 hours | All tasks validated |
| **Total** | **16-21 hours** | **80 tasks ready** |

With AI assistance and parallel work, this can be compressed to 1-2 days.

## Getting Started

**To generate all tasks now:**

1. **Option A: Manual (highest quality)**
   ```bash
   # Create each task file using TASK_TEMPLATE.md
   # Reference TASK_GRAPH.md for definitions
   # Reference docs/*/*.md for details
   # Use example tasks as reference
   ```

2. **Option B: AI-assisted (faster)**
   ```
   # Use the prompt template above with an AI agent
   # Generate 5-10 tasks at a time
   # Review and refine
   # Validate with scripts
   ```

3. **Option C: Hybrid (recommended)**
   ```
   # Generate skeleton with AI
   # Enhance manually
   # Validate thoroughly
   ```

**Start with Priority 1 tasks** to unblock the critical path!

---

**Last Updated:** 2025-01-20
**Tasks Created:** 10/80
**Tasks Remaining:** 70
**Estimated Completion:** 16-21 hours
