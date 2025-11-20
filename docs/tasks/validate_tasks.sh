#!/bin/bash
# Task Validation Script for FastRender V2
# Validates that all task files have required structure and content

set -e

TASKS_DIR="$(cd "$(dirname "$0")" && pwd)"
ERRORS=0

echo "🔍 Validating FastRender V2 task files..."
echo ""

# Count tasks
TOTAL_TASKS=$(find "$TASKS_DIR" -name "W*.md" -type f | wc -l)
echo "📊 Found $TOTAL_TASKS task files"
echo ""

# Check each wave
for wave in {1..6}; do
    wave_dir="$TASKS_DIR/wave-$wave"
    if [ ! -d "$wave_dir" ]; then
        echo "❌ Wave $wave directory missing!"
        ((ERRORS++))
        continue
    fi

    count=$(ls -1 "$wave_dir"/W*.md 2>/dev/null | wc -l)
    echo "Wave $wave: $count tasks"
done

echo ""
echo "🔍 Validating task file structure..."
echo ""

# Validate each task file
for task_file in $(find "$TASKS_DIR" -name "W*.md" -type f | sort); do
    filename=$(basename "$task_file")

    # Check frontmatter
    if ! grep -q "^---$" "$task_file"; then
        echo "❌ $filename: Missing YAML frontmatter"
        ((ERRORS++))
        continue
    fi

    # Check required sections
    required_sections=(
        "## Context"
        "## Prerequisites"
        "## Objectives"
        "## Implementation Guide"
        "## Output Artifacts"
        "## Verification Checklist"
    )

    for section in "${required_sections[@]}"; do
        if ! grep -q "$section" "$task_file"; then
            echo "⚠️  $filename: Missing section '$section'"
            ((ERRORS++))
        fi
    done

    # Check task_id in frontmatter
    if ! grep -q "^task_id:" "$task_file"; then
        echo "❌ $filename: Missing task_id in frontmatter"
        ((ERRORS++))
    fi
done

echo ""
if [ $ERRORS -eq 0 ]; then
    echo "✅ All task files validated successfully!"
    echo ""
    echo "📈 Summary:"
    echo "   - Total tasks: $TOTAL_TASKS"
    echo "   - All required sections present"
    echo "   - All frontmatter valid"
    echo ""
    echo "🚀 Ready for task execution!"
    exit 0
else
    echo "❌ Validation failed with $ERRORS errors"
    exit 1
fi
