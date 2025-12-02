#!/usr/bin/env python3
"""
Fix test assertions in text_pipeline.rs to use require_fonts! macro.
"""
import re
from pathlib import Path

def fix_file():
    filepath = Path('/workspace/tests/text_pipeline.rs')
    content = filepath.read_text()
    
    # Pattern 1: assert!(result.is_ok()); ... let runs/width = result.unwrap();
    # Replace with: let runs/width = require_fonts!(result);
    
    # Fix pattern: assert!(result.is_ok());\n\n    let X = result.unwrap();
    content = re.sub(
        r'assert!\(result\.is_ok\(\)\);\n\n(\s+let \w+ = )result\.unwrap\(\);',
        r'\1require_fonts!(result);',
        content
    )
    
    # Fix pattern: result.unwrap(); where preceded by assert on result
    # But be careful - some unwraps are fine after checks
    
    filepath.write_text(content)
    print("Fixed test assertions")

if __name__ == '__main__':
    fix_file()
