//! Guard against committing unresolved merge-conflict markers.

use std::fs;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

const MERGE_MARKERS: [&str; 3] = [
  concat!("<<<", "<<", "<<"),
  concat!("===", "==", "=="),
  concat!(">>>", ">>>", ">"),
];

#[test]
fn no_merge_conflict_markers_present() {
  let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let search_roots = [
    repo_root.join("src"),
    repo_root.join("tests"),
    repo_root.join("benches"),
    repo_root.join("fuzz"),
  ];

  let mut offenders = Vec::new();

  for root in search_roots {
    if !root.exists() {
      continue;
    }

    for path in rust_files(&root) {
      let rel_path = path
        .strip_prefix(&repo_root)
        .map(|p| p.to_path_buf())
        .unwrap_or(path.clone());

      let file = fs::File::open(&path).unwrap_or_else(|err| {
        panic!("failed to open {}: {}", rel_path.display(), err);
      });

      for (line_idx, line) in BufReader::new(file).lines().enumerate() {
        let line_number = line_idx + 1;
        let line = line.unwrap_or_else(|err| {
          panic!("failed to read {}: {}", rel_path.display(), err);
        });

        for marker in MERGE_MARKERS {
          if line.trim_start().starts_with(marker) {
            offenders.push(format!(
              "{}:{line_number}: contains merge-conflict marker {marker}",
              rel_path.display()
            ));
          }
        }
      }
    }
  }

  if !offenders.is_empty() {
    panic!(
      "found merge-conflict markers in Rust sources:\n{}",
      offenders.join("\n")
    );
  }
}

fn rust_files(root: &Path) -> Vec<PathBuf> {
  let mut files = Vec::new();
  let mut stack = vec![root.to_path_buf()];

  while let Some(dir) = stack.pop() {
    let entries = fs::read_dir(&dir).unwrap_or_else(|err| {
      panic!("failed to read directory {}: {}", dir.display(), err);
    });

    for entry in entries {
      let entry = entry.unwrap_or_else(|err| {
        panic!("failed to read entry under {}: {}", dir.display(), err);
      });
      let path = entry.path();

      if path.is_dir() {
        stack.push(path);
      } else if path.extension().map(|ext| ext == "rs").unwrap_or(false) {
        files.push(path);
      }
    }
  }

  files.sort();
  files
}
