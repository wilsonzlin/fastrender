use std::fs;
use std::path::Path;
use std::process::Command;
use tempfile::tempdir;

#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;

fn build_deep_parallel_html(depth: usize, siblings: usize) -> String {
  let mut html = String::new();
  html.push_str("<!doctype html><html><head><title>Deep Parallel</title></head><body>");
  for _ in 0..siblings {
    html.push_str(&"<div>".repeat(depth));
    html.push_str(&"</div>".repeat(depth));
  }
  html.push_str("</body></html>");
  html
}

fn build_deep_table_html(depth: usize, rows: usize, cols: usize) -> String {
  let mut html = String::new();
  html.push_str("<!doctype html><html><head><meta charset=\"utf-8\">");
  html.push_str(
    "<style>table{border-collapse:collapse}td{border:1px solid #000;padding:0;margin:0}</style>",
  );
  html.push_str("</head><body><table><tbody>");
  for _ in 0..rows {
    html.push_str("<tr>");
    for _ in 0..cols {
      html.push_str("<td>");
      html.push_str(&"<div>".repeat(depth));
      html.push_str("x");
      html.push_str(&"</div>".repeat(depth));
      html.push_str("</td>");
    }
    html.push_str("</tr>");
  }
  html.push_str("</tbody></table></body></html>");
  html
}

fn build_deep_table_multi_tbody_html(depth: usize, bodies: usize, rows: usize, cols: usize) -> String {
  let mut html = String::new();
  html.push_str("<!doctype html><html><head><meta charset=\"utf-8\">");
  html.push_str(
    "<style>table{border-collapse:collapse}td{border:1px solid #000;padding:0;margin:0}</style>",
  );
  html.push_str("</head><body><table>");
  for _ in 0..bodies {
    html.push_str("<tbody>");
    for _ in 0..rows {
      html.push_str("<tr>");
      for _ in 0..cols {
        html.push_str("<td>");
        html.push_str(&"<div>".repeat(depth));
        html.push_str("x");
        html.push_str(&"</div>".repeat(depth));
        html.push_str("</td>");
      }
      html.push_str("</tr>");
    }
    html.push_str("</tbody>");
  }
  html.push_str("</table></body></html>");
  html
}

fn run_fetch_and_render(temp_dir: &Path, url: &str, output_path: &Path, args: &[&str]) -> String {
  let output = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
    .current_dir(temp_dir)
    .env("FASTR_USE_BUNDLED_FONTS", "1")
    .env("RAYON_NUM_THREADS", "4")
    .args(args)
    .arg(url)
    .arg(output_path.to_str().unwrap())
    .output()
    .expect("run fetch_and_render on deep DOM");

  #[cfg(unix)]
  assert!(
    output.status.signal().is_none(),
    "fetch_and_render exited via signal: {:?}\nstderr:\n{}",
    output.status,
    String::from_utf8_lossy(&output.stderr)
  );
  assert!(
    output.status.success(),
    "fetch_and_render failed: {:?}\nstdout:\n{}\nstderr:\n{}",
    output.status,
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  assert!(output_path.exists(), "expected output image to exist");

  let combined = format!(
    "{}\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  let lowered = combined.to_ascii_lowercase();
  assert!(
    !lowered.contains("stack overflow") && !lowered.contains("overflowed its stack"),
    "output should not contain stack overflow indicators:\n{combined}"
  );
  combined
}

#[test]
fn fetch_and_render_layout_parallel_workers_use_large_stack() {
  let temp = tempdir().expect("tempdir");
  let html_path = temp.path().join("deep_parallel.html");
  fs::write(&html_path, build_deep_parallel_html(500, 8)).expect("write deep html");

  let url = format!("file://{}", html_path.display());
  let output_path = temp.path().join("out.png");
  let output = run_fetch_and_render(
    temp.path(),
    &url,
    &output_path,
    &[
      "--timeout",
      "30",
      "--viewport",
      "64x64",
      "--layout-parallel",
      "on",
      "--layout-parallel-min-fanout",
      "8",
      "--layout-parallel-max-threads",
      "2",
    ],
  );
  let lowered = output.to_ascii_lowercase();
  assert!(
    lowered.contains("layout parallelism:") && lowered.contains("engaged=true"),
    "expected layout parallelism to be engaged; got:\n{output}"
  );
}

#[test]
fn fetch_and_render_layout_parallel_without_max_threads_uses_large_stack() {
  let temp = tempdir().expect("tempdir");
  let html_path = temp.path().join("deep_parallel.html");
  fs::write(&html_path, build_deep_parallel_html(500, 8)).expect("write deep html");

  let url = format!("file://{}", html_path.display());
  let output_path = temp.path().join("out.png");
  let output = run_fetch_and_render(
    temp.path(),
    &url,
    &output_path,
    &[
      "--timeout",
      "30",
      "--viewport",
      "64x64",
      "--layout-parallel",
      "on",
      "--layout-parallel-min-fanout",
      "8",
    ],
  );
  let lowered = output.to_ascii_lowercase();
  assert!(
    lowered.contains("layout parallelism:") && lowered.contains("engaged=true"),
    "expected layout parallelism to be engaged; got:\n{output}"
  );
}

#[test]
fn fetch_and_render_layout_parallel_table_cell_workers_use_large_stack() {
  // Table layout can spawn Rayon work for cell intrinsic measurement even when the box tree doesn't
  // have enough sibling fan-out to engage the main layout-parallel fan-out heuristics. Ensure we
  // still run layout inside the large-stack dedicated pool so those Rayon worker threads don't
  // overflow their stacks on deep content.
  let temp = tempdir().expect("tempdir");
  let html_path = temp.path().join("deep_table.html");
  fs::write(&html_path, build_deep_table_html(300, 4, 4)).expect("write deep html");

  let url = format!("file://{}", html_path.display());
  let output_path = temp.path().join("out.png");
  let output = run_fetch_and_render(
    temp.path(),
    &url,
    &output_path,
    &[
      "--timeout",
      "30",
      "--viewport",
      "64x64",
      "--layout-parallel",
      "on",
      "--layout-parallel-min-fanout",
      "8",
      "--layout-parallel-max-threads",
      "2",
    ],
  );
  let lowered = output.to_ascii_lowercase();
  assert!(
    lowered.contains("layout parallelism:") && lowered.contains("mode=enabled"),
    "expected layout parallelism diagnostics; got:\n{output}"
  );
  assert!(
    lowered.contains("engaged=false"),
    "expected layout fan-out to be disengaged (table-only parallelism); got:\n{output}"
  );
}

#[test]
fn fetch_and_render_layout_parallel_auto_table_cell_workers_use_large_stack() {
  // Layout parallelism in auto mode can decide to skip the main sibling fan-out heuristics when the
  // box tree doesn't meet the configured `min_fanout`. Table layout still opportunistically uses
  // Rayon to measure intrinsic sizes for large tables. Ensure those measurements run inside the
  // large-stack layout thread pool so we don't overflow the default Rayon stack.
  let temp = tempdir().expect("tempdir");
  let html_path = temp.path().join("deep_table_auto.html");
  fs::write(&html_path, build_deep_table_multi_tbody_html(200, 6, 7, 7)).expect("write deep html");

  let url = format!("file://{}", html_path.display());
  let output_path = temp.path().join("out.png");
  let output = run_fetch_and_render(
    temp.path(),
    &url,
    &output_path,
    &[
      "--timeout",
      "30",
      "--viewport",
      "64x64",
      "--layout-parallel",
      "auto",
      "--layout-parallel-min-fanout",
      "8",
    ],
  );
  let lowered = output.to_ascii_lowercase();
  assert!(
    lowered.contains("layout parallelism:") && lowered.contains("mode=auto"),
    "expected layout parallelism diagnostics; got:\n{output}"
  );
  assert!(
    lowered.contains("work_items=7"),
    "expected auto fan-out workload to stay below the min_fanout threshold; got:\n{output}"
  );
  assert!(
    lowered.contains("engaged=false"),
    "expected layout fan-out to be disengaged (table-only parallelism); got:\n{output}"
  );
}
