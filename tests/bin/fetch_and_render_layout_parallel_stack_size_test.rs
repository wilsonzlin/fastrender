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

fn run_fetch_and_render(temp_dir: &Path, url: &str, output_path: &Path, args: &[&str]) {
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
    lowered.contains("layout parallelism:") && lowered.contains("engaged=true"),
    "expected layout parallelism to be engaged; got:\n{combined}"
  );
  assert!(
    !lowered.contains("stack overflow") && !lowered.contains("overflowed its stack"),
    "output should not contain stack overflow indicators:\n{combined}"
  );
}

#[test]
fn fetch_and_render_layout_parallel_workers_use_large_stack() {
  let temp = tempdir().expect("tempdir");
  let html_path = temp.path().join("deep_parallel.html");
  fs::write(&html_path, build_deep_parallel_html(500, 8)).expect("write deep html");

  let url = format!("file://{}", html_path.display());
  let output_path = temp.path().join("out.png");
  run_fetch_and_render(
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
}

#[test]
fn fetch_and_render_layout_parallel_without_max_threads_uses_large_stack() {
  let temp = tempdir().expect("tempdir");
  let html_path = temp.path().join("deep_parallel.html");
  fs::write(&html_path, build_deep_parallel_html(500, 8)).expect("write deep html");

  let url = format!("file://{}", html_path.display());
  let output_path = temp.path().join("out.png");
  run_fetch_and_render(
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
}

