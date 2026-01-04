mod r#ref;

use fastrender::image_output::{encode_image, OutputFormat};
use fastrender::style::media::MediaType;
use fastrender::{
  snapshot_pipeline, FastRender, FontConfig, Pixmap, PipelineSnapshot, RenderArtifactRequest,
  RenderDiagnostics, RenderOptions, ResourcePolicy,
};
use rayon::ThreadPoolBuilder;
use r#ref::image_compare::compare_pngs;
use r#ref::CompareConfig;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use url::Url;

struct Fixture<'a> {
  name: &'a str,
  html: &'a str,
}

const FIXTURES: &[Fixture<'static>] = &[
  Fixture {
    name: "preserve_3d_stack",
    html: "preserve_3d_stack/index.html",
  },
  Fixture {
    name: "filter_backdrop_scene",
    html: "filter_backdrop_scene/index.html",
  },
];

const DEFAULT_VIEWPORT: (u32, u32) = (1040, 1240);
const DEFAULT_DPR: f32 = 1.0;

// Intentionally small but high-signal run schedule:
// - multiple renders within the same process to catch cache/uninitialized-data nondeterminism
// - a mix of 1-thread (serial) and 4-thread (parallel) rayon pools to catch scheduling issues.
const RENDER_SCHEDULE: &[usize] = &[4, 4, 1];

fn fixtures_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/pages/fixtures")
}

fn diff_dir_for_fixture(name: &str) -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("target/determinism_diffs/in_process")
    .join(name)
}

fn base_url_for(html_path: &Path) -> Result<String, String> {
  let dir = html_path
    .parent()
    .ok_or_else(|| format!("No parent directory for {}", html_path.display()))?;
  Url::from_directory_path(dir)
    .map_err(|_| format!("Failed to build file:// base URL for {}", dir.display()))
    .map(|url| url.to_string())
}

fn render_pixmap(renderer: &mut FastRender, html: &str) -> Result<Pixmap, String> {
  let options = render_options();

  renderer
    .render_html_with_options(html, options)
    .map_err(|e| format!("Render failed: {:?}", e))
}

fn render_options() -> RenderOptions {
  RenderOptions::new()
    .with_viewport(DEFAULT_VIEWPORT.0, DEFAULT_VIEWPORT.1)
    .with_device_pixel_ratio(DEFAULT_DPR)
    .with_media_type(MediaType::Screen)
}

fn capture_snapshot(
  renderer: &mut FastRender,
  html: &str,
  base_url: &str,
  options: RenderOptions,
) -> Result<(PipelineSnapshot, RenderDiagnostics), String> {
  let report = renderer
    .render_html_with_stylesheets_report(html, base_url, options, RenderArtifactRequest::summary())
    .map_err(|e| format!("Render with artifacts failed: {:?}", e))?;

  let dom = report
    .artifacts
    .dom
    .as_ref()
    .ok_or_else(|| "missing DOM artifact".to_string())?;
  let styled = report
    .artifacts
    .styled_tree
    .as_ref()
    .ok_or_else(|| "missing styled tree artifact".to_string())?;
  let box_tree = report
    .artifacts
    .box_tree
    .as_ref()
    .ok_or_else(|| "missing box tree artifact".to_string())?;
  let fragment_tree = report
    .artifacts
    .fragment_tree
    .as_ref()
    .ok_or_else(|| "missing fragment tree artifact".to_string())?;
  let display_list = report
    .artifacts
    .display_list
    .as_ref()
    .ok_or_else(|| "missing display list artifact".to_string())?;

  Ok((
    snapshot_pipeline(dom, styled, box_tree, fragment_tree, display_list),
    report.diagnostics,
  ))
}

fn write_json_pretty(path: &Path, value: &impl serde::Serialize) -> Result<(), String> {
  let json =
    serde_json::to_string_pretty(value).map_err(|e| format!("serialize {}: {e}", path.display()))?;
  fs::write(path, json).map_err(|e| format!("write {}: {e}", path.display()))
}

fn run_diff_snapshots(before_dir: &Path, after_dir: &Path, out_dir: &Path) -> Result<(), String> {
  let json_path = out_dir.join("diff_snapshots.json");
  let html_path = out_dir.join("diff_snapshots.html");
  let status = Command::new(env!("CARGO_BIN_EXE_diff_snapshots"))
    .current_dir(PathBuf::from(env!("CARGO_MANIFEST_DIR")))
    .args([
      "--before",
      before_dir.to_str().ok_or_else(|| "before dir not utf-8".to_string())?,
      "--after",
      after_dir.to_str().ok_or_else(|| "after dir not utf-8".to_string())?,
      "--json",
      json_path.to_str().ok_or_else(|| "json path not utf-8".to_string())?,
      "--html",
      html_path.to_str().ok_or_else(|| "html path not utf-8".to_string())?,
    ])
    .status()
    .map_err(|e| format!("spawn diff_snapshots: {e}"))?;

  if status.success() {
    return Ok(());
  }

  Err(format!("diff_snapshots failed with status {status}"))
}

fn pixmap_to_straight_rgba(pixmap: &Pixmap) -> Vec<u8> {
  let mut rgba = Vec::with_capacity(pixmap.data().len());
  for chunk in pixmap.data().chunks_exact(4) {
    let r = chunk[0];
    let g = chunk[1];
    let b = chunk[2];
    let a = chunk[3];

    // Convert premultiplied RGBA (tiny-skia) to straight RGBA for stable, byte-for-byte comparisons
    // that match the bytes written by `encode_image(OutputFormat::Png)`.
    let (r, g, b) = if a > 0 {
      let alpha = a as f32 / 255.0;
      (
        ((r as f32 / alpha).min(255.0)) as u8,
        ((g as f32 / alpha).min(255.0)) as u8,
        ((b as f32 / alpha).min(255.0)) as u8,
      )
    } else {
      (0, 0, 0)
    };

    rgba.extend_from_slice(&[r, g, b, a]);
  }
  rgba
}

fn pixmap_matches_straight_rgba(pixmap: &Pixmap, expected_rgba: &[u8]) -> bool {
  if pixmap.data().len() != expected_rgba.len() {
    return false;
  }

  for (expected, chunk) in expected_rgba.chunks_exact(4).zip(pixmap.data().chunks_exact(4)) {
    let r = chunk[0];
    let g = chunk[1];
    let b = chunk[2];
    let a = chunk[3];
    let (r, g, b) = if a > 0 {
      let alpha = a as f32 / 255.0;
      (
        ((r as f32 / alpha).min(255.0)) as u8,
        ((g as f32 / alpha).min(255.0)) as u8,
        ((b as f32 / alpha).min(255.0)) as u8,
      )
    } else {
      (0, 0, 0)
    };

    if expected[0] != r || expected[1] != g || expected[2] != b || expected[3] != a {
      return false;
    }
  }

  true
}

fn run_fixture(fixture: &Fixture<'_>, compare_config: &CompareConfig) -> Result<(), String> {
  let html_path = fixtures_dir().join(fixture.html);
  let html = fs::read_to_string(&html_path)
    .map_err(|e| format!("Failed to read {}: {}", html_path.display(), e))?;
  let base_url = base_url_for(&html_path)?;

  let policy = ResourcePolicy::default()
    .allow_http(false)
    .allow_https(false)
    .allow_file(true)
    .allow_data(true);

  let mut renderer = FastRender::builder()
    .base_url(base_url.clone())
    .font_sources(FontConfig::bundled_only())
    .resource_policy(policy)
    .build()
    .map_err(|e| format!("Failed to create renderer: {:?}", e))?;

  let pool_1 = ThreadPoolBuilder::new()
    .num_threads(1)
    .build()
    .map_err(|e| format!("Failed to create 1-thread pool: {e}"))?;
  let pool_4 = ThreadPoolBuilder::new()
    .num_threads(4)
    .build()
    .map_err(|e| format!("Failed to create 4-thread pool: {e}"))?;

  let mut expected: Option<Pixmap> = None;
  let mut expected_rgba: Option<Vec<u8>> = None;
  let mut expected_threads: Option<usize> = None;
  let output_dir = diff_dir_for_fixture(fixture.name);

  for (idx, &threads) in RENDER_SCHEDULE.iter().enumerate() {
    let pool = match threads {
      1 => &pool_1,
      4 => &pool_4,
      other => return Err(format!("Unsupported thread pool size {other} in schedule")),
    };

    let rendered = pool.install(|| render_pixmap(&mut renderer, &html))?;

    if let (Some(expected_pixmap), Some(expected_rgba)) = (expected.as_ref(), expected_rgba.as_ref())
    {
      if !pixmap_matches_straight_rgba(&rendered, expected_rgba) {
        let label = format!("run_{idx}_threads_{threads}");
        let expected_png =
          encode_image(expected_pixmap, OutputFormat::Png).map_err(|e| format!("{e:?}"))?;
        let rendered_png =
          encode_image(&rendered, OutputFormat::Png).map_err(|e| format!("{e:?}"))?;
        let mut message =
          compare_pngs(&label, &rendered_png, &expected_png, compare_config, &output_dir)
            .unwrap_err();

        // If the pixel diff is due to nondeterminism, make it actionable by capturing pipeline
        // snapshots (DOM/styled/box/fragment/display-list) for both variants and running
        // `diff_snapshots` to produce a stage-level report.
        let snapshot_root = output_dir.join("snapshots");
        let before_dir = snapshot_root.join("run1").join(fixture.name);
        let after_dir = snapshot_root.join("run2").join(fixture.name);

        let mut snapshot_error = None::<String>;
        let expected_threads = expected_threads.unwrap_or(RENDER_SCHEDULE[0]);
        let expected_pool = match expected_threads {
          1 => &pool_1,
          4 => &pool_4,
          other => return Err(format!("Unsupported expected thread pool size {other}")),
        };

        let before_capture = expected_pool.install(|| {
          capture_snapshot(&mut renderer, &html, &base_url, render_options())
        });
        let after_capture =
          pool.install(|| capture_snapshot(&mut renderer, &html, &base_url, render_options()));

        match (before_capture, after_capture) {
          (Ok((before_snapshot, before_diag)), Ok((after_snapshot, after_diag))) => {
            fs::create_dir_all(&before_dir)
              .map_err(|e| format!("create {}: {e}", before_dir.display()))?;
            fs::create_dir_all(&after_dir)
              .map_err(|e| format!("create {}: {e}", after_dir.display()))?;

            if let Err(err) = write_json_pretty(&before_dir.join("snapshot.json"), &before_snapshot)
            {
              snapshot_error = Some(err);
            } else if let Err(err) =
              write_json_pretty(&after_dir.join("snapshot.json"), &after_snapshot)
            {
              snapshot_error = Some(err);
            } else if let Err(err) =
              write_json_pretty(&before_dir.join("diagnostics.json"), &before_diag)
            {
              snapshot_error = Some(err);
            } else if let Err(err) =
              write_json_pretty(&after_dir.join("diagnostics.json"), &after_diag)
            {
              snapshot_error = Some(err);
            } else if let Err(err) = fs::write(before_dir.join("render.png"), &expected_png)
              .map_err(|e| format!("write render.png: {e}"))
            {
              snapshot_error = Some(err);
            } else if let Err(err) = fs::write(after_dir.join("render.png"), &rendered_png)
              .map_err(|e| format!("write render.png: {e}"))
            {
              snapshot_error = Some(err);
            } else if let Err(err) = run_diff_snapshots(&before_dir, &after_dir, &output_dir) {
              snapshot_error = Some(err);
            }
          }
          (Err(err), _) => snapshot_error = Some(err),
          (_, Err(err)) => snapshot_error = Some(err),
        }

        message.push_str("\n\nSnapshot artifacts:");
        message.push_str(&format!("\n  before: {}", before_dir.display()));
        message.push_str(&format!("\n  after:  {}", after_dir.display()));
        message.push_str(&format!(
          "\n  diff_snapshots: {}",
          output_dir.join("diff_snapshots.html").display()
        ));
        if let Some(err) = snapshot_error {
          message.push_str(&format!("\n\nSnapshot capture failed:\n{err}"));
        }

        return Err(message);
      }
    } else {
      expected_rgba = Some(pixmap_to_straight_rgba(&rendered));
      expected = Some(rendered);
      expected_threads = Some(threads);
    }
  }

  Ok(())
}

#[test]
fn fixture_determinism_in_process() {
  let compare_config = CompareConfig::strict();
  for fixture in FIXTURES {
    run_fixture(fixture, &compare_config)
      .unwrap_or_else(|e| panic!("Fixture '{}' failed determinism check: {}", fixture.name, e));
  }
}
