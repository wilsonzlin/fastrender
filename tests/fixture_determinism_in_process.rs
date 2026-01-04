mod r#ref;

use fastrender::image_output::{encode_image, OutputFormat};
use fastrender::style::media::MediaType;
use fastrender::{FastRender, FontConfig, Pixmap, RenderOptions, ResourcePolicy};
use rayon::ThreadPoolBuilder;
use r#ref::image_compare::compare_pngs;
use r#ref::CompareConfig;
use std::fs;
use std::path::{Path, PathBuf};
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
  let options = RenderOptions::new()
    .with_viewport(DEFAULT_VIEWPORT.0, DEFAULT_VIEWPORT.1)
    .with_device_pixel_ratio(DEFAULT_DPR)
    .with_media_type(MediaType::Screen);

  renderer
    .render_html_with_options(html, options)
    .map_err(|e| format!("Render failed: {:?}", e))
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
    .base_url(base_url)
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
        compare_pngs(&label, &rendered_png, &expected_png, compare_config, &output_dir)?;
      }
    } else {
      expected_rgba = Some(pixmap_to_straight_rgba(&rendered));
      expected = Some(rendered);
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
