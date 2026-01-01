use clap::{ArgAction, Parser, ValueEnum};
use fastrender::api::{DiagnosticsLevel, FastRender, RenderOptions};
use fastrender::image_output::OutputFormat;
use fastrender::style::media::MediaType;
use fastrender::{FontConfig, ResourcePolicy};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use url::Url;

#[derive(Parser)]
#[command(about = "Offline perf smoke test for a small set of fixtures")]
struct Args {
  /// Which fixture suite to run (core/pageset-timeouts/all)
  #[arg(long, value_enum, default_value_t = Suite::Core)]
  suite: Suite,

  /// Only run fixtures matching these names (comma-separated)
  #[arg(long, value_delimiter = ',')]
  only: Option<Vec<String>>,

  /// Backwards-compatible alias for --only
  #[arg(long, value_delimiter = ',', hide = true)]
  fixtures: Option<Vec<String>>,

  /// Run each fixture in its own process to keep memory bounded
  #[arg(long, action = ArgAction::SetTrue)]
  isolate: bool,

  /// Disable per-fixture isolation (used by the orchestrating parent process)
  #[arg(long, action = ArgAction::SetTrue, hide = true)]
  no_isolate: bool,

  /// Print the slowest N fixtures to stderr
  #[arg(long)]
  top: Option<usize>,

  /// Write JSON summary to this path (always printed to stdout)
  #[arg(long, default_value = "target/perf_smoke.json")]
  output: PathBuf,

  /// Optional baseline JSON to compare against
  #[arg(long)]
  baseline: Option<PathBuf>,

  /// Relative regression threshold (0.05 = 5%)
  #[arg(long, default_value_t = 0.05)]
  threshold: f64,

  /// Exit with a non-zero status when any stage exceeds the regression threshold
  #[arg(long)]
  fail_on_regression: bool,

  /// Exit with a non-zero status when a pageset-timeouts manifest fixture is missing locally.
  ///
  /// Without this flag, missing pageset-timeouts fixtures are skipped so local runs can operate on
  /// a subset of captured pages.
  #[arg(long)]
  fail_on_missing_fixtures: bool,

  /// Exit with a non-zero status when any fixture exceeds its `budget_ms` (if provided).
  ///
  /// Budget checks are independent of baseline regression checking.
  #[arg(long)]
  fail_on_budget: bool,
}

#[derive(Clone, Copy, ValueEnum)]
enum Suite {
  Core,
  PagesetTimeouts,
  All,
}

#[derive(Clone)]
struct FixtureSpec {
  name: String,
  path: String,
  html_path: PathBuf,
  viewport: (u32, u32),
  dpr: f32,
  media: MediaType,
  budget_ms: Option<f64>,
}

type CoreFixture = (&'static str, &'static str, (u32, u32), f32, MediaType);

const CORE_FIXTURES: &[CoreFixture] = &[
  (
    "flex_dashboard",
    "tests/pages/fixtures/flex_dashboard/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "grid_news",
    "tests/pages/fixtures/grid_news/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "multicol_article",
    "tests/pages/fixtures/multicol_article/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "table_financial",
    "tests/pages/fixtures/table_financial/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "subgrid_showcase",
    "tests/pages/fixtures/subgrid_showcase/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "subgrid_alignment",
    "tests/pages/fixtures/subgrid_alignment/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "subgrid_writing_mode_gap",
    "tests/pages/fixtures/subgrid_writing_mode_gap/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "subgrid_vertical_inheritance",
    "tests/pages/fixtures/subgrid_vertical_inheritance/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "subgrid_vertical_stack",
    "tests/pages/fixtures/subgrid_vertical_stack/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "subgrid_nested_axes",
    "tests/pages/fixtures/subgrid_nested_axes/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "mask_filter_showcase",
    "tests/pages/fixtures/mask_filter_showcase/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "svg_embed",
    "tests/pages/fixtures/svg_embed/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "writing_modes",
    "tests/pages/fixtures/writing_modes/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "form_controls",
    "tests/pages/fixtures/form_controls/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "positioned_badge_regression",
    "tests/pages/fixtures/positioned_badge_regression/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "dom_parse_stress",
    "tests/pages/fixtures/dom_parse_stress/index.html",
    (1040, 1240),
    1.0,
    MediaType::Screen,
  ),
  (
    "paginated_report_print",
    "tests/pages/fixtures/paginated_report/index.html",
    (920, 1180),
    1.0,
    MediaType::Print,
  ),
  (
    "fragmentation_showcase_print",
    "tests/pages/fixtures/fragmentation_showcase/index.html",
    (920, 1180),
    1.0,
    MediaType::Print,
  ),
  (
    "running_elements_print",
    "tests/pages/fixtures/running_elements/index.html",
    (920, 1180),
    1.0,
    MediaType::Print,
  ),
];

const PERF_SMOKE_SCHEMA_VERSION: u32 = 4;
const PAGESET_TIMEOUT_MANIFEST_VERSION: u32 = 1;
const PAGESET_TIMEOUT_MANIFEST: &str = include_str!("../../tests/pages/pageset_timeouts.json");

const PAGESET_TIMEOUT_MANIFEST_ENV: &str = "FASTR_PERF_SMOKE_PAGESET_TIMEOUT_MANIFEST";

#[derive(Deserialize)]
struct PagesetTimeoutManifest {
  schema_version: u32,
  #[serde(default)]
  default_budget_ms: Option<f64>,
  fixtures: Vec<PagesetTimeoutFixture>,
}

#[derive(Deserialize)]
struct PagesetTimeoutFixture {
  name: String,
  viewport: [u32; 2],
  dpr: f32,
  media: String,
  #[serde(default)]
  budget_ms: Option<f64>,
}

#[derive(Clone, Serialize, Deserialize, Default)]
struct StageBreakdown {
  #[serde(default)]
  fetch: f64,
  #[serde(default)]
  css: f64,
  #[serde(default)]
  cascade: f64,
  #[serde(default)]
  layout: f64,
  #[serde(default)]
  paint: f64,
}

impl StageBreakdown {
  fn entries(&self) -> [(&'static str, f64); 5] {
    [
      ("fetch", self.fetch),
      ("css", self.css),
      ("cascade", self.cascade),
      ("layout", self.layout),
      ("paint", self.paint),
    ]
  }

  fn add_assign(&mut self, other: &StageBreakdown) {
    self.fetch += other.fetch;
    self.css += other.css;
    self.cascade += other.cascade;
    self.layout += other.layout;
    self.paint += other.paint;
  }

  fn rounded(&self) -> Self {
    Self {
      fetch: round_ms(self.fetch),
      css: round_ms(self.css),
      cascade: round_ms(self.cascade),
      layout: round_ms(self.layout),
      paint: round_ms(self.paint),
    }
  }
}

#[derive(Clone, Serialize, Deserialize, Default)]
struct StageTimingsSummary {
  #[serde(default)]
  html_decode_ms: f64,
  #[serde(default)]
  dom_parse_ms: f64,
  #[serde(default)]
  css_inlining_ms: f64,
  #[serde(default)]
  css_parse_ms: f64,
  #[serde(default)]
  cascade_ms: f64,
  #[serde(default)]
  box_tree_ms: f64,
  #[serde(default)]
  layout_ms: f64,
  #[serde(default)]
  text_fallback_ms: f64,
  #[serde(default)]
  text_shape_ms: f64,
  #[serde(default)]
  paint_build_ms: f64,
  #[serde(default)]
  paint_optimize_ms: f64,
  #[serde(default)]
  paint_rasterize_ms: f64,
  #[serde(default)]
  text_rasterize_ms: f64,
  #[serde(default)]
  encode_ms: f64,
}

impl StageTimingsSummary {
  fn entries(&self) -> [(&'static str, f64); 14] {
    [
      ("html_decode_ms", self.html_decode_ms),
      ("dom_parse_ms", self.dom_parse_ms),
      ("css_inlining_ms", self.css_inlining_ms),
      ("css_parse_ms", self.css_parse_ms),
      ("cascade_ms", self.cascade_ms),
      ("box_tree_ms", self.box_tree_ms),
      ("layout_ms", self.layout_ms),
      ("text_fallback_ms", self.text_fallback_ms),
      ("text_shape_ms", self.text_shape_ms),
      ("paint_build_ms", self.paint_build_ms),
      ("paint_optimize_ms", self.paint_optimize_ms),
      ("paint_rasterize_ms", self.paint_rasterize_ms),
      ("text_rasterize_ms", self.text_rasterize_ms),
      ("encode_ms", self.encode_ms),
    ]
  }
}

#[derive(Clone, Serialize, Deserialize, Default)]
struct CountsSummary {
  #[serde(default)]
  dom_nodes: u64,
  #[serde(default)]
  styled_nodes: u64,
  #[serde(default)]
  box_nodes: u64,
  #[serde(default)]
  fragments: u64,
  #[serde(default)]
  shaped_runs: u64,
  #[serde(default)]
  glyphs: u64,
  #[serde(default)]
  color_glyph_rasters: u64,
  #[serde(default)]
  fallback_cache_hits: u64,
  #[serde(default)]
  fallback_cache_misses: u64,
  #[serde(default)]
  last_resort_font_fallbacks: u64,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  last_resort_font_fallback_samples: Option<Vec<String>>,
  #[serde(default)]
  glyph_cache_hits: u64,
  #[serde(default)]
  glyph_cache_misses: u64,
  #[serde(default)]
  glyph_cache_evictions: u64,
  #[serde(default)]
  glyph_cache_bytes: u64,
  #[serde(default)]
  color_glyph_cache_hits: u64,
  #[serde(default)]
  color_glyph_cache_misses: u64,
  #[serde(default)]
  color_glyph_cache_evictions: u64,
  #[serde(default)]
  color_glyph_cache_bytes: u64,
}

#[derive(Clone, Serialize, Deserialize, Default)]
struct PaintSummary {
  #[serde(default)]
  display_items: u64,
  #[serde(default)]
  optimized_items: u64,
  #[serde(default)]
  culled_items: u64,
  #[serde(default)]
  transparent_removed: u64,
  #[serde(default)]
  noop_removed: u64,
  #[serde(default)]
  merged_items: u64,
}

#[derive(Clone, Serialize, Deserialize)]
struct ViewportSummary {
  width: u32,
  height: u32,
  dpr: f32,
  media: String,
}

#[derive(Clone, Serialize, Deserialize)]
struct FixtureSummary {
  name: String,
  path: String,
  viewport: ViewportSummary,
  total_ms: f64,
  #[serde(default, skip_serializing_if = "Option::is_none")]
  budget_ms: Option<f64>,
  #[serde(default)]
  stage_ms: StageBreakdown,
  timings_ms: StageTimingsSummary,
  counts: CountsSummary,
  paint: PaintSummary,
}

#[derive(Clone, Serialize, Deserialize)]
struct PerfSmokeSummary {
  schema_version: u32,
  fixtures: Vec<FixtureSummary>,
  total_ms: f64,
  #[serde(default)]
  stage_ms: StageBreakdown,
}

struct Regression {
  fixture: String,
  metric: RegressionMetric,
  baseline: f64,
  latest: f64,
}

enum RegressionMetric {
  Total,
  Stage(&'static str),
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let args = Args::parse();
  if args.threshold < 0.0 {
    return Err("--threshold must be non-negative".into());
  }
  if std::env::var_os("FASTR_USE_BUNDLED_FONTS").is_none() {
    std::env::set_var("FASTR_USE_BUNDLED_FONTS", "1");
  }

  let filters = args.only.as_ref().or(args.fixtures.as_ref());
  let auto_isolate = matches!(args.suite, Suite::PagesetTimeouts);
  let isolate = if args.no_isolate {
    false
  } else {
    args.isolate || auto_isolate
  };

  let baseline = if let Some(path) = args.baseline.as_ref() {
    Some(read_summary(path)?)
  } else {
    None
  };
  if let Some(base) = baseline.as_ref() {
    if base.schema_version != PERF_SMOKE_SCHEMA_VERSION {
      return Err(
        format!(
          "baseline schema_version {} does not match current schema_version {} (regenerate the baseline with the current perf_smoke)",
          base.schema_version, PERF_SMOKE_SCHEMA_VERSION
        )
        .into(),
      );
    }
  }
  if args.fail_on_regression && baseline.is_none() {
    return Err("--fail-on-regression requires --baseline".into());
  }

  let mut specs = fixture_specs_for_suite(args.suite, args.fail_on_missing_fixtures)?;
  specs = filter_specs(specs, filters)?;
  if specs.is_empty() {
    if matches!(args.suite, Suite::PagesetTimeouts) {
      eprintln!(
        "No pageset timeout fixtures available; capture them under tests/pages/fixtures before running this suite."
      );
      return Ok(());
    }
    return Err("no fixtures selected to run".into());
  }

  if isolate {
    let label = filters
      .map(|names| names.join(","))
      .filter(|names| !names.is_empty())
      .unwrap_or_else(|| "full suite".to_string());
    eprintln!("Running {} fixtures in isolation ({})", specs.len(), label);
  }

  let mut fixtures = Vec::new();
  let mut stage_totals = StageBreakdown::default();
  for spec in &specs {
    let fixture = if isolate {
      run_fixture_isolated(spec, &args)?
    } else {
      run_fixture(spec)?
    };
    stage_totals.add_assign(&fixture.stage_ms);
    fixtures.push(fixture);
  }
  fixtures.sort_by(|a, b| a.name.cmp(&b.name));
  let total_ms = round_ms(fixtures.iter().map(|f| f.total_ms).sum::<f64>());

  let summary = PerfSmokeSummary {
    schema_version: PERF_SMOKE_SCHEMA_VERSION,
    fixtures,
    total_ms,
    stage_ms: stage_totals.rounded(),
  };

  if let Some(parent) = args.output.parent() {
    if !parent.as_os_str().is_empty() {
      fs::create_dir_all(parent)?;
    }
  }
  let json = serde_json::to_string_pretty(&summary)?;
  fs::write(&args.output, &json)?;
  println!("{json}");

  let sorted_by_total = sorted_by_total(&summary.fixtures);
  if let Some(top) = args.top {
    print_top(&sorted_by_total, top);
    print_stage_breakdown(&sorted_by_total, top);
  } else {
    print_stage_breakdown(&sorted_by_total, sorted_by_total.len());
  }
  print_dom_parse_note(&summary);

  let mut exit_code = 0;

  if let Some(baseline) = baseline {
    let regressions = find_regressions(&summary, &baseline, args.threshold);
    if !regressions.is_empty() {
      eprintln!(
        "Regressions detected vs baseline (>{:.1}%):",
        args.threshold * 100.0
      );
      for regression in &regressions {
        eprintln!(
          "  {}: {} {:.3} -> {:.3} (+{:.2}%)",
          regression.fixture,
          regression.label(),
          regression.baseline,
          regression.latest,
          regression.percent_delta() * 100.0
        );
      }
      if args.fail_on_regression {
        exit_code = 1;
      }
    }
  }

  if args.fail_on_budget {
    let budget_failures = find_budget_failures(&summary.fixtures);
    if !budget_failures.is_empty() {
      eprintln!("Budget failures ({} fixtures exceeded budget_ms):", budget_failures.len());
      for failure in &budget_failures {
        if let Some((stage, stage_ms)) = failure.dominant_stage {
          eprintln!(
            "  {:<30} total_ms={:>8.3} budget_ms={:>8.3} dominant={stage}({stage_ms:.3})",
            failure.fixture, failure.total_ms, failure.budget_ms
          );
        } else {
          eprintln!(
            "  {:<30} total_ms={:>8.3} budget_ms={:>8.3}",
            failure.fixture, failure.total_ms, failure.budget_ms
          );
        }
      }
      exit_code = 1;
    }
  }

  if exit_code != 0 {
    std::process::exit(exit_code);
  }

  Ok(())
}

fn print_dom_parse_note(summary: &PerfSmokeSummary) {
  let Some(fixture) = summary
    .fixtures
    .iter()
    .find(|fixture| fixture.name.eq_ignore_ascii_case("dom_parse_stress"))
  else {
    return;
  };

  // `dom_parse_ms` is already printed in the per-fixture stage breakdown, but it may be
  // omitted when `--top` filters the stderr output. Print a dedicated line so the parsing
  // signal for this fixture is always visible.
  eprintln!(
    "dom_parse_stress: dom_parse_ms={:.3} (HTML parsing + DOM build)",
    fixture.timings_ms.dom_parse_ms
  );
}

fn fixture_specs_for_suite(
  suite: Suite,
  fail_on_missing_fixtures: bool,
) -> Result<Vec<FixtureSpec>, Box<dyn std::error::Error>> {
  let mut specs = match suite {
    Suite::Core => core_fixture_specs(),
    Suite::PagesetTimeouts => Vec::new(),
    Suite::All => core_fixture_specs(),
  };

  if matches!(suite, Suite::PagesetTimeouts | Suite::All) {
    specs.extend(pageset_timeout_fixture_specs(fail_on_missing_fixtures)?);
  }

  Ok(specs)
}

fn core_fixture_specs() -> Vec<FixtureSpec> {
  let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  CORE_FIXTURES
    .iter()
    .map(|(name, path, viewport, dpr, media)| FixtureSpec {
      name: (*name).to_string(),
      path: (*path).to_string(),
      html_path: root.join(path),
      viewport: *viewport,
      dpr: *dpr,
      media: *media,
      budget_ms: None,
    })
    .collect()
}

fn pageset_timeout_fixture_specs(
  fail_on_missing_fixtures: bool,
) -> Result<Vec<FixtureSpec>, Box<dyn std::error::Error>> {
  let manifest_contents = load_pageset_timeout_manifest_contents()?;
  let manifest: PagesetTimeoutManifest = serde_json::from_str(&manifest_contents)?;
  if manifest.schema_version != PAGESET_TIMEOUT_MANIFEST_VERSION {
    return Err(
      format!(
        "pageset timeout manifest version {} does not match expected {}",
        manifest.schema_version, PAGESET_TIMEOUT_MANIFEST_VERSION
      )
      .into(),
    );
  }

  let default_budget = manifest.default_budget_ms;
  let mut specs = Vec::new();
  let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let fixtures_root = root.join("tests/pages/fixtures");
  let mut missing = Vec::new();
  for fixture in manifest.fixtures {
    if fixture.viewport.len() != 2 {
      return Err(
        format!(
          "fixture {} has invalid viewport {:?}",
          fixture.name, fixture.viewport
        )
        .into(),
      );
    }
    let path = format!("tests/pages/fixtures/{}/index.html", fixture.name);
    let full_path = root.join(&path);
    if !full_path.exists() {
      if fail_on_missing_fixtures {
        missing.push((fixture.name.clone(), full_path));
      } else {
        eprintln!(
          "Skipping pageset timeout fixture {} (missing {})",
          fixture.name,
          full_path.display()
        );
      }
      continue;
    }
    specs.push(FixtureSpec {
      name: fixture.name.clone(),
      path,
      html_path: full_path,
      viewport: (fixture.viewport[0], fixture.viewport[1]),
      dpr: fixture.dpr,
      media: media_from_label(&fixture.media)?,
      budget_ms: fixture.budget_ms.or(default_budget),
    });
  }

  if fail_on_missing_fixtures && !missing.is_empty() {
    eprintln!(
      "Missing pageset-timeouts fixtures ({}):",
      missing.len()
    );
    for (name, path) in &missing {
      eprintln!("  {} ({})", name, path.display());
    }
    return Err("pageset-timeouts suite missing required fixtures".into());
  }

  if specs.is_empty() {
    eprintln!(
      "No pageset timeout fixtures found under {}; skipping pageset-timeouts suite.",
      fixtures_root.display()
    );
  }

  Ok(specs)
}

fn filter_specs(
  specs: Vec<FixtureSpec>,
  only: Option<&Vec<String>>,
) -> Result<Vec<FixtureSpec>, Box<dyn std::error::Error>> {
  let Some(only) = only else {
    return Ok(specs);
  };

  let wanted: HashSet<String> = only.iter().map(|name| name.to_ascii_lowercase()).collect();
  let filtered: Vec<FixtureSpec> = specs
    .into_iter()
    .filter(|spec| wanted.contains(&spec.name.to_ascii_lowercase()))
    .collect();

  if filtered.is_empty() {
    return Err(format!("no fixtures matched --only={}", only.join(",")).into());
  }

  Ok(filtered)
}

fn suite_cli_value(suite: Suite) -> &'static str {
  match suite {
    Suite::Core => "core",
    Suite::PagesetTimeouts => "pageset-timeouts",
    Suite::All => "all",
  }
}

fn temp_summary_path(spec: &FixtureSpec) -> PathBuf {
  let timestamp = SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .unwrap_or_default()
    .as_millis();
  std::env::temp_dir().join(format!(
    "perf_smoke_{}_{}_{}.json",
    spec.name.replace(['/', '\\'], "_"),
    std::process::id(),
    timestamp
  ))
}

fn run_fixture_isolated(
  spec: &FixtureSpec,
  args: &Args,
) -> Result<FixtureSummary, Box<dyn std::error::Error>> {
  let summary_path = temp_summary_path(spec);
  let mut cmd = Command::new(std::env::current_exe()?);
  cmd
    .arg("--suite")
    .arg(suite_cli_value(args.suite))
    .arg("--output")
    .arg(&summary_path)
    .arg("--only")
    .arg(&spec.name)
    .arg("--no-isolate");
  if let Some(top) = args.top {
    cmd.arg("--top").arg(top.to_string());
  }
  cmd.stdout(Stdio::null());

  let timeout_ms = spec
    .budget_ms
    .map(|ms| ms * 2.0)
    .unwrap_or(10_000.0)
    .max(1_000.0);
  let mut child = cmd.spawn()?;
  let timeout = Duration::from_millis(timeout_ms.ceil() as u64);
  let start = Instant::now();
  loop {
    if let Some(status) = child.try_wait()? {
      if !status.success() {
        return Err(
          format!(
            "isolated perf_smoke run failed for {} (status: {})",
            spec.name, status
          )
          .into(),
        );
      }
      break;
    }

    if start.elapsed() >= timeout {
      let _ = child.kill();
      let _ = child.wait();
      let _ = fs::remove_file(&summary_path);
      return Err(
        format!(
          "isolated perf_smoke run for {} exceeded timeout ({:.1}ms)",
          spec.name, timeout_ms
        )
        .into(),
      );
    }

    thread::sleep(Duration::from_millis(50));
  }

  let summary = read_summary(&summary_path)?;
  let mut fixtures = summary.fixtures;
  let _ = fs::remove_file(&summary_path);
  if fixtures.len() != 1 {
    return Err(
      format!(
        "isolated perf_smoke run for {} returned {} fixtures",
        spec.name,
        fixtures.len()
      )
      .into(),
    );
  }

  Ok(fixtures.remove(0))
}

fn run_fixture(spec: &FixtureSpec) -> Result<FixtureSummary, Box<dyn std::error::Error>> {
  let html = fs::read_to_string(&spec.html_path)?;
  let base_url = base_url_for(&spec.html_path)?;

  let policy = ResourcePolicy::default()
    .allow_http(false)
    .allow_https(false)
    .allow_file(true)
    .allow_data(true);
  let mut renderer = FastRender::builder()
    .viewport_size(spec.viewport.0, spec.viewport.1)
    .device_pixel_ratio(spec.dpr)
    .base_url(base_url.clone())
    .font_sources(FontConfig::bundled_only())
    .resource_policy(policy)
    .build()?;

  let options = RenderOptions::new()
    .with_viewport(spec.viewport.0, spec.viewport.1)
    .with_device_pixel_ratio(spec.dpr)
    .with_media_type(spec.media)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  let start = Instant::now();
  let rendered = renderer.render_html_with_diagnostics(&html, options)?;
  let (_, diagnostics) = rendered.encode(OutputFormat::Png)?;
  let total_ms = round_ms(start.elapsed().as_secs_f64() * 1000.0);

  let stats = diagnostics
    .stats
    .ok_or("diagnostics missing stats; expected DiagnosticsLevel::Basic")?;

  Ok(FixtureSummary {
    name: spec.name.clone(),
    path: spec.path.clone(),
    viewport: ViewportSummary {
      width: spec.viewport.0,
      height: spec.viewport.1,
      dpr: spec.dpr,
      media: media_label(spec.media).to_string(),
    },
    total_ms,
    budget_ms: spec.budget_ms,
    stage_ms: stage_breakdown_from_stats(&stats),
    timings_ms: timings_from_stats(&stats),
    counts: counts_from_stats(&stats),
    paint: paint_from_stats(&stats),
  })
}

struct BudgetFailure {
  fixture: String,
  total_ms: f64,
  budget_ms: f64,
  dominant_stage: Option<(&'static str, f64)>,
}

fn find_budget_failures(fixtures: &[FixtureSummary]) -> Vec<BudgetFailure> {
  let mut failures = Vec::new();
  for fixture in fixtures {
    let Some(budget_ms) = fixture.budget_ms else {
      continue;
    };
    if fixture.total_ms > budget_ms {
      let dominant_stage = fixture
        .stage_ms
        .entries()
        .into_iter()
        .filter(|(_, value)| *value > 0.0)
        .max_by(|a, b| a.1.partial_cmp(&b.1).unwrap());

      failures.push(BudgetFailure {
        fixture: fixture.name.clone(),
        total_ms: fixture.total_ms,
        budget_ms,
        dominant_stage,
      });
    }
  }
  failures
}

fn load_pageset_timeout_manifest_contents() -> Result<String, Box<dyn std::error::Error>> {
  if let Some(path) = std::env::var_os(PAGESET_TIMEOUT_MANIFEST_ENV) {
    let path = PathBuf::from(path);
    Ok(fs::read_to_string(&path).map_err(|e| {
      format!(
        "failed to read pageset timeout manifest at {}: {}",
        path.display(),
        e
      )
    })?)
  } else {
    Ok(PAGESET_TIMEOUT_MANIFEST.to_string())
  }
}

fn media_label(media: MediaType) -> &'static str {
  match media {
    MediaType::Screen => "screen",
    MediaType::Print => "print",
    MediaType::Speech => "speech",
    MediaType::All => "all",
  }
}

fn media_from_label(label: &str) -> Result<MediaType, Box<dyn std::error::Error>> {
  match label.to_ascii_lowercase().as_str() {
    "screen" => Ok(MediaType::Screen),
    "print" => Ok(MediaType::Print),
    "speech" => Ok(MediaType::Speech),
    "all" => Ok(MediaType::All),
    other => Err(format!("unsupported media type {other}").into()),
  }
}

fn base_url_for(path: &Path) -> Result<String, Box<dyn std::error::Error>> {
  let parent = path
    .parent()
    .ok_or_else(|| format!("{} has no parent directory", path.display()))?;
  Ok(
    Url::from_directory_path(parent)
      .map_err(|_| format!("could not build file:// URL for {}", parent.display()))?
      .to_string(),
  )
}

fn timings_from_stats(stats: &fastrender::RenderStats) -> StageTimingsSummary {
  let t = &stats.timings;
  StageTimingsSummary {
    html_decode_ms: round_opt(t.html_decode_ms),
    dom_parse_ms: round_opt(t.dom_parse_ms),
    css_inlining_ms: round_opt(t.css_inlining_ms),
    css_parse_ms: round_opt(t.css_parse_ms),
    cascade_ms: round_opt(t.cascade_ms),
    box_tree_ms: round_opt(t.box_tree_ms),
    layout_ms: round_opt(t.layout_ms),
    text_fallback_ms: round_opt(t.text_fallback_ms),
    text_shape_ms: round_opt(t.text_shape_ms),
    paint_build_ms: round_opt(t.paint_build_ms),
    paint_optimize_ms: round_opt(t.paint_optimize_ms),
    paint_rasterize_ms: round_opt(t.paint_rasterize_ms),
    text_rasterize_ms: round_opt(t.text_rasterize_ms),
    encode_ms: round_opt(t.encode_ms),
  }
}

/// Collapse detailed render diagnostics timings into coarse stage buckets.
///
/// Keep this consistent with `pageset_progress` so regressions attribute similarly across tools:
/// - Text shaping is grouped under `layout` because glyph positioning is part of line building.
/// - Text rasterization and final encode time are grouped under `paint` to capture output costs.
fn stage_breakdown_from_stats(stats: &fastrender::RenderStats) -> StageBreakdown {
  let t = &stats.timings;
  StageBreakdown {
    fetch: round_ms(sum_timings(&[
      t.html_decode_ms,
      t.dom_parse_ms,
      t.dom_meta_viewport_ms,
      t.dom_clone_ms,
      t.dom_top_layer_ms,
    ])),
    css: round_ms(sum_timings(&[t.css_inlining_ms, t.css_parse_ms])),
    cascade: round_ms(sum_timings(&[t.cascade_ms, t.box_tree_ms])),
    layout: round_ms(sum_timings(&[t.layout_ms, t.text_fallback_ms, t.text_shape_ms])),
    paint: round_ms(sum_timings(&[
      t.paint_build_ms,
      t.paint_optimize_ms,
      t.paint_rasterize_ms,
      t.text_rasterize_ms,
      t.encode_ms,
    ])),
  }
}

fn counts_from_stats(stats: &fastrender::RenderStats) -> CountsSummary {
  CountsSummary {
    dom_nodes: stats.counts.dom_nodes.unwrap_or(0) as u64,
    styled_nodes: stats.counts.styled_nodes.unwrap_or(0) as u64,
    box_nodes: stats.counts.box_nodes.unwrap_or(0) as u64,
    fragments: stats.counts.fragments.unwrap_or(0) as u64,
    shaped_runs: stats.counts.shaped_runs.unwrap_or(0) as u64,
    glyphs: stats.counts.glyphs.unwrap_or(0) as u64,
    color_glyph_rasters: stats.counts.color_glyph_rasters.unwrap_or(0) as u64,
    fallback_cache_hits: stats.counts.fallback_cache_hits.unwrap_or(0) as u64,
    fallback_cache_misses: stats.counts.fallback_cache_misses.unwrap_or(0) as u64,
    last_resort_font_fallbacks: stats.counts.last_resort_font_fallbacks.unwrap_or(0) as u64,
    last_resort_font_fallback_samples: stats
      .counts
      .last_resort_font_fallback_samples
      .clone()
      .filter(|samples| !samples.is_empty()),
    glyph_cache_hits: stats.counts.glyph_cache_hits.unwrap_or(0),
    glyph_cache_misses: stats.counts.glyph_cache_misses.unwrap_or(0),
    glyph_cache_evictions: stats.counts.glyph_cache_evictions.unwrap_or(0),
    glyph_cache_bytes: stats.counts.glyph_cache_bytes.unwrap_or(0) as u64,
    color_glyph_cache_hits: stats.counts.color_glyph_cache_hits.unwrap_or(0),
    color_glyph_cache_misses: stats.counts.color_glyph_cache_misses.unwrap_or(0),
    color_glyph_cache_evictions: stats.counts.color_glyph_cache_evictions.unwrap_or(0),
    color_glyph_cache_bytes: stats.counts.color_glyph_cache_bytes.unwrap_or(0) as u64,
  }
}

fn paint_from_stats(stats: &fastrender::RenderStats) -> PaintSummary {
  PaintSummary {
    display_items: stats.paint.display_items.unwrap_or(0) as u64,
    optimized_items: stats.paint.optimized_items.unwrap_or(0) as u64,
    culled_items: stats.paint.culled_items.unwrap_or(0) as u64,
    transparent_removed: stats.paint.transparent_removed.unwrap_or(0) as u64,
    noop_removed: stats.paint.noop_removed.unwrap_or(0) as u64,
    merged_items: stats.paint.merged_items.unwrap_or(0) as u64,
  }
}

fn round_opt(value: Option<f64>) -> f64 {
  value.map(round_ms).unwrap_or(0.0)
}

fn round_ms(value: f64) -> f64 {
  let rounded = (value * 1000.0).round() / 1000.0;
  if rounded == 0.0 {
    0.0
  } else {
    rounded
  }
}

fn sum_timings(values: &[Option<f64>]) -> f64 {
  values.iter().flatten().sum()
}

fn read_summary(path: &Path) -> Result<PerfSmokeSummary, Box<dyn std::error::Error>> {
  let data = fs::read_to_string(path)?;
  Ok(serde_json::from_str(&data)?)
}

fn find_regressions(
  latest: &PerfSmokeSummary,
  baseline: &PerfSmokeSummary,
  threshold: f64,
) -> Vec<Regression> {
  let mut regressions = Vec::new();
  let baseline_map = baseline
    .fixtures
    .iter()
    .map(|f| (f.name.as_str(), f))
    .collect::<BTreeMap<_, _>>();

  for fixture in &latest.fixtures {
    if let Some(base) = baseline_map.get(fixture.name.as_str()) {
      if base.total_ms > 0.0 && is_regression(base.total_ms, fixture.total_ms, threshold) {
        regressions.push(Regression {
          fixture: fixture.name.clone(),
          metric: RegressionMetric::Total,
          baseline: base.total_ms,
          latest: fixture.total_ms,
        });
      }
      for (label, value) in fixture.timings_ms.entries() {
        let base_value =
          base
            .timings_ms
            .entries()
            .iter()
            .find_map(|(l, v)| if *l == label { Some(*v) } else { None });
        if let Some(base_value) = base_value {
          if base_value > 0.0 && is_regression(base_value, value, threshold) {
            regressions.push(Regression {
              fixture: fixture.name.clone(),
              metric: RegressionMetric::Stage(label),
              baseline: base_value,
              latest: value,
            });
          }
        }
      }
      for (label, value) in fixture.stage_ms.entries() {
        let base_value =
          base
            .stage_ms
            .entries()
            .iter()
            .find_map(|(l, v)| if *l == label { Some(*v) } else { None });
        if let Some(base_value) = base_value {
          if base_value > 0.0 && is_regression(base_value, value, threshold) {
            regressions.push(Regression {
              fixture: fixture.name.clone(),
              metric: RegressionMetric::Stage(label),
              baseline: base_value,
              latest: value,
            });
          }
        }
      }
    }
  }

  regressions
}

fn is_regression(baseline: f64, latest: f64, threshold: f64) -> bool {
  ((latest - baseline) / baseline) > threshold
}

fn print_top(fixtures: &[FixtureSummary], count: usize) {
  let count = count.min(fixtures.len());
  if count == 0 {
    return;
  }
  eprintln!("Slowest {} fixtures:", count);
  for fixture in fixtures.iter().take(count) {
    let budget = format_budget(fixture.budget_ms);
    eprintln!("  {:>8.3} ms{}  {}", fixture.total_ms, budget, fixture.name);
  }
}

fn sorted_by_total(fixtures: &[FixtureSummary]) -> Vec<FixtureSummary> {
  let mut sorted = fixtures.to_vec();
  sorted.sort_by(|a, b| b.total_ms.partial_cmp(&a.total_ms).unwrap());
  sorted
}

fn print_stage_breakdown(fixtures: &[FixtureSummary], count: usize) {
  let count = count.min(fixtures.len());
  if count == 0 {
    return;
  }
  eprintln!("Stage timings ({} slowest fixtures):", count);
  for fixture in fixtures.iter().take(count) {
    let budget = format_budget(fixture.budget_ms);
    eprintln!(
      "  {:>8.3} ms{}  {:<25} {}",
      fixture.total_ms,
      budget,
      fixture.name,
      format_stage_breakdown(&fixture.timings_ms)
    );
  }
}

fn format_stage_breakdown(timings: &StageTimingsSummary) -> String {
  let parts: Vec<String> = timings
    .entries()
    .iter()
    .filter_map(|(label, value)| {
      if *value > 0.0 {
        Some(format!("{label}={value:.3}"))
      } else {
        None
      }
    })
    .collect();
  parts.join(", ")
}

fn format_budget(budget_ms: Option<f64>) -> String {
  budget_ms
    .map(|budget| format!(" (budget {:.0} ms)", budget))
    .unwrap_or_default()
}

impl Regression {
  fn percent_delta(&self) -> f64 {
    (self.latest - self.baseline) / self.baseline
  }

  fn label(&self) -> &'static str {
    match self.metric {
      RegressionMetric::Total => "total_ms",
      RegressionMetric::Stage(label) => label,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn stage_breakdown_includes_text_timings() {
    let mut stats = fastrender::RenderStats::default();
    stats.timings.layout_ms = Some(1.0);
    stats.timings.text_fallback_ms = Some(2.0);
    stats.timings.text_shape_ms = Some(3.0);
    stats.timings.paint_build_ms = Some(4.0);
    stats.timings.paint_optimize_ms = Some(5.0);
    stats.timings.paint_rasterize_ms = Some(6.0);
    stats.timings.text_rasterize_ms = Some(7.0);
    stats.timings.encode_ms = Some(8.0);

    let breakdown = stage_breakdown_from_stats(&stats);
    assert_eq!(breakdown.layout, 6.0);
    assert_eq!(breakdown.paint, 30.0);
  }
}
