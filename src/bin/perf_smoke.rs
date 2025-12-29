use clap::Parser;
use fastrender::api::{DiagnosticsLevel, FastRender, RenderOptions};
use fastrender::image_output::OutputFormat;
use fastrender::style::media::MediaType;
use fastrender::{FontConfig, ResourcePolicy};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;
use url::Url;

#[derive(Parser)]
#[command(about = "Offline perf smoke test for a small set of fixtures")]
struct Args {
  /// Print the slowest N fixtures to stderr
  #[arg(long)]
  top: Option<usize>,

  /// Write JSON summary to this path (always printed to stdout)
  #[arg(long, default_value = "target/perf_smoke.json")]
  output: PathBuf,

  /// Optional baseline JSON to compare against
  #[arg(long)]
  baseline: Option<PathBuf>,

  /// Only run the listed fixtures (comma-separated)
  #[arg(long, value_delimiter = ',')]
  fixtures: Option<Vec<String>>,

  /// Relative regression threshold (0.05 = 5%)
  #[arg(long, default_value_t = 0.05)]
  threshold: f64,

  /// Exit with a non-zero status when any stage exceeds the regression threshold
  #[arg(long)]
  fail_on_regression: bool,
}

#[derive(Clone, Copy)]
struct FixtureSpec {
  name: &'static str,
  path: &'static str,
  viewport: (u32, u32),
  dpr: f32,
  media: MediaType,
}

const PERF_FIXTURES: &[FixtureSpec] = &[
  FixtureSpec {
    name: "flex_dashboard",
    path: "tests/pages/fixtures/flex_dashboard/index.html",
    viewport: (1040, 1240),
    dpr: 1.0,
    media: MediaType::Screen,
  },
  FixtureSpec {
    name: "grid_news",
    path: "tests/pages/fixtures/grid_news/index.html",
    viewport: (1040, 1240),
    dpr: 1.0,
    media: MediaType::Screen,
  },
  FixtureSpec {
    name: "multicol_article",
    path: "tests/pages/fixtures/multicol_article/index.html",
    viewport: (1040, 1240),
    dpr: 1.0,
    media: MediaType::Screen,
  },
  FixtureSpec {
    name: "table_financial",
    path: "tests/pages/fixtures/table_financial/index.html",
    viewport: (1040, 1240),
    dpr: 1.0,
    media: MediaType::Screen,
  },
  FixtureSpec {
    name: "subgrid_showcase",
    path: "tests/pages/fixtures/subgrid_showcase/index.html",
    viewport: (1040, 1240),
    dpr: 1.0,
    media: MediaType::Screen,
  },
  FixtureSpec {
    name: "mask_filter_showcase",
    path: "tests/pages/fixtures/mask_filter_showcase/index.html",
    viewport: (1040, 1240),
    dpr: 1.0,
    media: MediaType::Screen,
  },
  FixtureSpec {
    name: "paginated_report_print",
    path: "tests/pages/fixtures/paginated_report/index.html",
    viewport: (920, 1180),
    dpr: 1.0,
    media: MediaType::Print,
  },
];

const PERF_SMOKE_SCHEMA_VERSION: u32 = 2;

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
  paint_build_ms: f64,
  #[serde(default)]
  paint_optimize_ms: f64,
  #[serde(default)]
  paint_rasterize_ms: f64,
  #[serde(default)]
  encode_ms: f64,
}

impl StageTimingsSummary {
  fn entries(&self) -> [(&'static str, f64); 11] {
    [
      ("html_decode_ms", self.html_decode_ms),
      ("dom_parse_ms", self.dom_parse_ms),
      ("css_inlining_ms", self.css_inlining_ms),
      ("css_parse_ms", self.css_parse_ms),
      ("cascade_ms", self.cascade_ms),
      ("box_tree_ms", self.box_tree_ms),
      ("layout_ms", self.layout_ms),
      ("paint_build_ms", self.paint_build_ms),
      ("paint_optimize_ms", self.paint_optimize_ms),
      ("paint_rasterize_ms", self.paint_rasterize_ms),
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

  let baseline = if let Some(path) = args.baseline.as_ref() {
    Some(read_summary(path)?)
  } else {
    None
  };
  if let Some(base) = baseline.as_ref() {
    if base.schema_version != PERF_SMOKE_SCHEMA_VERSION {
      return Err(
        format!(
          "baseline schema_version {} does not match current schema_version {}",
          base.schema_version, PERF_SMOKE_SCHEMA_VERSION
        )
        .into(),
      );
    }
  }
  if args.fail_on_regression && baseline.is_none() {
    return Err("--fail-on-regression requires --baseline".into());
  }

  let selected = select_fixtures(args.fixtures.as_ref())?;
  let mut fixtures = Vec::new();
  let mut stage_totals = StageBreakdown::default();
  for spec in selected {
    let fixture = run_fixture(spec)?;
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

  if let Some(top) = args.top {
    print_top(&summary, top);
  }

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
        std::process::exit(1);
      }
    }
  }

  Ok(())
}

fn run_fixture(spec: &FixtureSpec) -> Result<FixtureSummary, Box<dyn std::error::Error>> {
  let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  let html_path = root.join(spec.path);
  let html = fs::read_to_string(&html_path)?;
  let base_url = base_url_for(&html_path)?;

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
    name: spec.name.to_string(),
    path: spec.path.to_string(),
    viewport: ViewportSummary {
      width: spec.viewport.0,
      height: spec.viewport.1,
      dpr: spec.dpr,
      media: media_label(spec.media).to_string(),
    },
    total_ms,
    stage_ms: stage_breakdown_from_stats(&stats),
    timings_ms: timings_from_stats(&stats),
    counts: counts_from_stats(&stats),
    paint: paint_from_stats(&stats),
  })
}

fn media_label(media: MediaType) -> &'static str {
  match media {
    MediaType::Screen => "screen",
    MediaType::Print => "print",
    MediaType::Speech => "speech",
    MediaType::All => "all",
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
    paint_build_ms: round_opt(t.paint_build_ms),
    paint_optimize_ms: round_opt(t.paint_optimize_ms),
    paint_rasterize_ms: round_opt(t.paint_rasterize_ms),
    encode_ms: round_opt(t.encode_ms),
  }
}

fn stage_breakdown_from_stats(stats: &fastrender::RenderStats) -> StageBreakdown {
  let t = &stats.timings;
  StageBreakdown {
    fetch: round_ms(sum_timings(&[t.html_decode_ms, t.dom_parse_ms])),
    css: round_ms(sum_timings(&[t.css_inlining_ms, t.css_parse_ms])),
    cascade: round_ms(sum_timings(&[t.cascade_ms, t.box_tree_ms])),
    layout: round_ms(sum_timings(&[t.layout_ms])),
    paint: round_ms(sum_timings(&[
      t.paint_build_ms,
      t.paint_optimize_ms,
      t.paint_rasterize_ms,
    ])),
  }
}

fn counts_from_stats(stats: &fastrender::RenderStats) -> CountsSummary {
  CountsSummary {
    dom_nodes: stats.counts.dom_nodes.unwrap_or(0) as u64,
    styled_nodes: stats.counts.styled_nodes.unwrap_or(0) as u64,
    box_nodes: stats.counts.box_nodes.unwrap_or(0) as u64,
    fragments: stats.counts.fragments.unwrap_or(0) as u64,
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
          base.timings_ms.entries().iter().find_map(
            |(l, v)| {
              if *l == label {
                Some(*v)
              } else {
                None
              }
            },
          );
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

fn print_top(summary: &PerfSmokeSummary, count: usize) {
  let mut fixtures = summary.fixtures.clone();
  fixtures.sort_by(|a, b| b.total_ms.partial_cmp(&a.total_ms).unwrap());
  eprintln!("Slowest {} fixtures:", count.min(fixtures.len()));
  for fixture in fixtures.into_iter().take(count) {
    eprintln!("  {:>8.3} ms  {}", fixture.total_ms, fixture.name);
  }
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

fn select_fixtures<'a>(
  filter: Option<&Vec<String>>,
) -> Result<Vec<&'a FixtureSpec>, Box<dyn std::error::Error>> {
  let mut fixtures: Vec<&FixtureSpec> = PERF_FIXTURES.iter().collect();
  if let Some(filter) = filter {
    if filter.is_empty() {
      return Err("at least one fixture name must be provided to --fixtures".into());
    }
    let wanted: Vec<String> = filter
      .iter()
      .map(|name| name.trim().to_ascii_lowercase())
      .filter(|name| !name.is_empty())
      .collect();
    if wanted.is_empty() {
      return Err("at least one non-empty fixture name must be provided to --fixtures".into());
    }
    fixtures.retain(|spec| wanted.contains(&spec.name.to_ascii_lowercase()));
    if fixtures.is_empty() {
      let available = PERF_FIXTURES
        .iter()
        .map(|f| f.name)
        .collect::<Vec<_>>()
        .join(", ");
      return Err(
        format!(
          "no fixtures matched {:?}; available fixtures: {available}",
          filter
        )
        .into(),
      );
    }
  }
  Ok(fixtures)
}
