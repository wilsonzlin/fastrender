use std::{
  collections::BTreeMap,
  fs,
  path::{Path, PathBuf},
};

use clap::Parser;
use pathdiff::diff_paths;
use serde::Deserialize;

#[derive(Parser, Debug)]
#[command(about = "Compare Criterion benchmark outputs and flag regressions")]
struct Args {
  /// Baseline Criterion output directory (e.g. target/criterion from a previous run)
  #[arg(long)]
  baseline: PathBuf,

  /// New Criterion output directory to compare against the baseline
  #[arg(long)]
  new: PathBuf,

  /// Allowed relative regression before failing (e.g. 0.05 = 5%)
  #[arg(long, default_value = "0.05")]
  regression_threshold: f64,

  /// Statistic to compare (mean or median)
  #[arg(long, default_value = "mean", value_parser = parse_metric)]
  metric: Metric,
}

#[derive(Debug, Clone, Copy)]
enum Metric {
  Mean,
  Median,
}

impl Metric {
  fn value(self, estimates: &Estimates) -> f64 {
    match self {
      Metric::Mean => estimates.mean.point_estimate,
      Metric::Median => estimates.median.point_estimate,
    }
  }
}

fn parse_metric(raw: &str) -> Result<Metric, String> {
  match raw.to_ascii_lowercase().as_str() {
    "mean" => Ok(Metric::Mean),
    "median" => Ok(Metric::Median),
    other => Err(format!(
      "unknown metric {:?}, expected 'mean' or 'median'",
      other
    )),
  }
}

#[derive(Debug, Deserialize)]
struct Estimate {
  point_estimate: f64,
}

#[derive(Debug, Deserialize)]
struct Estimates {
  mean: Estimate,
  median: Estimate,
}

fn collect_estimates(
  root: &Path,
) -> Result<BTreeMap<String, Estimates>, Box<dyn std::error::Error>> {
  let mut stack = vec![root.to_path_buf()];
  let mut found = BTreeMap::new();

  while let Some(dir) = stack.pop() {
    for entry in fs::read_dir(&dir)? {
      let entry = entry?;
      let path = entry.path();
      if path.is_dir() {
        stack.push(path);
        continue;
      }

      if path
        .file_name()
        .map(|n| n == "estimates.json")
        .unwrap_or(false)
      {
        let parent = path.parent().unwrap_or(root);
        let mut rel = diff_paths(parent, root).unwrap_or_else(|| parent.to_path_buf());
        if matches!(
          rel.file_name().and_then(|n| n.to_str()),
          Some("new") | Some("base")
        ) {
          rel.pop();
        }
        let key = rel.display().to_string();
        let data = fs::read_to_string(&path)?;
        let estimate: Estimates = serde_json::from_str(&data)?;
        found.insert(key, estimate);
      }
    }
  }

  Ok(found)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let args = Args::parse();
  let baseline = collect_estimates(&args.baseline)?;
  let latest = collect_estimates(&args.new)?;

  if baseline.is_empty() {
    eprintln!("No estimates found in {:?}", args.baseline);
    std::process::exit(1);
  }
  if latest.is_empty() {
    eprintln!("No estimates found in {:?}", args.new);
    std::process::exit(1);
  }

  let mut regressions = Vec::new();
  let mut improvements = Vec::new();
  let mut added = Vec::new();

  for (name, new_estimates) in &latest {
    if let Some(base_estimates) = baseline.get(name) {
      let baseline_value = args.metric.value(base_estimates);
      let new_value = args.metric.value(new_estimates);
      if baseline_value > 0.0 {
        let delta = (new_value - baseline_value) / baseline_value;
        if delta > args.regression_threshold {
          regressions.push((name, baseline_value, new_value, delta));
        } else if delta < -args.regression_threshold {
          improvements.push((name, baseline_value, new_value, delta));
        }
      }
    } else {
      added.push(name.as_str());
    }
  }

  if !added.is_empty() {
    println!("New benchmarks:");
    for name in added {
      println!("  {}", name);
    }
    println!();
  }

  if !improvements.is_empty() {
    println!("Notable improvements:");
    for (name, base, new, delta) in &improvements {
      println!(
        "  {}: {:.4} -> {:.4} ({:.2}% faster)",
        name,
        base,
        new,
        -delta * 100.0
      );
    }
    println!();
  }

  if regressions.is_empty() {
    println!(
      "No regressions detected using {:?} (threshold {:.1}%).",
      args.metric,
      args.regression_threshold * 100.0
    );
    return Ok(());
  }

  eprintln!(
    "Regressions detected (>{:.1}%):",
    args.regression_threshold * 100.0
  );
  for (name, base, new, delta) in &regressions {
    eprintln!(
      "  {}: {:.4} -> {:.4} ({:.2}% slower)",
      name,
      base,
      new,
      delta * 100.0
    );
  }

  std::process::exit(1);
}
