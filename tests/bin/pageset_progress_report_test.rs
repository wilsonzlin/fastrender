use serde_json::json;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use tempfile::tempdir;

fn fixtures_dir() -> PathBuf {
  PathBuf::from("tests/fixtures/pageset_progress")
}

fn stats_fixtures_dir() -> PathBuf {
  PathBuf::from("tests/fixtures/pageset_progress_stats")
}

fn write_progress(
  dir: &Path,
  stem: &str,
  status: &str,
  total_ms: Option<f64>,
  stages: (f64, f64, f64, f64, f64),
  failure_stage: Option<&str>,
) {
  let mut progress = json!({
    "url": format!("https://{stem}.example.com/"),
    "status": status,
    "total_ms": total_ms,
    "stages_ms": {
      "fetch": stages.0,
      "css": stages.1,
      "cascade": stages.2,
      "layout": stages.3,
      "paint": stages.4
    },
    "notes": "",
    "hotspot": "",
    "last_good_commit": "",
    "last_regression_commit": ""
  });
  if let Some(stage) = failure_stage {
    progress
      .as_object_mut()
      .expect("progress is object")
      .insert("failure_stage".to_string(), json!(stage));
  }
  let path = dir.join(format!("{stem}.json"));
  fs::write(&path, serde_json::to_string_pretty(&progress).unwrap())
    .unwrap_or_else(|_| panic!("write {}", path.display()));
}

#[test]
fn pageset_progress_report_outputs_summary() {
  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args(["report", "--progress-dir", fixtures_dir().to_str().unwrap()])
    .output()
    .expect("run pageset_progress report");
  assert!(output.status.success(), "expected success for report");

  let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
  assert!(
    stdout.contains("Status counts (4 pages):"),
    "missing status header"
  );
  assert!(stdout.contains("ok: 2"));
  assert!(stdout.contains("timeout: 1"));
  assert!(stdout.contains("panic: 1"));
  assert!(stdout.contains("Pages with cached HTML HTTP error status (>=400): 0"));
  assert!(stdout.contains("Pages with bot mitigation subresource blocks: 0"));
  assert!(stdout.contains("Slowest pages (top 4 of 4 with timings):"));
  assert!(stdout.contains("1. timeout_page (timeout"));
  assert!(stdout.contains("2. slow_ok (ok"));
  assert!(stdout.contains("Failure hotspots (timeout/panic/error):"));
  assert!(stdout.contains("layout: 1"));
  assert!(stdout.contains("unknown: 1"));
  assert!(stdout.contains("Failure stages (timeout/panic/error):"));
  assert!(stdout.contains("unknown: 2"));
  assert!(stdout.contains("Top-slow hotspots (top 4):"));
  assert!(stdout.contains("layout: 3"));
  assert!(stdout.contains("Stage timings (ok pages with timings: 2):"));
  assert!(
    stdout.contains("totals_ms: fetch=55.00 css=110.00 cascade=220.00 box_tree=0.00 layout=3040.00 paint=545.00")
  );
}

#[cfg(unix)]
#[test]
fn pageset_progress_report_exits_success_on_broken_pipe() {
  let mut child = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      fixtures_dir().to_str().unwrap(),
      "--top",
      "0",
    ])
    .stdout(Stdio::piped())
    .stderr(Stdio::piped())
    .spawn()
    .expect("spawn pageset_progress report");

  drop(child.stdout.take());
  let output = child
    .wait_with_output()
    .expect("wait for pageset_progress report");
  assert!(
    output.status.success(),
    "expected report to exit successfully on broken pipe; status={:?} stderr:\n{}",
    output.status,
    String::from_utf8_lossy(&output.stderr)
  );
}

#[test]
fn pageset_progress_report_counts_cached_html_http_errors() {
  let temp = tempdir().expect("tempdir");
  let progress = json!({
    "url": "https://cached-status.test/",
    "status": "ok",
    "total_ms": 10.0,
    "stages_ms": {
      "fetch": 1.0,
      "css": 0.0,
      "cascade": 0.0,
      "layout": 0.0,
      "paint": 0.0
    },
    "notes": "",
    "auto_notes": "cached_html_status=403",
    "hotspot": "fetch",
    "last_good_commit": "",
    "last_regression_commit": ""
  });
  let path = temp.path().join("cached_status.json");
  fs::write(&path, serde_json::to_string_pretty(&progress).unwrap())
    .unwrap_or_else(|_| panic!("write {}", path.display()));

  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .args([
      "report",
      "--progress-dir",
      temp.path().to_str().unwrap(),
      "--top",
      "0",
    ])
    .output()
    .expect("run pageset_progress report");
  assert!(output.status.success(), "expected success for report");

  let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
  assert!(stdout.contains("Pages with cached HTML HTTP error status (>=400): 1"));
  assert!(stdout.contains("403: 1"));
  assert!(stdout.contains("cached_status status=ok cached_html_status=403"));
}

#[test]
fn pageset_progress_report_counts_bot_mitigation_blocks() {
  let temp = tempdir().expect("tempdir");
  let progress = json!({
    "url": "https://bot.test/",
    "status": "ok",
    "total_ms": 10.0,
    "stages_ms": {
      "fetch": 1.0,
      "css": 0.0,
      "cascade": 0.0,
      "layout": 0.0,
      "paint": 0.0
    },
    "notes": "",
    "hotspot": "paint",
    "last_good_commit": "",
    "last_regression_commit": "",
    "diagnostics": {
      "bot_mitigation_summary": {
        "total": 2,
        "by_kind": {
          "Image": 2
        },
        "samples": []
      }
    }
  });
  let path = temp.path().join("bot_page.json");
  fs::write(&path, serde_json::to_string_pretty(&progress).unwrap())
    .unwrap_or_else(|_| panic!("write {}", path.display()));

  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .args([
      "report",
      "--progress-dir",
      temp.path().to_str().unwrap(),
      "--top",
      "0",
    ])
    .output()
    .expect("run pageset_progress report");
  assert!(output.status.success(), "expected success for report");

  let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
  assert!(stdout.contains("Pages with bot mitigation subresource blocks: 1"));
  assert!(stdout.contains("total unique blocks: 2"));
  assert!(stdout.contains("img: 2"));
  assert!(stdout.contains("bot_page status=ok blocks=2"));
}

#[test]
fn pageset_progress_report_outputs_stats_when_verbose() {
  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      stats_fixtures_dir().to_str().unwrap(),
      "--top",
      "1",
      "--verbose-stats",
    ])
    .output()
    .expect("run pageset_progress report --verbose-stats");
  assert!(output.status.success(), "expected success for report");

  let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
  assert!(stdout.contains("Slowest pages (top 1 of 1 with timings):"));
  assert!(stdout.contains("stats:"));
  assert!(stdout.contains("Top fetch stage (top 1 of 1 ok pages):"));
  assert!(stdout.contains("nodes: dom=1200 styled=1100 boxes=900 fragments=1500"));
  assert!(
    stdout.contains(
      "text: runs=321 glyphs=6543 fallback_hits=100 fallback_misses=5 last_resort_fallbacks=2"
    ),
    "missing baseline text counters"
  );
  assert!(stdout.contains("shape_cache_hits=1234"));
  assert!(stdout.contains("shape_cache_misses=56"));
  assert!(stdout.contains("shape_cache_evict=7"));
  assert!(stdout.contains("shape_cache_entries=89"));
  assert!(stdout.contains("fallback_entries=10/100"));
  assert!(stdout.contains("fallback_cluster_entries=20/200"));
  assert!(stdout.contains("fallback_shards=4"));
  assert!(stdout.contains("fallback_desc_unique=3"));
  assert!(stdout.contains("fallback_desc_families=2"));
  assert!(stdout.contains("fallback_desc_lang=1"));
  assert!(stdout.contains("fallback_desc_weights=4"));
  assert!(stdout.contains("| timings text_shape_cpu_ms=123.45ms text_fallback_cpu_ms=67.89ms"));
  assert!(
    !stdout.contains("fallback_desc_samples="),
    "descriptor samples should not print without --verbose"
  );
  assert!(stdout.contains(
    "cascade: nodes=1100 candidates=6000 matches=2000 selector=320.50ms declaration=210.25ms"
  ));
  assert!(stdout.contains(
    "layout: layout_cache lookups=450 hits=300 stores=120 evictions=10 | intrinsic lookups=50 hits=20 | parallel enabled=true items=12 threads=2"
  ));
  assert!(stdout.contains("paint: display_items=800 optimized_items=500 culled_items=120"));
  assert!(stdout.contains(
    "resources: fetches doc=1 css=4 img=6 font=2 other=0 | image_cache hits=5 misses=3 | resource_cache fresh_hits=10 stale_hits=2 revalidated_hits=1 misses=3 bytes=12.1KiB | inflight waits=7 ms=45.67ms | disk_cache hits=4 misses=1 bytes=66.3KiB lock_waits=2 lock_wait=3.21ms ms=12.34ms | network fetches=5 bytes=4.1KiB ms=67.89ms"
  ));
  assert!(stdout.contains("Top text_fallback_cpu_ms (CPU-sum; top 1 of 1 ok pages with stats):"));
  assert!(stdout.contains("Top text_shape_cpu_ms (CPU-sum; top 1 of 1 ok pages with stats):"));
  assert!(stdout.contains("Resource totals (pages with stats: 1):"));
  assert!(stdout.contains("derived:"));
  assert!(stdout.contains("Top network fetch time (top 1 of 1 with stats):"));
  assert!(stdout.contains("Top inflight wait time (top 1 of 1 with stats):"));
  assert!(stdout.contains("Top disk cache time (top 1 of 1 with stats):"));
  assert!(stdout.contains("Top disk cache lock wait time (top 1 of 1 with stats):"));
  assert!(stdout.contains("Top timings.css_parse_ms (top 1 of 1 with stats):"));
  assert!(stdout.contains(
    "1. stats_ok timings.css_parse_ms=150.00ms total=1800.00ms hotspot=layout url=https://stats.test/"
  ));
  assert!(stdout.contains("Top layout.taffy_grid_compute_cpu_ms (top 1 of 1 with stats):"));
  assert!(stdout.contains(
    "1. stats_ok layout.taffy_grid_compute_cpu_ms=321.45ms total=1800.00ms hotspot=layout url=https://stats.test/"
  ));
  assert!(stdout.contains("Top paint.gradient_ms (top 1 of 1 with stats):"));
  assert!(stdout.contains(
    "1. stats_ok paint.gradient_ms=45.67ms total=1800.00ms hotspot=layout url=https://stats.test/"
  ));
  assert!(stdout.contains("Top paint.blur_ms (top 1 of 1 with stats):"));
  assert!(stdout.contains(
    "1. stats_ok paint.blur_ms=12.34ms total=1800.00ms hotspot=layout url=https://stats.test/"
  ));
  assert!(stdout.contains("Top paint.image_pixmap_ms (top 1 of 1 with stats):"));
  assert!(stdout.contains(
    "1. stats_ok paint.image_pixmap_ms=56.78ms total=1800.00ms hotspot=layout url=https://stats.test/"
  ));
  assert!(stdout.contains("Top layout.taffy_grid_measure_calls (top 1 of 1 with stats):"));
  assert!(stdout.contains(
    "1. stats_ok layout.taffy_grid_measure_calls=9012 total=1800.00ms hotspot=layout url=https://stats.test/"
  ));
}

#[test]
fn pageset_progress_report_outputs_descriptor_samples_when_verbose() {
  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      stats_fixtures_dir().to_str().unwrap(),
      "--top",
      "1",
      "--verbose-stats",
      "--verbose",
    ])
    .output()
    .expect("run pageset_progress report --verbose-stats --verbose");
  assert!(output.status.success(), "expected success for report");

  let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
  assert!(stdout.contains("fallback_desc_samples=["));
  assert!(stdout.contains("lang=en weight=400 families=[Arial]"));
  assert!(stdout.contains("lang=ja weight=700 families=[Noto Sans JP]"));
}

#[test]
fn pageset_progress_report_includes_text_cache_stats_tokens() {
  let temp = tempdir().expect("tempdir");
  let progress = json!({
    "url": "https://text-stats.test/",
    "status": "ok",
    "total_ms": 10.0,
    "stages_ms": {
      "fetch": 1.0,
      "css": 0.0,
      "cascade": 0.0,
      "layout": 0.0,
      "paint": 0.0
    },
    "notes": "",
    "hotspot": "layout",
    "last_good_commit": "",
    "last_regression_commit": "",
    "diagnostics": {
      "stats": {
        "timings": {
          "text_shape_ms": 1.23,
          "text_fallback_ms": 4.56
        },
        "counts": {
          "shaped_runs": 1,
          "glyphs": 2,
          "shaping_cache_hits": 3,
          "shaping_cache_misses": 4,
          "shaping_cache_evictions": 5,
          "shaping_cache_entries": 6,
          "fallback_cache_hits": 7,
          "fallback_cache_misses": 8,
          "fallback_cache_glyph_entries": 9,
          "fallback_cache_cluster_entries": 10,
          "fallback_cache_glyph_capacity": 11,
          "fallback_cache_cluster_capacity": 12,
          "fallback_cache_shards": 13,
          "fallback_cache_glyph_evictions": 14,
          "fallback_cache_cluster_evictions": 15,
          "fallback_cache_clears": 16,
          "fallback_descriptor_stats": {
            "unique_descriptors": 17,
            "unique_family_signatures": 18,
            "unique_languages": 19,
            "unique_weights": 20,
            "samples": ["lang=en families=[Arial]"]
          }
        }
      }
    }
  });

  let path = temp.path().join("text_stats.json");
  fs::write(&path, serde_json::to_string_pretty(&progress).unwrap())
    .unwrap_or_else(|_| panic!("write {}", path.display()));

  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      temp.path().to_str().unwrap(),
      "--top",
      "1",
      "--verbose-stats",
    ])
    .output()
    .expect("run pageset_progress report --verbose-stats");
  assert!(output.status.success(), "expected success for report");

  let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
  for token in [
    "shape_cache_hits=3",
    "shape_cache_misses=4",
    "shape_cache_evict=5",
    "shape_cache_entries=6",
    "fallback_entries=9/11",
    "fallback_cluster_entries=10/12",
    "fallback_shards=13",
    "fallback_glyph_evict=14",
    "fallback_cluster_evict=15",
    "fallback_clears=16",
    "fallback_desc_unique=17",
    "fallback_desc_families=18",
    "fallback_desc_lang=19",
    "fallback_desc_weights=20",
  ] {
    assert!(
      stdout.contains(token),
      "expected report output to contain {token:?}\n--- output ---\n{stdout}\n--- end output ---"
    );
  }
  assert!(
    !stdout.contains("fallback_desc_samples="),
    "descriptor samples should not print without --verbose"
  );
}

#[test]
fn pageset_progress_report_fail_on_bad_exits_non_zero() {
  let status = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      fixtures_dir().to_str().unwrap(),
      "--fail-on-bad",
    ])
    .status()
    .expect("run pageset_progress report --fail-on-bad");
  assert!(
    !status.success(),
    "expected non-zero exit for timeout/panic when --fail-on-bad is set"
  );
}

#[test]
fn pageset_progress_report_compares_and_detects_regressions() {
  let baseline = tempdir().expect("baseline dir");
  let current = tempdir().expect("current dir");

  write_progress(
    baseline.path(),
    "regress_slow",
    "ok",
    Some(100.0),
    (10.0, 10.0, 20.0, 40.0, 20.0),
    None,
  );
  write_progress(
    baseline.path(),
    "becomes_timeout",
    "ok",
    Some(80.0),
    (10.0, 10.0, 20.0, 20.0, 20.0),
    None,
  );
  write_progress(
    baseline.path(),
    "improves_fast",
    "ok",
    Some(300.0),
    (50.0, 50.0, 70.0, 80.0, 50.0),
    None,
  );
  write_progress(
    baseline.path(),
    "removed_page",
    "ok",
    Some(120.0),
    (20.0, 20.0, 20.0, 40.0, 20.0),
    None,
  );

  write_progress(
    current.path(),
    "regress_slow",
    "ok",
    Some(160.0),
    (10.0, 20.0, 40.0, 60.0, 30.0),
    None,
  );
  write_progress(
    current.path(),
    "becomes_timeout",
    "timeout",
    Some(5000.0),
    (1000.0, 1000.0, 1000.0, 1000.0, 1000.0),
    None,
  );
  write_progress(
    current.path(),
    "improves_fast",
    "ok",
    Some(150.0),
    (20.0, 30.0, 30.0, 40.0, 30.0),
    None,
  );
  write_progress(
    current.path(),
    "new_page",
    "ok",
    Some(70.0),
    (20.0, 10.0, 10.0, 20.0, 10.0),
    None,
  );

  let comparison = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      current.path().to_str().unwrap(),
      "--compare",
      baseline.path().to_str().unwrap(),
      "--top",
      "3",
    ])
    .output()
    .expect("run pageset_progress report with compare");
  assert!(
    comparison.status.success(),
    "expected success comparing progress dirs"
  );
  let stdout = String::from_utf8(comparison.stdout).expect("stdout is utf-8");
  assert!(
    stdout.contains("Status transitions"),
    "missing transitions header"
  );
  assert!(
    stdout.contains("ok -> timeout: 1"),
    "missing ok -> timeout transition"
  );
  assert!(
    stdout.contains("ok -> missing: 1"),
    "missing ok -> missing transition"
  );
  assert!(
    stdout.contains("missing -> ok: 1"),
    "missing missing -> ok transition"
  );
  assert!(
    stdout.contains("Regressions vs baseline (top 2 of 2 with timings):"),
    "missing regression header"
  );
  assert!(
    stdout.contains("becomes_timeout (ok -> timeout) Δtotal=+4920.00ms"),
    "missing timeout regression entry"
  );
  assert!(
    stdout.contains("regress_slow (ok -> ok) Δtotal=+60.00ms (+60.00%) stages_ms=fetch:+0.00 css:+10.00 cascade:+20.00"),
    "missing stage deltas for regression"
  );
  assert!(
    stdout.contains("Improvements vs baseline (top 1 of 1 with timings):"),
    "missing improvement header"
  );
  assert!(
    stdout.contains("improves_fast (ok -> ok) Δtotal=-150.00ms (-50.00%) stages_ms=fetch:-30.00 css:-20.00 cascade:-40.00 box_tree:+0.00 layout:-40.00 paint:-20.00"),
    "missing improvement entry"
  );

  let failure = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      current.path().to_str().unwrap(),
      "--compare",
      baseline.path().to_str().unwrap(),
      "--fail-on-regression",
      "--regression-threshold-percent",
      "20",
    ])
    .output()
    .expect("run pageset_progress report --fail-on-regression");
  assert!(
    !failure.status.success(),
    "expected non-zero exit for regressions"
  );
  let stderr = String::from_utf8(failure.stderr).expect("stderr is utf-8");
  assert!(
    stderr.contains("becomes_timeout: ok -> timeout"),
    "missing ok->timeout failure reason"
  );
  assert!(
    stderr.contains("regress_slow: Δtotal=+60.00ms (+60.00%)"),
    "missing slowdown failure reason"
  );
}

#[test]
fn pageset_progress_report_fail_on_slow_ok_ms_exits_non_zero() {
  let dir = tempdir().expect("progress dir");
  let progress = json!({
    "url": "https://slow_ok.example.com/",
    "status": "ok",
    "total_ms": 6000.0,
    "stages_ms": {
      "fetch": 100.0,
      "css": 200.0,
      "cascade": 300.0,
      "layout": 4000.0,
      "paint": 1400.0
    },
    "notes": "",
    "hotspot": "layout",
    "last_good_commit": "",
    "last_regression_commit": ""
  });
  fs::write(
    dir.path().join("slow_ok.json"),
    serde_json::to_string_pretty(&progress).unwrap(),
  )
  .expect("write progress json");

  let baseline = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args(["report", "--progress-dir", dir.path().to_str().unwrap()])
    .output()
    .expect("run pageset_progress report");
  assert!(
    baseline.status.success(),
    "expected report to succeed without --fail-on-slow-ok-ms"
  );

  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      dir.path().to_str().unwrap(),
      "--fail-on-slow-ok-ms",
      "5000",
    ])
    .output()
    .expect("run pageset_progress report --fail-on-slow-ok-ms");
  assert!(
    !output.status.success(),
    "expected non-zero exit for ok page exceeding threshold"
  );
  let stderr = String::from_utf8(output.stderr).expect("stderr is utf-8");
  assert!(stderr.contains("slow_ok"), "missing offending stem");
  assert!(
    stderr.contains("total=6000.00ms"),
    "missing offending total_ms"
  );
  assert!(
    stderr.contains("hotspot=layout"),
    "missing offending hotspot"
  );
}

#[test]
fn pageset_progress_report_surfaces_ok_pages_with_failure_stage_and_can_gate() {
  let dir = tempdir().expect("progress dir");
  write_progress(
    dir.path(),
    "ok_with_failure_stage",
    "ok",
    Some(123.0),
    (10.0, 20.0, 30.0, 40.0, 23.0),
    Some("paint"),
  );

  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      dir.path().to_str().unwrap(),
      "--top",
      "5",
    ])
    .output()
    .expect("run pageset_progress report with ok failure stage");
  assert!(output.status.success(), "expected success for report");
  let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
  assert!(
    stdout.contains("Ok pages with failures (failure_stage set): 1"),
    "missing ok-with-failures summary section"
  );
  assert!(
    stdout.contains("paint: 1"),
    "missing failure stage breakdown"
  );
  assert!(
    stdout.contains("ok_with_failure_stage total=123.00ms"),
    "missing ok-with-failures listing"
  );
  assert!(
    stdout.contains("failure_stage=paint"),
    "missing failure_stage in listing"
  );

  let failure = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args([
      "report",
      "--progress-dir",
      dir.path().to_str().unwrap(),
      "--fail-on-ok-with-failures",
    ])
    .output()
    .expect("run pageset_progress report --fail-on-ok-with-failures");
  assert!(
    !failure.status.success(),
    "expected non-zero exit for ok pages with failure_stage when --fail-on-ok-with-failures is set"
  );
  let stderr = String::from_utf8(failure.stderr).expect("stderr is utf-8");
  assert!(
    stderr.contains("ok page(s) with failure_stage set"),
    "missing ok-with-failures gate message"
  );
  assert!(
    stderr.contains("ok_with_failure_stage"),
    "missing offender stem in ok-with-failures gate output"
  );
}

#[test]
fn pageset_progress_report_surfaces_bot_mitigation_blocked_subresources() {
  let dir = tempdir().expect("progress dir");
  let progress = json!({
    "url": "https://blocked.example.com/",
    "status": "ok",
    "total_ms": 10.0,
    "stages_ms": {
      "fetch": 1.0,
      "css": 0.0,
      "cascade": 0.0,
      "layout": 0.0,
      "paint": 9.0
    },
    "notes": "",
    "hotspot": "paint",
    "last_good_commit": "",
    "last_regression_commit": "",
    "diagnostics": {
      "bot_mitigation_summary": {
        "total": 1,
        "by_kind": { "Image": 1 },
        "samples": [
          {
            "kind": "Image",
            "url": "https://example.com/a.jpg",
            "status": 405,
            "final_url": "https://example.com/a.jpg?captcha=deadbeef",
            "message": "HTTP status 405"
          }
        ]
      }
    }
  });
  fs::write(
    dir.path().join("blocked.json"),
    serde_json::to_string_pretty(&progress).unwrap(),
  )
  .expect("write progress json");

  let output = Command::new(env!("CARGO_BIN_EXE_pageset_progress"))
    .env("DISK_CACHE", "0")
    .env("NO_DISK_CACHE", "1")
    .args(["report", "--progress-dir", dir.path().to_str().unwrap(), "--top", "0"])
    .output()
    .expect("run pageset_progress report");
  assert!(output.status.success(), "expected report to succeed");

  let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
  assert!(
    stdout.contains("Pages with bot mitigation subresource blocks: 1"),
    "missing blocked-subresources summary section"
  );
  assert!(stdout.contains("total unique blocks: 1"));
  assert!(
    stdout.contains("img: 1"),
    "expected blocked-subresources summary to include img count"
  );
  assert!(
    stdout.contains("blocked status=ok blocks=1"),
    "expected report to list the blocked stem"
  );
}
