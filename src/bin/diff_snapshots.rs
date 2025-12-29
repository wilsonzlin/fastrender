mod common;

use clap::Parser;
use common::report::{display_path, ensure_parent_dir, escape_html, path_for_report};
use fastrender::debug::snapshot::{
  BoxKindSnapshot, BoxNodeSnapshot, DisplayItemSnapshot, DisplayListSnapshot, DomNodeKindSnapshot,
  DomNodeSnapshot, FragmentContentSnapshot, FragmentNodeSnapshot, PipelineSnapshot, RectSnapshot,
  SchemaVersion, StyledNodeSnapshot,
};
use serde::Serialize;
use serde_json::Value;
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};

const MAX_NODE_CHANGES: usize = 10;
const MAX_BOX_CHANGES: usize = 10;
const MAX_FRAGMENT_DELTAS: usize = 5;
const MAX_RUN_CHANGES: usize = 12;
const RUN_LOOKAHEAD: usize = 24;

#[derive(Parser, Debug)]
#[command(
  name = "diff_snapshots",
  about = "Compare pipeline snapshot JSON files and highlight stage-level changes"
)]
struct Args {
  /// Path to before snapshot directory or file
  #[arg(long)]
  before: PathBuf,

  /// Path to after snapshot directory or file
  #[arg(long)]
  after: PathBuf,

  /// Path to write diff_snapshots.json
  #[arg(long, default_value = "diff_snapshots.json")]
  json: PathBuf,

  /// Path to write diff_snapshots.html
  #[arg(long, default_value = "diff_snapshots.html")]
  html: PathBuf,
}

#[derive(Serialize, Default)]
struct SnapshotDiffReport {
  before: String,
  after: String,
  totals: SnapshotDiffTotals,
  entries: Vec<PageReport>,
}

#[derive(Serialize, Default)]
struct SnapshotDiffTotals {
  discovered: usize,
  matched: usize,
  missing: usize,
  errors: usize,
  schema_mismatch: usize,
}

#[derive(Serialize)]
struct PageReport {
  name: String,
  status: PageStatus,
  #[serde(skip_serializing_if = "Option::is_none")]
  before_snapshot: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  after_snapshot: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  before_png: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  after_png: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  schema: Option<SchemaSummary>,
  #[serde(skip_serializing_if = "Option::is_none")]
  summary: Option<PageSummary>,
  #[serde(skip_serializing_if = "Option::is_none")]
  error: Option<String>,
}

#[derive(Serialize, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
enum PageStatus {
  Matched,
  MissingBefore,
  MissingAfter,
  SchemaMismatch,
  Error,
}

impl PageStatus {
  fn sort_weight(&self) -> u8 {
    match self {
      PageStatus::Error => 4,
      PageStatus::SchemaMismatch => 3,
      PageStatus::MissingBefore | PageStatus::MissingAfter => 2,
      PageStatus::Matched => 0,
    }
  }

  fn label(&self) -> &'static str {
    match self {
      PageStatus::Matched => "matched",
      PageStatus::MissingBefore => "missing-before",
      PageStatus::MissingAfter => "missing-after",
      PageStatus::SchemaMismatch => "schema-mismatch",
      PageStatus::Error => "error",
    }
  }
}

#[derive(Serialize)]
struct SchemaSummary {
  pipeline: VersionPair,
  dom: VersionPair,
  styled: VersionPair,
  boxes: VersionPair,
  fragments: VersionPair,
  display_list: VersionPair,
  mixed_major: bool,
}

#[derive(Serialize)]
struct VersionPair {
  before: String,
  after: String,
  same: bool,
  same_major: bool,
}

#[derive(Serialize)]
struct PageSummary {
  counts: StageCountsSummary,
  #[serde(skip_serializing_if = "Option::is_none")]
  viewport: Option<ViewportDelta>,
  dom_changes: NodeSetChanges,
  box_changes: BoxChanges,
  fragment_changes: FragmentChanges,
  display_list_changes: DisplayListChanges,
}

#[derive(Serialize)]
struct StageCountsSummary {
  dom: CountDelta,
  styled: CountDelta,
  boxes: CountDelta,
  fragments: CountDelta,
  display_items: CountDelta,
}

#[derive(Serialize, Clone, Copy)]
struct CountDelta {
  before: usize,
  after: usize,
  delta: isize,
}

impl CountDelta {
  fn new(before: usize, after: usize) -> Self {
    CountDelta {
      before,
      after,
      delta: after as isize - before as isize,
    }
  }
}

#[derive(Serialize, Default)]
struct NodeSetChanges {
  added: Vec<NodeDescriptor>,
  removed: Vec<NodeDescriptor>,
}

#[derive(Serialize, Clone)]
struct NodeDescriptor {
  key: String,
  description: String,
}

#[derive(Serialize, Default)]
struct BoxChanges {
  structure: Vec<String>,
}

#[derive(Serialize, Default)]
struct FragmentChanges {
  area_deltas: Vec<FragmentDelta>,
  translation_deltas: Vec<FragmentDelta>,
}

#[derive(Serialize, Clone)]
struct FragmentDelta {
  key: String,
  description: String,
  before: RectSnapshot,
  after: RectSnapshot,
  area_delta: f32,
  translation: f32,
}

#[derive(Serialize)]
struct ViewportDelta {
  before: RectSnapshot,
  after: RectSnapshot,
  area_delta: f32,
}

#[derive(Serialize, Default)]
struct DisplayListChanges {
  histogram_deltas: Vec<ItemKindDelta>,
  sequence_runs: Vec<DisplayListRunChange>,
}

#[derive(Serialize)]
struct ItemKindDelta {
  kind: String,
  before: usize,
  after: usize,
  delta: isize,
}

#[derive(Serialize)]
struct DisplayListRunChange {
  change: RunKind,
  index: usize,
  count: usize,
  sample_kinds: Vec<String>,
}

#[derive(Serialize, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
enum RunKind {
  Inserted,
  Removed,
}

struct LoadedSnapshot {
  pipeline: PipelineSnapshot,
  raw: Value,
}

fn main() {
  match run() {
    Ok(()) => {}
    Err(err) => {
      eprintln!("error: {err}");
      std::process::exit(1);
    }
  }
}

fn run() -> Result<(), String> {
  let args = Args::parse();

  let before_meta =
    fs::metadata(&args.before).map_err(|e| format!("{}: {e}", args.before.display()))?;
  let after_meta =
    fs::metadata(&args.after).map_err(|e| format!("{}: {e}", args.after.display()))?;

  let html_dir = args
    .html
    .parent()
    .filter(|p| !p.as_os_str().is_empty())
    .map(PathBuf::from)
    .unwrap_or_else(|| PathBuf::from("."));
  fs::create_dir_all(&html_dir).map_err(|e| {
    format!(
      "failed to create report directory {}: {e}",
      html_dir.display()
    )
  })?;

  let (before_root, after_root, entries, totals) = match (before_meta.is_dir(), after_meta.is_dir())
  {
    (true, true) => {
      let before_dir = normalize_dir(&args.before)?;
      let after_dir = normalize_dir(&args.after)?;
      let result = process_directory(&before_dir, &after_dir, &html_dir)?;
      (before_dir, after_dir, result.0, result.1)
    }
    (false, false) => {
      let before_path = normalize_file(&args.before)?;
      let after_path = normalize_file(&args.after)?;
      let result = process_files(&before_path, &after_path, &html_dir)?;
      (before_path, after_path, vec![result.0], result.1)
    }
    _ => {
      return Err(
        "before and after must both be directories of snapshots or individual files".to_string(),
      )
    }
  };

  let report = SnapshotDiffReport {
    before: display_path(&before_root),
    after: display_path(&after_root),
    totals,
    entries,
  };

  write_json_report(&report, &args.json)?;
  write_html_report(&report, &args.html)?;

  Ok(())
}

fn process_directory(
  before_dir: &Path,
  after_dir: &Path,
  html_dir: &Path,
) -> Result<(Vec<PageReport>, SnapshotDiffTotals), String> {
  let before = collect_snapshots(before_dir)?;
  let after = collect_snapshots(after_dir)?;

  let mut stems = BTreeSet::new();
  for key in before.keys() {
    stems.insert(key.clone());
  }
  for key in after.keys() {
    stems.insert(key.clone());
  }

  let mut totals = SnapshotDiffTotals {
    discovered: stems.len(),
    ..SnapshotDiffTotals::default()
  };
  let mut entries = Vec::new();

  for stem in stems {
    let before_path = before.get(&stem);
    let after_path = after.get(&stem);
    let entry = process_pair(
      &stem,
      before_path.map(PathBuf::as_path),
      after_path.map(PathBuf::as_path),
      html_dir,
    );
    track_status(&mut totals, entry.status);
    entries.push(entry);
  }

  Ok((entries, totals))
}

fn process_files(
  before: &Path,
  after: &Path,
  html_dir: &Path,
) -> Result<(PageReport, SnapshotDiffTotals), String> {
  let stem = stem_from_snapshot_path(before)
    .or_else(|| stem_from_snapshot_path(after))
    .or_else(|| before.file_stem().map(|s| s.to_string_lossy().to_string()))
    .unwrap_or_else(|| "snapshot".to_string());

  let entry = process_pair(&stem, Some(before), Some(after), html_dir);
  let mut totals = SnapshotDiffTotals {
    discovered: 1,
    ..SnapshotDiffTotals::default()
  };
  track_status(&mut totals, entry.status);
  Ok((entry, totals))
}

fn process_pair(
  stem: &str,
  before_path: Option<&Path>,
  after_path: Option<&Path>,
  html_dir: &Path,
) -> PageReport {
  match (before_path, after_path) {
    (None, Some(after)) => PageReport {
      name: stem.to_string(),
      status: PageStatus::MissingBefore,
      before_snapshot: None,
      after_snapshot: Some(path_for_report(html_dir, after)),
      before_png: None,
      after_png: png_for_snapshot(after, stem, html_dir),
      schema: None,
      summary: None,
      error: Some("Missing in before input".to_string()),
    },
    (Some(before), None) => PageReport {
      name: stem.to_string(),
      status: PageStatus::MissingAfter,
      before_snapshot: Some(path_for_report(html_dir, before)),
      after_snapshot: None,
      before_png: png_for_snapshot(before, stem, html_dir),
      after_png: None,
      schema: None,
      summary: None,
      error: Some("Missing in after input".to_string()),
    },
    (Some(before), Some(after)) => match (load_snapshot(before), load_snapshot(after)) {
      (Ok(before_snap), Ok(after_snap)) => {
        let schema = summarize_schema(&before_snap.pipeline, &after_snap.pipeline);
        if schema.mixed_major {
          return PageReport {
            name: stem.to_string(),
            status: PageStatus::SchemaMismatch,
            before_snapshot: Some(path_for_report(html_dir, before)),
            after_snapshot: Some(path_for_report(html_dir, after)),
            before_png: png_for_snapshot(before, stem, html_dir),
            after_png: png_for_snapshot(after, stem, html_dir),
            schema: Some(schema),
            summary: None,
            error: Some(
              "Snapshot schemas differ (mixing major versions is not supported)".to_string(),
            ),
          };
        }

        let summary = build_summary(&before_snap, &after_snap);
        PageReport {
          name: stem.to_string(),
          status: PageStatus::Matched,
          before_snapshot: Some(path_for_report(html_dir, before)),
          after_snapshot: Some(path_for_report(html_dir, after)),
          before_png: png_for_snapshot(before, stem, html_dir),
          after_png: png_for_snapshot(after, stem, html_dir),
          schema: Some(schema),
          summary: Some(summary),
          error: None,
        }
      }
      (Err(err), _) => PageReport {
        name: stem.to_string(),
        status: PageStatus::Error,
        before_snapshot: Some(path_for_report(html_dir, before)),
        after_snapshot: after_path.map(|p| path_for_report(html_dir, p)),
        before_png: png_for_snapshot(before, stem, html_dir),
        after_png: after_path.and_then(|p| png_for_snapshot(p, stem, html_dir)),
        schema: None,
        summary: None,
        error: Some(err),
      },
      (_, Err(err)) => PageReport {
        name: stem.to_string(),
        status: PageStatus::Error,
        before_snapshot: Some(path_for_report(html_dir, before)),
        after_snapshot: Some(path_for_report(html_dir, after)),
        before_png: png_for_snapshot(before, stem, html_dir),
        after_png: png_for_snapshot(after, stem, html_dir),
        schema: None,
        summary: None,
        error: Some(err),
      },
    },
    (None, None) => unreachable!("stem discovered without any snapshots"),
  }
}

fn track_status(totals: &mut SnapshotDiffTotals, status: PageStatus) {
  match status {
    PageStatus::Matched => totals.matched += 1,
    PageStatus::MissingBefore | PageStatus::MissingAfter => totals.missing += 1,
    PageStatus::SchemaMismatch => totals.schema_mismatch += 1,
    PageStatus::Error => totals.errors += 1,
  }
}

fn normalize_dir(path: &Path) -> Result<PathBuf, String> {
  let canonical = fs::canonicalize(path).map_err(|e| format!("{}: {e}", path.display()))?;
  if !canonical.is_dir() {
    return Err(format!("{} is not a directory", canonical.display()));
  }
  Ok(canonical)
}

fn normalize_file(path: &Path) -> Result<PathBuf, String> {
  let canonical = fs::canonicalize(path).map_err(|e| format!("{}: {e}", path.display()))?;
  if !canonical.is_file() {
    return Err(format!("{} is not a file", canonical.display()));
  }
  Ok(canonical)
}

fn collect_snapshots(dir: &Path) -> Result<HashMap<String, PathBuf>, String> {
  let mut map = HashMap::new();
  for entry in fs::read_dir(dir).map_err(|e| format!("failed to read {}: {e}", dir.display()))? {
    let entry = entry.map_err(|e| format!("failed to read entry: {e}"))?;
    let path = entry.path();
    if !path.is_file() {
      continue;
    }
    let Some(stem) = stem_from_snapshot_path(&path) else {
      continue;
    };
    let canonical = fs::canonicalize(&path).map_err(|e| format!("{}: {e}", path.display()))?;
    map.entry(stem).or_insert(canonical);
  }
  Ok(map)
}

fn stem_from_snapshot_path(path: &Path) -> Option<String> {
  let file_name = path.file_name()?.to_string_lossy();
  file_name
    .strip_suffix(".snapshot.json")
    .map(|s| s.to_string())
}

fn png_for_snapshot(path: &Path, stem: &str, html_dir: &Path) -> Option<String> {
  let parent = path.parent()?;
  let png = parent.join(format!("{stem}.png"));
  if png.exists() {
    Some(path_for_report(html_dir, &png))
  } else {
    None
  }
}

fn load_snapshot(path: &Path) -> Result<LoadedSnapshot, String> {
  let data = fs::read(path).map_err(|e| format!("failed to read {}: {e}", path.display()))?;
  let raw: Value = serde_json::from_slice(&data)
    .map_err(|e| format!("failed to parse {}: {e}", path.display()))?;
  let pipeline: PipelineSnapshot = serde_json::from_value(raw.clone()).map_err(|e| {
    format!(
      "failed to decode snapshot {} (schema mismatch?): {e}",
      path.display()
    )
  })?;
  Ok(LoadedSnapshot { pipeline, raw })
}

fn summarize_schema(before: &PipelineSnapshot, after: &PipelineSnapshot) -> SchemaSummary {
  let pipeline = version_pair(before.schema_version, after.schema_version);
  let dom = version_pair(before.dom.schema_version, after.dom.schema_version);
  let styled = version_pair(before.styled.schema_version, after.styled.schema_version);
  let boxes = version_pair(
    before.box_tree.schema_version,
    after.box_tree.schema_version,
  );
  let fragments = version_pair(
    before.fragment_tree.schema_version,
    after.fragment_tree.schema_version,
  );
  let display_list = version_pair(
    before.display_list.schema_version,
    after.display_list.schema_version,
  );

  let mixed_major = !pipeline.same_major
    || !dom.same_major
    || !styled.same_major
    || !boxes.same_major
    || !fragments.same_major
    || !display_list.same_major;

  SchemaSummary {
    pipeline,
    dom,
    styled,
    boxes,
    fragments,
    display_list,
    mixed_major,
  }
}

fn version_pair(before: SchemaVersion, after: SchemaVersion) -> VersionPair {
  VersionPair {
    before: before.label().to_string(),
    after: after.label().to_string(),
    same: before == after,
    same_major: before.major() == after.major(),
  }
}

fn build_summary(before: &LoadedSnapshot, after: &LoadedSnapshot) -> PageSummary {
  let counts = StageCountsSummary {
    dom: CountDelta::new(
      count_dom_nodes(&before.pipeline.dom.root),
      count_dom_nodes(&after.pipeline.dom.root),
    ),
    styled: CountDelta::new(
      count_styled_nodes(&before.pipeline.styled.root),
      count_styled_nodes(&after.pipeline.styled.root),
    ),
    boxes: CountDelta::new(
      count_box_nodes(&before.pipeline.box_tree.root),
      count_box_nodes(&after.pipeline.box_tree.root),
    ),
    fragments: CountDelta::new(
      count_fragments(&before.pipeline.fragment_tree.roots),
      count_fragments(&after.pipeline.fragment_tree.roots),
    ),
    display_items: CountDelta::new(
      before.pipeline.display_list.items.len(),
      after.pipeline.display_list.items.len(),
    ),
  };

  let viewport = Some(ViewportDelta {
    before: before.pipeline.fragment_tree.viewport.clone(),
    after: after.pipeline.fragment_tree.viewport.clone(),
    area_delta: rect_area(&after.pipeline.fragment_tree.viewport)
      - rect_area(&before.pipeline.fragment_tree.viewport),
  });

  PageSummary {
    counts,
    viewport,
    dom_changes: diff_dom_nodes(before, after),
    box_changes: diff_boxes(before, after),
    fragment_changes: diff_fragments(before, after),
    display_list_changes: diff_display_lists(
      &before.pipeline.display_list,
      &after.pipeline.display_list,
    ),
  }
}

fn count_dom_nodes(node: &DomNodeSnapshot) -> usize {
  1 + node.children.iter().map(count_dom_nodes).sum::<usize>()
}

fn count_styled_nodes(node: &StyledNodeSnapshot) -> usize {
  1 + node.children.iter().map(count_styled_nodes).sum::<usize>()
}

fn count_box_nodes(node: &BoxNodeSnapshot) -> usize {
  1 + node.children.iter().map(count_box_nodes).sum::<usize>()
}

fn count_fragments(nodes: &[FragmentNodeSnapshot]) -> usize {
  nodes
    .iter()
    .map(|n| 1 + count_fragments(&n.children))
    .sum::<usize>()
}

fn diff_dom_nodes(before: &LoadedSnapshot, after: &LoadedSnapshot) -> NodeSetChanges {
  let mut before_map = HashMap::new();
  let mut after_map = HashMap::new();
  let mut path = Vec::new();
  collect_dom_nodes(
    &before.pipeline.dom.root,
    before.raw.get("dom").and_then(|d| d.get("root")),
    &mut path,
    &mut before_map,
  );
  collect_dom_nodes(
    &after.pipeline.dom.root,
    after.raw.get("dom").and_then(|d| d.get("root")),
    &mut path,
    &mut after_map,
  );

  let mut added = Vec::new();
  let mut removed = Vec::new();

  for (key, node) in after_map.iter() {
    if !before_map.contains_key(key) {
      added.push(node.clone());
    }
  }
  for (key, node) in before_map.iter() {
    if !after_map.contains_key(key) {
      removed.push(node.clone());
    }
  }

  added.sort_by(|a, b| a.key.cmp(&b.key));
  removed.sort_by(|a, b| a.key.cmp(&b.key));
  added.truncate(MAX_NODE_CHANGES);
  removed.truncate(MAX_NODE_CHANGES);

  NodeSetChanges { added, removed }
}

fn collect_dom_nodes(
  node: &DomNodeSnapshot,
  raw_node: Option<&Value>,
  path: &mut Vec<usize>,
  out: &mut HashMap<String, NodeDescriptor>,
) {
  let key = stable_dom_key(raw_node, path);
  out.insert(
    key.clone(),
    NodeDescriptor {
      key,
      description: describe_dom_node(node),
    },
  );

  for (idx, child) in node.children.iter().enumerate() {
    path.push(idx);
    let raw_child = raw_child(raw_node, idx);
    collect_dom_nodes(child, raw_child, path, out);
    path.pop();
  }
}

fn stable_dom_key(raw_node: Option<&Value>, path: &[usize]) -> String {
  if let Some(path) = raw_node
    .and_then(|v| v.get("dom_path"))
    .and_then(|v| v.as_str())
  {
    path.to_string()
  } else {
    format!("path:{}", path_string(path))
  }
}

fn describe_dom_node(node: &DomNodeSnapshot) -> String {
  match &node.kind {
    DomNodeKindSnapshot::Document => "document".to_string(),
    DomNodeKindSnapshot::ShadowRoot { mode } => format!("shadow-root({mode})"),
    DomNodeKindSnapshot::Slot { namespace, .. } => format!("slot[{namespace}]"),
    DomNodeKindSnapshot::Element {
      tag_name,
      attributes,
      ..
    } => {
      let mut label = tag_name.clone();
      let mut classes = Vec::new();
      let mut id = None;
      for attr in attributes {
        if attr.name.eq_ignore_ascii_case("id") {
          id = Some(attr.value.clone());
        } else if attr.name.eq_ignore_ascii_case("class") {
          classes.extend(
            attr
              .value
              .split_whitespace()
              .filter(|s| !s.is_empty())
              .map(ToString::to_string),
          );
        }
      }
      if let Some(id) = id {
        label.push('#');
        label.push_str(&id);
      }
      if !classes.is_empty() {
        label.push('.');
        label.push_str(&classes.join("."));
      }
      label
    }
    DomNodeKindSnapshot::Text { content } => format!("text: {}", truncate(content, 40)),
  }
}

fn diff_boxes(before: &LoadedSnapshot, after: &LoadedSnapshot) -> BoxChanges {
  let mut changes = Vec::new();
  let mut path = Vec::new();
  diff_box_structure(
    &before.pipeline.box_tree.root,
    &after.pipeline.box_tree.root,
    before.raw.get("box_tree").and_then(|b| b.get("root")),
    after.raw.get("box_tree").and_then(|b| b.get("root")),
    &mut path,
    &mut changes,
  );
  changes.truncate(MAX_BOX_CHANGES);
  BoxChanges { structure: changes }
}

fn diff_box_structure(
  before: &BoxNodeSnapshot,
  after: &BoxNodeSnapshot,
  raw_before: Option<&Value>,
  raw_after: Option<&Value>,
  path: &mut Vec<usize>,
  out: &mut Vec<String>,
) {
  let location = stable_box_key(raw_before.or(raw_after), path, before.box_id, after.box_id);

  if before.kind != after.kind {
    out.push(format!(
      "{location}: kind changed from {} to {}",
      describe_box_kind(&before.kind),
      describe_box_kind(&after.kind)
    ));
  }

  if before.children.len() != after.children.len() {
    out.push(format!(
      "{location}: child count {} -> {}",
      before.children.len(),
      after.children.len()
    ));
  }

  let shared = before.children.len().min(after.children.len());
  for idx in 0..shared {
    path.push(idx);
    let child_before = before.children.get(idx).unwrap();
    let child_after = after.children.get(idx).unwrap();
    let raw_b = raw_child(raw_before, idx);
    let raw_a = raw_child(raw_after, idx);
    diff_box_structure(child_before, child_after, raw_b, raw_a, path, out);
    path.pop();
  }
}

fn stable_box_key(
  raw: Option<&Value>,
  path: &[usize],
  before_id: usize,
  after_id: usize,
) -> String {
  if let Some(path) = raw.and_then(|v| v.get("box_path")).and_then(|v| v.as_str()) {
    path.to_string()
  } else {
    format!("box:{}->{}@{}", before_id, after_id, path_string(path))
  }
}

fn describe_box_kind(kind: &BoxKindSnapshot) -> String {
  match kind {
    BoxKindSnapshot::Block { formatting_context } => {
      format!("block({formatting_context})")
    }
    BoxKindSnapshot::Inline { formatting_context } => format!(
      "inline({})",
      formatting_context.as_deref().unwrap_or("none")
    ),
    BoxKindSnapshot::Text { text } => format!("text({})", truncate(text, 32)),
    BoxKindSnapshot::Marker { text } => {
      format!("marker({})", text.clone().unwrap_or_default())
    }
    BoxKindSnapshot::Replaced { replaced } => format!("replaced({:?})", replaced),
    BoxKindSnapshot::Anonymous { kind } => format!("anonymous({kind})"),
  }
}

fn diff_fragments(before: &LoadedSnapshot, after: &LoadedSnapshot) -> FragmentChanges {
  let before_roots = before
    .raw
    .get("fragment_tree")
    .and_then(|f| f.get("roots"))
    .and_then(|r| r.as_array());
  let after_roots = after
    .raw
    .get("fragment_tree")
    .and_then(|f| f.get("roots"))
    .and_then(|r| r.as_array());

  let before_map = collect_fragments(&before.pipeline.fragment_tree.roots, before_roots);
  let after_map = collect_fragments(&after.pipeline.fragment_tree.roots, after_roots);

  let mut deltas = Vec::new();

  for (key, before_frag) in before_map.iter() {
    if let Some(after_frag) = after_map.get(key) {
      let area_delta = rect_area(&after_frag.bounds) - rect_area(&before_frag.bounds);
      let translation = translation_distance(&before_frag.bounds, &after_frag.bounds);
      if area_delta != 0.0 || translation != 0.0 {
        deltas.push(FragmentDelta {
          key: key.clone(),
          description: before_frag.description.clone(),
          before: before_frag.bounds.clone(),
          after: after_frag.bounds.clone(),
          area_delta,
          translation,
        });
      }
    }
  }

  let mut by_area = deltas.clone();
  by_area.sort_by(|a, b| cmp_f32(b.area_delta.abs(), a.area_delta.abs()));
  by_area.truncate(MAX_FRAGMENT_DELTAS);

  let mut by_translation = deltas;
  by_translation.sort_by(|a, b| cmp_f32(b.translation, a.translation));
  by_translation.truncate(MAX_FRAGMENT_DELTAS);

  FragmentChanges {
    area_deltas: by_area,
    translation_deltas: by_translation,
  }
}

fn collect_fragments(
  roots: &[FragmentNodeSnapshot],
  raw_roots: Option<&Vec<Value>>,
) -> HashMap<String, FragmentInfo> {
  let mut map = HashMap::new();
  for (idx, root) in roots.iter().enumerate() {
    let mut path = vec![idx];
    let raw_root = raw_roots.and_then(|r| r.get(idx));
    collect_fragment_node(root, raw_root, &mut path, &mut map);
  }
  map
}

#[derive(Clone)]
struct FragmentInfo {
  bounds: RectSnapshot,
  description: String,
}

fn collect_fragment_node(
  node: &FragmentNodeSnapshot,
  raw: Option<&Value>,
  path: &mut Vec<usize>,
  out: &mut HashMap<String, FragmentInfo>,
) {
  let key = stable_fragment_key(node, raw, path);
  out.insert(
    key,
    FragmentInfo {
      bounds: node.bounds.clone(),
      description: describe_fragment(node),
    },
  );

  for (idx, child) in node.children.iter().enumerate() {
    path.push(idx);
    let raw_child = raw_child(raw, idx);
    collect_fragment_node(child, raw_child, path, out);
    path.pop();
  }
}

fn stable_fragment_key(node: &FragmentNodeSnapshot, raw: Option<&Value>, path: &[usize]) -> String {
  if let Some(path) = raw
    .and_then(|v| v.get("fragment_path"))
    .and_then(|v| v.as_str())
  {
    return path.to_string();
  }
  if let Some(box_id) = fragment_box_id(&node.content) {
    return format!(
      "box:{box_id}:fi{}of{}:fc{}@{}",
      node.fragment_index,
      node.fragment_count,
      node.fragmentainer_index,
      path_string(path)
    );
  }
  format!("fragment:{}@{}", node.fragment_id, path_string(path))
}

fn fragment_box_id(content: &FragmentContentSnapshot) -> Option<usize> {
  match content {
    FragmentContentSnapshot::Block { box_id }
    | FragmentContentSnapshot::Inline { box_id, .. }
    | FragmentContentSnapshot::Text { box_id, .. }
    | FragmentContentSnapshot::Replaced { box_id, .. } => *box_id,
    FragmentContentSnapshot::Line { .. } => None,
  }
}

fn describe_fragment(node: &FragmentNodeSnapshot) -> String {
  match &node.content {
    FragmentContentSnapshot::Block { box_id } => format!("block {:?}", box_id),
    FragmentContentSnapshot::Inline {
      box_id,
      fragment_index,
    } => format!("inline {:?} ({})", box_id, fragment_index),
    FragmentContentSnapshot::Text { text, box_id, .. } => {
      format!("text {:?} ({})", box_id, truncate(text, 32))
    }
    FragmentContentSnapshot::Line { .. } => "line".to_string(),
    FragmentContentSnapshot::Replaced { box_id, replaced } => {
      format!("replaced {:?} ({:?})", box_id, replaced)
    }
  }
}

fn diff_display_lists(
  before: &DisplayListSnapshot,
  after: &DisplayListSnapshot,
) -> DisplayListChanges {
  let mut histogram = Vec::new();
  let before_histogram = kind_histogram(before);
  let after_histogram = kind_histogram(after);
  let mut kinds = BTreeSet::new();
  for key in before_histogram.keys() {
    kinds.insert(key.clone());
  }
  for key in after_histogram.keys() {
    kinds.insert(key.clone());
  }
  for kind in kinds {
    let before_count = *before_histogram.get(&kind).unwrap_or(&0);
    let after_count = *after_histogram.get(&kind).unwrap_or(&0);
    let delta = after_count as isize - before_count as isize;
    if delta != 0 {
      histogram.push(ItemKindDelta {
        kind,
        before: before_count,
        after: after_count,
        delta,
      });
    }
  }
  histogram.sort_by(|a, b| {
    b.delta
      .abs()
      .cmp(&a.delta.abs())
      .then_with(|| a.kind.cmp(&b.kind))
  });

  let sequence_runs = diff_display_sequence(before, after);

  DisplayListChanges {
    histogram_deltas: histogram,
    sequence_runs,
  }
}

fn kind_histogram(list: &DisplayListSnapshot) -> HashMap<String, usize> {
  let mut map = HashMap::new();
  for item in &list.items {
    *map.entry(item.kind.clone()).or_insert(0) += 1;
  }
  map
}

fn diff_display_sequence(
  before: &DisplayListSnapshot,
  after: &DisplayListSnapshot,
) -> Vec<DisplayListRunChange> {
  let before_keys: Vec<String> = before.items.iter().map(display_item_key).collect();
  let after_keys: Vec<String> = after.items.iter().map(display_item_key).collect();

  let mut runs = Vec::new();
  let mut i = 0usize;
  let mut j = 0usize;

  while i < before_keys.len() || j < after_keys.len() {
    if i < before_keys.len() && j < after_keys.len() && before_keys[i] == after_keys[j] {
      i += 1;
      j += 1;
      continue;
    }

    let next_after = if i < before_keys.len() {
      after_keys
        .iter()
        .skip(j)
        .take(RUN_LOOKAHEAD)
        .position(|k| *k == before_keys[i])
    } else {
      None
    };
    let next_before = if j < after_keys.len() {
      before_keys
        .iter()
        .skip(i)
        .take(RUN_LOOKAHEAD)
        .position(|k| *k == after_keys[j])
    } else {
      None
    };

    if let Some(pos) = next_after {
      if pos > 0 {
        let start = j;
        let end = (j + pos).min(after.items.len());
        push_run(
          &mut runs,
          RunKind::Inserted,
          start,
          &after.items[start..end],
        );
        j = end;
        continue;
      }
    }

    if let Some(pos) = next_before {
      if pos > 0 {
        let start = i;
        let end = (i + pos).min(before.items.len());
        push_run(
          &mut runs,
          RunKind::Removed,
          start,
          &before.items[start..end],
        );
        i = end;
        continue;
      }
    }

    if i < before.items.len() {
      push_run(&mut runs, RunKind::Removed, i, &before.items[i..i + 1]);
      i += 1;
    } else if j < after.items.len() {
      push_run(&mut runs, RunKind::Inserted, j, &after.items[j..j + 1]);
      j += 1;
    }
  }

  runs.truncate(MAX_RUN_CHANGES);
  runs
}

fn push_run(
  runs: &mut Vec<DisplayListRunChange>,
  change: RunKind,
  index: usize,
  items: &[DisplayItemSnapshot],
) {
  if items.is_empty() {
    return;
  }
  if let Some(last) = runs.last_mut() {
    if last.change == change && last.index + last.count == index {
      last.count += items.len();
      merge_samples(&mut last.sample_kinds, items);
      return;
    }
  }
  runs.push(DisplayListRunChange {
    change,
    index,
    count: items.len(),
    sample_kinds: summarize_kinds(items),
  });
}

fn merge_samples(existing: &mut Vec<String>, items: &[DisplayItemSnapshot]) {
  let mut counts = HashMap::new();
  for entry in existing.drain(..) {
    if let Some((kind, count)) = entry.split_once(" x") {
      if let Ok(parsed) = count.parse::<usize>() {
        *counts.entry(kind.to_string()).or_insert(0) += parsed;
      }
    }
  }
  for item in items {
    *counts.entry(item.kind.clone()).or_insert(0) += 1;
  }
  let mut entries: Vec<_> = counts.into_iter().collect();
  entries.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
  existing.extend(
    entries
      .into_iter()
      .take(3)
      .map(|(k, c)| format!("{k} x{c}")),
  );
}

fn summarize_kinds(items: &[DisplayItemSnapshot]) -> Vec<String> {
  let mut counts = HashMap::new();
  for item in items {
    *counts.entry(item.kind.clone()).or_insert(0) += 1;
  }
  let mut entries: Vec<_> = counts.into_iter().collect();
  entries.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
  entries
    .into_iter()
    .take(3)
    .map(|(k, c)| format!("{k} x{c}"))
    .collect()
}

fn display_item_key(item: &DisplayItemSnapshot) -> String {
  let bounds = item
    .bounds
    .as_ref()
    .map(|b| format_rect(b))
    .unwrap_or_else(|| "none".to_string());
  format!("{}@{}", item.kind, bounds)
}

fn rect_area(rect: &RectSnapshot) -> f32 {
  rect.width * rect.height
}

fn translation_distance(a: &RectSnapshot, b: &RectSnapshot) -> f32 {
  let dx = b.x - a.x;
  let dy = b.y - a.y;
  (dx * dx + dy * dy).sqrt()
}

fn write_json_report(report: &SnapshotDiffReport, path: &Path) -> Result<(), String> {
  ensure_parent_dir(path)?;
  let json = serde_json::to_string_pretty(report)
    .map_err(|e| format!("failed to serialize JSON report: {e}"))?;
  fs::write(path, json).map_err(|e| format!("failed to write {}: {e}", path.display()))
}

fn write_html_report(report: &SnapshotDiffReport, path: &Path) -> Result<(), String> {
  ensure_parent_dir(path)?;

  let mut entries_html = String::new();
  let mut sorted: Vec<&PageReport> = report.entries.iter().collect();
  sorted.sort_by(|a, b| {
    b.status
      .sort_weight()
      .cmp(&a.status.sort_weight())
      .then_with(|| a.name.cmp(&b.name))
  });

  for entry in sorted {
    let open_attr = if entry.status == PageStatus::Matched {
      ""
    } else {
      " open"
    };
    let mut summary_bits = Vec::new();
    if let Some(counts) = entry.summary.as_ref().map(|s| &s.counts) {
      summary_bits.push(format!(
        "Δdom {:+}, Δboxes {:+}, Δfragments {:+}",
        counts.dom.delta, counts.boxes.delta, counts.fragments.delta
      ));
    }
    if entry.before_png.is_some() || entry.after_png.is_some() {
      let mut parts = Vec::new();
      if let Some(png) = &entry.before_png {
        parts.push(format!(r#"<a href="{}">before.png</a>"#, escape_html(png)));
      }
      if let Some(png) = &entry.after_png {
        parts.push(format!(r#"<a href="{}">after.png</a>"#, escape_html(png)));
      }
      summary_bits.push(parts.join(" | "));
    }
    entries_html.push_str(&format!(
      r#"<details class="{class}"{open}>
  <summary><strong>{name}</strong> <span class="status">{status}</span> {summary}</summary>
  "#,
      class = entry.status.label(),
      open = open_attr,
      name = escape_html(&entry.name),
      status = escape_html(entry.status.label()),
      summary = escape_html(&summary_bits.join(" | ")),
    ));

    entries_html.push_str("<div class=\"paths\">");
    if let Some(path) = &entry.before_snapshot {
      entries_html.push_str(&format!(
        "<div><strong>Before snapshot:</strong> {}</div>",
        escape_html(path)
      ));
    }
    if let Some(path) = &entry.after_snapshot {
      entries_html.push_str(&format!(
        "<div><strong>After snapshot:</strong> {}</div>",
        escape_html(path)
      ));
    }
    entries_html.push_str("</div>");

    if let Some(err) = &entry.error {
      entries_html.push_str(&format!("<div class=\"error\">{}</div>", escape_html(err)));
    }

    if let Some(schema) = &entry.schema {
      entries_html.push_str(&render_schema(schema));
    }
    if let Some(summary) = &entry.summary {
      entries_html.push_str(&render_summary(summary));
    }

    entries_html.push_str("</details>");
  }

  let content = format!(
    r#"<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Snapshot diff report</title>
    <style>
      body {{ font-family: sans-serif; margin: 20px; }}
      details {{ border: 1px solid #ddd; border-radius: 4px; padding: 8px 12px; margin-bottom: 12px; background: #fafafa; }}
      summary {{ cursor: pointer; }}
      summary .status {{ text-transform: uppercase; font-size: 0.9em; padding: 2px 6px; border-radius: 4px; margin-left: 6px; background: #eef; }}
      details.missing-before, details.missing-after, details.error {{ background: #fff0f0; }}
      details.schema-mismatch {{ background: #fff7e0; }}
      .paths {{ font-size: 0.9em; color: #444; }}
      table {{ border-collapse: collapse; margin: 8px 0; width: 100%; }}
      th, td {{ border: 1px solid #ddd; padding: 4px 6px; text-align: left; }}
      th {{ background: #f0f0f0; }}
      .error {{ color: #b00020; margin: 6px 0; }}
      .section {{ margin-top: 8px; }}
      .section h3 {{ margin: 0 0 4px 0; }}
      ul {{ margin: 0; padding-left: 18px; }}
      code {{ background: #eee; padding: 1px 3px; border-radius: 3px; }}
    </style>
  </head>
  <body>
    <h1>Snapshot diff report</h1>
    <p><strong>Before:</strong> {before}</p>
    <p><strong>After:</strong> {after}</p>
    <p>Processed {matched} matching pairs out of {discovered} (missing: {missing}, schema mismatches: {schema}, errors: {errors}).</p>
    {entries}
  </body>
</html>
"#,
    before = escape_html(&report.before),
    after = escape_html(&report.after),
    matched = report.totals.matched,
    discovered = report.totals.discovered,
    missing = report.totals.missing,
    schema = report.totals.schema_mismatch,
    errors = report.totals.errors,
    entries = entries_html,
  );

  fs::write(path, content).map_err(|e| format!("failed to write {}: {e}", path.display()))
}

fn render_schema(schema: &SchemaSummary) -> String {
  format!(
    r#"<div class="section">
  <h3>Schema</h3>
  <table>
    <tr><th>Stage</th><th>Before</th><th>After</th><th>Major match</th></tr>
    <tr><td>Pipeline</td><td>{pb}</td><td>{pa}</td><td>{pm}</td></tr>
    <tr><td>DOM</td><td>{db}</td><td>{da}</td><td>{dm}</td></tr>
    <tr><td>Styled</td><td>{sb}</td><td>{sa}</td><td>{sm}</td></tr>
    <tr><td>Boxes</td><td>{bb}</td><td>{ba}</td><td>{bm}</td></tr>
    <tr><td>Fragments</td><td>{fb}</td><td>{fa}</td><td>{fm}</td></tr>
    <tr><td>Display list</td><td>{lb}</td><td>{la}</td><td>{lm}</td></tr>
  </table>
</div>
"#,
    pb = escape_html(&schema.pipeline.before),
    pa = escape_html(&schema.pipeline.after),
    pm = yes_no(schema.pipeline.same_major),
    db = escape_html(&schema.dom.before),
    da = escape_html(&schema.dom.after),
    dm = yes_no(schema.dom.same_major),
    sb = escape_html(&schema.styled.before),
    sa = escape_html(&schema.styled.after),
    sm = yes_no(schema.styled.same_major),
    bb = escape_html(&schema.boxes.before),
    ba = escape_html(&schema.boxes.after),
    bm = yes_no(schema.boxes.same_major),
    fb = escape_html(&schema.fragments.before),
    fa = escape_html(&schema.fragments.after),
    fm = yes_no(schema.fragments.same_major),
    lb = escape_html(&schema.display_list.before),
    la = escape_html(&schema.display_list.after),
    lm = yes_no(schema.display_list.same_major),
  )
}

fn render_summary(summary: &PageSummary) -> String {
  let mut parts = String::new();
  parts.push_str(&render_counts(&summary.counts));
  if let Some(viewport) = &summary.viewport {
    parts.push_str(&format!(
      r#"<div class="section"><h3>Viewport</h3><div>Before: <code>{b}</code> | After: <code>{a}</code> | Δarea: {delta:+.1}</div></div>"#,
      b = escape_html(&format_rect(&viewport.before)),
      a = escape_html(&format_rect(&viewport.after)),
      delta = viewport.area_delta
    ));
  }
  if !summary.dom_changes.added.is_empty() || !summary.dom_changes.removed.is_empty() {
    parts.push_str("<div class=\"section\"><h3>DOM changes</h3>");
    if !summary.dom_changes.added.is_empty() {
      parts.push_str("<div><strong>Added:</strong><ul>");
      for node in &summary.dom_changes.added {
        parts.push_str(&format!(
          "<li><code>{}</code> — {}</li>",
          escape_html(&node.key),
          escape_html(&node.description)
        ));
      }
      parts.push_str("</ul></div>");
    }
    if !summary.dom_changes.removed.is_empty() {
      parts.push_str("<div><strong>Removed:</strong><ul>");
      for node in &summary.dom_changes.removed {
        parts.push_str(&format!(
          "<li><code>{}</code> — {}</li>",
          escape_html(&node.key),
          escape_html(&node.description)
        ));
      }
      parts.push_str("</ul></div>");
    }
    parts.push_str("</div>");
  }

  if !summary.box_changes.structure.is_empty() {
    parts.push_str("<div class=\"section\"><h3>Box tree</h3><ul>");
    for change in &summary.box_changes.structure {
      parts.push_str(&format!("<li>{}</li>", escape_html(change)));
    }
    parts.push_str("</ul></div>");
  }

  if !summary.fragment_changes.area_deltas.is_empty()
    || !summary.fragment_changes.translation_deltas.is_empty()
  {
    parts.push_str("<div class=\"section\"><h3>Fragment geometry</h3>");
    if !summary.fragment_changes.area_deltas.is_empty() {
      parts.push_str("<div><strong>Largest area deltas:</strong><table><tr><th>Key</th><th>Description</th><th>Before</th><th>After</th><th>Δarea</th><th>Move</th></tr>");
      for delta in &summary.fragment_changes.area_deltas {
        parts.push_str(&format!(
          "<tr><td><code>{}</code></td><td>{}</td><td><code>{}</code></td><td><code>{}</code></td><td>{:+.1}</td><td>{:.2}</td></tr>",
          escape_html(&delta.key),
          escape_html(&delta.description),
          escape_html(&format_rect(&delta.before)),
          escape_html(&format_rect(&delta.after)),
          delta.area_delta,
          delta.translation
        ));
      }
      parts.push_str("</table></div>");
    }
    if !summary.fragment_changes.translation_deltas.is_empty() {
      parts.push_str("<div><strong>Largest translations:</strong><table><tr><th>Key</th><th>Description</th><th>Before</th><th>After</th><th>Δarea</th><th>Move</th></tr>");
      for delta in &summary.fragment_changes.translation_deltas {
        parts.push_str(&format!(
          "<tr><td><code>{}</code></td><td>{}</td><td><code>{}</code></td><td><code>{}</code></td><td>{:+.1}</td><td>{:.2}</td></tr>",
          escape_html(&delta.key),
          escape_html(&delta.description),
          escape_html(&format_rect(&delta.before)),
          escape_html(&format_rect(&delta.after)),
          delta.area_delta,
          delta.translation
        ));
      }
      parts.push_str("</table></div>");
    }
    parts.push_str("</div>");
  }

  if !summary.display_list_changes.histogram_deltas.is_empty()
    || !summary.display_list_changes.sequence_runs.is_empty()
  {
    parts.push_str("<div class=\"section\"><h3>Display list</h3>");
    if !summary.display_list_changes.histogram_deltas.is_empty() {
      parts.push_str("<div><strong>Kind histogram changes:</strong><table><tr><th>Kind</th><th>Before</th><th>After</th><th>Δ</th></tr>");
      for delta in &summary.display_list_changes.histogram_deltas {
        parts.push_str(&format!(
          "<tr><td>{}</td><td>{}</td><td>{}</td><td>{:+}</td></tr>",
          escape_html(&delta.kind),
          delta.before,
          delta.after,
          delta.delta
        ));
      }
      parts.push_str("</table></div>");
    }
    if !summary.display_list_changes.sequence_runs.is_empty() {
      parts.push_str("<div><strong>Sequence changes:</strong><ul>");
      for run in &summary.display_list_changes.sequence_runs {
        let label = match run.change {
          RunKind::Inserted => "Inserted (after)",
          RunKind::Removed => "Removed (before)",
        };
        parts.push_str(&format!(
          "<li>{label} @ {index} ({count} items): {kinds}</li>",
          index = run.index,
          count = run.count,
          kinds = escape_html(&run.sample_kinds.join(", "))
        ));
      }
      parts.push_str("</ul></div>");
    }
    parts.push_str("</div>");
  }

  parts
}

fn render_counts(counts: &StageCountsSummary) -> String {
  format!(
    r#"<div class="section">
  <h3>Counts</h3>
  <table>
    <tr><th>Stage</th><th>Before</th><th>After</th><th>Δ</th></tr>
    {rows}
  </table>
</div>
"#,
    rows = [
      ("DOM", counts.dom),
      ("Styled", counts.styled),
      ("Boxes", counts.boxes),
      ("Fragments", counts.fragments),
      ("Display items", counts.display_items),
    ]
    .iter()
    .map(|(name, delta)| render_count_row(name, delta))
    .collect::<Vec<_>>()
    .join("")
  )
}

fn render_count_row(name: &str, delta: &CountDelta) -> String {
  format!(
    "<tr><td>{}</td><td>{}</td><td>{}</td><td>{:+}</td></tr>",
    escape_html(name),
    delta.before,
    delta.after,
    delta.delta
  )
}

fn yes_no(value: bool) -> String {
  if value {
    "yes".to_string()
  } else {
    "no".to_string()
  }
}

fn raw_child<'a>(raw: Option<&'a Value>, idx: usize) -> Option<&'a Value> {
  raw
    .and_then(|v| v.get("children"))
    .and_then(|c| c.as_array())
    .and_then(|arr| arr.get(idx))
}

fn path_string(path: &[usize]) -> String {
  if path.is_empty() {
    "root".to_string()
  } else {
    path
      .iter()
      .map(|p| p.to_string())
      .collect::<Vec<_>>()
      .join("/")
  }
}

fn truncate(input: &str, max: usize) -> String {
  if input.len() <= max {
    input.to_string()
  } else {
    format!("{}…", &input[..max])
  }
}

fn format_rect(rect: &RectSnapshot) -> String {
  format!(
    "{:.1},{:.1} {:.1}x{:.1}",
    rect.x, rect.y, rect.width, rect.height
  )
}

fn cmp_f32(a: f32, b: f32) -> Ordering {
  a.partial_cmp(&b).unwrap_or(Ordering::Equal)
}
