use crate::style::media::{
  ColorGamut, ColorScheme, ContrastPreference, DisplayMode, ForcedColors, InvertedColors,
  LightLevel, MediaType, ReducedData, ReducedMotion, ReducedTransparency, Scripting,
  UpdateFrequency,
};
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::OnceLock;
use std::sync::RwLock;

/// Parsed runtime debug/configuration toggles sourced from `FASTR_*` environment variables.
///
/// Values are captured once (via [`RuntimeToggles::from_env`]) and then reused throughout a render.
/// Callers can also construct instances manually to override environment-derived behavior when
/// embedding the library.
#[derive(Debug, Clone, Default)]
pub struct RuntimeToggles {
  raw: HashMap<String, String>,
  config: DebugConfig,
}

impl RuntimeToggles {
  /// Parse all `FASTR_*` environment variables into a toggle map.
  pub fn from_env() -> Self {
    let raw = std::env::vars()
      .filter(|(k, _)| k.starts_with("FASTR_"))
      .collect::<HashMap<_, _>>();
    let config = DebugConfig::from_env_map(&raw);
    Self { raw, config }
  }

  /// Construct a toggle set from a provided map of key/value pairs.
  pub fn from_map(raw: HashMap<String, String>) -> Self {
    let config = DebugConfig::from_env_map(&raw);
    Self { raw, config }
  }

  /// Returns parsed, typed debug configuration derived from the environment.
  pub fn config(&self) -> &DebugConfig {
    &self.config
  }

  /// Returns the raw string value for a toggle, if set.
  pub fn get(&self, key: &str) -> Option<&str> {
    self.raw.get(key).map(String::as_str)
  }

  /// Returns true when the toggle is present and truthy (`!= 0`/`false`/`off`).
  pub fn truthy(&self, key: &str) -> bool {
    self.truthy_with_default(key, false)
  }

  /// Returns true when the toggle is present and truthy, otherwise the provided default.
  pub fn truthy_with_default(&self, key: &str, default: bool) -> bool {
    if let Some(val) = self.config.bools.get(key) {
      *val
    } else {
      self
        .get(key)
        .map(|v| !matches_ignore_case(v, &["0", "false", "off"]))
        .unwrap_or(default)
    }
  }

  /// Parse a toggle as `usize`, returning `None` when unset or unparseable.
  pub fn usize(&self, key: &str) -> Option<usize> {
    if let Some(v) = self.config.usizes.get(key) {
      *v
    } else {
      self.get(key).and_then(|v| v.trim().parse::<usize>().ok())
    }
  }

  /// Parse a toggle as `usize`, falling back to a default when unset or invalid.
  pub fn usize_with_default(&self, key: &str, default: usize) -> usize {
    self.usize(key).unwrap_or(default)
  }

  /// Parse a toggle as `u64`, returning `None` when unset or unparseable.
  pub fn u64(&self, key: &str) -> Option<u64> {
    if let Some(v) = self.config.u64s.get(key) {
      *v
    } else {
      self.get(key).and_then(|v| v.trim().parse::<u64>().ok())
    }
  }

  /// Parse a toggle as `u128`, returning `None` when unset or unparseable.
  pub fn u128(&self, key: &str) -> Option<u128> {
    if let Some(v) = self.config.u128s.get(key) {
      *v
    } else {
      self.get(key).and_then(|v| v.trim().parse::<u128>().ok())
    }
  }

  /// Parse a toggle as `f64`, returning `None` when unset or unparseable.
  pub fn f64(&self, key: &str) -> Option<f64> {
    if let Some(v) = self.config.f64s.get(key) {
      *v
    } else {
      self.get(key).and_then(|v| v.trim().parse::<f64>().ok())
    }
  }

  /// Parse a comma-separated list of `usize` values.
  pub fn usize_list(&self, key: &str) -> Option<Vec<usize>> {
    if let Some(values) = self.config.usize_lists.get(key) {
      if values.is_empty() {
        None
      } else {
        Some(values.clone())
      }
    } else {
      let values = self.get(key).map(|raw| {
        raw
          .split(',')
          .filter_map(|tok| tok.trim().parse::<usize>().ok())
          .collect::<Vec<_>>()
      });
      values.filter(|v| !v.is_empty())
    }
  }

  /// Parse a comma-separated list of trimmed strings, discarding empty entries.
  pub fn string_list(&self, key: &str) -> Option<Vec<String>> {
    if let Some(values) = self.config.string_lists.get(key) {
      if values.is_empty() {
        None
      } else {
        Some(values.clone())
      }
    } else {
      let values = self.get(key).map(|raw| {
        raw
          .split(',')
          .filter_map(|tok| {
            let trimmed = tok.trim();
            if trimmed.is_empty() {
              None
            } else {
              Some(trimmed.to_string())
            }
          })
          .collect::<Vec<_>>()
      });
      values.filter(|v| !v.is_empty())
    }
  }
}

fn matches_ignore_case(value: &str, candidates: &[&str]) -> bool {
  let lower = value.trim().to_ascii_lowercase();
  candidates.iter().any(|c| lower == *c)
}

static DEFAULT_TOGGLES: OnceLock<Arc<RuntimeToggles>> = OnceLock::new();
static ACTIVE_TOGGLES: OnceLock<RwLock<Arc<RuntimeToggles>>> = OnceLock::new();

/// Returns the currently active runtime toggles.
///
/// Defaults to `RuntimeToggles::from_env()` if no overrides are installed.
pub fn runtime_toggles() -> Arc<RuntimeToggles> {
  ACTIVE_TOGGLES
    .get_or_init(|| {
      let default = default_toggles();
      RwLock::new(default)
    })
    .read()
    .expect("runtime toggles lock poisoned")
    .clone()
}

fn default_toggles() -> Arc<RuntimeToggles> {
  DEFAULT_TOGGLES
    .get_or_init(|| Arc::new(RuntimeToggles::from_env()))
    .clone()
}

/// Guard that restores the previous active toggles when dropped.
pub struct RuntimeTogglesGuard {
  previous: Arc<RuntimeToggles>,
}

impl Drop for RuntimeTogglesGuard {
  fn drop(&mut self) {
    if let Some(lock) = ACTIVE_TOGGLES.get() {
      if let Ok(mut guard) = lock.write() {
        *guard = self.previous.clone();
      }
    }
  }
}

/// Install the provided toggles as the active set for the duration of the returned guard.
pub fn set_runtime_toggles(toggles: Arc<RuntimeToggles>) -> RuntimeTogglesGuard {
  let previous = ACTIVE_TOGGLES
    .get_or_init(|| RwLock::new(default_toggles()))
    .write()
    .expect("runtime toggles lock poisoned")
    .clone();
  if let Some(lock) = ACTIVE_TOGGLES.get() {
    if let Ok(mut guard) = lock.write() {
      *guard = toggles;
    }
  }
  RuntimeTogglesGuard { previous }
}

/// Convenience helper to run a closure with a temporary toggles override.
pub fn with_runtime_toggles<T>(toggles: Arc<RuntimeToggles>, f: impl FnOnce() -> T) -> T {
  let guard = set_runtime_toggles(toggles);
  let result = f();
  drop(guard);
  result
}

#[derive(Debug, Clone, Default)]
pub struct MediaOverrides {
  pub media_type: Option<MediaType>,
  pub scripting: Option<Scripting>,
  pub update_frequency: Option<UpdateFrequency>,
  pub light_level: Option<LightLevel>,
  pub display_mode: Option<DisplayMode>,
  pub prefers_color_scheme: Option<ColorScheme>,
  pub prefers_reduced_motion: Option<bool>,
  pub prefers_contrast: Option<ContrastPreference>,
  pub prefers_reduced_transparency: Option<bool>,
  pub prefers_reduced_data: Option<bool>,
  pub color_gamut: Option<ColorGamut>,
  pub inverted_colors: Option<InvertedColors>,
  pub forced_colors: Option<bool>,
  pub color_depth: Option<u32>,
  pub color_index: Option<u32>,
  pub monochrome_depth: Option<u32>,
}

#[derive(Debug, Clone, Default)]
pub struct DebugConfig {
  pub bools: HashMap<&'static str, bool>,
  pub usizes: HashMap<&'static str, Option<usize>>,
  pub u64s: HashMap<&'static str, Option<u64>>,
  pub u128s: HashMap<&'static str, Option<u128>>,
  pub f64s: HashMap<&'static str, Option<f64>>,
  pub strings: HashMap<&'static str, Option<String>>,
  pub usize_lists: HashMap<&'static str, Vec<usize>>,
  pub string_lists: HashMap<&'static str, Vec<String>>,
  pub media: MediaOverrides,
}

impl DebugConfig {
  pub fn from_env_map(raw: &HashMap<String, String>) -> Self {
    let mut config = DebugConfig::default();
    config.insert_bool(
      "FASTR_RENDER_TIMINGS",
      raw.contains_key("FASTR_RENDER_TIMINGS"),
    );
    config.insert_bool("FASTR_FULL_PAGE", truthy(raw.get("FASTR_FULL_PAGE"), false));
    config.insert_bool(
      "FASTR_CASCADE_PROFILE",
      truthy(raw.get("FASTR_CASCADE_PROFILE"), false),
    );
    config.insert_bool(
      "FASTR_LAYOUT_PROFILE",
      truthy(raw.get("FASTR_LAYOUT_PROFILE"), false),
    );
    config.insert_bool(
      "FASTR_FLEX_PROFILE",
      truthy(raw.get("FASTR_FLEX_PROFILE"), false),
    );
    config.insert_bool(
      "FASTR_FLEX_PROFILE_HIST",
      truthy(raw.get("FASTR_FLEX_PROFILE_HIST"), false),
    );
    config.insert_bool(
      "FASTR_FLEX_PROFILE_NODES",
      truthy(raw.get("FASTR_FLEX_PROFILE_NODES"), false),
    );
    config.insert_bool(
      "FASTR_FLEX_PROFILE_NODE_KEYS",
      truthy(raw.get("FASTR_FLEX_PROFILE_NODE_KEYS"), false),
    );
    config.insert_bool(
      "FASTR_INTRINSIC_STATS",
      raw.contains_key("FASTR_INTRINSIC_STATS"),
    );
    config.insert_bool(
      "FASTR_LAYOUT_CACHE_STATS",
      raw.contains_key("FASTR_LAYOUT_CACHE_STATS"),
    );
    config.insert_bool(
      "FASTR_DISABLE_LAYOUT_CACHE",
      truthy(raw.get("FASTR_DISABLE_LAYOUT_CACHE"), false),
    );
    config.insert_bool(
      "FASTR_DISABLE_FLEX_CACHE",
      truthy(raw.get("FASTR_DISABLE_FLEX_CACHE"), false),
    );
    config.insert_bool(
      "FASTR_LOG_FRAG_BOUNDS",
      truthy(raw.get("FASTR_LOG_FRAG_BOUNDS"), false),
    );
    config.insert_bool(
      "FASTR_DUMP_COUNTS",
      truthy(raw.get("FASTR_DUMP_COUNTS"), false),
    );
    config.insert_bool("FASTR_DUMP_TEXT", truthy(raw.get("FASTR_DUMP_TEXT"), false));
    config.insert_bool(
      "FASTR_DUMP_STACK",
      truthy(raw.get("FASTR_DUMP_STACK"), false),
    );
    config.insert_bool(
      "FASTR_DUMP_FRAGMENTS",
      truthy(raw.get("FASTR_DUMP_FRAGMENTS"), false),
    );
    config.insert_bool(
      "FASTR_DUMP_TABLE",
      truthy(raw.get("FASTR_DUMP_TABLE"), false),
    );
    config.insert_bool(
      "FASTR_PAINT_STATS",
      truthy(raw.get("FASTR_PAINT_STATS"), false),
    );
    config.insert_bool("FASTR_FAST_BLUR", truthy(raw.get("FASTR_FAST_BLUR"), false));
    config.insert_bool(
      "FASTR_LOG_ABS_CLAMP",
      truthy(raw.get("FASTR_LOG_ABS_CLAMP"), false),
    );
    config.insert_bool(
      "FASTR_LOG_IMAGE_FAIL",
      truthy(raw.get("FASTR_LOG_IMAGE_FAIL"), false),
    );
    config.insert_bool(
      "FASTR_LOG_INLINE_BASELINE",
      truthy(raw.get("FASTR_LOG_INLINE_BASELINE"), false),
    );
    config.insert_bool(
      "FASTR_LOG_LINE_WIDTH",
      truthy(raw.get("FASTR_LOG_LINE_WIDTH"), false),
    );
    config.insert_bool(
      "FASTR_LOG_OVERFLOW_TEST",
      raw.contains_key("FASTR_LOG_OVERFLOW_TEST"),
    );
    config.insert_bool(
      "FASTR_REPLACED_INTRINSIC_PROFILE",
      truthy(raw.get("FASTR_REPLACED_INTRINSIC_PROFILE"), false),
    );
    config.insert_bool(
      "FASTR_LOG_CONTAINER_FIELDS",
      truthy(raw.get("FASTR_LOG_CONTAINER_FIELDS"), false),
    );
    config.insert_bool(
      "FASTR_LOG_CONTAINER_PASS",
      truthy(raw.get("FASTR_LOG_CONTAINER_PASS"), false),
    );
    config.insert_bool(
      "FASTR_LOG_CONTAINER_REUSE",
      truthy(raw.get("FASTR_LOG_CONTAINER_REUSE"), false),
    );
    config.insert_bool(
      "FASTR_LOG_CONTAINER_QUERY",
      raw.contains_key("FASTR_LOG_CONTAINER_QUERY"),
    );
    config.insert_bool(
      "FASTR_LOG_CSS_LINKS",
      raw.contains_key("FASTR_LOG_CSS_LINKS"),
    );
    config.insert_bool(
      "FASTR_TRACE_GRID_LAYOUT",
      truthy(raw.get("FASTR_TRACE_GRID_LAYOUT"), false),
    );
    config.insert_bool(
      "FASTR_DEBUG_GRID_BASELINE",
      raw.contains_key("FASTR_DEBUG_GRID_BASELINE"),
    );
    config.insert_bool(
      "FASTR_DISPLAY_LIST_PARALLEL",
      truthy(raw.get("FASTR_DISPLAY_LIST_PARALLEL"), true),
    );
    config.insert_bool(
      "FASTR_DUMP_CELL_CHILD_Y",
      truthy(raw.get("FASTR_DUMP_CELL_CHILD_Y"), false),
    );
    config.insert_bool(
      "FASTR_LOG_WIDE_FLEX",
      raw.contains_key("FASTR_LOG_WIDE_FLEX"),
    );
    config.insert_bool(
      "FASTR_LOG_NARROW_FLEX",
      truthy(raw.get("FASTR_LOG_NARROW_FLEX"), false),
    );
    config.insert_bool(
      "FASTR_LOG_SKINNY_FLEX",
      truthy(raw.get("FASTR_LOG_SKINNY_FLEX"), false),
    );
    config.insert_bool(
      "FASTR_LOG_SMALL_FLEX",
      truthy(raw.get("FASTR_LOG_SMALL_FLEX"), false),
    );
    config.insert_bool(
      "FASTR_LOG_SMALL_BLOCK",
      truthy(raw.get("FASTR_LOG_SMALL_BLOCK"), false),
    );
    config.insert_bool(
      "FASTR_LOG_SHRINK_TO_FIT",
      truthy(raw.get("FASTR_LOG_SHRINK_TO_FIT"), false),
    );
    config.insert_bool(
      "FASTR_LOG_BLOCK_WIDE",
      truthy(raw.get("FASTR_LOG_BLOCK_WIDE"), false),
    );
    config.insert_bool(
      "FASTR_LOG_FLEX_CHILD",
      truthy(raw.get("FASTR_LOG_FLEX_CHILD"), false),
    );
    config.insert_bool(
      "FASTR_LOG_FLEX_OVERFLOW",
      truthy(raw.get("FASTR_LOG_FLEX_OVERFLOW"), false),
    );
    config.insert_bool(
      "FASTR_LOG_FLEX_DRIFT",
      truthy(raw.get("FASTR_LOG_FLEX_DRIFT"), false),
    );
    config.insert_bool(
      "FASTR_FETCH_LINK_CSS",
      truthy(raw.get("FASTR_FETCH_LINK_CSS"), true),
    );
    config.insert_bool(
      "FASTR_LOG_VIEWPORT_OVERLAP",
      truthy(raw.get("FASTR_LOG_VIEWPORT_OVERLAP"), false),
    );

    config.insert_usize("FASTR_DISPLAY_LIST_PARALLEL_MIN", {
      let parsed = raw
        .get("FASTR_DISPLAY_LIST_PARALLEL_MIN")
        .and_then(|v| v.trim().parse::<usize>().ok())
        .filter(|v| *v > 0)
        .unwrap_or(32);
      Some(parsed)
    });
    config.insert_usize(
      "FASTR_DUMP_TEXT_FRAGMENTS",
      parse_limit_toggle(raw.get("FASTR_DUMP_TEXT_FRAGMENTS"), 20),
    );
    config.insert_usize(
      "FASTR_DUMP_TEXT_ITEMS",
      parse_limit_toggle(raw.get("FASTR_DUMP_TEXT_ITEMS"), 20),
    );
    config.insert_usize(
      "FASTR_DUMP_COMMANDS",
      parse_limit_toggle(raw.get("FASTR_DUMP_COMMANDS"), 50),
    );
    config.insert_usize(
      "FASTR_TRACE_IMAGE_PAINT",
      parse_limit_toggle(raw.get("FASTR_TRACE_IMAGE_PAINT"), 50),
    );
    config.insert_usize(
      "FASTR_LOG_BLOCK_PROGRESS_MS",
      raw
        .get("FASTR_LOG_BLOCK_PROGRESS_MS")
        .and_then(|v| v.parse().ok()),
    );
    config.insert_usize(
      "FASTR_LOG_BLOCK_PROGRESS_MAX",
      raw
        .get("FASTR_LOG_BLOCK_PROGRESS_MAX")
        .and_then(|v| v.parse().ok())
        .or(Some(10)),
    );
    config.insert_usize(
      "FASTR_LOG_BLOCK_PROGRESS_TOTAL_MAX",
      raw
        .get("FASTR_LOG_BLOCK_PROGRESS_TOTAL_MAX")
        .and_then(|v| v.parse().ok())
        .or(Some(50)),
    );
    config.insert_usize(
      "FASTR_LOG_CONTAINER_DIFF",
      raw
        .get("FASTR_LOG_CONTAINER_DIFF")
        .and_then(|v| v.parse().ok()),
    );
    config.insert_usize(
      "FASTR_LOG_FLEX_CONSTRAINTS_MAX",
      raw
        .get("FASTR_LOG_FLEX_CONSTRAINTS_MAX")
        .and_then(|v| v.parse().ok())
        .or(Some(10)),
    );
    config.insert_usize(
      "FASTR_LOG_FLEX_FIRST_N",
      raw
        .get("FASTR_LOG_FLEX_FIRST_N")
        .and_then(|v| v.parse().ok())
        .or(Some(0)),
    );
    config.insert_usize(
      "FASTR_LOG_FLEX_NODE_KEYS_MAX",
      raw
        .get("FASTR_LOG_FLEX_NODE_KEYS_MAX")
        .and_then(|v| v.parse().ok())
        .or(Some(10)),
    );
    config.insert_usize(
      "FASTR_LOG_FLEX_MEASURE_FIRST_N",
      raw
        .get("FASTR_LOG_FLEX_MEASURE_FIRST_N")
        .and_then(|v| v.parse().ok())
        .or(Some(0)),
    );
    config.insert_usize(
      "FASTR_FLEX_PROFILE_NODE_KEY_CAP",
      raw
        .get("FASTR_FLEX_PROFILE_NODE_KEY_CAP")
        .and_then(|v| v.parse().ok())
        .or(Some(20000)),
    );
    config.insert_usize(
      "FASTR_FLEX_PROFILE_NODES_TOP",
      raw
        .get("FASTR_FLEX_PROFILE_NODES_TOP")
        .and_then(|v| v.parse().ok())
        .or(Some(10)),
    );
    config.insert_usize(
      "FASTR_TRACE_GRID_TEXT",
      raw
        .get("FASTR_TRACE_GRID_TEXT")
        .and_then(|v| v.parse().ok()),
    );

    config.insert_u64(
      "FASTR_FLEX_PROFILE_PROGRESS",
      parse_progress_interval(raw.get("FASTR_FLEX_PROFILE_PROGRESS")),
    );

    config.insert_u128(
      "FASTR_LOG_SLOW_LAYOUT_MS",
      raw
        .get("FASTR_LOG_SLOW_LAYOUT_MS")
        .and_then(|v| v.parse().ok()),
    );
    config.insert_u128(
      "FASTR_LOG_FLEX_MEASURE_FIRST_N_MS",
      raw
        .get("FASTR_LOG_FLEX_MEASURE_FIRST_N_MS")
        .and_then(|v| v.parse().ok()),
    );

    config.insert_f64(
      "FASTR_IMAGE_PROFILE_MS",
      parse_non_negative(raw.get("FASTR_IMAGE_PROFILE_MS")),
    );
    config.insert_f64(
      "FASTR_STACK_PROFILE_MS",
      parse_non_negative(raw.get("FASTR_STACK_PROFILE_MS")),
    );
    config.insert_f64(
      "FASTR_TEXT_PROFILE_MS",
      parse_profile_toggle(raw.get("FASTR_TEXT_PROFILE_MS")),
    );
    config.insert_f64(
      "FASTR_CMD_PROFILE_MS",
      parse_profile_toggle(raw.get("FASTR_CMD_PROFILE_MS")),
    );

    config.insert_strings("FASTR_TRACE_TEXT", raw.get("FASTR_TRACE_TEXT").cloned());
    config.insert_strings(
      "FASTR_FIND_BOX_TEXT",
      raw.get("FASTR_FIND_BOX_TEXT").cloned(),
    );
    config.insert_strings("FASTR_FIND_TEXT", raw.get("FASTR_FIND_TEXT").cloned());
    config.insert_strings("FASTR_INSPECT_MASK", raw.get("FASTR_INSPECT_MASK").cloned());
    config.insert_strings("FASTR_NEEDLE", raw.get("FASTR_NEEDLE").cloned());

    config.insert_usize_list(
      "FASTR_TRACE_FLEX_TEXT",
      parse_usize_list(raw.get("FASTR_TRACE_FLEX_TEXT")),
    );
    config.insert_usize_list(
      "FASTR_LOG_FLEX_CONSTRAINTS",
      parse_usize_list(raw.get("FASTR_LOG_FLEX_CONSTRAINTS")),
    );
    config.insert_bool(
      "FASTR_TRACE_FLEX",
      truthy(raw.get("FASTR_TRACE_FLEX"), false),
    );
    config.insert_usize_list(
      "FASTR_TRACE_BOXES",
      parse_usize_list(raw.get("FASTR_TRACE_BOXES")),
    );
    config.insert_usize_list(
      "FASTR_TRACE_BOX_INFO",
      parse_usize_list(raw.get("FASTR_TRACE_BOX_INFO")),
    );
    config.insert_usize_list(
      "FASTR_TRACE_POSITIONED",
      parse_usize_list(raw.get("FASTR_TRACE_POSITIONED")),
    );
    config.insert_usize_list(
      "FASTR_TRACE_BLOCK_TEXT",
      parse_usize_list(raw.get("FASTR_TRACE_BLOCK_TEXT")),
    );
    config.insert_usize_list(
      "FASTR_LOG_CONTAINER_IDS",
      parse_usize_list(raw.get("FASTR_LOG_CONTAINER_IDS")),
    );
    config.insert_usize_list(
      "FASTR_LOG_BLOCK_PROGRESS_IDS",
      parse_usize_list(raw.get("FASTR_LOG_BLOCK_PROGRESS_IDS")),
    );
    config.insert_string_list(
      "FASTR_LOG_BLOCK_PROGRESS_MATCH",
      parse_string_list(raw.get("FASTR_LOG_BLOCK_PROGRESS_MATCH")),
    );
    config.insert_usize_list(
      "FASTR_LOG_FLEX_CHILD_IDS",
      parse_usize_list(raw.get("FASTR_LOG_FLEX_CHILD_IDS")),
    );
    config.insert_usize_list(
      "FASTR_LOG_FLEX_IDS",
      parse_usize_list(raw.get("FASTR_LOG_FLEX_IDS")),
    );
    config.insert_usize_list(
      "FASTR_LOG_FLEX_MEASURE_IDS",
      parse_usize_list(raw.get("FASTR_LOG_FLEX_MEASURE_IDS")),
    );
    config.insert_usize_list(
      "FASTR_LOG_FLEX_NODE_KEYS",
      parse_usize_list(raw.get("FASTR_LOG_FLEX_NODE_KEYS")),
    );
    config.insert_usize_list(
      "FASTR_LOG_FLEX_OVERFLOW_IDS",
      parse_usize_list(raw.get("FASTR_LOG_FLEX_OVERFLOW_IDS")),
    );
    config.insert_usize_list(
      "FASTR_LOG_FLEX_SHRINK_IDS",
      parse_usize_list(raw.get("FASTR_LOG_FLEX_SHRINK_IDS")),
    );
    config.insert_usize_list(
      "FASTR_LOG_INTRINSIC_IDS",
      parse_usize_list(raw.get("FASTR_LOG_INTRINSIC_IDS")),
    );
    config.insert_f64(
      "FASTR_LOG_LARGE_FLEX",
      raw
        .get("FASTR_LOG_LARGE_FLEX")
        .and_then(|v| v.parse::<f64>().ok())
        .filter(|v| *v > 0.0),
    );

    config.insert_bool(
      "FASTR_LOG_FLEX_ROOT",
      parse_presence_bool(raw.get("FASTR_LOG_FLEX_ROOT")),
    );
    config.insert_bool(
      "FASTR_LOG_SKINNY_FLEX",
      truthy(raw.get("FASTR_LOG_SKINNY_FLEX"), false),
    );
    config.insert_bool(
      "FASTR_LOG_SMALL_FLEX",
      truthy(raw.get("FASTR_LOG_SMALL_FLEX"), false),
    );
    config.insert_bool(
      "FASTR_LOG_FLEX_OVERFLOW",
      truthy(raw.get("FASTR_LOG_FLEX_OVERFLOW"), false),
    );
    config.insert_bool(
      "FASTR_LOG_FLEX_DRIFT",
      truthy(raw.get("FASTR_LOG_FLEX_DRIFT"), false),
    );
    config.insert_bool(
      "FASTR_ABORT_FLEX_AFTER_FIRST_N",
      parse_presence_bool(raw.get("FASTR_ABORT_FLEX_AFTER_FIRST_N")),
    );
    config.insert_bool(
      "FASTR_DEBUG_FLEX_CHILD",
      truthy(raw.get("FASTR_DEBUG_FLEX_CHILD"), false),
    );

    config.insert_usize(
      "FASTR_MAX_WEB_FONTS",
      raw
        .get("FASTR_MAX_WEB_FONTS")
        .and_then(|v| v.parse().ok())
        .or(Some(8)),
    );

    config.insert_strings(
      "FASTR_DUMP_FRAGMENT",
      raw.get("FASTR_DUMP_FRAGMENT").cloned(),
    );

    config.media = parse_media_overrides(raw);
    config
  }

  fn insert_bool(&mut self, key: &'static str, value: bool) {
    self.bools.insert(key, value);
  }

  fn insert_usize(&mut self, key: &'static str, value: Option<usize>) {
    self.usizes.insert(key, value);
  }

  fn insert_u64(&mut self, key: &'static str, value: Option<u64>) {
    self.u64s.insert(key, value);
  }

  fn insert_u128(&mut self, key: &'static str, value: Option<u128>) {
    self.u128s.insert(key, value);
  }

  fn insert_f64(&mut self, key: &'static str, value: Option<f64>) {
    self.f64s.insert(key, value);
  }

  fn insert_strings(&mut self, key: &'static str, value: Option<String>) {
    self.strings.insert(key, value);
  }

  fn insert_usize_list(&mut self, key: &'static str, value: Vec<usize>) {
    self.usize_lists.insert(key, value);
  }

  fn insert_string_list(&mut self, key: &'static str, value: Vec<String>) {
    self.string_lists.insert(key, value);
  }
}

fn parse_media_overrides(raw: &HashMap<String, String>) -> MediaOverrides {
  let mut overrides = MediaOverrides::default();
  if let Some(value) = raw.get("FASTR_MEDIA_TYPE") {
    if let Ok(mt) = MediaType::parse(value) {
      overrides.media_type = Some(mt);
    }
  }
  if let Some(value) = raw.get("FASTR_SCRIPTING") {
    overrides.scripting = Scripting::parse(value).ok().or_else(|| {
      let v = value.trim().to_ascii_lowercase();
      if matches!(v.as_str(), "0" | "false" | "off" | "none") {
        Some(Scripting::None)
      } else if matches!(v.as_str(), "1" | "true" | "yes" | "on") {
        Some(Scripting::Enabled)
      } else {
        None
      }
    });
  }
  if let Some(value) = raw.get("FASTR_UPDATE_FREQUENCY") {
    overrides.update_frequency = UpdateFrequency::parse(value).ok();
  }
  if let Some(value) = raw.get("FASTR_LIGHT_LEVEL") {
    overrides.light_level = LightLevel::parse(value).ok();
  }
  if let Some(value) = raw.get("FASTR_DISPLAY_MODE") {
    overrides.display_mode = DisplayMode::parse(value).ok();
  }
  if let Some(value) = raw.get("FASTR_PREFERS_COLOR_SCHEME") {
    let v = value.trim().to_ascii_lowercase();
    overrides.prefers_color_scheme = match v.as_str() {
      "light" => Some(ColorScheme::Light),
      "dark" => Some(ColorScheme::Dark),
      "no-preference" => Some(ColorScheme::NoPreference),
      _ => None,
    };
  }
  if let Some(value) = raw.get("FASTR_PREFERS_REDUCED_MOTION") {
    let v = value.trim().to_ascii_lowercase();
    overrides.prefers_reduced_motion = Some(
      matches!(
        v.as_str(),
        "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
      ) || matches!(ReducedMotion::parse(&v), Ok(ReducedMotion::Reduce)),
    );
  }
  if let Some(value) = raw.get("FASTR_PREFERS_CONTRAST") {
    overrides.prefers_contrast = ContrastPreference::parse(value).ok();
  }
  if let Some(value) = raw.get("FASTR_PREFERS_REDUCED_TRANSPARENCY") {
    let v = value.trim().to_ascii_lowercase();
    overrides.prefers_reduced_transparency = Some(
      matches!(
        v.as_str(),
        "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
      ) || matches!(
        ReducedTransparency::parse(&v),
        Ok(ReducedTransparency::Reduce)
      ),
    );
  }
  if let Some(value) = raw.get("FASTR_PREFERS_REDUCED_DATA") {
    let v = value.trim().to_ascii_lowercase();
    overrides.prefers_reduced_data = Some(
      matches!(
        v.as_str(),
        "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
      ) || matches!(ReducedData::parse(&v), Ok(ReducedData::Reduce)),
    );
  }
  if let Some(value) = raw.get("FASTR_COLOR_GAMUT") {
    overrides.color_gamut = ColorGamut::parse(value).ok();
  }
  if let Some(value) = raw.get("FASTR_INVERTED_COLORS") {
    let v = value.trim().to_ascii_lowercase();
    overrides.inverted_colors = match InvertedColors::parse(&v) {
      Ok(state) => Some(state),
      Err(_) if matches!(v.as_str(), "1" | "true" | "yes" | "on") => Some(InvertedColors::Inverted),
      Err(_) if matches!(v.as_str(), "0" | "false" | "off" | "none") => Some(InvertedColors::None),
      _ => None,
    };
  }
  if let Some(value) = raw.get("FASTR_FORCED_COLORS") {
    let v = value.trim().to_ascii_lowercase();
    overrides.forced_colors = match ForcedColors::parse(&v) {
      Ok(ForcedColors::Active) => Some(true),
      Ok(ForcedColors::None) => Some(false),
      Err(_) => Some(matches!(v.as_str(), "1" | "true" | "yes" | "on")),
    };
  }
  if let Some(value) = raw.get("FASTR_COLOR_DEPTH") {
    overrides.color_depth = value.trim().parse::<u32>().ok();
  }
  if let Some(value) = raw.get("FASTR_COLOR_INDEX") {
    overrides.color_index = value.trim().parse::<u32>().ok();
  }
  if let Some(value) = raw.get("FASTR_MONOCHROME_DEPTH") {
    overrides.monochrome_depth = value.trim().parse::<u32>().ok();
  }
  overrides
}

fn truthy(raw: Option<&String>, default: bool) -> bool {
  raw
    .map(|v| !matches_ignore_case(v, &["0", "false", "off"]))
    .unwrap_or(default)
}

fn parse_limit_toggle(raw: Option<&String>, default: usize) -> Option<usize> {
  let val = raw?;
  let trimmed = val.trim().to_ascii_lowercase();
  if trimmed.is_empty() {
    return Some(default);
  }
  if matches!(trimmed.as_str(), "0" | "false" | "off") {
    return None;
  }
  val.parse::<usize>().ok().or(Some(default))
}

fn parse_usize_list(raw: Option<&String>) -> Vec<usize> {
  raw
    .map(|raw| {
      raw
        .split(',')
        .filter_map(|tok| tok.trim().parse::<usize>().ok())
        .collect::<Vec<_>>()
    })
    .unwrap_or_default()
}

fn parse_string_list(raw: Option<&String>) -> Vec<String> {
  raw
    .map(|raw| {
      raw
        .split(',')
        .filter_map(|tok| {
          let trimmed = tok.trim();
          if trimmed.is_empty() {
            None
          } else {
            Some(trimmed.to_string())
          }
        })
        .collect::<Vec<_>>()
    })
    .unwrap_or_default()
}

fn parse_non_negative(raw: Option<&String>) -> Option<f64> {
  let trimmed = raw?.trim();
  if trimmed.is_empty() {
    return None;
  }
  trimmed.parse::<f64>().ok().filter(|v| *v >= 0.0)
}

fn parse_profile_toggle(raw: Option<&String>) -> Option<f64> {
  let trimmed = raw?.trim();
  if trimmed.is_empty() || matches!(trimmed, "0" | "false" | "off") {
    return None;
  }
  trimmed.parse::<f64>().ok().filter(|v| *v >= 0.0)
}

fn parse_progress_interval(raw: Option<&String>) -> Option<u64> {
  let val = if let Some(raw) = raw {
    if raw.trim().is_empty() {
      Some(100_000)
    } else {
      raw.parse().ok()
    }
  } else {
    None
  }
  .unwrap_or(0);
  if val == 0 {
    None
  } else {
    Some(val)
  }
}

fn parse_presence_bool(raw: Option<&String>) -> bool {
  raw
    .map(|v| !matches_ignore_case(v, &["0", "false", "off"]))
    .unwrap_or(false)
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::collections::HashMap;

  struct EnvGuard {
    vars: Vec<(String, Option<String>)>,
  }

  impl EnvGuard {
    fn set(pairs: &[(&str, &str)]) -> Self {
      let vars = pairs
        .iter()
        .map(|(key, val)| {
          let key = key.to_string();
          let prev = std::env::var(key.clone()).ok();
          std::env::set_var(key.clone(), val);
          (key, prev)
        })
        .collect();
      Self { vars }
    }
  }

  impl Drop for EnvGuard {
    fn drop(&mut self) {
      for (key, prev) in self.vars.iter().rev() {
        if let Some(val) = prev {
          std::env::set_var(key, val);
        } else {
          std::env::remove_var(key);
        }
      }
    }
  }

  #[test]
  fn parses_debug_config_from_map() {
    let raw = HashMap::from([
      ("FASTR_RENDER_TIMINGS".to_string(), "1".to_string()),
      ("FASTR_DISPLAY_LIST_PARALLEL".to_string(), "0".to_string()),
      (
        "FASTR_DISPLAY_LIST_PARALLEL_MIN".to_string(),
        "4".to_string(),
      ),
      ("FASTR_TRACE_IMAGE_PAINT".to_string(), "".to_string()),
      ("FASTR_LOG_SLOW_LAYOUT_MS".to_string(), "150".to_string()),
      ("FASTR_PREFERS_COLOR_SCHEME".to_string(), "dark".to_string()),
      (
        "FASTR_PREFERS_REDUCED_DATA".to_string(),
        "reduce".to_string(),
      ),
    ]);
    let toggles = RuntimeToggles::from_map(raw);

    assert!(toggles.truthy("FASTR_RENDER_TIMINGS"));
    assert!(!toggles.truthy_with_default("FASTR_DISPLAY_LIST_PARALLEL", true));
    assert_eq!(
      toggles.usize_with_default("FASTR_DISPLAY_LIST_PARALLEL_MIN", 32),
      4
    );
    assert_eq!(toggles.usize("FASTR_TRACE_IMAGE_PAINT"), Some(50));
    assert_eq!(toggles.u128("FASTR_LOG_SLOW_LAYOUT_MS"), Some(150));

    let media = &toggles.config().media;
    assert_eq!(media.prefers_color_scheme, Some(ColorScheme::Dark));
    assert_eq!(media.prefers_reduced_data, Some(true));
  }

  #[test]
  fn parses_debug_config_from_env() {
    let _guard = EnvGuard::set(&[
      ("FASTR_RENDER_TIMINGS", "1"),
      ("FASTR_DISPLAY_LIST_PARALLEL", "0"),
      ("FASTR_TRACE_TEXT", "needle"),
      ("FASTR_TRACE_FLEX_TEXT", "4,5"),
      ("FASTR_LOG_SLOW_LAYOUT_MS", "250"),
      ("FASTR_PREFERS_COLOR_SCHEME", "dark"),
    ]);

    let toggles = RuntimeToggles::from_env();

    assert!(toggles.truthy("FASTR_RENDER_TIMINGS"));
    assert!(!toggles.truthy_with_default("FASTR_DISPLAY_LIST_PARALLEL", true));
    assert_eq!(toggles.get("FASTR_TRACE_TEXT"), Some("needle"));
    assert_eq!(toggles.usize_list("FASTR_TRACE_FLEX_TEXT"), Some(vec![4, 5]));
    assert_eq!(toggles.u128("FASTR_LOG_SLOW_LAYOUT_MS"), Some(250));
    assert_eq!(toggles.config().media.prefers_color_scheme, Some(ColorScheme::Dark));
  }
}
