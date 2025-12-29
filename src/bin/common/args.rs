use clap::{Args, ValueEnum};
use fastrender::compat::CompatProfile;
use fastrender::dom::DomCompatibilityMode;
use fastrender::image_output::OutputFormat;
use fastrender::layout::engine::LayoutParallelism;
use fastrender::layout::engine::DEFAULT_LAYOUT_MIN_FANOUT;
use fastrender::style::media::MediaType;

#[derive(Debug, Clone, Args)]
pub struct ViewportArgs {
  /// Viewport size as WxH (e.g., 1200x800)
  #[arg(long, value_parser = parse_viewport, default_value = "1200x800")]
  pub viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset
  #[arg(long, default_value = "1.0")]
  pub dpr: f32,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, ValueEnum)]
pub enum MediaTypeArg {
  Screen,
  Print,
}

impl MediaTypeArg {
  pub fn as_media_type(self) -> MediaType {
    match self {
      MediaTypeArg::Screen => MediaType::Screen,
      MediaTypeArg::Print => MediaType::Print,
    }
  }
}

#[derive(Debug, Clone, Args)]
pub struct MediaArgs {
  /// Media type for evaluating media queries
  #[arg(long, value_enum, default_value_t = MediaTypeArg::Screen)]
  pub media: MediaTypeArg,

  #[command(flatten)]
  pub prefs: MediaPreferenceArgs,
}

impl MediaArgs {
  pub fn media_type(&self) -> MediaType {
    self.media.as_media_type()
  }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, ValueEnum)]
pub enum CompatProfileArg {
  Standards,
  Site,
}

impl CompatProfileArg {
  pub fn as_profile(self) -> CompatProfile {
    match self {
      CompatProfileArg::Standards => CompatProfile::Standards,
      CompatProfileArg::Site => CompatProfile::SiteCompatibility,
    }
  }

  pub fn as_str(self) -> &'static str {
    match self {
      CompatProfileArg::Standards => "standards",
      CompatProfileArg::Site => "site",
    }
  }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, ValueEnum)]
pub enum DomCompatArg {
  Standard,
  Compat,
}

impl DomCompatArg {
  pub fn as_mode(self) -> DomCompatibilityMode {
    match self {
      DomCompatArg::Standard => DomCompatibilityMode::Standard,
      DomCompatArg::Compat => DomCompatibilityMode::Compatibility,
    }
  }

  pub fn as_str(self) -> &'static str {
    match self {
      DomCompatArg::Standard => "standard",
      DomCompatArg::Compat => "compat",
    }
  }
}

#[derive(Debug, Clone, Args, Default)]
pub struct CompatArgs {
  /// Compatibility profile (defaults to spec-only standards mode)
  #[arg(long = "compat-profile", value_enum)]
  pub compat_profile: Option<CompatProfileArg>,

  /// Apply DOM compatibility mutations after parsing (default: standard/spec)
  #[arg(long = "dom-compat", value_enum)]
  pub dom_compat: Option<DomCompatArg>,
}

impl CompatArgs {
  pub fn compat_profile(&self) -> CompatProfile {
    self
      .compat_profile
      .unwrap_or(CompatProfileArg::Standards)
      .as_profile()
  }

  pub fn dom_compat_mode(&self) -> DomCompatibilityMode {
    self.dom_compat.unwrap_or(DomCompatArg::Standard).as_mode()
  }

  pub fn compat_profile_arg(&self) -> Option<CompatProfileArg> {
    self.compat_profile
  }

  pub fn dom_compat_arg(&self) -> Option<DomCompatArg> {
    self.dom_compat
  }
}

#[derive(Debug, Clone, Args)]
pub struct MediaPreferenceArgs {
  /// Reduced transparency preference (reduce|no-preference)
  #[arg(long, value_parser = parse_bool_preference)]
  pub prefers_reduced_transparency: Option<bool>,

  /// Reduced motion preference (reduce|no-preference)
  #[arg(long, value_parser = parse_bool_preference)]
  pub prefers_reduced_motion: Option<bool>,

  /// Reduced data preference (reduce|no-preference)
  #[arg(long, value_parser = parse_bool_preference)]
  pub prefers_reduced_data: Option<bool>,

  /// Contrast preference (more|high|less|low|custom|forced|no-preference)
  #[arg(long, value_parser = parse_contrast)]
  pub prefers_contrast: Option<String>,

  /// Color scheme preference (light|dark|no-preference)
  #[arg(long, value_parser = parse_color_scheme)]
  pub prefers_color_scheme: Option<String>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, ValueEnum)]
pub enum OutputFormatArg {
  Png,
  Jpeg,
  Webp,
}

#[derive(Debug, Clone, Args)]
pub struct OutputFormatArgs {
  /// Output format for encoded images
  #[arg(long, value_enum, default_value_t = OutputFormatArg::Png)]
  pub format: OutputFormatArg,

  /// Encoding quality for JPEG/WebP (0-100)
  #[arg(long, default_value = "90", value_parser = parse_quality)]
  pub quality: u8,
}

impl OutputFormatArgs {
  pub fn output_format(&self) -> OutputFormat {
    match self.format {
      OutputFormatArg::Png => OutputFormat::Png,
      OutputFormatArg::Jpeg => OutputFormat::Jpeg(self.quality),
      OutputFormatArg::Webp => OutputFormat::WebP(self.quality),
    }
  }

  pub fn extension(&self) -> &'static str {
    match self.format {
      OutputFormatArg::Png => "png",
      OutputFormatArg::Jpeg => "jpeg",
      OutputFormatArg::Webp => "webp",
    }
  }
}

#[derive(Debug, Clone, Args)]
pub struct BaseUrlArgs {
  /// Override the base URL used to resolve relative links
  #[arg(long)]
  pub base_url: Option<String>,
}

#[derive(Debug, Clone, Args)]
pub struct AllowPartialArgs {
  /// Return placeholder output when the document fetch fails
  #[arg(long)]
  pub allow_partial: bool,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, ValueEnum)]
pub enum LayoutParallelModeArg {
  Off,
  On,
  Auto,
}

#[derive(Debug, Clone, Args)]
pub struct LayoutParallelArgs {
  /// Layout fan-out mode (off|on|auto)
  #[arg(
    long,
    value_enum,
    default_value_t = LayoutParallelModeArg::Off,
    default_missing_value = "on",
    num_args = 0..=1
  )]
  pub layout_parallel: LayoutParallelModeArg,

  /// Minimum independent siblings before spawning layout threads
  #[arg(long, default_value_t = DEFAULT_LAYOUT_MIN_FANOUT, value_name = "N")]
  pub layout_parallel_min_fanout: usize,

  /// Maximum rayon worker threads used for layout fan-out
  #[arg(long, value_name = "N")]
  pub layout_parallel_max_threads: Option<usize>,

  /// Minimum box nodes before auto layout fan-out engages
  #[arg(long, value_name = "N")]
  pub layout_parallel_auto_min_nodes: Option<usize>,
}

impl LayoutParallelArgs {
  pub fn parallelism(&self) -> Option<LayoutParallelism> {
    let mut parallelism = match self.layout_parallel {
      LayoutParallelModeArg::Off => return None,
      LayoutParallelModeArg::On => LayoutParallelism::enabled(self.layout_parallel_min_fanout),
      LayoutParallelModeArg::Auto => LayoutParallelism::auto(self.layout_parallel_min_fanout),
    };
    parallelism = parallelism
      .with_min_fanout(self.layout_parallel_min_fanout)
      .with_max_threads(self.layout_parallel_max_threads);
    if let Some(min_nodes) = self.layout_parallel_auto_min_nodes {
      parallelism = parallelism.with_auto_min_nodes(min_nodes);
    }
    Some(parallelism)
  }
}

#[derive(Debug, Clone, Args)]
pub struct ResourceAccessArgs {
  /// Allow HTTP(S) documents to load file:// subresources
  #[arg(long)]
  pub allow_file_from_http: bool,

  /// Block mixed HTTP subresources when rendering HTTPS documents
  #[arg(long)]
  pub block_mixed_content: bool,

  /// Restrict subresource loads to the document origin unless allowlisted.
  #[arg(long)]
  pub same_origin_subresources: bool,

  /// Allow additional origins when blocking cross-origin subresources (repeatable).
  #[arg(long, value_name = "ORIGIN")]
  pub allow_subresource_origin: Vec<String>,
}

#[derive(Debug, Clone, Args)]
pub struct TimeoutArgs {
  /// Timeout in seconds (0 = no timeout)
  #[arg(long)]
  pub timeout: Option<u64>,
}

impl TimeoutArgs {
  pub fn seconds(&self, default: Option<u64>) -> Option<u64> {
    let value = self.timeout.or(default)?;
    if value == 0 {
      None
    } else {
      Some(value)
    }
  }
}

pub fn parse_viewport(s: &str) -> Result<(u32, u32), String> {
  let parts: Vec<&str> = s.split('x').collect();
  if parts.len() != 2 {
    return Err("viewport must be WxH (e.g., 1200x800)".to_string());
  }
  let w = parts[0].parse::<u32>().map_err(|_| "invalid width")?;
  let h = parts[1].parse::<u32>().map_err(|_| "invalid height")?;
  if w == 0 || h == 0 {
    return Err("width and height must be > 0".to_string());
  }
  Ok((w, h))
}

pub fn parse_bool_preference(s: &str) -> Result<bool, String> {
  let v = s.trim().to_ascii_lowercase();
  if matches!(
    v.as_str(),
    "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
  ) {
    return Ok(true);
  }
  if matches!(
    v.as_str(),
    "0" | "false" | "no" | "off" | "none" | "no-preference"
  ) {
    return Ok(false);
  }
  Err(format!("invalid value: {s}"))
}

pub fn parse_contrast(s: &str) -> Result<String, String> {
  let v = s.trim().to_ascii_lowercase();
  match v.as_str() {
    "more" | "high" | "less" | "low" | "custom" | "forced" | "no-preference" => Ok(v),
    _ => Err(format!("invalid contrast value: {s}")),
  }
}

pub fn parse_color_scheme(s: &str) -> Result<String, String> {
  let v = s.trim().to_ascii_lowercase();
  match v.as_str() {
    "light" | "dark" | "no-preference" => Ok(v),
    _ => Err(format!("invalid color scheme: {s}")),
  }
}

pub fn parse_shard(s: &str) -> Result<(usize, usize), String> {
  let parts: Vec<&str> = s.split('/').collect();
  if parts.len() != 2 {
    return Err("shard must be index/total (e.g., 0/4)".to_string());
  }
  let index = parts[0]
    .parse::<usize>()
    .map_err(|_| "invalid shard index".to_string())?;
  let total = parts[1]
    .parse::<usize>()
    .map_err(|_| "invalid shard total".to_string())?;
  if total == 0 {
    return Err("shard total must be > 0".to_string());
  }
  if index >= total {
    return Err("shard index must be < total".to_string());
  }
  Ok((index, total))
}

fn parse_quality(s: &str) -> Result<u8, String> {
  let value: u8 = s
    .trim()
    .parse()
    .map_err(|_| "quality must be an integer 0-100".to_string())?;
  if value > 100 {
    return Err("quality must be between 0 and 100".to_string());
  }
  Ok(value)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_viewport_values() {
    assert_eq!(parse_viewport("1200x800"), Ok((1200, 800)));
    assert_eq!(parse_viewport("800x600"), Ok((800, 600)));
    assert!(parse_viewport("0x600").is_err());
    assert!(parse_viewport("800").is_err());
    assert!(parse_viewport("800x").is_err());
  }

  #[test]
  fn parse_bool_preference_values() {
    assert_eq!(parse_bool_preference("reduce"), Ok(true));
    assert_eq!(parse_bool_preference("no-preference"), Ok(false));
    assert_eq!(parse_bool_preference("yes"), Ok(true));
    assert_eq!(parse_bool_preference("off"), Ok(false));
    assert!(parse_bool_preference("maybe").is_err());
  }

  #[test]
  fn parse_contrast_values() {
    assert_eq!(parse_contrast("more"), Ok("more".to_string()));
    assert_eq!(parse_contrast("HIGH"), Ok("high".to_string()));
    assert_eq!(parse_contrast("forced"), Ok("forced".to_string()));
    assert!(parse_contrast("maybe").is_err());
  }

  #[test]
  fn parse_color_scheme_values() {
    assert_eq!(parse_color_scheme("dark"), Ok("dark".to_string()));
    assert_eq!(parse_color_scheme("LIGHT"), Ok("light".to_string()));
    assert_eq!(
      parse_color_scheme("no-preference"),
      Ok("no-preference".to_string())
    );
    assert!(parse_color_scheme("pink").is_err());
  }

  #[test]
  fn parse_shard_values() {
    assert_eq!(parse_shard("0/4"), Ok((0, 4)));
    assert!(parse_shard("4/4").is_err());
    assert!(parse_shard("1/x").is_err());
    assert!(parse_shard("1").is_err());
  }

  #[test]
  fn parse_quality_values() {
    assert_eq!(parse_quality("0"), Ok(0));
    assert_eq!(parse_quality("80"), Ok(80));
    assert!(parse_quality("101").is_err());
  }

  #[test]
  fn output_format_args_maps_to_format() {
    let png = OutputFormatArgs {
      format: OutputFormatArg::Png,
      quality: 0,
    };
    assert_eq!(png.extension(), "png");
    assert_eq!(png.output_format(), OutputFormat::Png);

    let jpeg = OutputFormatArgs {
      format: OutputFormatArg::Jpeg,
      quality: 75,
    };
    assert_eq!(jpeg.extension(), "jpeg");
    assert_eq!(jpeg.output_format(), OutputFormat::Jpeg(75));

    let webp = OutputFormatArgs {
      format: OutputFormatArg::Webp,
      quality: 50,
    };
    assert_eq!(webp.extension(), "webp");
    assert_eq!(webp.output_format(), OutputFormat::WebP(50));
  }
}
