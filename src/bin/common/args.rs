use clap::Args;

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
}
