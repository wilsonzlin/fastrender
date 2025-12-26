use super::args::MediaPreferenceArgs;
use fastrender::style::media::{ColorScheme, ContrastPreference, MediaContext, MediaType};

#[derive(Debug, Clone, Default)]
pub struct MediaPreferences {
  pub prefers_reduced_transparency: Option<bool>,
  pub prefers_reduced_motion: Option<bool>,
  pub prefers_reduced_data: Option<bool>,
  pub prefers_contrast: Option<String>,
  pub prefers_color_scheme: Option<String>,
}

impl From<&MediaPreferenceArgs> for MediaPreferences {
  fn from(args: &MediaPreferenceArgs) -> Self {
    Self {
      prefers_reduced_transparency: args.prefers_reduced_transparency,
      prefers_reduced_motion: args.prefers_reduced_motion,
      prefers_reduced_data: args.prefers_reduced_data,
      prefers_contrast: args.prefers_contrast.clone(),
      prefers_color_scheme: args.prefers_color_scheme.clone(),
    }
  }
}

impl MediaPreferences {
  pub fn apply_env(&self) {
    if let Some(reduce) = self.prefers_reduced_transparency {
      std::env::set_var(
        "FASTR_PREFERS_REDUCED_TRANSPARENCY",
        if reduce { "reduce" } else { "no-preference" },
      );
    }

    if let Some(reduce) = self.prefers_reduced_motion {
      std::env::set_var(
        "FASTR_PREFERS_REDUCED_MOTION",
        if reduce { "reduce" } else { "no-preference" },
      );
    }

    if let Some(reduce) = self.prefers_reduced_data {
      std::env::set_var(
        "FASTR_PREFERS_REDUCED_DATA",
        if reduce { "reduce" } else { "no-preference" },
      );
    }

    if let Some(ref contrast) = self.prefers_contrast {
      std::env::set_var("FASTR_PREFERS_CONTRAST", contrast);
    }

    if let Some(ref color_scheme) = self.prefers_color_scheme {
      std::env::set_var("FASTR_PREFERS_COLOR_SCHEME", color_scheme);
    }
  }

  pub fn media_context_with_overrides(
    &self,
    viewport: (u32, u32),
    dpr: f32,
    media_type: MediaType,
  ) -> MediaContext {
    let ctx = match media_type {
      MediaType::Print => MediaContext::print(viewport.0 as f32, viewport.1 as f32),
      _ => MediaContext::screen(viewport.0 as f32, viewport.1 as f32),
    }
    .with_device_pixel_ratio(dpr)
    .with_env_overrides();
    self.apply_to_media_context(ctx)
  }

  pub fn apply_to_media_context(&self, mut ctx: MediaContext) -> MediaContext {
    if let Some(reduce) = self.prefers_reduced_transparency {
      ctx.prefers_reduced_transparency = reduce;
    }

    if let Some(reduce) = self.prefers_reduced_motion {
      ctx.prefers_reduced_motion = reduce;
    }

    if let Some(reduce) = self.prefers_reduced_data {
      ctx.prefers_reduced_data = reduce;
    }

    if let Some(contrast) = self.prefers_contrast.as_deref() {
      if let Ok(parsed) = ContrastPreference::parse(contrast) {
        ctx.prefers_contrast = parsed;
      }
    }

    if let Some(color_scheme) = self.prefers_color_scheme.as_deref() {
      if let Ok(parsed) = ColorScheme::parse(color_scheme) {
        ctx.prefers_color_scheme = Some(parsed);
      }
    }

    ctx
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn applies_preferences_to_media_context() {
    let prefs = MediaPreferences {
      prefers_reduced_transparency: Some(true),
      prefers_reduced_motion: Some(true),
      prefers_reduced_data: Some(true),
      prefers_contrast: Some("high".to_string()),
      prefers_color_scheme: Some("dark".to_string()),
    };

    let ctx = prefs.apply_to_media_context(MediaContext::screen(800.0, 600.0));

    assert!(ctx.prefers_reduced_transparency);
    assert!(ctx.prefers_reduced_motion);
    assert!(ctx.prefers_reduced_data);
    assert_eq!(ctx.prefers_contrast, ContrastPreference::More);
    assert_eq!(ctx.prefers_color_scheme, Some(ColorScheme::Dark));
  }
}
