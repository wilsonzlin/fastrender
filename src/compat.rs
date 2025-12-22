//! Compatibility profiles for opt-in, site-specific behavior.
//!
//! The core rendering pipeline aims to remain spec-faithful and free of
//! page-specific heuristics. When a targeted compatibility behavior is still
//! needed for internal captures, it should be isolated behind a
//! `CompatProfile` so the default pipeline remains clean.

/// Rendering compatibility profile.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompatProfile {
  /// Spec-only behavior; no site-specific heuristics are applied.
  Standards,

  /// Enables site-specific compatibility hacks used for internal page sets.
  SiteCompatibility,
}

impl CompatProfile {
  /// Returns true when site-specific compatibility behaviors should run.
  pub fn site_compat_hacks_enabled(self) -> bool {
    matches!(self, CompatProfile::SiteCompatibility)
  }
}

impl Default for CompatProfile {
  fn default() -> Self {
    CompatProfile::Standards
  }
}
