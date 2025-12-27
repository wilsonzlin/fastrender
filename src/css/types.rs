//! CSS type definitions
//!
//! Core types for representing CSS stylesheets, rules, and values.

use super::selectors::FastRenderSelectorImpl;
use super::selectors::PseudoClassParser;
use super::supports;
use crate::css::loader::resolve_href_with_base;
use crate::style::color::Color;
use crate::style::color::Rgba;
use crate::style::counter_styles::CounterStyleRule;
use crate::style::media::MediaContext;
use crate::style::media::MediaQuery;
use crate::style::media::MediaQueryCache;
use crate::style::values::Length;
use cssparser::Parser;
use cssparser::ParserInput;
use cssparser::ToCss;
use selectors::parser::SelectorList;
use std::fmt;

// ============================================================================
// CSS Parse Errors
// ============================================================================

/// A CSS parse error with location information
#[derive(Debug, Clone)]
pub struct CssParseError {
  /// Error message describing what went wrong
  pub message: String,
  /// Line number (1-indexed)
  pub line: u32,
  /// Column number (1-indexed)
  pub column: u32,
  /// Source line containing the error, when available.
  pub snippet: Option<String>,
}

impl CssParseError {
  /// Create a new parse error
  pub fn new(message: impl Into<String>, line: u32, column: u32) -> Self {
    Self {
      message: message.into(),
      line,
      column,
      snippet: None,
    }
  }

  /// Create a new parse error while capturing a relevant source snippet.
  pub fn with_snippet(
    message: impl Into<String>,
    line: u32,
    column: u32,
    css_source: &str,
  ) -> Self {
    let snippet = css_source
      .lines()
      .nth(line.saturating_sub(1) as usize)
      .map(|line| line.trim_end().to_string());
    Self {
      message: message.into(),
      line,
      column,
      snippet,
    }
  }
}

impl fmt::Display for CssParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "CSS error at line {}, column {}: {}",
      self.line, self.column, self.message
    )?;

    if let Some(snippet) = &self.snippet {
      let caret_pos = std::cmp::min(
        snippet.chars().count(),
        self.column.saturating_sub(1) as usize,
      );
      let caret = " ".repeat(caret_pos);
      write!(f, "\n  {snippet}\n  {caret}^")?;
    }

    Ok(())
  }
}

impl std::error::Error for CssParseError {}

/// Result of parsing a CSS stylesheet
///
/// Contains the parsed stylesheet along with any errors that were encountered.
/// The parser attempts error recovery, so even if errors are present, the
/// stylesheet may contain valid rules.
#[derive(Debug, Clone)]
pub struct CssParseResult {
  /// The parsed stylesheet (may be partial if errors occurred)
  pub stylesheet: StyleSheet,
  /// Any errors encountered during parsing
  pub errors: Vec<CssParseError>,
}

impl CssParseResult {
  /// Create a successful result with no errors
  pub fn ok(stylesheet: StyleSheet) -> Self {
    Self {
      stylesheet,
      errors: Vec::new(),
    }
  }

  /// Create a result with errors
  pub fn with_errors(stylesheet: StyleSheet, errors: Vec<CssParseError>) -> Self {
    Self { stylesheet, errors }
  }

  /// Returns true if parsing completed without errors
  pub fn is_ok(&self) -> bool {
    self.errors.is_empty()
  }

  /// Returns true if parsing encountered errors
  pub fn has_errors(&self) -> bool {
    !self.errors.is_empty()
  }

  /// Returns the number of errors
  pub fn error_count(&self) -> usize {
    self.errors.len()
  }
}

// ============================================================================
// CssString wrapper for selectors crate compatibility
// ============================================================================

/// Wrapper for String that implements ToCss
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct CssString(pub String);

impl From<&str> for CssString {
  fn from(s: &str) -> Self {
    CssString(s.to_string())
  }
}

impl From<String> for CssString {
  fn from(s: String) -> Self {
    CssString(s)
  }
}

impl std::ops::Deref for CssString {
  type Target = String;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl std::borrow::Borrow<str> for CssString {
  fn borrow(&self) -> &str {
    &self.0
  }
}

impl ToCss for CssString {
  fn to_css<W>(&self, dest: &mut W) -> fmt::Result
  where
    W: fmt::Write,
  {
    dest.write_str(&self.0)
  }
}

impl precomputed_hash::PrecomputedHash for CssString {
  fn precomputed_hash(&self) -> u32 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hash;
    use std::hash::Hasher;

    let mut hasher = DefaultHasher::new();
    self.0.hash(&mut hasher);
    hasher.finish() as u32
  }
}

// ============================================================================
// Stylesheet structures
// ============================================================================

/// Flattened style rule with its cascade layer ordering.
#[derive(Debug, Clone)]
pub struct CollectedRule<'a> {
  pub rule: &'a StyleRule,
  /// Cascade layer order (lexicographic; unlayered rules use a sentinel of u32::MAX).
  pub layer_order: Vec<u32>,
  /// Container query conditions that must match at cascade time.
  pub container_conditions: Vec<ContainerCondition>,
  /// Scoping contexts that constrain where the rule applies.
  pub scopes: Vec<ScopeContext<'a>>,
}

/// Flattened @page rule with cascade-layer ordering preserved.
#[derive(Debug, Clone)]
pub struct CollectedPageRule<'a> {
  pub rule: &'a PageRule,
  pub layer_order: Vec<u32>,
  pub order: usize,
}

/// Flattened @counter-style rule with cascade-layer ordering preserved.
#[derive(Debug, Clone)]
pub struct CollectedCounterStyleRule<'a> {
  pub rule: &'a CounterStyleRule,
  /// Cascade layer order (lexicographic; unlayered rules use a sentinel of u32::MAX).
  pub layer_order: Vec<u32>,
  pub order: usize,
}

/// Stylesheet containing CSS rules
#[derive(Debug, Clone)]
pub struct StyleSheet {
  /// All CSS rules in the stylesheet (style rules and @-rules)
  pub rules: Vec<CssRule>,
}

/// A page selector used by @page rules.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PageSelector {
  /// Optional named page identifier.
  pub name: Option<String>,
  /// Optional page pseudo-class (:first/:left/:right/:blank).
  pub pseudo: Option<PagePseudoClass>,
}

/// Supported page-side pseudo-classes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PagePseudoClass {
  First,
  Left,
  Right,
  Blank,
}

/// Margin-box positions inside a page rule.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PageMarginArea {
  TopLeftCorner,
  TopLeft,
  TopCenter,
  TopRight,
  TopRightCorner,
  RightTop,
  RightMiddle,
  RightBottom,
  BottomRightCorner,
  BottomRight,
  BottomCenter,
  BottomLeft,
  BottomLeftCorner,
  LeftBottom,
  LeftMiddle,
  LeftTop,
}

/// Nested @top-*/@bottom-* declarations inside @page.
#[derive(Debug, Clone)]
pub struct PageMarginRule {
  pub area: PageMarginArea,
  pub declarations: Vec<Declaration>,
}

/// A @page rule (page selectors + declarations + margin boxes).
#[derive(Debug, Clone)]
pub struct PageRule {
  pub selectors: Vec<PageSelector>,
  pub declarations: Vec<Declaration>,
  pub margin_rules: Vec<PageMarginRule>,
}

/// A minimal interface for loading imported stylesheets.
///
/// Implementations can fetch from network, filesystem, or an in-memory map.
pub trait CssImportLoader {
  fn load(&self, url: &str) -> crate::error::Result<String>;
}

impl StyleSheet {
  /// Creates an empty stylesheet
  pub fn new() -> Self {
    Self { rules: Vec::new() }
  }

  /// Collects all applicable style rules, evaluating @media queries
  ///
  /// This flattens nested @media rules and filters by the given media context.
  pub fn collect_style_rules(&self, media_ctx: &MediaContext) -> Vec<CollectedRule<'_>> {
    self.collect_style_rules_with_cache(media_ctx, None)
  }

  /// Collects applicable style rules using an optional media-query cache.
  ///
  /// Providing a cache allows callers to reuse media query evaluations across
  /// multiple collection passes (e.g., @font-face and style rules), reducing
  /// duplicate media feature work.
  pub fn collect_style_rules_with_cache(
    &self,
    media_ctx: &MediaContext,
    cache: Option<&mut MediaQueryCache>,
  ) -> Vec<CollectedRule<'_>> {
    let mut result = Vec::new();
    let mut registry = LayerRegistry::new();
    collect_rules_recursive(
      &self.rules,
      media_ctx,
      cache,
      &mut registry,
      &[],
      &[],
      &[],
      &mut result,
    );
    result
  }

  /// Collects applicable @page rules for the given media context.
  pub fn collect_page_rules(&self, media_ctx: &MediaContext) -> Vec<CollectedPageRule<'_>> {
    self.collect_page_rules_with_cache(media_ctx, None)
  }

  /// Collects @page rules using an optional media-query cache.
  pub fn collect_page_rules_with_cache(
    &self,
    media_ctx: &MediaContext,
    cache: Option<&mut MediaQueryCache>,
  ) -> Vec<CollectedPageRule<'_>> {
    let mut result = Vec::new();
    let mut registry = LayerRegistry::new();
    collect_page_rules_recursive(
      &self.rules,
      media_ctx,
      cache,
      &mut registry,
      &[],
      &mut result,
    );
    result
  }

  /// Collects all @font-face rules that apply to the current media context.
  pub fn collect_font_face_rules(&self, media_ctx: &MediaContext) -> Vec<FontFaceRule> {
    self.collect_font_face_rules_with_cache(media_ctx, None)
  }

  /// Collects applicable @font-face rules using an optional media-query cache.
  pub fn collect_font_face_rules_with_cache(
    &self,
    media_ctx: &MediaContext,
    cache: Option<&mut MediaQueryCache>,
  ) -> Vec<FontFaceRule> {
    let mut result = Vec::new();
    collect_font_faces_recursive(&self.rules, media_ctx, cache, &mut result);
    result
  }

  /// Collects all @keyframes rules that apply to the current media context.
  pub fn collect_keyframes(&self, media_ctx: &MediaContext) -> Vec<KeyframesRule> {
    self.collect_keyframes_with_cache(media_ctx, None)
  }

  /// Collects @keyframes rules using an optional media-query cache for reuse.
  pub fn collect_keyframes_with_cache(
    &self,
    media_ctx: &MediaContext,
    cache: Option<&mut MediaQueryCache>,
  ) -> Vec<KeyframesRule> {
    let mut result = Vec::new();
    collect_keyframes_recursive(&self.rules, media_ctx, cache, &mut result);
    result
  }

  /// Collects all @counter-style rules that apply to the current media context.
  pub fn collect_counter_style_rules(
    &self,
    media_ctx: &MediaContext,
  ) -> Vec<CollectedCounterStyleRule<'_>> {
    self.collect_counter_style_rules_with_cache(media_ctx, None)
  }

  /// Collects @counter-style rules using an optional media-query cache for reuse.
  pub fn collect_counter_style_rules_with_cache(
    &self,
    media_ctx: &MediaContext,
    cache: Option<&mut MediaQueryCache>,
  ) -> Vec<CollectedCounterStyleRule<'_>> {
    let mut result = Vec::new();
    let mut registry = LayerRegistry::new();
    collect_counter_styles_recursive(
      &self.rules,
      media_ctx,
      cache,
      &mut registry,
      &[],
      &mut result,
    );
    result
  }

  /// Resolve @import rules by fetching external stylesheets and inlining their rules.
  ///
  /// Imports are processed in order; only imports whose media lists match the provided
  /// `media_ctx` are inlined. Relative URLs are resolved against `base_url` when provided.
  pub fn resolve_imports<L: CssImportLoader + ?Sized>(
    &self,
    loader: &L,
    base_url: Option<&str>,
    media_ctx: &MediaContext,
  ) -> Self {
    self.resolve_imports_with_cache(loader, base_url, media_ctx, None)
  }

  /// Resolve @import rules with an optional media-query cache for reuse.
  pub fn resolve_imports_with_cache<L: CssImportLoader + ?Sized>(
    &self,
    loader: &L,
    base_url: Option<&str>,
    media_ctx: &MediaContext,
    cache: Option<&mut MediaQueryCache>,
  ) -> Self {
    let mut resolved = Vec::new();
    let mut seen = std::collections::HashSet::new();
    resolve_rules(
      &self.rules,
      loader,
      base_url,
      media_ctx,
      cache,
      &mut seen,
      &mut resolved,
    );
    StyleSheet { rules: resolved }
  }

  /// Returns true if the stylesheet (or any nested blocks) contains @container rules.
  pub fn has_container_rules(&self) -> bool {
    fn walk(rules: &[CssRule]) -> bool {
      for rule in rules {
        match rule {
          CssRule::Container(_) => return true,
          CssRule::Style(style) => {
            if walk(&style.nested_rules) {
              return true;
            }
          }
          CssRule::Media(media) => {
            if walk(&media.rules) {
              return true;
            }
          }
          CssRule::Supports(supports) => {
            if walk(&supports.rules) {
              return true;
            }
          }
          CssRule::Layer(layer) => {
            if walk(&layer.rules) {
              return true;
            }
          }
          CssRule::Scope(scope) => {
            if walk(&scope.rules) {
              return true;
            }
          }
          CssRule::Page(_)
          | CssRule::CounterStyle(_)
          | CssRule::Import(_)
          | CssRule::FontFace(_)
          | CssRule::Keyframes(_) => {}
        }
      }
      false
    }

    walk(&self.rules)
  }
}

/// Helper to recursively collect style rules from nested @media/@layer blocks
fn collect_rules_recursive<'a>(
  rules: &'a [CssRule],
  media_ctx: &MediaContext,
  cache: Option<&mut MediaQueryCache>,
  registry: &mut LayerRegistry,
  current_layer: &[u32],
  container_conditions: &[ContainerCondition],
  current_scopes: &[ScopeContext<'a>],
  out: &mut Vec<CollectedRule<'a>>,
) {
  // Keep the cache binding mutable so we can call as_deref_mut() repeatedly
  // when evaluating nested @media / @container rules.
  let mut cache = cache;
  for rule in rules {
    match rule {
      CssRule::Style(style_rule) => {
        let layer_order = if current_layer.is_empty() {
          vec![u32::MAX]
        } else {
          current_layer.to_vec()
        };
        out.push(CollectedRule {
          rule: style_rule,
          layer_order,
          container_conditions: container_conditions.to_vec(),
          scopes: current_scopes.to_vec(),
        });
        if !style_rule.nested_rules.is_empty() {
          collect_rules_recursive(
            &style_rule.nested_rules,
            media_ctx,
            cache.as_deref_mut(),
            registry,
            current_layer,
            container_conditions,
            current_scopes,
            out,
          );
        }
      }
      CssRule::Media(media_rule) => {
        // Only include rules from @media blocks that match
        if media_ctx.evaluate_with_cache(&media_rule.query, cache.as_deref_mut()) {
          collect_rules_recursive(
            &media_rule.rules,
            media_ctx,
            cache.as_deref_mut(),
            registry,
            current_layer,
            container_conditions,
            current_scopes,
            out,
          );
        }
      }
      CssRule::Container(container_rule) => {
        let mut updated_conditions = container_conditions.to_vec();
        updated_conditions.push(ContainerCondition {
          name: container_rule.name.clone(),
          query_list: container_rule.query_list.clone(),
        });
        collect_rules_recursive(
          &container_rule.rules,
          media_ctx,
          cache.as_deref_mut(),
          registry,
          current_layer,
          &updated_conditions,
          current_scopes,
          out,
        );
      }
      CssRule::Supports(supports_rule) => {
        if supports_rule.condition.matches() {
          collect_rules_recursive(
            &supports_rule.rules,
            media_ctx,
            cache.as_deref_mut(),
            registry,
            current_layer,
            container_conditions,
            current_scopes,
            out,
          );
        }
      }
      CssRule::Page(_) => {}
      CssRule::CounterStyle(_) => {}
      CssRule::Import(_) => {
        // Imports are resolved before collection; nothing to add here.
      }
      CssRule::Keyframes(_) => {}
      CssRule::Layer(layer_rule) => {
        if layer_rule.rules.is_empty() {
          for name in &layer_rule.names {
            registry.ensure_path(current_layer, name);
          }
          if layer_rule.anonymous {
            registry.ensure_anonymous(current_layer);
          }
          continue;
        }

        if layer_rule.anonymous {
          let path = registry.ensure_anonymous(current_layer);
          collect_rules_recursive(
            &layer_rule.rules,
            media_ctx,
            cache.as_deref_mut(),
            registry,
            &path,
            container_conditions,
            current_scopes,
            out,
          );
          continue;
        }

        if layer_rule.names.len() != 1 {
          // Invalid layer block; skip the rules.
          continue;
        }
        let path = registry.ensure_path(current_layer, &layer_rule.names[0]);
        collect_rules_recursive(
          &layer_rule.rules,
          media_ctx,
          cache.as_deref_mut(),
          registry,
          &path,
          container_conditions,
          current_scopes,
          out,
        );
      }
      CssRule::FontFace(_) => {}
      CssRule::Scope(scope_rule) => {
        let mut updated_scopes = current_scopes.to_vec();
        updated_scopes.push(ScopeContext {
          start: scope_rule.start.as_ref().map(|s| s.slice()),
          end: scope_rule.end.as_ref().map(|s| s.slice()),
          implicit_start: scope_rule.start.is_none(),
        });
        collect_rules_recursive(
          &scope_rule.rules,
          media_ctx,
          cache.as_deref_mut(),
          registry,
          current_layer,
          container_conditions,
          &updated_scopes,
          out,
        );
      }
    }
  }
}

/// Collect @page rules from nested @media/@layer blocks.
fn collect_page_rules_recursive<'a>(
  rules: &'a [CssRule],
  media_ctx: &MediaContext,
  cache: Option<&mut MediaQueryCache>,
  registry: &mut LayerRegistry,
  current_layer: &[u32],
  out: &mut Vec<CollectedPageRule<'a>>,
) {
  let mut cache = cache;
  for rule in rules {
    match rule {
      CssRule::Page(page_rule) => {
        let layer_order = if current_layer.is_empty() {
          vec![u32::MAX]
        } else {
          current_layer.to_vec()
        };
        let order = out.len();
        out.push(CollectedPageRule {
          rule: page_rule,
          layer_order,
          order,
        });
      }
      CssRule::Media(media_rule) => {
        if media_ctx.evaluate_with_cache(&media_rule.query, cache.as_deref_mut()) {
          collect_page_rules_recursive(
            &media_rule.rules,
            media_ctx,
            cache.as_deref_mut(),
            registry,
            current_layer,
            out,
          );
        }
      }
      CssRule::Supports(supports_rule) => {
        if supports_rule.condition.matches() {
          collect_page_rules_recursive(
            &supports_rule.rules,
            media_ctx,
            cache.as_deref_mut(),
            registry,
            current_layer,
            out,
          );
        }
      }
      CssRule::Layer(layer_rule) => {
        if layer_rule.rules.is_empty() {
          for name in &layer_rule.names {
            registry.ensure_path(current_layer, name);
          }
          if layer_rule.anonymous {
            registry.ensure_anonymous(current_layer);
          }
          continue;
        }

        if layer_rule.anonymous {
          let path = registry.ensure_anonymous(current_layer);
          collect_page_rules_recursive(
            &layer_rule.rules,
            media_ctx,
            cache.as_deref_mut(),
            registry,
            &path,
            out,
          );
          continue;
        }

        if layer_rule.names.len() != 1 {
          continue;
        }

        let path = registry.ensure_path(current_layer, &layer_rule.names[0]);
        collect_page_rules_recursive(
          &layer_rule.rules,
          media_ctx,
          cache.as_deref_mut(),
          registry,
          &path,
          out,
        );
      }
      CssRule::Container(_) => {}
      CssRule::Keyframes(_) | CssRule::CounterStyle(_) => {}
      CssRule::Style(_) | CssRule::Import(_) | CssRule::FontFace(_) => {}
      CssRule::Scope(scope_rule) => {
        collect_page_rules_recursive(
          &scope_rule.rules,
          media_ctx,
          cache.as_deref_mut(),
          registry,
          current_layer,
          out,
        );
      }
    }
  }
}

fn collect_font_faces_recursive(
  rules: &[CssRule],
  media_ctx: &MediaContext,
  cache: Option<&mut MediaQueryCache>,
  out: &mut Vec<FontFaceRule>,
) {
  let mut cache = cache;
  for rule in rules {
    match rule {
      CssRule::FontFace(face) => out.push(face.clone()),
      CssRule::Media(media_rule) => {
        if media_ctx.evaluate_with_cache(&media_rule.query, cache.as_deref_mut()) {
          collect_font_faces_recursive(&media_rule.rules, media_ctx, cache.as_deref_mut(), out);
        }
      }
      CssRule::Container(container_rule) => {
        if media_ctx.evaluate_list_with_cache(&container_rule.query_list, cache.as_deref_mut()) {
          collect_font_faces_recursive(&container_rule.rules, media_ctx, cache.as_deref_mut(), out);
        }
      }
      CssRule::Supports(supports_rule) => {
        if supports_rule.condition.matches() {
          collect_font_faces_recursive(&supports_rule.rules, media_ctx, cache.as_deref_mut(), out);
        }
      }
      CssRule::Page(_) => {}
      CssRule::CounterStyle(_) => {}
      CssRule::Layer(layer_rule) => {
        if layer_rule.rules.is_empty() {
          continue;
        }
        if layer_rule.anonymous {
          collect_font_faces_recursive(&layer_rule.rules, media_ctx, cache.as_deref_mut(), out);
          continue;
        }
        if layer_rule.names.len() != 1 {
          continue;
        }
        collect_font_faces_recursive(&layer_rule.rules, media_ctx, cache.as_deref_mut(), out);
      }
      CssRule::Style(_) | CssRule::Import(_) => {}
      CssRule::Keyframes(_) => {}
      CssRule::Scope(scope_rule) => {
        collect_font_faces_recursive(&scope_rule.rules, media_ctx, cache.as_deref_mut(), out);
      }
    }
  }
}

fn collect_keyframes_recursive(
  rules: &[CssRule],
  media_ctx: &MediaContext,
  cache: Option<&mut MediaQueryCache>,
  out: &mut Vec<KeyframesRule>,
) {
  let mut cache = cache;
  for rule in rules {
    match rule {
      CssRule::Keyframes(kf) => out.push(kf.clone()),
      CssRule::Media(media_rule) => {
        if media_ctx.evaluate_with_cache(&media_rule.query, cache.as_deref_mut()) {
          collect_keyframes_recursive(&media_rule.rules, media_ctx, cache.as_deref_mut(), out);
        }
      }
      CssRule::Container(container_rule) => {
        if media_ctx.evaluate_list_with_cache(&container_rule.query_list, cache.as_deref_mut()) {
          collect_keyframes_recursive(&container_rule.rules, media_ctx, cache.as_deref_mut(), out);
        }
      }
      CssRule::Supports(supports_rule) => {
        if supports_rule.condition.matches() {
          collect_keyframes_recursive(&supports_rule.rules, media_ctx, cache.as_deref_mut(), out);
        }
      }
      CssRule::Layer(layer_rule) => {
        if layer_rule.rules.is_empty() {
          continue;
        }
        collect_keyframes_recursive(&layer_rule.rules, media_ctx, cache.as_deref_mut(), out);
      }
      CssRule::Page(_) | CssRule::CounterStyle(_) => {}
      CssRule::Style(_) | CssRule::Import(_) | CssRule::FontFace(_) => {}
      CssRule::Scope(scope_rule) => {
        collect_keyframes_recursive(&scope_rule.rules, media_ctx, cache.as_deref_mut(), out);
      }
    }
  }
}

fn collect_counter_styles_recursive<'a>(
  rules: &'a [CssRule],
  media_ctx: &MediaContext,
  cache: Option<&mut MediaQueryCache>,
  registry: &mut LayerRegistry,
  current_layer: &[u32],
  out: &mut Vec<CollectedCounterStyleRule<'a>>,
) {
  let mut cache = cache;
  for rule in rules {
    match rule {
      CssRule::CounterStyle(counter) => {
        let layer_order = if current_layer.is_empty() {
          vec![u32::MAX]
        } else {
          current_layer.to_vec()
        };
        let order = out.len();
        out.push(CollectedCounterStyleRule {
          rule: counter,
          layer_order,
          order,
        });
      }
      CssRule::Media(media_rule) => {
        if media_ctx.evaluate_with_cache(&media_rule.query, cache.as_deref_mut()) {
          collect_counter_styles_recursive(
            &media_rule.rules,
            media_ctx,
            cache.as_deref_mut(),
            registry,
            current_layer,
            out,
          );
        }
      }
      CssRule::Supports(supports_rule) => {
        if supports_rule.condition.matches() {
          collect_counter_styles_recursive(
            &supports_rule.rules,
            media_ctx,
            cache.as_deref_mut(),
            registry,
            current_layer,
            out,
          );
        }
      }
      CssRule::Layer(layer_rule) => {
        if layer_rule.rules.is_empty() {
          for name in &layer_rule.names {
            registry.ensure_path(current_layer, name);
          }
          if layer_rule.anonymous {
            registry.ensure_anonymous(current_layer);
          }
          continue;
        }

        if layer_rule.anonymous {
          let path = registry.ensure_anonymous(current_layer);
          collect_counter_styles_recursive(
            &layer_rule.rules,
            media_ctx,
            cache.as_deref_mut(),
            registry,
            &path,
            out,
          );
          continue;
        }

        if layer_rule.names.len() != 1 {
          continue;
        }
        let path = registry.ensure_path(current_layer, &layer_rule.names[0]);
        collect_counter_styles_recursive(
          &layer_rule.rules,
          media_ctx,
          cache.as_deref_mut(),
          registry,
          &path,
          out,
        );
      }
      CssRule::Style(style_rule) => {
        if !style_rule.nested_rules.is_empty() {
          collect_counter_styles_recursive(
            &style_rule.nested_rules,
            media_ctx,
            cache.as_deref_mut(),
            registry,
            current_layer,
            out,
          );
        }
      }
      CssRule::Scope(scope_rule) => {
        collect_counter_styles_recursive(
          &scope_rule.rules,
          media_ctx,
          cache.as_deref_mut(),
          registry,
          current_layer,
          out,
        );
      }
      CssRule::Container(container_rule) => {
        collect_counter_styles_recursive(
          &container_rule.rules,
          media_ctx,
          cache.as_deref_mut(),
          registry,
          current_layer,
          out,
        );
      }
      CssRule::Import(_) => {}
      CssRule::Page(_) => {}
      CssRule::FontFace(_) => {}
      CssRule::Keyframes(_) => {}
    }
  }
}

impl SupportsCondition {
  /// Evaluate the @supports condition according to the features we implement.
  ///
  /// The declaration form is considered supported when the property is known and the value parses.
  pub fn matches(&self) -> bool {
    match self {
      SupportsCondition::Declaration { property, value } => {
        supports::supports_declaration(property, value)
      }
      SupportsCondition::Selector(selector_list) => supports_selector_is_valid(selector_list),
      SupportsCondition::Not(cond) => !cond.matches(),
      SupportsCondition::And(conds) => conds.iter().all(|c| c.matches()),
      SupportsCondition::Or(conds) => conds.iter().any(|c| c.matches()),
      SupportsCondition::True => true,
      SupportsCondition::False => false,
    }
  }
}

/// Determines whether a selector list is supported for @supports selector() queries.
///
/// The selector is considered supported when it parses successfully with the engine's selector
/// grammar. Selectors that fail to parse evaluate to false.
fn supports_selector_is_valid(selector_list: &str) -> bool {
  let mut input = ParserInput::new(selector_list);
  let mut parser = Parser::new(&mut input);
  match SelectorList::parse(
    &PseudoClassParser,
    &mut parser,
    selectors::parser::ParseRelative::No,
  ) {
    Ok(_) => true,
    Err(_) => false,
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn supports_selector_matches_for_basic_selector() {
    let cond = SupportsCondition::Selector("div > span".into());
    assert!(cond.matches(), "simple selector list should be supported");
  }

  #[test]
  fn supports_selector_rejects_unsupported_pseudo() {
    let cond = SupportsCondition::Selector("div:unknown-pseudo".into());
    assert!(
      !cond.matches(),
      "unsupported pseudo should fail selector() queries"
    );
  }

  #[test]
  fn supports_selector_handles_invalid_selector_list() {
    let cond = SupportsCondition::Selector("div:unknown-pseudo, span".into());
    assert!(
      !cond.matches(),
      "selector() should fail when any selector in the list is unsupported"
    );

    let cond = SupportsCondition::Selector("div + span".into());
    assert!(cond.matches(), "supported selector list should be accepted");
  }

  #[test]
  fn supports_selector_accepts_has_and_supported_selectors() {
    let with_has = SupportsCondition::Selector("div:has(span)".into());
    assert!(
      with_has.matches(),
      ":has() should be supported in selector() queries"
    );

    let supported = SupportsCondition::Selector(".foo:nth-child(2n+1)".into());
    assert!(
      supported.matches(),
      "nth-child selector should be supported in selector()"
    );
  }

  #[test]
  fn supports_declaration_accepts_sideways_left_text_orientation() {
    let cond = SupportsCondition::Declaration {
      property: "text-orientation".into(),
      value: "sideways-left".into(),
    };
    assert!(
      cond.matches(),
      "sideways-left should be considered supported"
    );
  }

  #[test]
  fn supports_rejects_page_only_properties() {
    let cond = SupportsCondition::Declaration {
      property: "size".into(),
      value: "A4".into(),
    };
    assert!(
      !cond.matches(),
      "page-only properties should be unsupported in style @supports"
    );
  }

  #[test]
  fn supports_declaration_rejects_invalid_opacity() {
    let cond = SupportsCondition::Declaration {
      property: "opacity".into(),
      value: "foo".into(),
    };
    assert!(
      !cond.matches(),
      "invalid opacity value should not be supported"
    );
  }

  #[test]
  fn supports_declaration_accepts_background_with_slash() {
    let cond = SupportsCondition::Declaration {
      property: "background".into(),
      value: "center/cover".into(),
    };
    assert!(
      cond.matches(),
      "background shorthand with slash-separated position and size should be supported"
    );
  }

  #[test]
  fn supports_declaration_accepts_border_image_slice_fill() {
    let cond = SupportsCondition::Declaration {
      property: "border-image-slice".into(),
      value: "30 30 fill".into(),
    };
    assert!(
      cond.matches(),
      "border-image-slice should accept numeric values with fill keyword"
    );
  }

  #[test]
  fn supports_declaration_rejects_unknown_properties() {
    let cond = SupportsCondition::Declaration {
      property: "not-a-property".into(),
      value: "1".into(),
    };
    assert!(
      !cond.matches(),
      "unknown properties should not be considered supported"
    );
  }
}

impl Default for StyleSheet {
  fn default() -> Self {
    Self::new()
  }
}

/// A CSS rule (style rule or @-rule)
#[derive(Debug, Clone)]
pub enum CssRule {
  /// A style rule (selectors + declarations)
  Style(StyleRule),
  /// A @media rule containing conditional rules
  Media(MediaRule),
  /// A @container rule containing conditional rules (container queries)
  Container(ContainerRule),
  /// A @supports rule containing conditional rules
  Supports(SupportsRule),
  /// A @page rule controlling paged-media page boxes
  Page(PageRule),
  /// A @counter-style rule defining a custom counter style.
  CounterStyle(CounterStyleRule),
  /// An @import rule (href + optional media list)
  Import(ImportRule),
  /// A @layer rule establishing cascade layers
  Layer(LayerRule),
  /// A @font-face rule defining a downloadable font.
  FontFace(FontFaceRule),
  /// A @keyframes rule defining animated keyframes.
  Keyframes(KeyframesRule),
  /// A @scope rule limiting descendant styles to a subtree.
  Scope(ScopeRule),
}

/// A @media rule containing conditional rules
#[derive(Debug, Clone)]
pub struct MediaRule {
  /// The media query to evaluate
  pub query: MediaQuery,
  /// Rules that apply when query matches (can be nested)
  pub rules: Vec<CssRule>,
}

/// A @container rule containing conditional rules
#[derive(Debug, Clone)]
pub struct ContainerRule {
  /// Optional named container to target.
  pub name: Option<String>,
  /// Container query list to evaluate (size queries only, OR semantics).
  pub query_list: Vec<MediaQuery>,
  /// Rules that apply when query matches (can be nested)
  pub rules: Vec<CssRule>,
}

/// A @scope rule constraining descendant style rules.
#[derive(Debug, Clone)]
pub struct ScopeRule {
  /// Optional selector list defining scope roots. When omitted, the scope defaults to the parent context.
  pub start: Option<SelectorList<FastRenderSelectorImpl>>,
  /// Optional selector list defining scope limits.
  pub end: Option<SelectorList<FastRenderSelectorImpl>>,
  /// Nested rules that participate in the scope.
  pub rules: Vec<CssRule>,
}

/// Container query condition attached to collected rules.
#[derive(Debug, Clone)]
pub struct ContainerCondition {
  /// Optional container name to match (None targets the nearest container).
  pub name: Option<String>,
  /// Parsed container queries (size queries only, OR semantics).
  pub query_list: Vec<MediaQuery>,
}

/// A scoping context attached to collected style rules.
#[derive(Debug, Clone)]
pub struct ScopeContext<'a> {
  /// Optional scope-start selector list; when None, the scope defaults to the parent context.
  pub start: Option<&'a [selectors::parser::Selector<FastRenderSelectorImpl>]>,
  /// Optional scope-limit selector list.
  pub end: Option<&'a [selectors::parser::Selector<FastRenderSelectorImpl>]>,
  /// Whether the scope prelude omitted a start selector (implicit root).
  pub implicit_start: bool,
}

/// A @supports rule containing conditional rules
#[derive(Debug, Clone)]
pub struct SupportsRule {
  /// The feature condition to evaluate
  pub condition: SupportsCondition,
  /// Rules that apply when the condition is satisfied
  pub rules: Vec<CssRule>,
}

/// A parsed @supports condition
#[derive(Debug, Clone)]
pub enum SupportsCondition {
  /// `(property: value)` feature query
  Declaration { property: String, value: String },
  /// `selector(<selector-list>)` feature query
  Selector(String),
  /// Logical negation
  Not(Box<SupportsCondition>),
  /// Logical conjunction
  And(Vec<SupportsCondition>),
  /// Logical disjunction
  Or(Vec<SupportsCondition>),
  /// Always true (used for parsing fallbacks)
  True,
  /// Always false (used when parsing fails)
  False,
}

/// A @import rule targeting another stylesheet
#[derive(Debug, Clone)]
pub struct ImportRule {
  /// Target stylesheet URL (as authored, possibly relative)
  pub href: String,
  /// Optional list of media queries that gate the import
  pub media: Vec<MediaQuery>,
}

/// A single CSS style rule (selectors + declarations)
#[derive(Debug, Clone)]
pub struct StyleRule {
  pub selectors: SelectorList<FastRenderSelectorImpl>,
  pub declarations: Vec<Declaration>,
  /// Nested rules that were authored inside this style rule's block.
  /// These are preserved in source order so they can be flattened during collection.
  pub nested_rules: Vec<CssRule>,
}

/// A @layer rule (blockless or with nested rules).
#[derive(Debug, Clone)]
pub struct LayerRule {
  /// Named layers declared in the prelude (empty for anonymous layers).
  pub names: Vec<Vec<String>>,
  /// Nested rules (empty for blockless declarations).
  pub rules: Vec<CssRule>,
  /// Whether this is an anonymous layer (no names).
  pub anonymous: bool,
}

/// A @font-face rule with parsed descriptors.
#[derive(Debug, Clone)]
pub struct FontFaceRule {
  /// The family name exposed to CSS.
  pub family: Option<String>,
  /// Ordered font sources from the `src` descriptor.
  pub sources: Vec<FontFaceSource>,
  /// Style descriptor (normal/italic/oblique with optional angle range).
  pub style: FontFaceStyle,
  /// Font display strategy (auto/block/swap/fallback/optional).
  pub display: FontDisplay,
  /// Weight range expressed in CSS absolute weights.
  pub weight: (u16, u16),
  /// Stretch range in percentages.
  pub stretch: (f32, f32),
  /// Unicode ranges that this face covers (inclusive start,end).
  pub unicode_ranges: Vec<(u32, u32)>,
}

impl Default for FontFaceRule {
  fn default() -> Self {
    Self {
      family: None,
      sources: Vec::new(),
      style: FontFaceStyle::Normal,
      display: FontDisplay::Auto,
      weight: (400, 400),
      stretch: (100.0, 100.0),
      unicode_ranges: vec![(0, 0x10ffff)],
    }
  }
}

/// A keyframe selector with declarations at a specific offset.
#[derive(Debug, Clone)]
pub struct Keyframe {
  /// Normalized offset in the 0-1 range.
  pub offset: f32,
  /// Declarations that apply at this offset.
  pub declarations: Vec<Declaration>,
}

/// A @keyframes rule with a name and associated keyframes.
#[derive(Debug, Clone)]
pub struct KeyframesRule {
  /// Animation name referenced by animation-name.
  pub name: String,
  /// Ordered list of keyframes.
  pub keyframes: Vec<Keyframe>,
}

/// Supported @font-face format() hint values.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FontSourceFormat {
  Woff2,
  Woff,
  Opentype,
  Truetype,
  Collection,
  EmbeddedOpenType,
  Svg,
  Unknown(String),
}

impl FontSourceFormat {
  /// Construct from a raw format() hint string.
  pub fn from_hint(hint: &str) -> Self {
    match hint.trim().to_ascii_lowercase().as_str() {
      "woff2" => FontSourceFormat::Woff2,
      "woff" => FontSourceFormat::Woff,
      "opentype" | "otf" => FontSourceFormat::Opentype,
      "truetype" | "ttf" | "truetype-aat" => FontSourceFormat::Truetype,
      "collection" | "ttc" => FontSourceFormat::Collection,
      "embedded-opentype" | "eot" => FontSourceFormat::EmbeddedOpenType,
      "svg" | "svgz" => FontSourceFormat::Svg,
      other => FontSourceFormat::Unknown(other.to_string()),
    }
  }
}

/// A downloadable font source.
#[derive(Debug, Clone)]
pub struct FontFaceUrlSource {
  /// The URL to fetch.
  pub url: String,
  /// Optional format() hints associated with the source.
  pub format_hints: Vec<FontSourceFormat>,
}

impl FontFaceUrlSource {
  pub fn new(url: impl Into<String>) -> Self {
    Self {
      url: url.into(),
      format_hints: Vec::new(),
    }
  }
}

/// A single font source in `src`.
#[derive(Debug, Clone)]
pub enum FontFaceSource {
  /// A downloadable URL source.
  Url(FontFaceUrlSource),
  /// A locally installed font name.
  Local(String),
}

impl FontFaceSource {
  pub fn url(url: impl Into<String>) -> Self {
    FontFaceSource::Url(FontFaceUrlSource::new(url))
  }

  pub fn local(name: impl Into<String>) -> Self {
    FontFaceSource::Local(name.into())
  }
}

/// font-display descriptor options for @font-face
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FontDisplay {
  Auto,
  Block,
  Swap,
  Fallback,
  Optional,
}

/// Font style descriptor for @font-face.
#[derive(Debug, Clone)]
pub enum FontFaceStyle {
  Normal,
  Italic,
  /// Oblique with an optional angle range (degrees).
  Oblique {
    range: Option<(f32, f32)>,
  },
}

/// A CSS property declaration
#[derive(Debug, Clone)]
pub struct Declaration {
  pub property: String,
  pub value: PropertyValue,
  /// Raw token string as authored (after stripping !important/semicolon). Used for custom properties.
  pub raw_value: String,
  pub important: bool,
}

fn resolve_rules<L: CssImportLoader + ?Sized>(
  rules: &[CssRule],
  loader: &L,
  base_url: Option<&str>,
  media_ctx: &MediaContext,
  cache: Option<&mut MediaQueryCache>,
  seen: &mut std::collections::HashSet<String>,
  out: &mut Vec<CssRule>,
) {
  const MAX_RESOLVED_IMPORTS: usize = 128;

  // Keep a mutable binding so nested calls can borrow the cache mutably via as_deref_mut().
  let mut cache = cache;

  for rule in rules {
    match rule {
      CssRule::Style(_)
      | CssRule::Media(_)
      | CssRule::FontFace(_)
      | CssRule::Keyframes(_)
      | CssRule::CounterStyle(_) => out.push(rule.clone()),
      CssRule::Container(container_rule) => {
        let mut resolved_children = Vec::new();
        resolve_rules(
          &container_rule.rules,
          loader,
          base_url,
          media_ctx,
          cache.as_deref_mut(),
          seen,
          &mut resolved_children,
        );
        out.push(CssRule::Container(ContainerRule {
          name: container_rule.name.clone(),
          query_list: container_rule.query_list.clone(),
          rules: resolved_children,
        }));
      }
      CssRule::Supports(supports_rule) => {
        let mut resolved_children = Vec::new();
        resolve_rules(
          &supports_rule.rules,
          loader,
          base_url,
          media_ctx,
          cache.as_deref_mut(),
          seen,
          &mut resolved_children,
        );
        out.push(CssRule::Supports(SupportsRule {
          condition: supports_rule.condition.clone(),
          rules: resolved_children,
        }));
      }
      CssRule::Page(page_rule) => {
        out.push(CssRule::Page(page_rule.clone()));
      }
      CssRule::Scope(scope_rule) => {
        let mut resolved_children = Vec::new();
        resolve_rules(
          &scope_rule.rules,
          loader,
          base_url,
          media_ctx,
          cache.as_deref_mut(),
          seen,
          &mut resolved_children,
        );
        out.push(CssRule::Scope(ScopeRule {
          start: scope_rule.start.clone(),
          end: scope_rule.end.clone(),
          rules: resolved_children,
        }));
      }
      CssRule::Layer(layer_rule) => {
        let mut resolved_children = Vec::new();
        resolve_rules(
          &layer_rule.rules,
          loader,
          base_url,
          media_ctx,
          cache.as_deref_mut(),
          seen,
          &mut resolved_children,
        );
        out.push(CssRule::Layer(LayerRule {
          names: layer_rule.names.clone(),
          rules: resolved_children,
          anonymous: layer_rule.anonymous,
        }));
      }
      CssRule::Import(import) => {
        let media_matches = import.media.is_empty()
          || media_ctx.evaluate_list_with_cache(&import.media, cache.as_deref_mut());
        if !media_matches {
          continue;
        }

        let resolved_href = if let Some(resolved) = resolve_href_with_base(base_url, &import.href) {
          resolved
        } else {
          continue;
        };

        if seen.len() >= MAX_RESOLVED_IMPORTS {
          continue;
        }

        if seen.contains(&resolved_href) {
          continue;
        }

        match loader.load(&resolved_href) {
          Ok(css_text) => {
            seen.insert(resolved_href.clone());
            if let Ok(sheet) = crate::css::parser::parse_stylesheet(&css_text) {
              resolve_rules(
                &sheet.rules,
                loader,
                Some(&resolved_href),
                media_ctx,
                cache.as_deref_mut(),
                seen,
                out,
              );
            }
          }
          Err(_) => {
            // Per spec, failed imports are ignored.
          }
        }
      }
    }
  }
}

#[derive(Debug, Default, Clone)]
struct LayerNode {
  children: Vec<LayerChild>,
}

#[derive(Debug, Clone)]
struct LayerChild {
  name: String,
  order: u32,
  node: LayerNode,
}

#[derive(Debug, Default, Clone)]
struct LayerRegistry {
  root: LayerNode,
  next_anonymous: u32,
}

impl LayerRegistry {
  fn new() -> Self {
    Self::default()
  }

  fn ensure_path(&mut self, base: &[u32], name: &[String]) -> Vec<u32> {
    let mut path = base.to_vec();
    let mut node: &mut LayerNode = if let Some(existing) = self.get_node_mut(base) {
      existing
    } else {
      &mut self.root
    };
    for component in name {
      let (order, next) = node.ensure_child(component);
      path.push(order);
      node = next;
    }
    path
  }

  fn ensure_anonymous(&mut self, base: &[u32]) -> Vec<u32> {
    let name = format!("__anon{}", self.next_anonymous);
    self.next_anonymous += 1;
    self.ensure_path(base, &[name])
  }

  fn get_node_mut(&mut self, path: &[u32]) -> Option<&mut LayerNode> {
    fn descend<'a>(node: &'a mut LayerNode, path: &[u32]) -> Option<&'a mut LayerNode> {
      if path.is_empty() {
        return Some(node);
      }
      let (first, rest) = path.split_first()?;
      let pos = node.children.iter().position(|c| c.order == *first)?;
      let child = &mut node.children[pos].node;
      if rest.is_empty() {
        Some(child)
      } else {
        descend(child, rest)
      }
    }
    descend(&mut self.root, path)
  }
}

impl LayerNode {
  fn ensure_child(&mut self, name: &str) -> (u32, &mut LayerNode) {
    if let Some(pos) = self.children.iter().position(|c| c.name == name) {
      let order = self.children[pos].order;
      let node = &mut self.children[pos].node;
      return (order, node);
    }
    let order = self.children.len() as u32;
    self.children.push(LayerChild {
      name: name.to_string(),
      order,
      node: LayerNode::default(),
    });
    let len = self.children.len();
    let node = &mut self.children[len - 1].node;
    (order, node)
  }
}

/// CSS property values
#[derive(Debug, Clone)]
pub enum PropertyValue {
  Color(Color),
  Length(Length),
  Percentage(f32),
  Number(f32),
  Keyword(String),
  String(String),
  Url(String),
  Multiple(Vec<PropertyValue>),
  FontFamily(Vec<String>),
  BoxShadow(Vec<BoxShadow>),
  TextShadow(Vec<TextShadow>),
  Transform(Vec<Transform>),
  LinearGradient {
    angle: f32,
    stops: Vec<ColorStop>,
  },
  RadialGradient {
    shape: RadialGradientShape,
    size: RadialGradientSize,
    position: GradientPosition,
    stops: Vec<ColorStop>,
  },
  RepeatingLinearGradient {
    angle: f32,
    stops: Vec<ColorStop>,
  },
  RepeatingRadialGradient {
    shape: RadialGradientShape,
    size: RadialGradientSize,
    position: GradientPosition,
    stops: Vec<ColorStop>,
  },
  ConicGradient {
    from_angle: f32,
    position: GradientPosition,
    stops: Vec<ColorStop>,
  },
  RepeatingConicGradient {
    from_angle: f32,
    position: GradientPosition,
    stops: Vec<ColorStop>,
  },
  /// Raw custom property value (stored unparsed)
  Custom(String),
}

// ============================================================================
// Shadow and gradient types
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct BoxShadow {
  pub offset_x: Length,
  pub offset_y: Length,
  pub blur_radius: Length,
  pub spread_radius: Length,
  pub color: Rgba,
  pub inset: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextShadow {
  pub offset_x: Length,
  pub offset_y: Length,
  pub blur_radius: Length,
  /// None represents currentColor.
  pub color: Option<Rgba>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RadialGradientShape {
  Circle,
  Ellipse,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RadialGradientSize {
  ClosestSide,
  FarthestSide,
  ClosestCorner,
  FarthestCorner,
  Explicit { x: Length, y: Option<Length> },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GradientPositionComponent {
  pub alignment: f32,
  pub offset: Length,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GradientPosition {
  pub x: GradientPositionComponent,
  pub y: GradientPositionComponent,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ColorStop {
  pub color: Color,
  pub position: Option<f32>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Transform {
  Translate(Length, Length),
  TranslateX(Length),
  TranslateY(Length),
  TranslateZ(Length),
  Translate3d(Length, Length, Length),
  Scale(f32, f32),
  ScaleX(f32),
  ScaleY(f32),
  ScaleZ(f32),
  Scale3d(f32, f32, f32),
  Rotate(f32),
  RotateZ(f32),
  RotateX(f32),
  RotateY(f32),
  Rotate3d(f32, f32, f32, f32),
  SkewX(f32),
  SkewY(f32),
  Skew(f32, f32),
  Perspective(Length),
  Matrix(f32, f32, f32, f32, f32, f32),
  Matrix3d([f32; 16]),
}
