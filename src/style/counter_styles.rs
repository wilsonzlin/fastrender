//! Custom counter style definitions and formatting.
//!
//! Implements `@counter-style` parsing and resolution along with runtime
//! formatting for `counter()`/`counters()` and list markers.

use crate::style::content::CounterStyle;
use std::collections::{HashMap, HashSet};
use std::fmt;

/// Reference to a counter style, either built-in or custom.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CounterStyleName {
  Builtin(CounterStyle),
  Custom(String),
}

impl CounterStyleName {
  /// Create a name from authored text (case-insensitive for identifiers).
  pub fn parse(name: &str) -> Self {
    if let Some(builtin) = CounterStyle::parse(name) {
      CounterStyleName::Builtin(builtin)
    } else {
      CounterStyleName::Custom(name.trim().to_ascii_lowercase())
    }
  }

  /// Normalized CSS name for lookup.
  pub fn as_css_name(&self) -> String {
    match self {
      CounterStyleName::Builtin(builtin) => builtin.to_string(),
      CounterStyleName::Custom(name) => name.clone(),
    }
  }
}

impl fmt::Display for CounterStyleName {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      CounterStyleName::Builtin(b) => write!(f, "{}", b),
      CounterStyleName::Custom(name) => write!(f, "{}", name),
    }
  }
}

impl From<CounterStyle> for CounterStyleName {
  fn from(value: CounterStyle) -> Self {
    CounterStyleName::Builtin(value)
  }
}

impl From<&str> for CounterStyleName {
  fn from(value: &str) -> Self {
    CounterStyleName::parse(value)
  }
}

/// The counter system descriptor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CounterSystem {
  Cyclic,
  Numeric,
  Alphabetic,
  Symbolic,
  Additive,
  Fixed(i32),
  Extends(String),
}

/// The speak-as descriptor. Currently stored for completeness.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpeakAs {
  Auto,
  Bullets,
  Numbers,
  Words,
  SpellOut,
  Other(String),
}

/// Parsed @counter-style rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CounterStyleRule {
  pub name: String,
  pub system: Option<CounterSystem>,
  pub symbols: Option<Vec<String>>,
  pub additive_symbols: Option<Vec<(i32, String)>>,
  pub negative: Option<(String, String)>,
  pub prefix: Option<String>,
  pub suffix: Option<String>,
  pub pad: Option<(u32, String)>,
  pub range: Option<Vec<(i64, i64)>>,
  pub fallback: Option<String>,
  pub speak_as: Option<SpeakAs>,
}

impl CounterStyleRule {
  pub fn new(name: impl Into<String>) -> Self {
    Self {
      name: name.into().to_ascii_lowercase(),
      system: None,
      symbols: None,
      additive_symbols: None,
      negative: None,
      prefix: None,
      suffix: None,
      pad: None,
      range: None,
      fallback: None,
      speak_as: None,
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
enum CounterStyleDefinition {
  Builtin(CounterStyle),
  Custom(CounterStyleRule),
}

/// Registry of counter styles available for formatting.
#[derive(Debug, Clone, PartialEq)]
pub struct CounterStyleRegistry {
  styles: HashMap<String, CounterStyleDefinition>,
}

impl Default for CounterStyleRegistry {
  fn default() -> Self {
    Self::with_builtins()
  }
}

impl CounterStyleRegistry {
  /// Create a registry pre-populated with built-in counter styles.
  pub fn with_builtins() -> Self {
    let mut styles = HashMap::new();
    for builtin in builtin_styles() {
      styles.insert(
        builtin.to_string(),
        CounterStyleDefinition::Builtin(*builtin),
      );
    }
    Self { styles }
  }

  /// Register/override a custom counter style definition.
  pub fn register(&mut self, rule: CounterStyleRule) {
    let name = rule.name.to_ascii_lowercase();
    self
      .styles
      .insert(name, CounterStyleDefinition::Custom(rule));
  }

  /// Format a value using the named counter style.
  pub fn format_value<S: Into<CounterStyleName>>(&self, value: i32, style: S) -> String {
    let style = style.into();
    let mut visited = HashSet::new();
    self.format_by_name(&style.as_css_name(), value, &mut visited)
  }

  fn format_by_name(&self, name: &str, value: i32, visited: &mut HashSet<String>) -> String {
    let key = name.to_ascii_lowercase();
    if !visited.insert(key.clone()) {
      return CounterStyle::Decimal.format(value);
    }

    if let Some(def) = self.styles.get(&key) {
      match def {
        CounterStyleDefinition::Builtin(builtin) => return builtin.format(value),
        CounterStyleDefinition::Custom(rule) => {
          if let Some(resolved) = self.resolve_rule(rule, visited) {
            return resolved.format(value, self, visited);
          }
        }
      }
    }

    if let Some(builtin) = CounterStyle::parse(&key) {
      return builtin.format(value);
    }

    CounterStyle::Decimal.format(value)
  }

  fn resolve_rule(
    &self,
    rule: &CounterStyleRule,
    visited: &mut HashSet<String>,
  ) -> Option<ResolvedCounterStyle> {
    let mut base: Option<ResolvedCounterStyle> = None;
    let mut system = rule.system.clone().unwrap_or(CounterSystem::Symbolic);
    if let CounterSystem::Extends(ref parent) = system {
      base = self.resolve_by_name(parent, visited);
      system = base.as_ref()?.system.clone();
    }

    let symbols = rule
      .symbols
      .clone()
      .or_else(|| base.as_ref().map(|b| b.symbols.clone()))
      .unwrap_or_default();
    let additive_symbols = rule
      .additive_symbols
      .clone()
      .or_else(|| base.as_ref().map(|b| b.additive_symbols.clone()))
      .unwrap_or_default();
    let negative = rule
      .negative
      .clone()
      .or_else(|| base.as_ref().map(|b| b.negative.clone()))
      .unwrap_or_else(|| ("-".to_string(), String::new()));
    let pad = rule
      .pad
      .clone()
      .or_else(|| base.as_ref().and_then(|b| b.pad.clone()));
    let range = if let Some(r) = &rule.range {
      r.clone()
    } else if let Some(base) = &base {
      base.range.clone()
    } else {
      default_range(&system, &symbols)
    };
    let fallback = rule
      .fallback
      .clone()
      .or_else(|| base.as_ref().map(|b| b.fallback.clone()))
      .unwrap_or_else(|| "decimal".to_string());
    let speak_as = rule
      .speak_as
      .clone()
      .or_else(|| base.as_ref().and_then(|b| b.speak_as.clone()));

    Some(ResolvedCounterStyle::new(
      system,
      symbols,
      additive_symbols,
      negative,
      pad,
      range,
      fallback,
      speak_as,
    ))
  }

  fn resolve_by_name(
    &self,
    name: &str,
    visited: &mut HashSet<String>,
  ) -> Option<ResolvedCounterStyle> {
    let key = name.to_ascii_lowercase();
    if let Some(def) = self.styles.get(&key) {
      return match def {
        CounterStyleDefinition::Builtin(builtin) => Some(builtin_style(*builtin)),
        CounterStyleDefinition::Custom(rule) => self.resolve_rule(rule, visited),
      };
    }
    CounterStyle::parse(&key).map(builtin_style)
  }
}

#[derive(Debug, Clone)]
struct ResolvedCounterStyle {
  system: CounterSystem,
  symbols: Vec<String>,
  additive_symbols: Vec<(i32, String)>,
  negative: (String, String),
  pad: Option<(u32, String)>,
  range: Vec<(i64, i64)>,
  fallback: String,
  speak_as: Option<SpeakAs>,
}

impl ResolvedCounterStyle {
  fn new(
    system: CounterSystem,
    mut symbols: Vec<String>,
    mut additive_symbols: Vec<(i32, String)>,
    negative: (String, String),
    pad: Option<(u32, String)>,
    range: Vec<(i64, i64)>,
    fallback: String,
    speak_as: Option<SpeakAs>,
  ) -> Self {
    additive_symbols.sort_by(|a, b| b.0.cmp(&a.0));
    additive_symbols.retain(|(weight, _)| *weight > 0);
    symbols.retain(|s| !s.is_empty());
    Self {
      system,
      symbols,
      additive_symbols,
      negative,
      pad,
      range,
      fallback,
      speak_as,
    }
  }

  fn in_range(&self, value: i64) -> bool {
    if self.range.is_empty() {
      return true;
    }
    self
      .range
      .iter()
      .any(|(start, end)| value >= *start && value <= *end)
  }

  fn format(
    &self,
    value: i32,
    registry: &CounterStyleRegistry,
    visited: &mut HashSet<String>,
  ) -> String {
    let value_i64 = value as i64;
    if !self.in_range(value_i64) {
      return registry.format_by_name(&self.fallback, value, visited);
    }

    let negative_value = value_i64 < 0;
    let magnitude = value_i64.abs();
    let mut repr = match self.format_positive(magnitude) {
      Some(r) => r,
      None => return registry.format_by_name(&self.fallback, value, visited),
    };

    if let Some((width, pad_symbol)) = &self.pad {
      let width = *width as usize;
      let current = repr.chars().count();
      if current < width {
        let mut pad = String::new();
        while pad.chars().count() + current < width {
          pad.push_str(pad_symbol);
        }
        repr = format!("{}{}", pad, repr);
      }
    }

    if negative_value {
      repr = format!("{}{}{}", self.negative.0, repr, self.negative.1);
    }

    repr
  }

  fn format_positive(&self, value: i64) -> Option<String> {
    match &self.system {
      CounterSystem::Cyclic => format_cyclic(value, &self.symbols),
      CounterSystem::Fixed(start) => format_fixed(value, *start as i64, &self.symbols),
      CounterSystem::Numeric => format_numeric(value, &self.symbols),
      CounterSystem::Alphabetic => format_alphabetic(value, &self.symbols),
      CounterSystem::Symbolic => format_symbolic(value, &self.symbols),
      CounterSystem::Additive => format_additive(value, &self.additive_symbols),
      CounterSystem::Extends(_) => None,
    }
  }
}

fn format_cyclic(value: i64, symbols: &[String]) -> Option<String> {
  if symbols.is_empty() {
    return None;
  }
  let len = symbols.len() as i64;
  let idx = if value == 0 {
    0
  } else {
    (value - 1).rem_euclid(len)
  } as usize;
  symbols.get(idx).cloned()
}

fn format_fixed(value: i64, start: i64, symbols: &[String]) -> Option<String> {
  if symbols.is_empty() {
    return None;
  }
  let idx = value - start;
  if idx < 0 || idx >= symbols.len() as i64 {
    return None;
  }
  symbols.get(idx as usize).cloned()
}

fn format_numeric(mut value: i64, symbols: &[String]) -> Option<String> {
  if symbols.len() < 2 {
    return None;
  }
  let base = symbols.len() as i64;
  if value == 0 {
    return symbols.get(0).cloned();
  }
  let mut out = Vec::new();
  while value > 0 {
    let digit = (value % base) as usize;
    out.push(symbols.get(digit)?.clone());
    value /= base;
  }
  out.reverse();
  Some(out.join(""))
}

fn format_alphabetic(mut value: i64, symbols: &[String]) -> Option<String> {
  if symbols.len() < 2 || value <= 0 {
    return None;
  }
  let base = symbols.len() as i64;
  let mut out = Vec::new();
  while value > 0 {
    value -= 1;
    let digit = (value % base) as usize;
    out.push(symbols.get(digit)?.clone());
    value /= base;
  }
  out.reverse();
  Some(out.join(""))
}

fn format_symbolic(value: i64, symbols: &[String]) -> Option<String> {
  if symbols.is_empty() || value <= 0 {
    return None;
  }
  if value as usize <= symbols.len() {
    return symbols.get(value as usize - 1).cloned();
  }
  let tail = symbols.last()?.clone();
  let repeat = (value as usize).saturating_sub(symbols.len()) + 1;
  Some(tail.repeat(repeat))
}

fn format_additive(mut value: i64, symbols: &[(i32, String)]) -> Option<String> {
  if symbols.is_empty() || value <= 0 {
    return None;
  }
  let mut out = String::new();
  for (weight, symbol) in symbols {
    let weight = *weight as i64;
    if weight <= 0 {
      continue;
    }
    while value >= weight {
      out.push_str(symbol);
      value -= weight;
    }
  }
  if value == 0 {
    Some(out)
  } else {
    None
  }
}

fn default_range(system: &CounterSystem, symbols: &[String]) -> Vec<(i64, i64)> {
  match system {
    CounterSystem::Alphabetic | CounterSystem::Symbolic => vec![(1, i64::MAX)],
    CounterSystem::Fixed(start) => vec![(*start as i64, *start as i64 + symbols.len() as i64 - 1)],
    CounterSystem::Additive => vec![(1, i64::MAX)],
    CounterSystem::Numeric | CounterSystem::Cyclic => vec![(i64::MIN, i64::MAX)],
    CounterSystem::Extends(_) => vec![(i64::MIN, i64::MAX)],
  }
  .into_iter()
  .filter(|(s, e)| s <= e)
  .collect()
}

fn builtin_styles() -> &'static [CounterStyle] {
  &[
    CounterStyle::Decimal,
    CounterStyle::DecimalLeadingZero,
    CounterStyle::Armenian,
    CounterStyle::LowerArmenian,
    CounterStyle::Georgian,
    CounterStyle::LowerRoman,
    CounterStyle::UpperRoman,
    CounterStyle::LowerAlpha,
    CounterStyle::UpperAlpha,
    CounterStyle::LowerGreek,
    CounterStyle::Disc,
    CounterStyle::Circle,
    CounterStyle::Square,
    CounterStyle::DisclosureOpen,
    CounterStyle::DisclosureClosed,
    CounterStyle::None,
  ]
}

fn builtin_style_symbols(
  style: CounterStyle,
) -> (
  CounterSystem,
  Vec<String>,
  Vec<(i32, String)>,
  Vec<(i64, i64)>,
) {
  match style {
    CounterStyle::Decimal => (
      CounterSystem::Numeric,
      (0..=9).map(|d| d.to_string()).collect(),
      Vec::new(),
      vec![(i64::MIN, i64::MAX)],
    ),
    CounterStyle::DecimalLeadingZero => (
      CounterSystem::Numeric,
      (0..=9).map(|d| d.to_string()).collect(),
      Vec::new(),
      vec![(i64::MIN, i64::MAX)],
    ),
    CounterStyle::LowerAlpha => (
      CounterSystem::Alphabetic,
      ('a'..='z').map(|c| c.to_string()).collect(),
      Vec::new(),
      vec![(1, i64::MAX)],
    ),
    CounterStyle::UpperAlpha => (
      CounterSystem::Alphabetic,
      ('A'..='Z').map(|c| c.to_string()).collect(),
      Vec::new(),
      vec![(1, i64::MAX)],
    ),
    CounterStyle::LowerRoman => (
      CounterSystem::Additive,
      Vec::new(),
      roman_symbols(true),
      vec![(1, 3999)],
    ),
    CounterStyle::UpperRoman => (
      CounterSystem::Additive,
      Vec::new(),
      roman_symbols(false),
      vec![(1, 3999)],
    ),
    CounterStyle::Armenian => (
      CounterSystem::Additive,
      Vec::new(),
      crate::style::content::ARMENIAN_UPPER
        .iter()
        .map(|(v, s)| (*v, s.to_string()))
        .collect(),
      vec![(1, 9999)],
    ),
    CounterStyle::LowerArmenian => (
      CounterSystem::Additive,
      Vec::new(),
      crate::style::content::ARMENIAN_LOWER
        .iter()
        .map(|(v, s)| (*v, s.to_string()))
        .collect(),
      vec![(1, 9999)],
    ),
    CounterStyle::Georgian => (
      CounterSystem::Additive,
      Vec::new(),
      crate::style::content::GEORGIAN_SYMBOLS
        .iter()
        .map(|(v, s)| (*v, s.to_string()))
        .collect(),
      vec![(1, 19999)],
    ),
    CounterStyle::LowerGreek => (
      CounterSystem::Alphabetic,
      crate::style::content::GREEK
        .iter()
        .map(|c| c.to_string())
        .collect(),
      Vec::new(),
      vec![(1, crate::style::content::GREEK.len() as i64)],
    ),
    CounterStyle::Disc => (
      CounterSystem::Cyclic,
      vec!["•".to_string()],
      Vec::new(),
      vec![(i64::MIN, i64::MAX)],
    ),
    CounterStyle::Circle => (
      CounterSystem::Cyclic,
      vec!["◦".to_string()],
      Vec::new(),
      vec![(i64::MIN, i64::MAX)],
    ),
    CounterStyle::Square => (
      CounterSystem::Cyclic,
      vec!["▪".to_string()],
      Vec::new(),
      vec![(i64::MIN, i64::MAX)],
    ),
    CounterStyle::DisclosureOpen => (
      CounterSystem::Cyclic,
      vec!["▾".to_string()],
      Vec::new(),
      vec![(i64::MIN, i64::MAX)],
    ),
    CounterStyle::DisclosureClosed => (
      CounterSystem::Cyclic,
      vec!["▸".to_string()],
      Vec::new(),
      vec![(i64::MIN, i64::MAX)],
    ),
    CounterStyle::None => (
      CounterSystem::Cyclic,
      vec![String::new()],
      Vec::new(),
      vec![(i64::MIN, i64::MAX)],
    ),
  }
}

fn builtin_style(style: CounterStyle) -> ResolvedCounterStyle {
  let (system, symbols, additive, range) = builtin_style_symbols(style);
  let pad = if matches!(style, CounterStyle::DecimalLeadingZero) {
    Some((2, "0".to_string()))
  } else {
    None
  };
  let fallback = if matches!(style, CounterStyle::None) {
    "none".to_string()
  } else {
    "decimal".to_string()
  };
  ResolvedCounterStyle::new(
    system,
    symbols,
    additive,
    ("-".to_string(), String::new()),
    pad,
    range,
    fallback,
    None,
  )
}

fn roman_symbols(lowercase: bool) -> Vec<(i32, String)> {
  let mut symbols = vec![
    (1000, "M".to_string()),
    (900, "CM".to_string()),
    (500, "D".to_string()),
    (400, "CD".to_string()),
    (100, "C".to_string()),
    (90, "XC".to_string()),
    (50, "L".to_string()),
    (40, "XL".to_string()),
    (10, "X".to_string()),
    (9, "IX".to_string()),
    (5, "V".to_string()),
    (4, "IV".to_string()),
    (1, "I".to_string()),
  ];
  if lowercase {
    for (_, sym) in symbols.iter_mut() {
      *sym = sym.to_lowercase();
    }
  }
  symbols
}
