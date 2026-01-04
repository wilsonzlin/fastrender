//! CSS Custom Property (var()) Resolution
//!
//! Implements token-based resolution of `var()` references using `cssparser`
//! so that nested functions, fallbacks with commas, and repeated substitutions
//! are handled correctly.

use crate::css::properties::{parse_length, parse_property_value_after_var_resolution};
use crate::css::types::PropertyValue;
use crate::style::custom_property_store::CustomPropertyStore;
use cssparser::ParseError;
use cssparser::ParseErrorKind;
use cssparser::Parser;
use cssparser::ParserInput;
use cssparser::ToCss;
use cssparser::Token;
use std::borrow::Cow;
#[cfg(test)]
use std::cell::Cell;

/// Maximum depth for recursive var() resolution to prevent infinite loops
const MAX_RECURSION_DEPTH: usize = 10;

/// Separator inserted when serializing token-spliced values via string concatenation.
///
/// CSS `var()` substitution operates on token streams; adjacent substitutions can create token
/// sequences that are not representable by naïvely concatenating the source text (e.g.
/// `0` + `calc(...)` would become `0calc(...)` which tokenizes as a dimension).
///
/// We model token splicing by inserting a minimal whitespace separator only when required to avoid
/// token merging during a later re-tokenization pass.
const TOKEN_SPLICE_SEPARATOR: &str = " ";

#[cfg(test)]
std::thread_local! {
  static TOKEN_RESOLVER_ENTRY_COUNT: Cell<usize> = Cell::new(0);
}

#[inline]
fn is_ident_byte(b: u8) -> bool {
  b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_') || b >= 0x80
}

#[inline]
fn is_css_whitespace_byte(b: u8) -> bool {
  matches!(b, b' ' | b'\n' | b'\t' | b'\r' | b'\x0C')
}

#[inline]
fn needs_token_splice_separator(prev: u8, next: u8, next_next: Option<u8>) -> bool {
  if is_css_whitespace_byte(prev) || is_css_whitespace_byte(next) {
    return false;
  }

  // Starting a comment across a splice boundary (`/*`) would drastically change the token stream.
  if prev == b'/' && next == b'*' {
    return true;
  }

  // `@foo` and `#foo` are single tokens. If the splice boundary would otherwise create them,
  // separate the tokens.
  if (prev == b'@' || prev == b'#') && is_ident_byte(next) {
    return true;
  }

  // Identifier / number adjacency is where most token-merging bugs happen:
  // - `0` + `calc(...)` => dimension token `0calc`
  // - `0` + `0` => number token `00`
  // - `px` + `calc(...)` => ident `pxcalc` (unit merging)
  if is_ident_byte(prev) && is_ident_byte(next) {
    return true;
  }

  // A bare identifier immediately followed by `(` becomes a function token. Token-stream splicing
  // can create `Ident` + `ParenthesisBlock` sequences that must not be re-tokenized as a function.
  if is_ident_byte(prev) && next == b'(' {
    return true;
  }

  // `+` / `-` / `.` can start a number token. If a splice boundary would cause them to be
  // re-tokenized as part of the following number, insert a separator.
  if (prev == b'+' || prev == b'-') && (next.is_ascii_digit() || next == b'.') {
    // For the `+.` / `-.` cases, only treat it as a number if there is a digit afterwards.
    if next != b'.' || next_next.is_some_and(|b| b.is_ascii_digit()) {
      return true;
    }
  }

  if prev == b'.' && next.is_ascii_digit() {
    return true;
  }

  false
}

#[inline]
fn push_css_with_token_splice_boundary(out: &mut String, chunk: &str) {
  if chunk.is_empty() {
    return;
  }

  if out.is_empty() {
    out.push_str(chunk);
    return;
  }

  let prev = *out
    .as_bytes()
    .last()
    .expect("non-empty string must have last byte");
  let next_bytes = chunk.as_bytes();
  let next = next_bytes[0];
  let next_next = next_bytes.get(1).copied();

  if needs_token_splice_separator(prev, next, next_next) {
    out.push_str(TOKEN_SPLICE_SEPARATOR);
  }
  out.push_str(chunk);
}

#[inline]
fn contains_ascii_case_insensitive_var_call(raw: &str) -> bool {
  let bytes = raw.as_bytes();
  if bytes.len() < 4 {
    return false;
  }

  let mut idx = 0usize;
  while idx + 3 < bytes.len() {
    let b0 = bytes[idx];
    if b0 == b'v' || b0 == b'V' {
      let b1 = bytes[idx + 1];
      let b2 = bytes[idx + 2];
      if (b1 == b'a' || b1 == b'A') && (b2 == b'r' || b2 == b'R') && bytes[idx + 3] == b'(' {
        return true;
      }
    }
    idx += 1;
  }
  false
}

#[inline]
fn parse_simple_var_call<'a>(raw: &'a str) -> Option<(&'a str, Option<&'a str>)> {
  let trimmed = raw.trim();
  if trimmed.len() < 6
    || !trimmed
      .get(..4)
      .is_some_and(|prefix| prefix.eq_ignore_ascii_case("var("))
    || !trimmed.ends_with(')')
  {
    return None;
  }

  // Reject anything with nested parentheses; those require a full tokenizer to interpret.
  let inner = trimmed.get(4..trimmed.len().saturating_sub(1))?;
  if inner.contains('(') || inner.contains(')') {
    return None;
  }

  let inner = inner.trim();
  let (name_chunk, fallback_chunk) = inner
    .split_once(',')
    .map(|(name, fallback)| (name, Some(fallback)))
    .unwrap_or((inner, None));

  let name = name_chunk.trim();
  if !name.starts_with("--") || name.contains(|c: char| c.is_whitespace()) {
    return None;
  }

  let fallback = fallback_chunk.map(str::trim);
  if let Some(fallback) = fallback {
    // `var(--x,)` uses an *empty* fallback. This is distinct from omitting the fallback entirely
    // (`var(--x)`) because empty fallbacks are valid in contexts where the substituted token stream
    // can disappear (e.g. Tailwind-style `transform: var(--tw-rotate-x,) ...`).
    if fallback.is_empty() {
      return Some((name, Some("")));
    }
    // Only support a single comma here; multiple commas require tokenization to disambiguate.
    if fallback_chunk.is_some_and(|rest| rest.contains(',')) {
      return None;
    }
    return Some((name, Some(fallback)));
  }

  Some((name, None))
}

#[inline]
fn try_resolve_var_calls_without_tokenizer<'a>(
  raw: &'a str,
  custom_properties: &'a CustomPropertyStore,
  depth: usize,
) -> Option<Result<String, VarResolutionResult<'a>>> {
  if depth >= MAX_RECURSION_DEPTH {
    return Some(Err(VarResolutionResult::RecursionLimitExceeded));
  }

  // The fast path only handles unescaped values. If the raw string contains backslashes it may
  // hide `var(` via escapes; fall back to cssparser in that case for correctness.
  if raw.as_bytes().contains(&b'\\') {
    return None;
  }

  // Cheap check: most values don't contain var() at all.
  if !contains_ascii_case_insensitive_var_call(raw) {
    return None;
  }

  let bytes = raw.as_bytes();
  let mut i = 0usize;
  let mut last = 0usize;
  let mut output = String::new();
  let mut in_comment = false;
  let mut in_string: Option<u8> = None;
  let mut any = false;
  // Reuse the same recursion stack for each top-level var() call to avoid repeated allocations.
  let mut stack: Vec<String> = Vec::new();

  while i < bytes.len() {
    let b = bytes[i];

    if in_comment {
      if b == b'*' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
        in_comment = false;
        i += 2;
        continue;
      }
      i += 1;
      continue;
    }

    if let Some(quote) = in_string {
      if b == quote {
        in_string = None;
      }
      i += 1;
      continue;
    }

    if b == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'*' {
      in_comment = true;
      i += 2;
      continue;
    }

    if b == b'"' || b == b'\'' {
      in_string = Some(b);
      i += 1;
      continue;
    }

    if i + 3 < bytes.len()
      && b.to_ascii_lowercase() == b'v'
      && bytes[i + 1].to_ascii_lowercase() == b'a'
      && bytes[i + 2].to_ascii_lowercase() == b'r'
      && bytes[i + 3] == b'('
    {
      // Ensure the match isn't part of a longer identifier (e.g. `somevar(`).
      let prev = i.checked_sub(1).and_then(|idx| bytes.get(idx).copied());
      if prev.map_or(false, is_ident_byte) {
        i += 1;
        continue;
      }

      // Find the end of the `var(...)` call. We only accept a "simple" var() call here with no
      // nested parentheses inside the argument list (nested blocks/fallback functions require a
      // full tokenizer).
      let mut end = i + 4;
      let mut saw_nested_paren = false;
      while end < bytes.len() {
        match bytes[end] {
          b')' => break,
          b'(' => {
            saw_nested_paren = true;
            break;
          }
          b'"' | b'\'' => return None,
          b'/' if end + 1 < bytes.len() && bytes[end + 1] == b'*' => return None,
          _ => end += 1,
        }
      }

      if saw_nested_paren || end >= bytes.len() {
        return None;
      }

      let var_call = raw.get(i..end + 1)?;
      let Some((name, fallback)) = parse_simple_var_call(var_call) else {
        return None;
      };

      if !any {
        output.reserve(raw.len());
      }
      push_css_with_token_splice_boundary(&mut output, raw.get(last..i)?);

      stack.clear();
      match resolve_variable_reference(
        name,
        fallback.map(Cow::Borrowed),
        custom_properties,
        &mut stack,
        depth,
      ) {
        Ok(resolved) => push_css_with_token_splice_boundary(&mut output, resolved.as_ref()),
        Err(err) => return Some(Err(err)),
      }

      any = true;
      last = end + 1;
      i = end + 1;
      continue;
    }

    i += 1;
  }

  if !any {
    return None;
  }

  push_css_with_token_splice_boundary(&mut output, raw.get(last..)?);
  Some(Ok(output))
}

/// Result of a var() resolution attempt
#[derive(Debug, Clone)]
pub enum VarResolutionResult<'a> {
  /// Successfully resolved to a value
  Resolved {
    value: ResolvedPropertyValue<'a>,
    css_text: Cow<'a, str>,
  },
  /// The variable was not found and no fallback was provided (or the fallback failed to resolve)
  NotFound(String),
  /// Recursion depth exceeded (possible circular reference)
  RecursionLimitExceeded,
  /// Invalid var() syntax
  InvalidSyntax(String),
}

#[derive(Debug, Clone)]
pub enum ResolvedPropertyValue<'a> {
  Borrowed(&'a PropertyValue),
  Owned(PropertyValue),
}

impl<'a> ResolvedPropertyValue<'a> {
  #[inline]
  pub fn as_ref(&self) -> &PropertyValue {
    match self {
      ResolvedPropertyValue::Borrowed(value) => value,
      ResolvedPropertyValue::Owned(value) => value,
    }
  }

  #[inline]
  pub fn into_owned(self) -> PropertyValue {
    match self {
      ResolvedPropertyValue::Borrowed(value) => value.clone(),
      ResolvedPropertyValue::Owned(value) => value,
    }
  }
}

impl<'a> AsRef<PropertyValue> for ResolvedPropertyValue<'a> {
  #[inline]
  fn as_ref(&self) -> &PropertyValue {
    Self::as_ref(self)
  }
}

impl<'a> VarResolutionResult<'a> {
  /// Returns the resolved value if successful, otherwise returns the original value
  pub fn unwrap_or(self, default: PropertyValue) -> PropertyValue {
    match self {
      VarResolutionResult::Resolved { value, .. } => value.into_owned(),
      _ => default,
    }
  }

  /// Returns true if the resolution was successful
  pub fn is_resolved(&self) -> bool {
    matches!(self, VarResolutionResult::Resolved { .. })
  }

  /// Returns the CSS serialization of the resolved value if available.
  ///
  /// For invalid syntax, this returns the resolved string that failed to parse,
  /// which is still useful for consumers that need the token-stream result.
  pub fn css_text(&self) -> Option<&str> {
    match self {
      VarResolutionResult::Resolved { css_text, .. } => Some(css_text.as_ref()),
      VarResolutionResult::InvalidSyntax(text) => Some(text.as_str()),
      _ => None,
    }
  }
}

/// Resolves CSS `var()` references using the provided custom properties.
///
/// This helper performs property-agnostic resolution (parses fallback/results without knowing
/// the destination property). For property-aware parsing, use `resolve_var_for_property`.
pub fn resolve_var(
  value: &PropertyValue,
  custom_properties: &CustomPropertyStore,
) -> PropertyValue {
  match resolve_var_recursive(value, custom_properties, 0, "") {
    VarResolutionResult::Resolved { value, .. } => value.into_owned(),
    other => other.unwrap_or(value.clone()),
  }
}

/// Resolves CSS `var()` references with knowledge of the target property.
///
/// Passing the property name allows the resolver to parse the substituted value using the
/// appropriate grammar (e.g., background layers with commas), rather than the generic parser.
pub fn resolve_var_for_property<'a>(
  value: &'a PropertyValue,
  custom_properties: &'a CustomPropertyStore,
  property_name: &str,
) -> VarResolutionResult<'a> {
  match value {
    PropertyValue::Keyword(raw) | PropertyValue::Custom(raw) => {
      // Most declarations are simple keywords (display, position, etc.) and do not contain any
      // var() references. Avoid feeding such values through cssparser tokenization by doing a
      // cheap ASCII-case-insensitive substring check for `var(` first.
      //
      // Note: If the value contains a backslash escape, conservatively fall back to token parsing
      // so we don't miss an escaped `var()` function name. Function tokens require a literal `(`,
      // so values without any `(` can skip the slow-path even if they contain backslashes.
      if !contains_ascii_case_insensitive_var_call(raw)
        && (!raw.as_bytes().contains(&b'\\') || !raw.as_bytes().contains(&b'('))
      {
        return VarResolutionResult::Resolved {
          value: ResolvedPropertyValue::Borrowed(value),
          css_text: Cow::Borrowed(""),
        };
      }

      // Fast path: `var(--x)` is extremely common (especially for color/spacing tokens). Avoid a
      // full `cssparser` token walk when the entire value is a single var() call with no fallback.
      //
      // This also avoids allocating/building an output string for the outer token stream; we only
      // materialize the referenced custom property's value.
      if !raw.as_bytes().contains(&b'\\') {
        if let Some((name, fallback)) = parse_simple_var_call(raw) {
          let mut stack = Vec::new();
          match resolve_variable_reference(
            name,
            fallback.map(Cow::Borrowed),
            custom_properties,
            &mut stack,
            0,
          ) {
            Ok(resolved) => match parse_value_after_resolution(resolved.as_ref(), property_name) {
              Some(parsed) => {
                return VarResolutionResult::Resolved {
                  value: ResolvedPropertyValue::Owned(parsed),
                  css_text: resolved,
                };
              }
              None => return VarResolutionResult::InvalidSyntax(resolved.into_owned()),
            },
            Err(err) => return err,
          }
        }

        if let Some(result) = try_resolve_var_calls_without_tokenizer(raw, custom_properties, 0) {
          match result {
            Ok(resolved) => match parse_value_after_resolution(&resolved, property_name) {
              Some(parsed) => {
                return VarResolutionResult::Resolved {
                  value: ResolvedPropertyValue::Owned(parsed),
                  css_text: Cow::Owned(resolved),
                };
              }
              None => return VarResolutionResult::InvalidSyntax(resolved),
            },
            Err(err) => return err,
          }
        }
      }
    }
    _ => {}
  }
  resolve_var_recursive(value, custom_properties, 0, property_name)
}

/// Resolves var() references with explicit depth tracking
///
/// This function is useful when you need to track the recursion depth,
/// for example when implementing custom resolution strategies.
pub fn resolve_var_with_depth(
  value: &PropertyValue,
  custom_properties: &CustomPropertyStore,
  depth: usize,
) -> PropertyValue {
  match resolve_var_recursive(value, custom_properties, depth, "") {
    VarResolutionResult::Resolved { value, .. } => value.into_owned(),
    other => other.unwrap_or(value.clone()),
  }
}

/// Internal recursive implementation of var() resolution
fn resolve_var_recursive<'a>(
  value: &'a PropertyValue,
  custom_properties: &'a CustomPropertyStore,
  depth: usize,
  property_name: &str,
) -> VarResolutionResult<'a> {
  if depth >= MAX_RECURSION_DEPTH {
    return VarResolutionResult::RecursionLimitExceeded;
  }

  match value {
    PropertyValue::Keyword(raw) => {
      resolve_from_string(raw, custom_properties, depth, property_name)
    }
    PropertyValue::Custom(raw) => resolve_from_string(raw, custom_properties, depth, property_name),
    _ => VarResolutionResult::Resolved {
      value: ResolvedPropertyValue::Borrowed(value),
      css_text: Cow::Borrowed(""),
    },
  }
}

fn resolve_from_string<'a>(
  raw: &'a str,
  custom_properties: &'a CustomPropertyStore,
  depth: usize,
  property_name: &str,
) -> VarResolutionResult<'a> {
  let mut stack = Vec::new();
  match resolve_value_tokens(raw, custom_properties, &mut stack, depth) {
    Ok(resolved) => match parse_value_after_resolution(&resolved, property_name) {
      Some(value) => VarResolutionResult::Resolved {
        value: ResolvedPropertyValue::Owned(value),
        css_text: Cow::Owned(resolved),
      },
      None => VarResolutionResult::InvalidSyntax(resolved),
    },
    Err(err) => err,
  }
}

fn resolve_value_tokens<'a, 'i>(
  value: &'i str,
  custom_properties: &'a CustomPropertyStore,
  stack: &mut Vec<String>,
  depth: usize,
) -> Result<String, VarResolutionResult<'a>>
where
  'a: 'i,
{
  if depth >= MAX_RECURSION_DEPTH {
    return Err(VarResolutionResult::RecursionLimitExceeded);
  }

  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);
  resolve_tokens_from_parser(&mut parser, custom_properties, stack, depth)
}

fn resolve_tokens_from_parser<'a, 'i, 't>(
  parser: &mut Parser<'i, 't>,
  custom_properties: &'a CustomPropertyStore,
  stack: &mut Vec<String>,
  depth: usize,
) -> Result<String, VarResolutionResult<'a>>
where
  'a: 'i,
{
  #[cfg(test)]
  TOKEN_RESOLVER_ENTRY_COUNT.with(|count| count.set(count.get() + 1));

  let mut output = String::new();

  while let Ok(token) = parser.next_including_whitespace_and_comments() {
    match token {
      Token::Function(name) if name.eq_ignore_ascii_case("var") => {
        let nested = parser.parse_nested_block(|nested| {
          parse_var_function(nested, custom_properties, stack, depth)
            .map_err(|err| nested.new_custom_error(err))
        });
        let resolved = map_nested_result(nested, "var")?;
        push_css_with_token_splice_boundary(&mut output, resolved.as_ref());
      }
      Token::Function(name) => {
        push_css_with_token_splice_boundary(&mut output, name.as_ref());
        output.push('(');
        let nested = parser.parse_nested_block(|nested| {
          resolve_tokens_from_parser(nested, custom_properties, stack, depth)
            .map_err(|err| nested.new_custom_error(err))
        });
        // Avoid keeping `name` (which borrows from the parser input) live across the nested parse.
        // If the nested block is invalid, we still surface a generic hint.
        let resolved = map_nested_result(nested, "fn")?;
        output.push_str(&resolved);
        output.push(')');
      }
      Token::ParenthesisBlock => {
        let nested = parser.parse_nested_block(|nested| {
          resolve_tokens_from_parser(nested, custom_properties, stack, depth)
            .map_err(|err| nested.new_custom_error(err))
        });
        let resolved = map_nested_result(nested, "()")?;
        push_css_with_token_splice_boundary(&mut output, "(");
        output.push_str(&resolved);
        output.push(')');
      }
      Token::SquareBracketBlock => {
        let nested = parser.parse_nested_block(|nested| {
          resolve_tokens_from_parser(nested, custom_properties, stack, depth)
            .map_err(|err| nested.new_custom_error(err))
        });
        let resolved = map_nested_result(nested, "[]")?;
        push_css_with_token_splice_boundary(&mut output, "[");
        output.push_str(&resolved);
        output.push(']');
      }
      Token::CurlyBracketBlock => {
        let nested = parser.parse_nested_block(|nested| {
          resolve_tokens_from_parser(nested, custom_properties, stack, depth)
            .map_err(|err| nested.new_custom_error(err))
        });
        let resolved = map_nested_result(nested, "{}")?;
        push_css_with_token_splice_boundary(&mut output, "{");
        output.push_str(&resolved);
        output.push('}');
      }
      other => push_token_to_css(&mut output, &other),
    }
  }

  Ok(output)
}

fn map_nested_result<'a, 'i, T>(
  result: Result<T, ParseError<'i, VarResolutionResult<'a>>>,
  hint: &str,
) -> Result<T, VarResolutionResult<'a>> {
  match result {
    Ok(tokens) => Ok(tokens),
    Err(err) => match err.kind {
      ParseErrorKind::Custom(inner) => Err(inner),
      _ => Err(VarResolutionResult::InvalidSyntax(hint.to_string())),
    },
  }
}

fn parse_var_function<'a, 'i, 't>(
  parser: &mut Parser<'i, 't>,
  custom_properties: &'a CustomPropertyStore,
  stack: &mut Vec<String>,
  depth: usize,
) -> Result<Cow<'a, str>, VarResolutionResult<'a>>
where
  'a: 'i,
{
  let (var_name, fallback) = parse_var_function_arguments(parser)?;
  resolve_variable_reference(
    &var_name,
    fallback.map(Cow::Owned),
    custom_properties,
    stack,
    depth,
  )
}

fn parse_var_function_arguments<'a, 'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> Result<(String, Option<String>), VarResolutionResult<'a>> {
  let mut var_name: Option<String> = None;

  while let Ok(token) = parser.next_including_whitespace_and_comments() {
    match token {
      Token::WhiteSpace(_) | Token::Comment(_) => continue,
      Token::Ident(ident) => {
        let name = ident.as_ref().to_string();
        if !name.starts_with("--") {
          return Err(VarResolutionResult::InvalidSyntax(name));
        }
        var_name = Some(name);
        break;
      }
      other => {
        return Err(VarResolutionResult::InvalidSyntax(token_to_css_string(
          &other,
        )))
      }
    }
  }

  let Some(name) = var_name else {
    return Err(VarResolutionResult::InvalidSyntax(String::new()));
  };

  let fallback_start = loop {
    match parser.next_including_whitespace_and_comments() {
      Ok(Token::WhiteSpace(_) | Token::Comment(_)) => continue,
      Ok(Token::Comma) => break parser.position(),
      Ok(other) => {
        return Err(VarResolutionResult::InvalidSyntax(token_to_css_string(
          &other,
        )))
      }
      Err(_) => return Ok((name, None)),
    }
  };

  while let Ok(_) = parser.next_including_whitespace_and_comments() {}
  let fallback_slice = parser.slice_from(fallback_start);
  Ok((name, Some(fallback_slice.to_string())))
}

fn resolve_variable_reference<'a>(
  name: &str,
  fallback: Option<Cow<'a, str>>,
  custom_properties: &'a CustomPropertyStore,
  stack: &mut Vec<String>,
  depth: usize,
) -> Result<Cow<'a, str>, VarResolutionResult<'a>> {
  if depth >= MAX_RECURSION_DEPTH {
    return Err(VarResolutionResult::RecursionLimitExceeded);
  }

  if stack.iter().any(|n| n == name) {
    return Err(VarResolutionResult::RecursionLimitExceeded);
  }

  if let Some(value) = custom_properties.get(name) {
    // Fast path: if the custom property value can't possibly contain var() references (including
    // escape-hiding), we can skip a full cssparser token walk and just substitute the raw tokens.
    let raw = value.value.as_str();
    if !contains_ascii_case_insensitive_var_call(raw)
      && (!raw.as_bytes().contains(&b'\\') || !raw.as_bytes().contains(&b'('))
    {
      return Ok(Cow::Borrowed(raw));
    }

    stack.push(name.to_string());
    let result = if !raw.as_bytes().contains(&b'\\') {
      if let Some((nested_name, nested_fallback)) = parse_simple_var_call(raw) {
        resolve_variable_reference(
          nested_name,
          nested_fallback.map(Cow::Borrowed),
          custom_properties,
          stack,
          depth + 1,
        )
      } else {
        resolve_value_tokens(raw, custom_properties, stack, depth + 1).map(Cow::Owned)
      }
    } else {
      resolve_value_tokens(raw, custom_properties, stack, depth + 1).map(Cow::Owned)
    };
    stack.pop();
    return result;
  }

  if let Some(fallback_value) = fallback {
    // Same fast-path as above for literal fallback tokens.
    if !contains_ascii_case_insensitive_var_call(fallback_value.as_ref())
      && (!fallback_value.as_bytes().contains(&b'\\') || !fallback_value.as_bytes().contains(&b'('))
    {
      return Ok(fallback_value);
    }

    return resolve_value_tokens(fallback_value.as_ref(), custom_properties, stack, depth + 1)
      .map(Cow::Owned)
      .map_err(|err| match err {
        VarResolutionResult::NotFound(_) => VarResolutionResult::NotFound(name.to_string()),
        other => other,
      });
  }

  Err(VarResolutionResult::NotFound(name.to_string()))
}

fn parse_value_after_resolution(value: &str, property_name: &str) -> Option<PropertyValue> {
  if contains_var(value) {
    return None;
  }

  if property_name.is_empty() {
    Some(parse_untyped_value(value))
  } else {
    parse_property_value_after_var_resolution(property_name, value)
  }
}

fn parse_untyped_value(value: &str) -> PropertyValue {
  let trimmed = value.trim();
  if let Some(len) = parse_length(trimmed) {
    return PropertyValue::Length(len);
  }
  if let Ok(num) = trimmed.parse::<f32>() {
    return PropertyValue::Number(num);
  }
  if trimmed.ends_with('%') {
    if let Ok(num) = trimmed[..trimmed.len() - 1].parse::<f32>() {
      return PropertyValue::Percentage(num);
    }
  }
  PropertyValue::Keyword(trimmed.to_string())
}

#[inline]
fn push_token_to_css(out: &mut String, token: &Token) {
  let needs_boundary = match token {
    Token::WhiteSpace(_) | Token::Comment(_) => false,
    _ => true,
  };

  if needs_boundary && !out.is_empty() {
    // Compute the first byte of the serialized token so we can decide whether a token-splice
    // boundary separator is required.
    let (first, second) = match token {
      Token::Ident(ident) => (
        ident.as_bytes()[0],
        ident.as_bytes().get(1).copied(),
      ),
      Token::AtKeyword(_) => (b'@', None),
      Token::Hash(_) | Token::IDHash(_) => (b'#', None),
      Token::QuotedString(_) => (b'"', None),
      Token::UnquotedUrl(_) | Token::BadUrl(_) => (b'u', Some(b'r')),
      Token::Number { has_sign, value, .. }
      | Token::Percentage {
        has_sign,
        unit_value: value,
        ..
      }
      | Token::Dimension { has_sign, value, .. } => {
        if value.is_sign_negative() {
          (b'-', None)
        } else if *has_sign {
          (b'+', None)
        } else {
          (b'0', None)
        }
      }
      Token::Delim(ch) => (*ch as u8, None),
      Token::Colon => (b':', None),
      Token::Semicolon => (b';', None),
      Token::Comma => (b',', None),
      Token::IncludeMatch => (b'~', Some(b'=')),
      Token::DashMatch => (b'|', Some(b'=')),
      Token::PrefixMatch => (b'^', Some(b'=')),
      Token::SuffixMatch => (b'$', Some(b'=')),
      Token::SubstringMatch => (b'*', Some(b'=')),
      Token::CDO => (b'<', Some(b'!')),
      Token::CDC => (b'-', Some(b'-')),
      // Fallback: this token kind isn't important for our splice-boundary heuristic.
      _ => (b'?', None),
    };

    let prev = *out
      .as_bytes()
      .last()
      .expect("non-empty string must have last byte");
    if needs_token_splice_separator(prev, first, second) {
      out.push_str(TOKEN_SPLICE_SEPARATOR);
    }
  }

  match token {
    Token::WhiteSpace(ws) => out.push_str(ws.as_ref()),
    Token::Comment(text) => {
      out.push_str("/*");
      out.push_str(text.as_ref());
      out.push_str("*/");
    }
    // `cssparser`'s `to_css_string()` escapes quotes inside strings/URLs to guarantee the output is
    // valid CSS. Our property-value parser, however, consumes the resolved string without
    // interpreting CSS string escapes (it expects the raw token contents). This mismatch can turn
    // `"` into `\"` inside `data:` URLs (e.g. SVG XML), breaking downstream consumers like `usvg`.
    //
    // Prefer emitting quoted strings with a quote character that does not appear in the content so
    // we can preserve the raw value without adding backslash escapes.
    Token::QuotedString(text) => {
      let raw = text.as_ref();
      if !raw.contains('\'') {
        out.push('\'');
        out.push_str(raw);
        out.push('\'');
      } else if !raw.contains('"') {
        out.push('"');
        out.push_str(raw);
        out.push('"');
      } else {
        token
          .to_css(out)
          .expect("writing to String should be infallible");
      }
    }
    other => other
      .to_css(out)
      .expect("writing to String should be infallible"),
  }
}

fn token_to_css_string(token: &Token) -> String {
  match token {
    Token::WhiteSpace(ws) => ws.to_string(),
    Token::Comment(text) => format!("/*{}*/", text),
    Token::QuotedString(text) => {
      let raw = text.as_ref();
      if !raw.contains('\'') {
        format!("'{raw}'")
      } else if !raw.contains('"') {
        format!("\"{raw}\"")
      } else {
        token.to_css_string()
      }
    }
    _ => token.to_css_string(),
  }
}

/// Checks if a string contains any var() references (case-insensitive)
pub fn contains_var(value: &str) -> bool {
  // `parse_known_property_value` calls this for *every* declaration value while parsing CSS.
  // Tokenizing each value with `cssparser` is very expensive for large stylesheets, so use a
  // cheap substring-based detector with a rare correctness slow-path.
  //
  // Function tokens cannot contain whitespace between the name and `(`, so the literal `var(`
  // is sufficient for the fast path (with ASCII-case-insensitive matching).
  let bytes = value.as_bytes();
  if bytes.len() < 4 {
    return false;
  }

  #[inline]
  fn is_ident_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_') || b >= 0x80
  }

  let mut in_string: Option<u8> = None;
  let mut in_comment = false;
  let mut has_backslash = false;
  let mut has_open_paren = false;

  let mut i = 0usize;
  while i < bytes.len() {
    let byte = bytes[i];

    if in_comment {
      if byte == b'*' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
        in_comment = false;
        i += 2;
        continue;
      }
      i += 1;
      continue;
    }

    if let Some(quote) = in_string {
      if byte == b'\\' {
        // Skip the escaped byte so `\"` doesn't terminate the string.
        i = (i + 2).min(bytes.len());
        continue;
      }
      if byte == quote {
        in_string = None;
      }
      i += 1;
      continue;
    }

    // Not inside a string/comment.
    if byte == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'*' {
      in_comment = true;
      i += 2;
      continue;
    }

    if byte == b'"' || byte == b'\'' {
      in_string = Some(byte);
      i += 1;
      continue;
    }

    if byte == b'\\' {
      has_backslash = true;
    } else if byte == b'(' {
      has_open_paren = true;
    }

    if i + 3 < bytes.len()
      && byte.to_ascii_lowercase() == b'v'
      && bytes[i + 1].to_ascii_lowercase() == b'a'
      && bytes[i + 2].to_ascii_lowercase() == b'r'
      && bytes[i + 3] == b'('
    {
      // `var(` must be the full function name, so ensure the match is not preceded by an
      // identifier character (e.g. `somevar(` should not match).
      let prev = i.checked_sub(1).and_then(|idx| bytes.get(idx).copied());
      if prev.map_or(true, |b| !is_ident_byte(b)) {
        return true;
      }
    }

    i += 1;
  }

  // Escaped function names (e.g. `v\61 r(`) require a proper tokenizer to interpret escapes.
  //
  // A function token also requires a literal `(` delimiter, so if the raw string contains
  // backslashes but *no* `(` at all, it's impossible for it to contain a `var()` call.
  if has_backslash && has_open_paren {
    return contains_var_via_cssparser(value);
  }

  false
}

pub(crate) fn contains_var_via_cssparser(value: &str) -> bool {
  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);
  contains_var_in_parser(&mut parser)
}

fn contains_var_in_parser<'i, 't>(parser: &mut Parser<'i, 't>) -> bool {
  let mut found = false;

  while let Ok(token) = parser.next_including_whitespace_and_comments() {
    match token {
      Token::Function(name) if name.eq_ignore_ascii_case("var") => {
        found = true;
        let _ = parser
          .parse_nested_block(|nested| Ok::<_, ParseError<'i, ()>>(contains_var_in_parser(nested)));
      }
      Token::Function(_)
      | Token::ParenthesisBlock
      | Token::SquareBracketBlock
      | Token::CurlyBracketBlock => {
        if let Ok(nested_found) = parser
          .parse_nested_block(|nested| Ok::<_, ParseError<'i, ()>>(contains_var_in_parser(nested)))
        {
          if nested_found {
            found = true;
          }
        }
      }
      _ => {}
    }
  }

  found
}

/// Extracts all custom property names referenced in a value
pub fn extract_var_references(value: &str) -> Vec<String> {
  let mut refs = Vec::new();
  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);
  collect_var_references_from_parser(&mut parser, &mut refs);
  refs
}

fn collect_var_references_from_parser<'i, 't>(parser: &mut Parser<'i, 't>, refs: &mut Vec<String>) {
  while let Ok(token) = parser.next_including_whitespace_and_comments() {
    match token {
      Token::Function(name) if name.eq_ignore_ascii_case("var") => {
        let _ = parser.parse_nested_block(|nested| {
          if let Ok((name, fallback)) = parse_var_function_arguments(nested) {
            refs.push(name);
            if let Some(fallback_value) = fallback {
              let mut input = ParserInput::new(&fallback_value);
              let mut nested_parser = Parser::new(&mut input);
              collect_var_references_from_parser(&mut nested_parser, refs);
            }
          }
          Ok::<_, ParseError<'i, ()>>(())
        });
      }
      Token::Function(_)
      | Token::ParenthesisBlock
      | Token::SquareBracketBlock
      | Token::CurlyBracketBlock => {
        let _ = parser.parse_nested_block(|nested| {
          collect_var_references_from_parser(nested, refs);
          Ok::<_, ParseError<'i, ()>>(())
        });
      }
      _ => {}
    }
  }
}

/// Validates that a custom property name follows CSS naming rules
pub fn is_valid_custom_property_name(name: &str) -> bool {
  if !name.starts_with("--") {
    return false;
  }

  if name.len() <= 2 {
    return false; // Just "--" is not valid
  }

  // The rest can be any character except whitespace
  // (CSS spec allows almost any character in custom property names)
  !name[2..].chars().any(char::is_whitespace)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::style::values::CustomPropertyValue;
  use crate::style::values::Length;
  use crate::style::values::LengthUnit;

  fn make_props(pairs: &[(&str, &str)]) -> CustomPropertyStore {
    let mut store = CustomPropertyStore::default();
    for (name, value) in pairs.iter().copied() {
      store.insert(name.into(), CustomPropertyValue::new(value, None));
    }
    store
  }

  // Basic var() resolution tests
  #[test]
  fn test_resolve_simple_var() {
    let props = make_props(&[("--color", "#ff0000")]);
    let value = PropertyValue::Keyword("var(--color)".to_string());
    let resolved = resolve_var(&value, &props);

    // Should resolve to a color keyword when property context is missing
    matches!(resolved, PropertyValue::Keyword(ref kw) if kw == "#ff0000");
  }

  #[test]
  fn test_resolve_var_with_length() {
    let props = make_props(&[("--size", "16px")]);
    let value = PropertyValue::Keyword("var(--size)".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Length(len) = resolved {
      assert_eq!(len.value, 16.0);
    } else {
      panic!("Expected Length, got {:?}", resolved);
    }
  }

  #[test]
  fn test_resolve_var_not_found() {
    let props = CustomPropertyStore::default();
    let value = PropertyValue::Keyword("var(--missing)".to_string());
    let resolved = resolve_var(&value, &props);

    // Should return the original var() call
    if let PropertyValue::Keyword(kw) = resolved {
      assert!(kw.contains("var(--missing)"));
    } else {
      panic!("Expected Keyword, got {:?}", resolved);
    }
  }

  // Fallback value tests
  #[test]
  fn test_resolve_var_with_fallback_not_needed() {
    let props = make_props(&[("--color", "blue")]);
    let value = PropertyValue::Keyword("var(--color, red)".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Keyword(kw) = resolved {
      assert_eq!(kw, "blue");
    } else {
      panic!("Expected Keyword, got {:?}", resolved);
    }
  }

  #[test]
  fn test_resolve_var_with_fallback_used() {
    let props = CustomPropertyStore::default();
    let value = PropertyValue::Keyword("var(--missing, red)".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Keyword(kw) = resolved {
      assert_eq!(kw, "red");
    } else {
      panic!("Expected Keyword 'red', got {:?}", resolved);
    }
  }

  #[test]
  fn test_resolve_var_with_fallback_length() {
    let props = CustomPropertyStore::default();
    let value = PropertyValue::Keyword("var(--spacing, 10px)".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Length(len) = resolved {
      assert_eq!(len.value, 10.0);
    } else {
      panic!("Expected Length, got {:?}", resolved);
    }
  }

  // Nested var() tests
  #[test]
  fn test_resolve_nested_var_in_fallback() {
    let props = make_props(&[("--fallback-color", "green")]);
    let value = PropertyValue::Keyword("var(--color, var(--fallback-color))".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Keyword(kw) = resolved {
      assert_eq!(kw, "green");
    } else {
      panic!("Expected Keyword 'green', got {:?}", resolved);
    }
  }

  #[test]
  fn test_resolve_chained_vars() {
    let props = make_props(&[("--primary", "var(--base)"), ("--base", "#0000ff")]);
    let value = PropertyValue::Keyword("var(--primary)".to_string());
    let resolved = resolve_var(&value, &props);

    // Should resolve through the chain
    matches!(resolved, PropertyValue::Keyword(ref kw) if kw == "#0000ff");
  }

  // Embedded var() tests
  #[test]
  fn test_resolve_embedded_var_in_calc() {
    let props = make_props(&[("--size", "10px")]);
    let value = PropertyValue::Keyword("calc(var(--size) + 5px)".to_string());
    let resolved = resolve_var(&value, &props);

    assert!(
      matches!(resolved, PropertyValue::Length(len) if (len.value - 15.0).abs() < f32::EPSILON && len.unit == LengthUnit::Px),
      "Expected resolved calc length, got {:?}",
      resolved
    );
  }

  #[test]
  fn test_resolve_multiple_embedded_vars() {
    let props = make_props(&[("--x", "10px"), ("--y", "20px")]);
    let value = PropertyValue::Keyword("var(--x) var(--y)".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Keyword(kw) = resolved {
      assert!(kw.contains("10px"));
      assert!(kw.contains("20px"));
    } else {
      panic!("Expected Keyword, got {:?}", resolved);
    }
  }

  #[test]
  fn test_resolve_var_uses_property_specific_parser() {
    let props = make_props(&[("--bg", "url(image.png), linear-gradient(red, blue)")]);
    let value = PropertyValue::Keyword("var(--bg)".to_string());
    let resolved = resolve_var_for_property(&value, &props, "background-image");

    if let VarResolutionResult::Resolved { value, .. } = resolved {
      let list = match value.into_owned() {
        PropertyValue::Multiple(list) => list,
        other => panic!("Expected Multiple for background layers, got {:?}", other),
      };
      assert_eq!(list.len(), 3); // url, comma token, gradient
      assert!(matches!(list[0], PropertyValue::Url(ref u) if u == "image.png"));
      assert!(matches!(
        list[2],
        PropertyValue::LinearGradient { .. } | PropertyValue::RepeatingLinearGradient { .. }
      ));
    } else {
      panic!(
        "Expected Multiple for background layers, got {:?}",
        resolved
      );
    }
  }

  #[test]
  fn unresolved_var_marks_declaration_invalid() {
    let props = CustomPropertyStore::default();
    let value = PropertyValue::Keyword("var(--missing)".to_string());
    let resolved = resolve_var_for_property(&value, &props, "color");
    assert!(matches!(resolved, VarResolutionResult::NotFound(_)));
  }

  #[test]
  fn unresolved_fallback_var_marks_declaration_invalid() {
    let props = make_props(&[("--fallback", "var(--still-missing)")]);
    let value = PropertyValue::Keyword("var(--missing, var(--fallback))".to_string());
    let resolved = resolve_var_for_property(&value, &props, "color");
    assert!(matches!(resolved, VarResolutionResult::NotFound(_)));
  }

  #[test]
  fn resolved_width_parses_length_after_var_resolution() {
    let props = make_props(&[("--x", "10px")]);
    let value = PropertyValue::Keyword("var(--x)".to_string());
    let resolved = resolve_var_for_property(&value, &props, "width");

    let VarResolutionResult::Resolved { value, .. } = resolved else {
      panic!("expected successful var() resolution, got {resolved:?}");
    };

    match value.as_ref() {
      PropertyValue::Length(len) => {
        assert!((len.value - 10.0).abs() < f32::EPSILON);
        assert_eq!(len.unit, LengthUnit::Px);
      }
      other => panic!("expected Length(10px), got {other:?}"),
    }
  }

  #[test]
  fn parse_value_after_resolution_rejects_unresolved_var_function() {
    assert!(
      parse_value_after_resolution("var(--x)", "width").is_none(),
      "unresolved var() should invalidate the resolved value"
    );
  }

  #[test]
  fn parse_value_after_resolution_detects_escaped_var_function_name() {
    assert!(
      parse_value_after_resolution("v\\61 r(--x)", "width").is_none(),
      "escaped var() should invalidate the resolved value"
    );
  }

  // Recursion limit tests
  #[test]
  fn test_recursion_limit() {
    // Create a circular reference
    let props = make_props(&[
      ("--a", "var(--b)"),
      ("--b", "var(--c)"),
      ("--c", "var(--a)"), // Circular!
    ]);
    let value = PropertyValue::Keyword("var(--a)".to_string());

    // Should not stack overflow - recursion limit should kick in
    let _resolved = resolve_var(&value, &props);
    // If we get here without panicking, the test passes
  }

  // Utility function tests
  #[test]
  fn test_contains_var() {
    assert!(contains_var("var(--x)"));
    assert!(contains_var("calc(var(--x) + 1px)"));
    assert!(contains_var("var(--color)"));
    assert!(contains_var("calc(var(--size) + 10px)"));
    assert!(contains_var("0 0 var(--blur) black"));
    assert!(contains_var("v\\61 r(--x)"));
    assert!(
      contains_var("url(var(--x))"),
      "var() inside url() should be detected"
    );
    assert!(!contains_var("10px"));
    assert!(!contains_var("red"));
    assert!(!contains_var("color: red"));
    assert!(!contains_var(""));
  }

  #[test]
  fn contains_var_ignores_strings_and_comments() {
    assert!(
      !contains_var("\"var(--x)\""),
      "var() inside quoted strings is not a var() token"
    );
    assert!(
      !contains_var("/* var(--x) */"),
      "var() inside comments is not a var() token"
    );
    assert!(
      !contains_var("url(\"var(--x)\")"),
      "var() inside url()'s quoted string is not a var() token"
    );
  }

  #[test]
  fn test_extract_var_references() {
    let refs = extract_var_references("var(--color)");
    assert_eq!(refs, vec!["--color"]);

    let refs = extract_var_references("calc(var(--size) + var(--margin))");
    assert_eq!(refs, vec!["--size", "--margin"]);

    let refs = extract_var_references("var(--x, var(--y))");
    assert_eq!(refs, vec!["--x", "--y"]);

    let refs = extract_var_references("10px");
    assert!(refs.is_empty());
  }

  #[test]
  fn test_is_valid_custom_property_name() {
    assert!(is_valid_custom_property_name("--color"));
    assert!(is_valid_custom_property_name("--color-primary"));
    assert!(is_valid_custom_property_name("--_internal"));
    assert!(is_valid_custom_property_name("--123"));
    assert!(is_valid_custom_property_name("--myVar"));

    assert!(!is_valid_custom_property_name("color"));
    assert!(!is_valid_custom_property_name("-color"));
    assert!(!is_valid_custom_property_name("--"));
    assert!(!is_valid_custom_property_name("--has space"));
  }

  // Edge cases
  #[test]
  fn test_empty_var() {
    let props = CustomPropertyStore::default();
    let value = PropertyValue::Keyword("var()".to_string());
    let resolved = resolve_var(&value, &props);

    // Should return the original malformed var()
    if let PropertyValue::Keyword(kw) = resolved {
      assert_eq!(kw, "var()");
    }
  }

  #[test]
  fn test_var_with_whitespace() {
    let props = make_props(&[("--color", "blue")]);
    let value = PropertyValue::Keyword("var(  --color  )".to_string());
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Keyword(kw) = resolved {
      assert_eq!(kw, "blue");
    } else {
      panic!("Expected Keyword 'blue', got {:?}", resolved);
    }
  }

  #[test]
  fn test_non_var_value_unchanged() {
    let props = make_props(&[("--color", "blue")]);
    let value = PropertyValue::Length(Length::px(10.0));
    let resolved = resolve_var(&value, &props);

    if let PropertyValue::Length(len) = resolved {
      assert_eq!(len.value, 10.0);
    } else {
      panic!("Expected Length, got {:?}", resolved);
    }
  }

  #[test]
  fn test_keyword_without_var_skips_tokenization() {
    let props = CustomPropertyStore::default();
    let value = PropertyValue::Keyword("block".to_string());

    TOKEN_RESOLVER_ENTRY_COUNT.with(|count| count.set(0));
    let resolved = resolve_var_for_property(&value, &props, "display");

    match resolved {
      VarResolutionResult::Resolved { value, css_text } => {
        assert!(css_text.is_empty());
        assert!(
          matches!(value, ResolvedPropertyValue::Borrowed(_)),
          "expected var-free resolution to borrow the original PropertyValue"
        );
        assert!(matches!(
          value.as_ref(),
          PropertyValue::Keyword(ref kw) if kw == "block"
        ));
      }
      other => panic!("Expected Resolved, got {:?}", other),
    }

    assert_eq!(TOKEN_RESOLVER_ENTRY_COUNT.with(|count| count.get()), 0);
  }

  #[test]
  fn test_simple_var_call_skips_tokenization() {
    let props = make_props(&[("--x", "10px")]);
    let value = PropertyValue::Keyword("var(--x)".to_string());

    TOKEN_RESOLVER_ENTRY_COUNT.with(|count| count.set(0));
    let resolved = resolve_var_for_property(&value, &props, "width");

    let VarResolutionResult::Resolved { value, css_text } = resolved else {
      panic!("expected var() resolution to succeed, got {resolved:?}");
    };
    assert_eq!(css_text.as_ref(), "10px");
    assert!(matches!(
      value.as_ref(),
      PropertyValue::Length(len) if (len.value - 10.0).abs() < f32::EPSILON && len.unit == LengthUnit::Px
    ));

    assert_eq!(TOKEN_RESOLVER_ENTRY_COUNT.with(|count| count.get()), 0);
  }

  #[test]
  fn test_simple_var_call_with_fallback_skips_tokenization() {
    let props = CustomPropertyStore::default();
    let value = PropertyValue::Keyword("var(--missing, 10px)".to_string());

    TOKEN_RESOLVER_ENTRY_COUNT.with(|count| count.set(0));
    let resolved = resolve_var_for_property(&value, &props, "width");

    let VarResolutionResult::Resolved { value, css_text } = resolved else {
      panic!("expected fallback var() resolution to succeed, got {resolved:?}");
    };
    assert_eq!(css_text.trim(), "10px");
    assert!(matches!(
      value.as_ref(),
      PropertyValue::Length(len) if (len.value - 10.0).abs() < f32::EPSILON && len.unit == LengthUnit::Px
    ));

    assert_eq!(TOKEN_RESOLVER_ENTRY_COUNT.with(|count| count.get()), 0);
  }

  #[test]
  fn test_simple_var_call_with_empty_fallback_resolves_to_empty() {
    let props = CustomPropertyStore::default();
    let value = PropertyValue::Keyword("var(--missing,)".to_string());

    TOKEN_RESOLVER_ENTRY_COUNT.with(|count| count.set(0));
    let resolved = resolve_var_for_property(&value, &props, "");

    let VarResolutionResult::Resolved { css_text, .. } = resolved else {
      panic!("expected empty-fallback var() resolution to succeed, got {resolved:?}");
    };

    assert_eq!(css_text.as_ref(), "");
    assert_eq!(TOKEN_RESOLVER_ENTRY_COUNT.with(|count| count.get()), 0);
  }

  #[test]
  fn token_splice_preserves_boundaries_between_adjacent_vars() {
    let props = make_props(&[("--x", "0"), ("--y", "calc(1px)")]);
    let value = PropertyValue::Keyword("var(--x)var(--y)".to_string());

    let VarResolutionResult::Resolved { css_text, .. } = resolve_var_for_property(&value, &props, "")
    else {
      panic!("expected var() resolution to succeed");
    };

    let mut input = ParserInput::new(css_text.as_ref());
    let mut parser = Parser::new(&mut input);

    let mut seen_number = false;
    let mut seen_calc = false;
    while let Ok(token) = parser.next_including_whitespace_and_comments() {
      match token {
        Token::WhiteSpace(_) | Token::Comment(_) => continue,
        Token::Number { int_value, .. } if !seen_number => {
          assert_eq!(int_value, &Some(0));
          seen_number = true;
        }
        Token::Function(name) if seen_number && !seen_calc => {
          assert!(
            name.eq_ignore_ascii_case("calc"),
            "expected `calc()` function token after number, got {name:?}"
          );
          seen_calc = true;
          break;
        }
        other => panic!("unexpected token after var() splice: {other:?}"),
      }
    }

    assert!(seen_number, "expected a number token at start of spliced result");
    assert!(seen_calc, "expected a calc() function token after number");
  }

  #[test]
  fn test_multi_var_calls_skip_tokenization() {
    let props = make_props(&[("--x", "10px"), ("--y", "20px")]);
    let value = PropertyValue::Keyword("translate(var(--x), var(--y))".to_string());

    TOKEN_RESOLVER_ENTRY_COUNT.with(|count| count.set(0));
    let resolved = resolve_var_for_property(&value, &props, "");

    let VarResolutionResult::Resolved { value, css_text } = resolved else {
      panic!("expected multi-var() resolution to succeed, got {resolved:?}");
    };
    assert_eq!(css_text.as_ref(), "translate(10px, 20px)");
    assert!(matches!(
      value.as_ref(),
      PropertyValue::Keyword(ref kw) if kw == "translate(10px, 20px)"
    ));

    assert_eq!(TOKEN_RESOLVER_ENTRY_COUNT.with(|count| count.get()), 0);
  }

  #[test]
  fn test_chained_simple_var_calls_skip_tokenization() {
    let props = make_props(&[("--a", "var(--b)"), ("--b", "10px")]);
    let value = PropertyValue::Keyword("var(--a)".to_string());

    TOKEN_RESOLVER_ENTRY_COUNT.with(|count| count.set(0));
    let resolved = resolve_var_for_property(&value, &props, "width");

    let VarResolutionResult::Resolved { value, css_text } = resolved else {
      panic!("expected chained var() resolution to succeed, got {resolved:?}");
    };
    assert_eq!(css_text.as_ref(), "10px");
    assert!(matches!(
      value.as_ref(),
      PropertyValue::Length(len)
        if (len.value - 10.0).abs() < f32::EPSILON && len.unit == LengthUnit::Px
    ));

    assert_eq!(TOKEN_RESOLVER_ENTRY_COUNT.with(|count| count.get()), 0);
  }

  #[test]
  fn test_resolve_var_result_methods() {
    let resolved: VarResolutionResult<'static> = VarResolutionResult::Resolved {
      value: ResolvedPropertyValue::Owned(PropertyValue::Keyword("blue".to_string())),
      css_text: Cow::Borrowed("blue"),
    };
    assert!(resolved.is_resolved());

    let default = PropertyValue::Keyword("red".to_string());
    let result = VarResolutionResult::NotFound("--missing".to_string());
    let value = result.unwrap_or(default.clone());
    if let PropertyValue::Keyword(kw) = value {
      assert_eq!(kw, "red");
    }
  }
}
