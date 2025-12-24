//! CSS parsing
//!
//! Parses CSS stylesheets and declarations.
//!
//! The parser supports error recovery - when it encounters invalid CSS,
//! it attempts to skip to the next valid rule and continue parsing.
//! Errors are collected and returned alongside the parsed stylesheet.

use super::properties::parse_property_value;
use super::selectors::FastRenderSelectorImpl;
use super::selectors::PseudoClassParser;
use super::types::ContainerRule;
use super::types::CssParseError;
use super::types::CssParseResult;
use super::types::CssRule;
use super::types::Declaration;
use super::types::FontDisplay;
use super::types::FontFaceRule;
use super::types::FontFaceSource;
use super::types::FontFaceStyle;
use super::types::FontFaceUrlSource;
use super::types::FontSourceFormat;
use super::types::ImportRule;
use super::types::Keyframe;
use super::types::KeyframesRule;
use super::types::LayerRule;
use super::types::MediaRule;
use super::types::PageMarginArea;
use super::types::PageMarginRule;
use super::types::PagePseudoClass;
use super::types::PageRule;
use super::types::PageSelector;
use super::types::ScopeRule;
use super::types::StyleRule;
use super::types::StyleSheet;
use super::types::SupportsCondition;
use super::types::SupportsRule;
use crate::dom::DomNode;
use crate::error::Result;
use crate::style::counter_styles::{CounterStyleRule, CounterSystem, SpeakAs};
use crate::style::media::MediaQuery;
use cssparser::ParseError;
use cssparser::Parser;
use cssparser::ParserInput;
use cssparser::ToCss;
use cssparser::Token;
use selectors::parser::SelectorList;
use selectors::parser::SelectorParseErrorKind;

// ============================================================================
// Main parsing functions
// ============================================================================

/// Parse a CSS stylesheet
///
/// This parser handles @media rules, building a tree of CssRule that can be
/// evaluated against a MediaContext during cascade.
///
/// Errors are silently ignored for backward compatibility. Use
/// [`parse_stylesheet_with_errors`] to capture parse errors.
pub fn parse_stylesheet(css: &str) -> Result<StyleSheet> {
  let result = parse_stylesheet_collecting_errors(css);
  Ok(result.stylesheet)
}

/// Parse a CSS stylesheet and collect any parse errors
///
/// This parser attempts error recovery, so even if errors are present,
/// the returned stylesheet may contain valid rules.
///
/// # Example
///
/// ```rust,ignore
/// use fastrender::css::parser::parse_stylesheet_with_errors;
///
/// let css = "p { color: red; } .invalid {{ } span { color: blue; }";
/// let result = parse_stylesheet_with_errors(css);
///
/// println!("Found {} errors", result.error_count());
/// for error in &result.errors {
///     eprintln!("{}", error);
/// }
///
/// // The stylesheet still contains the valid rules
/// assert!(result.stylesheet.rules.len() > 0);
/// ```
pub fn parse_stylesheet_with_errors(css: &str) -> CssParseResult {
  parse_stylesheet_collecting_errors(css)
}

/// Internal function that does the actual parsing with error collection
fn parse_stylesheet_collecting_errors(css: &str) -> CssParseResult {
  let mut input = ParserInput::new(css);
  let mut parser = Parser::new(&mut input);
  let mut errors = Vec::new();

  let rules = parse_rule_list_collecting(&mut parser, &mut errors, None);

  CssParseResult::with_errors(StyleSheet { rules }, errors)
}

/// Parse a list of CSS rules, collecting errors
fn parse_rule_list_collecting<'i, 't>(
  parser: &mut Parser<'i, 't>,
  errors: &mut Vec<CssParseError>,
  parent_selectors: Option<&SelectorList<FastRenderSelectorImpl>>,
) -> Vec<CssRule> {
  let mut rules = Vec::new();

  while !parser.is_exhausted() {
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }

    match parse_rule(parser, parent_selectors) {
      Ok(Some(rule)) => {
        rules.push(rule);
      }
      Ok(None) => {} // Comment or skipped at-rule
      Err(e) => {
        // Collect the error
        let location = e.location;
        let message = format!("{:?}", e.kind);
        errors.push(CssParseError::new(message, location.line, location.column));

        // Try to recover by skipping to next rule
        recover_from_error(parser);
      }
    }
  }

  rules
}

/// Parse a list of CSS rules (internal use - discards errors)
///
/// Used for parsing nested rule lists inside @media and @layer blocks
fn parse_rule_list<'i, 't>(parser: &mut Parser<'i, 't>) -> Vec<CssRule> {
  let mut errors = Vec::new();
  parse_rule_list_collecting(parser, &mut errors, None)
  // Errors from nested rules are discarded here
  // In the future, we could pass down the error collector
}

fn parse_rule_list_with_context<'i, 't>(
  parser: &mut Parser<'i, 't>,
  parent_selectors: Option<&SelectorList<FastRenderSelectorImpl>>,
) -> Vec<CssRule> {
  let mut errors = Vec::new();
  parse_rule_list_collecting(parser, &mut errors, parent_selectors)
}

/// Recover from a parse error by skipping to the next rule
fn recover_from_error<'i, 't>(parser: &mut Parser<'i, 't>) {
  while !parser.is_exhausted() {
    match parser.next() {
      Ok(Token::CurlyBracketBlock) => {
        let _: std::result::Result<(), ParseError<()>> = parser.parse_nested_block(|_| Ok(()));
        break;
      }
      Ok(Token::ParenthesisBlock) | Ok(Token::SquareBracketBlock) => {
        let _: std::result::Result<(), ParseError<()>> = parser.parse_nested_block(|_| Ok(()));
      }
      _ => {}
    }
  }
}

/// Parse a single CSS rule (style rule or @-rule)
fn parse_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
  parent_selectors: Option<&SelectorList<FastRenderSelectorImpl>>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  parser.skip_whitespace();

  // Try to parse as @-rule first
  // We need to use try_parse and do the full parsing inside
  // because try_parse only restores position on ERROR
  if let Ok(result) = parser.try_parse(|p| {
    if let Ok(Token::AtKeyword(kw)) = p.next() {
      let kw_str = kw.to_string();
      match kw_str.as_str() {
        "import" => parse_import_rule(p),
        "media" => parse_media_rule(p, parent_selectors),
        "container" => parse_container_rule(p, parent_selectors),
        "scope" => parse_scope_rule(p, parent_selectors),
        "supports" => parse_supports_rule(p, parent_selectors),
        "layer" => parse_layer_rule(p, parent_selectors),
        "page" => parse_page_rule(p),
        "counter-style" => parse_counter_style_rule(p),
        "font-face" => parse_font_face_rule(p),
        "keyframes" | "-webkit-keyframes" => parse_keyframes_rule(p),
        _ => {
          skip_at_rule(p);
          Ok(None)
        }
      }
    } else {
      // Not an at-rule, return error to restore position
      Err(p.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
        "not at-rule".into(),
      )))
    }
  }) {
    return Ok(result);
  }

  // Parse style rule
  parse_style_rule(parser, parent_selectors).map(|opt| opt.map(CssRule::Style))
}

/// Parse an @import rule
fn parse_import_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  parser.skip_whitespace();

  // Import target can be url() or string
  let href = match parser.next_including_whitespace() {
    Ok(Token::Function(f)) if f.as_ref().eq_ignore_ascii_case("url") => {
      parser.parse_nested_block(|p| {
        p.skip_whitespace();
        match p.next_including_whitespace() {
          Ok(Token::QuotedString(s)) | Ok(Token::Ident(s)) => Ok(s.to_string()),
          _ => Err(p.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
            "expected url".into(),
          ))),
        }
      })?
    }
    Ok(Token::QuotedString(s)) => s.to_string(),
    _ => return Ok(None),
  };

  // Parse optional media list until semicolon or block start (best-effort; unknown tokens are ignored).
  let mut media_queries = Vec::new();
  let mut media_tokens = String::new();
  while let Ok(token) = parser.next_including_whitespace() {
    match token {
      Token::Semicolon | Token::CurlyBracketBlock => break,
      Token::WhiteSpace(ws) => media_tokens.push_str(ws),
      other => media_tokens.push_str(&other.to_css_string()),
    }
  }
  if !media_tokens.trim().is_empty() {
    if let Ok(list) = MediaQuery::parse_list(&media_tokens) {
      media_queries = list;
    }
  }

  // Skip until semicolon
  // (already consumed above)

  Ok(Some(CssRule::Import(ImportRule {
    href,
    media: media_queries,
  })))
}

/// Parse a @media rule
fn parse_media_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
  parent_selectors: Option<&SelectorList<FastRenderSelectorImpl>>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  // Collect tokens as strings until we hit the {
  let mut query_parts = Vec::new();
  loop {
    match parser.next_including_whitespace() {
      Ok(Token::CurlyBracketBlock) => break,
      Ok(Token::WhiteSpace(ws)) => query_parts.push((*ws).to_string()),
      Ok(Token::Ident(id)) => query_parts.push(id.to_string()),
      Ok(Token::Number { value, .. }) => query_parts.push(value.to_string()),
      Ok(Token::Dimension { value, unit, .. }) => {
        query_parts.push(format!("{}{}", value, unit));
      }
      Ok(Token::Colon) => query_parts.push(":".to_string()),
      Ok(Token::ParenthesisBlock) => {
        // Parse contents of parenthesis block
        let inner = parser.parse_nested_block(|p| {
          let mut inner_parts = Vec::new();
          while !p.is_exhausted() {
            match p.next_including_whitespace() {
              Ok(Token::WhiteSpace(ws)) => inner_parts.push((*ws).to_string()),
              Ok(Token::Ident(id)) => inner_parts.push(id.to_string()),
              Ok(Token::Number { value, .. }) => inner_parts.push(value.to_string()),
              Ok(Token::Dimension { value, unit, .. }) => {
                inner_parts.push(format!("{}{}", value, unit));
              }
              Ok(Token::Colon) => inner_parts.push(":".to_string()),
              Ok(_) => {}
              Err(_) => break,
            }
          }
          Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(inner_parts.join(""))
        })?;
        query_parts.push(format!("({})", inner));
      }
      Ok(_) => {}
      Err(_) => break,
    }
  }
  let query_str = query_parts.join("");

  // Parse the media query
  let query = MediaQuery::parse(query_str.trim()).unwrap_or_else(|_| {
    // If parsing fails, create a query that always matches
    // This is for compatibility - we don't want to drop rules we can't parse
    MediaQuery::new()
  });

  // Parse the nested rules from the block we already matched
  let nested_rules = parser.parse_nested_block(|nested_parser| {
    Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(parse_rule_list_with_context(
      nested_parser,
      parent_selectors,
    ))
  })?;

  Ok(Some(CssRule::Media(MediaRule {
    query,
    rules: nested_rules,
  })))
}

/// Parse a @container rule (size queries only; name is optional and stored for future use).
fn parse_container_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
  parent_selectors: Option<&SelectorList<FastRenderSelectorImpl>>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  // Collect tokens forming the prelude (optional name + query) up to the block.
  let mut prelude_parts = Vec::new();
  loop {
    match parser.next_including_whitespace() {
      Ok(Token::CurlyBracketBlock) => break,
      Ok(Token::WhiteSpace(ws)) => prelude_parts.push((*ws).to_string()),
      Ok(Token::Ident(id)) => prelude_parts.push(id.to_string()),
      Ok(Token::Number { value, .. }) => prelude_parts.push(value.to_string()),
      Ok(Token::Dimension { value, unit, .. }) => prelude_parts.push(format!("{}{}", value, unit)),
      Ok(Token::Colon) => prelude_parts.push(":".to_string()),
      Ok(Token::Delim(c)) => prelude_parts.push(c.to_string()),
      Ok(Token::ParenthesisBlock) => {
        let inner = parser.parse_nested_block(|p| {
          let mut inner_parts = Vec::new();
          while !p.is_exhausted() {
            match p.next_including_whitespace() {
              Ok(Token::WhiteSpace(ws)) => inner_parts.push((*ws).to_string()),
              Ok(Token::Ident(id)) => inner_parts.push(id.to_string()),
              Ok(Token::Number { value, .. }) => inner_parts.push(value.to_string()),
              Ok(Token::Dimension { value, unit, .. }) => {
                inner_parts.push(format!("{}{}", value, unit))
              }
              Ok(Token::Colon) => inner_parts.push(":".to_string()),
              Ok(Token::Delim(c)) => inner_parts.push(c.to_string()),
              Ok(_) => {}
              Err(_) => break,
            }
          }
          Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(inner_parts.join(""))
        })?;
        prelude_parts.push(format!("({})", inner));
      }
      Ok(_) => {}
      Err(_) => break,
    }
  }

  let prelude = prelude_parts.join("");
  let prelude_trimmed = prelude.trim();
  if prelude_trimmed.is_empty() {
    skip_at_rule(parser);
    return Ok(None);
  }

  // Split optional name and query: text before the first '(' is treated as the name.
  let (name, query_str) = if let Some(idx) = prelude_trimmed.find('(') {
    let (before, after) = prelude_trimmed.split_at(idx);
    let name = before.trim();
    let name = if name.is_empty() {
      None
    } else {
      Some(name.to_string())
    };
    (name, after)
  } else {
    (None, prelude_trimmed)
  };

  let query = MediaQuery::parse(query_str).map_err(|_| {
    skip_at_rule(parser);
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
      "invalid container query".into(),
    ))
  })?;

  let nested_rules = parser.parse_nested_block(|nested_parser| {
    Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(parse_rule_list_with_context(
      nested_parser,
      parent_selectors,
    ))
  })?;

  if !query.is_size_query() {
    // Container queries level 1 accept only size features; drop invalid container rules.
    return Ok(None);
  }

  Ok(Some(CssRule::Container(ContainerRule {
    name,
    query,
    rules: nested_rules,
  })))
}

/// Parse an @scope rule with optional scope root/limit selectors.
fn parse_scope_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
  parent_selectors: Option<&SelectorList<FastRenderSelectorImpl>>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  fn parse_selector_list<'i, 't>(
    parser: &mut Parser<'i, 't>,
  ) -> std::result::Result<
    SelectorList<FastRenderSelectorImpl>,
    ParseError<'i, SelectorParseErrorKind<'i>>,
  > {
    parser.skip_whitespace();
    match parser.next_including_whitespace()? {
      Token::ParenthesisBlock => parser.parse_nested_block(|nested| {
        Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(SelectorList::parse(
          &PseudoClassParser,
          nested,
          selectors::parser::ParseRelative::ForScope,
        )?)
      }),
      _ => Err(
        parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
          "expected scope selector list in parentheses".into(),
        )),
      ),
    }
  }

  let parsed: std::result::Result<_, ParseError<'i, SelectorParseErrorKind<'i>>> = parser
    .parse_until_before(cssparser::Delimiter::CurlyBracketBlock, |p| {
      p.skip_whitespace();
      if p.is_exhausted() {
        return Ok((None, None));
      }

      if p.try_parse(|p2| p2.expect_ident_matching("to")).is_ok() {
        let end = Some(parse_selector_list(p)?);
        p.skip_whitespace();
        return Ok((None, end));
      }

      let start = Some(parse_selector_list(p)?);

      p.skip_whitespace();
      let end = if p.try_parse(|p2| p2.expect_ident_matching("to")).is_ok() {
        Some(parse_selector_list(p)?)
      } else {
        None
      };

      p.skip_whitespace();
      Ok((start, end))
    });

  let (start, end) = match parsed {
    Ok(value) => value,
    Err(_) => {
      skip_at_rule(parser);
      return Ok(None);
    }
  };

  parser.expect_curly_bracket_block().map_err(|_| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("expected {".into()))
  })?;

  let nested_rules = parser.parse_nested_block(|nested_parser| {
    Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(parse_rule_list_with_context(
      nested_parser,
      parent_selectors,
    ))
  })?;

  Ok(Some(CssRule::Scope(ScopeRule {
    start,
    end,
    rules: nested_rules,
  })))
}

/// Parse a @supports rule
fn parse_supports_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
  parent_selectors: Option<&SelectorList<FastRenderSelectorImpl>>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let start = parser.position();
  parser.parse_until_before(cssparser::Delimiter::CurlyBracketBlock, |parser| {
    while !parser.is_exhausted() {
      let _ = parser.next_including_whitespace()?;
    }
    Ok(())
  })?;

  let prelude = parser.slice_from(start);

  parser.expect_curly_bracket_block().map_err(|_| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("expected {".into()))
  })?;

  let condition = parse_supports_prelude(prelude);
  let rules = parser.parse_nested_block(|nested| {
    Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(parse_rule_list_with_context(
      nested,
      parent_selectors,
    ))
  })?;
  Ok(Some(CssRule::Supports(SupportsRule { condition, rules })))
}

fn parse_supports_prelude(prelude: &str) -> SupportsCondition {
  let mut input = ParserInput::new(prelude);
  let mut parser = Parser::new(&mut input);
  match parse_supports_condition(&mut parser) {
    Ok(cond) => {
      parser.skip_whitespace();
      if parser.is_exhausted() {
        cond
      } else {
        SupportsCondition::False
      }
    }
    Err(_) => SupportsCondition::False,
  }
}

fn parse_supports_condition<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  parse_supports_disjunction(parser)
}

fn parse_supports_bare_condition<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  parse_supports_bare_disjunction(parser)
}

fn parse_supports_disjunction<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  let mut conditions = vec![parse_supports_conjunction(parser)?];

  loop {
    parser.skip_whitespace();
    if parser.try_parse(|p| p.expect_ident_matching("or")).is_ok() {
      parser.skip_whitespace();
      conditions.push(parse_supports_conjunction(parser)?);
    } else if parser
      .try_parse(|p| p.expect_function_matching("or"))
      .is_ok()
    {
      let term = parser.parse_nested_block(parse_supports_parenthesized_contents)?;
      conditions.push(parse_supports_conjunction_tail(parser, term)?);
    } else {
      break;
    }
  }

  if conditions.len() == 1 {
    Ok(conditions.pop().unwrap())
  } else {
    Ok(SupportsCondition::Or(conditions))
  }
}

fn parse_supports_conjunction<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  let mut conditions = vec![parse_supports_negation(parser)?];

  loop {
    parser.skip_whitespace();
    if parser.try_parse(|p| p.expect_ident_matching("and")).is_ok() {
      parser.skip_whitespace();
      conditions.push(parse_supports_negation(parser)?);
    } else if parser
      .try_parse(|p| p.expect_function_matching("and"))
      .is_ok()
    {
      let term = parser.parse_nested_block(parse_supports_parenthesized_contents)?;
      conditions.push(term);
    } else {
      break;
    }
  }

  if conditions.len() == 1 {
    Ok(conditions.pop().unwrap())
  } else {
    Ok(SupportsCondition::And(conditions))
  }
}

fn parse_supports_negation<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  parser.skip_whitespace();
  if parser.try_parse(|p| p.expect_ident_matching("not")).is_ok() {
    parser.skip_whitespace();
    let inner = parse_supports_condition_in_parens(parser)?;
    return Ok(SupportsCondition::Not(Box::new(inner)));
  }
  if parser
    .try_parse(|p| p.expect_function_matching("not"))
    .is_ok()
  {
    let inner = parser.parse_nested_block(parse_supports_parenthesized_contents)?;
    return Ok(SupportsCondition::Not(Box::new(inner)));
  }

  parse_supports_condition_in_parens(parser)
}

fn parse_supports_condition_in_parens<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  parser.skip_whitespace();

  if let Ok(selector) = parser.try_parse(|p| parse_supports_selector_function(p)) {
    return Ok(selector);
  }

  parser
    .try_parse(|p| parse_supports_parenthesized_condition(p))
    .map_err(|_| parser.new_custom_error(()))
}

fn parse_supports_selector_function<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  if let Ok(selector) = parser.try_parse(|p| {
    p.expect_function_matching("selector")?;
    parse_supports_selector_arguments(p)
  }) {
    return Ok(selector);
  }

  parser.try_parse(|p| {
    let ident = p.expect_ident()?;
    if !ident.as_ref().eq_ignore_ascii_case("selector") {
      return Err(p.new_custom_error(()));
    }
    p.skip_whitespace();
    match p.next_including_whitespace()? {
      Token::ParenthesisBlock => parse_supports_selector_arguments(p),
      _ => Err(p.new_custom_error(())),
    }
  })
}

fn parse_supports_selector_arguments<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  let selector_list = parser.parse_nested_block(|nested| {
    let start = nested.position();
    consume_nested_tokens(nested)?;
    Ok::<_, ParseError<'i, ()>>(nested.slice_from(start).trim().to_string())
  })?;

  if selector_list.is_empty() {
    return Err(parser.new_custom_error(()));
  }

  Ok(SupportsCondition::Selector(selector_list))
}

fn consume_nested_tokens<'i, 't, E>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<(), ParseError<'i, E>> {
  while !parser.is_exhausted() {
    let token = parser.next_including_whitespace()?;
    match token {
      Token::CurlyBracketBlock
      | Token::ParenthesisBlock
      | Token::SquareBracketBlock
      | Token::Function(_) => {
        parser.parse_nested_block(consume_nested_tokens)?;
      }
      _ => {}
    }
  }
  Ok(())
}

fn parse_supports_parenthesized_condition<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  match parser.next_including_whitespace() {
    Ok(Token::ParenthesisBlock) => parser.parse_nested_block(parse_supports_parenthesized_contents),
    _ => Err(parser.new_custom_error(())),
  }
}

fn parse_supports_parenthesized_contents<'i, 't>(
  nested: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  let state = nested.state();
  if let Ok(cond) = nested.try_parse(|p| parse_supports_condition(p)) {
    nested.skip_whitespace();
    if nested.is_exhausted() {
      return Ok(cond);
    }
  }
  nested.reset(&state);

  if let Ok(cond) = nested.try_parse(|p| parse_supports_bare_condition(p)) {
    nested.skip_whitespace();
    if nested.is_exhausted() {
      return Ok(cond);
    }
  }
  nested.reset(&state);

  if let Some(decl) = parse_supports_declaration_in_parens(nested) {
    nested.skip_whitespace();
    if nested.is_exhausted() {
      return Ok(decl);
    }
  }
  Ok(SupportsCondition::False)
}

fn parse_supports_conjunction_tail<'i, 't>(
  parser: &mut Parser<'i, 't>,
  first: SupportsCondition,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  let mut conditions = vec![first];

  loop {
    parser.skip_whitespace();
    if parser.try_parse(|p| p.expect_ident_matching("and")).is_ok() {
      parser.skip_whitespace();
      conditions.push(parse_supports_negation(parser)?);
    } else if parser
      .try_parse(|p| p.expect_function_matching("and"))
      .is_ok()
    {
      let term = parser.parse_nested_block(parse_supports_parenthesized_contents)?;
      conditions.push(term);
    } else {
      break;
    }
  }

  if conditions.len() == 1 {
    Ok(conditions.pop().unwrap())
  } else {
    Ok(SupportsCondition::And(conditions))
  }
}

fn parse_supports_bare_disjunction<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  let mut conditions = vec![parse_supports_bare_conjunction(parser)?];

  loop {
    parser.skip_whitespace();
    if parser.try_parse(|p| p.expect_ident_matching("or")).is_ok() {
      parser.skip_whitespace();
      conditions.push(parse_supports_bare_conjunction(parser)?);
    } else {
      break;
    }
  }

  if conditions.len() == 1 {
    Ok(conditions.pop().unwrap())
  } else {
    Ok(SupportsCondition::Or(conditions))
  }
}

fn parse_supports_bare_conjunction<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  let mut conditions = vec![parse_supports_bare_negation(parser)?];

  loop {
    parser.skip_whitespace();
    if parser.try_parse(|p| p.expect_ident_matching("and")).is_ok() {
      parser.skip_whitespace();
      conditions.push(parse_supports_bare_negation(parser)?);
    } else {
      break;
    }
  }

  if conditions.len() == 1 {
    Ok(conditions.pop().unwrap())
  } else {
    Ok(SupportsCondition::And(conditions))
  }
}

fn parse_supports_bare_negation<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  parser.skip_whitespace();
  if parser.try_parse(|p| p.expect_ident_matching("not")).is_ok() {
    parser.skip_whitespace();
    let inner = parse_supports_bare_term(parser)?;
    return Ok(SupportsCondition::Not(Box::new(inner)));
  }

  parse_supports_bare_term(parser)
}

fn parse_supports_bare_term<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SupportsCondition, ParseError<'i, ()>> {
  parser.skip_whitespace();

  if let Ok(selector) = parser.try_parse(|p| parse_supports_selector_function(p)) {
    return Ok(selector);
  }

  parse_supports_bare_declaration(parser).ok_or_else(|| parser.new_custom_error(()))
}

fn parse_supports_bare_declaration<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> Option<SupportsCondition> {
  parser.skip_whitespace();
  let property = parser.expect_ident().ok()?.to_string();
  parser.skip_whitespace();
  if parser.expect_colon().is_err() {
    return None;
  }

  parser.skip_whitespace();
  let value_start = parser.position();

  loop {
    parser.skip_whitespace();
    let state = parser.state();

    let operator = match parser.next() {
      Ok(Token::Ident(kw)) if kw.eq_ignore_ascii_case("and") || kw.eq_ignore_ascii_case("or") => {
        Some(kw.to_ascii_lowercase())
      }
      _ => None,
    };

    if operator.is_some() {
      parser.skip_whitespace();
      let boundary = parser.expect_ident().is_ok() && parser.expect_colon().is_ok();
      parser.reset(&state);
      if boundary {
        break;
      }
    } else {
      parser.reset(&state);
    }

    if parser.next_including_whitespace().is_err() {
      break;
    }
  }

  let value = parser.slice_from(value_start).trim();
  if value.is_empty() && !property.starts_with("--") {
    return None;
  }

  Some(SupportsCondition::Declaration {
    property: property.to_ascii_lowercase(),
    value: value.to_string(),
  })
}

fn parse_supports_declaration_in_parens<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> Option<SupportsCondition> {
  parser.skip_whitespace();
  let property = parser.expect_ident().ok()?.to_string();
  parser.skip_whitespace();
  if parser.expect_colon().is_err() {
    return None;
  }

  parser.skip_whitespace();
  let value_start = parser.position();
  while parser.next_including_whitespace().is_ok() {}
  let value = parser.slice_from(value_start).trim();

  if value.is_empty() && !property.starts_with("--") {
    return None;
  }

  Some(SupportsCondition::Declaration {
    property: property.to_ascii_lowercase(),
    value: value.to_string(),
  })
}

fn parse_layer_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
  parent_selectors: Option<&SelectorList<FastRenderSelectorImpl>>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let mut names: Vec<Vec<String>> = Vec::new();
  let mut current: Vec<String> = Vec::new();
  let mut saw_block = false;
  let mut anonymous = false;

  loop {
    match parser.next_including_whitespace() {
      Ok(Token::Ident(id)) => current.push(id.to_string()),
      Ok(Token::Delim('.')) => {
        // part of dotted layer name; keep collecting
      }
      Ok(Token::Comma) => {
        if !current.is_empty() {
          names.push(current.clone());
          current.clear();
        }
      }
      Ok(Token::CurlyBracketBlock) => {
        saw_block = true;
        break;
      }
      Ok(Token::Semicolon) => break,
      Ok(Token::WhiteSpace(_)) => {}
      Ok(_) => {}
      Err(_) => break,
    }
  }

  if !current.is_empty() {
    names.push(current);
  }

  let rules = if saw_block {
    parser.parse_nested_block(|nested| {
      Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(parse_rule_list_with_context(
        nested,
        parent_selectors,
      ))
    })?
  } else {
    Vec::new()
  };

  if names.is_empty() && rules.is_empty() {
    return Ok(None);
  }

  if names.is_empty() {
    anonymous = true;
  }

  Ok(Some(CssRule::Layer(LayerRule {
    names,
    rules,
    anonymous,
  })))
}

fn parse_page_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let selectors = parse_page_selectors(parser)?;

  parser.expect_curly_bracket_block().map_err(|_| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("expected {".into()))
  })?;

  let (declarations, margin_rules) =
    parser.parse_nested_block(|nested| parse_page_block(nested))?;

  Ok(Some(CssRule::Page(PageRule {
    selectors,
    declarations,
    margin_rules,
  })))
}

fn parse_page_selectors<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Vec<PageSelector>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let selectors = parser.parse_until_before(cssparser::Delimiter::CurlyBracketBlock, |parser| {
    let mut selectors = Vec::new();
    loop {
      parser.skip_whitespace();
      if parser.is_exhausted() {
        break;
      }

      let mut name = None;
      if let Ok(ident) = parser.try_parse(|p| p.expect_ident().map(|i| i.to_string())) {
        name = Some(ident);
      }

      let mut pseudo = None;
      if parser.try_parse(|p| p.expect_colon()).is_ok() {
        if let Ok(pseudo_ident) = parser.expect_ident() {
          pseudo = match pseudo_ident.as_ref().to_ascii_lowercase().as_str() {
            "first" => Some(PagePseudoClass::First),
            "left" => Some(PagePseudoClass::Left),
            "right" => Some(PagePseudoClass::Right),
            "blank" => Some(PagePseudoClass::Blank),
            _ => None,
          };
        }
      }

      if name.is_some() || pseudo.is_some() {
        selectors.push(PageSelector { name, pseudo });
      }

      parser.skip_whitespace();
      if parser.try_parse(|p| p.expect_comma()).is_ok() {
        continue;
      }
      if parser.is_exhausted() {
        break;
      }
    }

    if selectors.is_empty() {
      selectors.push(PageSelector {
        name: None,
        pseudo: None,
      });
    }

    Ok(selectors)
  })?;

  Ok(selectors)
}

fn parse_page_block<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<
  (Vec<Declaration>, Vec<PageMarginRule>),
  ParseError<'i, SelectorParseErrorKind<'i>>,
> {
  let mut declarations = Vec::new();
  let mut margin_rules = Vec::new();

  while !parser.is_exhausted() {
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }

    let state = parser.state();
    match parser.next_including_whitespace() {
      Ok(Token::AtKeyword(kw)) => {
        if let Some(area) = parse_margin_area(&kw.to_string()) {
          parser.expect_curly_bracket_block().map_err(|_| {
            parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("expected {".into()))
          })?;
          let nested = parser.parse_nested_block(|nested| {
            parse_declaration_list(nested).map_err(|_| {
              nested.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
                "declaration".into(),
              ))
            })
          })?;
          margin_rules.push(PageMarginRule {
            area,
            declarations: nested,
          });
          continue;
        }
        skip_at_rule(parser);
      }
      Ok(_) | Err(_) => {
        parser.reset(&state);
        if let Some(decl) = parse_declaration(parser) {
          declarations.push(decl);
        }
      }
    }
  }

  Ok((declarations, margin_rules))
}

fn parse_margin_area(name: &str) -> Option<PageMarginArea> {
  match name.to_ascii_lowercase().as_str() {
    "top-left" | "top-left-corner" => Some(PageMarginArea::TopLeft),
    "top-center" => Some(PageMarginArea::TopCenter),
    "top-right" | "top-right-corner" => Some(PageMarginArea::TopRight),
    "bottom-left" | "bottom-left-corner" => Some(PageMarginArea::BottomLeft),
    "bottom-center" => Some(PageMarginArea::BottomCenter),
    "bottom-right" | "bottom-right-corner" => Some(PageMarginArea::BottomRight),
    _ => None,
  }
}

fn parse_font_face_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  parser.expect_curly_bracket_block().map_err(|_| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("expected {".into()))
  })?;

  let face = parser.parse_nested_block(|parser| parse_font_face_descriptors(parser))?;
  Ok(face.map(CssRule::FontFace))
}

fn parse_font_face_descriptors<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Option<FontFaceRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let mut face = FontFaceRule::default();

  while !parser.is_exhausted() {
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }

    let property = match parser.expect_ident() {
      Ok(id) => id.to_string(),
      Err(_) => {
        skip_to_semicolon(parser);
        continue;
      }
    };

    if parser.expect_colon().is_err() {
      skip_to_semicolon(parser);
      continue;
    }

    let value_start = parser.position();
    let mut important = false;
    loop {
      match parser.next() {
        Ok(Token::Semicolon) | Err(_) => break,
        Ok(Token::Delim('!')) => {
          if parser
            .try_parse(|p| p.expect_ident_matching("important"))
            .is_ok()
          {
            important = true;
            skip_to_semicolon(parser);
            break;
          }
        }
        _ => {}
      }
    }

    let full_value = parser.slice_from(value_start);
    let trimmed_value = if important {
      full_value
        .rsplit_once("!important")
        .map(|(before, _)| before.trim_end_matches(';').trim())
        .unwrap_or_else(|| full_value.trim_end_matches(';').trim())
    } else {
      full_value.trim_end_matches(';').trim()
    };

    let prop = property.to_ascii_lowercase();
    match prop.as_str() {
      "font-family" => {
        face.family = parse_font_face_family(trimmed_value);
      }
      "src" => {
        let parsed = parse_font_face_src(trimmed_value);
        if !parsed.is_empty() {
          face.sources = parsed;
        }
      }
      "font-style" => {
        if let Some(style) = parse_font_face_style(trimmed_value) {
          face.style = style;
        }
      }
      "font-weight" => {
        if let Some(range) = parse_font_face_weight(trimmed_value) {
          face.weight = range;
        }
      }
      "font-stretch" => {
        if let Some(range) = parse_font_face_stretch(trimmed_value) {
          face.stretch = range;
        }
      }
      "font-display" => {
        if let Some(display) = parse_font_display(trimmed_value) {
          face.display = display;
        }
      }
      "unicode-range" => {
        let ranges = parse_unicode_range_list(trimmed_value);
        if !ranges.is_empty() {
          face.unicode_ranges = ranges;
        }
      }
      _ => {}
    }
  }

  if face.family.is_none() || face.sources.is_empty() {
    return Ok(None);
  }

  Ok(Some(face))
}

fn parse_keyframes_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  parser.skip_whitespace();
  let name = match parser.next_including_whitespace() {
    Ok(Token::Ident(id)) => id.to_string(),
    Ok(Token::QuotedString(s)) => s.to_string(),
    _ => return Ok(None),
  };

  parser.expect_curly_bracket_block().map_err(|_| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("expected {".into()))
  })?;

  let frames = parser.parse_nested_block(|nested| parse_keyframe_list(nested))?;
  Ok(Some(CssRule::Keyframes(KeyframesRule {
    name,
    keyframes: frames,
  })))
}

fn parse_counter_style_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  parser.skip_whitespace();

  let name = match parser.next_including_whitespace() {
    Ok(Token::Ident(id)) => id.to_string(),
    Ok(Token::QuotedString(s)) => s.to_string(),
    _ => {
      skip_at_rule(parser);
      return Ok(None);
    }
  };

  parser.expect_curly_bracket_block().map_err(|_| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("expected {".into()))
  })?;

  let rule = parser.parse_nested_block(|parser| parse_counter_style_descriptors(parser, &name))?;
  Ok(rule.map(CssRule::CounterStyle))
}

fn parse_counter_style_descriptors<'i, 't>(
  parser: &mut Parser<'i, 't>,
  name: &str,
) -> std::result::Result<Option<CounterStyleRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let mut rule = CounterStyleRule::new(name);

  while !parser.is_exhausted() {
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }

    let location = parser.current_source_location();
    let ident = match parser.next_including_whitespace() {
      Ok(Token::Ident(id)) => id.to_ascii_lowercase(),
      _ => {
        return Err(
          parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
            "expected ident".into(),
          )),
        )
      }
    };

    match ident.as_str() {
      "system" => {
        rule.system = Some(parse_counter_style_system(parser)?);
      }
      "symbols" => {
        rule.symbols = Some(parse_counter_style_symbols(parser)?);
      }
      "additive-symbols" => {
        rule.additive_symbols = Some(parse_counter_style_additive_symbols(parser)?);
      }
      "negative" => {
        rule.negative = Some(parse_counter_style_negative(parser)?);
      }
      "prefix" => {
        rule.prefix = Some(parse_counter_style_component(parser)?);
      }
      "suffix" => {
        rule.suffix = Some(parse_counter_style_component(parser)?);
      }
      "range" => {
        rule.range = Some(parse_counter_style_range(parser)?);
      }
      "pad" => {
        rule.pad = Some(parse_counter_style_pad(parser)?);
      }
      "fallback" => {
        rule.fallback = Some(parse_counter_style_fallback(parser)?);
      }
      "speak-as" => {
        rule.speak_as = Some(parse_counter_style_speak_as(parser)?);
      }
      _ => {
        return Err(
          location.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(ident.into())),
        )
      }
    }

    parser.skip_whitespace();
  }

  Ok(Some(rule))
}

fn parse_counter_style_descriptor_value<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<String, ParseError<'i, SelectorParseErrorKind<'i>>> {
  if parser.expect_colon().is_err() {
    skip_to_semicolon(parser);
    return Err(
      parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
        "expected ':'".into(),
      )),
    );
  }

  let value_start = parser.position();
  let mut important = false;
  loop {
    match parser.next() {
      Ok(Token::Semicolon) | Err(_) => break,
      Ok(Token::Delim('!')) => {
        if parser
          .try_parse(|p| p.expect_ident_matching("important"))
          .is_ok()
        {
          important = true;
          skip_to_semicolon(parser);
          break;
        }
      }
      _ => {}
    }
  }

  let full_value = parser.slice_from(value_start);
  let trimmed_value = if important {
    full_value
      .rsplit_once("!important")
      .map(|(before, _)| before.trim_end_matches(';').trim())
      .unwrap_or_else(|| full_value.trim_end_matches(';').trim())
  } else {
    full_value.trim_end_matches(';').trim()
  };
  Ok(trimmed_value.to_string())
}

fn parse_counter_style_system<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<CounterSystem, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let value = parse_counter_style_descriptor_value(parser)?;
  parse_counter_system(&value).ok_or_else(|| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
      "invalid system".into(),
    ))
  })
}

fn parse_counter_style_symbols<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Vec<String>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let value = parse_counter_style_descriptor_value(parser)?;
  parse_symbols_descriptor(&value).ok_or_else(|| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
      "invalid symbols".into(),
    ))
  })
}

fn parse_counter_style_additive_symbols<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Vec<(i32, String)>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let value = parse_counter_style_descriptor_value(parser)?;
  parse_additive_symbols_descriptor(&value).ok_or_else(|| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
      "invalid additive-symbols".into(),
    ))
  })
}

fn parse_counter_style_negative<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<(String, String), ParseError<'i, SelectorParseErrorKind<'i>>> {
  let value = parse_counter_style_descriptor_value(parser)?;
  parse_negative_descriptor(&value).ok_or_else(|| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
      "invalid negative".into(),
    ))
  })
}

fn parse_counter_style_component<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<String, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let value = parse_counter_style_descriptor_value(parser)?;
  parse_symbols_descriptor(&value)
    .and_then(|mut list| list.drain(..).next())
    .ok_or_else(|| {
      parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
        "invalid component".into(),
      ))
    })
}

fn parse_counter_style_range<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Vec<(i64, i64)>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let value = parse_counter_style_descriptor_value(parser)?;
  Ok(parse_range_descriptor(&value).unwrap_or_default())
}

fn parse_counter_style_pad<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<(u32, String), ParseError<'i, SelectorParseErrorKind<'i>>> {
  let value = parse_counter_style_descriptor_value(parser)?;
  parse_pad_descriptor(&value).ok_or_else(|| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
      "invalid pad".into(),
    ))
  })
}

fn parse_counter_style_fallback<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<String, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let value = parse_counter_style_descriptor_value(parser)?;
  let trimmed = value.trim();
  if trimmed.is_empty() {
    return Err(
      parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
        "expected ident".into(),
      )),
    );
  }
  Ok(trimmed.to_string())
}

fn parse_counter_style_speak_as<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<SpeakAs, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let value = parse_counter_style_descriptor_value(parser)?;
  parse_speak_as_descriptor(&value).ok_or_else(|| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
      "invalid speak-as".into(),
    ))
  })
}

fn parse_counter_system<'i>(value: &'i str) -> Option<CounterSystem> {
  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);
  let ident = parser.expect_ident().ok()?.to_string().to_ascii_lowercase();
  match ident.as_str() {
    "cyclic" => Some(CounterSystem::Cyclic),
    "numeric" => Some(CounterSystem::Numeric),
    "alphabetic" => Some(CounterSystem::Alphabetic),
    "symbolic" => Some(CounterSystem::Symbolic),
    "additive" => Some(CounterSystem::Additive),
    "fixed" => {
      let start = parser
        .try_parse(|p| {
          p.skip_whitespace();
          match p.next_including_whitespace() {
            Ok(Token::Number {
              int_value: Some(v), ..
            }) => Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(Some(*v as i32)),
            Ok(Token::Dimension {
              int_value: Some(v), ..
            }) => Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(Some(*v as i32)),
            Ok(Token::Ident(v)) => {
              Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(v.parse::<i32>().ok())
            }
            _ => Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(None),
          }
        })
        .ok()
        .flatten()
        .unwrap_or(1);
      Some(CounterSystem::Fixed(start))
    }
    "extends" => {
      parser.skip_whitespace();
      let name = parser.expect_ident().ok()?.to_string();
      Some(CounterSystem::Extends(name.to_ascii_lowercase()))
    }
    _ => None,
  }
}

fn token_to_symbol(token: &Token) -> Option<String> {
  match token {
    Token::QuotedString(s) => Some(s.to_string()),
    Token::Ident(s) => Some(s.to_string()),
    Token::Number { value, .. } => Some(value.to_string()),
    Token::Dimension { value, unit, .. } => Some(format!("{}{}", value, unit)),
    _ => None,
  }
}

fn parse_symbols_descriptor(value: &str) -> Option<Vec<String>> {
  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);
  let mut symbols = Vec::new();
  while !parser.is_exhausted() {
    parser.skip_whitespace();
    if let Ok(token) = parser.next_including_whitespace() {
      if matches!(token, Token::Comma | Token::Semicolon | Token::Delim('/')) {
        continue;
      }
      if let Some(sym) = token_to_symbol(&token) {
        symbols.push(sym);
      }
    }
  }
  if symbols.is_empty() {
    None
  } else {
    Some(symbols)
  }
}

fn parse_additive_symbols_descriptor(value: &str) -> Option<Vec<(i32, String)>> {
  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);
  let mut items = Vec::new();

  while !parser.is_exhausted() {
    parser.skip_whitespace();
    let weight = match parser.next_including_whitespace() {
      Ok(Token::Number {
        int_value: Some(v), ..
      }) => *v as i32,
      Ok(Token::Ident(v)) => v.parse::<i32>().ok().unwrap_or(0),
      Ok(Token::Dimension {
        int_value: Some(v), ..
      }) => *v as i32,
      Ok(Token::Comma) => continue,
      _ => break,
    };

    parser.skip_whitespace();
    let symbol_token = match parser.next_including_whitespace() {
      Ok(t) => t,
      Err(_) => break,
    };
    if let Some(sym) = token_to_symbol(&symbol_token) {
      items.push((weight, sym));
    }
  }

  if items.is_empty() {
    None
  } else {
    Some(items)
  }
}

fn parse_negative_descriptor(value: &str) -> Option<(String, String)> {
  let symbols = parse_symbols_descriptor(value)?;
  if symbols.is_empty() {
    return None;
  }
  let first = symbols.get(0).cloned().unwrap_or_default();
  let second = symbols.get(1).cloned().unwrap_or_default();
  Some((first, second))
}

fn parse_range_descriptor(value: &str) -> Option<Vec<(i64, i64)>> {
  if value.trim().eq_ignore_ascii_case("auto") {
    return None;
  }
  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);
  let mut numbers: Vec<i64> = Vec::new();
  while !parser.is_exhausted() {
    parser.skip_whitespace();
    match parser.next_including_whitespace() {
      Ok(Token::Number {
        int_value: Some(v), ..
      }) => numbers.push(*v as i64),
      Ok(Token::Ident(ident)) => match ident.to_ascii_lowercase().as_str() {
        "infinite" => numbers.push(i64::MAX),
        "-infinite" => numbers.push(i64::MIN),
        other => {
          if let Ok(parsed) = other.parse::<i64>() {
            numbers.push(parsed)
          }
        }
      },
      Ok(Token::Comma) => continue,
      _ => break,
    }
  }

  if numbers.is_empty() {
    return None;
  }

  let mut ranges = Vec::new();
  let mut iter = numbers.into_iter();
  while let Some(start) = iter.next() {
    let end = iter.next().unwrap_or(start);
    let (a, b) = if start <= end {
      (start, end)
    } else {
      (end, start)
    };
    ranges.push((a, b));
  }
  Some(ranges)
}

fn parse_pad_descriptor(value: &str) -> Option<(u32, String)> {
  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);
  parser.skip_whitespace();
  let width = match parser.next_including_whitespace() {
    Ok(Token::Number {
      int_value: Some(v), ..
    }) if *v > 0 => *v as u32,
    Ok(Token::Ident(v)) => v.parse::<u32>().ok()?,
    _ => return None,
  };
  parser.skip_whitespace();
  let symbol = match parser.next_including_whitespace() {
    Ok(tok) => token_to_symbol(&tok)?,
    Err(_) => return None,
  };
  Some((width, symbol))
}

fn parse_speak_as_descriptor(value: &str) -> Option<SpeakAs> {
  let trimmed = value.trim().to_ascii_lowercase();
  match trimmed.as_str() {
    "auto" => Some(SpeakAs::Auto),
    "bullets" => Some(SpeakAs::Bullets),
    "numbers" => Some(SpeakAs::Numbers),
    "words" => Some(SpeakAs::Words),
    "spell-out" => Some(SpeakAs::SpellOut),
    other if other.is_empty() => None,
    other => Some(SpeakAs::Other(other.to_string())),
  }
}

fn parse_keyframe_list<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Vec<Keyframe>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let mut frames = Vec::new();
  while !parser.is_exhausted() {
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }

    match parse_single_keyframe(parser)? {
      Some(mut list) => frames.append(&mut list),
      None => break,
    }
  }

  frames.sort_by(|a, b| {
    a.offset
      .partial_cmp(&b.offset)
      .unwrap_or(std::cmp::Ordering::Equal)
  });
  Ok(frames)
}

fn parse_single_keyframe<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Option<Vec<Keyframe>>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let mut offsets: Vec<f32> = Vec::new();

  loop {
    match parser.next_including_whitespace() {
      Ok(Token::Percentage { unit_value, .. }) => offsets.push(unit_value.clamp(0.0, 1.0)),
      Ok(Token::Ident(id)) => match id.to_ascii_lowercase().as_str() {
        "from" => offsets.push(0.0),
        "to" => offsets.push(1.0),
        _ => {}
      },
      Ok(Token::Comma) => {}
      Ok(Token::CurlyBracketBlock) => {
        if offsets.is_empty() {
          return Ok(None);
        }
        let declarations = parser.parse_nested_block(|nested| {
          parse_declaration_list(nested).map_err(|_| {
            nested.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
              "declaration".into(),
            ))
          })
        })?;
        let mut frames = Vec::new();
        for offset in offsets {
          frames.push(Keyframe {
            offset,
            declarations: declarations.clone(),
          });
        }
        return Ok(Some(frames));
      }
      Ok(_) => {}
      Err(_) => return Ok(None),
    }
  }
}

fn parse_font_face_family(value: &str) -> Option<String> {
  let trimmed = value.trim();
  if trimmed.is_empty() {
    return None;
  }
  let unquoted = trimmed
    .strip_prefix('"')
    .and_then(|v| v.strip_suffix('"'))
    .or_else(|| {
      trimmed
        .strip_prefix('\'')
        .and_then(|v| v.strip_suffix('\''))
    })
    .unwrap_or(trimmed);
  if unquoted.is_empty() {
    None
  } else {
    Some(unquoted.to_string())
  }
}

fn parse_font_face_style(value: &str) -> Option<FontFaceStyle> {
  let tokens: Vec<&str> = value.split_whitespace().collect();
  if tokens.is_empty() {
    return None;
  }
  match tokens[0].to_ascii_lowercase().as_str() {
    "normal" => Some(FontFaceStyle::Normal),
    "italic" => Some(FontFaceStyle::Italic),
    "oblique" => {
      let angles: Vec<f32> = tokens[1..]
        .iter()
        .filter_map(|t| parse_angle_token(t))
        .collect();
      let range = match angles.as_slice() {
        [a, b] => Some((a.min(*b), a.max(*b))),
        [a] => Some((*a, *a)),
        _ => None,
      };
      Some(FontFaceStyle::Oblique { range })
    }
    _ => None,
  }
}

fn parse_font_face_weight(value: &str) -> Option<(u16, u16)> {
  let weights: Vec<u16> = value
    .split_whitespace()
    .filter_map(|token| parse_weight_token(token))
    .collect();
  match weights.as_slice() {
    [w] => Some((*w, *w)),
    [a, b] => {
      let (a, b) = (*a, *b);
      Some((a.min(b), a.max(b)))
    }
    _ => None,
  }
}

fn parse_font_face_stretch(value: &str) -> Option<(f32, f32)> {
  let stretches: Vec<f32> = value
    .split_whitespace()
    .filter_map(|token| parse_stretch_token(token))
    .collect();
  match stretches.as_slice() {
    [s] => Some((*s, *s)),
    [a, b] => {
      let (a, b) = (*a, *b);
      Some((a.min(b), a.max(b)))
    }
    _ => None,
  }
}

fn parse_font_display(value: &str) -> Option<FontDisplay> {
  match value.trim().to_ascii_lowercase().as_str() {
    "auto" => Some(FontDisplay::Auto),
    "block" => Some(FontDisplay::Block),
    "swap" => Some(FontDisplay::Swap),
    "fallback" => Some(FontDisplay::Fallback),
    "optional" => Some(FontDisplay::Optional),
    _ => None,
  }
}

fn parse_unicode_range_list(value: &str) -> Vec<(u32, u32)> {
  value
    .split(',')
    .filter_map(|part| parse_unicode_range(part.trim()))
    .collect()
}

fn parse_unicode_range(part: &str) -> Option<(u32, u32)> {
  let part = part.trim();
  if !part.to_ascii_lowercase().starts_with("u+") {
    return None;
  }
  let body = &part[2..];
  if let Some((start, end)) = body.split_once('-') {
    let start = u32::from_str_radix(start.trim(), 16).ok()?;
    let end = u32::from_str_radix(end.trim(), 16).ok()?;
    if start <= end {
      return Some((start, end));
    }
    return None;
  }

  if body.contains('?') {
    let mut start = String::new();
    let mut end = String::new();
    for ch in body.chars() {
      match ch {
        '?' => {
          start.push('0');
          end.push('F');
        }
        _ => {
          start.push(ch);
          end.push(ch);
        }
      }
    }
    let s = u32::from_str_radix(&start, 16).ok()?;
    let e = u32::from_str_radix(&end, 16).ok()?;
    return Some((s, e));
  }

  let single = u32::from_str_radix(body.trim(), 16).ok()?;
  Some((single, single))
}

fn parse_font_face_src(value: &str) -> Vec<FontFaceSource> {
  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);
  let mut sources = Vec::new();

  while !parser.is_exhausted() {
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }

    let state = parser.state();
    if let Some(src) = parse_font_face_src_item(&mut parser) {
      sources.push(src);
    } else {
      parser.reset(&state);
      consume_until_comma(&mut parser);
    }
    let _ = parser.try_parse(|p| p.expect_comma());
  }

  sources
}

fn parse_font_face_src_item<'i, 't>(parser: &mut Parser<'i, 't>) -> Option<FontFaceSource> {
  if let Ok(url) = parser.try_parse(|p| p.expect_url()) {
    let mut src = FontFaceUrlSource::new(url.as_ref().to_string());
    src.format_hints = parse_format_hints(parser);
    return Some(FontFaceSource::Url(src));
  }

  match parser.next_including_whitespace() {
    Ok(Token::Function(f)) if f.as_ref().eq_ignore_ascii_case("url") => {
      if let Ok(url) = parser.parse_nested_block(|p| parse_string_or_ident(p)) {
        let mut src = FontFaceUrlSource::new(url);
        src.format_hints = parse_format_hints(parser);
        return Some(FontFaceSource::Url(src));
      }
    }
    Ok(Token::Function(f)) if f.as_ref().eq_ignore_ascii_case("local") => {
      if let Ok(name) = parser.parse_nested_block(|p| parse_string_or_ident(p)) {
        return Some(FontFaceSource::Local(name));
      }
    }
    Ok(Token::Comma) | Err(_) => return None,
    _ => {}
  }

  None
}

fn parse_format_hints<'i, 't>(parser: &mut Parser<'i, 't>) -> Vec<FontSourceFormat> {
  let mut hints = Vec::new();
  loop {
    let state = parser.state();
    parser.skip_whitespace();
    let Ok(token) = parser.next_including_whitespace() else {
      break;
    };
    match token {
      Token::Function(f) if f.as_ref().eq_ignore_ascii_case("format") => {
        if let Ok(mut parsed) = parser.parse_nested_block(|nested| parse_format_list(nested)) {
          hints.append(&mut parsed);
        }
      }
      _ => {
        parser.reset(&state);
        break;
      }
    }
  }
  hints
}

fn parse_format_list<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Vec<FontSourceFormat>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let mut hints = Vec::new();
  while !parser.is_exhausted() {
    parser.skip_whitespace();
    match parser.next_including_whitespace() {
      Ok(Token::QuotedString(s)) | Ok(Token::Ident(s)) => {
        hints.push(FontSourceFormat::from_hint(s.as_ref()));
        let _ = parser.try_parse(|p| p.expect_comma());
      }
      Err(_) => break,
      _ => break,
    }
  }
  Ok(hints)
}

fn consume_until_comma<'i, 't>(parser: &mut Parser<'i, 't>) {
  loop {
    match parser.next_including_whitespace() {
      Ok(Token::Comma) | Err(_) => break,
      _ => continue,
    }
  }
}

fn parse_string_or_ident<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<String, ParseError<'i, SelectorParseErrorKind<'i>>> {
  if let Ok(s) = parser.try_parse(|p| p.expect_string().map(|cow| cow.as_ref().to_string())) {
    return Ok(s);
  }

  parser
    .expect_ident()
    .map(|s| s.to_string())
    .map_err(ParseError::from)
}

fn parse_angle_token(token: &str) -> Option<f32> {
  let trimmed = token.trim();
  if trimmed.ends_with("deg") {
    trimmed[..trimmed.len() - 3].trim().parse::<f32>().ok()
  } else if trimmed.ends_with("rad") {
    trimmed[..trimmed.len() - 3]
      .trim()
      .parse::<f32>()
      .ok()
      .map(|r| r.to_degrees())
  } else if trimmed.ends_with("turn") {
    trimmed[..trimmed.len() - 4]
      .trim()
      .parse::<f32>()
      .ok()
      .map(|t| t * 360.0)
  } else if trimmed.ends_with("grad") {
    trimmed[..trimmed.len() - 4]
      .trim()
      .parse::<f32>()
      .ok()
      .map(|g| g * 0.9)
  } else {
    None
  }
}

fn parse_weight_token(token: &str) -> Option<u16> {
  let lower = token.to_ascii_lowercase();
  match lower.as_str() {
    "normal" => Some(400),
    "bold" => Some(700),
    _ => token.trim().parse::<u16>().ok().map(|w| w.clamp(1, 1000)),
  }
}

fn parse_stretch_token(token: &str) -> Option<f32> {
  let lower = token.to_ascii_lowercase();
  if lower.ends_with('%') {
    return lower[..lower.len() - 1].trim().parse::<f32>().ok();
  }
  match lower.as_str() {
    "ultra-condensed" => Some(50.0),
    "extra-condensed" => Some(62.5),
    "condensed" => Some(75.0),
    "semi-condensed" => Some(87.5),
    "normal" => Some(100.0),
    "semi-expanded" => Some(112.5),
    "expanded" => Some(125.0),
    "extra-expanded" => Some(150.0),
    "ultra-expanded" => Some(200.0),
    _ => None,
  }
}

fn split_selector_components(selector_text: &str) -> Vec<String> {
  let mut parts = Vec::new();
  let mut current = String::new();
  let mut paren_depth = 0usize;
  let mut bracket_depth = 0usize;
  let mut brace_depth = 0usize;
  let mut in_string: Option<char> = None;
  let mut chars = selector_text.chars().peekable();

  while let Some(ch) = chars.next() {
    if let Some(q) = in_string {
      current.push(ch);
      if ch == '\\' {
        if let Some(next) = chars.next() {
          current.push(next);
        }
      } else if ch == q {
        in_string = None;
      }
      continue;
    }

    match ch {
      '\\' => {
        current.push(ch);
        if let Some(next) = chars.next() {
          current.push(next);
        }
        continue;
      }
      '"' | '\'' => {
        in_string = Some(ch);
        current.push(ch);
        continue;
      }
      '(' => paren_depth += 1,
      ')' => paren_depth = paren_depth.saturating_sub(1),
      '[' => bracket_depth += 1,
      ']' => bracket_depth = bracket_depth.saturating_sub(1),
      '{' => brace_depth += 1,
      '}' => brace_depth = brace_depth.saturating_sub(1),
      ',' if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
        if !current.trim().is_empty() {
          parts.push(current.trim().to_string());
        }
        current.clear();
        continue;
      }
      _ => {}
    }

    current.push(ch);
  }

  if !current.trim().is_empty() {
    parts.push(current.trim().to_string());
  }

  parts
}

fn replace_nesting_selector(component: &str, parent: &str) -> (String, bool) {
  let mut out = String::new();
  let mut paren_depth = 0usize;
  let mut bracket_depth = 0usize;
  let mut brace_depth = 0usize;
  let mut in_string: Option<char> = None;
  let mut found = false;
  let mut chars = component.chars().peekable();

  while let Some(ch) = chars.next() {
    if let Some(q) = in_string {
      out.push(ch);
      if ch == '\\' {
        if let Some(next) = chars.next() {
          out.push(next);
        }
      } else if ch == q {
        in_string = None;
      }
      continue;
    }

    match ch {
      '\\' => {
        out.push(ch);
        if let Some(next) = chars.next() {
          out.push(next);
        }
        continue;
      }
      '"' | '\'' => {
        in_string = Some(ch);
        out.push(ch);
        continue;
      }
      '(' => paren_depth += 1,
      ')' => paren_depth = paren_depth.saturating_sub(1),
      '[' => {
        bracket_depth += 1;
      }
      ']' => bracket_depth = bracket_depth.saturating_sub(1),
      '{' => brace_depth += 1,
      '}' => brace_depth = brace_depth.saturating_sub(1),
      '&' if bracket_depth == 0 => {
        out.push_str(parent);
        found = true;
        continue;
      }
      _ => {}
    }

    out.push(ch);
  }

  (out, found)
}

fn combine_nested_selectors(
  parent_selectors: &SelectorList<FastRenderSelectorImpl>,
  nested_text: &str,
) -> Option<SelectorList<FastRenderSelectorImpl>> {
  let parent_strings: Vec<String> = parent_selectors
    .slice()
    .iter()
    .map(|sel| sel.to_css_string())
    .collect();
  if parent_strings.is_empty() {
    return None;
  }

  let components = split_selector_components(nested_text);
  if components.is_empty() {
    return None;
  }

  let mut combined_parts = Vec::new();
  for comp in components {
    if comp.is_empty() {
      continue;
    }

    let has_ampersand = replace_nesting_selector(&comp, "").1;
    if has_ampersand {
      for parent in &parent_strings {
        let (replaced, _) = replace_nesting_selector(&comp, parent);
        combined_parts.push(replaced.trim().to_string());
      }
    } else {
      for parent in &parent_strings {
        combined_parts.push(format!("{} {}", parent, comp.trim()));
      }
    }
  }

  if combined_parts.is_empty() {
    return None;
  }

  let combined = combined_parts.join(", ");
  let mut input = ParserInput::new(&combined);
  let mut parser = Parser::new(&mut input);
  SelectorList::parse(
    &PseudoClassParser,
    &mut parser,
    selectors::parser::ParseRelative::No,
  )
  .ok()
}

/// Skip an unknown @-rule
fn skip_at_rule<'i, 't>(parser: &mut Parser<'i, 't>) {
  loop {
    match parser.next() {
      Ok(Token::CurlyBracketBlock) => {
        let _: std::result::Result<(), ParseError<SelectorParseErrorKind>> =
          parser.parse_nested_block(|_| Ok(()));
        break;
      }
      Ok(Token::Semicolon) => break,
      Err(_) => break,
      Ok(_) => continue,
    }
  }
}

fn parse_nested_at_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
  parent_selectors: &SelectorList<FastRenderSelectorImpl>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  parser.skip_whitespace();
  let kw = match parser.next() {
    Ok(Token::AtKeyword(kw)) => kw,
    _ => {
      return Err(
        parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
          "not at-rule".into(),
        )),
      )
    }
  };
  let kw_lower = kw.to_ascii_lowercase();
  match kw_lower.as_str() {
    "media" => parse_media_rule(parser, Some(parent_selectors)),
    "supports" => parse_supports_rule(parser, Some(parent_selectors)),
    "container" => parse_container_rule(parser, Some(parent_selectors)),
    "layer" => parse_layer_rule(parser, Some(parent_selectors)),
    "nest" => parse_nest_rule(parser, parent_selectors),
    _ => {
      skip_at_rule(parser);
      Ok(None)
    }
  }
}

fn parse_style_block<'i, 't>(
  parser: &mut Parser<'i, 't>,
  parent_selectors: &SelectorList<FastRenderSelectorImpl>,
) -> std::result::Result<(Vec<Declaration>, Vec<CssRule>), ParseError<'i, SelectorParseErrorKind<'i>>>
{
  let mut declarations = Vec::new();
  let mut nested_rules = Vec::new();

  while !parser.is_exhausted() {
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }

    if let Ok(Some(rule)) = parser.try_parse(|p| parse_nested_at_rule(p, parent_selectors)) {
      nested_rules.push(rule);
      continue;
    }

    if let Ok(Some(rule)) = parser.try_parse(|p| parse_style_rule(p, Some(parent_selectors))) {
      nested_rules.push(CssRule::Style(rule));
      continue;
    }

    if let Some(decl) = parse_declaration(parser) {
      declarations.push(decl);
    }
  }

  Ok((declarations, nested_rules))
}

/// Parse a style rule (selectors + declarations)
fn parse_style_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
  parent_selectors: Option<&SelectorList<FastRenderSelectorImpl>>,
) -> std::result::Result<Option<StyleRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let selectors = if let Some(parent) = parent_selectors {
    let start = parser.position();
    parser.parse_until_before(
      cssparser::Delimiter::CurlyBracketBlock | cssparser::Delimiter::Semicolon,
      |parser| {
        while !parser.is_exhausted() {
          let _ = parser.next_including_whitespace()?;
        }
        Ok(())
      },
    )?;

    let selector_text = parser.slice_from(start).trim();
    if selector_text.is_empty() {
      return Err(
        parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
          "expected nested selector".into(),
        )),
      );
    }

    let state = parser.state();
    let next_token = parser.next_including_whitespace().map(|t| t.clone());
    parser.reset(&state);
    match next_token {
      Ok(Token::CurlyBracketBlock) => {}
      _ => {
        return Err(
          parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
            "expected nested rule block".into(),
          )),
        )
      }
    }

    let combined = combine_nested_selectors(parent, selector_text);
    if combined.is_none() {
      if parser.expect_curly_bracket_block().is_ok() {
        let _ = parser.parse_nested_block(|nested| {
          Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(parse_rule_list_with_context(
            nested,
            Some(parent),
          ))
        });
      }
      return Ok(None);
    }
    combined.unwrap()
  } else {
    parser.parse_until_before(cssparser::Delimiter::CurlyBracketBlock, |parser| {
      SelectorList::parse(
        &PseudoClassParser,
        parser,
        selectors::parser::ParseRelative::No,
      )
    })?
  };

  parser.expect_curly_bracket_block().map_err(|_| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("expected {".into()))
  })?;

  let (declarations, nested_rules) = parser.parse_nested_block(|parser| {
    parse_style_block(parser, &selectors).map_err(|_| {
      parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
        "declaration".into(),
      ))
    })
  })?;

  Ok(Some(StyleRule {
    selectors,
    declarations,
    nested_rules,
  }))
}

fn parse_nest_rule<'i, 't>(
  parser: &mut Parser<'i, 't>,
  parent_selectors: &SelectorList<FastRenderSelectorImpl>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let start = parser.position();
  parser.parse_until_before(cssparser::Delimiter::CurlyBracketBlock, |parser| {
    while !parser.is_exhausted() {
      let _ = parser.next_including_whitespace()?;
    }
    Ok(())
  })?;

  let selector_text = parser.slice_from(start).trim();
  if selector_text.is_empty() {
    skip_at_rule(parser);
    return Ok(None);
  }

  let Some(selectors) = combine_nested_selectors(parent_selectors, selector_text) else {
    if parser.expect_curly_bracket_block().is_ok() {
      let _ = parser.parse_nested_block(|nested| {
        Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(parse_rule_list_with_context(
          nested,
          Some(parent_selectors),
        ))
      });
    }
    return Ok(None);
  };

  parser.expect_curly_bracket_block().map_err(|_| {
    parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("expected {".into()))
  })?;

  let (declarations, nested_rules) = parser.parse_nested_block(|nested| {
    parse_style_block(nested, &selectors).map_err(|_| {
      nested.new_custom_error(SelectorParseErrorKind::UnexpectedIdent(
        "declaration".into(),
      ))
    })
  })?;

  Ok(Some(CssRule::Style(StyleRule {
    selectors,
    declarations,
    nested_rules,
  })))
}

fn parse_declaration<'i, 't>(parser: &mut Parser<'i, 't>) -> Option<Declaration> {
  let property = match parser.expect_ident() {
    Ok(ident) => ident.to_string(),
    Err(_) => {
      skip_to_semicolon(parser);
      return None;
    }
  };

  if parser.expect_colon().is_err() {
    skip_to_semicolon(parser);
    return None;
  }

  let value_start = parser.position();
  let mut important = false;

  loop {
    match parser.next() {
      Ok(Token::Semicolon) | Err(_) => break,
      Ok(Token::Delim('!')) => {
        if parser
          .try_parse(|p| p.expect_ident_matching("important"))
          .is_ok()
        {
          important = true;
        }
        break;
      }
      Ok(Token::Function(_)) => {
        let _ = parser.parse_nested_block(|p| {
          while !p.is_exhausted() {
            let _ = p.next();
          }
          Ok::<_, ParseError<()>>(())
        });
      }
      Ok(_) => {}
    }
  }

  let full_slice_raw = parser.slice_from(value_start);
  let value = if important {
    let without_important = if let Some((before, _)) = full_slice_raw.rsplit_once("!important") {
      before
    } else {
      full_slice_raw
    };
    without_important.trim_end_matches(';').trim_end()
  } else {
    full_slice_raw.trim_end_matches(';').trim_end()
  };

  parse_property_value(&property, value).map(|parsed_value| Declaration {
    property,
    value: parsed_value,
    raw_value: value.to_string(),
    important,
  })
}

/// Parse a list of declarations
fn parse_declaration_list<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<Vec<Declaration>, ParseError<'i, ()>> {
  let mut declarations = Vec::new();

  while !parser.is_exhausted() {
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }
    if let Some(decl) = parse_declaration(parser) {
      declarations.push(decl);
    }
  }

  Ok(declarations)
}

/// Skip tokens until we hit a semicolon
fn skip_to_semicolon<'i, 't>(parser: &mut Parser<'i, 't>) {
  while !parser.is_exhausted() {
    match parser.next() {
      Ok(Token::Semicolon) | Err(_) => break,
      _ => continue,
    }
  }
}

/// Parse declarations from an inline style attribute
pub fn parse_declarations(declarations_str: &str) -> Vec<Declaration> {
  let mut input = ParserInput::new(declarations_str);
  let mut parser = Parser::new(&mut input);
  parse_declaration_list(&mut parser).unwrap_or_default()
}

/// Inline `<style>` block extracted from the DOM.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InlineStyle {
  /// Raw CSS text inside the `<style>` element.
  pub css: String,
  /// Optional `media` attribute value.
  pub media: Option<String>,
  /// Optional `type` attribute value.
  pub type_attr: Option<String>,
  /// Whether the element is disabled via the boolean `disabled` attribute.
  pub disabled: bool,
}

/// External stylesheet reference extracted from a `<link>` element.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StylesheetLink {
  /// The href value, as-authored.
  pub href: String,
  /// Tokenized `rel` attribute, lowercased.
  pub rel: Vec<String>,
  /// Optional `media` attribute value.
  pub media: Option<String>,
  /// Optional `type` attribute value.
  pub type_attr: Option<String>,
  /// Whether the link is disabled via the boolean `disabled` attribute.
  pub disabled: bool,
}

/// Stylesheet sources (inline or external) in document order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StylesheetSource {
  /// Inline `<style>` element.
  Inline(InlineStyle),
  /// External `<link rel="stylesheet">` reference.
  External(StylesheetLink),
}

/// Tokenize a `rel` attribute into lowercase relationship values.
pub fn tokenize_rel_list(rel: &str) -> Vec<String> {
  rel
    .split_whitespace()
    .filter(|t| !t.is_empty())
    .map(|t| t.to_ascii_lowercase())
    .collect()
}

/// Returns true if the tokenized rel list contains the `stylesheet` keyword.
pub fn rel_list_contains_stylesheet(tokens: &[String]) -> bool {
  tokens
    .iter()
    .any(|token| token.eq_ignore_ascii_case("stylesheet"))
}

/// Extract inline `<style>` blocks and external `<link rel="stylesheet">` entries from a DOM.
///
/// The returned list preserves document order so callers can maintain the correct cascade
/// ordering when loading and parsing the resulting stylesheets.
pub fn extract_css_sources(dom: &DomNode) -> Vec<StylesheetSource> {
  let mut sources = Vec::new();

  dom.walk_tree(&mut |node| {
    if let Some(tag) = node.tag_name() {
      if tag.eq_ignore_ascii_case("style") {
        let mut css = String::new();
        for child in &node.children {
          if let Some(text) = child.text_content() {
            css.push_str(text);
          }
        }

        sources.push(StylesheetSource::Inline(InlineStyle {
          css,
          media: node.get_attribute("media"),
          type_attr: node.get_attribute("type"),
          disabled: node.get_attribute_ref("disabled").is_some(),
        }));
      } else if tag.eq_ignore_ascii_case("link") {
        let rel_attr = node.get_attribute("rel");
        let href_attr = node.get_attribute("href");
        if let (Some(rel), Some(href)) = (rel_attr, href_attr) {
          let rel = tokenize_rel_list(&rel);
          sources.push(StylesheetSource::External(StylesheetLink {
            href,
            rel,
            media: node.get_attribute("media"),
            type_attr: node.get_attribute("type"),
            disabled: node.get_attribute_ref("disabled").is_some(),
          }));
        }
      }
    }
  });

  sources
}

/// Extracts CSS from style tags in the DOM
///
/// Walks the DOM tree looking for `<style>` elements and concatenates
/// their text content to form a stylesheet.
pub fn extract_css(dom: &DomNode) -> Result<StyleSheet> {
  let mut css_content = String::new();

  dom.walk_tree(&mut |node| {
    if let Some(tag) = node.tag_name() {
      if tag == "style" {
        for child in &node.children {
          if let Some(text) = child.text_content() {
            css_content.push_str(text);
            css_content.push('\n');
          }
        }
      }
    }
  });

  if css_content.is_empty() {
    Ok(StyleSheet::new())
  } else {
    parse_stylesheet(&css_content)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::css::types::CssImportLoader;
  use crate::css::types::FontFaceStyle;
  use crate::css::types::FontSourceFormat;
  use crate::PropertyValue;

  #[test]
  fn debug_parse_rule_body_style() {
    let css = "body { color: red; }";
    let mut input = ParserInput::new(css);
    let mut parser = Parser::new(&mut input);
    assert!(!parser.is_exhausted(), "parser should see input tokens");
    let rule = parse_rule(&mut parser, None).expect("parse_rule");
    assert!(
      matches!(rule, Some(CssRule::Style(_))),
      "expected a style rule, got {:?}",
      rule
    );

    let mut input = ParserInput::new(css);
    let mut parser = Parser::new(&mut input);
    let mut errors = Vec::new();
    let rules = parse_rule_list_collecting(&mut parser, &mut errors, None);
    assert!(
      errors.is_empty(),
      "expected no parse errors, got {:?}",
      errors
    );
    assert_eq!(
      rules.len(),
      1,
      "expected rule list to include the style rule"
    );

    let sheet = parse_stylesheet(css).expect("parse_stylesheet");
    assert_eq!(
      sheet.rules.len(),
      1,
      "parse_stylesheet should return 1 rule"
    );
  }

  #[test]
  fn test_parse_simple_stylesheet() {
    let css = "body { color: red; }";
    let stylesheet = parse_stylesheet(css).unwrap();
    assert_eq!(stylesheet.rules.len(), 1);
    if let CssRule::Style(rule) = &stylesheet.rules[0] {
      assert_eq!(rule.declarations.len(), 1);
    } else {
      panic!("Expected style rule");
    }
  }

  #[test]
  fn test_parse_import_rule() {
    let css = r#"@import url("https://example.com/base.css") screen and (min-width: 800px);"#;
    let stylesheet = parse_stylesheet(css).unwrap();
    assert_eq!(stylesheet.rules.len(), 1);
    if let CssRule::Import(import) = &stylesheet.rules[0] {
      assert_eq!(import.href, "https://example.com/base.css");
    } else {
      panic!("Expected import rule");
    }
  }

  #[test]
  fn parses_font_face_rule() {
    let css = r#"@font-face {
            font-family: "TestFamily";
            src: url("fonts/test.woff2") format("woff2"), local(TestLocal);
            font-weight: 400 700;
            font-style: oblique 10deg 20deg;
            font-stretch: 75% 125%;
            unicode-range: U+0041-005A, U+0061-007A, U+1F600;
        }"#;
    let stylesheet = parse_stylesheet(css).unwrap();
    assert_eq!(stylesheet.rules.len(), 1);
    match &stylesheet.rules[0] {
      CssRule::FontFace(face) => {
        assert_eq!(face.family.as_deref(), Some("TestFamily"));
        assert_eq!(face.sources.len(), 2);
        match &face.sources[0] {
          FontFaceSource::Url(src) => {
            assert_eq!(src.url, "fonts/test.woff2");
            assert!(src
              .format_hints
              .iter()
              .any(|hint| matches!(hint, FontSourceFormat::Woff2)));
          }
          other => panic!("expected url source, got {:?}", other),
        }
        assert!(matches!(&face.sources[1], FontFaceSource::Local(name) if name == "TestLocal"));
        assert_eq!(face.weight, (400, 700));
        assert_eq!(face.stretch, (75.0, 125.0));
        assert!(matches!(face.style, FontFaceStyle::Oblique { .. }));
        assert_eq!(face.unicode_ranges.len(), 3);
        assert_eq!(face.unicode_ranges[0], (0x0041, 0x005a));
        assert_eq!(face.unicode_ranges[1], (0x0061, 0x007a));
        assert_eq!(face.unicode_ranges[2], (0x1f600, 0x1f600));
      }
      other => panic!("Unexpected rule: {:?}", other),
    }
  }

  #[test]
  fn resolve_imports_inlines_before_other_rules() {
    struct Loader;
    impl CssImportLoader for Loader {
      fn load(&self, url: &str) -> crate::error::Result<String> {
        assert_eq!(url, "https://example.com/base.css");
        Ok(".from-import { color: red; }".to_string())
      }
    }

    let css = r#"
            @import url("https://example.com/base.css");
            .local { color: blue; }
        "#;
    let sheet = parse_stylesheet(css).unwrap();
    let media_ctx = crate::style::media::MediaContext::screen(800.0, 600.0);
    let resolved = sheet.resolve_imports(&Loader, Some("https://example.com/page.css"), &media_ctx);
    assert_eq!(resolved.rules.len(), 2);
    assert!(matches!(resolved.rules[0], CssRule::Style(_)));
    assert!(matches!(resolved.rules[1], CssRule::Style(_)));
  }

  #[test]
  fn test_parse_media_query() {
    let css = "@media (min-width: 768px) { .foo { color: blue; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    assert_eq!(stylesheet.rules.len(), 1);
    if let CssRule::Media(media) = &stylesheet.rules[0] {
      assert_eq!(media.rules.len(), 1);
    } else {
      panic!("Expected media rule");
    }
  }

  #[test]
  fn test_parse_nested_media_queries() {
    let css = r"
            .outer { color: red; }
            @media (min-width: 768px) {
                .inner { color: blue; }
                @media (max-width: 1024px) {
                    .nested { color: green; }
                }
            }
        ";
    let stylesheet = parse_stylesheet(css).unwrap();
    assert_eq!(stylesheet.rules.len(), 2);
  }

  #[test]
  fn test_collect_style_rules_with_media() {
    use crate::style::media::MediaContext;

    let css = r"
            .always { color: red; }
            @media (min-width: 768px) {
                .wide { color: blue; }
            }
            @media (max-width: 500px) {
                .narrow { color: green; }
            }
        ";
    let stylesheet = parse_stylesheet(css).unwrap();

    // Wide viewport - should include .always and .wide but not .narrow
    let wide_ctx = MediaContext::screen(1024.0, 768.0);
    let wide_rules = stylesheet.collect_style_rules(&wide_ctx);
    assert_eq!(wide_rules.len(), 2, "Wide viewport should have 2 rules");

    // Narrow viewport - should include .always and .narrow but not .wide
    let narrow_ctx = MediaContext::screen(400.0, 600.0);
    let narrow_rules = stylesheet.collect_style_rules(&narrow_ctx);
    assert_eq!(narrow_rules.len(), 2, "Narrow viewport should have 2 rules");
  }

  #[test]
  fn custom_properties_preserve_raw_tokens() {
    let css = "--foo:  10px  var(--bar) ;";
    let decls = parse_declarations(css);
    assert_eq!(decls.len(), 1);
    assert_eq!(decls[0].property, "--foo");
    match &decls[0].value {
      PropertyValue::Custom(raw) => assert_eq!(raw, "  10px  var(--bar)"),
      other => panic!("expected custom value, got {:?}", other),
    }
    assert_eq!(decls[0].raw_value, "  10px  var(--bar)");
  }

  #[test]
  fn test_is_selector_parsing() {
    // Test that :is() pseudo-class is handled natively by the selectors crate
    let css = ":is(main > article) .toc { color: red; }";
    let stylesheet = parse_stylesheet(css).unwrap();
    assert_eq!(stylesheet.rules.len(), 1, "Should parse :is() selector");
    if let CssRule::Style(rule) = &stylesheet.rules[0] {
      assert_eq!(rule.declarations.len(), 1);
    } else {
      panic!("Expected style rule");
    }
  }

  #[test]
  fn test_is_selector_with_multiple_arguments() {
    let css = ":is(h1, h2, h3) { font-weight: bold; }";
    let stylesheet = parse_stylesheet(css).unwrap();
    assert_eq!(
      stylesheet.rules.len(),
      1,
      "Should parse :is() with multiple args"
    );
  }

  #[test]
  fn test_where_selector_parsing() {
    // :where() should also work (same mechanism as :is())
    let css = ":where(section, article) p { margin: 1em; }";
    let stylesheet = parse_stylesheet(css).unwrap();
    assert_eq!(stylesheet.rules.len(), 1, "Should parse :where() selector");
  }

  #[test]
  fn supports_rule_applies_when_feature_supported() {
    let css = r"@supports (display: grid) { .ok { color: red; } }";
    let result = parse_stylesheet_with_errors(css);
    assert_eq!(result.error_count(), 0, "parse errors: {:?}", result.errors);
    let stylesheet = result.stylesheet;
    assert_eq!(stylesheet.rules.len(), 1);
    assert!(matches!(stylesheet.rules[0], CssRule::Supports(_)));
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(rules.len(), 1);
    let rule = rules[0].rule;
    assert_eq!(rule.declarations.len(), 1);
  }

  #[test]
  fn supports_rule_skipped_when_feature_unsupported() {
    let css = r"@supports (not-a-prop: foo) { .skip { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      0,
      "unsupported feature should drop nested rules"
    );
  }

  #[test]
  fn supports_is_case_insensitive_for_properties() {
    let css = r"@supports (DISPLAY: GRID) { .ok { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      1,
      "upper-case property name should still match"
    );
  }

  #[test]
  fn supports_rejects_invalid_value_for_known_property() {
    let css = r"@supports (display: not-a-value) { .nope { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      0,
      "invalid value should cause @supports to fail"
    );
  }

  #[test]
  fn supports_selector_applies_when_selector_parses() {
    let css = r"@supports selector(div > span) { .ok { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    assert_eq!(stylesheet.rules.len(), 1, "expected one @supports rule");
    let cond = match &stylesheet.rules[0] {
      CssRule::Supports(rule) => rule.condition.clone(),
      other => panic!("expected supports rule, got {:?}", other),
    };
    assert!(
      cond.matches(),
      "selector() condition should match, got {:?}",
      cond
    );
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      1,
      "supported selector should enable nested rules"
    );
  }

  #[test]
  fn supports_selector_skips_when_selector_unsupported() {
    let css = r"@supports selector(div:unknown-pseudo) { .skip { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      0,
      "unsupported selector should disable nested rules"
    );
  }

  #[test]
  fn supports_selector_list_fails_when_any_selector_unsupported() {
    // A selector list that contains an unsupported selector should cause the whole selector()
    // feature query to evaluate to false per the spec.
    let css = r"@supports selector(div, span:unknown-pseudo) { .skip { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      0,
      "any unsupported selector in list should disable nested rules"
    );
  }

  #[test]
  fn supports_not_selector_inverts_result() {
    let css = r"@supports not selector(div:unknown-pseudo) { .ok { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      1,
      "not selector(...) should invert unsupported selector result"
    );
  }

  #[test]
  fn supports_selector_allows_has() {
    let css = r"@supports selector(div:has(span)) { .ok { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      1,
      "supported :has() selector should enable rules"
    );
  }

  #[test]
  fn supports_selector_prelude_parses_function() {
    match parse_supports_prelude("selector(div > span)") {
      SupportsCondition::Selector(sel) => assert_eq!(sel, "div > span"),
      other => panic!("expected selector condition, got {:?}", other),
    }
  }

  #[test]
  fn supports_selector_allows_whitespace_and_case() {
    let css = r"@supports SeLeCtOr ( div.foo , span.bar ) { .ok { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      1,
      "case-insensitive selector() with whitespace should match"
    );
  }

  #[test]
  fn supports_and_is_case_insensitive() {
    let css = r"@supports (display: grid) AND (position: sticky) { .ok { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(rules.len(), 1, "AND should be case-insensitive");
  }

  #[test]
  fn supports_or_is_case_insensitive() {
    let css = r"@supports (not-a-prop: foo) Or (display: grid) { .ok { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      1,
      "OR should be case-insensitive and allow a supported branch"
    );
  }

  #[test]
  fn supports_not_is_case_insensitive() {
    let css = r"@supports NOT (display: grid) { .skip { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      0,
      "NOT should be case-insensitive and flip supported feature to false"
    );
  }

  #[test]
  fn supports_and_or_precedence_is_correct() {
    let css = r"@supports (display: grid) or ((position: sticky) and (float: left)) { .ok { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      1,
      "or of supported and unsupported groups should still match"
    );

    let css2 =
      r"@supports (not-a-prop: foo) or (position: sticky and float: left) { .ok { color: red; } }";
    let stylesheet2 = parse_stylesheet(css2).unwrap();
    let rules2 = stylesheet2.collect_style_rules(&media);
    assert_eq!(rules2.len(), 1, "OR with a supported branch should succeed");
  }

  #[test]
  fn supports_custom_property_always_matches_with_value() {
    let css = r"@supports (--foo: bar) { .ok { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      1,
      "custom property should be considered supported in @supports"
    );
  }

  #[test]
  fn supports_custom_property_requires_value() {
    let css = r"@supports (--foo: ) { .skip { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      1,
      "custom properties are always supported even with empty values"
    );
  }

  #[test]
  fn supports_nested_not_inside_parens() {
    let css = r"@supports (not (display: grid)) { .skip { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      0,
      "nested not with supported feature should fail"
    );
  }

  #[test]
  fn supports_double_not_inside_parens() {
    let css = r"@supports (not (not (display: grid))) { .ok { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      1,
      "double not should restore supported condition to true"
    );
  }

  #[test]
  fn supports_not_with_inner_or() {
    let css = r"@supports (not ((display: grid) or (position: sticky))) { .skip { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(rules.len(), 0, "not of true inner or should fail");
  }

  #[test]
  fn supports_operator_boundary_allows_parens_without_spaces() {
    let css = r"@supports ((display: grid)and(position: sticky)) { .ok { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();
    let media = crate::style::media::MediaContext::screen(800.0, 600.0);
    let rules = stylesheet.collect_style_rules(&media);
    assert_eq!(
      rules.len(),
      1,
      "parentheses-adjacent operator should still parse"
    );
  }

  #[test]
  fn rel_tokenization_splits_and_lowercases() {
    let tokens = tokenize_rel_list(" StyleSheet  Alternate   PREFETCH ");
    assert_eq!(tokens, vec!["stylesheet", "alternate", "prefetch"]);
    assert!(rel_list_contains_stylesheet(&tokens));
  }

  #[test]
  fn extract_css_sources_preserves_order_and_attrs() {
    let html = r#"
      <head>
        <style id="one">body { color: red; }</style>
        <link rel="StyleSheet alternate" href="a.css" media="screen">
        <style id="two" media="print">p { color: blue; }</style>
      </head>
    "#;
    let dom = crate::dom::parse_html(html).unwrap();
    let sources = extract_css_sources(&dom);
    assert_eq!(sources.len(), 3);

    match &sources[0] {
      StylesheetSource::Inline(inline) => assert!(inline.css.contains("red")),
      other => panic!("expected inline style, got {:?}", other),
    }

    match &sources[1] {
      StylesheetSource::External(link) => {
        assert_eq!(link.href, "a.css");
        assert!(rel_list_contains_stylesheet(&link.rel));
        assert_eq!(link.media.as_deref(), Some("screen"));
      }
      other => panic!("expected external stylesheet link, got {:?}", other),
    }

    match &sources[2] {
      StylesheetSource::Inline(inline) => {
        assert_eq!(inline.media.as_deref(), Some("print"));
        assert!(inline.css.contains("blue"));
      }
      other => panic!("expected inline style, got {:?}", other),
    }
  }
}
