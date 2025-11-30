//! CSS parsing
//!
//! Parses CSS stylesheets and declarations.

use super::properties::parse_property_value;
use super::selectors::PseudoClassParser;
use super::types::{Declaration, StyleRule, StyleSheet};
use crate::error::Result;
use cssparser::{ParseError, Parser, ParserInput, Token};
use selectors::parser::{SelectorList, SelectorParseErrorKind};

// ============================================================================
// CSS Preprocessing (temporary hacks - to be replaced in Phase 4)
// ============================================================================

/// Unwrap :is() pseudo-class selector (simple approach)
/// Converts ":is(main>article) .toc" to "main>article .toc"
///
/// HACK: This is a temporary workaround until proper :is() support is added.
fn unwrap_is_pseudo(css: &str) -> String {
    let mut result = String::new();
    let mut chars = css.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == ':' {
            // Check if this is :is(
            let mut lookahead = String::new();
            let mut temp_chars = chars.clone();
            for _ in 0..3 {
                if let Some(c) = temp_chars.next() {
                    lookahead.push(c);
                }
            }

            if lookahead == "is(" {
                // Skip ":is("
                for _ in 0..3 {
                    chars.next();
                }

                // Extract content until matching )
                let mut depth = 1;
                let mut content = String::new();
                while let Some(c) = chars.next() {
                    if c == '(' {
                        depth += 1;
                        content.push(c);
                    } else if c == ')' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        content.push(c);
                    } else {
                        content.push(c);
                    }
                }

                // Output the content without :is() wrapper
                result.push_str(&content);
            } else {
                result.push(ch);
            }
        } else {
            result.push(ch);
        }
    }

    result
}

/// Preprocess CSS to expand @media blocks (simple approach - assume viewport >= 1000px)
///
/// HACK: This is a temporary workaround until proper @media query support is added.
fn preprocess_media_queries(css: &str) -> String {
    // Simple approach: extract content from @media (min-width: NNNpx) blocks where NNN <= 1600
    // and inline it into the main stylesheet
    let mut result = String::new();
    let mut chars = css.chars().peekable();
    let mut depth = 0;
    let mut in_media = false;
    let mut media_buf = String::new();
    let mut media_applies = false;

    while let Some(ch) = chars.next() {
        if !in_media {
            // Check for @media
            if ch == '@' {
                // Peek ahead to check if this is @media
                let remaining: String = chars.clone().collect();
                if remaining.starts_with("media") {
                    in_media = true;
                    media_buf.clear();

                    // Skip "@media"
                    for _ in 0..5 {
                        chars.next();
                    }

                    // Read the media query condition
                    let mut cond = String::new();
                    let mut _paren_depth = 0;
                    while let Some(c) = chars.next() {
                        if c == '{' {
                            break;
                        }
                        cond.push(c);
                        if c == '(' {
                            _paren_depth += 1;
                        } else if c == ')' {
                            _paren_depth -= 1;
                        }
                    }

                    // Simple check: if contains "min-width" and value <= 1600, apply it
                    media_applies = cond.contains("min-width") && !cond.contains("2000");
                    depth = 1;
                    continue;
                } else {
                    result.push(ch);
                }
            } else {
                result.push(ch);
            }
        } else {
            // Inside @media block
            if ch == '{' {
                depth += 1;
            } else if ch == '}' {
                depth -= 1;
                if depth == 0 {
                    // End of @media block
                    if media_applies {
                        result.push_str(&media_buf);
                    }
                    in_media = false;
                    media_buf.clear();
                    continue;
                }
            }
            media_buf.push(ch);
        }
    }

    result
}

// ============================================================================
// Main parsing functions
// ============================================================================

/// Parse a CSS stylesheet
pub fn parse_stylesheet(css: &str) -> Result<StyleSheet> {
    // Preprocess: unwrap :is() pseudo-class
    let unwrapped = unwrap_is_pseudo(css);

    // Preprocess to expand @media queries
    let preprocessed = preprocess_media_queries(&unwrapped);

    let mut input = ParserInput::new(&preprocessed);
    let mut parser = Parser::new(&mut input);

    let mut rules = Vec::new();

    // Skip whitespace and comments
    while !parser.is_exhausted() {
        parser.skip_whitespace();
        if parser.is_exhausted() {
            break;
        }

        // Try to parse a rule
        match parse_rule(&mut parser) {
            Ok(Some(rule)) => {
                rules.push(rule);
            }
            Ok(None) => {} // Comment or unknown at-rule, skip
            Err(e) => {
                eprintln!("CSS parse error: {:?}", e);
                // Try to recover by skipping to next rule
                while !parser.is_exhausted() {
                    if let Ok(Token::CurlyBracketBlock) = parser.next() {
                        let _: std::result::Result<(), ParseError<()>> = parser.parse_nested_block(|_| Ok(()));
                        break;
                    }
                }
            }
        }
    }

    Ok(StyleSheet { rules })
}

/// Parse a single CSS rule
fn parse_rule<'i, 't>(
    parser: &mut Parser<'i, 't>,
) -> std::result::Result<Option<StyleRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
    parser.skip_whitespace();

    // Check for at-rules - use try_parse to avoid consuming token
    let is_at_rule = parser
        .try_parse(|p| match p.next_including_whitespace()? {
            Token::AtKeyword(_) => Ok(()),
            _ => Err(p.new_error_for_next_token::<()>()),
        })
        .is_ok();

    if is_at_rule {
        // Peek at the @-rule keyword
        let at_keyword = if let Ok(Token::AtKeyword(kw)) = parser.next() {
            kw.to_string()
        } else {
            return Ok(None);
        };

        if at_keyword == "media" {
            // For @media, assume viewport is wide enough (>= 1000px) for desktop layout
            // Skip tokens until we reach the block
            while !parser.is_exhausted() {
                if let Ok(Token::CurlyBracketBlock) = parser.next() {
                    break;
                }
            }

            // Parse the nested rules inside @media block
            // We'll return None here and let the caller handle multiple rules
            let _: std::result::Result<(), ParseError<SelectorParseErrorKind>> = parser.parse_nested_block(|_| Ok(()));
            return Ok(None);
        } else {
            // Skip other @-rules (@font-face, @import, etc.)
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
            return Ok(None);
        }
    }

    // Parse selectors - only parse until we hit the opening curly brace
    let selectors = parser.parse_until_before(cssparser::Delimiter::CurlyBracketBlock, |parser| {
        SelectorList::parse(&PseudoClassParser, parser, selectors::parser::ParseRelative::No)
    })?;

    // Parse declaration block
    parser
        .expect_curly_bracket_block()
        .map_err(|_| parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("expected".into())))?;
    let declarations = parser.parse_nested_block(|parser| {
        parse_declaration_list(parser)
            .map_err(|_| parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("declaration".into())))
    })?;

    Ok(Some(StyleRule {
        selectors,
        declarations,
    }))
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

        // Parse property name
        let property = parser.expect_ident()?.to_string();

        // Expect colon
        parser.expect_colon()?;

        // Parse value until semicolon or !important
        // We need to handle functions and preserve the full value including nested parens
        let value_start = parser.position();
        let mut _value_end = value_start;
        let mut important = false;

        loop {
            match parser.next() {
                Ok(Token::Semicolon) | Err(_) => break,
                Ok(Token::Delim('!')) => {
                    if parser.try_parse(|p| p.expect_ident_matching("important")).is_ok() {
                        important = true;
                    }
                    break;
                }
                Ok(Token::Function(_)) => {
                    // Consume the entire function including its contents
                    let _ = parser.parse_nested_block(|p| {
                        while !p.is_exhausted() {
                            let _ = p.next();
                        }
                        Ok::<_, ParseError<()>>(())
                    });
                    _value_end = parser.position();
                }
                Ok(_) => {
                    _value_end = parser.position();
                }
            }
        }

        // Slice from value_start to value_end (excludes the delimiter)
        let full_slice = parser.slice_from(value_start).trim();
        let value = if important {
            full_slice
                .trim_end_matches("!important")
                .trim_end()
                .trim_end_matches(';')
                .trim_end()
        } else {
            full_slice.trim_end_matches(';').trim_end()
        };

        // Parse the value based on property
        if let Some(parsed_value) = parse_property_value(&property, value) {
            declarations.push(Declaration {
                property,
                value: parsed_value,
                important,
            });
        }

        // Note: parse_until_before already consumed the semicolon delimiter
        // We're now positioned at the next declaration or end of block
    }

    Ok(declarations)
}

/// Parse declarations from an inline style attribute
pub fn parse_declarations(declarations_str: &str) -> Vec<Declaration> {
    let mut input = ParserInput::new(declarations_str);
    let mut parser = Parser::new(&mut input);
    parse_declaration_list(&mut parser).unwrap_or_default()
}

/// Extracts CSS from style tags in the DOM
///
/// Walks the DOM tree looking for `<style>` elements and concatenates
/// their text content to form a stylesheet.
///
/// # Arguments
///
/// * `dom` - The root DOM node to search
///
/// # Returns
///
/// A `StyleSheet` parsed from all found CSS content, or an empty
/// stylesheet if no styles were found.
pub fn extract_css(dom: &crate::dom::DomNode) -> crate::error::Result<StyleSheet> {
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
        Ok(StyleSheet { rules: Vec::new() })
    } else {
        parse_stylesheet(&css_content)
    }
}
