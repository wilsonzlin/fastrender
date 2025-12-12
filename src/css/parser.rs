//! CSS parsing
//!
//! Parses CSS stylesheets and declarations.

use super::properties::parse_property_value;
use super::selectors::PseudoClassParser;
use super::types::{CssRule, Declaration, ImportRule, LayerRule, MediaRule, StyleRule, StyleSheet};
use crate::dom::DomNode;
use crate::error::Result;
use crate::style::media::MediaQuery;
use cssparser::{ParseError, Parser, ParserInput, ToCss, Token};
use selectors::parser::{SelectorList, SelectorParseErrorKind};

// ============================================================================
// Main parsing functions
// ============================================================================

/// Parse a CSS stylesheet
///
/// This parser now properly handles @media rules, building a tree of
/// CssRule that can be evaluated against a MediaContext during cascade.
pub fn parse_stylesheet(css: &str) -> Result<StyleSheet> {
    let mut input = ParserInput::new(css);
    let mut parser = Parser::new(&mut input);

    let rules = parse_rule_list(&mut parser);

    Ok(StyleSheet { rules })
}

/// Parse a list of CSS rules (top-level or inside @media block)
fn parse_rule_list<'i, 't>(parser: &mut Parser<'i, 't>) -> Vec<CssRule> {
    let mut rules = Vec::new();

    while !parser.is_exhausted() {
        parser.skip_whitespace();
        if parser.is_exhausted() {
            break;
        }

        match parse_rule(parser) {
            Ok(Some(rule)) => {
                rules.push(rule);
            }
            Ok(None) => {} // Comment or skipped at-rule
            Err(e) => {
                eprintln!("CSS parse error: {:?}", e);
                // Try to recover by skipping to next rule
                recover_from_error(parser);
            }
        }
    }

    rules
}

/// Recover from a parse error by skipping to the next rule
fn recover_from_error<'i, 't>(parser: &mut Parser<'i, 't>) {
    while !parser.is_exhausted() {
        if let Ok(Token::CurlyBracketBlock) = parser.next() {
            let _: std::result::Result<(), ParseError<()>> = parser.parse_nested_block(|_| Ok(()));
            break;
        }
    }
}

/// Parse a single CSS rule (style rule or @-rule)
fn parse_rule<'i, 't>(
    parser: &mut Parser<'i, 't>,
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
                "media" => parse_media_rule(p),
                "layer" => parse_layer_rule(p),
                _ => {
                    skip_at_rule(p);
                    Ok(None)
                }
            }
        } else {
            // Not an at-rule, return error to restore position
            Err(p.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("not at-rule".into())))
        }
    }) {
        return Ok(result);
    }

    // Parse style rule
    parse_style_rule(parser).map(|opt| opt.map(CssRule::Style))
}

/// Parse an @import rule
fn parse_import_rule<'i, 't>(
    parser: &mut Parser<'i, 't>,
) -> std::result::Result<Option<CssRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
    parser.skip_whitespace();

    // Import target can be url() or string
    let href = match parser.next_including_whitespace() {
        Ok(Token::Function(f)) if f.as_ref().eq_ignore_ascii_case("url") => parser.parse_nested_block(|p| {
            p.skip_whitespace();
            match p.next_including_whitespace() {
                Ok(Token::QuotedString(s)) | Ok(Token::Ident(s)) => Ok(s.to_string()),
                _ => Err(p.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("expected url".into()))),
            }
        })?,
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
        Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(parse_rule_list(nested_parser))
    })?;

    Ok(Some(CssRule::Media(MediaRule {
        query,
        rules: nested_rules,
    })))
}

fn parse_layer_rule<'i, 't>(
    parser: &mut Parser<'i, 't>,
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
        parser.parse_nested_block(|nested| Ok::<_, ParseError<'i, SelectorParseErrorKind<'i>>>(parse_rule_list(nested)))?
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

/// Parse a style rule (selectors + declarations)
fn parse_style_rule<'i, 't>(
    parser: &mut Parser<'i, 't>,
) -> std::result::Result<Option<StyleRule>, ParseError<'i, SelectorParseErrorKind<'i>>> {
    // Parse selectors
    let selectors = parser.parse_until_before(cssparser::Delimiter::CurlyBracketBlock, |parser| {
        SelectorList::parse(&PseudoClassParser, parser, selectors::parser::ParseRelative::No)
    })?;

    // Parse declaration block
    parser
        .expect_curly_bracket_block()
        .map_err(|_| parser.new_custom_error(SelectorParseErrorKind::UnexpectedIdent("expected {".into())))?;

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
        let property = match parser.expect_ident() {
            Ok(ident) => ident.to_string(),
            Err(_) => {
                // Skip to next declaration
                skip_to_semicolon(parser);
                continue;
            }
        };

        // Expect colon
        if parser.expect_colon().is_err() {
            skip_to_semicolon(parser);
            continue;
        }

        // Parse value until semicolon or !important
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

        // Slice from value_start to value_end
        let full_slice_raw = parser.slice_from(value_start);

        // Strip trailing "!important" / semicolon but preserve leading whitespace for custom properties.
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

        // Parse the value based on property
        if let Some(parsed_value) = parse_property_value(&property, value) {
            declarations.push(Declaration {
                property,
                value: parsed_value,
                raw_value: value.to_string(),
                important,
            });
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
    use crate::PropertyValue;

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
        assert_eq!(stylesheet.rules.len(), 1, "Should parse :is() with multiple args");
    }

    #[test]
    fn test_where_selector_parsing() {
        // :where() should also work (same mechanism as :is())
        let css = ":where(section, article) p { margin: 1em; }";
        let stylesheet = parse_stylesheet(css).unwrap();
        assert_eq!(stylesheet.rules.len(), 1, "Should parse :where() selector");
    }
}
