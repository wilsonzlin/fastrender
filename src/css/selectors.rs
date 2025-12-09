//! CSS Selector support
//!
//! Implements selector parsing and matching using the selectors crate.

use super::types::CssString;
use cssparser::{ParseError, Parser, ToCss, Token};
use selectors::parser::{SelectorImpl, SelectorParseErrorKind};
use std::fmt;

// ============================================================================
// Selector implementation for FastRender
// ============================================================================

/// Our custom SelectorImpl for FastRender
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FastRenderSelectorImpl;

impl SelectorImpl for FastRenderSelectorImpl {
    type ExtraMatchingData<'a> = ();
    type AttrValue = CssString;
    type Identifier = CssString;
    type LocalName = CssString;
    type NamespacePrefix = CssString;
    type NamespaceUrl = CssString;
    type BorrowedLocalName = str;
    type BorrowedNamespaceUrl = str;

    type NonTSPseudoClass = PseudoClass;
    type PseudoElement = PseudoElement;
}

// ============================================================================
// Pseudo-classes
// ============================================================================

/// Pseudo-classes we support
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PseudoClass {
    Root,
    FirstChild,
    LastChild,
    NthChild(i32, i32), // an + b
    NthLastChild(i32, i32),
    OnlyChild,
    Hover,
    Active,
    Focus,
    Link,
    Visited,
}

impl selectors::parser::NonTSPseudoClass for PseudoClass {
    type Impl = FastRenderSelectorImpl;

    fn is_active_or_hover(&self) -> bool {
        matches!(self, PseudoClass::Active | PseudoClass::Hover)
    }

    fn is_user_action_state(&self) -> bool {
        matches!(self, PseudoClass::Hover | PseudoClass::Active | PseudoClass::Focus)
    }
}

impl ToCss for PseudoClass {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        match self {
            PseudoClass::Root => dest.write_str(":root"),
            PseudoClass::FirstChild => dest.write_str(":first-child"),
            PseudoClass::LastChild => dest.write_str(":last-child"),
            PseudoClass::NthChild(a, b) => write!(dest, ":nth-child({}n+{})", a, b),
            PseudoClass::NthLastChild(a, b) => write!(dest, ":nth-last-child({}n+{})", a, b),
            PseudoClass::OnlyChild => dest.write_str(":only-child"),
            PseudoClass::Hover => dest.write_str(":hover"),
            PseudoClass::Active => dest.write_str(":active"),
            PseudoClass::Focus => dest.write_str(":focus"),
            PseudoClass::Link => dest.write_str(":link"),
            PseudoClass::Visited => dest.write_str(":visited"),
        }
    }
}

// ============================================================================
// Pseudo-elements
// ============================================================================

/// Pseudo-elements we support
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PseudoElement {
    Before,
    After,
    Marker,
}

impl selectors::parser::PseudoElement for PseudoElement {
    type Impl = FastRenderSelectorImpl;
}

impl ToCss for PseudoElement {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        match self {
            PseudoElement::Before => dest.write_str("::before"),
            PseudoElement::After => dest.write_str("::after"),
            PseudoElement::Marker => dest.write_str("::marker"),
        }
    }
}

// ============================================================================
// Pseudo-class parser
// ============================================================================

/// Custom parser for pseudo-classes
pub(crate) struct PseudoClassParser;

impl<'i> selectors::parser::Parser<'i> for PseudoClassParser {
    type Impl = FastRenderSelectorImpl;
    type Error = SelectorParseErrorKind<'i>;

    fn parse_non_ts_pseudo_class(
        &self,
        _location: cssparser::SourceLocation,
        name: cssparser::CowRcStr<'i>,
    ) -> std::result::Result<PseudoClass, ParseError<'i, Self::Error>> {
        match &*name {
            "root" => Ok(PseudoClass::Root),
            "first-child" => Ok(PseudoClass::FirstChild),
            "last-child" => Ok(PseudoClass::LastChild),
            "only-child" => Ok(PseudoClass::OnlyChild),
            "hover" => Ok(PseudoClass::Hover),
            "active" => Ok(PseudoClass::Active),
            "focus" => Ok(PseudoClass::Focus),
            "link" => Ok(PseudoClass::Link),
            "visited" => Ok(PseudoClass::Visited),
            _ => Err(ParseError {
                kind: cssparser::ParseErrorKind::Basic(cssparser::BasicParseErrorKind::UnexpectedToken(Token::Ident(
                    name,
                ))),
                location: _location,
            }),
        }
    }

    fn parse_non_ts_functional_pseudo_class<'t>(
        &self,
        name: cssparser::CowRcStr<'i>,
        parser: &mut Parser<'i, 't>,
        _is_starting_single_colon: bool,
    ) -> std::result::Result<PseudoClass, ParseError<'i, Self::Error>> {
        match &*name {
            "nth-child" => {
                let (a, b) = parse_nth(parser).map_err(|_| {
                    parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name.clone()))
                })?;
                Ok(PseudoClass::NthChild(a, b))
            }
            "nth-last-child" => {
                let (a, b) = parse_nth(parser).map_err(|_| {
                    parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name.clone()))
                })?;
                Ok(PseudoClass::NthLastChild(a, b))
            }
            _ => Err(parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name))),
        }
    }

    fn parse_pseudo_element(
        &self,
        _location: cssparser::SourceLocation,
        name: cssparser::CowRcStr<'i>,
    ) -> std::result::Result<PseudoElement, ParseError<'i, Self::Error>> {
        match &*name {
            "before" => Ok(PseudoElement::Before),
            "after" => Ok(PseudoElement::After),
            "marker" => Ok(PseudoElement::Marker),
            _ => Err(ParseError {
                kind: cssparser::ParseErrorKind::Basic(cssparser::BasicParseErrorKind::UnexpectedToken(Token::Ident(
                    name,
                ))),
                location: _location,
            }),
        }
    }

    fn parse_is_and_where(&self) -> bool {
        // Enable parsing of :is() and :where() pseudo-classes
        true
    }
}

/// Parse nth-child/nth-last-child expressions
fn parse_nth<'i, 't>(parser: &mut Parser<'i, 't>) -> std::result::Result<(i32, i32), ParseError<'i, ()>> {
    // Simplified: just handle numbers and "odd"/"even"
    let location = parser.current_source_location();
    let token = parser.next()?.clone();
    match &token {
        Token::Number { int_value: Some(b), .. } => {
            // Just a number: 0n+b
            Ok((0, *b))
        }
        Token::Ident(ident) => {
            match &**ident {
                "odd" => Ok((2, 1)),  // 2n+1
                "even" => Ok((2, 0)), // 2n+0
                _ => Err(location.new_unexpected_token_error(token.clone())),
            }
        }
        _ => Err(location.new_unexpected_token_error(token)),
    }
}
