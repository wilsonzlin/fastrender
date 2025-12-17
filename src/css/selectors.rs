//! CSS Selector support
//!
//! Implements selector parsing and matching using the selectors crate.

use super::types::CssString;
use cssparser::{ParseError, Parser, ToCss, Token};
use selectors::parser::{SelectorImpl, SelectorParseErrorKind};
use std::fmt;

/// Direction keyword for :dir()
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextDirection {
    Ltr,
    Rtl,
}

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
    FirstOfType,
    LastOfType,
    OnlyOfType,
    NthOfType(i32, i32),
    NthLastOfType(i32, i32),
    Lang(Vec<String>),
    Dir(TextDirection),
    AnyLink,
    Target,
    Scope,
    Empty,
    Hover,
    Active,
    Focus,
    FocusWithin,
    FocusVisible,
    Disabled,
    Enabled,
    Required,
    Optional,
    Valid,
    Invalid,
    InRange,
    OutOfRange,
    ReadOnly,
    ReadWrite,
    PlaceholderShown,
    Autofill,
    Checked,
    Indeterminate,
    Link,
    Visited,
}

impl selectors::parser::NonTSPseudoClass for PseudoClass {
    type Impl = FastRenderSelectorImpl;

    fn is_active_or_hover(&self) -> bool {
        matches!(self, PseudoClass::Active | PseudoClass::Hover)
    }

    fn is_user_action_state(&self) -> bool {
        matches!(
            self,
            PseudoClass::Hover
                | PseudoClass::Active
                | PseudoClass::Focus
                | PseudoClass::FocusWithin
                | PseudoClass::FocusVisible
        )
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
            PseudoClass::FirstOfType => dest.write_str(":first-of-type"),
            PseudoClass::LastOfType => dest.write_str(":last-of-type"),
            PseudoClass::OnlyOfType => dest.write_str(":only-of-type"),
            PseudoClass::NthOfType(a, b) => write!(dest, ":nth-of-type({}n+{})", a, b),
            PseudoClass::NthLastOfType(a, b) => write!(dest, ":nth-last-of-type({}n+{})", a, b),
            PseudoClass::Lang(langs) => {
                dest.write_str(":lang(")?;
                for (i, lang) in langs.iter().enumerate() {
                    if i > 0 {
                        dest.write_str(", ")?;
                    }
                    dest.write_str(lang)?;
                }
                dest.write_str(")")
            }
            PseudoClass::Dir(dir) => match dir {
                TextDirection::Ltr => dest.write_str(":dir(ltr)"),
                TextDirection::Rtl => dest.write_str(":dir(rtl)"),
            },
            PseudoClass::AnyLink => dest.write_str(":any-link"),
            PseudoClass::Target => dest.write_str(":target"),
            PseudoClass::Scope => dest.write_str(":scope"),
            PseudoClass::Empty => dest.write_str(":empty"),
            PseudoClass::Hover => dest.write_str(":hover"),
            PseudoClass::Active => dest.write_str(":active"),
            PseudoClass::Focus => dest.write_str(":focus"),
            PseudoClass::FocusWithin => dest.write_str(":focus-within"),
            PseudoClass::FocusVisible => dest.write_str(":focus-visible"),
            PseudoClass::Disabled => dest.write_str(":disabled"),
            PseudoClass::Enabled => dest.write_str(":enabled"),
            PseudoClass::Required => dest.write_str(":required"),
            PseudoClass::Optional => dest.write_str(":optional"),
            PseudoClass::Valid => dest.write_str(":valid"),
            PseudoClass::Invalid => dest.write_str(":invalid"),
            PseudoClass::InRange => dest.write_str(":in-range"),
            PseudoClass::OutOfRange => dest.write_str(":out-of-range"),
            PseudoClass::Indeterminate => dest.write_str(":indeterminate"),
            PseudoClass::ReadOnly => dest.write_str(":read-only"),
            PseudoClass::ReadWrite => dest.write_str(":read-write"),
            PseudoClass::PlaceholderShown => dest.write_str(":placeholder-shown"),
            PseudoClass::Autofill => dest.write_str(":autofill"),
            PseudoClass::Checked => dest.write_str(":checked"),
            PseudoClass::Link => dest.write_str(":link"),
            PseudoClass::Visited => dest.write_str(":visited"),
        }
    }
}

// ============================================================================
// Pseudo-elements
// ============================================================================

/// Pseudo-elements we support
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
        let lowered = name.to_ascii_lowercase();
        match lowered.as_str() {
            "root" => Ok(PseudoClass::Root),
            "first-child" => Ok(PseudoClass::FirstChild),
            "last-child" => Ok(PseudoClass::LastChild),
            "only-child" => Ok(PseudoClass::OnlyChild),
            "first-of-type" => Ok(PseudoClass::FirstOfType),
            "last-of-type" => Ok(PseudoClass::LastOfType),
            "only-of-type" => Ok(PseudoClass::OnlyOfType),
            "empty" => Ok(PseudoClass::Empty),
            "hover" => Ok(PseudoClass::Hover),
            "active" => Ok(PseudoClass::Active),
            "focus" => Ok(PseudoClass::Focus),
            "focus-within" => Ok(PseudoClass::FocusWithin),
            "focus-visible" => Ok(PseudoClass::FocusVisible),
            "disabled" => Ok(PseudoClass::Disabled),
            "enabled" => Ok(PseudoClass::Enabled),
            "required" => Ok(PseudoClass::Required),
            "optional" => Ok(PseudoClass::Optional),
            "valid" => Ok(PseudoClass::Valid),
            "invalid" => Ok(PseudoClass::Invalid),
            "in-range" => Ok(PseudoClass::InRange),
            "out-of-range" => Ok(PseudoClass::OutOfRange),
            "indeterminate" => Ok(PseudoClass::Indeterminate),
            "read-only" => Ok(PseudoClass::ReadOnly),
            "read-write" => Ok(PseudoClass::ReadWrite),
            "placeholder-shown" => Ok(PseudoClass::PlaceholderShown),
            "autofill" => Ok(PseudoClass::Autofill),
            "checked" => Ok(PseudoClass::Checked),
            "link" => Ok(PseudoClass::Link),
            "visited" => Ok(PseudoClass::Visited),
            "any-link" => Ok(PseudoClass::AnyLink),
            "target" => Ok(PseudoClass::Target),
            "scope" => Ok(PseudoClass::Scope),
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
        let lowered = name.to_ascii_lowercase();
        match lowered.as_str() {
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
            "nth-of-type" => {
                let (a, b) = parse_nth(parser).map_err(|_| {
                    parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name.clone()))
                })?;
                Ok(PseudoClass::NthOfType(a, b))
            }
            "nth-last-of-type" => {
                let (a, b) = parse_nth(parser).map_err(|_| {
                    parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name.clone()))
                })?;
                Ok(PseudoClass::NthLastOfType(a, b))
            }
            "dir" => {
                let dir = match parser.expect_ident() {
                    Ok(d) => d,
                    Err(_) => {
                        return Err(parser
                            .new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name.clone())))
                    }
                };
                let lowered = dir.to_ascii_lowercase();
                match lowered.as_str() {
                    "ltr" => Ok(PseudoClass::Dir(TextDirection::Ltr)),
                    "rtl" => Ok(PseudoClass::Dir(TextDirection::Rtl)),
                    _ => {
                        Err(parser
                            .new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name.clone())))
                    }
                }
            }
            "lang" => {
                let mut langs = Vec::new();
                loop {
                    let range = match parser.expect_ident_or_string() {
                        Ok(r) => r,
                        Err(_) => {
                            return Err(parser.new_custom_error(
                                SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name.clone()),
                            ))
                        }
                    };
                    langs.push(range.as_ref().to_ascii_lowercase());
                    if parser.try_parse(|p| p.expect_comma()).is_err() {
                        break;
                    }
                }
                Ok(PseudoClass::Lang(langs))
            }
            _ => Err(parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name))),
        }
    }

    fn parse_pseudo_element(
        &self,
        _location: cssparser::SourceLocation,
        name: cssparser::CowRcStr<'i>,
    ) -> std::result::Result<PseudoElement, ParseError<'i, Self::Error>> {
        let lowered = name.to_ascii_lowercase();
        match lowered.as_str() {
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
    cssparser::parse_nth(parser).map_err(Into::into)
}

#[cfg(test)]
mod tests {
    use super::*;
    use cssparser::{ParserInput, SourceLocation, ToCss};
    use selectors::Parser as SelectorsParser;

    fn parse(expr: &str) -> (i32, i32) {
        let mut input = ParserInput::new(expr);
        let mut parser = Parser::new(&mut input);
        parse_nth(&mut parser).expect("should parse nth expression")
    }

    #[test]
    fn parses_an_plus_b_syntax() {
        assert_eq!(parse("odd"), (2, 1));
        assert_eq!(parse("even"), (2, 0));
        assert_eq!(parse("2n+1"), (2, 1));
        assert_eq!(parse("-2n+3"), (-2, 3));
        assert_eq!(parse("n"), (1, 0));
        assert_eq!(parse("+n-1"), (1, -1));
        assert_eq!(parse("4"), (0, 4));
        assert_eq!(parse("-5"), (0, -5));
    }

    #[test]
    fn rejects_invalid_nth_expression() {
        let mut input = ParserInput::new("n+");
        let mut parser = Parser::new(&mut input);
        assert!(parse_nth(&mut parser).is_err());
    }

    #[test]
    fn parses_pseudo_classes_case_insensitively() {
        let parser = PseudoClassParser;
        let loc = SourceLocation { line: 0, column: 0 };
        let root = parser
            .parse_non_ts_pseudo_class(loc, cssparser::CowRcStr::from("RoOt"))
            .expect("root pseudo should parse");
        assert_eq!(root, PseudoClass::Root);

        let mut input = ParserInput::new("2n+1");
        let mut css_parser = Parser::new(&mut input);
        let nth = parser
            .parse_non_ts_functional_pseudo_class(cssparser::CowRcStr::from("NTH-CHILD"), &mut css_parser, false)
            .expect("nth-child pseudo should parse");
        assert!(matches!(nth, PseudoClass::NthChild(_, _)));
    }

    #[test]
    fn parses_pseudo_elements_case_insensitively() {
        let parser = PseudoClassParser;
        let loc = SourceLocation { line: 0, column: 0 };
        assert_eq!(
            parser
                .parse_pseudo_element(loc, cssparser::CowRcStr::from("BeFoRe"))
                .expect("before pseudo"),
            PseudoElement::Before
        );
        assert_eq!(
            parser
                .parse_pseudo_element(loc, cssparser::CowRcStr::from("MARKER"))
                .expect("marker pseudo"),
            PseudoElement::Marker
        );
    }

    #[test]
    fn to_css_serializes_new_pseudo_classes() {
        assert_eq!(PseudoClass::FirstOfType.to_css_string(), ":first-of-type");
        assert_eq!(PseudoClass::LastOfType.to_css_string(), ":last-of-type");
        assert_eq!(PseudoClass::OnlyOfType.to_css_string(), ":only-of-type");
        assert_eq!(PseudoClass::Empty.to_css_string(), ":empty");
        assert_eq!(PseudoClass::NthOfType(2, 1).to_css_string(), ":nth-of-type(2n+1)");
        assert_eq!(
            PseudoClass::NthLastOfType(-1, 3).to_css_string(),
            ":nth-last-of-type(-1n+3)"
        );
        assert_eq!(
            PseudoClass::Lang(vec!["en".into(), "fr-ca".into()]).to_css_string(),
            ":lang(en, fr-ca)"
        );
        assert_eq!(PseudoClass::Dir(TextDirection::Ltr).to_css_string(), ":dir(ltr)");
        assert_eq!(PseudoClass::AnyLink.to_css_string(), ":any-link");
        assert_eq!(PseudoClass::Target.to_css_string(), ":target");
        assert_eq!(PseudoClass::Scope.to_css_string(), ":scope");
        assert_eq!(PseudoClass::Disabled.to_css_string(), ":disabled");
        assert_eq!(PseudoClass::Enabled.to_css_string(), ":enabled");
        assert_eq!(PseudoClass::Required.to_css_string(), ":required");
        assert_eq!(PseudoClass::Optional.to_css_string(), ":optional");
        assert_eq!(PseudoClass::Valid.to_css_string(), ":valid");
        assert_eq!(PseudoClass::Invalid.to_css_string(), ":invalid");
        assert_eq!(PseudoClass::InRange.to_css_string(), ":in-range");
        assert_eq!(PseudoClass::OutOfRange.to_css_string(), ":out-of-range");
        assert_eq!(PseudoClass::Indeterminate.to_css_string(), ":indeterminate");
        assert_eq!(PseudoClass::FocusWithin.to_css_string(), ":focus-within");
        assert_eq!(PseudoClass::FocusVisible.to_css_string(), ":focus-visible");
        assert_eq!(PseudoClass::ReadOnly.to_css_string(), ":read-only");
        assert_eq!(PseudoClass::ReadWrite.to_css_string(), ":read-write");
        assert_eq!(PseudoClass::PlaceholderShown.to_css_string(), ":placeholder-shown");
        assert_eq!(PseudoClass::Autofill.to_css_string(), ":autofill");
    }
}
