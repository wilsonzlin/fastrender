use crate::error::Result;
use crate::style::{Length, LengthUnit};
use cssparser::{ParseError, Parser, ParserInput, ToCss, Token};
use selectors::parser::{SelectorList, SelectorParseErrorKind};
use selectors::Element;
use std::fmt;

// Re-export selectors types we use
pub use selectors::matching::matches_selector;
pub use selectors::parser::SelectorImpl;

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
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        self.0.hash(&mut hasher);
        hasher.finish() as u32
    }
}

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

/// Pseudo-elements we support
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PseudoElement {
    Before,
    After,
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
        }
    }
}

/// Stylesheet containing CSS rules
#[derive(Debug, Clone)]
pub struct StyleSheet {
    pub rules: Vec<StyleRule>,
}

/// A single CSS rule (selectors + declarations)
#[derive(Debug, Clone)]
pub struct StyleRule {
    pub selectors: SelectorList<FastRenderSelectorImpl>,
    pub declarations: Vec<Declaration>,
}

/// A CSS property declaration
#[derive(Debug, Clone)]
pub struct Declaration {
    pub property: String,
    pub value: PropertyValue,
    pub important: bool,
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
    LinearGradient { angle: f32, stops: Vec<ColorStop> },
    RadialGradient { stops: Vec<ColorStop> },
}

/// Color representation
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Color {
    pub const TRANSPARENT: Self = Self { r: 0, g: 0, b: 0, a: 0 };
    pub const WHITE: Self = Self {
        r: 255,
        g: 255,
        b: 255,
        a: 255,
    };
    pub const BLACK: Self = Self {
        r: 0,
        g: 0,
        b: 0,
        a: 255,
    };

    pub const fn rgba(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self { r, g, b, a }
    }

    pub const fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b, a: 255 }
    }

    pub const fn transparent() -> Self {
        Self { r: 0, g: 0, b: 0, a: 0 }
    }

    pub const fn black() -> Self {
        Self::rgb(0, 0, 0)
    }

    pub const fn white() -> Self {
        Self::rgb(255, 255, 255)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoxShadow {
    pub offset_x: Length,
    pub offset_y: Length,
    pub blur_radius: Length,
    pub spread_radius: Length,
    pub color: Color,
    pub inset: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextShadow {
    pub offset_x: Length,
    pub offset_y: Length,
    pub blur_radius: Length,
    pub color: Color,
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
    Scale(f32, f32),
    ScaleX(f32),
    ScaleY(f32),
    Rotate(f32),
    SkewX(f32),
    SkewY(f32),
    Matrix(f32, f32, f32, f32, f32, f32),
}

/// Custom parser for pseudo-classes
struct PseudoClassParser;

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

/// Unwrap :is() pseudo-class selector (simple approach)
/// Converts ":is(main>article) .toc" to "main>article .toc"
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
                    media_applies = false;

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

/// Parse a property value (simplified)
pub fn parse_property_value(property: &str, value_str: &str) -> Option<PropertyValue> {
    let value_str = value_str.trim();
    if value_str.is_empty() {
        return None;
    }

    // Remove trailing !important if present
    let value_str = value_str.trim_end_matches("!important").trim();

    // Try to parse as color first for color properties
    if matches!(
        property,
        "color"
            | "background"
            | "background-color"
            | "border-color"
            | "border-top-color"
            | "border-right-color"
            | "border-bottom-color"
            | "border-left-color"
    ) {
        if let Ok(color) = csscolorparser::parse(value_str) {
            return Some(PropertyValue::Color(Color::rgb(
                (color.r * 255.0) as u8,
                (color.g * 255.0) as u8,
                (color.b * 255.0) as u8,
            )));
        }
    }

    // Try to parse as length
    if let Some(length) = parse_length(value_str) {
        return Some(PropertyValue::Length(length));
    }

    // Try to parse as number
    if let Ok(num) = value_str.parse::<f32>() {
        return Some(PropertyValue::Number(num));
    }

    // Check for percentage
    if value_str.ends_with('%') {
        if let Ok(num) = value_str[..value_str.len() - 1].parse::<f32>() {
            return Some(PropertyValue::Percentage(num));
        }
    }

    // Font family special handling
    if property == "font-family" {
        let families: Vec<String> = value_str
            .split(',')
            .map(|f| f.trim().trim_matches('"').trim_matches('\'').to_string())
            .collect();
        return Some(PropertyValue::FontFamily(families));
    }

    // Default to keyword
    Some(PropertyValue::Keyword(value_str.to_string()))
}

/// Parse a length value
fn parse_length(s: &str) -> Option<Length> {
    let s = s.trim();

    if s == "0" {
        return Some(Length::px(0.0));
    }

    if let Some(rest) = s.strip_suffix("px") {
        return rest.parse::<f32>().ok().map(Length::px);
    }

    if let Some(rest) = s.strip_suffix("rem") {
        return rest.parse::<f32>().ok().map(Length::rem);
    }

    if let Some(rest) = s.strip_suffix("em") {
        return rest.parse::<f32>().ok().map(Length::em);
    }

    if let Some(rest) = s.strip_suffix("pt") {
        return rest.parse::<f32>().ok().map(|v| Length {
            value: v,
            unit: LengthUnit::Pt,
        });
    }

    if let Some(rest) = s.strip_suffix("%") {
        return rest.parse::<f32>().ok().map(Length::percent);
    }

    if let Some(rest) = s.strip_suffix("vw") {
        return rest.parse::<f32>().ok().map(|v| Length {
            value: v,
            unit: LengthUnit::Vw,
        });
    }

    if let Some(rest) = s.strip_suffix("vh") {
        return rest.parse::<f32>().ok().map(|v| Length {
            value: v,
            unit: LengthUnit::Vh,
        });
    }

    if let Some(rest) = s.strip_suffix("cm") {
        return rest.parse::<f32>().ok().map(|v| Length {
            value: v,
            unit: LengthUnit::Cm,
        });
    }

    if let Some(rest) = s.strip_suffix("mm") {
        return rest.parse::<f32>().ok().map(|v| Length {
            value: v,
            unit: LengthUnit::Mm,
        });
    }

    if let Some(rest) = s.strip_suffix("in") {
        return rest.parse::<f32>().ok().map(|v| Length {
            value: v,
            unit: LengthUnit::In,
        });
    }

    None
}

/// Parse declarations from an inline style attribute
pub fn parse_declarations(declarations_str: &str) -> Vec<Declaration> {
    let mut input = ParserInput::new(declarations_str);
    let mut parser = Parser::new(&mut input);
    parse_declaration_list(&mut parser).unwrap_or_default()
}
