# Phase 4: CSS Parser Improvements

**Goal**: Replace hacky CSS preprocessing with proper parsing and evaluation.

**Prerequisites**: Phase 2 (Module Surgery) - for clean code structure

**Priority**: HIGH - This unblocks Phase 3 hack removal

---

## Overview

The CSS parser has two major hacks:
1. `unwrap_is_pseudo()` - char-by-char :is() removal
2. `preprocess_media_queries()` - string-based media query "evaluation"

These prevent proper CSS processing and force site-specific hacks.

---

## Task 4.1: Implement Proper @media Query Support

### Current State

**Location**: `src/css.rs` lines 426-499 (after Phase 2: `src/css/parser.rs`)

**Current "Implementation"**:
```rust
fn preprocess_media_queries(css: &str) -> String {
    // Simple approach: extract content from @media (min-width: NNNpx) blocks
    // where NNN <= 1600 and inline it into the main stylesheet
    // ...
    media_applies = cond.contains("min-width") && !cond.contains("2000");
    // ...
}
```

**Problems**:
- Assumes viewport >= 1000px always
- String matching instead of parsing
- Can't handle complex media queries
- No actual viewport-aware evaluation

### Target State

- Parse @media rules into an AST
- Store media rules in stylesheet
- Evaluate media queries against a `MediaContext`
- Apply rules conditionally during cascade

### Existing Code to Leverage

`src/style/media.rs` (2075 lines) already has:
- `MediaQuery` struct
- `MediaFeature` enum
- `MediaType` enum
- Parsing logic

**Key types from media.rs**:
```rust
pub struct MediaQuery {
    pub negated: bool,
    pub media_type: MediaType,
    pub features: Vec<MediaFeature>,
}

pub enum MediaFeature {
    Width(MediaRange<Length>),
    Height(MediaRange<Length>),
    MinWidth(Length),
    MaxWidth(Length),
    MinHeight(Length),
    MaxHeight(Length),
    AspectRatio(MediaRange<(u32, u32)>),
    // ... more
}

pub struct MediaRange<T> {
    pub min: Option<T>,
    pub max: Option<T>,
    pub exact: Option<T>,
}
```

### Implementation Plan

#### Step 1: Define MediaContext

**Create/update in style/media.rs or layout/constraints.rs**:
```rust
/// Context for evaluating media queries
#[derive(Debug, Clone)]
pub struct MediaContext {
    /// Viewport width in CSS pixels
    pub viewport_width: f32,
    /// Viewport height in CSS pixels
    pub viewport_height: f32,
    /// Device pixel ratio (1.0 = standard, 2.0 = retina)
    pub device_pixel_ratio: f32,
    /// Color bit depth
    pub color_depth: u8,
    /// Preferred color scheme
    pub prefers_color_scheme: ColorScheme,
    /// Reduced motion preference
    pub prefers_reduced_motion: bool,
}

impl Default for MediaContext {
    fn default() -> Self {
        Self {
            viewport_width: 1200.0,  // Desktop default
            viewport_height: 800.0,
            device_pixel_ratio: 1.0,
            color_depth: 24,
            prefers_color_scheme: ColorScheme::Light,
            prefers_reduced_motion: false,
        }
    }
}
```

#### Step 2: Add MediaRule to StyleSheet

**Update css/types.rs**:
```rust
/// A CSS rule (style rule or @-rule)
pub enum CssRule {
    /// A style rule (selectors + declarations)
    Style(StyleRule),
    /// A @media rule containing conditional rules
    Media(MediaRule),
    // Future: @import, @font-face, @keyframes, etc.
}

/// A @media rule
pub struct MediaRule {
    /// The media query to evaluate
    pub query: MediaQuery,
    /// Rules that apply when query matches
    pub rules: Vec<CssRule>,  // Can be nested!
}

/// Updated StyleSheet
pub struct StyleSheet {
    pub rules: Vec<CssRule>,
}
```

#### Step 3: Parse @media Rules

**Update css/parser.rs**:
```rust
use crate::style::media::{parse_media_query, MediaQuery};

fn parse_rule<'i, 't>(parser: &mut Parser<'i, 't>) -> Result<Option<CssRule>, ...> {
    parser.skip_whitespace();

    // Check for @-rule
    if let Ok(Token::AtKeyword(kw)) = parser.next() {
        match &*kw {
            "media" => {
                // Parse media query condition
                let query = parse_media_condition(parser)?;

                // Parse nested rules block
                parser.expect_curly_bracket_block()?;
                let nested_rules = parser.parse_nested_block(|parser| {
                    let mut rules = Vec::new();
                    while !parser.is_exhausted() {
                        if let Ok(Some(rule)) = parse_rule(parser) {
                            rules.push(rule);
                        }
                    }
                    Ok(rules)
                })?;

                return Ok(Some(CssRule::Media(MediaRule {
                    query,
                    rules: nested_rules,
                })));
            }
            // Other @-rules: @import, @font-face, etc.
            _ => {
                skip_at_rule(parser);
                return Ok(None);
            }
        }
    }

    // Regular style rule
    let style_rule = parse_style_rule(parser)?;
    Ok(Some(CssRule::Style(style_rule)))
}

fn parse_media_condition<'i, 't>(parser: &mut Parser<'i, 't>) -> Result<MediaQuery, ...> {
    // Use the existing parsing from style/media.rs
    // Parse until we hit the opening brace
    let condition_str = parser.parse_until_before(
        cssparser::Delimiter::CurlyBracketBlock,
        |p| {
            let start = p.position();
            while !p.is_exhausted() {
                let _ = p.next();
            }
            Ok(p.slice_from(start).to_string())
        }
    )?;

    crate::style::media::parse_media_query(&condition_str)
        .ok_or_else(|| parser.new_custom_error(...))
}
```

#### Step 4: Evaluate Media Queries During Cascade

**Update style/cascade.rs**:
```rust
use crate::style::media::MediaContext;

pub fn apply_styles(
    dom: &DomNode,
    stylesheet: &StyleSheet,
    media_context: &MediaContext,  // NEW PARAMETER
) -> StyledNode {
    // Filter rules by media query
    let applicable_rules = collect_applicable_rules(stylesheet, media_context);

    // Rest of cascade...
}

fn collect_applicable_rules(
    stylesheet: &StyleSheet,
    media_context: &MediaContext,
) -> Vec<&StyleRule> {
    let mut rules = Vec::new();
    collect_rules_recursive(&stylesheet.rules, media_context, &mut rules);
    rules
}

fn collect_rules_recursive<'a>(
    css_rules: &'a [CssRule],
    media_context: &MediaContext,
    out: &mut Vec<&'a StyleRule>,
) {
    for rule in css_rules {
        match rule {
            CssRule::Style(style_rule) => {
                out.push(style_rule);
            }
            CssRule::Media(media_rule) => {
                if media_rule.query.evaluate(media_context) {
                    // Recursively collect from nested rules
                    collect_rules_recursive(&media_rule.rules, media_context, out);
                }
            }
        }
    }
}
```

#### Step 5: Implement MediaQuery::evaluate

**Update style/media.rs**:
```rust
impl MediaQuery {
    /// Evaluate this media query against a context
    pub fn evaluate(&self, ctx: &MediaContext) -> bool {
        // Check media type
        let type_matches = match self.media_type {
            MediaType::All => true,
            MediaType::Screen => true,  // We're always "screen"
            MediaType::Print => false,
        };

        if !type_matches {
            return self.negated;
        }

        // Check all features
        let features_match = self.features.iter().all(|f| f.evaluate(ctx));

        if self.negated {
            !features_match
        } else {
            features_match
        }
    }
}

impl MediaFeature {
    /// Evaluate this feature against a context
    pub fn evaluate(&self, ctx: &MediaContext) -> bool {
        match self {
            Self::MinWidth(len) => {
                let px = len.to_px(ctx.viewport_width, 16.0, 16.0);
                ctx.viewport_width >= px
            }
            Self::MaxWidth(len) => {
                let px = len.to_px(ctx.viewport_width, 16.0, 16.0);
                ctx.viewport_width <= px
            }
            Self::MinHeight(len) => {
                let px = len.to_px(ctx.viewport_height, 16.0, 16.0);
                ctx.viewport_height >= px
            }
            Self::MaxHeight(len) => {
                let px = len.to_px(ctx.viewport_height, 16.0, 16.0);
                ctx.viewport_height <= px
            }
            Self::Width(range) => range.contains(ctx.viewport_width),
            Self::Height(range) => range.contains(ctx.viewport_height),
            // ... other features
            _ => true,  // Unknown features pass
        }
    }
}
```

#### Step 6: Update API

**Update api.rs**:
```rust
impl FastRender {
    pub fn render(&self, html: &str) -> Result<Pixmap> {
        let dom = parse_html(html)?;
        let stylesheet = extract_css(&dom)?;

        // Use configured media context or default
        let media_context = self.config.media_context.clone()
            .unwrap_or_else(|| MediaContext {
                viewport_width: self.config.width as f32,
                viewport_height: self.config.height as f32,
                ..Default::default()
            });

        let styled = apply_styles(&dom, &stylesheet, &media_context);
        // ... rest of pipeline
    }
}

pub struct FastRenderConfig {
    pub width: u32,
    pub height: u32,
    pub media_context: Option<MediaContext>,  // NEW
    // ...
}
```

#### Step 7: Delete preprocess_media_queries()

After all the above is working:
1. Remove `preprocess_media_queries()` from css/parser.rs
2. Remove call to it in `parse_stylesheet()`
3. Run tests

### Success Criteria

- [ ] @media rules parsed into `MediaRule` AST
- [ ] `MediaContext` can be configured
- [ ] `MediaQuery::evaluate()` works correctly
- [ ] Rules inside @media only apply when query matches
- [ ] `preprocess_media_queries()` deleted
- [ ] Tests pass
- [ ] Phase 3 Hacks 1 & 2 can be removed

---

## Task 4.2: Fix :is() Selector Support

### Current State

**Location**: `src/css.rs` lines 372-424 (after Phase 2: `src/css/parser.rs`)

**Current "Implementation"**:
```rust
fn unwrap_is_pseudo(css: &str) -> String {
    // Character-by-character string manipulation
    // Converts ":is(main>article) .toc" to "main>article .toc"
    // ...
}
```

**Problems**:
- Fragile string manipulation
- Doesn't handle nested :is()
- Doesn't handle :is() with multiple arguments
- May break valid CSS

### Investigation

First, determine if this hack is even necessary:

1. **Check selectors crate support**:
   The `selectors` crate we use supports `:is()` pseudo-class.
   We have `parse_is_and_where() -> true` in our `PseudoClassParser`.

2. **Test without the hack**:
   - Comment out `unwrap_is_pseudo()` call
   - Try parsing CSS with `:is()`
   - Check if selector matching works

**Hypothesis**: The hack might be unnecessary if the selectors crate handles it.

### Implementation Plan

#### Step 1: Test Native :is() Support

```rust
#[test]
fn test_is_selector_parsing() {
    let css = ":is(main > article) .toc { color: red; }";
    let stylesheet = parse_stylesheet(css).unwrap();
    assert_eq!(stylesheet.rules.len(), 1);
}

#[test]
fn test_is_selector_matching() {
    let html = r#"
        <main><article><div class="toc">content</div></article></main>
    "#;
    let dom = parse_html(html).unwrap();
    let css = ":is(main > article) .toc { color: red; }";
    let stylesheet = parse_stylesheet(css).unwrap();

    // Find the .toc element and check if rule matches
    // ...
}
```

#### Step 2a: If Native Support Works

If tests pass without `unwrap_is_pseudo()`:
1. Remove the function
2. Remove the call in `parse_stylesheet()`
3. Run full test suite
4. Done!

#### Step 2b: If Native Support Doesn't Work

Debug why:

1. **Selector parsing fails**:
   - Check `PseudoClassParser` implementation
   - May need to implement `parse_non_ts_functional_pseudo_class` for `:is()`

2. **Selector matching fails**:
   - Check `match_non_ts_pseudo_class` in dom.rs
   - May need to add `:is()` matching logic

**Implementing :is() matching** (if needed):
```rust
// In dom.rs, ElementRef impl
fn match_non_ts_pseudo_class(
    &self,
    pseudo: &PseudoClass,
    context: &mut MatchingContext<Self::Impl>,
) -> bool {
    match pseudo {
        // ... existing cases
        PseudoClass::Is(selectors) => {
            // :is() matches if ANY of its argument selectors match
            selectors.iter().any(|sel| {
                matches_selector(sel, self, context)
            })
        }
        // ...
    }
}
```

**Note**: The selectors crate might handle this internally. Check documentation.

### Success Criteria

- [ ] CSS with `:is()` parses correctly
- [ ] Selector matching works for `:is()`
- [ ] `unwrap_is_pseudo()` function deleted
- [ ] Tests pass

---

## Task 4.3: Improve Property Value Parsing

### Current State

`parse_property_value()` in css.rs (after Phase 2: css/properties.rs) handles basic cases but misses many.

### Areas for Improvement

#### Shorthand Properties

Currently, shorthands like `margin: 10px 20px` may not expand properly.

**Need to implement**:
- `margin` → `margin-top/right/bottom/left`
- `padding` → `padding-top/right/bottom/left`
- `border` → `border-width/style/color`
- `background` → `background-color/image/position/size/repeat`
- `font` → `font-family/size/weight/style/line-height`
- `border-radius` → `border-*-radius`

#### Complex Values

- `linear-gradient()`
- `radial-gradient()`
- `calc()`
- `var()` (already partially handled)
- `rgba()`, `hsla()`

### Implementation Priority

**P1** (blocks hack removal):
- `border-radius` shorthand
- Complex selectors

**P2** (nice to have):
- Gradient parsing
- calc() support
- Better shorthand expansion

### This Phase Focus

Focus on what blocks Phase 3 hack removal:

1. **Verify `border-radius` parsing** for Hack 3
2. **Verify shorthand expansion** for Hack 4
3. **Fix any gaps found**

Comprehensive property parsing improvement can be a separate task.

---

## Validation

After completing Phase 4:

```bash
# Tests pass
cargo test

# No hack functions remain
grep -n "unwrap_is_pseudo" src/  # Should return nothing
grep -n "preprocess_media_queries" src/  # Should return nothing

# Media queries work
# (write specific test cases)

# :is() selectors work
# (write specific test cases)
```

---

## Test Cases to Add

### Media Query Tests

```rust
#[test]
fn test_media_query_min_width() {
    let css = "@media (min-width: 1000px) { .foo { color: red; } }";
    let stylesheet = parse_stylesheet(css).unwrap();

    // Wide viewport - rule should apply
    let wide_ctx = MediaContext { viewport_width: 1200.0, ..Default::default() };
    let rules = collect_applicable_rules(&stylesheet, &wide_ctx);
    assert_eq!(rules.len(), 1);

    // Narrow viewport - rule should not apply
    let narrow_ctx = MediaContext { viewport_width: 800.0, ..Default::default() };
    let rules = collect_applicable_rules(&stylesheet, &narrow_ctx);
    assert_eq!(rules.len(), 0);
}

#[test]
fn test_media_query_max_width() {
    let css = "@media (max-width: 600px) { .mobile { display: block; } }";
    // ...
}

#[test]
fn test_media_query_combined() {
    let css = "@media screen and (min-width: 768px) and (max-width: 1024px) { ... }";
    // ...
}

#[test]
fn test_nested_media_queries() {
    let css = "@media (min-width: 500px) { @media (max-width: 1000px) { ... } }";
    // Note: Nested @media is valid CSS
    // ...
}
```

### :is() Selector Tests

```rust
#[test]
fn test_is_simple() {
    let css = ":is(h1, h2, h3) { font-weight: bold; }";
    // Should match h1, h2, or h3
}

#[test]
fn test_is_with_descendant() {
    let css = ":is(main, article) p { margin: 1em; }";
    // Should match p inside main OR article
}

#[test]
fn test_is_nested() {
    let css = ":is(:is(section, article) p, aside span) { color: gray; }";
    // Nested :is()
}
```

---

*Previous Phase: [03-phase3-hacks.md](./03-phase3-hacks.md)*
*Next Phase: [05-phase5-quality.md](./05-phase5-quality.md)*
