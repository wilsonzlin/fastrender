//! CSS Grid Track Parsing
//!
//! This module handles parsing CSS Grid layout values including
//! track definitions, named lines, and grid placement.
//!
//! Reference: CSS Grid Layout Module Level 1
//! <https://www.w3.org/TR/css-grid-1/>

use crate::css::properties::parse_property_value;
use crate::css::types::PropertyValue;
use crate::style::types::GridTrack;
use crate::style::values::Length;
use crate::style::ComputedStyle;
use cssparser::{Parser, ParserInput, Token};
use std::collections::HashMap;

/// Parse grid-template-columns/rows into track list with named lines
///
/// Handles syntax like: `[text-start] 1fr [text-end sidebar-start] 300px [sidebar-end]`
pub fn parse_grid_tracks_with_names(
    tracks_str: &str,
) -> (Vec<GridTrack>, HashMap<String, Vec<usize>>, Vec<Vec<String>>) {
    let ParsedTracks {
        tracks,
        named_lines,
        line_names,
    } = parse_track_list(tracks_str);

    (tracks, named_lines, line_names)
}

/// Parse a single grid line reference (e.g., "text-start", "3", "auto")
pub fn parse_grid_line(value: &str, named_lines: &HashMap<String, Vec<usize>>) -> i32 {
    let value = value.trim();

    // Try parsing as integer first
    if let Ok(n) = value.parse::<i32>() {
        return n;
    }

    // Check if it's "auto"
    if value == "auto" {
        return 0; // 0 means auto-placement in Taffy
    }

    // Try to resolve as named grid line
    if let Some(positions) = named_lines.get(value) {
        if let Some(&pos) = positions.first() {
            // Grid lines are 1-indexed in CSS (line 1 is before track 0)
            return (pos + 1) as i32;
        }
    }

    // Default to auto
    0
}

/// Parse `grid-template-areas` into row/column names, validating rectangular areas.
///
/// Returns `None` on syntax errors or non-rectangular area definitions. The rows are stored with `None`
/// representing an empty cell (`.` in authored CSS).
pub fn parse_grid_template_areas(value: &str) -> Option<Vec<Vec<Option<String>>>> {
    let trimmed = value.trim();
    if trimmed.eq_ignore_ascii_case("none") {
        return Some(Vec::new());
    }

    let mut input = ParserInput::new(trimmed);
    let mut parser = Parser::new(&mut input);
    let mut rows: Vec<Vec<Option<String>>> = Vec::new();

    while !parser.is_exhausted() {
        match parser.next_including_whitespace() {
            Ok(Token::WhiteSpace(_)) => continue,
            Ok(Token::QuotedString(s)) => {
                let cols: Vec<Option<String>> = s
                    .split_whitespace()
                    .map(|name| if name == "." { None } else { Some(name.to_string()) })
                    .collect();
                if cols.is_empty() {
                    return None;
                }
                if let Some(expected) = rows.first().map(|r| r.len()) {
                    if expected != cols.len() {
                        return None;
                    }
                }
                rows.push(cols);
            }
            Ok(_) => return None,
            Err(_) => break,
        }
    }

    if rows.is_empty() {
        return None;
    }

    validate_area_rectangles(&rows).map(|_| rows)
}

/// Validate that each named area forms a rectangle and return area bounds per name.
pub fn validate_area_rectangles(rows: &[Vec<Option<String>>]) -> Option<HashMap<String, (usize, usize, usize, usize)>> {
    let mut bounds: HashMap<String, (usize, usize, usize, usize)> = HashMap::new();

    for (row_idx, row) in rows.iter().enumerate() {
        for (col_idx, cell) in row.iter().enumerate() {
            let Some(name) = cell else { continue };
            let entry = bounds
                .entry(name.clone())
                .or_insert((row_idx, row_idx, col_idx, col_idx));
            let (top, bottom, left, right) = entry;
            *top = (*top).min(row_idx);
            *bottom = (*bottom).max(row_idx);
            *left = (*left).min(col_idx);
            *right = (*right).max(col_idx);
        }
    }

    for (name, (top, bottom, left, right)) in bounds.iter() {
        for r in *top..=*bottom {
            for c in *left..=*right {
                match rows.get(r).and_then(|row| row.get(c)).and_then(|cell| cell.as_ref()) {
                    Some(cell_name) if cell_name == name => {}
                    _ => return None,
                }
            }
        }
    }

    Some(bounds)
}

/// Parsed representation of the `grid-template` shorthand.
pub struct ParsedGridTemplate {
    pub areas: Option<Vec<Vec<Option<String>>>>,
    pub row_tracks: Option<(Vec<GridTrack>, Vec<Vec<String>>)>,
    pub column_tracks: Option<(Vec<GridTrack>, Vec<Vec<String>>)>,
}

/// Parsed representation of the `grid` shorthand.
pub struct ParsedGridShorthand {
    pub template: Option<ParsedGridTemplate>,
    pub auto_rows: Option<Vec<GridTrack>>,
    pub auto_columns: Option<Vec<GridTrack>>,
    pub auto_flow: Option<crate::style::types::GridAutoFlow>,
}

/// Parse the `grid-template` shorthand.
///
/// Supports two forms:
/// 1) `<track-list> / <track-list>` (explicit rows/columns)
/// 2) `<area-rows> [ / <col-tracks> ]`, where area rows are quoted strings with optional
///    per-row track sizes following each string.
pub fn parse_grid_template_shorthand(value: &str) -> Option<ParsedGridTemplate> {
    let value = value.trim();
    if value.eq_ignore_ascii_case("none") {
        return Some(ParsedGridTemplate {
            areas: Some(Vec::new()),
            row_tracks: Some((Vec::new(), Vec::new())),
            column_tracks: Some((Vec::new(), Vec::new())),
        });
    }

    let (main, cols_part) = split_once_unquoted(value, '/');

    let main = main.trim();
    let cols_part = cols_part.and_then(|s| {
        let trimmed = s.trim();
        if trimmed.is_empty() {
            None
        } else {
            Some(trimmed)
        }
    });

    // If there are no area strings, treat as pure track list shorthand.
    if !main.contains('"') {
        // Per spec the track-list form requires both rows and columns separated by a slash.
        let cols_raw = cols_part?;
        let ParsedTracks {
            tracks: row_tracks,
            line_names: row_line_names,
            ..
        } = parse_track_list(main);
        if row_tracks.is_empty() {
            return None;
        }

        let ParsedTracks {
            tracks: col_tracks,
            line_names: col_line_names,
            ..
        } = parse_track_list(cols_raw);
        if col_tracks.is_empty() {
            return None;
        }

        return Some(ParsedGridTemplate {
            areas: None,
            row_tracks: Some((row_tracks, row_line_names)),
            column_tracks: Some((col_tracks, col_line_names)),
        });
    }

    // Area form: parse quoted rows with optional row sizes.
    let (area_rows, row_sizes_raw) = parse_area_rows_with_sizes(main)?;
    let areas = build_area_matrix(&area_rows)?;
    validate_area_rectangles(&areas)?;

    // Row tracks: if row sizes were provided, use them; otherwise default to auto.
    let row_tracks = if row_sizes_raw.iter().any(|s| s.is_some()) {
        let mut tracks = Vec::with_capacity(row_sizes_raw.len());
        for size in &row_sizes_raw {
            let Some(size_str) = size else {
                tracks.push(GridTrack::Auto);
                continue;
            };
            let track = parse_single_grid_track(&size_str)?;
            tracks.push(track);
        }
        Some((tracks, vec![Vec::new(); row_sizes_raw.len() + 1]))
    } else {
        Some((vec![GridTrack::Auto; areas.len()], vec![Vec::new(); areas.len() + 1]))
    };

    // Column tracks: explicit slash wins; otherwise derive auto from area width.
    let column_tracks = if let Some(cols_raw) = cols_part {
        let ParsedTracks { tracks, line_names, .. } = parse_track_list(cols_raw);
        if tracks.is_empty() {
            return None;
        }
        Some((tracks, line_names))
    } else {
        let cols = areas.first().map(|r| r.len()).unwrap_or(0);
        Some((vec![GridTrack::Auto; cols], vec![Vec::new(); cols + 1]))
    };

    Some(ParsedGridTemplate {
        areas: Some(areas),
        row_tracks,
        column_tracks,
    })
}

/// Parse the `grid` shorthand (template or auto-flow forms).
pub fn parse_grid_shorthand(value: &str) -> Option<ParsedGridShorthand> {
    let value = value.trim();
    if value.eq_ignore_ascii_case("none") {
        return Some(reset_grid_shorthand());
    }

    if !value.contains("auto-flow") {
        return parse_grid_template_shorthand(value).map(|template| ParsedGridShorthand {
            template: Some(template),
            auto_rows: Some(vec![GridTrack::Auto]),
            auto_columns: Some(vec![GridTrack::Auto]),
            auto_flow: Some(crate::style::types::GridAutoFlow::Row),
        });
    }

    // Auto-flow form: either left or right of the slash contains auto-flow
    let (left, right_opt) = split_once_unquoted(value, '/');
    let left = left.trim();
    let right = right_opt.map(str::trim);

    let mut auto_rows: Option<Vec<GridTrack>> = None;
    let mut auto_cols: Option<Vec<GridTrack>> = None;
    let mut auto_flow: Option<crate::style::types::GridAutoFlow> = None;

    let parse_auto_flow_tokens = |tokens: &str| -> (Option<crate::style::types::GridAutoFlow>, Option<String>) {
        let lower = tokens.to_ascii_lowercase();
        if !lower.contains("auto-flow") {
            return (None, None);
        }
        let dense = lower.contains("dense");
        let primary = if lower.contains("column") { "column" } else { "row" };
        let flow = match (primary, dense) {
            ("row", false) => crate::style::types::GridAutoFlow::Row,
            ("row", true) => crate::style::types::GridAutoFlow::RowDense,
            ("column", false) => crate::style::types::GridAutoFlow::Column,
            ("column", true) => crate::style::types::GridAutoFlow::ColumnDense,
            _ => crate::style::types::GridAutoFlow::Row,
        };
        // Strip the auto-flow keywords to leave a potential track list
        let remainder = tokens
            .replace("auto-flow", "")
            .replace("Auto-Flow", "")
            .replace("AUTO-FLOW", "")
            .replace("dense", "")
            .replace("DENSE", "")
            .trim()
            .to_string();
        let remainder = if remainder.is_empty() { None } else { Some(remainder) };
        (Some(flow), remainder)
    };

    let left_has_flow = left.to_ascii_lowercase().contains("auto-flow");
    let right_has_flow = right
        .as_ref()
        .is_some_and(|r| r.to_ascii_lowercase().contains("auto-flow"));

    if left_has_flow {
        let (flow_parsed, remainder) = parse_auto_flow_tokens(left);
        if let Some(flow) = flow_parsed {
            auto_flow = Some(flow);
        }
        if let Some(rem) = remainder {
            let ParsedTracks { tracks, .. } = parse_track_list(&rem);
            if !tracks.is_empty() {
                auto_rows = Some(tracks);
            }
        }
        if let Some(r) = right {
            let ParsedTracks { tracks, .. } = parse_track_list(r);
            if !tracks.is_empty() {
                auto_cols = Some(tracks);
            }
        }
    } else if right_has_flow {
        let (flow_parsed, remainder) = parse_auto_flow_tokens(right.unwrap());
        if let Some(flow) = flow_parsed {
            auto_flow = Some(flow);
        }
        if let Some(rem) = remainder {
            let ParsedTracks { tracks, .. } = parse_track_list(&rem);
            if !tracks.is_empty() {
                auto_cols = Some(tracks);
            }
        }
        let ParsedTracks { tracks, .. } = parse_track_list(left);
        if !tracks.is_empty() {
            auto_rows = Some(tracks);
        }
    } else {
        return None;
    }

    Some(ParsedGridShorthand {
        template: Some(empty_template_reset()),
        auto_rows: auto_rows.or(Some(vec![GridTrack::Auto])),
        auto_columns: auto_cols.or(Some(vec![GridTrack::Auto])),
        auto_flow: auto_flow.or(Some(crate::style::types::GridAutoFlow::Row)),
    })
}

fn reset_grid_shorthand() -> ParsedGridShorthand {
    ParsedGridShorthand {
        template: Some(empty_template_reset()),
        auto_rows: Some(vec![GridTrack::Auto]),
        auto_columns: Some(vec![GridTrack::Auto]),
        auto_flow: Some(crate::style::types::GridAutoFlow::Row),
    }
}

fn empty_template_reset() -> ParsedGridTemplate {
    ParsedGridTemplate {
        areas: Some(Vec::new()),
        row_tracks: Some((Vec::new(), Vec::new())),
        column_tracks: Some((Vec::new(), Vec::new())),
    }
}

fn parse_area_rows_with_sizes(input: &str) -> Option<(Vec<String>, Vec<Option<String>>)> {
    let mut rows = Vec::new();
    let mut row_sizes = Vec::new();
    let mut i = 0;
    let bytes = input.as_bytes();
    while i < bytes.len() {
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        if i >= bytes.len() {
            break;
        }
        if bytes[i] != b'"' {
            return None;
        }
        i += 1;
        let start = i;
        while i < bytes.len() && bytes[i] != b'"' {
            i += 1;
        }
        if i >= bytes.len() {
            return None;
        }
        let row_str = &input[start..i];
        rows.push(row_str.to_string());
        i += 1; // skip closing quote

        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        // Capture optional row size until next quote or slash
        let size_start = i;
        while i < bytes.len() && bytes[i] != b'"' && bytes[i] != b'/' {
            i += 1;
        }
        let size = input[size_start..i].trim();
        if size.is_empty() {
            row_sizes.push(None);
        } else {
            row_sizes.push(Some(size.to_string()));
        }
    }

    if rows.is_empty() {
        None
    } else {
        Some((rows, row_sizes))
    }
}

fn build_area_matrix(rows: &[String]) -> Option<Vec<Vec<Option<String>>>> {
    let mut matrix = Vec::with_capacity(rows.len());
    let mut expected_cols: Option<usize> = None;
    for row in rows {
        let cols: Vec<Option<String>> = row
            .split_whitespace()
            .map(|name| if name == "." { None } else { Some(name.to_string()) })
            .collect();
        if cols.is_empty() {
            return None;
        }
        if let Some(exp) = expected_cols {
            if cols.len() != exp {
                return None;
            }
        } else {
            expected_cols = Some(cols.len());
        }
        matrix.push(cols);
    }
    Some(matrix)
}

fn split_once_unquoted(input: &str, delim: char) -> (&str, Option<&str>) {
    let mut in_quote = false;
    for (idx, ch) in input.char_indices() {
        match ch {
            '"' => in_quote = !in_quote,
            d if d == delim && !in_quote => {
                let (left, right) = input.split_at(idx);
                return (left, Some(&right[delim.len_utf8()..]));
            }
            _ => {}
        }
    }
    (input, None)
}

/// Finalize grid placement: keep raw values for Taffy to resolve (named/numeric/nth)
pub fn finalize_grid_placement(_styles: &mut ComputedStyle) {}

/// Parse grid-column or grid-row placement (e.g., "text", "1 / 3", "auto")
pub fn parse_grid_line_placement(value: &str, named_lines: &HashMap<String, Vec<usize>>) -> (i32, i32) {
    let value = value.trim();

    // Check if it contains a slash (explicit start / end)
    if let Some(slash_pos) = value.find('/') {
        let start_str = value[..slash_pos].trim();
        let end_str = value[slash_pos + 1..].trim();
        let start = parse_grid_line(start_str, named_lines);
        let end = parse_grid_line(end_str, named_lines);
        return (start, end);
    }

    // Single numeric value - treat as "start / span 1" (e.g., "2" means grid-row: 2 / 3)
    if let Ok(n) = value.parse::<i32>() {
        return (n, n + 1);
    }

    // Single value - check if it's a named area (e.g., "text")
    // Named areas should expand to area-start / area-end
    let start_name = format!("{}-start", value);
    let end_name = format!("{}-end", value);

    let start = if let Some(positions) = named_lines.get(&start_name) {
        if let Some(&pos) = positions.first() {
            (pos + 1) as i32
        } else {
            parse_grid_line(value, named_lines)
        }
    } else {
        parse_grid_line(value, named_lines)
    };

    let end = if let Some(positions) = named_lines.get(&end_name) {
        if let Some(&pos) = positions.first() {
            (pos + 1) as i32
        } else {
            0 // auto
        }
    } else {
        0 // auto
    };

    (start, end)
}

/// A parsed track list containing the concrete tracks and named line offsets.
#[derive(Default)]
pub(crate) struct ParsedTracks {
    pub tracks: Vec<GridTrack>,
    pub named_lines: HashMap<String, Vec<usize>>,
    pub line_names: Vec<Vec<String>>,
}

impl ParsedTracks {
    fn with_track(track: GridTrack) -> Self {
        Self {
            tracks: vec![track],
            named_lines: HashMap::new(),
            line_names: vec![Vec::new(), Vec::new()],
        }
    }
}

/// Lightweight parser for grid track lists
struct TrackListParser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> TrackListParser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn remaining(&self) -> &'a str {
        &self.input[self.pos..]
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.advance_char();
            } else {
                break;
            }
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
        let mut iter = self.remaining().char_indices();
        if let Some((idx, ch)) = iter.next() {
            self.pos += idx + ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }

    fn starts_with_ident(&self, ident: &str) -> bool {
        let rem = self.remaining();
        rem.len() >= ident.len() && rem[..ident.len()].eq_ignore_ascii_case(ident)
    }

    fn consume_bracketed_names(&mut self) -> Option<Vec<String>> {
        if self.peek_char()? != '[' {
            return None;
        }
        // Skip '['
        self.advance_char();
        let start = self.pos;
        let mut i = self.pos;
        while i < self.input.len() {
            let ch = self.input[i..].chars().next().unwrap();
            let ch_len = ch.len_utf8();
            if ch == ']' {
                let names_raw = &self.input[start..i];
                self.pos = i + ch_len;
                let names = names_raw
                    .split_whitespace()
                    .filter(|n| !n.is_empty())
                    .map(|n| n.to_string())
                    .collect::<Vec<_>>();
                return Some(names);
            }
            i += ch_len;
        }
        None
    }

    fn consume_function_arguments(&mut self, name: &str) -> Option<String> {
        if !self.starts_with_ident(name) {
            return None;
        }
        let after_name = self.pos + name.len();
        let mut chars = self.input[after_name..].chars();
        if chars.next()? != '(' {
            return None;
        }
        let mut i = after_name + 1; // position after '('
        let start = i;
        let mut depth = 1;
        while i < self.input.len() {
            let ch = self.input[i..].chars().next().unwrap();
            let ch_len = ch.len_utf8();
            match ch {
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if depth == 0 {
                        let inner = self.input[start..i].to_string();
                        self.pos = i + ch_len;
                        return Some(inner);
                    }
                }
                _ => {}
            }
            i += ch_len;
        }
        None
    }

    /// Consumes a single track token, stopping at top-level whitespace or '['
    fn consume_track_token(&mut self) -> Option<String> {
        let start = self.pos;
        let mut depth: usize = 0;
        let mut i = self.pos;
        while i < self.input.len() {
            let ch = self.input[i..].chars().next().unwrap();
            let ch_len = ch.len_utf8();
            match ch {
                '(' => depth += 1,
                ')' => depth = depth.saturating_sub(1),
                '[' if depth == 0 => {
                    let token = self.input[start..i].trim();
                    self.pos = i;
                    return if token.is_empty() {
                        None
                    } else {
                        Some(token.to_string())
                    };
                }
                _ if ch.is_whitespace() && depth == 0 => {
                    let token = self.input[start..i].trim();
                    self.pos = i + ch_len;
                    return if token.is_empty() {
                        None
                    } else {
                        Some(token.to_string())
                    };
                }
                _ => {}
            }
            i += ch_len;
        }

        self.pos = self.input.len();
        let token = self.input[start..].trim();
        if token.is_empty() {
            None
        } else {
            Some(token.to_string())
        }
    }

    fn parse_repeat(&mut self) -> Option<ParsedTracks> {
        let inner = self.consume_function_arguments("repeat")?;
        let (count_str, pattern_str) = split_once_comma(&inner)?;
        let count_str = count_str.trim();
        let pattern = parse_track_list(pattern_str);
        if pattern.tracks.is_empty() {
            return None;
        }

        if count_str.eq_ignore_ascii_case("auto-fill") {
            return Some(ParsedTracks {
                tracks: vec![GridTrack::RepeatAutoFill {
                    tracks: pattern.tracks,
                    line_names: pattern.line_names.clone(),
                }],
                named_lines: pattern.named_lines,
                line_names: pattern.line_names,
            });
        }
        if count_str.eq_ignore_ascii_case("auto-fit") {
            return Some(ParsedTracks {
                tracks: vec![GridTrack::RepeatAutoFit {
                    tracks: pattern.tracks,
                    line_names: pattern.line_names.clone(),
                }],
                named_lines: pattern.named_lines,
                line_names: pattern.line_names,
            });
        }

        let repeat_count: usize = count_str.parse().ok()?;
        let mut tracks = Vec::new();
        let mut named_lines = HashMap::new();
        let mut line_names: Vec<Vec<String>> = vec![Vec::new()];
        for _ in 0..repeat_count {
            let offset = tracks.len();
            tracks.extend(pattern.tracks.iter().cloned());
            for (name, positions) in pattern.named_lines.iter() {
                let entry = named_lines.entry(name.clone()).or_insert_with(Vec::new);
                entry.extend(positions.iter().map(|p| p + offset));
            }
            // repeat line names: merge first entry with current line, then append rest
            if let Some(first) = pattern.line_names.first() {
                line_names.last_mut().unwrap().extend(first.iter().cloned());
            }
            for names in pattern.line_names.iter().skip(1) {
                line_names.push(names.clone());
            }
        }

        if line_names.len() < tracks.len() + 1 {
            line_names.resize(tracks.len() + 1, Vec::new());
        }

        Some(ParsedTracks {
            tracks,
            named_lines,
            line_names,
        })
    }

    fn parse_component(&mut self) -> Option<ParsedTracks> {
        if self.starts_with_ident("repeat") {
            if let Some(repeated) = self.parse_repeat() {
                return Some(repeated);
            }
        }

        let token = self.consume_track_token()?;
        parse_single_grid_track(&token).map(ParsedTracks::with_track)
    }
}

fn split_once_comma(input: &str) -> Option<(&str, &str)> {
    let mut depth: usize = 0;
    let mut i = 0;
    while i < input.len() {
        let ch = input[i..].chars().next().unwrap();
        let ch_len = ch.len_utf8();
        match ch {
            '(' => depth += 1,
            ')' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                let first = input[..i].trim();
                let second = input[i + ch_len..].trim();
                return Some((first, second));
            }
            _ => {}
        }
        i += ch_len;
    }
    None
}

pub(crate) fn parse_track_list(input: &str) -> ParsedTracks {
    let mut parser = TrackListParser::new(input);
    let mut tracks = Vec::new();
    let mut named_lines: HashMap<String, Vec<usize>> = HashMap::new();
    let mut line_names: Vec<Vec<String>> = vec![Vec::new()];

    while !parser.is_eof() {
        parser.skip_whitespace();
        while let Some(names) = parser.consume_bracketed_names() {
            let line = tracks.len();
            if line_names.len() <= line {
                line_names.resize(line + 1, Vec::new());
            }
            for name in names {
                line_names[line].push(name.clone());
                named_lines.entry(name).or_insert_with(Vec::new).push(line);
            }
            parser.skip_whitespace();
        }

        parser.skip_whitespace();
        if parser.is_eof() {
            break;
        }

        let parsed = match parser.parse_component() {
            Some(component) => component,
            None => break,
        };

        let offset = tracks.len();
        if !parsed.line_names.is_empty() {
            // Merge first line names into current line (before the first track of the parsed chunk)
            if let Some(first) = parsed.line_names.first() {
                line_names.last_mut().unwrap().extend(first.iter().cloned());
            }
            // Append remaining line names
            for names in parsed.line_names.iter().skip(1) {
                line_names.push(names.clone());
            }
        } else {
            // Ensure we have a slot for subsequent lines
            line_names.resize(tracks.len().saturating_add(parsed.tracks.len()) + 1, Vec::new());
        }

        for (name, positions) in parsed.named_lines {
            let entry = named_lines.entry(name).or_insert_with(Vec::new);
            entry.extend(positions.into_iter().map(|p| p + offset));
        }
        tracks.extend(parsed.tracks);

        parser.skip_whitespace();
        while let Some(names) = parser.consume_bracketed_names() {
            let line = tracks.len();
            if line_names.len() <= line {
                line_names.resize(line + 1, Vec::new());
            }
            for name in names {
                line_names[line].push(name.clone());
                named_lines.entry(name).or_insert_with(Vec::new).push(line);
            }
            parser.skip_whitespace();
        }
    }

    if line_names.len() < tracks.len() + 1 {
        line_names.resize(tracks.len() + 1, Vec::new());
    }

    ParsedTracks {
        tracks,
        named_lines,
        line_names,
    }
}

/// Parse a single grid track value
pub(crate) fn parse_single_grid_track(track_str: &str) -> Option<GridTrack> {
    let track_str = track_str.trim();
    if track_str.is_empty() {
        return None;
    }

    let lower = track_str.to_ascii_lowercase();

    if let Some(inner) = lower.strip_prefix("minmax(").and_then(|s| s.strip_suffix(')')) {
        let (min_str, max_str) = split_once_comma(inner)?;
        let min = parse_track_breadth(min_str)?;
        let max = parse_track_breadth(max_str)?;
        return Some(GridTrack::MinMax(Box::new(min), Box::new(max)));
    }

    if let Some(inner) = lower.strip_prefix("fit-content(").and_then(|s| s.strip_suffix(')')) {
        let len = parse_length_value(inner)?;
        return Some(GridTrack::FitContent(len));
    }

    if lower == "min-content" {
        return Some(GridTrack::MinContent);
    }
    if lower == "max-content" {
        return Some(GridTrack::MaxContent);
    }
    if lower == "auto" {
        return Some(GridTrack::Auto);
    }

    if let Some(val_str) = lower.strip_suffix("fr") {
        if let Ok(val) = val_str.trim().parse::<f32>() {
            return Some(GridTrack::Fr(val));
        }
    }

    if let Some(length) = parse_length_value(&lower) {
        return Some(GridTrack::Length(length));
    }

    None
}

fn parse_track_breadth(value: &str) -> Option<GridTrack> {
    let trimmed = value.trim();
    let lower = trimmed.to_ascii_lowercase();
    if lower == "auto" {
        return Some(GridTrack::Auto);
    }
    if lower == "min-content" {
        return Some(GridTrack::MinContent);
    }
    if lower == "max-content" {
        return Some(GridTrack::MaxContent);
    }
    if let Some(inner) = lower.strip_prefix("fit-content(").and_then(|s| s.strip_suffix(')')) {
        return parse_length_value(inner).map(GridTrack::FitContent);
    }
    if let Some(val_str) = lower.strip_suffix("fr") {
        if let Ok(val) = val_str.trim().parse::<f32>() {
            return Some(GridTrack::Fr(val));
        }
    }

    parse_length_value(&lower).map(GridTrack::Length)
}

fn parse_length_value(raw: &str) -> Option<Length> {
    match parse_property_value("", raw)? {
        PropertyValue::Length(l) => Some(l),
        PropertyValue::Percentage(p) => Some(Length::percent(p)),
        PropertyValue::Number(n) if n == 0.0 => Some(Length::px(n)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_minmax_and_content_keywords() {
        let (tracks, _, _) = parse_grid_tracks_with_names("minmax(10px, 1fr) max-content min-content");
        assert_eq!(tracks.len(), 3);
        match &tracks[0] {
            GridTrack::MinMax(min, max) => {
                assert!(matches!(**min, GridTrack::Length(_)));
                assert!(matches!(**max, GridTrack::Fr(_)));
            }
            other => panic!("expected minmax track, got {:?}", other),
        }
        assert!(matches!(tracks[1], GridTrack::MaxContent));
        assert!(matches!(tracks[2], GridTrack::MinContent));
    }

    #[test]
    fn parses_fit_content_and_percentages() {
        let (tracks, _, _) = parse_grid_tracks_with_names("fit-content(50%) 25%");
        assert_eq!(tracks.len(), 2);
        match &tracks[0] {
            GridTrack::FitContent(len) => assert_eq!(len.unit, crate::style::values::LengthUnit::Percent),
            other => panic!("expected fit-content track, got {:?}", other),
        }
        match &tracks[1] {
            GridTrack::Length(len) => assert_eq!(len.unit, crate::style::values::LengthUnit::Percent),
            other => panic!("expected percent length, got {:?}", other),
        }
    }

    #[test]
    fn parses_repeat_with_line_names() {
        let (tracks, names, line_names) =
            parse_grid_tracks_with_names("[a] repeat(2, [b] 10px [c] minmax(0, 1fr)) [d]");
        assert_eq!(tracks.len(), 4);
        assert_eq!(names.get("a"), Some(&vec![0]));
        assert_eq!(names.get("b"), Some(&vec![0, 2]));
        assert_eq!(names.get("c"), Some(&vec![1, 3]));
        assert_eq!(names.get("d"), Some(&vec![4]));
        assert_eq!(line_names.len(), 5);
        assert!(line_names[0].contains(&"a".to_string()));
        assert!(line_names[0].contains(&"b".to_string())); // first repeat merges into current line
        assert!(line_names[1].contains(&"c".to_string()));
        assert!(line_names[2].contains(&"b".to_string()));
    }

    #[test]
    fn parses_auto_fit_and_fill_repeat() {
        let (tracks_fit, names_fit, _) = parse_grid_tracks_with_names("repeat(auto-fit, 100px 1fr)");
        assert_eq!(tracks_fit.len(), 1);
        assert!(matches!(tracks_fit[0], GridTrack::RepeatAutoFit { .. }));
        assert!(names_fit.is_empty());

        let (tracks_fill, names_fill, _) = parse_grid_tracks_with_names("repeat(auto-fill, minmax(0, 1fr))");
        assert_eq!(tracks_fill.len(), 1);
        assert!(matches!(tracks_fill[0], GridTrack::RepeatAutoFill { .. }));
        assert!(names_fill.is_empty());
    }

    #[test]
    fn auto_fit_repeat_keeps_named_lines() {
        let (_tracks, names, line_names) = parse_grid_tracks_with_names("repeat(auto-fit, [col-start] 10px [col-end])");
        assert_eq!(names.get("col-start"), Some(&vec![0]));
        assert_eq!(names.get("col-end"), Some(&vec![1]));
        assert!(line_names[0].contains(&"col-start".to_string()));
        assert!(line_names[1].contains(&"col-end".to_string()));
        assert_eq!(parse_grid_line("col-start", &names), 1);
        assert_eq!(parse_grid_line("col-end", &names), 2);
    }

    #[test]
    fn auto_fill_repeat_keeps_named_lines() {
        let (_tracks, names, line_names) = parse_grid_tracks_with_names("repeat(auto-fill, [a] 20px [b c])");
        assert_eq!(names.get("a"), Some(&vec![0]));
        assert_eq!(names.get("b"), Some(&vec![1]));
        assert_eq!(names.get("c"), Some(&vec![1]));
        assert!(line_names[0].contains(&"a".to_string()));
        assert!(line_names[1].contains(&"b".to_string()));
        assert!(line_names[1].contains(&"c".to_string()));
        assert_eq!(parse_grid_line("a", &names), 1);
        assert_eq!(parse_grid_line("b", &names), 2);
        assert_eq!(parse_grid_line("c", &names), 2);
    }

    #[test]
    fn finalize_leaves_named_tokens_for_layout() {
        let mut style = ComputedStyle::default();
        style.grid_column_raw = Some("foo / span 2".to_string());
        finalize_grid_placement(&mut style);
        assert_eq!(style.grid_column_start, 0);
        assert_eq!(style.grid_column_end, 0);
    }

    #[test]
    fn finalize_skips_auto_repeat_resolution() {
        let mut style = ComputedStyle::default();
        style.grid_template_columns = vec![GridTrack::RepeatAutoFit {
            tracks: vec![GridTrack::Length(Length::px(50.0))],
            line_names: vec![vec!["a".into()], vec!["b".into()]],
        }];
        style.grid_column_raw = Some("1 / 2".to_string());
        finalize_grid_placement(&mut style);
        // Auto-repeat present: leave resolution to layout
        assert_eq!(style.grid_column_start, 0);
        assert_eq!(style.grid_column_end, 0);
        assert!(style.grid_column_raw.is_some());
    }

    #[test]
    fn parses_grid_template_areas_rectangles() {
        let areas = parse_grid_template_areas("\"a a\" \"b .\"").expect("should parse");
        assert_eq!(areas.len(), 2);
        assert_eq!(areas[0].len(), 2);
        assert_eq!(areas[1].len(), 2);
        assert_eq!(areas[0][0], Some("a".into()));
        assert_eq!(areas[1][0], Some("b".into()));
        assert_eq!(areas[1][1], None);
    }

    #[test]
    fn rejects_mismatched_columns_or_non_rectangles() {
        assert!(parse_grid_template_areas("\"a\" \"a a\"").is_none());
        // Non-rectangular area usage of "a"
        assert!(parse_grid_template_areas("\"a b\" \"a a\"").is_none());
    }

    #[test]
    fn grid_template_areas_populates_tracks_when_empty() {
        let mut styles = ComputedStyle::default();
        let value = PropertyValue::Keyword("\"a b\" \"a b\"".into());
        match &value {
            PropertyValue::Keyword(kw) | PropertyValue::String(kw) => {
                if let Some(areas) = parse_grid_template_areas(kw) {
                    let row_count = areas.len();
                    let col_count = areas.first().map(|r| r.len()).unwrap_or(0);
                    if col_count != 0 {
                        styles.grid_template_areas = areas;
                        if styles.grid_template_columns.is_empty() {
                            styles.grid_template_columns = vec![GridTrack::Auto; col_count];
                        }
                        if styles.grid_template_rows.is_empty() {
                            styles.grid_template_rows = vec![GridTrack::Auto; row_count];
                        }
                    }
                }
            }
            _ => {}
        }
        assert_eq!(styles.grid_template_columns.len(), 2);
        assert_eq!(styles.grid_template_rows.len(), 2);
    }

    #[test]
    fn grid_template_shorthand_tracks_only() {
        let parsed = parse_grid_template_shorthand("100px auto / 1fr 2fr").expect("should parse");
        let (rows, _) = parsed.row_tracks.expect("rows");
        let (cols, _) = parsed.column_tracks.expect("cols");
        assert_eq!(rows.len(), 2);
        assert!(matches!(rows[0], GridTrack::Length(_)));
        assert!(matches!(rows[1], GridTrack::Auto));
        assert_eq!(cols.len(), 2);
        assert!(matches!(cols[0], GridTrack::Fr(_)));
    }

    #[test]
    fn grid_template_shorthand_areas_with_sizes_and_cols() {
        let parsed = parse_grid_template_shorthand("\"a b\" 40px \"c d\" 50px / 20px 30px").expect("should parse");
        let areas = parsed.areas.expect("areas");
        assert_eq!(areas.len(), 2);
        assert_eq!(areas[0][0], Some("a".into()));
        let (rows, _) = parsed.row_tracks.expect("rows");
        assert_eq!(rows.len(), 2);
        assert!(matches!(rows[0], GridTrack::Length(_)));
        let (cols, _) = parsed.column_tracks.expect("cols");
        assert_eq!(cols.len(), 2);
        assert!(matches!(cols[0], GridTrack::Length(_)));
    }

    #[test]
    fn grid_template_shorthand_invalid_without_cols() {
        assert!(parse_grid_template_shorthand("100px auto").is_none());
    }

    #[test]
    fn grid_template_shorthand_none_resets() {
        let parsed = parse_grid_template_shorthand("none").expect("should parse");
        assert!(parsed.areas.as_ref().unwrap().is_empty());
        assert!(parsed.row_tracks.as_ref().unwrap().0.is_empty());
        assert!(parsed.column_tracks.as_ref().unwrap().0.is_empty());
    }
}
