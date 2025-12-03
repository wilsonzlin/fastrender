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
use crate::style::ComputedStyle;
use std::collections::HashMap;

/// Parse grid-template-columns/rows into track list with named lines
///
/// Handles syntax like: `[text-start] 1fr [text-end sidebar-start] 300px [sidebar-end]`
pub fn parse_grid_tracks_with_names(tracks_str: &str) -> (Vec<GridTrack>, HashMap<String, Vec<usize>>) {
    let mut tracks = Vec::new();
    let mut named_lines: HashMap<String, Vec<usize>> = HashMap::new();
    let mut in_brackets = false;
    let mut in_parens = 0;
    let mut current_token = String::new();
    let mut bracket_content = String::new();

    // Parse character by character to handle brackets, functions
    for ch in tracks_str.chars() {
        match ch {
            '[' if in_parens == 0 => {
                // Start of grid line name
                in_brackets = true;
                bracket_content.clear();
                if !current_token.trim().is_empty() {
                    // Process accumulated token before bracket
                    process_track_token(&current_token, &mut tracks);
                    current_token.clear();
                }
            }
            ']' if in_parens == 0 => {
                // End of grid line name - store it
                let line_position = tracks.len(); // Line is before this track
                for name in bracket_content.split_whitespace() {
                    named_lines
                        .entry(name.to_string())
                        .or_insert_with(Vec::new)
                        .push(line_position);
                }
                in_brackets = false;
                bracket_content.clear();
                current_token.clear();
            }
            '(' => {
                in_parens += 1;
                current_token.push(ch);
            }
            ')' => {
                in_parens -= 1;
                current_token.push(ch);
                // If we just closed a function, process it
                if in_parens == 0 && !current_token.trim().is_empty() {
                    process_track_token(&current_token, &mut tracks);
                    current_token.clear();
                }
            }
            ' ' if !in_brackets && in_parens == 0 => {
                // Whitespace outside brackets and functions - end of token
                if !current_token.trim().is_empty() {
                    process_track_token(&current_token, &mut tracks);
                    current_token.clear();
                }
            }
            _ if in_brackets => {
                // Inside brackets - accumulate name
                bracket_content.push(ch);
            }
            _ => {
                // Outside brackets - accumulate token
                current_token.push(ch);
            }
        }
    }

    // Handle last token
    if !current_token.trim().is_empty() {
        process_track_token(&current_token, &mut tracks);
    }

    (tracks, named_lines)
}

/// Parse a single grid line reference (e.g., "text-start", "3", "auto")
#[allow(clippy::implicit_hasher)]
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

/// Finalize grid placement by resolving raw grid-column/row values with named lines
pub fn finalize_grid_placement(styles: &mut ComputedStyle) {
    // Resolve grid-column if raw value exists
    if let Some(raw_value) = &styles.grid_column_raw {
        let (start, end) = parse_grid_line_placement(raw_value, &styles.grid_column_names);
        styles.grid_column_start = start;
        styles.grid_column_end = end;
    }

    // Resolve grid-row if raw value exists
    if let Some(raw_value) = &styles.grid_row_raw {
        let (start, end) = parse_grid_line_placement(raw_value, &styles.grid_row_names);
        styles.grid_row_start = start;
        styles.grid_row_end = end;
    }
}

/// Parse grid-column or grid-row placement (e.g., "text", "1 / 3", "auto")
#[allow(clippy::implicit_hasher)]
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

/// Process a track token (could be a single track or a repeat() function)
fn process_track_token(token: &str, tracks: &mut Vec<GridTrack>) {
    let token = token.trim();

    // Check for repeat() function
    if token.starts_with("repeat(") && token.ends_with(')') {
        let inner = &token[7..token.len() - 1]; // Extract "3, 1fr" from "repeat(3, 1fr)"

        // Split by comma to get count and pattern
        if let Some(comma_pos) = inner.find(',') {
            let count_str = inner[..comma_pos].trim();
            let pattern_str = inner[comma_pos + 1..].trim();

            if let Ok(count) = count_str.parse::<usize>() {
                // Parse the pattern track
                if let Some(track) = parse_single_grid_track(pattern_str) {
                    // Add it count times
                    for _ in 0..count {
                        tracks.push(track.clone());
                    }
                }
            }
        }
    } else {
        // Single track
        if let Some(track) = parse_single_grid_track(token) {
            tracks.push(track);
        }
    }
}

/// Parse a single grid track value
fn parse_single_grid_track(track_str: &str) -> Option<GridTrack> {
    let track_str = track_str.trim();

    // Skip empty strings
    if track_str.is_empty() {
        return None;
    }

    // Handle repeat() - for now, just skip it
    if track_str.starts_with("repeat(") {
        // TODO: Properly parse repeat() syntax
        // For now, return None to skip
        return None;
    }

    // Handle var() - these will be already resolved by the time we get here
    // but just in case, try to parse the value
    if track_str.starts_with("var(") {
        // This shouldn't happen if CSS variables are properly resolved
        // but return None to be safe
        return None;
    }

    // Check for fr unit
    if let Some(val_str) = track_str.strip_suffix("fr") {
        if let Ok(val) = val_str.parse::<f32>() {
            return Some(GridTrack::Fr(val));
        }
    }

    // Check for auto
    if track_str == "auto" {
        return Some(GridTrack::Auto);
    }

    // Try to parse as length
    if let Some(len) = parse_property_value("", track_str).and_then(|pv| match pv {
        PropertyValue::Length(l) => Some(l),
        _ => None,
    }) {
        return Some(GridTrack::Length(len));
    }

    None
}
