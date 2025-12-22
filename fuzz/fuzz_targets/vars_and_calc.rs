#![no_main]

use arbitrary::Arbitrary;
use fastrender::css::properties::parse_property_value;
use fastrender::css::types::PropertyValue;
use fastrender::style::var_resolution::{
  resolve_var, resolve_var_for_property, resolve_var_with_depth,
};
use libfuzzer_sys::fuzz_target;
use std::collections::HashMap;

const MAX_LEN: usize = 8 * 1024;
const CALC_PROPERTIES: &[&str] = &[
  "width",
  "height",
  "left",
  "top",
  "margin",
  "padding",
  "border-radius",
  "transform",
  "line-height",
];

#[derive(Arbitrary, Debug)]
struct VarCalcCase {
  property_name: String,
  value_bytes: Vec<u8>,
  calc_fragment: String,
  custom_properties: Vec<(String, String)>,
}

fn lossy_truncate(bytes: &[u8]) -> String {
  let slice = if bytes.len() > MAX_LEN {
    &bytes[..MAX_LEN]
  } else {
    bytes
  };
  String::from_utf8_lossy(slice).into_owned()
}

fn truncate_str(s: &str) -> String {
  s.chars().take(MAX_LEN).collect()
}

fn sanitize_custom_name(name: &str) -> String {
  let filtered: String = name
    .chars()
    .filter(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '_' | ':'))
    .take(48)
    .collect();
  if filtered.is_empty() {
    "fuzz".to_string()
  } else {
    filtered
  }
}

fn normalized_property(input: &str) -> String {
  let trimmed = input.trim();
  if trimmed.starts_with("--") && trimmed.len() < 64 {
    return trimmed.to_string();
  }

  let hash = trimmed
    .bytes()
    .fold(0usize, |acc, b| acc.wrapping_mul(31).wrapping_add(b as usize));
  let idx = hash % CALC_PROPERTIES.len();
  CALC_PROPERTIES[idx].to_string()
}

fn pick_calc_property(seed_a: &str, seed_b: &str) -> &'static str {
  let hash = seed_a
    .bytes()
    .chain(seed_b.bytes())
    .fold(0usize, |acc, b| acc.wrapping_add(b as usize));
  let idx = hash % CALC_PROPERTIES.len();
  CALC_PROPERTIES[idx]
}

fn build_custom_properties(entries: Vec<(String, String)>) -> HashMap<String, String> {
  let mut map = HashMap::new();
  for (name, value) in entries.into_iter().take(16) {
    let key = if name.trim_start().starts_with("--") {
      name.trim().to_string()
    } else {
      format!("--{}", sanitize_custom_name(&name))
    };

    let val = truncate_str(&value);
    map.entry(key).or_insert(val);
  }
  map
}

fuzz_target!(|case: VarCalcCase| {
  let property = normalized_property(&case.property_name);
  let value = lossy_truncate(&case.value_bytes);
  let calc_fragment = truncate_str(&case.calc_fragment);
  let calc_expr = format!("calc({})", calc_fragment);
  let custom_props = build_custom_properties(case.custom_properties);
  let custom_name = custom_props
    .keys()
    .next()
    .cloned()
    .unwrap_or_else(|| "--fuzz-var".to_string());
  let var_expr = format!("var({}, {})", custom_name, value);

  let parsed_value = parse_property_value(&property, &value)
    .unwrap_or_else(|| PropertyValue::Keyword(value.clone()));

  let calc_prop = pick_calc_property(&property, &calc_expr);
  let calc_value = parse_property_value(calc_prop, &calc_expr)
    .unwrap_or_else(|| PropertyValue::Keyword(calc_expr.clone()));

  let parsed_var = parse_property_value(&property, &var_expr)
    .unwrap_or_else(|| PropertyValue::Keyword(var_expr.clone()));

  let _ = resolve_var(&parsed_value, &custom_props);
  let _ = resolve_var_for_property(&parsed_value, &custom_props, &property);
  let _ = resolve_var_with_depth(&calc_value, &custom_props, 0);
  let _ = resolve_var_for_property(&parsed_var, &custom_props, &property);
  let _ = resolve_var_for_property(&calc_value, &custom_props, calc_prop);
});
