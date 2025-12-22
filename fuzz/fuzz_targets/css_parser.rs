#![no_main]

use arbitrary::Arbitrary;
use fastrender::css::parser::{parse_declarations, parse_stylesheet_with_errors};
use libfuzzer_sys::fuzz_target;

const MAX_LEN: usize = 32 * 1024;

#[derive(Arbitrary, Debug)]
struct CssParserInput {
  raw_bytes: Vec<u8>,
  unicode: String,
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
  let mut out = String::new();
  for ch in s.chars().take(MAX_LEN) {
    out.push(ch);
  }
  out
}

fuzz_target!(|input: CssParserInput| {
  let raw = lossy_truncate(&input.raw_bytes);
  let unicode = truncate_str(&input.unicode);

  let _ = parse_stylesheet_with_errors(&raw);
  let _ = parse_stylesheet_with_errors(&unicode);

  let _ = parse_declarations(&raw);
  let _ = parse_declarations(&unicode);
});
