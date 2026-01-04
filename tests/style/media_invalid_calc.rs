use fastrender::style::media::MediaQuery;

#[test]
fn media_width_accepts_calc() {
  assert!(MediaQuery::parse("(min-width: calc(10px + 10px))").is_ok());
  assert!(MediaQuery::parse("(max-height: calc(50vh + 20px))").is_ok());
}

#[test]
fn media_range_accepts_calc_values() {
  assert!(MediaQuery::parse("(calc(10px) < width < 800px)").is_ok());
  assert!(MediaQuery::parse("(400px < height < calc(800px))").is_ok());
}
