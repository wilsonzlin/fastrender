use fastrender::style::media::MediaQuery;

#[test]
fn media_width_rejects_calc() {
  assert!(MediaQuery::parse("(min-width: calc(10px + 10px))").is_err());
  assert!(MediaQuery::parse("(max-height: calc(50vh + 20px))").is_err());
}

#[test]
fn media_range_rejects_calc_values() {
  assert!(MediaQuery::parse("(calc(10px) < width < 800px)").is_err());
  assert!(MediaQuery::parse("(400px < height < calc(800px))").is_err());
}
