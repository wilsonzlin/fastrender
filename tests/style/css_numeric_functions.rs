use fastrender::css::properties::{parse_length, parse_property_value};
use fastrender::css::types::{PropertyValue, Transform};
use fastrender::style::values::{Length, LengthUnit};

fn as_number(prop: &str, value: &str) -> Option<f32> {
  match parse_property_value(prop, value)? {
    PropertyValue::Number(n) => Some(n),
    _ => None,
  }
}

#[test]
fn numeric_properties_accept_calc_min_max_clamp() {
  assert_eq!(as_number("opacity", "calc(0.5)"), Some(0.5));
  assert_eq!(as_number("opacity", "min(0.5, 0.7)"), Some(0.5));
  assert_eq!(as_number("opacity", "max(0.3, 0.7)"), Some(0.7));
  assert_eq!(as_number("opacity", "clamp(0.2, 0.4, 0.6)"), Some(0.4));

  assert_eq!(as_number("z-index", "calc(2)"), Some(2.0));
  assert_eq!(as_number("z-index", "min(1, 3)"), Some(1.0));
  assert_eq!(as_number("z-index", "max(5, 3)"), Some(5.0));
  assert_eq!(as_number("z-index", "clamp(1, 4, 6)"), Some(4.0));
}

#[test]
fn trig_and_math_functions_resolve_for_numbers() {
  let sin_val = as_number("opacity", "sin(90deg)").expect("sin parsed");
  assert!((sin_val - 1.0).abs() < 1e-4);

  let cos_val = as_number("opacity", "cos(0.5turn)").expect("cos parsed");
  assert!((cos_val + 1.0).abs() < 1e-4);

  assert_eq!(as_number("opacity", "pow(2, 3)"), Some(8.0));
  assert_eq!(as_number("opacity", "sqrt(9)"), Some(3.0));
  assert_eq!(as_number("opacity", "hypot(3, 4)"), Some(5.0));
  assert_eq!(as_number("opacity", "mod(10, 6)"), Some(4.0));
  assert_eq!(as_number("opacity", "rem(10, 6)"), Some(4.0));
  assert_eq!(as_number("opacity", "clamped(-1)"), Some(0.0));
  assert_eq!(as_number("opacity", "clamped(2)"), Some(1.0));

  let rounded = as_number("opacity", "round(up, 2.1, 2)").unwrap();
  assert!((rounded - 4.0).abs() < 1e-6);
  assert_eq!(as_number("opacity", "round(down, 2.9)"), Some(2.0));

  assert_eq!(as_number("opacity", "sign(-3)"), Some(-1.0));

  let nested = as_number("opacity", "sin(asin(0.5))").unwrap();
  assert!((nested - 0.5).abs() < 1e-6);

  let log_val = as_number("opacity", "log(8, 2)").unwrap();
  assert!((log_val - 3.0).abs() < 1e-6);

  let exp_val = as_number("opacity", "exp(0)").unwrap();
  assert!((exp_val - 1.0).abs() < 1e-6);
}

#[test]
fn math_functions_apply_to_lengths() {
  let len = parse_length("calc(sin(90deg) * 10px + abs(-5px))").expect("length with trig");
  assert_eq!(len, Length::px(15.0));

  let hypot_len = parse_length("calc(hypot(3px, 4px))").expect("hypot length");
  assert_eq!(hypot_len, Length::px(5.0));

  let rounded = parse_length("calc(round(50%) + 10px)").expect("rounded percent");
  let calc = rounded.calc.expect("calc terms");
  let mut percent = None;
  let mut px = None;
  for term in calc.terms() {
    match term.unit {
      LengthUnit::Percent => percent = Some(term.value),
      LengthUnit::Px => px = Some(term.value),
      _ => {}
    }
  }
  assert_eq!(percent, Some(50.0));
  assert_eq!(px, Some(10.0));
}

#[test]
fn math_functions_work_in_gradients_and_transforms() {
  let gradient = parse_property_value(
    "background-image",
    "linear-gradient(calc(90deg - 45deg), red, blue)",
  )
  .expect("gradient parsed");
  match gradient {
    PropertyValue::LinearGradient { angle, .. } => assert!((angle - 45.0).abs() < 0.1),
    _ => panic!("expected linear gradient"),
  }

  let transform_value =
    parse_property_value("transform", "rotate(calc(atan2(1, 1)))").expect("transform value");
  let transforms = match transform_value {
    PropertyValue::Transform(list) => list,
    _ => panic!("expected transform"),
  };
  match transforms.first() {
    Some(Transform::Rotate(angle)) => assert!((angle - 45.0).abs() < 0.1),
    Some(other) => panic!("unexpected transform {:?}", other),
    None => panic!("no transform parsed"),
  }
}
