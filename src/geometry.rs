//! Core geometry types for layout and painting
//!
//! This module provides fundamental geometric primitives used throughout
//! the rendering engine. All units are in CSS pixels unless otherwise noted.
//!
//! # CSS Pixels
//!
//! CSS pixels are logical units defined as 1/96th of an inch. They are
//! independent of device pixels and get scaled by the device pixel ratio.
//!
//! # Coordinate System
//!
//! The coordinate system has its origin at the top-left corner:
//! - Positive X extends to the right
//! - Positive Y extends downward
//!
//! This matches CSS's coordinate system as defined in CSS 2.1 Section 8.3.1.

use std::fmt;

/// A 2D point in CSS pixel space
///
/// Represents a coordinate in the rendering surface's coordinate system.
/// The origin (0, 0) is at the top-left corner.
///
/// # Examples
///
/// ```
/// use fastrender::Point;
///
/// let p1 = Point::new(10.0, 20.0);
/// let p2 = Point::ZERO;
///
/// assert_eq!(p1.x, 10.0);
/// assert_eq!(p1.y, 20.0);
/// assert_eq!(p2, Point::new(0.0, 0.0));
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Point {
  /// X coordinate (horizontal position, increases to the right)
  pub x: f32,
  /// Y coordinate (vertical position, increases downward)
  pub y: f32,
}

impl Point {
  /// The zero point at the origin (0, 0)
  pub const ZERO: Self = Self { x: 0.0, y: 0.0 };

  /// Creates a new point at the given coordinates
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Point;
  ///
  /// let point = Point::new(100.0, 50.0);
  /// assert_eq!(point.x, 100.0);
  /// assert_eq!(point.y, 50.0);
  /// ```
  pub const fn new(x: f32, y: f32) -> Self {
    Self { x, y }
  }

  /// Translates this point by another point's coordinates
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Point;
  ///
  /// let p1 = Point::new(10.0, 20.0);
  /// let p2 = Point::new(5.0, 3.0);
  /// let result = p1.translate(p2);
  ///
  /// assert_eq!(result, Point::new(15.0, 23.0));
  /// ```
  pub fn translate(self, other: Point) -> Self {
    Self {
      x: self.x + other.x,
      y: self.y + other.y,
    }
  }

  /// Computes the distance to another point
  ///
  /// Uses Euclidean distance formula: sqrt((x2-x1)² + (y2-y1)²)
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Point;
  ///
  /// let p1 = Point::new(0.0, 0.0);
  /// let p2 = Point::new(3.0, 4.0);
  ///
  /// assert_eq!(p1.distance_to(p2), 5.0); // 3-4-5 triangle
  /// ```
  pub fn distance_to(self, other: Point) -> f32 {
    let dx = other.x - self.x;
    let dy = other.y - self.y;
    (dx * dx + dy * dy).sqrt()
  }
}

// Implement Display for better debug output
impl fmt::Display for Point {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({}, {})", self.x, self.y)
  }
}

/// A 2D size in CSS pixels
///
/// Represents the dimensions of a rectangular region.
/// Both width and height are non-negative (though not enforced by the type).
///
/// # Examples
///
/// ```
/// use fastrender::Size;
///
/// let size = Size::new(100.0, 50.0);
/// assert_eq!(size.width, 100.0);
/// assert_eq!(size.height, 50.0);
/// assert_eq!(size.area(), 5000.0);
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Size {
  /// Width (horizontal extent)
  pub width: f32,
  /// Height (vertical extent)
  pub height: f32,
}

impl Size {
  /// A size with zero width and height
  pub const ZERO: Self = Self {
    width: 0.0,
    height: 0.0,
  };

  /// Creates a new size with the given dimensions
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Size;
  ///
  /// let size = Size::new(200.0, 100.0);
  /// assert_eq!(size.width, 200.0);
  /// assert_eq!(size.height, 100.0);
  /// ```
  pub const fn new(width: f32, height: f32) -> Self {
    Self { width, height }
  }

  /// Computes the area (width × height)
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Size;
  ///
  /// let size = Size::new(10.0, 20.0);
  /// assert_eq!(size.area(), 200.0);
  /// ```
  pub fn area(self) -> f32 {
    self.width * self.height
  }

  /// Returns true if either width or height is zero
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Size;
  ///
  /// assert!(Size::ZERO.is_empty());
  /// assert!(Size::new(0.0, 10.0).is_empty());
  /// assert!(!Size::new(10.0, 10.0).is_empty());
  /// ```
  pub fn is_empty(self) -> bool {
    self.width == 0.0 || self.height == 0.0
  }

  /// Scales this size by a factor
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Size;
  ///
  /// let size = Size::new(100.0, 50.0);
  /// let scaled = size.scale(2.0);
  ///
  /// assert_eq!(scaled, Size::new(200.0, 100.0));
  /// ```
  pub fn scale(self, factor: f32) -> Self {
    Self {
      width: self.width * factor,
      height: self.height * factor,
    }
  }
}

impl fmt::Display for Size {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}×{}", self.width, self.height)
  }
}

/// An axis-aligned rectangle in CSS pixel space
///
/// Defined by an origin point (top-left corner) and a size.
///
/// # Examples
///
/// ```
/// use fastrender::{Rect, Point, Size};
///
/// let rect = Rect::new(Point::new(10.0, 20.0), Size::new(100.0, 50.0));
/// assert_eq!(rect.x(), 10.0);
/// assert_eq!(rect.y(), 20.0);
/// assert_eq!(rect.width(), 100.0);
/// assert_eq!(rect.height(), 50.0);
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Rect {
  /// The top-left corner of the rectangle
  pub origin: Point,
  /// The size (width and height) of the rectangle
  pub size: Size,
}

impl Rect {
  /// A zero-sized rectangle at the origin
  pub const ZERO: Self = Self {
    origin: Point::ZERO,
    size: Size::ZERO,
  };

  /// Creates a new rectangle from an origin point and size
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::{Rect, Point, Size};
  ///
  /// let rect = Rect::new(Point::new(5.0, 5.0), Size::new(10.0, 10.0));
  /// assert_eq!(rect.origin.x, 5.0);
  /// ```
  pub const fn new(origin: Point, size: Size) -> Self {
    Self { origin, size }
  }

  /// Creates a rectangle from x, y, width, height components
  ///
  /// This is a convenience constructor for the common case.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Rect;
  ///
  /// let rect = Rect::from_xywh(10.0, 20.0, 100.0, 50.0);
  /// assert_eq!(rect.x(), 10.0);
  /// assert_eq!(rect.width(), 100.0);
  /// ```
  pub const fn from_xywh(x: f32, y: f32, width: f32, height: f32) -> Self {
    Self {
      origin: Point::new(x, y),
      size: Size::new(width, height),
    }
  }

  /// Creates a rectangle from two corner points
  ///
  /// Takes the top-left and bottom-right corners.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::{Rect, Point};
  ///
  /// let rect = Rect::from_points(Point::new(10.0, 20.0), Point::new(50.0, 70.0));
  /// assert_eq!(rect.width(), 40.0);
  /// assert_eq!(rect.height(), 50.0);
  /// ```
  pub fn from_points(top_left: Point, bottom_right: Point) -> Self {
    Self {
      origin: top_left,
      size: Size::new(bottom_right.x - top_left.x, bottom_right.y - top_left.y),
    }
  }

  // Accessor methods

  /// Returns the x coordinate of the left edge
  pub fn x(self) -> f32 {
    self.origin.x
  }

  /// Returns the y coordinate of the top edge
  pub fn y(self) -> f32 {
    self.origin.y
  }

  /// Returns the width
  pub fn width(self) -> f32 {
    self.size.width
  }

  /// Returns the height
  pub fn height(self) -> f32 {
    self.size.height
  }

  /// Returns the x coordinate of the left edge (same as x())
  pub fn min_x(self) -> f32 {
    self.origin.x
  }

  /// Returns the x coordinate of the right edge
  pub fn max_x(self) -> f32 {
    self.origin.x + self.size.width
  }

  /// Returns the y coordinate of the top edge (same as y())
  pub fn min_y(self) -> f32 {
    self.origin.y
  }

  /// Returns the y coordinate of the bottom edge
  pub fn max_y(self) -> f32 {
    self.origin.y + self.size.height
  }

  /// Returns the center point of the rectangle
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::{Rect, Point};
  ///
  /// let rect = Rect::from_xywh(0.0, 0.0, 100.0, 50.0);
  /// assert_eq!(rect.center(), Point::new(50.0, 25.0));
  /// ```
  pub fn center(self) -> Point {
    Point::new(
      self.origin.x + self.size.width / 2.0,
      self.origin.y + self.size.height / 2.0,
    )
  }

  // Geometric operations

  /// Returns true if this rectangle contains the given point
  ///
  /// Points on the boundary are considered inside.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::{Rect, Point};
  ///
  /// let rect = Rect::from_xywh(10.0, 10.0, 20.0, 20.0);
  /// assert!(rect.contains_point(Point::new(15.0, 15.0)));
  /// assert!(!rect.contains_point(Point::new(5.0, 5.0)));
  /// ```
  pub fn contains_point(self, point: Point) -> bool {
    point.x >= self.min_x()
      && point.x <= self.max_x()
      && point.y >= self.min_y()
      && point.y <= self.max_y()
  }

  /// Returns true if this rectangle intersects another rectangle
  ///
  /// Rectangles that touch at an edge or corner are considered intersecting.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Rect;
  ///
  /// let rect1 = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
  /// let rect2 = Rect::from_xywh(5.0, 5.0, 10.0, 10.0);
  /// let rect3 = Rect::from_xywh(20.0, 20.0, 10.0, 10.0);
  ///
  /// assert!(rect1.intersects(rect2));
  /// assert!(!rect1.intersects(rect3));
  /// ```
  pub fn intersects(self, other: Rect) -> bool {
    self.min_x() <= other.max_x()
      && self.max_x() >= other.min_x()
      && self.min_y() <= other.max_y()
      && self.max_y() >= other.min_y()
  }

  /// Computes the union of two rectangles
  ///
  /// Returns the smallest rectangle that contains both rectangles.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Rect;
  ///
  /// let rect1 = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
  /// let rect2 = Rect::from_xywh(5.0, 5.0, 10.0, 10.0);
  /// let union = rect1.union(rect2);
  ///
  /// assert_eq!(union, Rect::from_xywh(0.0, 0.0, 15.0, 15.0));
  /// ```
  pub fn union(self, other: Rect) -> Rect {
    let min_x = self.min_x().min(other.min_x());
    let min_y = self.min_y().min(other.min_y());
    let max_x = self.max_x().max(other.max_x());
    let max_y = self.max_y().max(other.max_y());

    Rect::from_xywh(min_x, min_y, max_x - min_x, max_y - min_y)
  }

  /// Computes the intersection of two rectangles
  ///
  /// Returns None if the rectangles don't intersect.
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Rect;
  ///
  /// let rect1 = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
  /// let rect2 = Rect::from_xywh(5.0, 5.0, 10.0, 10.0);
  ///
  /// let intersection = rect1.intersection(rect2);
  /// assert_eq!(intersection, Some(Rect::from_xywh(5.0, 5.0, 5.0, 5.0)));
  /// ```
  pub fn intersection(self, other: Rect) -> Option<Rect> {
    if !self.intersects(other) {
      return None;
    }

    let min_x = self.min_x().max(other.min_x());
    let min_y = self.min_y().max(other.min_y());
    let max_x = self.max_x().min(other.max_x());
    let max_y = self.max_y().min(other.max_y());

    Some(Rect::from_xywh(min_x, min_y, max_x - min_x, max_y - min_y))
  }

  /// Translates this rectangle by an offset
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::{Rect, Point};
  ///
  /// let rect = Rect::from_xywh(10.0, 10.0, 20.0, 20.0);
  /// let translated = rect.translate(Point::new(5.0, 3.0));
  ///
  /// assert_eq!(translated, Rect::from_xywh(15.0, 13.0, 20.0, 20.0));
  /// ```
  pub fn translate(self, offset: Point) -> Rect {
    Rect {
      origin: self.origin.translate(offset),
      size: self.size,
    }
  }

  /// Inflates or deflates the rectangle by the given amount on all sides
  ///
  /// Positive values inflate (make larger), negative values deflate (make smaller).
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::Rect;
  ///
  /// let rect = Rect::from_xywh(10.0, 10.0, 20.0, 20.0);
  /// let inflated = rect.inflate(5.0);
  ///
  /// assert_eq!(inflated, Rect::from_xywh(5.0, 5.0, 30.0, 30.0));
  /// ```
  pub fn inflate(self, amount: f32) -> Rect {
    Rect::from_xywh(
      self.origin.x - amount,
      self.origin.y - amount,
      self.size.width + 2.0 * amount,
      self.size.height + 2.0 * amount,
    )
  }
}

/// Edge offsets representing spacing on all four sides
///
/// Used for margin, padding, and border widths.
/// Follows CSS box model convention: top, right, bottom, left.
///
/// # Examples
///
/// ```
/// use fastrender::geometry::EdgeOffsets;
///
/// let padding = EdgeOffsets::all(10.0);
/// assert_eq!(padding.horizontal(), 20.0);
/// assert_eq!(padding.vertical(), 20.0);
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EdgeOffsets {
  /// Top edge offset
  pub top: f32,
  /// Right edge offset
  pub right: f32,
  /// Bottom edge offset
  pub bottom: f32,
  /// Left edge offset
  pub left: f32,
}

impl EdgeOffsets {
  /// Zero offsets on all sides
  pub const ZERO: Self = Self {
    top: 0.0,
    right: 0.0,
    bottom: 0.0,
    left: 0.0,
  };

  /// Creates edge offsets with the same value on all sides
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::geometry::EdgeOffsets;
  ///
  /// let padding = EdgeOffsets::all(10.0);
  /// assert_eq!(padding.top, 10.0);
  /// assert_eq!(padding.left, 10.0);
  /// ```
  pub const fn all(value: f32) -> Self {
    Self {
      top: value,
      right: value,
      bottom: value,
      left: value,
    }
  }

  /// Creates edge offsets with individual values for each side
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::geometry::EdgeOffsets;
  ///
  /// let margin = EdgeOffsets::new(10.0, 20.0, 10.0, 20.0);
  /// assert_eq!(margin.horizontal(), 40.0);
  /// ```
  pub const fn new(top: f32, right: f32, bottom: f32, left: f32) -> Self {
    Self {
      top,
      right,
      bottom,
      left,
    }
  }

  /// Creates symmetric edge offsets
  ///
  /// # Arguments
  /// * `vertical` - Value for top and bottom
  /// * `horizontal` - Value for left and right
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::geometry::EdgeOffsets;
  ///
  /// let padding = EdgeOffsets::symmetric(10.0, 20.0);
  /// assert_eq!(padding.top, 10.0);
  /// assert_eq!(padding.left, 20.0);
  /// ```
  pub const fn symmetric(vertical: f32, horizontal: f32) -> Self {
    Self {
      top: vertical,
      right: horizontal,
      bottom: vertical,
      left: horizontal,
    }
  }

  /// Returns the sum of left and right offsets
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::geometry::EdgeOffsets;
  ///
  /// let offsets = EdgeOffsets::new(5.0, 10.0, 5.0, 15.0);
  /// assert_eq!(offsets.horizontal(), 25.0);
  /// ```
  pub fn horizontal(self) -> f32 {
    self.left + self.right
  }

  /// Returns the sum of top and bottom offsets
  ///
  /// # Examples
  ///
  /// ```
  /// use fastrender::geometry::EdgeOffsets;
  ///
  /// let offsets = EdgeOffsets::new(10.0, 5.0, 20.0, 5.0);
  /// assert_eq!(offsets.vertical(), 30.0);
  /// ```
  pub fn vertical(self) -> f32 {
    self.top + self.bottom
  }
}

impl fmt::Display for EdgeOffsets {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "[t:{}, r:{}, b:{}, l:{}]",
      self.top, self.right, self.bottom, self.left
    )
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  // Point tests
  #[test]
  fn test_point_creation() {
    let p = Point::new(10.0, 20.0);
    assert_eq!(p.x, 10.0);
    assert_eq!(p.y, 20.0);
  }

  #[test]
  fn test_point_zero() {
    let p = Point::ZERO;
    assert_eq!(p.x, 0.0);
    assert_eq!(p.y, 0.0);
  }

  #[test]
  fn test_point_translate() {
    let p1 = Point::new(10.0, 20.0);
    let p2 = Point::new(5.0, 3.0);
    let result = p1.translate(p2);
    assert_eq!(result, Point::new(15.0, 23.0));
  }

  #[test]
  fn test_point_distance() {
    let p1 = Point::new(0.0, 0.0);
    let p2 = Point::new(3.0, 4.0);
    assert!((p1.distance_to(p2) - 5.0).abs() < 0.001);
  }

  // Size tests
  #[test]
  fn test_size_creation() {
    let s = Size::new(100.0, 50.0);
    assert_eq!(s.width, 100.0);
    assert_eq!(s.height, 50.0);
  }

  #[test]
  fn test_size_area() {
    let s = Size::new(10.0, 20.0);
    assert_eq!(s.area(), 200.0);
  }

  #[test]
  fn test_size_is_empty() {
    assert!(Size::ZERO.is_empty());
    assert!(Size::new(0.0, 10.0).is_empty());
    assert!(Size::new(10.0, 0.0).is_empty());
    assert!(!Size::new(10.0, 10.0).is_empty());
  }

  #[test]
  fn test_size_scale() {
    let s = Size::new(100.0, 50.0);
    let scaled = s.scale(2.0);
    assert_eq!(scaled, Size::new(200.0, 100.0));
  }

  // Rect tests
  #[test]
  fn test_rect_creation() {
    let rect = Rect::from_xywh(10.0, 20.0, 100.0, 50.0);
    assert_eq!(rect.x(), 10.0);
    assert_eq!(rect.y(), 20.0);
    assert_eq!(rect.width(), 100.0);
    assert_eq!(rect.height(), 50.0);
  }

  #[test]
  fn test_rect_from_points() {
    let rect = Rect::from_points(Point::new(10.0, 20.0), Point::new(50.0, 70.0));
    assert_eq!(rect.width(), 40.0);
    assert_eq!(rect.height(), 50.0);
  }

  #[test]
  fn test_rect_accessors() {
    let rect = Rect::from_xywh(10.0, 20.0, 100.0, 50.0);
    assert_eq!(rect.min_x(), 10.0);
    assert_eq!(rect.max_x(), 110.0);
    assert_eq!(rect.min_y(), 20.0);
    assert_eq!(rect.max_y(), 70.0);
  }

  #[test]
  fn test_rect_center() {
    let rect = Rect::from_xywh(0.0, 0.0, 100.0, 50.0);
    assert_eq!(rect.center(), Point::new(50.0, 25.0));
  }

  #[test]
  fn test_rect_contains_point() {
    let rect = Rect::from_xywh(10.0, 10.0, 20.0, 20.0);
    assert!(rect.contains_point(Point::new(15.0, 15.0)));
    assert!(rect.contains_point(Point::new(10.0, 10.0))); // Boundary
    assert!(rect.contains_point(Point::new(30.0, 30.0))); // Boundary
    assert!(!rect.contains_point(Point::new(5.0, 5.0)));
    assert!(!rect.contains_point(Point::new(35.0, 35.0)));
  }

  #[test]
  fn test_rect_intersects() {
    let rect1 = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
    let rect2 = Rect::from_xywh(5.0, 5.0, 10.0, 10.0);
    let rect3 = Rect::from_xywh(20.0, 20.0, 10.0, 10.0);
    let rect4 = Rect::from_xywh(10.0, 10.0, 10.0, 10.0); // Touches corner

    assert!(rect1.intersects(rect2));
    assert!(rect2.intersects(rect1)); // Symmetric
    assert!(!rect1.intersects(rect3));
    assert!(rect1.intersects(rect4)); // Corner touch counts
  }

  #[test]
  fn test_rect_union() {
    let rect1 = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
    let rect2 = Rect::from_xywh(5.0, 5.0, 10.0, 10.0);
    let union = rect1.union(rect2);

    assert_eq!(union, Rect::from_xywh(0.0, 0.0, 15.0, 15.0));
  }

  #[test]
  fn test_rect_intersection() {
    let rect1 = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
    let rect2 = Rect::from_xywh(5.0, 5.0, 10.0, 10.0);
    let rect3 = Rect::from_xywh(20.0, 20.0, 10.0, 10.0);

    let intersection = rect1.intersection(rect2);
    assert_eq!(intersection, Some(Rect::from_xywh(5.0, 5.0, 5.0, 5.0)));

    let no_intersection = rect1.intersection(rect3);
    assert_eq!(no_intersection, None);
  }

  #[test]
  fn test_rect_translate() {
    let rect = Rect::from_xywh(10.0, 10.0, 20.0, 20.0);
    let translated = rect.translate(Point::new(5.0, 3.0));

    assert_eq!(translated, Rect::from_xywh(15.0, 13.0, 20.0, 20.0));
  }

  #[test]
  fn test_rect_inflate() {
    let rect = Rect::from_xywh(10.0, 10.0, 20.0, 20.0);
    let inflated = rect.inflate(5.0);
    assert_eq!(inflated, Rect::from_xywh(5.0, 5.0, 30.0, 30.0));

    let deflated = rect.inflate(-2.0);
    assert_eq!(deflated, Rect::from_xywh(12.0, 12.0, 16.0, 16.0));
  }

  // EdgeOffsets tests
  #[test]
  fn test_edge_offsets_creation() {
    let offsets = EdgeOffsets::new(10.0, 20.0, 30.0, 40.0);
    assert_eq!(offsets.top, 10.0);
    assert_eq!(offsets.right, 20.0);
    assert_eq!(offsets.bottom, 30.0);
    assert_eq!(offsets.left, 40.0);
  }

  #[test]
  fn test_edge_offsets_all() {
    let offsets = EdgeOffsets::all(10.0);
    assert_eq!(offsets.top, 10.0);
    assert_eq!(offsets.right, 10.0);
    assert_eq!(offsets.bottom, 10.0);
    assert_eq!(offsets.left, 10.0);
  }

  #[test]
  fn test_edge_offsets_symmetric() {
    let offsets = EdgeOffsets::symmetric(10.0, 20.0);
    assert_eq!(offsets.top, 10.0);
    assert_eq!(offsets.bottom, 10.0);
    assert_eq!(offsets.left, 20.0);
    assert_eq!(offsets.right, 20.0);
  }

  #[test]
  fn test_edge_offsets_horizontal() {
    let offsets = EdgeOffsets::new(5.0, 10.0, 5.0, 15.0);
    assert_eq!(offsets.horizontal(), 25.0);
  }

  #[test]
  fn test_edge_offsets_vertical() {
    let offsets = EdgeOffsets::new(10.0, 5.0, 20.0, 5.0);
    assert_eq!(offsets.vertical(), 30.0);
  }
}
