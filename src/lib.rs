pub mod css;
pub mod dom;
pub mod error;
pub mod geometry;
pub mod image_loader;
pub mod image_output;
pub mod layout;
pub mod paint;
pub mod renderer;
pub mod style;
pub mod text;

pub use error::{Error, Result};
pub use geometry::{EdgeOffsets, Point, Rect, Size};
pub use renderer::{ImageFormat, RenderOptions, Renderer};

// Re-export new color types from style module
pub use style::color::{Color as StyleColor, ColorParseError, Hsla, Rgba};
pub use style::{Length, LengthUnit, LengthOrAuto};
