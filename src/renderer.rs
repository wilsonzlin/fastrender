//! HTML to image renderer
//!
//! This module provides the main entry point for rendering HTML/CSS to images.
//!
//! Note: The renderer is currently non-functional as the V1 rendering pipeline
//! has been removed. The W4+ implementations for layout and paint need to be
//! integrated to restore rendering functionality.

use crate::css::Color;
use crate::error::{Error, Result};

pub use crate::image_output::OutputFormat as ImageFormat;

#[derive(Debug, Clone)]
pub struct Renderer {
    viewport_width: u32,
    viewport_height: u32,
    background_color: Color,
}

#[derive(Debug, Clone)]
pub struct RenderOptions {
    pub format: ImageFormat,
    pub background_color: Color,
}

impl Default for RenderOptions {
    fn default() -> Self {
        Self {
            format: ImageFormat::Png,
            background_color: Color::WHITE,
        }
    }
}

fn not_implemented() -> Error {
    Error::Render(crate::error::RenderError::InvalidParameters {
        message: "Renderer not implemented: V1 pipeline removed, W4+ integration pending".to_string(),
    })
}

impl Renderer {
    pub fn new() -> Self {
        Self {
            viewport_width: 1920,
            viewport_height: 1080,
            background_color: Color::WHITE,
        }
    }

    pub fn builder() -> RendererBuilder {
        RendererBuilder::new()
    }

    pub fn render_to_png(&self, _html: &str, _width: u32, _height: u32) -> Result<Vec<u8>> {
        Err(not_implemented())
    }

    pub fn render_to_png_auto_height(&self, _html: &str, _width: u32) -> Result<Vec<u8>> {
        Err(not_implemented())
    }

    pub fn render_to_png_with_scroll(&self, _html: &str, _width: u32, _height: u32, _scroll_y: u32) -> Result<Vec<u8>> {
        Err(not_implemented())
    }

    pub fn render_to_png_with_scroll_and_base_url(
        &self,
        _html: &str,
        _width: u32,
        _height: u32,
        _scroll_y: u32,
        _base_url: String,
    ) -> Result<Vec<u8>> {
        Err(not_implemented())
    }

    pub fn render_to_jpeg(&self, _html: &str, _width: u32, _height: u32, _quality: u8) -> Result<Vec<u8>> {
        Err(not_implemented())
    }

    pub fn render_to_webp(&self, _html: &str, _width: u32, _height: u32, _quality: u8) -> Result<Vec<u8>> {
        Err(not_implemented())
    }

    pub fn render(&self, _html: &str) -> Result<Vec<u8>> {
        Err(not_implemented())
    }

    pub fn render_with_options(&self, _html: &str, _options: RenderOptions) -> Result<Vec<u8>> {
        Err(not_implemented())
    }
}

impl Default for Renderer {
    fn default() -> Self {
        Self::new()
    }
}

pub struct RendererBuilder {
    viewport_width: u32,
    viewport_height: u32,
    background_color: Color,
}

impl RendererBuilder {
    pub fn new() -> Self {
        Self {
            viewport_width: 1920,
            viewport_height: 1080,
            background_color: Color::WHITE,
        }
    }

    pub fn viewport_width(mut self, width: u32) -> Self {
        self.viewport_width = width;
        self
    }

    pub fn viewport_height(mut self, height: u32) -> Self {
        self.viewport_height = height;
        self
    }

    pub fn viewport_size(mut self, width: u32, height: u32) -> Self {
        self.viewport_width = width;
        self.viewport_height = height;
        self
    }

    pub fn background_color(mut self, color: Color) -> Self {
        self.background_color = color;
        self
    }

    pub fn build(self) -> Renderer {
        Renderer {
            viewport_width: self.viewport_width,
            viewport_height: self.viewport_height,
            background_color: self.background_color,
        }
    }
}

impl Default for RendererBuilder {
    fn default() -> Self {
        Self::new()
    }
}
