use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("HTML parsing error: {0}")]
    HtmlParse(String),

    #[error("CSS parsing error: {0}")]
    CssParse(String),

    #[error("Layout error: {0}")]
    Layout(String),

    #[error("Font error: {0}")]
    Font(String),

    #[error("Rendering error: {0}")]
    Render(String),

    #[error("Image encoding error: {0}")]
    ImageEncode(String),

    #[error("Image decoding error: {0}")]
    ImageDecode(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Invalid dimensions: width={0}, height={1}")]
    InvalidDimensions(u32, u32),
}

pub type Result<T> = std::result::Result<T, Error>;
