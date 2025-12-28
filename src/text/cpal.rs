use crate::style::color::Rgba;
use crate::style::types::FontPalette;

const PALETTE_TYPE_LIGHT_BACKGROUND: u32 = 0x0000_0001;
const PALETTE_TYPE_DARK_BACKGROUND: u32 = 0x0000_0002;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PaletteTypeFlags {
  pub light_background: bool,
  pub dark_background: bool,
}

impl PaletteTypeFlags {
  fn from_raw(raw: u32) -> Self {
    Self {
      light_background: raw & PALETTE_TYPE_LIGHT_BACKGROUND != 0,
      dark_background: raw & PALETTE_TYPE_DARK_BACKGROUND != 0,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CpalMetadata {
  pub num_palettes: usize,
  pub palette_types: Vec<PaletteTypeFlags>,
}

struct CpalHeader {
  num_entries: usize,
  num_palettes: usize,
  num_color_records: usize,
  color_records_offset: usize,
  palette_types_offset: Option<usize>,
}

/// Parses the CPAL palette metadata from a font face, including palette type flags when
/// present (version 1+).
pub fn cpal_metadata(face: &ttf_parser::Face<'_>) -> Option<CpalMetadata> {
  let data = face
    .raw_face()
    .table(ttf_parser::Tag::from_bytes(b"CPAL"))?;
  let header = parse_cpal_header(data)?;

  let palette_types = header
    .palette_types_offset
    .filter(|offset| *offset != 0)
    .and_then(|offset| {
      let needed = header.num_palettes.checked_mul(4)?;
      let end = offset.checked_add(needed)?;
      if end > data.len() {
        return None;
      }
      let mut types = Vec::with_capacity(header.num_palettes);
      for idx in 0..header.num_palettes {
        let raw = read_u32(data, offset + idx * 4).unwrap_or_default();
        types.push(PaletteTypeFlags::from_raw(raw));
      }
      Some(types)
    })
    .unwrap_or_default();

  Some(CpalMetadata {
    num_palettes: header.num_palettes,
    palette_types,
  })
}

/// Chooses a palette index based on the requested CSS `font-palette` value and CPAL metadata.
///
/// Falls back to palette 0 if no matching palette is found or if the font does not expose
/// a CPAL table.
pub fn select_palette_index(face: &ttf_parser::Face<'_>, request: FontPalette) -> u16 {
  let Some(meta) = cpal_metadata(face) else {
    return 0;
  };

  if meta.num_palettes == 0 {
    return 0;
  }

  let requested_index = match request {
    FontPalette::Normal => Some(0),
    FontPalette::Light => meta
      .palette_types
      .iter()
      .position(|p| p.light_background)
      .map(|idx| idx as u16),
    FontPalette::Dark => meta
      .palette_types
      .iter()
      .position(|p| p.dark_background)
      .map(|idx| idx as u16),
    FontPalette::Named(_) => None,
  };

  let idx = requested_index.unwrap_or(0);
  idx.min(meta.num_palettes.saturating_sub(1) as u16)
}

/// Parses the colors for a specific palette index from the CPAL table.
pub fn parse_cpal_palette(face: &ttf_parser::Face<'_>, palette_index: u16) -> Option<Vec<Rgba>> {
  let data = face
    .raw_face()
    .table(ttf_parser::Tag::from_bytes(b"CPAL"))?;
  let header = parse_cpal_header(data)?;

  let palette = palette_index.min(header.num_palettes.saturating_sub(1) as u16) as usize;
  let indices_start = 12;
  let start_index = read_u16(data, indices_start + palette * 2)? as usize;

  if start_index >= header.num_color_records {
    return None;
  }

  let colors_start = header.color_records_offset + start_index * 4;
  let needed = header.num_entries * 4;
  if colors_start + needed > data.len() {
    return None;
  }

  let mut colors = Vec::with_capacity(header.num_entries);
  for i in 0..header.num_entries {
    let off = colors_start + i * 4;
    let b = data[off];
    let g = data[off + 1];
    let r = data[off + 2];
    let a = data[off + 3];
    colors.push(Rgba::from_rgba8(r, g, b, a));
  }

  Some(colors)
}

fn parse_cpal_header(data: &[u8]) -> Option<CpalHeader> {
  if data.len() < 12 {
    return None;
  }

  let version = read_u16(data, 0)?;
  let num_entries = read_u16(data, 2)? as usize;
  let num_palettes = read_u16(data, 4)? as usize;
  let num_color_records = read_u16(data, 6)? as usize;
  let color_records_offset = read_u32(data, 8)? as usize;

  let indices_start = 12usize;
  let indices_len = num_palettes.checked_mul(2)?;
  let indices_end = indices_start.checked_add(indices_len)?;

  if num_entries == 0 || num_palettes == 0 || num_color_records == 0 {
    return None;
  }
  if indices_end > data.len() {
    return None;
  }
  if color_records_offset.checked_add(num_color_records * 4)? > data.len() {
    return None;
  }

  let palette_types_offset = if version >= 1 {
    if indices_end + 12 > data.len() {
      None
    } else {
      read_u32(data, indices_end).map(|v| v as usize)
    }
  } else {
    None
  };

  Some(CpalHeader {
    num_entries,
    num_palettes,
    num_color_records,
    color_records_offset,
    palette_types_offset,
  })
}

fn read_u16(data: &[u8], offset: usize) -> Option<u16> {
  let bytes = data.get(offset..offset + 2)?;
  Some(u16::from_be_bytes([bytes[0], bytes[1]]))
}

fn read_u32(data: &[u8], offset: usize) -> Option<u32> {
  let bytes = data.get(offset..offset + 4)?;
  Some(u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
}
