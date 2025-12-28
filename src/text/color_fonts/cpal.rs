use super::{read_u16, read_u32};
use crate::css::types::FontPaletteBase;
use crate::style::color::Rgba;

/// Parsed CPAL palette with optional type metadata (v1+).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct ParsedPalette {
  pub colors: Vec<Rgba>,
  pub palette_type: Option<u32>,
}

pub fn parse_cpal_palette(data: &[u8], palette_index: u16) -> Option<ParsedPalette> {
  if data.len() < 12 {
    return None;
  }

  let version = read_u16(data, 0)?;
  let num_entries = read_u16(data, 2)? as usize;
  let num_palettes = read_u16(data, 4)? as usize;
  let num_color_records = read_u16(data, 6)? as usize;
  let color_offset = read_u32(data, 8)? as usize;

  if num_entries == 0 || num_palettes == 0 || num_color_records == 0 {
    return None;
  }

  let palette = palette_index.min(num_palettes.saturating_sub(1) as u16) as usize;
  let indices_start = 12;
  let indices_end = indices_start + num_palettes * 2;
  if indices_end > data.len() {
    return None;
  }
  if color_offset + num_color_records * 4 > data.len() {
    return None;
  }
  let start_index = read_u16(data, indices_start + palette * 2)? as usize;
  if start_index >= num_color_records {
    return None;
  }

  let colors_start = color_offset + start_index * 4;
  let needed = num_entries * 4;
  if colors_start + needed > data.len() {
    return None;
  }

  let mut colors = Vec::with_capacity(num_entries);
  for i in 0..num_entries {
    let off = colors_start + i * 4;
    let b = data[off];
    let g = data[off + 1];
    let r = data[off + 2];
    let a = data[off + 3];
    colors.push(Rgba::from_rgba8(r, g, b, a));
  }

  let palette_type = if version >= 1 {
    let type_offset_pos = indices_end;
    if type_offset_pos + 4 <= data.len() {
      let offset = read_u32(data, type_offset_pos)? as usize;
      if offset > 0 && offset + num_palettes * 4 <= data.len() {
        Some(read_u32(data, offset + palette * 4)?)
      } else {
        None
      }
    } else {
      None
    }
  } else {
    None
  };

  Some(ParsedPalette {
    colors,
    palette_type,
  })
}

/// Selects a CPAL palette index based on a preference and palette metadata.
pub fn select_cpal_palette(face: &ttf_parser::Face<'_>, preference: FontPaletteBase) -> u16 {
  let data = match face.raw_face().table(ttf_parser::Tag::from_bytes(b"CPAL")) {
    Some(data) => data,
    None => return 0,
  };
  if data.len() < 12 {
    return 0;
  }
  let version = match read_u16(data, 0) {
    Some(v) => v,
    None => return 0,
  };
  let num_palettes = match read_u16(data, 4) {
    Some(v) if v > 0 => v as usize,
    _ => return 0,
  };
  let clamp_index = |idx: u16| idx.min(num_palettes.saturating_sub(1) as u16);
  match preference {
    FontPaletteBase::Index(idx) => return clamp_index(idx),
    FontPaletteBase::Normal => return 0,
    _ => {}
  }

  if version < 1 {
    return 0;
  }
  // paletteTypesArrayOffset follows the colorRecordIndices array.
  let start = 12usize.checked_add(num_palettes * 2).unwrap_or(data.len());
  if start + 4 > data.len() {
    return 0;
  }
  let palette_types_offset = match read_u32(data, start) {
    Some(off) if off > 0 => off as usize,
    _ => return 0,
  };
  if palette_types_offset + num_palettes * 4 > data.len() {
    return 0;
  }
  let desired_bit = match preference {
    FontPaletteBase::Light => 0x0001,
    FontPaletteBase::Dark => 0x0002,
    _ => 0,
  };
  if desired_bit == 0 {
    return 0;
  }
  for i in 0..num_palettes {
    if let Some(ty) = read_u32(data, palette_types_offset + i * 4) {
      if ty & desired_bit != 0 {
        return i as u16;
      }
    }
  }
  0
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_simple_cpal_v0_palette() {
    // version=0, numEntries=2, numPalettes=1, numColorRecords=2, colorOffset=16
    let mut data = vec![
      0x00, 0x00, // version
      0x00, 0x02, // numEntries
      0x00, 0x01, // numPalettes
      0x00, 0x02, // numColorRecords
      0x00, 0x00, 0x00, 0x10, // colorOffset = 16
      0x00, 0x00, // palette index start
    ];
    // Pad to color offset
    data.resize(16, 0);
    // Two color records: BGRA
    data.extend_from_slice(&[1, 2, 3, 4]); // r=3,g=2,b=1,a=4
    data.extend_from_slice(&[5, 6, 7, 8]); // r=7,g=6,b=5,a=8

    let parsed = parse_cpal_palette(&data, 0).expect("palette should parse");
    assert_eq!(parsed.colors.len(), 2);
    assert_eq!(parsed.colors[0], Rgba::from_rgba8(3, 2, 1, 4));
    assert_eq!(parsed.colors[1], Rgba::from_rgba8(7, 6, 5, 8));
  }

  #[test]
  fn parse_cpal_v1_palette_type() {
    // version=1, numEntries=1, numPalettes=1, numColorRecords=1, colorOffset=32
    let mut data = vec![
      0x00, 0x01, // version
      0x00, 0x01, // numEntries
      0x00, 0x01, // numPalettes
      0x00, 0x01, // numColorRecords
      0x00, 0x00, 0x00, 0x20, // colorOffset = 32
      0x00, 0x00, // palette start index
      0x00, 0x00, 0x00, 0x1A, // palette types offset = 26
      0x00, 0x00, 0x00, 0x00, // palette label offset
      0x00, 0x00, 0x00, 0x00, // entry label offset
    ];
    // Pad to palette types offset (26)
    data.resize(26, 0);
    data.extend_from_slice(&[0xAB, 0xCD, 0xEF, 0x01]); // palette type
                                                       // Pad to color offset (32)
    data.resize(32, 0);
    data.extend_from_slice(&[1, 2, 3, 255]); // single BGRA record

    let parsed = parse_cpal_palette(&data, 0).expect("palette v1 should parse");
    assert_eq!(parsed.colors.len(), 1);
    assert_eq!(parsed.palette_type, Some(0xABCDEF01));
  }
}
