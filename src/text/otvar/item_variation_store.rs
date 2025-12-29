use ttf_parser::Tag;

/// Error returned when parsing variation tables.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
  UnexpectedEof,
  InvalidVersion(u16),
  InvalidFormat(u16),
  InvalidValue(&'static str),
}

/// Offsets found in the COLR v1 header.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ColrV1HeaderOffsets {
  pub version: u16,
  pub num_base_glyph_records: u16,
  pub base_glyph_records_offset: u32,
  pub layer_records_offset: u32,
  pub num_layer_records: u16,
  pub base_glyph_list_offset: u32,
  pub layer_list_offset: u32,
  pub clip_list_offset: u32,
  pub var_index_map_offset: u32,
  pub item_var_store_offset: u32,
}

/// Axis bounds for a single region axis in F2Dot14 units.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VariationAxisRecord {
  pub start: i16,
  pub peak: i16,
  pub end: i16,
}

impl VariationAxisRecord {
  #[inline]
  fn to_f32(self) -> (f32, f32, f32) {
    (
      self.start as f32 / 16384.0,
      self.peak as f32 / 16384.0,
      self.end as f32 / 16384.0,
    )
  }
}

/// Region covering one or more axes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariationRegion {
  pub axes: Vec<VariationAxisRecord>,
}

impl VariationRegion {
  /// Computes the region's scalar for the provided normalized variation coordinates.
  ///
  /// Returns `None` if the coordinate vector is shorter than the region axis count.
  pub fn support_scalar(&self, coords: &[f32]) -> Option<f32> {
    if coords.len() < self.axes.len() {
      return None;
    }
    let mut scalar = 1.0;
    for (axis, coord) in self.axes.iter().zip(coords.iter()) {
      let (start, peak, end) = axis.to_f32();
      let coord = *coord;
      if coord < start || coord > end {
        return Some(0.0);
      }
      let axis_scalar = if start == peak && peak == end {
        1.0
      } else if coord < peak {
        if peak == start {
          0.0
        } else {
          (coord - start) / (peak - start)
        }
      } else if coord > peak {
        if end == peak {
          0.0
        } else {
          (end - coord) / (end - peak)
        }
      } else {
        1.0
      };
      scalar *= axis_scalar.clamp(0.0, 1.0);
    }
    Some(scalar)
  }
}

/// List of regions used by ItemVariationData tables.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariationRegionList {
  pub axis_count: u16,
  pub regions: Vec<VariationRegion>,
}

/// Delta set indexes reference an [outer, inner] pair selecting an item variation data subtable and
/// a delta set within it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DeltaSetIndex {
  pub outer_index: u16,
  pub inner_index: u16,
}

impl DeltaSetIndex {
  #[inline]
  pub fn from_u32(value: u32) -> Self {
    Self {
      outer_index: (value >> 16) as u16,
      inner_index: value as u16,
    }
  }
}

/// A mapping of packed delta-set indexes used by COLR varIndexMap.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeltaSetIndexMapParsed {
  pub format: u16,
  pub entry_size: u8,
  pub inner_index_bit_count: u8,
  pub map_count: u32,
  pub entries: Vec<DeltaSetIndex>,
}

/// Item variation deltas for a set of items in a single subtable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemVariationData {
  pub item_count: u16,
  pub short_delta_count: u16,
  pub region_indices: Vec<u16>,
  pub delta_sets: Vec<Vec<i16>>,
}

/// Parsed ItemVariationStore.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemVariationStoreParsed {
  pub format: u16,
  pub variation_region_list_offset: u32,
  pub item_variation_data_offsets: Vec<u32>,
  pub region_list: VariationRegionList,
  pub item_variation_data: Vec<ItemVariationData>,
}

impl ItemVariationStoreParsed {
  /// Computes a scalar delta for the provided delta-set index and normalized coordinates.
  pub fn evaluate_delta(&self, index: DeltaSetIndex, coords: &[f32]) -> Option<f32> {
    let data = self.item_variation_data.get(index.outer_index as usize)?;
    let deltas = data.delta_sets.get(index.inner_index as usize)?;
    if deltas.len() != data.region_indices.len() {
      return None;
    }
    let mut total = 0.0;
    for (delta, region_idx) in deltas.iter().zip(data.region_indices.iter()) {
      let region = self.region_list.regions.get(*region_idx as usize)?;
      let scalar = region.support_scalar(coords)?;
      total += *delta as f32 * scalar;
    }
    Some(total)
  }
}

/// Parsed var-store related data from a COLR v1 table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ColrVarStoreTables {
  pub header: ColrV1HeaderOffsets,
  pub var_index_map: Option<DeltaSetIndexMapParsed>,
  pub item_variation_store: Option<ItemVariationStoreParsed>,
}

/// Parses a COLR v1 header, returning the v0/v1 offsets.
pub fn parse_colr_v1_header_offsets(data: &[u8]) -> Result<ColrV1HeaderOffsets, ParseError> {
  if data.len() < 34 {
    return Err(ParseError::UnexpectedEof);
  }
  let version = read_u16(data, 0)?;
  if version != 1 {
    return Err(ParseError::InvalidVersion(version));
  }

  Ok(ColrV1HeaderOffsets {
    version,
    num_base_glyph_records: read_u16(data, 2)?,
    base_glyph_records_offset: read_u32(data, 4)?,
    layer_records_offset: read_u32(data, 8)?,
    num_layer_records: read_u16(data, 12)?,
    base_glyph_list_offset: read_u32(data, 14)?,
    layer_list_offset: read_u32(data, 18)?,
    clip_list_offset: read_u32(data, 22)?,
    var_index_map_offset: read_u32(data, 26)?,
    item_var_store_offset: read_u32(data, 30)?,
  })
}

/// Attempts to parse var-store related data from a COLR table using raw bytes as a fallback when
/// higher-level APIs aren't available.
pub fn parse_colr_v1_var_store(
  face: &ttf_parser::Face<'_>,
) -> Result<Option<ColrVarStoreTables>, ParseError> {
  let Some(colr_data) = face.raw_face().table(Tag::from_bytes(b"COLR")) else {
    return Ok(None);
  };
  let header = match parse_colr_v1_header_offsets(colr_data) {
    Ok(header) => header,
    Err(ParseError::InvalidVersion(_)) => return Ok(None),
    Err(err) => return Err(err),
  };

  let var_index_map = if header.var_index_map_offset != 0 {
    let offset = header.var_index_map_offset as usize;
    Some(parse_delta_set_index_map(
      colr_data.get(offset..).ok_or(ParseError::UnexpectedEof)?,
    )?)
  } else {
    None
  };

  let item_variation_store = if header.item_var_store_offset != 0 {
    let offset = header.item_var_store_offset as usize;
    Some(parse_item_variation_store(
      colr_data.get(offset..).ok_or(ParseError::UnexpectedEof)?,
    )?)
  } else {
    None
  };

  Ok(Some(ColrVarStoreTables {
    header,
    var_index_map,
    item_variation_store,
  }))
}

/// Parses an ItemVariationStore table (format 1).
pub fn parse_item_variation_store(data: &[u8]) -> Result<ItemVariationStoreParsed, ParseError> {
  if data.len() < 8 {
    return Err(ParseError::UnexpectedEof);
  }
  let format = read_u16(data, 0)?;
  if format != 1 {
    return Err(ParseError::InvalidFormat(format));
  }
  let variation_region_list_offset = read_u32(data, 2)?;
  let item_variation_data_count = read_u16(data, 6)? as usize;

  let mut item_variation_data_offsets = Vec::with_capacity(item_variation_data_count);
  let mut offset = 8;
  for _ in 0..item_variation_data_count {
    item_variation_data_offsets.push(read_u32(data, offset)?);
    offset += 4;
  }

  let region_list =
    parse_variation_region_list(data, variation_region_list_offset as usize)?.unwrap_or_default();

  let mut item_variation_data = Vec::with_capacity(item_variation_data_count);
  for item_offset in &item_variation_data_offsets {
    let Some(start) = data.get(*item_offset as usize..) else {
      return Err(ParseError::UnexpectedEof);
    };
    item_variation_data.push(parse_item_variation_data(start)?);
  }

  Ok(ItemVariationStoreParsed {
    format,
    variation_region_list_offset,
    item_variation_data_offsets,
    region_list,
    item_variation_data,
  })
}

/// Parses a DeltaSetIndexMap (format 0 or 1).
pub fn parse_delta_set_index_map(data: &[u8]) -> Result<DeltaSetIndexMapParsed, ParseError> {
  if data.len() < 6 {
    return Err(ParseError::UnexpectedEof);
  }
  let format = read_u16(data, 0)?;
  let entry_format = read_u16(data, 2)?;
  let (map_count, mut offset) = match format {
    0 => (read_u16(data, 4)? as u32, 6),
    1 => (read_u32(data, 4)?, 8),
    _ => return Err(ParseError::InvalidFormat(format)),
  };

  let entry_size = (entry_format & 0x0F) as u8 + 1;
  let inner_index_bit_count = ((entry_format >> 4) & 0x0F) as u8 + 1;
  if entry_size as usize > 8 {
    return Err(ParseError::InvalidValue("entry size too large"));
  }
  let total_bits = entry_size as u32 * 8;
  if inner_index_bit_count as u32 >= total_bits {
    return Err(ParseError::InvalidValue(
      "inner index bit count exceeds entry size",
    ));
  }

  let entry_mask = if inner_index_bit_count == 64 {
    u64::MAX
  } else {
    (1u64 << inner_index_bit_count) - 1
  };

  let mut entries = Vec::with_capacity(map_count as usize);
  for _ in 0..map_count {
    let slice = data
      .get(offset..offset + entry_size as usize)
      .ok_or(ParseError::UnexpectedEof)?;
    let mut raw = 0u64;
    for byte in slice {
      raw = (raw << 8) | *byte as u64;
    }
    let inner_index = (raw & entry_mask) as u16;
    let outer_index = (raw >> inner_index_bit_count) as u16;
    entries.push(DeltaSetIndex {
      outer_index,
      inner_index,
    });
    offset += entry_size as usize;
  }

  Ok(DeltaSetIndexMapParsed {
    format,
    entry_size,
    inner_index_bit_count,
    map_count,
    entries,
  })
}

fn parse_variation_region_list(
  data: &[u8],
  offset: usize,
) -> Result<Option<VariationRegionList>, ParseError> {
  if offset == 0 {
    return Ok(None);
  }
  let Some(head) = data.get(offset..) else {
    return Err(ParseError::UnexpectedEof);
  };
  if head.len() < 4 {
    return Err(ParseError::UnexpectedEof);
  }
  let axis_count = read_u16(head, 0)?;
  let region_count = read_u16(head, 2)? as usize;
  let mut regions = Vec::with_capacity(region_count);
  let mut pos = 4;
  for _ in 0..region_count {
    let mut axes = Vec::with_capacity(axis_count as usize);
    for _ in 0..axis_count {
      let start = read_i16(head, pos)?;
      let peak = read_i16(head, pos + 2)?;
      let end = read_i16(head, pos + 4)?;
      axes.push(VariationAxisRecord { start, peak, end });
      pos += 6;
    }
    regions.push(VariationRegion { axes });
  }
  Ok(Some(VariationRegionList {
    axis_count,
    regions,
  }))
}

fn parse_item_variation_data(data: &[u8]) -> Result<ItemVariationData, ParseError> {
  if data.len() < 6 {
    return Err(ParseError::UnexpectedEof);
  }

  let item_count = read_u16(data, 0)?;
  let short_delta_count = read_u16(data, 2)?;
  let region_index_count = read_u16(data, 4)?;
  if short_delta_count > region_index_count {
    return Err(ParseError::InvalidValue(
      "short deltas exceed region index count",
    ));
  }

  let mut region_indices = Vec::with_capacity(region_index_count as usize);
  let mut offset = 6;
  for _ in 0..region_index_count {
    region_indices.push(read_u16(data, offset)?);
    offset += 2;
  }

  let mut delta_sets = Vec::with_capacity(item_count as usize);
  for _ in 0..item_count {
    let mut deltas = Vec::with_capacity(region_index_count as usize);
    for _ in 0..short_delta_count {
      deltas.push(read_i16(data, offset)?);
      offset += 2;
    }
    for _ in short_delta_count..region_index_count {
      deltas.push(read_i8(data, offset)? as i16);
      offset += 1;
    }
    delta_sets.push(deltas);
  }

  Ok(ItemVariationData {
    item_count,
    short_delta_count,
    region_indices,
    delta_sets,
  })
}

fn read_u16(data: &[u8], offset: usize) -> Result<u16, ParseError> {
  let bytes = data
    .get(offset..offset + 2)
    .ok_or(ParseError::UnexpectedEof)?;
  Ok(u16::from_be_bytes([bytes[0], bytes[1]]))
}

fn read_u32(data: &[u8], offset: usize) -> Result<u32, ParseError> {
  let bytes = data
    .get(offset..offset + 4)
    .ok_or(ParseError::UnexpectedEof)?;
  Ok(u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
}

fn read_i8(data: &[u8], offset: usize) -> Result<i8, ParseError> {
  let byte = *data.get(offset).ok_or(ParseError::UnexpectedEof)?;
  Ok(byte as i8)
}

fn read_i16(data: &[u8], offset: usize) -> Result<i16, ParseError> {
  let bytes = data
    .get(offset..offset + 2)
    .ok_or(ParseError::UnexpectedEof)?;
  Ok(i16::from_be_bytes([bytes[0], bytes[1]]))
}

impl Default for VariationRegionList {
  fn default() -> Self {
    Self {
      axis_count: 0,
      regions: Vec::new(),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::path::PathBuf;

  fn fixtures_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("fixtures")
      .join("fonts")
  }

  fn load_colr_table(name: &str) -> Vec<u8> {
    let path = fixtures_path().join(name);
    let data = std::fs::read(path).expect("failed to read test font");
    let face = ttf_parser::Face::parse(&data, 0).expect("failed to parse test face");
    face
      .raw_face()
      .table(Tag::from_bytes(b"COLR"))
      .expect("missing COLR table")
      .to_vec()
  }

  fn assert_fixture_common(header: &ColrV1HeaderOffsets, expected_item_offset: u32) {
    assert_eq!(header.version, 1);
    assert_eq!(header.base_glyph_list_offset, 34);
    assert_eq!(header.item_var_store_offset, expected_item_offset);
    assert_eq!(header.var_index_map_offset, 0);
  }

  #[test]
  fn parses_item_variation_store_from_colrv1_var_test() {
    let colr_data = load_colr_table("colrv1-var-test.ttf");
    let header = parse_colr_v1_header_offsets(&colr_data).expect("header");
    assert_fixture_common(&header, 93);

    let store_data = &colr_data[header.item_var_store_offset as usize..];
    let store = parse_item_variation_store(store_data).expect("var store");
    assert_eq!(store.format, 1);
    assert_eq!(store.variation_region_list_offset, 12);
    assert_eq!(store.item_variation_data_offsets, vec![22]);
    assert_eq!(store.region_list.axis_count, 1);
    assert_eq!(store.region_list.regions.len(), 1);

    let region = &store.region_list.regions[0];
    assert_eq!(region.axes.len(), 1);
    assert_eq!(
      region.axes[0],
      VariationAxisRecord {
        start: 0,
        peak: 0x4000,
        end: 0x4000
      }
    );

    assert_eq!(store.item_variation_data.len(), 1);
    let data = &store.item_variation_data[0];
    assert_eq!(data.item_count, 10);
    assert_eq!(data.short_delta_count, 1);
    assert_eq!(data.region_indices, vec![0]);
    assert_eq!(data.delta_sets.len(), 10);
    assert_eq!(data.delta_sets[3], vec![200]);

    let delta = store
      .evaluate_delta(
        DeltaSetIndex {
          outer_index: 0,
          inner_index: 3,
        },
        &[1.0],
      )
      .expect("delta");
    assert!((delta - 200.0).abs() < f32::EPSILON);
  }

  #[test]
  fn parses_item_variation_store_from_colrv1_var_sweep() {
    let colr_data = load_colr_table("colrv1-var-sweep-test.ttf");
    let header = parse_colr_v1_header_offsets(&colr_data).expect("header");
    assert_fixture_common(&header, 99);

    let store_data = &colr_data[header.item_var_store_offset as usize..];
    let store = parse_item_variation_store(store_data).expect("var store");
    assert_eq!(store.format, 1);
    assert_eq!(store.variation_region_list_offset, 12);
    assert_eq!(store.item_variation_data_offsets, vec![22]);
    assert_eq!(store.region_list.axis_count, 1);
    assert_eq!(store.region_list.regions.len(), 1);

    let region = &store.region_list.regions[0];
    assert_eq!(region.axes.len(), 1);
    assert_eq!(
      region.axes[0],
      VariationAxisRecord {
        start: 0,
        peak: 0x4000,
        end: 0x4000
      }
    );

    assert_eq!(store.item_variation_data.len(), 1);
    let data = &store.item_variation_data[0];
    assert_eq!(data.item_count, 10);
    assert_eq!(data.short_delta_count, 1);
    assert_eq!(data.region_indices, vec![0]);
    assert_eq!(data.delta_sets.len(), 10);
    assert_eq!(data.delta_sets[3], vec![8192]);
    assert_eq!(data.delta_sets[6], vec![1638]);
    assert_eq!(data.delta_sets[7], vec![-3277]);

    let delta = store
      .evaluate_delta(
        DeltaSetIndex {
          outer_index: 0,
          inner_index: 3,
        },
        &[1.0],
      )
      .expect("delta");
    assert!((delta - 8192.0).abs() < f32::EPSILON);
  }

  #[test]
  fn parses_var_store_via_face_fallback() {
    let path = fixtures_path().join("colrv1-var-test.ttf");
    let data = std::fs::read(path).expect("font bytes");
    let face = ttf_parser::Face::parse(&data, 0).expect("face");
    let parsed = parse_colr_v1_var_store(&face)
      .expect("var store parse")
      .expect("var store present");
    assert_eq!(parsed.header.item_var_store_offset, 93);
    assert!(parsed.var_index_map.is_none());

    let store = parsed
      .item_variation_store
      .expect("item variation store missing");
    assert_eq!(store.item_variation_data.len(), 1);
    assert_eq!(store.item_variation_data[0].delta_sets[3], vec![200]);
  }
}
