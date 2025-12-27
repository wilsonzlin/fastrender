use crate::style::types::FontVariantEmoji;
use crate::text::emoji;
use crate::text::font_db::FontDatabase;
use crate::text::font_db::FontStretch as DbFontStretch;
use crate::text::font_db::FontStyle;
use crate::text::font_db::GenericFamily;
use crate::text::font_db::LoadedFont;
use crate::text::font_fallback::FamilyEntry;
use fontdb::Family;
use fontdb::ID;
use ttf_parser::Face as ParserFace;

#[derive(Clone, Debug)]
pub(crate) struct ResolvedFont {
  pub font: LoadedFont,
  pub id: Option<ID>,
}

pub(crate) trait FontResolverContext {
  fn database(&self) -> &FontDatabase;

  #[allow(unused_variables)]
  fn match_web_font_for_char(
    &self,
    family: &str,
    weight: u16,
    style: FontStyle,
    stretch: DbFontStretch,
    oblique_angle: Option<f32>,
    ch: char,
  ) -> Option<LoadedFont> {
    None
  }

  fn is_web_family_declared(&self, _family: &str) -> bool {
    false
  }

  fn math_family_names(&self) -> Vec<String> {
    Vec::new()
  }
}

pub(crate) fn slope_preference_order(style: FontStyle) -> &'static [FontStyle] {
  match style {
    FontStyle::Normal => &[FontStyle::Normal],
    FontStyle::Italic => &[FontStyle::Italic, FontStyle::Oblique, FontStyle::Normal],
    FontStyle::Oblique => &[FontStyle::Oblique, FontStyle::Italic, FontStyle::Normal],
  }
}

pub(crate) fn weight_preference_order(weight: u16) -> Vec<u16> {
  let desired = weight.clamp(1, 1000);
  let mut candidates: Vec<u16> = (1..=9).map(|i| i * 100).collect();
  if !candidates.contains(&desired) {
    candidates.push(desired);
  }
  candidates.sort_by(|a, b| {
    weight_order_key(*a, desired)
      .cmp(&weight_order_key(*b, desired))
      .then_with(|| a.cmp(b))
  });
  candidates.dedup();
  candidates
}

pub(crate) fn stretch_preference_order(stretch: DbFontStretch) -> Vec<DbFontStretch> {
  let target = stretch.to_percentage();
  let mut variants = [
    DbFontStretch::UltraCondensed,
    DbFontStretch::ExtraCondensed,
    DbFontStretch::Condensed,
    DbFontStretch::SemiCondensed,
    DbFontStretch::Normal,
    DbFontStretch::SemiExpanded,
    DbFontStretch::Expanded,
    DbFontStretch::ExtraExpanded,
    DbFontStretch::UltraExpanded,
  ];
  variants.sort_by(|a, b| {
    let ka = stretch_order_key(a.to_percentage(), target);
    let kb = stretch_order_key(b.to_percentage(), target);
    match ka.0.cmp(&kb.0) {
      std::cmp::Ordering::Equal => ka.1.partial_cmp(&kb.1).unwrap_or(std::cmp::Ordering::Equal),
      other => other,
    }
  });
  variants.to_vec()
}

fn weight_order_key(candidate: u16, desired: u16) -> (u8, i32) {
  let desired = desired.clamp(1, 1000) as i32;
  let candidate = candidate as i32;

  if (400..=500).contains(&desired) {
    if candidate >= desired && candidate <= 500 {
      return (0, (candidate - desired).abs());
    }
    if candidate < desired {
      return (1, (desired - candidate).abs());
    }
    return (2, (candidate - desired).abs());
  }

  if desired < 400 {
    if candidate <= desired {
      return (0, (desired - candidate).abs());
    }
    return (1, (candidate - desired).abs());
  }

  // desired > 500
  if candidate >= desired {
    (0, (candidate - desired).abs())
  } else {
    (1, (desired - candidate).abs())
  }
}

fn stretch_order_key(candidate: f32, desired: f32) -> (u8, f32) {
  if (candidate - desired).abs() < f32::EPSILON {
    return (0, 0.0);
  }

  if desired <= 100.0 {
    if candidate <= desired {
      return (0, (desired - candidate).abs());
    }
    return (1, (candidate - desired).abs());
  }

  // desired > 100
  if candidate >= desired {
    (0, (candidate - desired).abs())
  } else {
    (1, (desired - candidate).abs())
  }
}

pub(crate) fn font_is_emoji_font(db: &FontDatabase, id: Option<ID>, font: &LoadedFont) -> bool {
  if let Some(id) = id {
    if let Some(is_color) = db.is_color_capable_font(id) {
      return is_color;
    }
  }

  let name = font.family.to_lowercase();
  name.contains("emoji")
    || name.contains("color")
    || name.contains("twemoji")
    || name.contains("symbola")
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum EmojiPreference {
  PreferEmoji,
  AvoidEmoji,
  Neutral,
}

#[derive(Default)]
pub(crate) struct FontPreferencePicker {
  prefer_emoji: bool,
  avoid_emoji: bool,
  first_emoji: Option<(ResolvedFont, usize)>,
  first_text: Option<(ResolvedFont, usize)>,
  first_emoji_any: Option<(ResolvedFont, usize)>,
  first_text_any: Option<(ResolvedFont, usize)>,
  order: usize,
}

impl FontPreferencePicker {
  pub(crate) fn new(pref: EmojiPreference) -> Self {
    Self {
      prefer_emoji: matches!(pref, EmojiPreference::PreferEmoji),
      avoid_emoji: matches!(pref, EmojiPreference::AvoidEmoji),
      ..Self::default()
    }
  }

  pub(crate) fn bump_order(&mut self) -> usize {
    let idx = self.order;
    self.order += 1;
    idx
  }

  fn record_any(&mut self, font: &ResolvedFont, is_emoji_font: bool, idx: usize) {
    if is_emoji_font {
      if self.first_emoji_any.is_none() {
        self.first_emoji_any = Some((font.clone(), idx));
      }
    } else if self.first_text_any.is_none() {
      self.first_text_any = Some((font.clone(), idx));
    }
  }

  pub(crate) fn consider(
    &mut self,
    font: ResolvedFont,
    is_emoji_font: bool,
    idx: usize,
  ) -> Option<ResolvedFont> {
    if is_emoji_font {
      if self.first_emoji.is_none() {
        self.first_emoji = Some((font.clone(), idx));
      }
      if self.prefer_emoji && !self.avoid_emoji {
        return Some(font);
      }
      if !self.prefer_emoji && !self.avoid_emoji {
        return Some(font);
      }
      // avoid_emoji => keep as fallback if nothing else has a glyph
    } else {
      if self.first_text.is_none() {
        self.first_text = Some((font.clone(), idx));
      }
      if self.avoid_emoji {
        return Some(font);
      }
      if !self.prefer_emoji {
        return Some(font);
      }
      // prefer_emoji => keep searching for an emoji font with coverage
    }

    None
  }

  pub(crate) fn finish(&mut self) -> Option<ResolvedFont> {
    let first_emoji = self
      .first_emoji
      .take()
      .or_else(|| self.first_emoji_any.take());
    let first_text = self
      .first_text
      .take()
      .or_else(|| self.first_text_any.take());

    if self.prefer_emoji && !self.avoid_emoji {
      first_emoji
        .map(|(f, _)| f)
        .or_else(|| first_text.map(|(f, _)| f))
    } else if self.avoid_emoji {
      first_text
        .map(|(f, _)| f)
        .or_else(|| first_emoji.map(|(f, _)| f))
    } else {
      match (first_text, first_emoji) {
        (Some((text, ti)), Some((emoji, ei))) => {
          if ti <= ei {
            Some(text)
          } else {
            Some(emoji)
          }
        }
        (Some((text, _)), None) => Some(text),
        (None, Some((emoji, _))) => Some(emoji),
        (None, None) => None,
      }
    }
  }
}

pub(crate) fn emoji_preference_for_char(ch: char, variant: FontVariantEmoji) -> EmojiPreference {
  match variant {
    FontVariantEmoji::Emoji => EmojiPreference::PreferEmoji,
    FontVariantEmoji::Text => EmojiPreference::AvoidEmoji,
    FontVariantEmoji::Unicode => {
      if emoji::is_emoji_presentation(ch) {
        EmojiPreference::PreferEmoji
      } else {
        EmojiPreference::AvoidEmoji
      }
    }
    FontVariantEmoji::Normal => {
      if emoji::is_emoji_presentation(ch) {
        EmojiPreference::PreferEmoji
      } else {
        EmojiPreference::Neutral
      }
    }
  }
}

pub(crate) fn emoji_preference_with_selector(
  ch: char,
  next: Option<char>,
  variant: FontVariantEmoji,
) -> EmojiPreference {
  if let Some(sel) = next {
    if sel == '\u{FE0F}' {
      return EmojiPreference::PreferEmoji;
    }
    if sel == '\u{FE0E}' {
      return EmojiPreference::AvoidEmoji;
    }
  }

  let base_pref = emoji_preference_for_char(ch, variant);

  if let Some('\u{200d}') = next {
    if emoji::is_emoji(ch) || emoji::is_emoji_presentation(ch) {
      return EmojiPreference::PreferEmoji;
    }
  }

  base_pref
}

fn font_supports_all_chars(font: &LoadedFont, chars: &[char]) -> bool {
  if chars.is_empty() {
    return true;
  }

  let face = match ParserFace::parse(&font.data, font.index) {
    Ok(face) => face,
    Err(_) => return false,
  };
  chars.iter().all(|c| face.glyph_index(*c).is_some())
}

#[allow(clippy::cognitive_complexity)]
pub(crate) fn resolve_font_for_char<C: FontResolverContext>(
  ch: char,
  families: &[FamilyEntry],
  weight: u16,
  style: FontStyle,
  oblique_angle: Option<f32>,
  stretch: DbFontStretch,
  context: &C,
  picker: &mut FontPreferencePicker,
) -> Option<ResolvedFont> {
  let db = context.database();
  let is_emoji = FontDatabase::is_emoji(ch);
  let weight_preferences = weight_preference_order(weight);
  let slope_preferences = slope_preference_order(style);
  let stretch_preferences = stretch_preference_order(stretch);
  let math_families = context.math_family_names();
  for entry in families {
    if let FamilyEntry::Generic(GenericFamily::Math) = entry {
      for family in &math_families {
        if let Some(font) =
          context.match_web_font_for_char(family, weight, style, stretch, oblique_angle, ch)
        {
          return Some(ResolvedFont { font, id: None });
        }
        for stretch_choice in &stretch_preferences {
          for slope in slope_preferences {
            for weight_choice in &weight_preferences {
              let query = fontdb::Query {
                families: &[Family::Name(family.as_str())],
                weight: fontdb::Weight(*weight_choice),
                stretch: (*stretch_choice).into(),
                style: (*slope).into(),
              };
              if let Some(id) = db.inner().query(&query) {
                if let Some(font) = db.load_font(id) {
                  let resolved = ResolvedFont { font, id: Some(id) };
                  let is_emoji_font = font_is_emoji_font(db, resolved.id, &resolved.font);
                  let idx = picker.bump_order();
                  picker.record_any(&resolved, is_emoji_font, idx);
                  if db.has_glyph_cached(id, ch) {
                    if let Some(font) = picker.consider(resolved, is_emoji_font, idx) {
                      return Some(font);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    if let FamilyEntry::Named(name) = entry {
      if let Some(font) =
        context.match_web_font_for_char(name, weight, style, stretch, oblique_angle, ch)
      {
        let resolved = ResolvedFont { font, id: None };
        return Some(resolved);
      }
      if context.is_web_family_declared(name) {
        continue;
      }
    }

    for stretch_choice in &stretch_preferences {
      for slope in slope_preferences {
        for weight_choice in &weight_preferences {
          let query = match entry {
            FamilyEntry::Named(name) => fontdb::Query {
              families: &[Family::Name(name)],
              weight: fontdb::Weight(*weight_choice),
              stretch: (*stretch_choice).into(),
              style: (*slope).into(),
            },
            FamilyEntry::Generic(generic) => fontdb::Query {
              families: &[generic.to_fontdb()],
              weight: fontdb::Weight(*weight_choice),
              stretch: (*stretch_choice).into(),
              style: (*slope).into(),
            },
          };

          if let Some(id) = db.inner().query(&query) {
            if let Some(font) = db.load_font(id) {
              let resolved = ResolvedFont { font, id: Some(id) };
              let is_emoji_font = font_is_emoji_font(db, resolved.id, &resolved.font);
              let idx = picker.bump_order();
              picker.record_any(&resolved, is_emoji_font, idx);
              if db.has_glyph_cached(id, ch) {
                if let Some(font) = picker.consider(resolved, is_emoji_font, idx) {
                  return Some(font);
                }
              }
            }
          }
        }
      }
    }

    if let FamilyEntry::Generic(generic) = entry {
      for name in generic.fallback_families() {
        for weight_choice in &weight_preferences {
          for slope in slope_preferences {
            for stretch_choice in &stretch_preferences {
              let query = fontdb::Query {
                families: &[Family::Name(name)],
                weight: fontdb::Weight(*weight_choice),
                stretch: (*stretch_choice).into(),
                style: (*slope).into(),
              };
              if let Some(id) = db.inner().query(&query) {
                if let Some(font) = db.load_font(id) {
                  let resolved = ResolvedFont { font, id: Some(id) };
                  let is_emoji_font = font_is_emoji_font(db, resolved.id, &resolved.font);
                  let idx = picker.bump_order();
                  picker.record_any(&resolved, is_emoji_font, idx);
                  if db.has_glyph_cached(id, ch) {
                    if let Some(font) = picker.consider(resolved, is_emoji_font, idx) {
                      return Some(font);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  if is_emoji && !picker.avoid_emoji {
    for id in db.find_emoji_fonts() {
      if let Some(font) = db.load_font(id) {
        let resolved = ResolvedFont { font, id: Some(id) };
        let idx = picker.bump_order();
        picker.record_any(&resolved, true, idx);
        if db.has_glyph_cached(id, ch) {
          if let Some(font) = picker.consider(resolved, true, idx) {
            return Some(font);
          }
        }
      }
    }
  }

  for face in db.faces() {
    if let Some(font) = db.load_font(face.id) {
      let resolved = ResolvedFont {
        font,
        id: Some(face.id),
      };
      let is_emoji_font = font_is_emoji_font(db, resolved.id, &resolved.font);
      let idx = picker.bump_order();
      picker.record_any(&resolved, is_emoji_font, idx);
      if db.has_glyph_cached(face.id, ch) {
        if let Some(font) = picker.consider(resolved, is_emoji_font, idx) {
          return Some(font);
        }
      }
    }
  }

  picker.finish()
}

#[allow(clippy::cognitive_complexity)]
pub(crate) fn resolve_font_for_cluster<C: FontResolverContext>(
  base_char: char,
  coverage_chars: &[char],
  require_base_glyph: bool,
  families: &[FamilyEntry],
  weight: u16,
  style: FontStyle,
  oblique_angle: Option<f32>,
  stretch: DbFontStretch,
  context: &C,
  emoji_pref: EmojiPreference,
) -> Option<ResolvedFont> {
  let db = context.database();
  let is_emoji = FontDatabase::is_emoji(base_char);
  let weight_preferences = weight_preference_order(weight);
  let slope_preferences = slope_preference_order(style);
  let stretch_preferences = stretch_preference_order(stretch);
  let math_families = context.math_family_names();
  let mut picker = FontPreferencePicker::new(emoji_pref);

  let mut needed: Vec<char> = coverage_chars.iter().copied().collect();
  needed.sort_unstable();
  needed.dedup();
  if needed.is_empty() && require_base_glyph {
    needed.push(base_char);
  }

  let base_supported = |id: fontdb::ID| !require_base_glyph || db.has_glyph_cached(id, base_char);
  let covers_needed = |id: fontdb::ID| needed.iter().all(|c| db.has_glyph_cached(id, *c));

  for entry in families {
    if let FamilyEntry::Generic(GenericFamily::Math) = entry {
      for family in &math_families {
        if let Some(font) =
          context.match_web_font_for_char(family, weight, style, stretch, oblique_angle, base_char)
        {
          if require_base_glyph && !font_supports_all_chars(&font, &[base_char]) {
            continue;
          }
          let resolved = ResolvedFont { font, id: None };
          let is_emoji_font = font_is_emoji_font(db, resolved.id, &resolved.font);
          let idx = picker.bump_order();
          picker.record_any(&resolved, is_emoji_font, idx);
          if needed.is_empty() || font_supports_all_chars(&resolved.font, &needed) {
            if let Some(font) = picker.consider(resolved, is_emoji_font, idx) {
              return Some(font);
            }
          }
        }
        for stretch_choice in &stretch_preferences {
          for slope in slope_preferences {
            for weight_choice in &weight_preferences {
              let query = fontdb::Query {
                families: &[Family::Name(family.as_str())],
                weight: fontdb::Weight(*weight_choice),
                stretch: (*stretch_choice).into(),
                style: (*slope).into(),
              };
              if let Some(id) = db.inner().query(&query) {
                if let Some(font) = db.load_font(id) {
                  if !base_supported(id) {
                    continue;
                  }
                  let resolved = ResolvedFont { font, id: Some(id) };
                  let is_emoji_font = font_is_emoji_font(db, resolved.id, &resolved.font);
                  let idx = picker.bump_order();
                  picker.record_any(&resolved, is_emoji_font, idx);
                  if needed.is_empty() || covers_needed(id) {
                    if let Some(font) = picker.consider(resolved, is_emoji_font, idx) {
                      return Some(font);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    if let FamilyEntry::Named(name) = entry {
      if let Some(font) =
        context.match_web_font_for_char(name, weight, style, stretch, oblique_angle, base_char)
      {
        if require_base_glyph && !font_supports_all_chars(&font, &[base_char]) {
          continue;
        }
        let resolved = ResolvedFont { font, id: None };
        let is_emoji_font = font_is_emoji_font(db, resolved.id, &resolved.font);
        let idx = picker.bump_order();
        picker.record_any(&resolved, is_emoji_font, idx);
        if needed.is_empty() || font_supports_all_chars(&resolved.font, &needed) {
          if let Some(font) = picker.consider(resolved, is_emoji_font, idx) {
            return Some(font);
          }
        }
      }
      if context.is_web_family_declared(name) {
        continue;
      }
    }

    for stretch_choice in &stretch_preferences {
      for slope in slope_preferences {
        for weight_choice in &weight_preferences {
          let query = match entry {
            FamilyEntry::Named(name) => fontdb::Query {
              families: &[Family::Name(name)],
              weight: fontdb::Weight(*weight_choice),
              stretch: (*stretch_choice).into(),
              style: (*slope).into(),
            },
            FamilyEntry::Generic(generic) => fontdb::Query {
              families: &[generic.to_fontdb()],
              weight: fontdb::Weight(*weight_choice),
              stretch: (*stretch_choice).into(),
              style: (*slope).into(),
            },
          };

          if let Some(id) = db.inner().query(&query) {
            if let Some(font) = db.load_font(id) {
              if !base_supported(id) {
                continue;
              }
              let resolved = ResolvedFont { font, id: Some(id) };
              let is_emoji_font = font_is_emoji_font(db, resolved.id, &resolved.font);
              let idx = picker.bump_order();
              picker.record_any(&resolved, is_emoji_font, idx);
              if needed.is_empty() || covers_needed(id) {
                if let Some(font) = picker.consider(resolved, is_emoji_font, idx) {
                  return Some(font);
                }
              }
            }
          }
        }
      }
    }

    if let FamilyEntry::Generic(generic) = entry {
      for name in generic.fallback_families() {
        for weight_choice in &weight_preferences {
          for slope in slope_preferences {
            for stretch_choice in &stretch_preferences {
              let query = fontdb::Query {
                families: &[Family::Name(name)],
                weight: fontdb::Weight(*weight_choice),
                stretch: (*stretch_choice).into(),
                style: (*slope).into(),
              };
              if let Some(id) = db.inner().query(&query) {
                if let Some(font) = db.load_font(id) {
                  if !base_supported(id) {
                    continue;
                  }
                  let resolved = ResolvedFont { font, id: Some(id) };
                  let is_emoji_font = font_is_emoji_font(db, resolved.id, &resolved.font);
                  let idx = picker.bump_order();
                  picker.record_any(&resolved, is_emoji_font, idx);
                  if needed.is_empty() || covers_needed(id) {
                    if let Some(font) = picker.consider(resolved, is_emoji_font, idx) {
                      return Some(font);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  if is_emoji && !picker.avoid_emoji {
    for id in db.find_emoji_fonts() {
      if let Some(font) = db.load_font(id) {
        if !base_supported(id) {
          continue;
        }
        let resolved = ResolvedFont { font, id: Some(id) };
        let idx = picker.bump_order();
        picker.record_any(&resolved, true, idx);
        if needed.is_empty() || covers_needed(id) {
          if let Some(font) = picker.consider(resolved, true, idx) {
            return Some(font);
          }
        }
      }
    }
  }

  for face in db.faces() {
    if let Some(font) = db.load_font(face.id) {
      if !base_supported(face.id) {
        continue;
      }
      let resolved = ResolvedFont {
        font,
        id: Some(face.id),
      };
      let is_emoji_font = font_is_emoji_font(db, resolved.id, &resolved.font);
      let idx = picker.bump_order();
      picker.record_any(&resolved, is_emoji_font, idx);
      if needed.is_empty() || covers_needed(face.id) {
        if let Some(font) = picker.consider(resolved, is_emoji_font, idx) {
          return Some(font);
        }
      }
    }
  }

  picker.finish()
}
