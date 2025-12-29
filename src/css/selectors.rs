//! CSS Selector support
//!
//! Implements selector parsing and matching using the selectors crate.

use super::types::CssString;
use crate::dom::AssignedSlot;
use crate::dom::DomNode;
use crate::error::RenderError;
use cssparser::ParseError;
use cssparser::Parser;
use cssparser::ToCss;
use cssparser::Token;
use selectors::parser::Selector;
use selectors::parser::SelectorImpl;
use selectors::parser::SelectorList;
use selectors::parser::SelectorParseErrorKind;
use selectors::parser::{Combinator, RelativeSelector, RelativeSelectorMatchHint};
use selectors::OpaqueElement;
use std::collections::HashMap;
use std::fmt;

/// Direction keyword for :dir()
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextDirection {
  Ltr,
  Rtl,
}

// ============================================================================
// Selector implementation for FastRender
// ============================================================================

/// Our custom SelectorImpl for FastRender
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FastRenderSelectorImpl;

/// Additional per-match context needed for shadow-aware selector evaluation.
#[derive(Debug, Default)]
pub struct ShadowMatchData<'a> {
  /// The shadow host for the stylesheet being matched, or None for document styles.
  pub shadow_host: Option<OpaqueElement>,
  /// Mapping from slot elements to their assigned nodes for ::slotted() resolution.
  pub slot_map: Option<&'a SlotAssignmentMap<'a>>,
  /// Exported part mappings for resolving ::part() across shadow boundaries.
  pub part_export_map: Option<&'a PartExportMap>,
  /// Deferred error from deadline checks performed during selector matching.
  pub deadline_error: Option<RenderError>,
}

impl<'a> ShadowMatchData<'a> {
  pub fn for_document() -> Self {
    Self::default()
  }

  pub fn for_shadow_host(shadow_host: OpaqueElement) -> Self {
    Self {
      shadow_host: Some(shadow_host),
      ..Self::default()
    }
  }

  pub fn with_slot_map(mut self, slot_map: &'a SlotAssignmentMap<'a>) -> Self {
    self.slot_map = Some(slot_map);
    self
  }

  pub fn with_part_export_map(mut self, part_export_map: Option<&'a PartExportMap>) -> Self {
    self.part_export_map = part_export_map;
    self
  }

  pub fn record_deadline_error(&mut self, err: RenderError) {
    if self.deadline_error.is_none() {
      self.deadline_error = Some(err);
    }
  }
}

/// Mapping helpers for shadow slot assignments during selector matching.
#[derive(Debug, Clone)]
pub struct SlotAssignmentMap<'a> {
  pub slot_to_nodes: HashMap<usize, Vec<usize>>,
  pub node_to_slot: HashMap<usize, AssignedSlot>,
  pub slot_ancestors: HashMap<usize, Vec<&'a DomNode>>,
  pub id_to_node: HashMap<usize, *const DomNode>,
  pub node_to_id: HashMap<*const DomNode, usize>,
}

#[derive(Debug, Clone, Copy)]
pub struct AssignedSlotRef<'a> {
  pub slot: &'a DomNode,
  pub ancestors: &'a [&'a DomNode],
  pub shadow_root_id: usize,
}

impl<'a> SlotAssignmentMap<'a> {
  pub fn new(
    node_to_id: &HashMap<*const DomNode, usize>,
    id_to_node: &HashMap<usize, *const DomNode>,
  ) -> Self {
    Self {
      slot_to_nodes: HashMap::new(),
      node_to_slot: HashMap::new(),
      slot_ancestors: HashMap::new(),
      id_to_node: id_to_node.clone(),
      node_to_id: node_to_id.clone(),
    }
  }

  pub fn add_slot(
    &mut self,
    slot: &'a DomNode,
    ancestors: Vec<&'a DomNode>,
    assigned_nodes: Vec<&'a DomNode>,
    shadow_root_id: usize,
  ) {
    let Some(slot_id) = self.slot_id(slot) else {
      return;
    };

    let assigned_ids: Vec<usize> = assigned_nodes
      .iter()
      .filter_map(|node| self.node_id(node))
      .collect();
    if assigned_ids.is_empty() {
      return;
    }

    self.slot_ancestors.entry(slot_id).or_insert(ancestors);
    let slot_name = slot.get_attribute_ref("name").unwrap_or("").to_string();
    for node_id in assigned_ids.iter().copied() {
      self.node_to_slot.insert(
        node_id,
        AssignedSlot {
          slot_name: slot_name.clone(),
          slot_node_id: slot_id,
          shadow_root_id,
        },
      );
    }

    self.slot_to_nodes.insert(slot_id, assigned_ids);
  }

  pub fn slot_id(&self, slot: &DomNode) -> Option<usize> {
    self.node_to_id.get(&(slot as *const DomNode)).copied()
  }

  pub fn node_id(&self, node: &DomNode) -> Option<usize> {
    self.node_to_id.get(&(node as *const DomNode)).copied()
  }

  pub fn assigned_node_ids(&self, slot_id: usize) -> Option<&[usize]> {
    self
      .slot_to_nodes
      .get(&slot_id)
      .map(|nodes| nodes.as_slice())
  }

  pub fn node_for_id(&self, node_id: usize) -> Option<&'a DomNode> {
    self.id_to_node.get(&node_id).map(|ptr| unsafe { &**ptr })
  }

  pub fn assigned_slot(&'a self, node: &DomNode) -> Option<AssignedSlotRef<'a>> {
    let node_id = self.node_id(node)?;
    let slot = self.node_to_slot.get(&node_id)?;
    let slot_node = self.node_for_id(slot.slot_node_id)?;
    let ancestors = self.slot_ancestors.get(&slot.slot_node_id)?;
    Some(AssignedSlotRef {
      slot: slot_node,
      ancestors,
      shadow_root_id: slot.shadow_root_id,
    })
  }
}

/// Mapping from shadow hosts to their `exportparts` mappings.
#[derive(Debug, Default, Clone)]
pub struct PartExportMap {
  hosts: HashMap<usize, HashMap<String, Vec<usize>>>,
}

impl PartExportMap {
  pub fn exports_for_host(&self, host: usize) -> Option<&HashMap<String, Vec<usize>>> {
    self.hosts.get(&host)
  }

  pub fn insert_host_exports(&mut self, host: usize, exports: HashMap<String, Vec<usize>>) {
    self.hosts.insert(host, exports);
  }
}

impl SelectorImpl for FastRenderSelectorImpl {
  type AttrValue = CssString;
  type BorrowedLocalName = str;
  type BorrowedNamespaceUrl = str;
  type ExtraMatchingData<'a> = ShadowMatchData<'a>;
  type Identifier = CssString;
  type LocalName = CssString;
  type NamespacePrefix = CssString;
  type NamespaceUrl = CssString;
  type NonTSPseudoClass = PseudoClass;
  type PseudoElement = PseudoElement;

  fn should_collect_attr_hash(_name: &Self::LocalName) -> bool {
    // Attribute selectors are indexed in RuleIndex; allow bloom filters to prune on them.
    true
  }
}

// ============================================================================
// Pseudo-classes
// ============================================================================

/// Pseudo-classes we support
#[derive(Clone, PartialEq, Eq)]
pub enum PseudoClass {
  Has(Box<[RelativeSelector<FastRenderSelectorImpl>]>),
  Host(Option<SelectorList<FastRenderSelectorImpl>>),
  HostContext(SelectorList<FastRenderSelectorImpl>),
  Root,
  FirstChild,
  LastChild,
  NthChild(i32, i32, Option<SelectorList<FastRenderSelectorImpl>>), // an + b
  NthLastChild(i32, i32, Option<SelectorList<FastRenderSelectorImpl>>),
  OnlyChild,
  FirstOfType,
  LastOfType,
  OnlyOfType,
  NthOfType(i32, i32),
  NthLastOfType(i32, i32),
  Lang(Vec<String>),
  Dir(TextDirection),
  AnyLink,
  Target,
  TargetWithin,
  Scope,
  Empty,
  Hover,
  Active,
  Focus,
  FocusWithin,
  FocusVisible,
  Disabled,
  Enabled,
  Required,
  Optional,
  Valid,
  Invalid,
  InRange,
  OutOfRange,
  ReadOnly,
  ReadWrite,
  PlaceholderShown,
  Autofill,
  Checked,
  Indeterminate,
  Default,
  Link,
  Visited,
}

impl fmt::Debug for PseudoClass {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      PseudoClass::Has(relative) => {
        let selectors: Vec<String> = relative
          .iter()
          .map(|sel| sel.selector.to_css_string())
          .collect();
        f.debug_tuple("Has").field(&selectors).finish()
      }
      _ => f.write_str(&self.to_css_string()),
    }
  }
}

impl selectors::parser::NonTSPseudoClass for PseudoClass {
  type Impl = FastRenderSelectorImpl;

  fn is_active_or_hover(&self) -> bool {
    matches!(self, PseudoClass::Active | PseudoClass::Hover)
  }

  fn is_user_action_state(&self) -> bool {
    matches!(
      self,
      PseudoClass::Hover
        | PseudoClass::Active
        | PseudoClass::Focus
        | PseudoClass::FocusWithin
        | PseudoClass::FocusVisible
    )
  }

  fn specificity(&self) -> u32 {
    const PSEUDO_CLASS_SPECIFICITY: u32 = 1 << 10;
    let argument_specificity = |selectors: &SelectorList<FastRenderSelectorImpl>| {
      selectors
        .slice()
        .iter()
        .map(|selector| selector.specificity())
        .max()
        .unwrap_or(0)
    };
    match self {
      PseudoClass::Has(relative) => relative
        .iter()
        .map(|selector| selector.selector.specificity())
        .max()
        .unwrap_or(0),
      PseudoClass::Host(None) => PSEUDO_CLASS_SPECIFICITY,
      PseudoClass::Host(Some(selectors)) => {
        PSEUDO_CLASS_SPECIFICITY + argument_specificity(selectors)
      }
      PseudoClass::HostContext(selectors) => {
        PSEUDO_CLASS_SPECIFICITY + argument_specificity(selectors)
      }
      _ => PSEUDO_CLASS_SPECIFICITY, // Pseudo-classes have class-level specificity by default.
    }
  }

  fn matches_featureless_host(&self) -> selectors::parser::MatchesFeaturelessHost {
    match self {
      PseudoClass::Host(_) | PseudoClass::HostContext(_) => {
        selectors::parser::MatchesFeaturelessHost::Only
      }
      _ => selectors::parser::MatchesFeaturelessHost::Never,
    }
  }
}

fn write_nth_pseudo<W: fmt::Write>(
  dest: &mut W,
  name: &str,
  a: i32,
  b: i32,
  of: &Option<SelectorList<FastRenderSelectorImpl>>,
) -> fmt::Result {
  write!(dest, "{}({}n+{}", name, a, b)?;
  if let Some(selectors) = of {
    dest.write_str(" of ")?;
    selectors.to_css(dest)?;
  }
  dest.write_str(")")
}

impl ToCss for PseudoClass {
  fn to_css<W>(&self, dest: &mut W) -> fmt::Result
  where
    W: fmt::Write,
  {
    match self {
      PseudoClass::Has(selectors) => {
        dest.write_str(":has(")?;
        let mut first = true;
        for rel in selectors.iter() {
          if !first {
            dest.write_str(", ")?;
          }
          first = false;
          rel.selector.to_css(dest)?;
        }
        dest.write_str(")")
      }
      PseudoClass::Host(None) => dest.write_str(":host"),
      PseudoClass::Host(Some(selectors)) => {
        dest.write_str(":host(")?;
        selectors.to_css(dest)?;
        dest.write_str(")")
      }
      PseudoClass::HostContext(selectors) => {
        dest.write_str(":host-context(")?;
        selectors.to_css(dest)?;
        dest.write_str(")")
      }
      PseudoClass::Root => dest.write_str(":root"),
      PseudoClass::FirstChild => dest.write_str(":first-child"),
      PseudoClass::LastChild => dest.write_str(":last-child"),
      PseudoClass::NthChild(a, b, of) => write_nth_pseudo(dest, ":nth-child", *a, *b, of),
      PseudoClass::NthLastChild(a, b, of) => write_nth_pseudo(dest, ":nth-last-child", *a, *b, of),
      PseudoClass::OnlyChild => dest.write_str(":only-child"),
      PseudoClass::FirstOfType => dest.write_str(":first-of-type"),
      PseudoClass::LastOfType => dest.write_str(":last-of-type"),
      PseudoClass::OnlyOfType => dest.write_str(":only-of-type"),
      PseudoClass::NthOfType(a, b) => write!(dest, ":nth-of-type({}n+{})", a, b),
      PseudoClass::NthLastOfType(a, b) => write!(dest, ":nth-last-of-type({}n+{})", a, b),
      PseudoClass::Lang(langs) => {
        dest.write_str(":lang(")?;
        for (i, lang) in langs.iter().enumerate() {
          if i > 0 {
            dest.write_str(", ")?;
          }
          dest.write_str(lang)?;
        }
        dest.write_str(")")
      }
      PseudoClass::Dir(dir) => match dir {
        TextDirection::Ltr => dest.write_str(":dir(ltr)"),
        TextDirection::Rtl => dest.write_str(":dir(rtl)"),
      },
      PseudoClass::AnyLink => dest.write_str(":any-link"),
      PseudoClass::Target => dest.write_str(":target"),
      PseudoClass::TargetWithin => dest.write_str(":target-within"),
      PseudoClass::Scope => dest.write_str(":scope"),
      PseudoClass::Empty => dest.write_str(":empty"),
      PseudoClass::Hover => dest.write_str(":hover"),
      PseudoClass::Active => dest.write_str(":active"),
      PseudoClass::Focus => dest.write_str(":focus"),
      PseudoClass::FocusWithin => dest.write_str(":focus-within"),
      PseudoClass::FocusVisible => dest.write_str(":focus-visible"),
      PseudoClass::Disabled => dest.write_str(":disabled"),
      PseudoClass::Enabled => dest.write_str(":enabled"),
      PseudoClass::Required => dest.write_str(":required"),
      PseudoClass::Optional => dest.write_str(":optional"),
      PseudoClass::Valid => dest.write_str(":valid"),
      PseudoClass::Invalid => dest.write_str(":invalid"),
      PseudoClass::InRange => dest.write_str(":in-range"),
      PseudoClass::OutOfRange => dest.write_str(":out-of-range"),
      PseudoClass::Indeterminate => dest.write_str(":indeterminate"),
      PseudoClass::Default => dest.write_str(":default"),
      PseudoClass::ReadOnly => dest.write_str(":read-only"),
      PseudoClass::ReadWrite => dest.write_str(":read-write"),
      PseudoClass::PlaceholderShown => dest.write_str(":placeholder-shown"),
      PseudoClass::Autofill => dest.write_str(":autofill"),
      PseudoClass::Checked => dest.write_str(":checked"),
      PseudoClass::Link => dest.write_str(":link"),
      PseudoClass::Visited => dest.write_str(":visited"),
    }
  }
}

// ============================================================================
// Pseudo-elements
// ============================================================================

/// Pseudo-elements we support
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PseudoElement {
  Before,
  After,
  FirstLine,
  FirstLetter,
  Marker,
  Backdrop,
  Slotted(Box<[Selector<FastRenderSelectorImpl>]>),
  Part(CssString),
}

impl selectors::parser::PseudoElement for PseudoElement {
  type Impl = FastRenderSelectorImpl;
}

impl std::hash::Hash for PseudoElement {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    std::mem::discriminant(self).hash(state);
    match self {
      PseudoElement::Slotted(selectors) => {
        selectors.len().hash(state);
        for selector in selectors.iter() {
          selector.to_css_string().hash(state);
        }
      }
      PseudoElement::Part(name) => name.hash(state),
      _ => {}
    }
  }
}

impl PseudoElement {
  /// Pseudo-elements that generate their own boxes (::before/::after/::marker/::backdrop).
  pub fn is_generated_box(&self) -> bool {
    matches!(
      self,
      PseudoElement::Before
        | PseudoElement::After
        | PseudoElement::Marker
        | PseudoElement::Backdrop
    )
  }
}

impl ToCss for PseudoElement {
  fn to_css<W>(&self, dest: &mut W) -> fmt::Result
  where
    W: fmt::Write,
  {
    match self {
      PseudoElement::Before => dest.write_str("::before"),
      PseudoElement::After => dest.write_str("::after"),
      PseudoElement::FirstLine => dest.write_str("::first-line"),
      PseudoElement::FirstLetter => dest.write_str("::first-letter"),
      PseudoElement::Marker => dest.write_str("::marker"),
      PseudoElement::Backdrop => dest.write_str("::backdrop"),
      PseudoElement::Slotted(selectors) => {
        dest.write_str("::slotted(")?;
        for (i, selector) in selectors.iter().enumerate() {
          if i > 0 {
            dest.write_str(", ")?;
          }
          selector.to_css(dest)?;
        }
        dest.write_str(")")
      }
      PseudoElement::Part(name) => {
        dest.write_str("::part(")?;
        name.to_css(dest)?;
        dest.write_str(")")
      }
    }
  }
}

// ============================================================================
// Pseudo-class parser
// ============================================================================

/// Custom parser for pseudo-classes
/// Public parser entrypoint for selector parsing.
///
/// This is exposed so fuzzers and tools outside the crate can reuse the
/// canonical parser configuration without duplicating selector setup.
pub struct PseudoClassParser;

impl<'i> selectors::parser::Parser<'i> for PseudoClassParser {
  type Error = SelectorParseErrorKind<'i>;
  type Impl = FastRenderSelectorImpl;

  fn parse_nth_child_of(&self) -> bool {
    true
  }

  fn parse_non_ts_pseudo_class(
    &self,
    _location: cssparser::SourceLocation,
    name: cssparser::CowRcStr<'i>,
  ) -> std::result::Result<PseudoClass, ParseError<'i, Self::Error>> {
    let lowered = name.to_ascii_lowercase();
    match lowered.as_str() {
      "host" => Ok(PseudoClass::Host(None)),
      "root" => Ok(PseudoClass::Root),
      "first-child" => Ok(PseudoClass::FirstChild),
      "last-child" => Ok(PseudoClass::LastChild),
      "only-child" => Ok(PseudoClass::OnlyChild),
      "first-of-type" => Ok(PseudoClass::FirstOfType),
      "last-of-type" => Ok(PseudoClass::LastOfType),
      "only-of-type" => Ok(PseudoClass::OnlyOfType),
      "empty" => Ok(PseudoClass::Empty),
      "hover" => Ok(PseudoClass::Hover),
      "active" => Ok(PseudoClass::Active),
      "focus" => Ok(PseudoClass::Focus),
      "focus-within" => Ok(PseudoClass::FocusWithin),
      "focus-visible" => Ok(PseudoClass::FocusVisible),
      "disabled" => Ok(PseudoClass::Disabled),
      "enabled" => Ok(PseudoClass::Enabled),
      "required" => Ok(PseudoClass::Required),
      "optional" => Ok(PseudoClass::Optional),
      "valid" => Ok(PseudoClass::Valid),
      "invalid" => Ok(PseudoClass::Invalid),
      "in-range" => Ok(PseudoClass::InRange),
      "out-of-range" => Ok(PseudoClass::OutOfRange),
      "indeterminate" => Ok(PseudoClass::Indeterminate),
      "default" => Ok(PseudoClass::Default),
      "read-only" => Ok(PseudoClass::ReadOnly),
      "read-write" => Ok(PseudoClass::ReadWrite),
      "placeholder-shown" => Ok(PseudoClass::PlaceholderShown),
      "autofill" => Ok(PseudoClass::Autofill),
      "checked" => Ok(PseudoClass::Checked),
      "link" => Ok(PseudoClass::Link),
      "visited" => Ok(PseudoClass::Visited),
      "any-link" => Ok(PseudoClass::AnyLink),
      "target" => Ok(PseudoClass::Target),
      "target-within" => Ok(PseudoClass::TargetWithin),
      "scope" => Ok(PseudoClass::Scope),
      _ => Err(ParseError {
        kind: cssparser::ParseErrorKind::Custom(
          SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name),
        ),
        location: _location,
      }),
    }
  }

  fn parse_non_ts_functional_pseudo_class<'t>(
    &self,
    name: cssparser::CowRcStr<'i>,
    parser: &mut Parser<'i, 't>,
    _is_starting_single_colon: bool,
  ) -> std::result::Result<PseudoClass, ParseError<'i, Self::Error>> {
    let lowered = name.to_ascii_lowercase();
    match lowered.as_str() {
      "host" => {
        let selectors = SelectorList::parse(
          &PseudoClassParser,
          parser,
          selectors::parser::ParseRelative::No,
        )
        .map_err(|_| {
          parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(
            name.clone(),
          ))
        })?;
        if selectors.slice().iter().any(selector_has_combinators) {
          return Err(parser.new_custom_error(
            SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name.clone()),
          ));
        }
        Ok(PseudoClass::Host(Some(selectors)))
      }
      "host-context" => {
        let selectors = SelectorList::parse(
          &PseudoClassParser,
          parser,
          selectors::parser::ParseRelative::No,
        )
        .map_err(|_| {
          parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(
            name.clone(),
          ))
        })?;
        Ok(PseudoClass::HostContext(selectors))
      }
      "has" => {
        let list = SelectorList::parse(
          &PseudoClassParser,
          parser,
          selectors::parser::ParseRelative::ForHas,
        )
        .map_err(|_| {
          parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(
            name.clone(),
          ))
        })?;
        let relative = build_relative_selectors(list);
        Ok(PseudoClass::Has(relative))
      }
      "nth-child" => {
        let (a, b, of) = parse_nth_with_of(&name, parser)?;
        Ok(PseudoClass::NthChild(a, b, of))
      }
      "nth-last-child" => {
        let (a, b, of) = parse_nth_with_of(&name, parser)?;
        Ok(PseudoClass::NthLastChild(a, b, of))
      }
      "nth-of-type" => {
        let (a, b) = parse_nth(parser).map_err(|_| {
          parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(
            name.clone(),
          ))
        })?;
        Ok(PseudoClass::NthOfType(a, b))
      }
      "nth-last-of-type" => {
        let (a, b) = parse_nth(parser).map_err(|_| {
          parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(
            name.clone(),
          ))
        })?;
        Ok(PseudoClass::NthLastOfType(a, b))
      }
      "dir" => {
        let dir = match parser.expect_ident() {
          Ok(d) => d,
          Err(_) => {
            return Err(parser.new_custom_error(
              SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name.clone()),
            ))
          }
        };
        let lowered = dir.to_ascii_lowercase();
        match lowered.as_str() {
          "ltr" => Ok(PseudoClass::Dir(TextDirection::Ltr)),
          "rtl" => Ok(PseudoClass::Dir(TextDirection::Rtl)),
          _ => Err(parser.new_custom_error(
            SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name.clone()),
          )),
        }
      }
      "lang" => {
        let mut langs = Vec::new();
        loop {
          let range = match parser.expect_ident_or_string() {
            Ok(r) => r,
            Err(_) => {
              return Err(parser.new_custom_error(
                SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name.clone()),
              ))
            }
          };
          langs.push(range.as_ref().to_ascii_lowercase());
          if parser.try_parse(|p| p.expect_comma()).is_err() {
            break;
          }
        }
        Ok(PseudoClass::Lang(langs))
      }
      _ => Err(
        parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(
          name,
        )),
      ),
    }
  }

  fn parse_pseudo_element(
    &self,
    _location: cssparser::SourceLocation,
    name: cssparser::CowRcStr<'i>,
  ) -> std::result::Result<PseudoElement, ParseError<'i, Self::Error>> {
    let lowered = name.to_ascii_lowercase();
    match lowered.as_str() {
      "before" => Ok(PseudoElement::Before),
      "after" => Ok(PseudoElement::After),
      "first-line" => Ok(PseudoElement::FirstLine),
      "first-letter" => Ok(PseudoElement::FirstLetter),
      "marker" => Ok(PseudoElement::Marker),
      "backdrop" => Ok(PseudoElement::Backdrop),
      _ => Err(ParseError {
        kind: cssparser::ParseErrorKind::Custom(
          SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name),
        ),
        location: _location,
      }),
    }
  }

  fn parse_functional_pseudo_element<'t>(
    &self,
    name: cssparser::CowRcStr<'i>,
    parser: &mut Parser<'i, 't>,
  ) -> std::result::Result<PseudoElement, ParseError<'i, Self::Error>> {
    let lowered = name.to_ascii_lowercase();
    match lowered.as_str() {
      "slotted" => parse_slotted_pseudo_element(parser, &name),
      "part" => parse_part_pseudo_element(parser, &name),
      _ => Err(
        parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(
          name,
        )),
      ),
    }
  }

  fn parse_is_and_where(&self) -> bool {
    // Enable parsing of :is() and :where() pseudo-classes
    true
  }
}

fn build_relative_selectors(
  selector_list: SelectorList<FastRenderSelectorImpl>,
) -> Box<[RelativeSelector<FastRenderSelectorImpl>]> {
  selector_list
    .slice()
    .iter()
    .map(|selector| {
      let mut has_child_or_descendants = false;
      let mut has_adjacent_or_next_siblings = false;
      let mut iter = selector.iter_skip_relative_selector_anchor();

      loop {
        while iter.next().is_some() {}
        match iter.next_sequence() {
          Some(Combinator::Descendant) | Some(Combinator::Child) => {
            has_child_or_descendants = true;
          }
          Some(Combinator::NextSibling) | Some(Combinator::LaterSibling) => {
            has_adjacent_or_next_siblings = true;
          }
          Some(_) => {}
          None => break,
        }
      }

      let match_hint = RelativeSelectorMatchHint::new(
        selector.combinator_at_parse_order(1),
        has_child_or_descendants,
        has_adjacent_or_next_siblings,
      );

      RelativeSelector {
        match_hint,
        selector: selector.clone(),
      }
    })
    .collect::<Vec<_>>()
    .into_boxed_slice()
}

fn parse_slotted_pseudo_element<'i, 't>(
  parser: &mut Parser<'i, 't>,
  name: &cssparser::CowRcStr<'i>,
) -> std::result::Result<PseudoElement, ParseError<'i, SelectorParseErrorKind<'i>>> {
  let list = SelectorList::parse(
    &PseudoClassParser,
    parser,
    selectors::parser::ParseRelative::No,
  )
  .map_err(|_| {
    parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(
      name.clone(),
    ))
  })?;

  let selectors: Vec<_> = list.slice().iter().cloned().collect();
  if selectors.is_empty() || selectors.iter().any(selector_has_combinators) {
    return Err(
      parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(
        name.clone(),
      )),
    );
  }

  Ok(PseudoElement::Slotted(selectors.into_boxed_slice()))
}

fn parse_part_pseudo_element<'i, 't>(
  parser: &mut Parser<'i, 't>,
  name: &cssparser::CowRcStr<'i>,
) -> std::result::Result<PseudoElement, ParseError<'i, SelectorParseErrorKind<'i>>> {
  // Per CSS Shadow Parts, ::part() accepts exactly one <ident>. Reject additional tokens.
  parser.skip_whitespace();
  let ident =
    match parser.expect_ident() {
      Ok(first) => CssString::from(first.as_ref()),
      Err(_) => {
        return Err(parser.new_custom_error(
          SelectorParseErrorKind::UnsupportedPseudoClassOrElement(name.clone()),
        ))
      }
    };

  parser.skip_whitespace();
  if !parser.is_exhausted() {
    return Err(
      parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(
        name.clone(),
      )),
    );
  }

  Ok(PseudoElement::Part(ident))
}

fn selector_has_combinators(selector: &Selector<FastRenderSelectorImpl>) -> bool {
  let mut iter = selector.iter();
  loop {
    while iter.next().is_some() {}
    match iter.next_sequence() {
      Some(_) => return true,
      None => return false,
    }
  }
}

/// Parse nth-child/nth-last-child expressions
fn parse_nth_with_of<'i, 't>(
  name: &cssparser::CowRcStr<'i>,
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<
  (i32, i32, Option<SelectorList<FastRenderSelectorImpl>>),
  ParseError<'i, SelectorParseErrorKind<'i>>,
> {
  let (a, b) = parse_nth(parser).map_err(|_| {
    parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(
      name.clone(),
    ))
  })?;

  let selector_list = if parser.is_exhausted() {
    None
  } else {
    Some(parse_of_selector_list(name, parser)?)
  };

  Ok((a, b, selector_list))
}

fn parse_of_selector_list<'i, 't>(
  name: &cssparser::CowRcStr<'i>,
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<
  SelectorList<FastRenderSelectorImpl>,
  ParseError<'i, SelectorParseErrorKind<'i>>,
> {
  let ident = parser.expect_ident()?.clone();
  if !ident.eq_ignore_ascii_case("of") {
    return Err(
      parser.new_error(cssparser::BasicParseErrorKind::UnexpectedToken(
        Token::Ident(ident),
      )),
    );
  }

  SelectorList::parse(
    &PseudoClassParser,
    parser,
    selectors::parser::ParseRelative::No,
  )
  .map_err(|_| {
    parser.new_custom_error(SelectorParseErrorKind::UnsupportedPseudoClassOrElement(
      name.clone(),
    ))
  })
}

fn parse_nth<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> std::result::Result<(i32, i32), ParseError<'i, ()>> {
  cssparser::parse_nth(parser).map_err(Into::into)
}

#[cfg(test)]
mod tests {
  use super::*;
  use cssparser::ParserInput;
  use cssparser::SourceLocation;
  use cssparser::ToCss;
  use selectors::parser::ParseRelative;
  use selectors::parser::Parser as SelectorParser;

  fn parse(expr: &str) -> (i32, i32) {
    let mut input = ParserInput::new(expr);
    let mut parser = Parser::new(&mut input);
    parse_nth(&mut parser).expect("should parse nth expression")
  }

  #[test]
  fn parses_an_plus_b_syntax() {
    assert_eq!(parse("odd"), (2, 1));
    assert_eq!(parse("even"), (2, 0));
    assert_eq!(parse("2n+1"), (2, 1));
    assert_eq!(parse("-2n+3"), (-2, 3));
    assert_eq!(parse("n"), (1, 0));
    assert_eq!(parse("+n-1"), (1, -1));
    assert_eq!(parse("4"), (0, 4));
    assert_eq!(parse("-5"), (0, -5));
  }

  #[test]
  fn rejects_invalid_nth_expression() {
    let mut input = ParserInput::new("n+");
    let mut parser = Parser::new(&mut input);
    assert!(parse_nth(&mut parser).is_err());
  }

  #[test]
  fn parses_pseudo_classes_case_insensitively() {
    let parser = PseudoClassParser;
    let loc = SourceLocation { line: 0, column: 0 };
    let root = parser
      .parse_non_ts_pseudo_class(loc, cssparser::CowRcStr::from("RoOt"))
      .expect("root pseudo should parse");
    assert_eq!(root, PseudoClass::Root);

    let target_within = parser
      .parse_non_ts_pseudo_class(loc, cssparser::CowRcStr::from("TARGET-WITHIN"))
      .expect("target-within pseudo should parse");
    assert_eq!(target_within, PseudoClass::TargetWithin);

    let mut input = ParserInput::new("2n+1");
    let mut css_parser = Parser::new(&mut input);
    let nth = parser
      .parse_non_ts_functional_pseudo_class(
        cssparser::CowRcStr::from("NTH-CHILD"),
        &mut css_parser,
        false,
      )
      .expect("nth-child pseudo should parse");
    assert!(matches!(nth, PseudoClass::NthChild(_, _, _)));
  }

  #[test]
  fn parses_pseudo_elements_case_insensitively() {
    let parser = PseudoClassParser;
    let loc = SourceLocation { line: 0, column: 0 };
    assert_eq!(
      parser
        .parse_pseudo_element(loc, cssparser::CowRcStr::from("BeFoRe"))
        .expect("before pseudo"),
      PseudoElement::Before
    );
    assert_eq!(
      parser
        .parse_pseudo_element(loc, cssparser::CowRcStr::from("FIRST-LINE"))
        .expect("first-line pseudo"),
      PseudoElement::FirstLine
    );
    assert_eq!(
      parser
        .parse_pseudo_element(loc, cssparser::CowRcStr::from("First-Letter"))
        .expect("first-letter pseudo"),
      PseudoElement::FirstLetter
    );
    assert_eq!(
      parser
        .parse_pseudo_element(loc, cssparser::CowRcStr::from("MARKER"))
        .expect("marker pseudo"),
      PseudoElement::Marker
    );
  }

  #[test]
  fn parses_single_colon_before_as_pseudo_element() {
    let mut input = ParserInput::new("div:before");
    let mut css_parser = Parser::new(&mut input);
    let selector_list = SelectorList::parse(
      &PseudoClassParser,
      &mut css_parser,
      selectors::parser::ParseRelative::No,
    );

    let list = selector_list.expect("should parse selector list");
    let selector = list.slice().first().expect("one selector");
    assert_eq!(selector.pseudo_element(), Some(&PseudoElement::Before));
  }

  #[test]
  fn to_css_serializes_new_pseudo_classes() {
    assert_eq!(PseudoClass::FirstOfType.to_css_string(), ":first-of-type");
    assert_eq!(PseudoClass::LastOfType.to_css_string(), ":last-of-type");
    assert_eq!(PseudoClass::OnlyOfType.to_css_string(), ":only-of-type");
    assert_eq!(PseudoClass::Empty.to_css_string(), ":empty");
    assert_eq!(
      PseudoClass::NthOfType(2, 1).to_css_string(),
      ":nth-of-type(2n+1)"
    );
    assert_eq!(
      PseudoClass::NthLastOfType(-1, 3).to_css_string(),
      ":nth-last-of-type(-1n+3)"
    );
    assert_eq!(
      PseudoClass::Lang(vec!["en".into(), "fr-ca".into()]).to_css_string(),
      ":lang(en, fr-ca)"
    );
    assert_eq!(
      PseudoClass::Dir(TextDirection::Ltr).to_css_string(),
      ":dir(ltr)"
    );
    assert_eq!(PseudoClass::AnyLink.to_css_string(), ":any-link");
    assert_eq!(PseudoClass::Target.to_css_string(), ":target");
    assert_eq!(PseudoClass::TargetWithin.to_css_string(), ":target-within");
    assert_eq!(PseudoClass::Scope.to_css_string(), ":scope");
    assert_eq!(PseudoClass::Disabled.to_css_string(), ":disabled");
    assert_eq!(PseudoClass::Enabled.to_css_string(), ":enabled");
    assert_eq!(PseudoClass::Required.to_css_string(), ":required");
    assert_eq!(PseudoClass::Optional.to_css_string(), ":optional");
    assert_eq!(PseudoClass::Valid.to_css_string(), ":valid");
    assert_eq!(PseudoClass::Invalid.to_css_string(), ":invalid");
    assert_eq!(PseudoClass::InRange.to_css_string(), ":in-range");
    assert_eq!(PseudoClass::OutOfRange.to_css_string(), ":out-of-range");
    assert_eq!(PseudoClass::Indeterminate.to_css_string(), ":indeterminate");
    assert_eq!(PseudoClass::Default.to_css_string(), ":default");
    assert_eq!(PseudoClass::FocusWithin.to_css_string(), ":focus-within");
    assert_eq!(PseudoClass::FocusVisible.to_css_string(), ":focus-visible");
    assert_eq!(PseudoClass::ReadOnly.to_css_string(), ":read-only");
    assert_eq!(PseudoClass::ReadWrite.to_css_string(), ":read-write");
    assert_eq!(
      PseudoClass::PlaceholderShown.to_css_string(),
      ":placeholder-shown"
    );
    assert_eq!(PseudoClass::Autofill.to_css_string(), ":autofill");
  }

  #[test]
  fn parses_shadow_pseudo_elements() {
    let mut input = ParserInput::new("div::slotted(.a)");
    let mut parser = Parser::new(&mut input);
    assert!(SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::No).is_ok());

    let mut input = ParserInput::new("button::part(name)");
    let mut parser = Parser::new(&mut input);
    assert!(SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::No).is_ok());
  }

  #[test]
  fn rejects_part_with_multiple_names() {
    let mut input = ParserInput::new("button::part(name badge)");
    let mut parser = Parser::new(&mut input);
    assert!(SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::No).is_err());
  }

  #[test]
  fn rejects_non_compound_slotted_selector() {
    let mut input = ParserInput::new("div::slotted(.a .b)");
    let mut parser = Parser::new(&mut input);
    assert!(SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::No).is_err());
  }

  #[test]
  fn to_css_serializes_pseudo_elements() {
    let mut input = ParserInput::new(".foo");
    let mut parser = Parser::new(&mut input);
    let selector_list = SelectorList::parse(&PseudoClassParser, &mut parser, ParseRelative::No)
      .expect("should parse selector");
    let selector = selector_list
      .slice()
      .first()
      .expect("expected selector")
      .clone();
    let slotted = PseudoElement::Slotted(vec![selector].into_boxed_slice());
    assert_eq!(slotted.to_css_string(), "::slotted(.foo)");

    let part = PseudoElement::Part(CssString::from("name"));
    assert_eq!(part.to_css_string(), "::part(name)");
  }
}
