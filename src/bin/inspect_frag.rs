mod common;

use clap::Parser;
use common::args::{parse_viewport, MediaPreferenceArgs};
use common::media_prefs::MediaPreferences;
use cssparser::Parser as CssParser;
use cssparser::ParserInput;
use fastrender::api::FastRender;
use fastrender::css::encoding::decode_css_bytes;
use fastrender::css::loader::absolutize_css_urls;
use fastrender::css::loader::extract_css_links;
use fastrender::css::loader::extract_embedded_css_urls;
use fastrender::css::loader::infer_base_url;
use fastrender::css::loader::inject_css_into_html;
use fastrender::css::loader::inline_imports;
use fastrender::css::parser::extract_css;
use fastrender::css::selectors::FastRenderSelectorImpl;
use fastrender::css::selectors::PseudoClassParser;
use fastrender::debug::inspect::InspectQuery;
use fastrender::debug::tree_printer::{JsonExportConfig, TreeJsonExporter};
use fastrender::dom::DomNodeType;
use fastrender::dom::{self};
use fastrender::geometry::Point;
use fastrender::geometry::Rect;
use fastrender::geometry::Size;
use fastrender::image_output::encode_image;
use fastrender::layout::engine::LayoutConfig;
use fastrender::layout::engine::LayoutEngine;
use fastrender::paint::canvas::Canvas;
use fastrender::paint::display_list::Transform3D;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::paint::stacking::creates_stacking_context;
use fastrender::resource::HttpFetcher;
use fastrender::resource::ResourceFetcher;
use fastrender::resource::DEFAULT_ACCEPT_LANGUAGE;
use fastrender::resource::DEFAULT_USER_AGENT;
use fastrender::style::cascade::apply_styles_with_media_and_target;
use fastrender::style::cascade::StyledNode;
use fastrender::style::color::Rgba;
use fastrender::style::computed::Visibility;
use fastrender::style::display::Display;
use fastrender::style::media::MediaType;
use fastrender::style::position::Position;
use fastrender::style::types::Overflow;
use fastrender::style::ComputedStyle;
use fastrender::text::font_loader::FontContext;
use fastrender::text::pipeline::ShapingPipeline;
use fastrender::tree::box_generation::generate_box_tree_with_anonymous_fixup;
use fastrender::tree::box_tree::BoxNode;
use fastrender::tree::box_tree::BoxTree;
use fastrender::tree::fragment_tree::FragmentContent;
use fastrender::tree::fragment_tree::FragmentNode;
use fastrender::tree::fragment_tree::FragmentTree;
use fastrender::OutputFormat;
use selectors::matching::matches_selector;
use selectors::matching::MatchingContext;
use selectors::matching::MatchingForInvalidation;
use selectors::matching::MatchingMode;
use selectors::matching::NeedsSelectorFlags;
use selectors::matching::QuirksMode;
use selectors::matching::SelectorCaches;
use selectors::parser::SelectorList;
use serde_json::Value;
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::Path;
use std::time::Duration;
use url::Url;

/// Inspect fragment tree for a given HTML file
#[derive(Parser, Debug)]
#[command(name = "inspect_frag", version, about)]
struct Args {
  /// HTML file or file:// URL to inspect
  file: String,

  /// Viewport size as WxH (e.g., 1200x800)
  #[arg(long, value_parser = parse_viewport, default_value = "1200x800")]
  viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset
  #[arg(long, default_value = "1.0")]
  dpr: f32,

  /// Horizontal scroll offset in CSS px
  #[arg(long, default_value = "0.0")]
  scroll_x: f32,

  /// Vertical scroll offset in CSS px
  #[arg(long, default_value = "0.0")]
  scroll_y: f32,

  /// Emit fragmentation decision traces (also available via FASTR_TRACE_FRAGMENTATION=1)
  #[arg(long)]
  trace_fragmentation: bool,

  /// Directory to write JSON dumps for each pipeline stage
  #[arg(long, value_name = "DIR")]
  dump_json: Option<String>,

  /// Render the document with debug overlays to a PNG file
  #[arg(long, value_name = "PNG")]
  render_overlay: Option<String>,

  /// CSS selector to filter dumps/overlays to the first matching element
  #[arg(long, conflicts_with = "filter_id")]
  filter_selector: Option<String>,

  /// Element id to filter dumps/overlays to the first matching element
  #[arg(long, conflicts_with = "filter_selector")]
  filter_id: Option<String>,

  /// Whether to draw fragment bounds when rendering overlays
  #[arg(long, default_value_t = true)]
  overlay_fragments: bool,

  /// Whether to annotate box ids when rendering overlays
  #[arg(long, default_value_t = true)]
  overlay_box_ids: bool,

  /// Whether to outline stacking contexts when rendering overlays
  #[arg(long, default_value_t = true)]
  overlay_stacking_contexts: bool,

  /// Whether to highlight scroll containers when rendering overlays
  #[arg(long, default_value_t = true)]
  overlay_scroll_containers: bool,

  #[command(flatten)]
  media_prefs: MediaPreferenceArgs,

  /// Override the User-Agent header
  #[arg(long, default_value = DEFAULT_USER_AGENT)]
  user_agent: String,

  /// Override the Accept-Language header
  #[arg(long, default_value = DEFAULT_ACCEPT_LANGUAGE)]
  accept_language: String,

  /// Abort after this many seconds
  #[arg(long)]
  timeout: Option<u64>,

  /// Dump inspection JSON for a CSS selector (bypasses other output).
  #[arg(long)]
  query: Option<String>,

  /// Dump inspection JSON for an element with this id.
  #[arg(long)]
  query_id: Option<String>,

  /// Dump inspection JSON for a styled node id.
  #[arg(long)]
  query_node: Option<usize>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  // Avoid panicking on SIGPIPE/BrokenPipe when piped through tools like `head`.
  let default_hook = std::panic::take_hook();
  std::panic::set_hook(Box::new(move |info| {
    let mut msg = info.to_string();
    if let Some(s) = info.payload().downcast_ref::<&str>() {
      msg = (*s).to_string();
    } else if let Some(s) = info.payload().downcast_ref::<String>() {
      msg = s.clone();
    }
    if msg.contains("Broken pipe") {
      // Exit silently with success for broken pipe to mirror common CLI behavior.
      std::process::exit(0);
    }
    default_hook(info);
  }));

  let args = Args::parse();
  let media_prefs = MediaPreferences::from(&args.media_prefs);
  if args.trace_fragmentation {
    env::set_var("FASTR_TRACE_FRAGMENTATION", "1");
  }

  let (viewport_w, viewport_h) = args.viewport;

  if let Some(sec) = args.timeout {
    std::thread::spawn(move || {
      std::thread::sleep(Duration::from_secs(sec));
      eprintln!("inspect_frag: timed out after {}s", sec);
      std::process::exit(1);
    });
  }

  let (path, input_url) = if let Ok(url) = Url::parse(&args.file) {
    if url.scheme() == "file" {
      let path_buf = url.to_file_path().map_err(|_| {
        std::io::Error::new(std::io::ErrorKind::InvalidInput, "invalid file:// path")
      })?;
      let canonical = path_buf.to_string_lossy().into_owned();
      (canonical, url.to_string())
    } else {
      // Treat non-file URLs as paths; callers should pass a plain file path for cached HTML.
      let fallback = args.file.clone();
      let input = Url::from_file_path(&fallback)
        .map(|u| u.to_string())
        .unwrap_or_else(|_| format!("file://{fallback}"));
      (fallback, input)
    }
  } else {
    let input = Url::from_file_path(&args.file)
      .map(|u| u.to_string())
      .unwrap_or_else(|_| format!("file://{}", args.file));
    (args.file.clone(), input)
  };

  let mut html = fs::read_to_string(&path)?;
  let resource_base = infer_base_url(&html, &input_url).into_owned();

  media_prefs.apply_env();

  if args.scroll_x != 0.0 || args.scroll_y != 0.0 {
    eprintln!(
      "Applying scroll offset: x={:.1}px y={:.1}px",
      args.scroll_x, args.scroll_y
    );
  }

  let mut renderer = FastRender::builder().device_pixel_ratio(args.dpr).build()?;
  renderer.set_base_url(resource_base.clone());

  if let Ok(val) = env::var("FASTR_TRACE_BOXES") {
    eprintln!("FASTR_TRACE_BOXES env in inspect_frag: {val}");
  }
  let inspect_mask = env::var("FASTR_INSPECT_MASK")
    .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
    .unwrap_or(false);

  // Optionally fetch and inline external CSS links to mirror the render_pages pipeline.
  let fetch_css = env::var("FASTR_FETCH_LINK_CSS")
    .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
    .unwrap_or(true);
  if fetch_css {
    let mut css_links = extract_css_links(&html, &resource_base, MediaType::Screen);
    let mut seen_links: HashSet<String> = css_links.iter().cloned().collect();
    for extra in extract_embedded_css_urls(&html, &resource_base) {
      if seen_links.insert(extra.clone()) {
        css_links.push(extra);
      }
    }
    if !css_links.is_empty() {
      let fetcher = HttpFetcher::new()
        .with_user_agent(args.user_agent.clone())
        .with_accept_language(args.accept_language.clone());
      let mut combined_css = String::new();
      let mut seen_imports = HashSet::new();
      for link in css_links {
        seen_imports.insert(link.clone());
        match fetcher.fetch(&link) {
          Ok(res) => {
            let css_text = decode_css_bytes(&res.bytes, res.content_type.as_deref());
            let rewritten = absolutize_css_urls(&css_text, &link);
            let fetcher_for_imports = fetcher.clone();
            let mut import_fetch = move |u: &str| {
              fetcher_for_imports
                .fetch(u)
                .map(|res| decode_css_bytes(&res.bytes, res.content_type.as_deref()))
            };
            let inlined = inline_imports(
              &rewritten,
              &link,
              &mut import_fetch,
              &mut seen_imports,
              None,
            )
            .map_err(|e| -> Box<dyn std::error::Error> { Box::new(e) })?;
            combined_css.push_str(&inlined);
            combined_css.push('\n');
          }
          Err(err) => eprintln!("warn: failed to fetch css {link}: {err}"),
        }
      }
      if !combined_css.is_empty() {
        html = inject_css_into_html(&html, &combined_css);
      }
    }
  }

  let dom = dom::parse_html(&html)?;

  let query = args
    .query_node
    .map(InspectQuery::NodeId)
    .or_else(|| args.query_id.clone().map(InspectQuery::Id))
    .or_else(|| args.query.clone().map(InspectQuery::Selector));
  if let Some(query) = query {
    let inspections = renderer.inspect(&dom, viewport_w, viewport_h, query)?;
    println!("{}", serde_json::to_string_pretty(&inspections)?);
    return Ok(());
  }

  let media_ctx = media_prefs.media_context_with_overrides((viewport_w, viewport_h), args.dpr);
  let stylesheet = extract_css(&dom)?;
  let styled = apply_styles_with_media_and_target(&dom, &stylesheet, &media_ctx, None);

  let mut styled_text = 0;
  let mut styled_text_visible = 0;
  let mut header_styles: Vec<(ComputedStyle, String)> = Vec::new();
  let mut body_styles: Vec<(ComputedStyle, String)> = Vec::new();
  let mut first_pagetop: Option<ComputedStyle> = None;
  walk_styled(&styled, &mut |style, node| {
    if matches!(node.node_type, DomNodeType::Text { .. }) {
      styled_text += 1;
      if !style.display.is_none() && matches!(style.visibility, Visibility::Visible) {
        styled_text_visible += 1;
      }
    }
    if first_pagetop.is_none() {
      if let DomNodeType::Element { ref tag_name, .. } = node.node_type {
        if tag_name == "span" {
          if let Some(class_attr) = node.get_attribute("class") {
            if class_attr.split_whitespace().any(|c| c == "pagetop") {
              first_pagetop = Some(style.clone());
            }
          }
        }
      }
    }
    if let DomNodeType::Element { ref tag_name, .. } = node.node_type {
      if inspect_mask {
        if let Some(id) = node.get_attribute("id") {
          if id == "target" || style.mask_layers.iter().any(|layer| layer.image.is_some()) {
            let layer_summary = style
              .mask_layers
              .iter()
              .enumerate()
              .map(|(idx, layer)| {
                let image_summary = match layer.image.as_ref() {
                  None => "none".to_string(),
                  Some(fastrender::style::types::BackgroundImage::LinearGradient {
                    angle,
                    stops,
                  }) => format!("linear-gradient(angle={angle} stops={})", stops.len()),
                  Some(fastrender::style::types::BackgroundImage::RepeatingLinearGradient {
                    angle,
                    stops,
                  }) => format!(
                    "repeating-linear-gradient(angle={angle} stops={})",
                    stops.len()
                  ),
                  Some(fastrender::style::types::BackgroundImage::RadialGradient { stops, .. }) => {
                    format!("radial-gradient(stops={})", stops.len())
                  }
                  Some(fastrender::style::types::BackgroundImage::RepeatingRadialGradient {
                    stops,
                    ..
                  }) => format!("repeating-radial-gradient(stops={})", stops.len()),
                  Some(fastrender::style::types::BackgroundImage::ConicGradient { stops, .. }) => {
                    format!("conic-gradient(stops={})", stops.len())
                  }
                  Some(fastrender::style::types::BackgroundImage::RepeatingConicGradient {
                    stops,
                    ..
                  }) => format!("repeating-conic-gradient(stops={})", stops.len()),
                  Some(fastrender::style::types::BackgroundImage::Url(url)) => {
                    format!("url({url})")
                  }
                  Some(fastrender::style::types::BackgroundImage::None) => "none".to_string(),
                };

                format!(
                  "#{idx} img={image_summary} size={:?} repeat={:?} pos={:?} composite={:?} mode={:?} origin={:?} clip={:?}",
                  layer.size,
                  layer.repeat,
                  layer.position,
                  layer.composite,
                  layer.mode,
                  layer.origin,
                  layer.clip
                )
              })
              .collect::<Vec<_>>()
              .join(", ");
            eprintln!(
              "mask debug: <{tag_name} id=\"{id}\"> mask_layers={} [{}]",
              style.mask_layers.len(),
              layer_summary
            );
          }
        }
      }
      if tag_name == "nav" {
        if let Some(id) = node.get_attribute("id") {
          if id == "pageHeader" {
            let mut summary = String::new();
            summary.push_str(&format!(
              "pageHeader bg=rgba({},{},{},{:.2}) color=rgba({},{},{},{:.2})",
              style.background_color.r,
              style.background_color.g,
              style.background_color.b,
              style.background_color.a,
              style.color.r,
              style.color.g,
              style.color.b,
              style.color.a
            ));
            if let Some(bg_var) = style.custom_properties.get("--theme-header__background") {
              summary.push_str(&format!(" var(--theme-header__background)={bg_var}"));
            }
            if let Some(copy_var) = style.custom_properties.get("--theme-header__copy-accent") {
              summary.push_str(&format!(" var(--theme-header__copy-accent)={copy_var}"));
            }
            header_styles.push((style.clone(), summary));
          }
        }
      } else if tag_name == "body" {
        let mut summary = String::new();
        summary.push_str(&format!(
          "body bg=rgba({},{},{},{:.2}) color=rgba({},{},{},{:.2})",
          style.background_color.r,
          style.background_color.g,
          style.background_color.b,
          style.background_color.a,
          style.color.r,
          style.color.g,
          style.color.b,
          style.color.a
        ));
        if let Some(bg_var) = style.custom_properties.get("--semantic-color-bg-primary") {
          summary.push_str(&format!(" var(--semantic-color-bg-primary)={bg_var}"));
        }
        body_styles.push((style.clone(), summary));
      }
    }
  });
  println!(
    "styled text nodes: {} (visible: {})",
    styled_text, styled_text_visible
  );

  if let Ok(needle) = env::var("FASTR_FIND_TEXT") {
    fn walk(node: &StyledNode, needle: &str, stack: &mut Vec<String>, found: &mut bool) {
      stack.push(format!(
        "{}({:?})",
        node.node.tag_name().unwrap_or("text"),
        node.styles.display
      ));
      if let dom::DomNodeType::Text { content } = &node.node.node_type {
        if content.contains(needle) {
          *found = true;
          eprintln!(
            "styled text node containing {:?}: display={:?} visibility={:?} opacity={} stack={}",
            needle,
            node.styles.display,
            node.styles.visibility,
            node.styles.opacity,
            stack.join(" -> ")
          );
        }
      }
      for child in &node.children {
        walk(child, needle, stack, found);
      }
      stack.pop();
    }

    let mut stack = Vec::new();
    let mut found = false;
    walk(&styled, &needle, &mut stack, &mut found);
    if !found {
      eprintln!("styled text node containing {:?} not found", needle);
    }
  }
  if let Some(style) = first_pagetop.as_ref() {
    println!(
            "first span.pagetop display={:?} margin=({:?},{:?},{:?},{:?}) padding=({},{},{},{}) line_height={:?} font_size={}",
            style.display,
            style.margin_top,
            style.margin_right,
            style.margin_bottom,
            style.margin_left,
            style.padding_top,
            style.padding_right,
            style.padding_bottom,
            style.padding_left,
            style.line_height,
            style.font_size
        );
  } else {
    println!("no span.pagetop found");
  }
  if !header_styles.is_empty() {
    for (idx, (_style, summary)) in header_styles.iter().enumerate().take(3) {
      println!("header[{idx}]: {summary}");
    }
  }
  if !body_styles.is_empty() {
    for (idx, (_style, summary)) in body_styles.iter().enumerate().take(1) {
      println!("body[{idx}]: {summary}");
    }
  }

  let find_box_text = env::var("FASTR_FIND_BOX_TEXT").ok();
  let box_tree = generate_box_tree_with_anonymous_fixup(&styled);
  let mut text_boxes = 0;
  let mut path: Vec<String> = Vec::new();
  walk_boxes(&box_tree.root, &mut path, &mut |node, ancestors| {
    if matches!(node.box_type, fastrender::tree::box_tree::BoxType::Text(_)) {
      text_boxes += 1;
    }
    if let Some(needle) = find_box_text.as_deref() {
      if let fastrender::tree::box_tree::BoxType::Text(text) = &node.box_type {
        if text.text.contains(needle) {
          println!(
                        "found box text {:?} display={:?} visibility={:?} position={:?} color={:?} id={} ancestors={}",
                        text.text,
                        node.style.display,
                        node.style.visibility,
                        node.style.position,
                        node.style.color,
                        node.id,
                        ancestors.join(" -> ")
                    );
        }
      }
    }
    if let fastrender::tree::box_tree::BoxType::Text(text) = &node.box_type {
      if text.text.contains("New photos") {
        println!(
          "found headline text box display={:?} font_size={} color={:?}",
          node.style.display, node.style.font_size, node.style.color
        );
        println!("ancestor chain (box types):");
        for (depth, ancestor) in ancestors.iter().enumerate() {
          println!("  {depth}: {ancestor}");
        }
      }
    }
  });
  println!("text boxes: {}", text_boxes);

  let engine = LayoutEngine::with_font_context(
    LayoutConfig::for_viewport(Size::new(viewport_w as f32, viewport_h as f32)),
    renderer.font_context().clone(),
  );
  let fragment_tree = engine.layout_tree(&box_tree)?;
  let scroll_offset = Point::new(-args.scroll_x, -args.scroll_y);
  let filter_path = resolve_filter_path(
    &dom,
    args.filter_id.as_deref(),
    args.filter_selector.as_deref(),
  );
  let dump_targets = build_dump_targets(
    &dom,
    &styled,
    &box_tree,
    &fragment_tree,
    filter_path.as_deref(),
  );
  if let Some(dir) = args.dump_json.as_deref() {
    dump_json_outputs(
      Path::new(dir),
      &dump_targets,
      scroll_offset,
      &resource_base,
      args.dpr,
      renderer.font_context(),
    )?;
  }
  let mut box_debug: HashMap<usize, String> = HashMap::new();
  collect_box_debug(&box_tree.root, &mut box_debug);
  let mut box_styles: HashMap<usize, std::sync::Arc<ComputedStyle>> = HashMap::new();
  collect_box_styles(&box_tree.root, &mut box_styles);
  let mut dark_boxes = Vec::new();
  collect_dark_boxes(&box_tree.root, &mut dark_boxes);
  let mut li_nodes = Vec::new();
  collect_tag_debug(&box_tree.root, "li", &mut li_nodes);
  println!("first li nodes (id, debug):");
  for (idx, (id, dbg)) in li_nodes.iter().take(5).enumerate() {
    println!("  li#{idx}: id={id} {dbg}");
  }
  if let Some(body) = find_first_tag(&box_tree.root, "body") {
    println!(
      "body id={} children={} display={:?}",
      body.id,
      body.children.len(),
      body.style.display
    );
    for (idx, child) in body.children.iter().take(10).enumerate() {
      println!(
        "  body child#{idx}: id={} type={:?} display={:?} is_block={} dbg={}",
        child.id,
        child.box_type,
        child.style.display,
        child.is_block_level(),
        child
          .debug_info
          .as_ref()
          .map(|d| format!("{d}"))
          .unwrap_or_else(|| format!("{:?}", child.box_type))
      );
    }
  }
  if let Some(zone_outer) = find_box_by_id(&box_tree.root, 6527) {
    let dbg = zone_outer
      .debug_info
      .as_ref()
      .map(|d| d.to_selector())
      .unwrap_or_else(|| "<anon>".to_string());
    println!(
            "zone__outer id={} selector={} display={:?} width={:?} min_w={:?} max_w={:?} margin=({:?},{:?})",
            zone_outer.id,
            dbg,
            zone_outer.style.display,
            zone_outer.style.width,
            zone_outer.style.min_width,
            zone_outer.style.max_width,
            zone_outer.style.margin_left,
            zone_outer.style.margin_right,
        );
  }
  if let Some(main_wrapper) = find_box_by_id(&box_tree.root, 279) {
    println!(
      "box 279 subtree: type={:?} display={:?} children={}",
      main_wrapper.box_type,
      main_wrapper.style.display,
      main_wrapper.children.len()
    );
    for (idx, child) in main_wrapper.children.iter().take(8).enumerate() {
      println!(
        "  child#{idx}: id={} type={:?} display={:?} dbg={}",
        child.id,
        child.box_type,
        child.style.display,
        child
          .debug_info
          .as_ref()
          .map(|d| format!("{d}"))
          .unwrap_or_else(|| format!("{:?}", child.box_type))
      );
    }
  }
  if !dark_boxes.is_empty() {
    for (idx, entry) in dark_boxes.iter().take(10).enumerate() {
      println!(
        "dark box #{idx}: id={} bg=rgba({},{},{},{:.2}) {}",
        entry.id, entry.r, entry.g, entry.b, entry.a, entry.debug
      );
    }
  }

  println!("fragments: {}", fragment_tree.fragment_count());

  let mut fragments_abs: Vec<(Rect, &FragmentNode)> = Vec::new();
  collect_fragments_abs(&fragment_tree.root, scroll_offset, &mut fragments_abs);

  let mut fragments_by_box: HashMap<usize, Vec<(Rect, &FragmentNode)>> = HashMap::new();
  let mut transformed: Vec<(Rect, &FragmentNode, Option<Transform3D>)> = Vec::new();

  let mut text_count = 0;
  let mut block_count = 0;
  let mut line_count = 0;
  let mut replaced_count = 0;
  let mut minx = f32::MAX;
  let mut miny = f32::MAX;
  let mut maxx = f32::MIN;
  let mut maxy = f32::MIN;

  for (abs, frag) in &fragments_abs {
    minx = minx.min(abs.x());
    miny = miny.min(abs.y());
    maxx = maxx.max(abs.max_x());
    maxy = maxy.max(abs.max_y());

    match &frag.content {
      FragmentContent::Text { text, .. } => {
        text_count += 1;
        if text_count <= 5 {
          let preview: String = text.chars().take(50).collect();
          println!("text {:?} @ {:?}", preview, abs);
        }
      }
      FragmentContent::Block { .. } => block_count += 1,
      FragmentContent::Line { .. } => line_count += 1,
      FragmentContent::Replaced { .. } => replaced_count += 1,
      FragmentContent::Inline { .. } => {}
    }

    if let Some(box_id) = fragment_box_id(frag) {
      fragments_by_box
        .entry(box_id)
        .or_default()
        .push((*abs, *frag));
    }
    if let Some(style) = frag.style.as_deref() {
      if !style.transform.is_empty() {
        let matrix = DisplayListBuilder::debug_resolve_transform(
          style,
          Rect::from_xywh(abs.x(), abs.y(), abs.width(), abs.height()),
          Some((viewport_w as f32, viewport_h as f32)),
        );
        transformed.push((*abs, *frag, matrix));
      }
    }
  }

  println!(
    "text_count {} block {} line {} replaced {}",
    text_count, block_count, line_count, replaced_count
  );
  println!("bbox [{minx:.1},{miny:.1}] -> [{maxx:.1},{maxy:.1}]");

  let mut split_boxes: Vec<_> = fragments_by_box
    .iter()
    .filter(|(_, frags)| frags.len() > 1)
    .collect();
  split_boxes.sort_by_key(|(id, _)| **id);
  println!(
    "boxes with multiple fragments (column breaks/lines/pages): {}",
    split_boxes.len()
  );
  for (idx, (box_id, frags)) in split_boxes.iter().enumerate().take(8) {
    let label = box_debug
      .get(box_id)
      .cloned()
      .unwrap_or_else(|| format!("box {box_id}"));
    let ranges: Vec<String> = frags
      .iter()
      .map(|(rect, frag)| {
        let extra = match &frag.content {
          FragmentContent::Inline { fragment_index, .. } => {
            format!(" fragment_index={fragment_index}")
          }
          _ => String::new(),
        };
        format!(
          "({:.1},{:.1},{:.1},{:.1}){}",
          rect.x(),
          rect.y(),
          rect.width(),
          rect.height(),
          extra
        )
      })
      .collect();
    println!(
      "  #{idx}: box_id={} {} -> [{}]",
      box_id,
      label,
      ranges.join(" | ")
    );
  }

  println!("fragments with transforms: {}", transformed.len());
  for (idx, (abs, frag, matrix)) in transformed.iter().enumerate().take(8) {
    let label = label_fragment(frag, *abs, &box_debug);
    if let Some(m) = matrix {
      let mm = &m.m;
      println!(
        "  #{idx}: {} matrix=[[{:.3} {:.3} {:.3} {:.3}] [{:.3} {:.3} {:.3} {:.3}] [{:.3} {:.3} {:.3} {:.3}] [{:.3} {:.3} {:.3} {:.3}]]",
        label,
        mm[0], mm[4], mm[8], mm[12],
        mm[1], mm[5], mm[9], mm[13],
        mm[2], mm[6], mm[10], mm[14],
        mm[3], mm[7], mm[11], mm[15]
      );
    } else {
      println!("  #{idx}: {} matrix=<unresolved>", label);
    }
  }

  let mut column_boxes = Vec::new();
  let mut spanning_cells = Vec::new();
  collect_column_info(&box_tree.root, &mut column_boxes, &mut spanning_cells);
  if !column_boxes.is_empty() {
    println!("table columns/colgroups: {}", column_boxes.len());
    for (idx, (id, display, span, dbg)) in column_boxes.iter().enumerate().take(8) {
      println!(
        "  #{idx}: id={} display={:?} span={} {}",
        id, display, span, dbg
      );
    }
  }
  if !spanning_cells.is_empty() {
    println!("table cells with spans: {}", spanning_cells.len());
    for (idx, (id, colspan, rowspan, dbg)) in spanning_cells.iter().enumerate().take(8) {
      println!(
        "  cell#{idx}: id={} colspan={} rowspan={} {}",
        id, colspan, rowspan, dbg
      );
    }
  }
  // Optional viewport-overlap stats to understand what appears in the initial viewport.
  let log_viewport = env::var("FASTR_LOG_VIEWPORT_OVERLAP")
    .map(|v| v != "0" && !v.eq_ignore_ascii_case("false"))
    .unwrap_or(false);
  if log_viewport {
    let viewport_rect = Rect::from_xywh(0.0, 0.0, viewport_w as f32, viewport_h as f32);
    let mut in_view = 0usize;
    let mut text_in_view = 0usize;
    let mut samples: Vec<(f32, String)> = Vec::new();
    for (abs, frag) in &fragments_abs {
      let intersects = abs.x() < viewport_rect.x() + viewport_rect.width()
        && abs.x() + abs.width() > viewport_rect.x()
        && abs.y() < viewport_rect.y() + viewport_rect.height()
        && abs.y() + abs.height() > viewport_rect.y();
      if !intersects {
        continue;
      }
      in_view += 1;
      if let FragmentContent::Text { text, .. } = &frag.content {
        text_in_view += 1;
        let mut snippet = String::new();
        for (idx, ch) in text.trim().replace('\n', " ").chars().enumerate() {
          if idx >= 80 {
            snippet.push('…');
            break;
          }
          snippet.push(ch);
        }
        samples.push((
          abs.y(),
          format!("y={:.1} h={:.1} \"{snippet}\"", abs.y(), abs.height()),
        ));
      }
    }
    samples.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
    let sample_strings: Vec<_> = samples.iter().take(10).map(|(_, s)| s.clone()).collect();
    println!(
      "viewport fragments: {} total, {} text; samples: [{}]",
      in_view,
      text_in_view,
      sample_strings.join(" | ")
    );
  }
  let mut dark_backgrounds: Vec<(f32, f32, Rect, String)> = Vec::new();
  for (abs, frag) in &fragments_abs {
    if let Some(style) = frag.style.as_deref() {
      let bg = style.background_color;
      if bg.a > 0.0 && bg.r == 12 && bg.g == 12 && bg.b == 12 {
        dark_backgrounds.push((
          abs.y(),
          abs.x(),
          *abs,
          match &frag.content {
            FragmentContent::Block { box_id } | FragmentContent::Inline { box_id, .. } => box_id
              .and_then(|id| box_debug.get(&id))
              .cloned()
              .unwrap_or_else(|| format!("{:?}", frag.content)),
            _ => format!("{:?}", frag.content),
          },
        ));
      }
    }
  }
  dark_backgrounds.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
  for (idx, (_, _, rect, content)) in dark_backgrounds.iter().take(5).enumerate() {
    println!("dark bg #{idx}: {:?} @ {:?}", content, rect);
  }
  let mut all_text: Vec<(f32, f32, String)> = Vec::new();
  collect_text_abs(&fragment_tree.root, scroll_offset, &mut all_text);
  all_text.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
  println!("leftmost text fragments:");
  for (idx, (x, y, text)) in all_text.iter().take(10).enumerate() {
    let preview: String = text.chars().take(40).collect();
    println!("  {idx}: ({x:.1}, {y:.1}) {preview:?}");
  }
  println!("rightmost text fragments:");
  for (idx, (x, y, text)) in all_text.iter().rev().take(10).enumerate() {
    let preview: String = text.chars().take(40).collect();
    println!("  {idx}: ({x:.1}, {y:.1}) {preview:?}");
  }
  let mut ranks: Vec<(i32, f32, f32)> = Vec::new();
  for (x, y, text) in &all_text {
    if let Some(stripped) = text.strip_suffix('.') {
      if let Ok(n) = stripped.trim().parse::<i32>() {
        ranks.push((n, *x, *y));
      }
    }
  }
  ranks.sort_by_key(|(n, _, _)| *n);
  println!("rank fragments (first 15):");
  for (idx, (n, x, y)) in ranks.iter().take(15).enumerate() {
    println!("  {idx}: rank={} at ({:.1},{:.1})", n, x, y);
  }
  // Surface far-right fragments to catch runaway translations.
  let mut far_right: Vec<_> = fragments_abs
    .iter()
    .filter(|(abs, _)| abs.max_x() > 2000.0)
    .map(|(abs, f)| {
      let (id, debug) = match &f.content {
        FragmentContent::Block { box_id } | FragmentContent::Inline { box_id, .. } => {
          if let Some(id) = box_id {
            let dbg = box_debug
              .get(id)
              .cloned()
              .unwrap_or_else(|| format!("{:?}", f.content));
            (Some(*id), dbg)
          } else {
            (None, format!("{:?}", f.content))
          }
        }
        _ => (None, format!("{:?}", f.content)),
      };
      (
        abs.max_x(),
        abs.x(),
        abs.y(),
        abs.width(),
        abs.height(),
        id,
        debug,
      )
    })
    .collect();
  far_right.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap_or(std::cmp::Ordering::Equal));
  println!("far-right fragments (max_x > 2000): {}", far_right.len());
  for (idx, (max_x, x, y, w, h, id, content)) in far_right.iter().take(10).enumerate() {
    let id_part = id.map(|i| format!(" box_id={i}")).unwrap_or_default();
    println!("  #{idx}: max_x={max_x:.1} at ({x:.1},{y:.1},{w:.1},{h:.1}){id_part} {content}");
  }
  let mut by_y = all_text.clone();
  by_y.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
  println!("top-most text fragments:");
  for (idx, (x, y, text)) in by_y.iter().take(10).enumerate() {
    let preview: String = text.chars().take(40).collect();
    println!("  {idx}: ({x:.1}, {y:.1}) {preview:?}");
  }
  println!("first 5 text fragments with color/font-size:");
  for (abs, frag) in fragments_abs
    .iter()
    .filter(|(_, f)| matches!(f.content, FragmentContent::Text { .. }))
    .take(5)
  {
    if let FragmentContent::Text { text, .. } = &frag.content {
      let style = frag.style.as_deref();
      let color = style.map(|s| s.color);
      let fs = style.map(|s| s.font_size).unwrap_or(0.0);
      let vis = style
        .map(|s| format!("{:?}", s.visibility))
        .unwrap_or_else(|| "None".into());
      let (r, g, b, a) = color
        .map(|c| (c.r, c.g, c.b, c.a))
        .unwrap_or((0, 0, 0, 0.0));
      println!(
        "  text {:?} @ ({:.1},{:.1}) size={:.1} color=rgba({},{},{},{:.2}) visibility={}",
        text.chars().take(40).collect::<String>(),
        abs.x(),
        abs.y(),
        fs,
        r,
        g,
        b,
        a,
        vis
      );
    }
  }
  println!("path to first 'US' text fragment (absolute bounds):");
  let mut path: Vec<String> = Vec::new();
  find_us_fragment(&fragment_tree.root, scroll_offset, &mut path);

  let mut stacks = Vec::new();
  collect_stacking_contexts(&fragment_tree.root, scroll_offset, None, true, &mut stacks);
  stacks.sort_by(|a, b| {
    a.rect
      .y()
      .partial_cmp(&b.rect.y())
      .unwrap_or(std::cmp::Ordering::Equal)
  });
  println!("stacking contexts (top to bottom): {}", stacks.len());
  for (idx, ctx) in stacks.iter().enumerate() {
    let bg = ctx.style.background_color;
    let bg = format!("rgba({},{},{},{:.2})", bg.r, bg.g, bg.b, bg.a);
    println!(
            "  #{idx}: ({:.1}, {:.1}, {:.1}, {:.1}) display={:?} pos={:?} z={:?} opacity={:.2} transform_ops={} bg={}",
            ctx.rect.x(),
            ctx.rect.y(),
            ctx.rect.width(),
            ctx.rect.height(),
            ctx.display,
            ctx.position,
            ctx.z_index,
            ctx.opacity,
            ctx.transform_ops,
            bg
        );
    if idx < 5 {
      // Show a couple of custom properties for early contexts when present.
      for key in ["--theme-header__background", "--semantic-color-bg-primary"] {
        if let Some(val) = ctx.style.custom_properties.get(key) {
          println!("      var {key} = {}", val);
        }
      }
    }
  }
  // Also list contexts intersecting the viewport to locate visible layers.
  println!("viewport-intersecting stacking contexts:");
  let viewport = Rect::from_xywh(0.0, 0.0, viewport_w as f32, viewport_h as f32);
  for ctx in stacks.iter().filter(|c| c.rect.intersects(viewport)) {
    let bg = ctx.style.background_color;
    println!(
      "  vis: ({:.1},{:.1},{:.1},{:.1}) z={:?} display={:?} bg=rgba({},{},{},{:.2})",
      ctx.rect.x(),
      ctx.rect.y(),
      ctx.rect.width(),
      ctx.rect.height(),
      ctx.z_index,
      ctx.display,
      bg.r,
      bg.g,
      bg.b,
      bg.a
    );
  }

  let mut backgrounds = Vec::new();
  for (abs, frag) in &fragments_abs {
    if let Some(style) = frag.style.as_deref() {
      let bg = style.background_color;
      if bg.a > 0.0 {
        backgrounds.push((
          abs.y(),
          abs.x(),
          bg,
          format!("{:?}", frag.content),
          abs.width(),
          abs.height(),
        ));
      }
    }
  }
  backgrounds.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
  println!("first backgrounds with alpha > 0:");
  for (idx, (y, x, bg, content, w, h)) in backgrounds.iter().take(10).enumerate() {
    println!(
      "  #{idx}: ({x:.1},{y:.1},{w:.1},{h:.1}) bg=rgba({},{},{},{:.2}) content={}",
      bg.r, bg.g, bg.b, bg.a, content
    );
  }
  // Backgrounds that intersect the viewport
  let viewport = Rect::from_xywh(0.0, 0.0, viewport_w as f32, viewport_h as f32);
  let mut view_bgs: Vec<_> = fragments_abs
    .iter()
    .filter_map(|(abs, frag)| {
      frag
        .style
        .as_deref()
        .map(|style| (style.background_color, *abs, frag))
    })
    .filter(|(bg, rect, _)| bg.a > 0.0 && rect.intersects(viewport))
    .collect();
  view_bgs.sort_by(|a, b| {
    a.1
      .y()
      .partial_cmp(&b.1.y())
      .unwrap_or(std::cmp::Ordering::Equal)
  });
  println!(
    "viewport-intersecting backgrounds (alpha>0): {}",
    view_bgs.len()
  );
  for (idx, (bg, rect, frag)) in view_bgs
    .iter()
    .filter(|(_, r, _)| r.width() * r.height() > 1000.0)
    .take(20)
    .enumerate()
  {
    println!(
      "  #{idx}: ({:.1},{:.1},{:.1},{:.1}) bg=rgba({},{},{},{:.2}) content={:?}",
      rect.x(),
      rect.y(),
      rect.width(),
      rect.height(),
      bg.r,
      bg.g,
      bg.b,
      bg.a,
      frag.content
    );
  }
  let mut view_bgs_by_area: Vec<_> = view_bgs
    .iter()
    .map(|(bg, rect, frag)| (rect.width() * rect.height(), *bg, rect, frag))
    .collect();
  view_bgs_by_area.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap_or(std::cmp::Ordering::Equal));
  println!("largest viewport-intersecting backgrounds (alpha>0):");
  for (idx, (area, bg, rect, frag)) in view_bgs_by_area.iter().take(10).enumerate() {
    println!(
      "  #{idx}: area={:.1} rect=({:.1},{:.1},{:.1},{:.1}) bg=rgba({},{},{},{:.2}) content={:?}",
      *area,
      rect.x(),
      rect.y(),
      rect.width(),
      rect.height(),
      bg.r,
      bg.g,
      bg.b,
      bg.a,
      frag.content
    );
  }

  // Absolute-coordinate background analysis.
  let mut abs_backgrounds: Vec<(Rect, &FragmentNode)> = Vec::new();
  collect_backgrounds_abs(&fragment_tree.root, scroll_offset, &mut abs_backgrounds);
  let mut abs_view_bgs: Vec<_> = abs_backgrounds
    .iter()
    .filter_map(|(rect, frag)| {
      frag
        .style
        .as_deref()
        .map(|style| (rect, style.background_color, *frag))
    })
    .filter(|(rect, bg, _)| bg.a > 0.0 && rect.intersects(viewport))
    .collect();
  abs_view_bgs.sort_by(|a, b| {
    a.0
      .y()
      .partial_cmp(&b.0.y())
      .unwrap_or(std::cmp::Ordering::Equal)
  });
  println!(
    "abs viewport-intersecting backgrounds (alpha>0): {}",
    abs_view_bgs.len()
  );
  for (idx, (rect, bg, frag)) in abs_view_bgs
    .iter()
    .filter(|(r, _, _)| r.width() * r.height() > 1000.0)
    .take(20)
    .enumerate()
  {
    println!(
      "  #{idx}: ({:.1},{:.1},{:.1},{:.1}) bg=rgba({},{},{},{:.2}) content={:?}",
      rect.x(),
      rect.y(),
      rect.width(),
      rect.height(),
      bg.r,
      bg.g,
      bg.b,
      bg.a,
      frag.content
    );
  }
  let mut abs_by_area: Vec<_> = abs_backgrounds
    .iter()
    .filter_map(|(rect, frag)| {
      frag.style.as_deref().map(|style| {
        (
          rect.width() * rect.height(),
          *rect,
          style.background_color,
          *frag,
        )
      })
    })
    .collect();
  abs_by_area.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap_or(std::cmp::Ordering::Equal));
  println!("largest absolute backgrounds (alpha>0):");
  for (idx, (area, rect, bg, frag)) in abs_by_area
    .iter()
    .filter(|(_, _, bg, _)| bg.a > 0.0)
    .take(10)
    .enumerate()
  {
    println!(
      "  #{idx}: area={:.1} rect=({:.1},{:.1},{:.1},{:.1}) bg=rgba({},{},{},{:.2}) content={:?}",
      *area,
      rect.x(),
      rect.y(),
      rect.width(),
      rect.height(),
      bg.r,
      bg.g,
      bg.b,
      bg.a,
      frag.content
    );
  }

  // Trace nav/header positioning for HN: find first text containing "Hacker News" or ".pagetop" span.
  let mut nav_path = Vec::new();
  if find_fragment_with_text(
    &fragment_tree.root,
    scroll_offset,
    "Hacker News",
    &mut nav_path,
  ) {
    println!("path to 'Hacker News' fragment:");
    for (idx, entry) in nav_path.iter().enumerate() {
      println!("  {idx}: {entry}");
    }
  } else {
    println!("no 'Hacker News' text fragment found");
  }

  // Surface fragments that have effectively zero width but large height, which can
  // inflate the document height (seen on CNN flex items).
  let mut skinny: Vec<_> = abs_backgrounds
        .iter()
        .filter(|(rect, _)| rect.width() <= 5.0 && rect.height() >= 600.0)
        .map(|(rect, frag)| {
            let mut info = match &frag.content {
                FragmentContent::Block { .. } => "block".to_string(),
                FragmentContent::Inline { .. } => "inline".to_string(),
                FragmentContent::Line { .. } => "line".to_string(),
                FragmentContent::Text { text, .. } => format!("text {:?}", text.chars().take(30).collect::<String>()),
                FragmentContent::Replaced { .. } => "replaced".to_string(),
            };
            if let Some(style) = frag.style.as_deref() {
                info.push_str(&format!(
                    " display={:?} pos={:?} flex=({:?},{:?},{:?},{:?},{:?}) align=({:?},{:?}) size=({:?},{:?}) min=({:?},{:?}) max=({:?},{:?})",
                    style.display,
                    style.position,
                    style.flex_direction,
                    style.flex_wrap,
                    style.flex_basis,
                    style.flex_grow,
                    style.flex_shrink,
                    style.align_items,
                    style.align_self,
                    style.width,
                    style.height,
                    style.min_width,
                    style.min_height,
                    style.max_width,
                    style.max_height
                ));
            }
            if let Some(box_id) = match &frag.content {
                FragmentContent::Block { box_id } => *box_id,
                FragmentContent::Inline { box_id, .. } => *box_id,
                FragmentContent::Replaced { box_id, .. } => *box_id,
                _ => None,
            } {
                if let Some(debug) = box_debug.get(&box_id) {
                    info.push_str(&format!(" box#{box_id} {debug}"));
                } else {
                    info.push_str(&format!(" box#{box_id}"));
                }
            }
            (*rect, info)
        })
        .collect();
  skinny.sort_by(|a, b| {
    a.0
      .y()
      .partial_cmp(&b.0.y())
      .unwrap_or(std::cmp::Ordering::Equal)
  });
  println!(
    "skinny/tall fragments (<=5px wide, >=600px tall): {}",
    skinny.len()
  );
  for (idx, (rect, info)) in skinny.iter().take(20).enumerate() {
    println!(
      "  #{idx}: ({:.1},{:.1},{:.1},{:.1}) {}",
      rect.x(),
      rect.y(),
      rect.width(),
      rect.height(),
      info
    );
  }
  if skinny.len() > 20 {
    println!("  ...");
    for (idx, (rect, info)) in skinny.iter().rev().take(10).enumerate() {
      let global_idx = skinny.len().saturating_sub(idx + 1);
      println!(
        "  #{}: ({:.1},{:.1},{:.1},{:.1}) {}",
        global_idx,
        rect.x(),
        rect.y(),
        rect.width(),
        rect.height(),
        info
      );
    }
  }
  if let Some(path) = find_first_skinny(&fragment_tree.root, scroll_offset, &box_debug) {
    println!("path to first skinny fragment:");
    for (idx, entry) in path.iter().enumerate() {
      println!("  {idx}: {entry}");
    }
  }
  if let Some((rect, frag)) = abs_backgrounds
    .iter()
    .find(|(rect, _)| rect.width() <= 5.0 && rect.height() >= 600.0)
  {
    println!(
            "first skinny fragment children (absolute coords, first 10): parent @ ({:.1},{:.1},{:.1},{:.1})",
            rect.x(),
            rect.y(),
            rect.width(),
            rect.height()
        );
    for (idx, child) in frag.children.iter().take(10).enumerate() {
      let child_abs = Rect::from_xywh(
        rect.x() + child.bounds.x(),
        rect.y() + child.bounds.y(),
        child.bounds.width(),
        child.bounds.height(),
      );
      println!(
        "  child#{idx}: {}",
        label_fragment(child, child_abs, &box_debug)
      );
    }
  }

  // Trace a problematic headline (or a caller-provided needle) to understand how its ancestors
  // are sized/positioned.
  let needle =
    env::var("FASTR_NEEDLE").unwrap_or_else(|_| "New photos released from Epstein".into());
  if !needle.is_empty() {
    if let Some(path) = find_fragment_path(&fragment_tree.root, scroll_offset, &needle) {
      println!("ancestor chain for text containing {:?}:", needle);
      for (depth, (label, rect)) in path.iter().enumerate() {
        println!(
          "  {depth}: {label} @ ({:.1},{:.1},{:.1},{:.1})",
          rect.x(),
          rect.y(),
          rect.width(),
          rect.height()
        );
      }
    } else {
      println!("no fragment found containing {:?}", needle);
    }
  }

  let mut trace_ids = vec![210usize, 200, 180, 6912, 6457, 279, 281, 6909];
  if let Some((first_li, _)) = li_nodes.first() {
    trace_ids.push(*first_li);
  }
  if let Ok(env_ids) = env::var("FASTR_TRACE_BOXES") {
    for id in env_ids
      .split(',')
      .filter_map(|tok| tok.trim().parse::<usize>().ok())
    {
      trace_ids.push(id);
    }
  }
  if let Ok(info_ids) = env::var("FASTR_TRACE_BOX_INFO") {
    for id in info_ids
      .split(',')
      .filter_map(|tok| tok.trim().parse::<usize>().ok())
    {
      if let Some(node) = find_box_by_id(&box_tree.root, id) {
        println!(
                    "box {} info: display={:?} position={:?} visibility={:?} opacity={} size=({:?},{:?}) min=({:?},{:?}) max=({:?},{:?}) overflow=({:?},{:?}) flex=({:.2},{:.2},{:?}) order={} selector={:?}",
                    id,
                    node.style.display,
                    node.style.position,
                    node.style.visibility,
                    node.style.opacity,
                    node.style.width,
                    node.style.height,
                    node.style.min_width,
                    node.style.min_height,
                    node.style.max_width,
                    node.style.max_height,
                    node.style.overflow_x,
                    node.style.overflow_y,
                    node.style.flex_grow,
                    node.style.flex_shrink,
                    node.style.flex_basis,
                    node.style.order,
                    node.debug_info.as_ref().map(|d| d.to_selector())
                );
      }
    }
  }
  if let Some(dump_id) = env::var("FASTR_DUMP_FRAGMENT")
    .ok()
    .and_then(|v| v.parse::<usize>().ok())
  {
    if let Some(fragment) = find_fragment_node(&fragment_tree.root, scroll_offset, dump_id) {
      println!(
        "fragment subtree for box_id {}: text_fragments={}",
        dump_id,
        count_text_fragments(fragment)
      );
      print_fragment_tree(fragment, 0, 80);
    } else {
      println!("no fragment found for box_id {}", dump_id);
    }
  }
  for target_id in trace_ids {
    if let Some(path) =
      find_fragment_by_box_id(&fragment_tree.root, scroll_offset, target_id, &box_debug)
    {
      println!("path to box_id {target_id}:");
      for (idx, entry) in path.iter().enumerate() {
        println!("  {idx}: {entry}");
      }
      if let Some(style) = box_styles.get(&target_id) {
        println!(
                    "  style: display={:?} position={:?} width={:?} height={:?} min=({:?},{:?}) max=({:?},{:?}) flex=({:?},{:?},{:?}) flex_dir={:?} flex_wrap={:?} align=({:?},{:?}) justify={:?} opacity={:.2} visibility={:?}",
                    style.display,
                    style.position,
                    style.width,
                    style.height,
                    style.min_width,
                    style.min_height,
                    style.max_width,
                    style.max_height,
                    style.flex_grow,
                    style.flex_shrink,
                    style.flex_basis,
                    style.flex_direction,
                    style.flex_wrap,
                    style.align_items,
                    style.align_self,
                    style.justify_content,
                    style.opacity,
                    style.visibility,
                );

        if !style.background_layers.is_empty() {
          let summaries: Vec<String> = style
            .background_layers
            .iter()
            .map(|layer| match &layer.image {
              Some(fastrender::style::types::BackgroundImage::Url(url)) => {
                format!("url({})", url)
              }
              Some(fastrender::style::types::BackgroundImage::None) => "none".to_string(),
              Some(_) => "gradient".to_string(),
              None => "(none)".to_string(),
            })
            .collect();
          println!("  backgrounds: {:?}", summaries);
        }
      }
    } else {
      println!("box_id {target_id} not found in fragments");
    }
  }
  if let Some(path) = find_max_x_fragment(&fragment_tree.root, scroll_offset) {
    println!("path to fragment with largest max_x:");
    for (idx, entry) in path.iter().enumerate() {
      println!("  {idx}: {entry}");
    }
  }

  // Inspect the subtree of the first ribbon list item to understand why it balloons vertically.
  if let Some(li_279) = find_box_by_id(&box_tree.root, 279) {
    println!(
      "box 279 debug: type={:?} display={:?} children={}",
      li_279.box_type,
      li_279.style.display,
      li_279.children.len()
    );
    for (idx, child) in li_279.children.iter().take(10).enumerate() {
      let tag = child
        .debug_info
        .as_ref()
        .and_then(|d| d.tag_name.clone())
        .unwrap_or_else(|| format!("{:?}", child.box_type));
      println!(
        "  child#{idx}: id={} type={:?} display={:?} tag={}",
        child.id, child.box_type, child.style.display, tag
      );
    }
    let mut nested_lis = Vec::new();
    collect_tag_ids(li_279, "li", &mut nested_lis);
    println!(
      "box 279 contains {} <li> descendants (including itself)",
      nested_lis.len()
    );
    for id in nested_lis.iter().filter(|id| **id != 279).take(5) {
      println!("  nested li id={}", id);
    }
  }

  if let Some(output) = args.render_overlay.as_deref() {
    let overlay_tree = if filter_path.is_some() {
      &dump_targets.fragment_tree
    } else {
      &fragment_tree
    };
    let options = OverlayOptions {
      fragments: args.overlay_fragments,
      box_ids: args.overlay_box_ids,
      stacking_contexts: args.overlay_stacking_contexts,
      scroll_containers: args.overlay_scroll_containers,
    };
    render_overlay_png(
      &renderer,
      &fragment_tree,
      overlay_tree,
      &box_debug,
      scroll_offset,
      (viewport_w, viewport_h),
      Path::new(output),
      &options,
      renderer.font_context(),
    )?;
  }

  Ok(())
}

struct DumpTrees {
  dom: dom::DomNode,
  styled: StyledNode,
  box_root: BoxNode,
  fragment_tree: FragmentTree,
}

fn resolve_filter_path(
  dom: &dom::DomNode,
  filter_id: Option<&str>,
  filter_selector: Option<&str>,
) -> Option<Vec<usize>> {
  if let Some(id) = filter_id {
    return find_dom_path_by_id(dom, id);
  }

  let selector_list = if let Some(selector) = filter_selector {
    let mut input = ParserInput::new(selector);
    let mut parser = CssParser::new(&mut input);
    match SelectorList::parse(
      &PseudoClassParser,
      &mut parser,
      selectors::parser::ParseRelative::No,
    ) {
      Ok(list) => Some(list),
      Err(err) => {
        eprintln!("warn: failed to parse filter selector {selector:?}: {err:?}");
        None
      }
    }
  } else {
    None
  };

  if let Some(selectors) = selector_list.as_ref() {
    return find_dom_path_by_selector(dom, selectors);
  }

  None
}

fn build_dump_targets(
  dom: &dom::DomNode,
  styled: &StyledNode,
  box_tree: &BoxTree,
  fragment_tree: &FragmentTree,
  filter_path: Option<&[usize]>,
) -> DumpTrees {
  if let Some(path) = filter_path {
    let dom_sub = dom_subtree_by_path(dom, path).unwrap_or_else(|| dom.clone());
    let styled_sub = styled_subtree_by_path(styled, path).unwrap_or_else(|| styled.clone());

    let mut styled_ids = HashSet::new();
    collect_styled_ids(&styled_sub, &mut styled_ids);

    let box_root = filter_box_tree_for_styled(&box_tree.root, &styled_ids)
      .unwrap_or_else(|| box_tree.root.clone());

    let mut box_ids = HashSet::new();
    collect_box_ids_for_filter(&box_root, &mut box_ids);
    let fragment_tree = filter_fragment_tree_for_boxes(fragment_tree, &box_ids)
      .unwrap_or_else(|| fragment_tree.clone());

    DumpTrees {
      dom: dom_sub,
      styled: styled_sub,
      box_root,
      fragment_tree,
    }
  } else {
    DumpTrees {
      dom: dom.clone(),
      styled: styled.clone(),
      box_root: box_tree.root.clone(),
      fragment_tree: fragment_tree.clone(),
    }
  }
}

fn dump_json_outputs(
  dir: &Path,
  targets: &DumpTrees,
  scroll_offset: Point,
  base_url: &str,
  dpr: f32,
  font_ctx: &FontContext,
) -> Result<(), Box<dyn std::error::Error>> {
  fs::create_dir_all(dir)?;

  let exporter = TreeJsonExporter::with_config(JsonExportConfig {
    scroll_offset: Some(scroll_offset),
  });

  write_json_file(&dir.join("dom.json"), &exporter.export_dom(&targets.dom))?;
  write_json_file(
    &dir.join("styled.json"),
    &exporter.export_styled_tree(&targets.styled),
  )?;
  write_json_file(
    &dir.join("box_tree.json"),
    &exporter.export_box_tree(&targets.box_root),
  )?;
  write_json_file(
    &dir.join("fragment_tree.json"),
    &exporter.export_fragment_tree(&targets.fragment_tree.root),
  )?;

  let display_list = DisplayListBuilder::new()
    .with_font_context(font_ctx.clone())
    .with_device_pixel_ratio(dpr)
    .with_base_url(base_url.to_string())
    .build_tree(&targets.fragment_tree);
  write_json_file(
    &dir.join("display_list.json"),
    &exporter.export_display_list(&display_list),
  )?;

  Ok(())
}

fn write_json_file(path: &Path, value: &Value) -> Result<(), Box<dyn std::error::Error>> {
  let json = serde_json::to_string_pretty(value)?;
  fs::write(path, json)?;
  Ok(())
}

fn dom_subtree_by_path(node: &dom::DomNode, path: &[usize]) -> Option<dom::DomNode> {
  if path.is_empty() {
    return Some(node.clone());
  }
  let (idx, rest) = path.split_first()?;
  node
    .children
    .get(*idx)
    .and_then(|child| dom_subtree_by_path(child, rest))
}

fn styled_subtree_by_path(node: &StyledNode, path: &[usize]) -> Option<StyledNode> {
  if path.is_empty() {
    return Some(node.clone());
  }
  let (idx, rest) = path.split_first()?;
  node
    .children
    .get(*idx)
    .and_then(|child| styled_subtree_by_path(child, rest))
}

fn collect_styled_ids(node: &StyledNode, out: &mut HashSet<usize>) {
  out.insert(node.node_id);
  for child in &node.children {
    collect_styled_ids(child, out);
  }
}

fn filter_box_tree_for_styled(node: &BoxNode, allowed: &HashSet<usize>) -> Option<BoxNode> {
  let filtered_children: Vec<_> = node
    .children
    .iter()
    .filter_map(|child| filter_box_tree_for_styled(child, allowed))
    .collect();
  let keep = node
    .styled_node_id
    .map(|id| allowed.contains(&id))
    .unwrap_or(false)
    || !filtered_children.is_empty();
  if keep {
    let mut clone = node.clone();
    clone.children = filtered_children;
    Some(clone)
  } else {
    None
  }
}

fn collect_box_ids_for_filter(node: &BoxNode, out: &mut HashSet<usize>) {
  out.insert(node.id);
  for child in &node.children {
    collect_box_ids_for_filter(child, out);
  }
}

fn filter_fragment_tree_for_boxes(
  tree: &FragmentTree,
  allowed: &HashSet<usize>,
) -> Option<FragmentTree> {
  filter_fragment_node_for_boxes(&tree.root, allowed).map(|root| {
    let mut clone = tree.clone();
    clone.root = root;
    clone.additional_fragments.clear();
    clone
  })
}

fn filter_fragment_node_for_boxes(
  node: &FragmentNode,
  allowed: &HashSet<usize>,
) -> Option<FragmentNode> {
  let filtered_children: Vec<_> = node
    .children
    .iter()
    .filter_map(|child| filter_fragment_node_for_boxes(child, allowed))
    .collect();
  let keep = fragment_box_id(node)
    .map(|id| allowed.contains(&id))
    .unwrap_or(false)
    || !filtered_children.is_empty();

  if keep {
    let mut clone = node.clone();
    clone.children = filtered_children;
    Some(clone)
  } else {
    None
  }
}

fn find_dom_path_by_id(node: &dom::DomNode, target: &str) -> Option<Vec<usize>> {
  let mut path = Vec::new();
  fn walk(node: &dom::DomNode, target: &str, path: &mut Vec<usize>) -> Option<Vec<usize>> {
    if node
      .get_attribute_ref("id")
      .map(|id| id == target)
      .unwrap_or(false)
    {
      return Some(path.clone());
    }
    for (idx, child) in node.children.iter().enumerate() {
      path.push(idx);
      if let Some(found) = walk(child, target, path) {
        return Some(found);
      }
      path.pop();
    }
    None
  }

  walk(node, target, &mut path)
}

fn find_dom_path_by_selector(
  node: &dom::DomNode,
  selectors: &SelectorList<FastRenderSelectorImpl>,
) -> Option<Vec<usize>> {
  let mut caches = SelectorCaches::default();
  let mut context = MatchingContext::new(
    MatchingMode::Normal,
    None,
    &mut caches,
    QuirksMode::NoQuirks,
    NeedsSelectorFlags::Yes,
    MatchingForInvalidation::No,
  );
  let mut ancestors: Vec<&dom::DomNode> = Vec::new();
  let mut path = Vec::new();

  fn walk<'a>(
    node: &'a dom::DomNode,
    selectors: &SelectorList<FastRenderSelectorImpl>,
    context: &mut MatchingContext<FastRenderSelectorImpl>,
    ancestors: &mut Vec<&'a dom::DomNode>,
    path: &mut Vec<usize>,
  ) -> Option<Vec<usize>> {
    if node.is_element() || matches!(node.node_type, DomNodeType::Slot { .. }) {
      let element_ref = dom::ElementRef::with_ancestors(node, ancestors);
      if selectors
        .slice()
        .iter()
        .any(|sel| matches_selector(sel, 0, None, &element_ref, context))
      {
        return Some(path.clone());
      }
    }

    ancestors.push(node);
    for (idx, child) in node.children.iter().enumerate() {
      path.push(idx);
      if let Some(found) = walk(child, selectors, context, ancestors, path) {
        return Some(found);
      }
      path.pop();
    }
    ancestors.pop();
    None
  }

  walk(node, selectors, &mut context, &mut ancestors, &mut path)
}

struct OverlayOptions {
  fragments: bool,
  box_ids: bool,
  stacking_contexts: bool,
  scroll_containers: bool,
}

fn render_overlay_png(
  renderer: &FastRender,
  paint_tree: &FragmentTree,
  overlay_tree: &FragmentTree,
  box_debug: &HashMap<usize, String>,
  scroll_offset: Point,
  viewport: (u32, u32),
  output: &Path,
  options: &OverlayOptions,
  font_ctx: &FontContext,
) -> Result<(), Box<dyn std::error::Error>> {
  let pixmap = renderer.paint_with_offset(paint_tree, viewport.0, viewport.1, scroll_offset)?;
  let mut canvas = Canvas::from_pixmap(pixmap);
  let mut fragments_abs = Vec::new();
  collect_fragments_abs(&overlay_tree.root, scroll_offset, &mut fragments_abs);

  if options.fragments {
    for (rect, _) in &fragments_abs {
      canvas.stroke_rect(*rect, Rgba::rgb(46, 204, 113), 1.0);
    }
  }

  if options.stacking_contexts {
    let mut stacks = Vec::new();
    collect_stacking_contexts(&overlay_tree.root, scroll_offset, None, true, &mut stacks);
    for ctx in &stacks {
      canvas.stroke_rect(ctx.rect, Rgba::rgb(52, 152, 219), 1.2);
    }
  }

  if options.scroll_containers {
    for (rect, frag) in &fragments_abs {
      if fragment_is_scroll_container(frag) {
        canvas.stroke_rect(*rect, Rgba::rgb(231, 76, 60), 1.2);
      }
    }
  }

  if options.box_ids {
    let mut by_box: HashMap<usize, Rect> = HashMap::new();
    for (rect, frag) in &fragments_abs {
      if let Some(id) = fragment_box_id(frag) {
        by_box.entry(id).or_insert(*rect);
      }
    }

    let mut shaper = ShapingPipeline::new();
    let mut label_style = ComputedStyle::default();
    label_style.font_size = 11.0;
    label_style.color = Rgba::rgb(20, 20, 20);

    for (id, rect) in by_box {
      let label = box_debug
        .get(&id)
        .cloned()
        .unwrap_or_else(|| format!("box {id}"));
      let origin = Point::new(rect.x() + 2.0, rect.y() + 12.0);
      draw_overlay_label(
        &mut canvas,
        &label,
        origin,
        &mut shaper,
        font_ctx,
        &label_style,
      );
    }
  }

  let png = encode_image(&canvas.into_pixmap(), OutputFormat::Png)?;
  fs::write(output, png)?;
  Ok(())
}

fn fragment_is_scroll_container(fragment: &FragmentNode) -> bool {
  fragment
    .style
    .as_deref()
    .map(|style| {
      matches!(style.overflow_x, Overflow::Auto | Overflow::Scroll)
        || matches!(style.overflow_y, Overflow::Auto | Overflow::Scroll)
    })
    .unwrap_or(false)
}

fn draw_overlay_label(
  canvas: &mut Canvas,
  text: &str,
  origin: Point,
  shaper: &mut ShapingPipeline,
  font_ctx: &FontContext,
  style: &ComputedStyle,
) {
  if let Ok(runs) = shaper.shape(text, style, font_ctx) {
    let mut cursor = origin;
    for run in runs {
      canvas.draw_text(
        cursor,
        &run.glyphs,
        &run.font,
        run.font_size,
        style.color,
        run.synthetic_bold,
        run.synthetic_oblique,
      );
      cursor.x += run.advance;
    }
  }
}

fn collect_tag_ids(node: &BoxNode, tag: &str, out: &mut Vec<usize>) {
  if node
    .debug_info
    .as_ref()
    .and_then(|d| d.tag_name.as_deref())
    .map(|t| t.eq_ignore_ascii_case(tag))
    .unwrap_or(false)
  {
    out.push(node.id);
  }
  for child in &node.children {
    collect_tag_ids(child, tag, out);
  }
}

fn find_fragment_node(node: &FragmentNode, offset: Point, target: usize) -> Option<&FragmentNode> {
  let abs = Rect::from_xywh(
    node.bounds.x() + offset.x,
    node.bounds.y() + offset.y,
    node.bounds.width(),
    node.bounds.height(),
  );
  let mut matches = false;
  match &node.content {
    FragmentContent::Block { box_id }
    | FragmentContent::Inline { box_id, .. }
    | FragmentContent::Replaced { box_id, .. } => {
      if box_id == &Some(target) {
        matches = true;
      }
    }
    _ => {}
  }
  if matches {
    return Some(node);
  }
  let next_offset = Point::new(abs.x(), abs.y());
  for child in &node.children {
    if let Some(found) = find_fragment_node(child, next_offset, target) {
      return Some(found);
    }
  }
  None
}

fn count_text_fragments(fragment: &FragmentNode) -> usize {
  fn walk(node: &FragmentNode, total: &mut usize) {
    if matches!(node.content, FragmentContent::Text { .. }) {
      *total += 1;
    }
    for child in &node.children {
      walk(child, total);
    }
  }

  let mut total = 0;
  walk(fragment, &mut total);
  total
}

fn print_fragment_tree(node: &FragmentNode, indent: usize, max_lines: usize) {
  fn fmt_content(node: &FragmentNode) -> String {
    match &node.content {
      FragmentContent::Block { box_id } => format!("block box_id={:?}", box_id),
      FragmentContent::Inline { box_id, .. } => format!("inline box_id={:?}", box_id),
      FragmentContent::Line { .. } => "line".into(),
      FragmentContent::Text { text, .. } => format!("text {:?}", text),
      FragmentContent::Replaced { box_id, .. } => format!("replaced box_id={:?}", box_id),
    }
  }

  fn walk(node: &FragmentNode, indent: usize, remaining: &mut usize) {
    if *remaining == 0 {
      return;
    }
    *remaining -= 1;
    println!(
      "{space}{desc}",
      space = " ".repeat(indent * 2),
      desc = fmt_content(node)
    );
    for child in &node.children {
      walk(child, indent + 1, remaining);
      if *remaining == 0 {
        break;
      }
    }
  }

  let mut remaining = max_lines;
  walk(node, indent, &mut remaining);
}

fn find_box_by_id(node: &BoxNode, target: usize) -> Option<&BoxNode> {
  if node.id == target {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_box_by_id(child, target) {
      return Some(found);
    }
  }
  None
}

fn collect_tag_debug(node: &BoxNode, tag: &str, out: &mut Vec<(usize, String)>) {
  if node
    .debug_info
    .as_ref()
    .and_then(|d| d.tag_name.as_deref())
    .map(|t| t.eq_ignore_ascii_case(tag))
    .unwrap_or(false)
  {
    let dbg = node
      .debug_info
      .as_ref()
      .map(|d| format!("{d}"))
      .unwrap_or_else(|| format!("{:?}", node.box_type));
    out.push((node.id, dbg));
  }
  for child in &node.children {
    collect_tag_debug(child, tag, out);
  }
}

fn find_first_tag<'a>(node: &'a BoxNode, tag: &str) -> Option<&'a BoxNode> {
  if node
    .debug_info
    .as_ref()
    .and_then(|d| d.tag_name.as_deref())
    .map(|t| t.eq_ignore_ascii_case(tag))
    .unwrap_or(false)
  {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_first_tag(child, tag) {
      return Some(found);
    }
  }
  None
}

fn walk_boxes<F: FnMut(&BoxNode, &[String])>(node: &BoxNode, path: &mut Vec<String>, f: &mut F) {
  f(node, path);
  if let fastrender::tree::box_tree::BoxType::Text(text) = &node.box_type {
    if text.text.trim() == "US" {
      println!(
        "debug text box 'US' display={:?} position={:?} width={:?} flex=({}, {}, {:?})",
        node.style.display,
        node.style.position,
        node.style.width,
        node.style.flex_grow,
        node.style.flex_shrink,
        node.style.flex_basis
      );
      println!("  ancestors {:?}", path);
    }
  }
  path.push(format!("#{} {:?}", node.id, node.box_type));
  for child in &node.children {
    walk_boxes(child, path, f);
  }
  path.pop();
}

fn find_fragment_path(
  node: &FragmentNode,
  offset: Point,
  needle: &str,
) -> Option<Vec<(String, Rect)>> {
  let abs = Rect::from_xywh(
    node.bounds.x() + offset.x,
    node.bounds.y() + offset.y,
    node.bounds.width(),
    node.bounds.height(),
  );
  let mut label = match &node.content {
    FragmentContent::Text { text, .. } => {
      format!("text {:?}", text.chars().take(40).collect::<String>())
    }
    FragmentContent::Inline { .. } => "inline".to_string(),
    FragmentContent::Line { .. } => "line".to_string(),
    FragmentContent::Block { .. } => "block".to_string(),
    FragmentContent::Replaced { .. } => "replaced".to_string(),
  };
  if let Some(style) = node.style.as_deref() {
    label.push_str(&format!(
      " display={:?} pos={:?}",
      style.display, style.position
    ));
  }

  let mut path = vec![(label, abs)];

  if let FragmentContent::Text { text, .. } = &node.content {
    if text.contains(needle) {
      return Some(path);
    }
  }

  let next_offset = Point::new(offset.x + node.bounds.x(), offset.y + node.bounds.y());
  for child in &node.children {
    if let Some(mut child_path) = find_fragment_path(child, next_offset, needle) {
      path.append(&mut child_path);
      return Some(path);
    }
  }

  None
}

fn find_fragment_by_box_id(
  node: &FragmentNode,
  offset: Point,
  target: usize,
  box_debug: &HashMap<usize, String>,
) -> Option<Vec<String>> {
  let abs = Rect::from_xywh(
    node.bounds.x() + offset.x,
    node.bounds.y() + offset.y,
    node.bounds.width(),
    node.bounds.height(),
  );
  let mut label = match &node.content {
    FragmentContent::Block { box_id }
    | FragmentContent::Inline { box_id, .. }
    | FragmentContent::Replaced { box_id, .. } => {
      if let Some(id) = box_id {
        let dbg = box_debug
          .get(id)
          .cloned()
          .unwrap_or_else(|| format!("{:?}", node.content));
        format!("id={id} {dbg}")
      } else {
        format!("{:?}", node.content)
      }
    }
    _ => format!("{:?}", node.content),
  };
  label.push_str(&format!(
    " @ ({:.1},{:.1},{:.1},{:.1})",
    abs.x(),
    abs.y(),
    abs.width(),
    abs.height()
  ));

  let mut path = vec![label];
  let mut matches = false;
  match &node.content {
    FragmentContent::Block { box_id }
    | FragmentContent::Inline { box_id, .. }
    | FragmentContent::Replaced { box_id, .. } => {
      if box_id == &Some(target) {
        matches = true;
      }
    }
    _ => {}
  }

  let next_offset = Point::new(abs.x(), abs.y());
  for child in &node.children {
    if let Some(mut child_path) = find_fragment_by_box_id(child, next_offset, target, box_debug) {
      path.append(&mut child_path);
      matches = true;
      break;
    }
  }

  if matches {
    Some(path)
  } else {
    None
  }
}

fn find_max_x_fragment(node: &FragmentNode, offset: Point) -> Option<Vec<String>> {
  fn walk(
    node: &FragmentNode,
    offset: Point,
    best: &mut (f32, Vec<String>),
    current: &mut Vec<String>,
  ) {
    let abs = Rect::from_xywh(
      node.bounds.x() + offset.x,
      node.bounds.y() + offset.y,
      node.bounds.width(),
      node.bounds.height(),
    );
    let label = match &node.content {
      FragmentContent::Block { .. } => "block",
      FragmentContent::Inline { .. } => "inline",
      FragmentContent::Line { .. } => "line",
      FragmentContent::Text { .. } => "text",
      FragmentContent::Replaced { .. } => "replaced",
    };
    let mut entry = format!(
      "{} @ ({:.1},{:.1},{:.1},{:.1})",
      label,
      abs.x(),
      abs.y(),
      abs.width(),
      abs.height()
    );
    if let Some(style) = node.style.as_deref() {
      entry.push_str(&format!(
        " display={:?} pos={:?} transform_ops={} z={:?}",
        style.display,
        style.position,
        style.transform.len(),
        style.z_index
      ));
    }
    current.push(entry);
    if abs.max_x() > best.0 {
      best.0 = abs.max_x();
      best.1 = current.clone();
    }
    let next_offset = Point::new(abs.x(), abs.y());
    for child in &node.children {
      walk(child, next_offset, best, current);
    }
    current.pop();
  }

  let mut best = (f32::MIN, Vec::new());
  let mut current = Vec::new();
  walk(node, offset, &mut best, &mut current);
  if best.1.is_empty() {
    None
  } else {
    Some(best.1)
  }
}

fn walk_styled<F: FnMut(&ComputedStyle, &dom::DomNode)>(node: &StyledNode, f: &mut F) {
  f(&node.styles, &node.node);
  if let Some(before) = &node.before_styles {
    f(before, &node.node);
  }
  if let Some(after) = &node.after_styles {
    f(after, &node.node);
  }
  if let Some(marker) = &node.marker_styles {
    f(marker, &node.node);
  }
  for child in &node.children {
    walk_styled(child, f);
  }
}

fn collect_box_debug(node: &BoxNode, out: &mut HashMap<usize, String>) {
  out.insert(node.id, format_debug_info(node));
  for child in &node.children {
    collect_box_debug(child, out);
  }
}

fn format_debug_info(node: &BoxNode) -> String {
  if let Some(info) = &node.debug_info {
    let mut label = info.to_selector();
    let mut spans = Vec::new();
    if info.colspan > 1 {
      spans.push(format!("colspan={}", info.colspan));
    }
    if info.rowspan > 1 {
      spans.push(format!("rowspan={}", info.rowspan));
    }
    if info.column_span > 1 {
      spans.push(format!("column-span={}", info.column_span));
    }
    if !spans.is_empty() {
      label.push_str(&format!(" ({})", spans.join(" ")));
    }
    label
  } else {
    format!("{:?}", node.box_type)
  }
}

fn collect_box_styles(node: &BoxNode, out: &mut HashMap<usize, std::sync::Arc<ComputedStyle>>) {
  out.insert(node.id, node.style.clone());
  for child in &node.children {
    collect_box_styles(child, out);
  }
}

fn collect_column_info(
  node: &BoxNode,
  columns: &mut Vec<(usize, Display, usize, String)>,
  spanning_cells: &mut Vec<(usize, usize, usize, String)>,
) {
  let display = node.style.display;
  if matches!(display, Display::TableColumn | Display::TableColumnGroup) {
    let span = node.debug_info.as_ref().map(|d| d.column_span).unwrap_or(1);
    columns.push((node.id, display, span, format_debug_info(node)));
  }
  if matches!(display, Display::TableCell) {
    if let Some(info) = node.debug_info.as_ref() {
      if info.colspan > 1 || info.rowspan > 1 {
        spanning_cells.push((
          node.id,
          info.colspan.max(1),
          info.rowspan.max(1),
          format_debug_info(node),
        ));
      }
    }
  }

  for child in &node.children {
    collect_column_info(child, columns, spanning_cells);
  }
}

#[derive(Debug)]
struct DarkBox {
  id: usize,
  r: u8,
  g: u8,
  b: u8,
  a: f32,
  debug: String,
}

fn collect_dark_boxes(node: &BoxNode, out: &mut Vec<DarkBox>) {
  let bg = node.style.background_color;
  if bg.a > 0.0 && bg.r == 12 && bg.g == 12 && bg.b == 12 {
    let debug = node
      .debug_info
      .as_ref()
      .map(|d| format!("{d}"))
      .unwrap_or_else(|| format!("{:?}", node.box_type));
    out.push(DarkBox {
      id: node.id,
      r: bg.r,
      g: bg.g,
      b: bg.b,
      a: bg.a,
      debug,
    });
  }
  for child in &node.children {
    collect_dark_boxes(child, out);
  }
}

fn collect_fragments_abs<'a>(
  fragment: &'a FragmentNode,
  offset: Point,
  out: &mut Vec<(Rect, &'a FragmentNode)>,
) {
  let abs = Rect::from_xywh(
    fragment.bounds.x() + offset.x,
    fragment.bounds.y() + offset.y,
    fragment.bounds.width(),
    fragment.bounds.height(),
  );
  out.push((abs, fragment));
  let next_offset = Point::new(abs.x(), abs.y());
  for child in &fragment.children {
    collect_fragments_abs(child, next_offset, out);
  }
}

fn collect_backgrounds_abs<'a>(
  fragment: &'a FragmentNode,
  offset: Point,
  out: &mut Vec<(Rect, &'a FragmentNode)>,
) {
  let abs = Rect::from_xywh(
    fragment.bounds.x() + offset.x,
    fragment.bounds.y() + offset.y,
    fragment.bounds.width(),
    fragment.bounds.height(),
  );
  out.push((abs, fragment));
  let next_offset = Point::new(abs.x(), abs.y());
  for child in &fragment.children {
    collect_backgrounds_abs(child, next_offset, out);
  }
}

fn collect_text_abs(fragment: &FragmentNode, offset: Point, out: &mut Vec<(f32, f32, String)>) {
  let abs = Rect::from_xywh(
    fragment.bounds.x() + offset.x,
    fragment.bounds.y() + offset.y,
    fragment.bounds.width(),
    fragment.bounds.height(),
  );
  if let FragmentContent::Text { text, .. } = &fragment.content {
    out.push((abs.x(), abs.y(), text.clone()));
  }
  let next_offset = Point::new(abs.x(), abs.y());
  for child in &fragment.children {
    collect_text_abs(child, next_offset, out);
  }
}

fn find_us_fragment(node: &FragmentNode, offset: Point, path: &mut Vec<String>) -> bool {
  let abs = Rect::from_xywh(
    node.bounds.x() + offset.x,
    node.bounds.y() + offset.y,
    node.bounds.width(),
    node.bounds.height(),
  );
  let style = node.style.as_deref();
  let label = match &node.content {
    FragmentContent::Text { text, .. } => {
      format!("text \"{}\"", text.chars().take(30).collect::<String>())
    }
    FragmentContent::Inline { .. } => "inline".to_string(),
    FragmentContent::Line { .. } => "line".to_string(),
    FragmentContent::Block { .. } => "block".to_string(),
    FragmentContent::Replaced { .. } => "replaced".to_string(),
  };
  let extra = style.map(|s| {
    format!(
      " display={:?} flex=({:?}, {:?}, {:?}) opacity={} visibility={:?}",
      s.display, s.flex_direction, s.flex_wrap, s.flex_basis, s.opacity, s.visibility
    )
  });
  path.push(format!(
    "{label} @ ({:.1},{:.1},{:.1},{:.1}){}",
    abs.x(),
    abs.y(),
    abs.width(),
    abs.height(),
    extra.unwrap_or_default()
  ));

  let found = match &node.content {
    FragmentContent::Text { text, .. } if text.trim() == "US" => true,
    _ => {
      let next_offset = Point::new(offset.x + node.bounds.x(), offset.y + node.bounds.y());
      node
        .children
        .iter()
        .any(|child| find_us_fragment(child, next_offset, path))
    }
  };

  if found {
    for (idx, entry) in path.iter().enumerate() {
      println!("  {idx}: {entry}");
    }
  }
  path.pop();
  found
}

#[derive(Debug)]
struct StackCtx<'a> {
  rect: Rect,
  display: Display,
  position: Position,
  z_index: Option<i32>,
  opacity: f32,
  transform_ops: usize,
  style: &'a ComputedStyle,
}

fn collect_stacking_contexts<'a>(
  fragment: &'a FragmentNode,
  offset: Point,
  parent_style: Option<&'a ComputedStyle>,
  is_root: bool,
  out: &mut Vec<StackCtx<'a>>,
) {
  let abs = Rect::from_xywh(
    fragment.bounds.x() + offset.x,
    fragment.bounds.y() + offset.y,
    fragment.bounds.width(),
    fragment.bounds.height(),
  );
  let style = fragment.style.as_deref();
  let establishes = style
    .map(|s| creates_stacking_context(s, parent_style, is_root))
    .unwrap_or(is_root);

  let next_offset = Point::new(abs.x(), abs.y());
  let next_parent = style.or(parent_style);

  if establishes {
    if let Some(style) = style {
      out.push(StackCtx {
        rect: abs,
        display: style.display,
        position: style.position,
        z_index: style.z_index,
        opacity: style.opacity,
        transform_ops: style.transform.len(),
        style,
      });
    }
  }

  for child in &fragment.children {
    collect_stacking_contexts(child, next_offset, next_parent, false, out);
  }
}

fn label_fragment(
  fragment: &FragmentNode,
  abs: Rect,
  box_debug: &HashMap<usize, String>,
) -> String {
  let mut label = match &fragment.content {
    FragmentContent::Block { .. } => "block".to_string(),
    FragmentContent::Inline { .. } => "inline".to_string(),
    FragmentContent::Line { .. } => "line".to_string(),
    FragmentContent::Text { text, .. } => {
      format!("text {:?}", text.chars().take(40).collect::<String>())
    }
    FragmentContent::Replaced { .. } => "replaced".to_string(),
  };
  label.push_str(&format!(
    " @ ({:.1},{:.1},{:.1},{:.1})",
    abs.x(),
    abs.y(),
    abs.width(),
    abs.height()
  ));
  if let Some(style) = fragment.style.as_deref() {
    label.push_str(&format!(
            " display={:?} pos={:?} flex=({:?},{:?},{:?},{:?},{:?}) align=({:?},{:?}) size=({:?},{:?}) min=({:?},{:?}) max=({:?},{:?})",
            style.display,
            style.position,
            style.flex_direction,
            style.flex_wrap,
            style.flex_basis,
            style.flex_grow,
            style.flex_shrink,
            style.align_items,
            style.align_self,
            style.width,
            style.height,
            style.min_width,
            style.min_height,
            style.max_width,
            style.max_height
        ));
  }
  if let Some(box_id) = match &fragment.content {
    FragmentContent::Block { box_id } => *box_id,
    FragmentContent::Inline { box_id, .. } => *box_id,
    FragmentContent::Replaced { box_id, .. } => *box_id,
    _ => None,
  } {
    if let Some(debug) = box_debug.get(&box_id) {
      label.push_str(&format!(" box#{box_id} {debug}"));
    } else {
      label.push_str(&format!(" box#{box_id}"));
    }
  }
  label
}

fn fragment_box_id(fragment: &FragmentNode) -> Option<usize> {
  match &fragment.content {
    FragmentContent::Block { box_id }
    | FragmentContent::Inline { box_id, .. }
    | FragmentContent::Text { box_id, .. }
    | FragmentContent::Replaced { box_id, .. } => *box_id,
    FragmentContent::Line { .. } => None,
  }
}

fn find_first_skinny(
  fragment: &FragmentNode,
  offset: Point,
  box_debug: &HashMap<usize, String>,
) -> Option<Vec<String>> {
  let abs = Rect::from_xywh(
    fragment.bounds.x() + offset.x,
    fragment.bounds.y() + offset.y,
    fragment.bounds.width(),
    fragment.bounds.height(),
  );
  let label = label_fragment(fragment, abs, box_debug);
  let is_skinny = abs.width() <= 5.0 && abs.height() >= 600.0;

  if is_skinny {
    return Some(vec![label]);
  }

  let next_offset = Point::new(abs.x(), abs.y());
  for child in &fragment.children {
    if let Some(mut path) = find_first_skinny(child, next_offset, box_debug) {
      path.insert(0, label.clone());
      return Some(path);
    }
  }
  None
}

fn find_fragment_with_text(
  fragment: &FragmentNode,
  offset: Point,
  needle: &str,
  path: &mut Vec<String>,
) -> bool {
  let abs = Rect::from_xywh(
    fragment.bounds.x() + offset.x,
    fragment.bounds.y() + offset.y,
    fragment.bounds.width(),
    fragment.bounds.height(),
  );
  let mut label = format!(
    "{:?} @ ({:.1},{:.1},{:.1},{:.1})",
    fragment.content,
    abs.x(),
    abs.y(),
    abs.width(),
    abs.height()
  );
  if let Some(style) = fragment.style.as_deref() {
    label.push_str(&format!(
      " display={:?} pos={:?}",
      style.display, style.position
    ));
  }

  if let FragmentContent::Text { text, .. } = &fragment.content {
    if text.contains(needle) {
      path.push(label);
      return true;
    }
  }

  let next_offset = Point::new(abs.x(), abs.y());
  for child in &fragment.children {
    if find_fragment_with_text(child, next_offset, needle, path) {
      path.insert(0, label);
      return true;
    }
  }
  false
}
