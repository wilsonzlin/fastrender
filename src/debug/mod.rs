//! Enhanced debug and visualization tools
//!
//! This module provides additional visualization capabilities beyond the basic
//! tree printing in `tree::debug`. Features include:
//!
//! - **Configurable output**: Colors, depth limits, format options
//! - **Tree diffing**: Compare two trees and show differences
//! - **DOT export**: Generate Graphviz diagrams
//!
//! # Examples
//!
//! ```
//! use fastrender::debug::{EnhancedTreePrinter, PrintConfig, ColorMode};
//! use fastrender::tree::{BoxNode, FormattingContextType};
//! use fastrender::tree::box_tree::ComputedStyle;
//! use std::sync::Arc;
//!
//! let style = Arc::new(ComputedStyle::default());
//! let root = BoxNode::new_block(style, FormattingContextType::Block, vec![]);
//!
//! let printer = EnhancedTreePrinter::with_config(PrintConfig::detailed());
//! println!("{}", printer.print_box_tree(&root));
//! ```

mod tree_printer;

pub use tree_printer::{ColorMode, DiffMode, DotExporter, EnhancedTreePrinter, PrintConfig, TreeDiff};
