//! Table Layout Module
//!
//! This module contains all table-related layout algorithms and types.
//!
//! # Architecture
//!
//! Table layout follows CSS Tables Module Level 3 and CSS 2.1 Chapter 17:
//!
//! 1. **Structure Analysis**: Parse table into rows, columns, cells
//! 2. **Column Width Computation**: Calculate column widths based on constraints
//! 3. **Row Height Computation**: Calculate row heights based on content
//! 4. **Cell Positioning**: Position cells in the table grid
//!
//! # Module Organization
//!
//! - `column_distribution` - Column width distribution algorithm (W3.T07)
//!
//! # References
//!
//! - CSS Tables Module Level 3: https://www.w3.org/TR/css-tables-3/
//! - CSS 2.1 Section 17: https://www.w3.org/TR/CSS21/tables.html

pub mod column_distribution;
