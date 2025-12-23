//! Integration tests for Table Column Width Distribution Algorithm
//!
//! These tests verify the table column distribution algorithm works correctly
//! for various real-world scenarios based on CSS 2.1 Section 17.5.2.2 and
//! CSS Tables Module Level 3.

use fastrender::layout::contexts::table::column_distribution::compute_column_constraints;
use fastrender::layout::contexts::table::column_distribution::distribute_spanning_cell_width;
use fastrender::layout::contexts::table::column_distribution::ColumnConstraints;
use fastrender::layout::contexts::table::column_distribution::ColumnDistributor;
use fastrender::layout::contexts::table::column_distribution::DistributionMode;

// =============================================================================
// Test Utilities
// =============================================================================

/// Helper to check that total width is approximately equal to expected
fn assert_total_width(widths: &[f32], expected: f32, tolerance: f32) {
  let total: f32 = widths.iter().sum();
  assert!(
    (total - expected).abs() <= tolerance,
    "Expected total width ~{}, got {}",
    expected,
    total
  );
}

/// Helper to check all widths are at least their minimum
fn assert_respects_minimums(widths: &[f32], columns: &[ColumnConstraints]) {
  for (i, (width, col)) in widths.iter().zip(columns.iter()).enumerate() {
    assert!(
      *width >= col.min_width - 0.01, // Small tolerance for floating point
      "Column {} has width {} < min {}",
      i,
      width,
      col.min_width
    );
  }
}

// =============================================================================
// Equal Distribution Tests
// =============================================================================

#[test]
fn test_equal_distribution_two_columns() {
  let columns = vec![
    ColumnConstraints::new(50.0, 200.0),
    ColumnConstraints::new(50.0, 200.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 200.0);

  // Both columns should get equal width
  assert!((result.widths[0] - result.widths[1]).abs() < 0.01);
  assert_total_width(&result.widths, 200.0, 0.01);
}

#[test]
fn test_equal_distribution_three_columns() {
  let columns = vec![
    ColumnConstraints::new(50.0, 300.0),
    ColumnConstraints::new(50.0, 300.0),
    ColumnConstraints::new(50.0, 300.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 300.0);

  // All columns should be approximately equal
  let avg = result.total_width / 3.0;
  for width in &result.widths {
    assert!((width - avg).abs() < 0.01);
  }
}

#[test]
fn test_equal_distribution_many_columns() {
  let columns: Vec<_> = (0..10)
    .map(|_| ColumnConstraints::new(20.0, 200.0))
    .collect();
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 500.0);

  assert_eq!(result.column_count(), 10);
  assert_total_width(&result.widths, 500.0, 0.1);
}

// =============================================================================
// Fixed Width Tests
// =============================================================================

#[test]
fn test_fixed_width_single_column() {
  let columns = vec![ColumnConstraints::fixed(150.0)];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 300.0);

  assert_eq!(result.widths[0], 150.0);
}

#[test]
fn test_fixed_width_all_columns() {
  let columns = vec![
    ColumnConstraints::fixed(100.0),
    ColumnConstraints::fixed(150.0),
    ColumnConstraints::fixed(200.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 600.0);

  assert_eq!(result.widths[0], 100.0);
  assert_eq!(result.widths[1], 150.0);
  assert_eq!(result.widths[2], 200.0);
}

#[test]
fn test_fixed_width_mixed_with_auto() {
  let columns = vec![
    ColumnConstraints::fixed(100.0),
    ColumnConstraints::new(50.0, 500.0), // Flexible
    ColumnConstraints::fixed(100.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 500.0);

  assert_eq!(result.widths[0], 100.0);
  assert!((result.widths[1] - 300.0).abs() < 0.01); // Gets remaining
  assert_eq!(result.widths[2], 100.0);
}

#[test]
fn test_fixed_width_exceeds_available() {
  let columns = vec![
    ColumnConstraints::fixed(200.0),
    ColumnConstraints::fixed(200.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 300.0);

  // When overconstrained, fixed widths hold and we report overflow instead of scaling.
  assert!((result.widths[0] - 200.0).abs() < 0.01);
  assert!((result.widths[1] - 200.0).abs() < 0.01);
  assert!(result.is_over_constrained);
  assert!((result.overflow_amount - 100.0).abs() < 0.01);
  assert!((result.total_width - 400.0).abs() < 0.01);
}

// =============================================================================
// Percentage Width Tests
// =============================================================================

#[test]
fn test_percentage_width_single() {
  let columns = vec![ColumnConstraints::percentage(50.0, 10.0, 500.0)];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 400.0);

  // 50% of 400 = 200
  assert!((result.widths[0] - 200.0).abs() < 0.01);
}

#[test]
fn test_percentage_width_multiple() {
  let columns = vec![
    ColumnConstraints::percentage(25.0, 10.0, 500.0),
    ColumnConstraints::percentage(75.0, 10.0, 500.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 400.0);

  assert!((result.widths[0] - 100.0).abs() < 0.01); // 25%
  assert!((result.widths[1] - 300.0).abs() < 0.01); // 75%
}

#[test]
fn test_percentage_width_exceeds_100() {
  let columns = vec![
    ColumnConstraints::percentage(60.0, 10.0, 500.0),
    ColumnConstraints::percentage(60.0, 10.0, 500.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 400.0);

  // Percentages above 100% are applied as-authored, making the table over-constrained.
  assert!((result.widths[0] - 240.0).abs() < 0.01);
  assert!((result.widths[1] - 240.0).abs() < 0.01);
  assert!(result.is_over_constrained);
  assert!((result.total_width - 480.0).abs() < 0.01);
}

#[test]
fn test_percentage_width_respects_minimum() {
  // Percentage would give 40, but min is 100
  let columns = vec![ColumnConstraints::percentage(10.0, 100.0, 500.0)];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 400.0);

  // Min width should take precedence
  assert!(result.widths[0] >= 100.0);
}

// =============================================================================
// Mixed Constraints Tests
// =============================================================================

#[test]
fn test_mixed_fixed_percentage_auto() {
  let columns = vec![
    ColumnConstraints::fixed(100.0),
    ColumnConstraints::percentage(20.0, 50.0, 300.0),
    ColumnConstraints::new(50.0, 400.0), // Auto
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 500.0);

  assert_eq!(result.widths[0], 100.0);
  assert!((result.widths[1] - 100.0).abs() < 0.01); // 20% of 500
                                                    // Remaining ~300 goes to auto column
  assert!((result.widths[2] - 300.0).abs() < 1.0);
}

#[test]
fn test_mixed_all_types_complex() {
  let columns = vec![
    ColumnConstraints::fixed(80.0),
    ColumnConstraints::percentage(15.0, 40.0, 200.0),
    ColumnConstraints::new(60.0, 250.0),
    ColumnConstraints::percentage(25.0, 50.0, 300.0),
    ColumnConstraints::new(40.0, 180.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 600.0);

  assert_eq!(result.column_count(), 5);
  assert_respects_minimums(&result.widths, &columns);
}

// =============================================================================
// Over-Constrained Tests
// =============================================================================

#[test]
fn test_over_constrained_keeps_minimums() {
  let columns = vec![
    ColumnConstraints::new(100.0, 200.0),
    ColumnConstraints::new(100.0, 200.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  // Only 100 available, but min is 200
  let result = distributor.distribute(&columns, 100.0);

  assert!(result.is_over_constrained);
  // Keep minimum widths and expose the overflow rather than scaling below min.
  assert!((result.widths[0] - 100.0).abs() < 0.01);
  assert!((result.widths[1] - 100.0).abs() < 0.01);
  assert!((result.overflow_amount - 100.0).abs() < 0.01);
  assert!((result.total_width - 200.0).abs() < 0.01);
}

#[test]
fn test_over_constrained_unequal_minimums_keep_minimums() {
  let columns = vec![
    ColumnConstraints::new(200.0, 400.0),
    ColumnConstraints::new(100.0, 200.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  // Total min = 300, available = 150 (half)
  let result = distributor.distribute(&columns, 150.0);

  assert!(result.is_over_constrained);
  // Minimum widths are preserved even when over-constrained.
  assert!((result.widths[0] - 200.0).abs() < 0.01);
  assert!((result.widths[1] - 100.0).abs() < 0.01);
  assert!((result.overflow_amount - 150.0).abs() < 0.01);
  assert!((result.total_width - 300.0).abs() < 0.01);
}

// =============================================================================
// Colspan Tests
// =============================================================================

#[test]
fn test_colspan_increases_column_widths() {
  let mut columns = vec![
    ColumnConstraints::new(50.0, 100.0),
    ColumnConstraints::new(50.0, 100.0),
  ];

  // Spanning cell requires 250px minimum
  distribute_spanning_cell_width(&mut columns, 0, 2, 250.0, 400.0);

  let total_min: f32 = columns.iter().map(|c| c.min_width).sum();
  assert!(total_min >= 250.0);
}

#[test]
fn test_colspan_no_change_if_sufficient() {
  let mut columns = vec![
    ColumnConstraints::new(150.0, 200.0),
    ColumnConstraints::new(150.0, 200.0),
  ];

  // Spanning cell requires only 200px (already satisfied)
  distribute_spanning_cell_width(&mut columns, 0, 2, 200.0, 300.0);

  // Should not change
  assert_eq!(columns[0].min_width, 150.0);
  assert_eq!(columns[1].min_width, 150.0);
}

#[test]
fn test_colspan_partial_span() {
  let mut columns = vec![
    ColumnConstraints::new(50.0, 100.0),
    ColumnConstraints::new(50.0, 100.0),
    ColumnConstraints::new(50.0, 100.0),
  ];

  // Span only columns 1 and 2
  distribute_spanning_cell_width(&mut columns, 1, 3, 200.0, 400.0);

  // Column 0 should be unchanged
  assert_eq!(columns[0].min_width, 50.0);
  // Columns 1 and 2 should have increased minimums
  assert!(columns[1].min_width + columns[2].min_width >= 200.0);
}

#[test]
fn test_compute_constraints_with_colspan() {
  // Same test as unit test but with 3 columns
  let cell_widths = vec![
    vec![
      (50.0, 100.0, 1_usize),
      (50.0, 100.0, 1_usize),
      (50.0, 100.0, 1_usize),
    ],
    vec![(250.0, 400.0, 3_usize)], // Spans all 3 columns
  ];

  let constraints = compute_column_constraints(&cell_widths, 3);

  // After processing:
  // - First pass: each column gets min=50 from non-spanning cells
  // - Second pass: spanning cell adds (250-150)/3 = 33.33 to each
  // Total min should be ~250
  let total_min: f32 = constraints.iter().map(|c| c.min_width).sum();
  assert!(
    total_min >= 249.9, // Use 249.9 to account for float rounding
    "Expected total_min >= 250, got {}: cols=[{:.2}, {:.2}, {:.2}]",
    total_min,
    constraints[0].min_width,
    constraints[1].min_width,
    constraints[2].min_width
  );
}

// =============================================================================
// Fixed Layout Mode Tests
// =============================================================================

#[test]
fn test_fixed_mode_equal_distribution() {
  let columns = vec![
    ColumnConstraints::new(50.0, 200.0),
    ColumnConstraints::new(50.0, 200.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Fixed);

  let result = distributor.distribute(&columns, 300.0);

  // Fixed mode divides equally among flexible columns
  assert!((result.widths[0] - 150.0).abs() < 0.01);
  assert!((result.widths[1] - 150.0).abs() < 0.01);
}

#[test]
fn test_fixed_mode_with_fixed_column() {
  let columns = vec![
    ColumnConstraints::fixed(100.0),
    ColumnConstraints::new(50.0, 300.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Fixed);

  let result = distributor.distribute(&columns, 400.0);

  assert_eq!(result.widths[0], 100.0);
  assert!((result.widths[1] - 300.0).abs() < 0.01);
}

#[test]
fn test_fixed_mode_with_percentage() {
  let columns = vec![
    ColumnConstraints::percentage(30.0, 50.0, 300.0),
    ColumnConstraints::new(50.0, 400.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Fixed);

  let result = distributor.distribute(&columns, 500.0);

  // 30% of 500 = 150
  assert!((result.widths[0] - 150.0).abs() < 0.01);
  // Remaining 350 goes to flexible column
  assert!((result.widths[1] - 350.0).abs() < 0.01);
}

// =============================================================================
// Edge Cases
// =============================================================================

#[test]
fn test_empty_columns() {
  let distributor = ColumnDistributor::new(DistributionMode::Auto);
  let result = distributor.distribute(&[], 500.0);

  assert_eq!(result.column_count(), 0);
  assert_eq!(result.total_width, 0.0);
  assert!(!result.is_over_constrained);
}

#[test]
fn test_zero_available_width() {
  let columns = vec![ColumnConstraints::new(100.0, 200.0)];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 0.0);

  // Should return minimums
  assert_eq!(result.widths[0], 100.0);
}

#[test]
fn test_negative_available_width() {
  let columns = vec![ColumnConstraints::new(50.0, 100.0)];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, -100.0);

  // Should handle gracefully
  assert!(result.widths[0] > 0.0);
}

#[test]
fn test_very_large_available_width() {
  let columns = vec![
    ColumnConstraints::new(50.0, 100.0),
    ColumnConstraints::new(50.0, 100.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 10000.0);

  // Should cap at maximums
  assert_eq!(result.widths[0], 100.0);
  assert_eq!(result.widths[1], 100.0);
}

#[test]
fn test_zero_min_max_columns() {
  let columns = vec![ColumnConstraints::zero(), ColumnConstraints::zero()];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 200.0);

  // Should handle zero-sized columns
  assert_eq!(result.column_count(), 2);
}

#[test]
fn test_min_column_width_setting() {
  let columns = vec![ColumnConstraints::zero(), ColumnConstraints::zero()];
  let distributor = ColumnDistributor::new(DistributionMode::Auto).with_min_column_width(25.0);

  let result = distributor.distribute(&columns, 200.0);

  assert!(result.widths[0] >= 25.0);
  assert!(result.widths[1] >= 25.0);
}

// =============================================================================
// Compute Column Constraints Tests
// =============================================================================

#[test]
fn test_compute_constraints_simple_table() {
  // 2x2 table
  let cell_widths = vec![
    vec![(50.0, 100.0, 1), (75.0, 150.0, 1)],
    vec![(60.0, 120.0, 1), (80.0, 160.0, 1)],
  ];

  let constraints = compute_column_constraints(&cell_widths, 2);

  // Column 0: max(50, 60) = 60
  assert_eq!(constraints[0].min_width, 60.0);
  // Column 0: max(100, 120) = 120
  assert_eq!(constraints[0].max_width, 120.0);
  // Column 1: max(75, 80) = 80
  assert_eq!(constraints[1].min_width, 80.0);
  // Column 1: max(150, 160) = 160
  assert_eq!(constraints[1].max_width, 160.0);
}

#[test]
fn test_compute_constraints_uneven_rows() {
  // First row has 2 cells, second has 1 wide cell
  let cell_widths = vec![
    vec![(50.0, 100.0, 1), (50.0, 100.0, 1)],
    vec![(200.0, 400.0, 2)],
  ];

  let constraints = compute_column_constraints(&cell_widths, 2);

  // Spanning cell requires 200 across 2 columns
  let total_min: f32 = constraints.iter().map(|c| c.min_width).sum();
  assert!(total_min >= 200.0);
}

#[test]
fn test_compute_constraints_empty_table() {
  let cell_widths: Vec<Vec<(f32, f32, usize)>> = vec![];

  let constraints = compute_column_constraints(&cell_widths, 5);

  assert_eq!(constraints.len(), 5);
  for col in &constraints {
    assert_eq!(col.min_width, 0.0);
    assert_eq!(col.max_width, 0.0);
  }
}

// =============================================================================
// Real-World Scenario Tests
// =============================================================================

#[test]
fn test_typical_data_table() {
  // Typical data table with header and data rows
  // Max widths sum to 50+200+150+120 = 520
  let columns = vec![
    ColumnConstraints::new(30.0, 50.0),   // Checkbox column
    ColumnConstraints::new(100.0, 200.0), // Name column
    ColumnConstraints::new(80.0, 150.0),  // Date column
    ColumnConstraints::new(60.0, 120.0),  // Status column
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 600.0);

  assert_respects_minimums(&result.widths, &columns);
  // When available > total_max, columns get their max widths (520 total)
  assert_total_width(&result.widths, 520.0, 1.0);
}

#[test]
fn test_responsive_table_narrow() {
  // Same table at narrow width
  let columns = vec![
    ColumnConstraints::new(30.0, 50.0),
    ColumnConstraints::new(100.0, 200.0),
    ColumnConstraints::new(80.0, 150.0),
    ColumnConstraints::new(60.0, 120.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  // Narrow viewport
  let result = distributor.distribute(&columns, 350.0);

  // Should use minimums since 350 > 270 (sum of mins)
  assert_respects_minimums(&result.widths, &columns);
  assert_total_width(&result.widths, 350.0, 1.0);
}

#[test]
fn test_table_with_percentage_columns() {
  // Design table with fixed first column and percentage-based layout
  let columns = vec![
    ColumnConstraints::fixed(60.0),
    ColumnConstraints::percentage(40.0, 100.0, 400.0),
    ColumnConstraints::percentage(60.0, 150.0, 600.0),
  ];
  let distributor = ColumnDistributor::new(DistributionMode::Auto);

  let result = distributor.distribute(&columns, 1000.0);

  assert_eq!(result.widths[0], 60.0);
  // Note: percentages are computed off total, then fixed is subtracted
  // So behavior may differ based on implementation
  assert!(result.widths[1] >= 100.0); // At least min
  assert!(result.widths[2] >= 150.0); // At least min
}
