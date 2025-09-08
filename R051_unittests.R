# Unit Tests for R051 Functions
# Tests for parliament size analysis and data integrity checking functions

library(testthat)
library(data.table)

# Test grab_pct_women function
test_that("grab_pct_women returns correct structure", {
  
  # Create test data
  test_term_starts <- data.table(
    parliament_id = c("TEST_2020", "TEST_2024"),
    term_start = as.Date(c("2020-01-01", "2024-01-01"))
  )
  
  test_daily_counts <- data.table(
    thisday = seq(as.Date("2019-12-30"), as.Date("2024-01-03"), by = "day"),
    pol_all = 150,
    pol_f = 50,
    proportion_female = 50/150
  )
  
  # Test function call
  result <- grab_pct_women(test_term_starts, -1, test_daily_counts)
  
  # Check structure
  expect_s3_class(result, "data.table")
  expect_true(all(c("parliament_id", "term_start", "target_day", "offset_days", 
                    "pol_all", "pol_f", "proportion_female", "pct_women") %in% names(result)))
  expect_equal(nrow(result), 2)
  expect_equal(result$offset_days, c(-1, -1))
})

test_that("grab_pct_women handles different offset days", {
  
  # Create minimal test data
  test_term_starts <- data.table(
    parliament_id = "TEST_2020",
    term_start = as.Date("2020-01-01")
  )
  
  test_daily_counts <- data.table(
    thisday = seq(as.Date("2019-12-25"), as.Date("2020-01-10"), by = "day"),
    pol_all = 100,
    pol_f = 30,
    proportion_female = 0.3
  )
  
  # Test different offsets
  result_before <- grab_pct_women(test_term_starts, -5, test_daily_counts)
  result_after <- grab_pct_women(test_term_starts, 7, test_daily_counts)
  
  expect_equal(result_before$target_day, as.Date("2019-12-27"))
  expect_equal(result_after$target_day, as.Date("2020-01-08"))
  expect_equal(result_before$pct_women, 30)
  expect_equal(result_after$pct_women, 30)
})

test_that("grab_pct_women handles missing data gracefully", {
  
  test_term_starts <- data.table(
    parliament_id = "TEST_2020",
    term_start = as.Date("2020-01-01")
  )
  
  # Limited daily counts (missing target day)
  test_daily_counts <- data.table(
    thisday = seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day"),
    pol_all = 100,
    pol_f = 30,
    proportion_female = 0.3
  )
  
  # Request data from before the available range
  result <- grab_pct_women(test_term_starts, -10, test_daily_counts)
  
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$pct_women))
})

# Test detect_parliament_deviations function
test_that("detect_parliament_deviations identifies deviations correctly", {
  
  # Create test data with known deviation
  test_daily_counts <- data.table(
    thisday = seq(as.Date("2020-01-01"), as.Date("2020-06-30"), by = "day"),
    pol_all = c(rep(145, 90), rep(160, 91)) # 5 low, then 10 high
  )
  
  test_baseline <- data.table(
    parliament_id = "TEST_2020",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-12-31"),
    baseline_size = 150
  )
  
  # Test with default thresholds (5 seats, 90 days)
  result <- detect_parliament_deviations(test_daily_counts, test_baseline)
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1) # Should find one period (the high period)
  expect_equal(result$deviation_type, "structurally_too_high")
  expect_equal(result$avg_deviation, 10)
  expect_true(result$duration_days >= 90)
})

test_that("detect_parliament_deviations respects thresholds", {
  
  # Create test data with small deviation
  test_daily_counts <- data.table(
    thisday = seq(as.Date("2020-01-01"), as.Date("2020-06-30"), by = "day"),
    pol_all = rep(147, 181) # Only 3 seats low
  )
  
  test_baseline <- data.table(
    parliament_id = "TEST_2020",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-12-31"),
    baseline_size = 150
  )
  
  # Test with default threshold (5 seats) - should find nothing
  result_default <- detect_parliament_deviations(test_daily_counts, test_baseline)
  expect_equal(nrow(result_default), 0)
  
  # Test with lower threshold (2 seats) - should find the deviation
  result_strict <- detect_parliament_deviations(test_daily_counts, test_baseline, 
                                               seat_threshold = 2, 
                                               duration_threshold_days = 90)
  expect_equal(nrow(result_strict), 1)
  expect_equal(result_strict$deviation_type, "structurally_too_low")
})

test_that("detect_parliament_deviations handles duration threshold", {
  
  # Create test data with short deviation
  test_daily_counts <- data.table(
    thisday = seq(as.Date("2020-01-01"), as.Date("2020-03-31"), by = "day"),
    pol_all = rep(140, 91) # 10 seats low for ~91 days
  )
  
  test_baseline <- data.table(
    parliament_id = "TEST_2020",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-12-31"),
    baseline_size = 150
  )
  
  # Test with long duration threshold - should find nothing
  result_long <- detect_parliament_deviations(test_daily_counts, test_baseline,
                                             duration_threshold_days = 100)
  expect_equal(nrow(result_long), 0)
  
  # Test with short duration threshold - should find the deviation
  result_short <- detect_parliament_deviations(test_daily_counts, test_baseline,
                                              duration_threshold_days = 80)
  expect_equal(nrow(result_short), 1)
})

test_that("detect_parliament_deviations handles multiple parliament periods", {
  
  # Create test data spanning two parliament periods
  test_daily_counts <- data.table(
    thisday = seq(as.Date("2020-01-01"), as.Date("2025-01-01"), by = "day"),
    pol_all = c(rep(90, 365*2), rep(140, 365*3)) # Low for 2 years, then different baseline
  )
  
  test_baseline <- data.table(
    parliament_id = c("TEST_2020", "TEST_2022"),
    start_date = c(as.Date("2020-01-01"), as.Date("2022-01-01")),
    end_date = c(as.Date("2021-12-31"), as.Date("2024-12-31")),
    baseline_size = c(100, 150) # Different baseline sizes
  )
  
  result <- detect_parliament_deviations(test_daily_counts, test_baseline)
  
  expect_s3_class(result, "data.table")
  # Should find one major deviation in the second period
  expect_true(nrow(result) >= 1)
  expect_true(any(grepl("TEST_2022", result$parliament_ids)))
})

test_that("detect_parliament_deviations returns correct column names", {
  
  # Minimal test data
  test_daily_counts <- data.table(
    thisday = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"),
    pol_all = rep(140, 366)
  )
  
  test_baseline <- data.table(
    parliament_id = "TEST_2020",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-12-31"),
    baseline_size = 150
  )
  
  result <- detect_parliament_deviations(test_daily_counts, test_baseline)
  
  expected_columns <- c("start_date", "end_date", "duration_days", 
                       "deviation_type", "avg_deviation", "max_deviation", 
                       "parliament_ids")
  
  if(nrow(result) > 0) {
    expect_true(all(expected_columns %in% names(result)))
  }
})

test_that("detect_parliament_deviations handles empty input", {
  
  # Empty daily counts
  empty_daily_counts <- data.table(
    thisday = as.Date(character(0)),
    pol_all = integer(0)
  )
  
  test_baseline <- data.table(
    parliament_id = "TEST_2020",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-12-31"),
    baseline_size = 150
  )
  
  result <- detect_parliament_deviations(empty_daily_counts, test_baseline)
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

# Integration test combining both functions
test_that("Functions work together in typical workflow", {
  
  # Create realistic test data
  test_parl <- data.table(
    parliament_id = c("NL_TEST_2020", "NL_TEST_2024"),
    leg_period_start_dateformat = as.Date(c("2020-01-01", "2024-01-01")),
    leg_period_end_dateformat = as.Date(c("2023-12-31", "2027-12-31"))
  )
  
  test_daily_counts <- data.table(
    thisday = seq(as.Date("2019-12-01"), as.Date("2024-02-01"), by = "day"),
    pol_all = 150,
    pol_f = 50,
    proportion_female = 50/150
  )
  
  # Test grab_pct_women workflow
  term_starts <- unique(test_parl[, .(parliament_id, term_start = leg_period_start_dateformat)])
  before_data <- grab_pct_women(term_starts, -30, test_daily_counts)
  after_data <- grab_pct_women(term_starts, 30, test_daily_counts)
  
  expect_equal(nrow(before_data), 2)
  expect_equal(nrow(after_data), 2)
  expect_true(all(!is.na(before_data$pct_women)))
  expect_true(all(!is.na(after_data$pct_women)))
  
  # Test detect_parliament_deviations workflow
  baseline_data <- test_parl[, .(
    parliament_id,
    start_date = leg_period_start_dateformat,
    end_date = leg_period_end_dateformat,
    baseline_size = 150
  )]
  
  deviations <- detect_parliament_deviations(test_daily_counts, baseline_data)
  
  expect_s3_class(deviations, "data.table")
  # With perfect data, should find no deviations
  expect_equal(nrow(deviations), 0)
})