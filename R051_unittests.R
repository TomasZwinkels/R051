# Unit Tests for R051 Functions
# Tests for parliament size analysis and data integrity checking functions

library(testthat)
library(data.table)

test_file_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) getwd()
)
source(file.path(test_file_dir, "R051_functions.R"))

# Suppress expected warnings from edge case testing
# These warnings are expected and harmless:
# - data.table length recycling in test data creation  
# - min/max of empty data when testing edge cases

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
    pol_all = c(rep(145, 90), rep(160, 92)) # 5 low, then 10 high
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
    pol_all = rep(147, 182) # Only 3 seats low
  )
  
  test_baseline <- data.table(
    parliament_id = "TEST_2020",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-12-31"),
    baseline_size = 150
  )
  
  # Test with default threshold (5 seats) - should find nothing
  result_default <- suppressWarnings(detect_parliament_deviations(test_daily_counts, test_baseline))
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
    pol_all = c(rep(90, 365*2 + 1), rep(140, 365*3 + 2)) # Low for 2 years, then different baseline
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
  
  result <- suppressWarnings(detect_parliament_deviations(empty_daily_counts, test_baseline))
  
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
  
  deviations <- suppressWarnings(detect_parliament_deviations(test_daily_counts, baseline_data))
  
  expect_s3_class(deviations, "data.table")
  # With perfect data, should find no deviations
  expect_equal(nrow(deviations), 0)
})

# Test merge_gap_days functionality
test_that("detect_parliament_deviations merges closely spaced periods", {
  
  # Create test data with two close deviation periods
  test_daily_counts <- data.table(
    thisday = seq(as.Date("2020-01-01"), as.Date("2020-07-31"), by = "day"),
    pol_all = c(rep(140, 90),     # 90 days too low
                rep(150, 5),      # 5 days normal (gap)
                rep(140, 90),     # 90 days too low
                rep(150, 28))     # rest normal
  )
  
  test_baseline <- data.table(
    parliament_id = "TEST_2020",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-12-31"),
    baseline_size = 150
  )
  
  # Test with default merge_gap_days (7) - should merge
  result_merged <- detect_parliament_deviations(test_daily_counts, test_baseline,
                                               merge_gap_days = 7)
  expect_equal(nrow(result_merged), 1)
  expect_equal(result_merged$duration_days, 185) # 90 + 5 + 90
  
  # Test with merge_gap_days = 0 - should NOT merge
  result_not_merged <- detect_parliament_deviations(test_daily_counts, test_baseline,
                                                   merge_gap_days = 0)
  expect_equal(nrow(result_not_merged), 2)
  expect_true(all(result_not_merged$duration_days == 90))
})

test_that("detect_parliament_deviations respects merge_gap_days threshold", {
  
  # Create test data with periods separated by 10 days
  test_daily_counts <- data.table(
    thisday = seq(as.Date("2020-01-01"), as.Date("2020-08-31"), by = "day"),
    pol_all = c(rep(140, 90),     # 90 days too low
                rep(150, 10),     # 10 days normal (gap)
                rep(140, 90),     # 90 days too low
                rep(150, 54))     # rest normal
  )
  
  test_baseline <- data.table(
    parliament_id = "TEST_2020",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-12-31"),
    baseline_size = 150
  )
  
  # Test with merge_gap_days = 7 - should NOT merge (gap is 10 days)
  result_7_days <- detect_parliament_deviations(test_daily_counts, test_baseline,
                                               merge_gap_days = 7)
  expect_equal(nrow(result_7_days), 2)
  
  # Test with merge_gap_days = 15 - should merge (gap is 10 days)
  result_15_days <- detect_parliament_deviations(test_daily_counts, test_baseline,
                                                merge_gap_days = 15)
  expect_equal(nrow(result_15_days), 1)
  expect_equal(result_15_days$duration_days, 190) # 90 + 10 + 90
})

test_that("detect_parliament_deviations only merges same deviation types", {
  
  # Create test data with alternating high/low deviations
  test_daily_counts <- data.table(
    thisday = seq(as.Date("2020-01-01"), as.Date("2020-09-30"), by = "day"),
    pol_all = c(rep(140, 90),     # 90 days too low
                rep(150, 3),      # 3 days normal (small gap)
                rep(160, 90),     # 90 days too high
                rep(150, 91))     # rest normal
  )
  
  test_baseline <- data.table(
    parliament_id = "TEST_2020",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-12-31"),
    baseline_size = 150
  )
  
  # Even with large merge_gap_days, should NOT merge different types
  result <- detect_parliament_deviations(test_daily_counts, test_baseline,
                                        merge_gap_days = 30)
  expect_equal(nrow(result), 2)
  expect_true("structurally_too_low" %in% result$deviation_type)
  expect_true("structurally_too_high" %in% result$deviation_type)
  expect_true(all(result$duration_days == 90))
})

test_that("detect_parliament_deviations updates merged period statistics correctly", {
  
  # Create test data with two periods having different average deviations
  test_daily_counts <- data.table(
    thisday = seq(as.Date("2020-01-01"), as.Date("2020-07-31"), by = "day"),
    pol_all = c(rep(140, 90),     # 90 days, -10 deviation
                rep(150, 2),      # 2 days normal (small gap)
                rep(135, 90),     # 90 days, -15 deviation
                rep(150, 31))     # rest normal
  )
  
  test_baseline <- data.table(
    parliament_id = "TEST_2020",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-12-31"),
    baseline_size = 150
  )
  
  result <- detect_parliament_deviations(test_daily_counts, test_baseline,
                                        merge_gap_days = 5)
  
  expect_equal(nrow(result), 1)
  # Check that statistics are properly calculated for merged period
  expect_equal(result$duration_days, 182) # 90 + 2 + 90
  expect_equal(result$avg_deviation, -11.7) # Weighted average: (-10*90 + -15*90)/(90+2+90)
  expect_equal(result$max_deviation, 15) # Maximum absolute deviation
})

# ==================================================================
# Block: find_new_cohort_day()
# ==================================================================

# Helper to build minimal PARL for find_new_cohort_day tests
mk_test_parl_cohort <- function(parliament_id, start, end) {
  data.frame(
    parliament_id = parliament_id,
    leg_period_start_dateformat = as.Date(start),
    leg_period_end_dateformat = as.Date(end),
    stringsAsFactors = FALSE
  )
}

# Helper to build minimal RESE for find_new_cohort_day tests
mk_test_rese_cohort <- function(starts, ends) {
  data.frame(
    res_entry_start = starts,
    res_entry_end = ends,
    stringsAsFactors = FALSE
  )
}

test_that("find_new_cohort_day returns the day with most transitions", {
  # Two consecutive parliaments so the midpoint search window works
  PARL <- rbind(
    mk_test_parl_cohort("P0", "2015-01-01", "2018-12-31"),
    mk_test_parl_cohort("P1", "2019-01-01", "2023-01-01")
  )
  # 5 entries on 2019-10-21, 3 exits on 2019-10-20, 1 entry on 2020-03-01
  RESE <- mk_test_rese_cohort(
    starts = c(rep("21oct2019", 5), "01mar2020"),
    ends   = c(rep("20oct2019", 3), rep("01jan2023", 3))
  )
  result <- find_new_cohort_day("P1", RESE, PARL)
  expect_equal(result, as.Date("2019-10-21"))
})

test_that("find_new_cohort_day picks the peak when entries and exits are on same day", {
  PARL <- rbind(
    mk_test_parl_cohort("P0", "2015-01-01", "2018-12-31"),
    mk_test_parl_cohort("P1", "2019-01-01", "2023-01-01")
  )
  RESE <- mk_test_rese_cohort(
    starts = c(rep("21oct2019", 5), rep("01jan2019", 4)),
    ends   = c(rep("21oct2019", 4), rep("01jan2023", 5))
  )
  result <- find_new_cohort_day("P1", RESE, PARL)
  expect_equal(result, as.Date("2019-10-21"))
})

test_that("find_new_cohort_day returns NA when no transitions in search window", {
  PARL <- rbind(
    mk_test_parl_cohort("P0", "2010-01-01", "2014-12-31"),
    mk_test_parl_cohort("P1", "2019-01-01", "2023-01-01")
  )
  # All dates well outside the search window
  RESE <- mk_test_rese_cohort(
    starts = c("01jan2000"),
    ends   = c("01jan2005")
  )
  result <- find_new_cohort_day("P1", RESE, PARL)
  expect_true(is.na(result))
})

test_that("find_new_cohort_day warns on unknown parliament_id", {
  PARL <- mk_test_parl_cohort("P1", "2019-01-01", "2023-01-01")
  RESE <- mk_test_rese_cohort("21oct2019", "01jan2023")
  expect_warning(find_new_cohort_day("UNKNOWN", RESE, PARL), "not found")
})

test_that("find_new_cohort_day handles rcen markers in dates", {
  PARL <- rbind(
    mk_test_parl_cohort("P0", "2015-01-01", "2018-12-31"),
    mk_test_parl_cohort("P1", "2019-01-01", "2026-12-31")
  )
  RESE <- mk_test_rese_cohort(
    starts = c(rep("21oct2019", 5)),
    ends   = c(rep("10mar2026[[rcen]]", 5))
  )
  result <- find_new_cohort_day("P1", RESE, PARL)
  expect_equal(result, as.Date("2019-10-21"))
})

test_that("find_new_cohort_day works when PARL is a data.table", {
  # This test catches the scoping bug where data.table's [.data.table
  # evaluates parliament_id as the column name instead of the function argument
  PARL <- rbind(
    mk_test_parl_cohort("P0", "2015-01-01", "2018-12-31"),
    mk_test_parl_cohort("P1", "2019-01-01", "2023-01-01")
  )
  setDT(PARL)  # convert to data.table — this is what R051.R does
  RESE <- mk_test_rese_cohort(
    starts = c(rep("21oct2019", 5)),
    ends   = c(rep("01jan2023", 5))
  )
  result <- find_new_cohort_day("P1", RESE, PARL)
  expect_equal(result, as.Date("2019-10-21"))
})

test_that("find_new_cohort_day finds election day before session start", {
  # Simulates the Canada pattern: election Oct 21, session starts Dec 5
  # The function should find the election day, not a mid-term date
  PARL <- rbind(
    mk_test_parl_cohort("P0", "2015-12-03", "2019-09-11"),
    mk_test_parl_cohort("P1", "2019-12-05", "2021-08-15")
  )
  # Election turnover happens on Oct 21 (before session start Dec 5)
  RESE <- mk_test_rese_cohort(
    starts = c(rep("21oct2019", 200), "15jan2020"),
    ends   = c(rep("20oct2019", 180), rep("15aug2021", 21))
  )
  result <- find_new_cohort_day("P1", RESE, PARL)
  expect_equal(result, as.Date("2019-10-21"))
})

# ==================================================================
# Block: count_mp_transitions()
# ==================================================================

# Helper to build minimal test data for count_mp_transitions
mk_test_rese <- function(pers_ids, starts, ends,
                          country = "CA",
                          pf = "NT_LE-LH_T3_NA_01") {
  data.frame(
    pers_id = pers_ids,
    res_entry_start = starts,
    res_entry_end = ends,
    country_abb = country,
    political_function = pf,
    stringsAsFactors = FALSE
  )
}

mk_test_poli <- function(pers_ids, genders) {
  data.frame(
    pers_id = pers_ids,
    gender = genders,
    stringsAsFactors = FALSE
  )
}

test_that("counts women entering in date window", {
  RESE <- mk_test_rese(
    c("P1", "P2", "P3"),
    c("19oct2015", "21oct2019", "20sep2021"),
    c("20oct2019", "19sep2021", "27apr2025")
  )
  POLI <- mk_test_poli(c("P1", "P2", "P3"), c("f", "f", "m"))

  # P2 (female) entered in the 2019 window
  result <- count_mp_transitions(
    as.Date("2019-10-15"), as.Date("2019-10-25"),
    "entered", "f", "CA", RESE, POLI
  )
  expect_equal(result, 1L)
})

test_that("counts men leaving in date window", {
  RESE <- mk_test_rese(
    c("P1", "P2", "P3"),
    c("19oct2015", "21oct2019", "19oct2015"),
    c("20oct2019", "27apr2025", "20oct2019")
  )
  POLI <- mk_test_poli(c("P1", "P2", "P3"), c("m", "f", "m"))

  # P1 and P3 (both male) left around oct 2019
  result <- count_mp_transitions(
    as.Date("2019-10-15"), as.Date("2019-10-25"),
    "left", "m", "CA", RESE, POLI
  )
  expect_equal(result, 2L)
})

test_that("returns 0 when no transitions in window", {
  RESE <- mk_test_rese(
    c("P1"),
    c("19oct2015"),
    c("20oct2019")
  )
  POLI <- mk_test_poli(c("P1"), c("f"))

  result <- count_mp_transitions(
    as.Date("2020-01-01"), as.Date("2020-12-31"),
    "entered", "f", "CA", RESE, POLI
  )
  expect_equal(result, 0L)
})

test_that("returns 0 for wrong country", {
  RESE <- mk_test_rese(
    c("P1"),
    c("19oct2015"),
    c("20oct2019"),
    country = "NO"
  )
  POLI <- mk_test_poli(c("P1"), c("f"))

  result <- count_mp_transitions(
    as.Date("2015-10-01"), as.Date("2015-10-31"),
    "entered", "f", "CA", RESE, POLI
  )
  expect_equal(result, 0L)
})

test_that("handles tf/tm gender codes", {
  RESE <- mk_test_rese(
    c("P1", "P2"),
    c("19oct2015", "19oct2015"),
    c("20oct2019", "20oct2019")
  )
  POLI <- mk_test_poli(c("P1", "P2"), c("tf", "tm"))

  expect_equal(count_mp_transitions(
    as.Date("2015-10-01"), as.Date("2015-10-31"),
    "entered", "f", "CA", RESE, POLI
  ), 1L)

  expect_equal(count_mp_transitions(
    as.Date("2015-10-01"), as.Date("2015-10-31"),
    "entered", "m", "CA", RESE, POLI
  ), 1L)
})

test_that("filters to parliamentary membership functions only", {
  RESE <- mk_test_rese(
    c("P1", "P2"),
    c("19oct2015", "19oct2015"),
    c("20oct2019", "20oct2019"),
    pf = c("NT_LE-LH_T3_NA_01", "EDUC_1234")
  )
  POLI <- mk_test_poli(c("P1", "P2"), c("f", "f"))

  # Only P1 has a parliamentary function
  result <- count_mp_transitions(
    as.Date("2015-10-01"), as.Date("2015-10-31"),
    "entered", "f", "CA", RESE, POLI
  )
  expect_equal(result, 1L)
})

test_that("counts unique persons not episodes", {
  # Same person with two episodes both starting in the window
  RESE <- mk_test_rese(
    c("P1", "P1"),
    c("19oct2015", "21oct2015"),
    c("20oct2019", "20oct2019")
  )
  POLI <- mk_test_poli(c("P1"), c("f"))

  result <- count_mp_transitions(
    as.Date("2015-10-01"), as.Date("2015-10-31"),
    "entered", "f", "CA", RESE, POLI
  )
  expect_equal(result, 1L)  # unique person, not 2 episodes
})

test_that("errors on invalid direction", {
  RESE <- mk_test_rese("P1", "19oct2015", "20oct2019")
  POLI <- mk_test_poli("P1", "f")
  expect_error(count_mp_transitions(
    as.Date("2015-10-01"), as.Date("2015-10-31"),
    "stayed", "f", "CA", RESE, POLI
  ), "direction")
})

test_that("errors on invalid gender", {
  RESE <- mk_test_rese("P1", "19oct2015", "20oct2019")
  POLI <- mk_test_poli("P1", "f")
  expect_error(count_mp_transitions(
    as.Date("2015-10-01"), as.Date("2015-10-31"),
    "entered", "x", "CA", RESE, POLI
  ), "gender")
})

test_that("handles rcen markers in dates", {
  RESE <- mk_test_rese(
    c("P1"),
    c("19oct2015"),
    c("10mar2026[[rcen]]")
  )
  POLI <- mk_test_poli(c("P1"), c("f"))

  # Should still count the entry
  result <- count_mp_transitions(
    as.Date("2015-10-01"), as.Date("2015-10-31"),
    "entered", "f", "CA", RESE, POLI
  )
  expect_equal(result, 1L)

  # The end date with rcen should parse correctly
  result2 <- count_mp_transitions(
    as.Date("2026-03-01"), as.Date("2026-03-31"),
    "left", "f", "CA", RESE, POLI
  )
  expect_equal(result2, 1L)
})

# ==================================================================
# Block: count_fresh_entrants()
# ==================================================================

# Helper: build a minimal gender_episodes data.table for testing
mk_gender_episodes <- function(pers_ids, starts, ends) {
  data.table(
    pers_id = pers_ids,
    res_entry_start_dateformat = as.Date(starts),
    res_entry_end_dateformat = as.Date(ends)
  )
}

test_that("count_fresh_entrants counts MPs present today but not yesterday", {
  eps <- mk_gender_episodes(
    c("A", "B", "C"),
    c("2019-10-21", "2019-10-21", "2019-01-01"),  # A,B enter on 21st; C already there
    c("2023-01-01", "2023-01-01", "2023-01-01")
  )
  result <- count_fresh_entrants(as.Date("2019-10-21"), eps)
  expect_equal(result, 2L)  # A and B are new; C was already there yesterday
})

test_that("count_fresh_entrants returns 0 when no new entrants", {
  eps <- mk_gender_episodes(
    c("A", "B"),
    c("2019-01-01", "2019-01-01"),
    c("2023-01-01", "2023-01-01")
  )
  result <- count_fresh_entrants(as.Date("2019-06-15"), eps)
  expect_equal(result, 0L)  # both were already there
})

test_that("count_fresh_entrants returns NA for NA date", {
  eps <- mk_gender_episodes("A", "2019-01-01", "2023-01-01")
  result <- count_fresh_entrants(as.Date(NA), eps)
  expect_true(is.na(result))
})

test_that("count_fresh_entrants counts returning MP as fresh if gap in service", {
  # MP served 2015-2019, left, returned 2021
  eps <- mk_gender_episodes(
    c("A", "A"),
    c("2015-01-01", "2021-09-20"),
    c("2019-10-20", "2025-04-27")
  )
  result <- count_fresh_entrants(as.Date("2021-09-20"), eps)
  expect_equal(result, 1L)  # A is back but wasn't there yesterday
})

test_that("count_fresh_entrants does not double-count same person with overlapping episodes", {
  # Same person with two episodes both covering today
  eps <- mk_gender_episodes(
    c("A", "A"),
    c("2019-10-21", "2019-10-21"),
    c("2023-01-01", "2023-06-01")
  )
  result <- count_fresh_entrants(as.Date("2019-10-21"), eps)
  expect_equal(result, 1L)  # unique persons, not episodes
})

# ==================================================================
# Block: count_midterm_attrition()
# ==================================================================

test_that("count_midterm_attrition counts MPs who left between elections", {
  eps <- mk_gender_episodes(
    c("A", "B", "C"),
    c("2019-10-22", "2019-10-22", "2019-10-22"),
    c("2021-03-15", "2023-09-19", "2023-09-19")  # A leaves mid-term, B and C stay
  )
  result <- count_midterm_attrition(as.Date("2019-10-21"), as.Date("2023-09-20"), eps)
  expect_equal(result, 1L)  # only A dropped out
})

test_that("count_midterm_attrition returns 0 when nobody leaves", {
  eps <- mk_gender_episodes(
    c("A", "B"),
    c("2019-10-22", "2019-10-22"),
    c("2023-09-19", "2023-09-19")  # both stay until the day before next election
  )
  result <- count_midterm_attrition(as.Date("2019-10-21"), as.Date("2023-09-20"), eps)
  expect_equal(result, 0L)
})

test_that("count_midterm_attrition returns NA when dates are NA", {
  eps <- mk_gender_episodes("A", "2019-10-22", "2023-01-01")
  expect_true(is.na(count_midterm_attrition(as.Date(NA), as.Date("2023-09-20"), eps)))
  expect_true(is.na(count_midterm_attrition(as.Date("2019-10-21"), as.Date(NA), eps)))
})

test_that("count_midterm_attrition excludes MPs who left on election day boundary", {
  # MP's episode ends on the day before the next election (dissolution)
  # This is NOT attrition — it's a normal end of term
  eps <- mk_gender_episodes(
    c("A", "B"),
    c("2019-10-22", "2019-10-22"),
    c("2023-09-19", "2022-06-15")  # A stays to end, B leaves mid-term
  )
  result <- count_midterm_attrition(as.Date("2019-10-21"), as.Date("2023-09-20"), eps)
  expect_equal(result, 1L)  # only B is attrition; A served the full term
})

test_that("count_midterm_attrition works with data.table input", {
  eps <- mk_gender_episodes(
    c("A", "B", "C"),
    c("2019-10-22", "2019-10-22", "2019-10-22"),
    c("2020-06-01", "2023-09-19", "2023-09-19")
  )
  setDT(eps)
  result <- count_midterm_attrition(as.Date("2019-10-21"), as.Date("2023-09-20"), eps)
  expect_equal(result, 1L)
})

# ==================================================================
# Block: count_midterm_reinforcements()
# ==================================================================

test_that("count_midterm_reinforcements counts MPs who entered mid-term", {
  eps <- mk_gender_episodes(
    c("A", "B", "C"),
    c("2019-10-22", "2021-03-15", "2019-10-22"),  # B enters mid-term
    c("2023-09-19", "2023-09-19", "2023-09-19")
  )
  result <- count_midterm_reinforcements(as.Date("2019-10-21"), as.Date("2023-09-20"), eps)
  expect_equal(result, 1L)  # only B is a mid-term entrant
})

test_that("count_midterm_reinforcements returns 0 when no mid-term entries", {
  eps <- mk_gender_episodes(
    c("A", "B"),
    c("2019-10-22", "2019-10-22"),
    c("2023-09-19", "2023-09-19")
  )
  result <- count_midterm_reinforcements(as.Date("2019-10-21"), as.Date("2023-09-20"), eps)
  expect_equal(result, 0L)
})

test_that("count_midterm_reinforcements returns NA when dates are NA", {
  eps <- mk_gender_episodes("A", "2021-03-15", "2023-09-19")
  expect_true(is.na(count_midterm_reinforcements(as.Date(NA), as.Date("2023-09-20"), eps)))
  expect_true(is.na(count_midterm_reinforcements(as.Date("2019-10-21"), as.Date(NA), eps)))
})

test_that("count_midterm_reinforcements excludes MPs who entered and left mid-term", {
  # D enters mid-term but leaves before the next election — not a reinforcement
  eps <- mk_gender_episodes(
    c("A", "D"),
    c("2019-10-22", "2021-03-15"),
    c("2023-09-19", "2022-06-01")  # D leaves before next election
  )
  result <- count_midterm_reinforcements(as.Date("2019-10-21"), as.Date("2023-09-20"), eps)
  expect_equal(result, 0L)  # D isn't there before the next election
})

test_that("count_midterm_reinforcements works with data.table input", {
  eps <- mk_gender_episodes(
    c("A", "B"),
    c("2019-10-22", "2021-06-01"),
    c("2023-09-19", "2023-09-19")
  )
  setDT(eps)
  result <- count_midterm_reinforcements(as.Date("2019-10-21"), as.Date("2023-09-20"), eps)
  expect_equal(result, 1L)
})
