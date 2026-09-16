library(testthat)
library(data.table)
source("R051_functions.R")

test_that("the US Senate registry includes Hawaii from its admission day", {
  segs <- parse_parliament_size_series(
    "US_NT-SE_1959", as.Date("1959-01-03"), as.Date("1961-01-02"), "98;100")
  expect_identical(segs$size, c(98L, 100L))
  expect_identical(segs$seg_start, as.Date(c("1959-01-03", "1959-08-21")))
  expect_identical(segs$seg_end, as.Date(c("1959-08-20", "1961-01-02")))
  expect_identical(segs$seg_end[1] + 1L, segs$seg_start[2])
  baseline <- data.table::data.table(
    parliament_id = segs$parliament_id, start_date = segs$seg_start,
    end_date = segs$seg_end, baseline_size = as.numeric(segs$size))
  daily <- data.table::data.table(
    thisday = seq(as.Date("1959-01-03"), as.Date("1961-01-02"), by = "day"),
    pol_all = 98L)
  deviations <- detect_parliament_deviations(
    daily, baseline, seat_threshold = 0, duration_threshold_days = 1)
  low <- deviations[deviation_type == "structurally_too_low"]
  expect_equal(nrow(low), 1L)
  expect_equal(low$start_date, as.Date("1959-08-21"))
  expect_equal(low$end_date, as.Date("1961-01-02"))
  expect_equal(abs(low$max_deviation), 2)
  expect_false(any(deviations$deviation_type == "structurally_too_high"))
})
