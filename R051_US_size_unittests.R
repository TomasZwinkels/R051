library(testthat)
library(data.table)
source("R051_functions.R")

# Explicit-registry fixture: verifies date handling independently of the
# production registry. Reviewed production dates are covered separately.
test_that("US House size changes at midnight without overlapping segments", {
  registry <- list("US_NT-HR_1945" = as.Date("1946-07-04"))
  segs <- parse_parliament_size_series(
    "US_NT-HR_1945", as.Date("1945-01-03"), as.Date("1947-01-02"),
    "439;438", registry = registry)
  expect_equal(segs$size, c(439L, 438L))
  expect_equal(segs$seg_start, as.Date(c("1945-01-03", "1946-07-04")))
  expect_equal(segs$seg_end, as.Date(c("1946-07-03", "1947-01-02")))
  days <- as.Date(c("1946-07-03", "1946-07-04", "1946-07-05"))
  sizes <- vapply(days, function(day) {
    selected <- segs[day >= segs$seg_start & day <= segs$seg_end, ]
    expect_equal(nrow(selected), 1L)
    selected$size
  }, integer(1))
  expect_equal(sizes, c(439L, 438L, 438L))
})

test_that("US size expansion stops when transition dates are unknown or empty", {
  for (registry in list(list(), list("US_NT-HR_1945" = as.Date(character())),
                       list("US_NT-HR_1945" = as.Date(NA)))) {
    expect_error(parse_parliament_size_series(
      "US_NT-HR_1945", as.Date("1945-01-03"), as.Date("1947-01-02"),
      "439;438", registry = registry), "changes mid-term")
  }
})

test_that("a US size decrease changes the deviation baseline on the exact day", {
  registry <- list("US_NT-HR_1945" = as.Date("1946-07-04"))
  segs <- parse_parliament_size_series(
    "US_NT-HR_1945", as.Date("1945-01-03"), as.Date("1947-01-02"),
    "439;438", registry = registry)
  baseline <- data.table::data.table(
    parliament_id = segs$parliament_id, start_date = segs$seg_start,
    end_date = segs$seg_end, baseline_size = as.numeric(segs$size))
  daily <- data.table::data.table(
    thisday = seq(as.Date("1945-01-03"), as.Date("1947-01-02"), by = "day"),
    pol_all = 439L)
  deviations <- detect_parliament_deviations(
    daily, baseline, seat_threshold = 0, duration_threshold_days = 1)
  high <- deviations[deviation_type == "structurally_too_high"]
  expect_equal(nrow(high), 1L)
  expect_equal(high$start_date, as.Date("1946-07-04"))
  expect_equal(high$end_date, as.Date("1947-01-02"))
  expect_equal(high$max_deviation, 1)
  expect_false(any(deviations$deviation_type == "structurally_too_low"))
})

# Reviewed production boundaries: only voting-seat changes affect the baseline.
test_that("non-voting House changes leave the voting-seat baseline constant", {
  years <- c(1793, 1871, 1899, 1905, 1935, 1945, 1971)
  sizes <- c(105L, 243L, 357L, 386L, 435L, 435L, 435L)
  for (i in seq_along(years)) {
    id <- paste0("US_NT-HR_", years[i])
    start <- as.Date(paste0(years[i], "-01-03"))
    end <- as.Date(paste0(years[i] + 2L, "-01-02"))
    segs <- parse_parliament_size_series(id, start, end, as.character(sizes[i]))
    expect_equal(segs$size, sizes[i])
    expect_equal(nrow(segs), 1L)
    expect_null(SIZE_CHANGE_DATES[[id]])
  }
})

test_that("Oklahoma statehood adds five voting House seats on the exact day", {
  segs <- parse_parliament_size_series(
    "US_NT-HR_1907", as.Date("1907-03-04"), as.Date("1909-03-03"), "386;391")
  expect_equal(segs$size, c(386L, 391L))
  expect_equal(segs$seg_start, as.Date(c("1907-03-04", "1907-11-16")))
  expect_equal(segs$seg_end, as.Date(c("1907-11-15", "1909-03-03")))
})

test_that("Tennessee statehood adds a voting seat even when total seats stay constant", {
  segs <- parse_parliament_size_series(
    "US_NT-HR_1795", as.Date("1795-03-04"), as.Date("1797-03-03"), "105;106")
  expect_equal(segs$size, c(105L, 106L))
  expect_equal(segs$seg_start, as.Date(c("1795-03-04", "1796-06-01")))
  expect_equal(segs$seg_end, as.Date(c("1796-05-31", "1797-03-03")))
})

test_that("German non-voting changes are excluded but Saarland adds voting seats", {
  segs <- parse_parliament_size_series(
    "DE_NT-BT_1949", as.Date("1949-09-07"), as.Date("1953-10-05"), "402")
  expect_equal(segs$size, 402L)
  expect_null(SIZE_CHANGE_DATES[["DE_NT-BT_1949"]])
  segs <- parse_parliament_size_series(
    "DE_NT-BT_1953", as.Date("1953-10-06"), as.Date("1957-10-14"), "487;497")
  expect_equal(segs$size, c(487L, 497L))
  expect_equal(segs$seg_end[1], as.Date("1957-01-03"))
  expect_equal(segs$seg_start[2], as.Date("1957-01-04"))
})
