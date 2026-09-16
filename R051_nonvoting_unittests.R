# Exercise the actual analytical filters and daily calculations without loading
# production data, rendering plots or overwriting a daily-count cache.
library(testthat)
library(data.table)

test_that("US and German BF11 members are excluded from totals and women's shares", {
  script <- as.list(parse("R051.R"))
  membership_filters <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("RESE")) &&
      all(c("political_function", "%in%") %in% all.names(expr[[3]]))
  }, script)
  expect_length(membership_filters, 2L)
  counting_block <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1]], as.name("if")) &&
      identical(expr[[2]], as.name("recalculate_needed"))
  }, script)
  expect_length(counting_block, 1L)
  # These statements calculate all counts and the focal proportion; the
  # following statements write caches and must not run during this test.
  counting_steps <- as.list(counting_block[[1]][[3]])[2:6]
  fixture <- data.frame(
    pers_id = c("US_man", "US_woman", "US_woman", "US_delegate", "US_senator",
                "DE_man", "DE_woman", "DE_berlin"),
    country_abb = c(rep("US", 5), rep("DE", 3)),
    political_function = c(rep("NT_LE-LH_T3_NA_01", 3), "NT_LE-LH_T3_NA_11",
                           "NT_LE-UH_T3_NA_01", rep("NT_LE_T3_NA_01", 2),
                           "NT_LE_T3_NA_11"),
    gender = c("m", "f", "f", "f", "f", "m", "f", "f"),
    stringsAsFactors = FALSE
  )
  for (country in c("US", "DE")) {
    env <- new.env(parent = environment())
    env$country_code <- country
    env$RESE <- fixture
    eval(membership_filters[[1]], env)
    expect_false(any(grepl("_11$", env$RESE$political_function)))
    eval(membership_filters[[2]], env)
    expect_setequal(env$RESE$pers_id, paste0(country, c("_man", "_woman")))
    env$RESEBU <- as.data.table(env$RESE)
    env$RESEBU[, `:=`(is_focal = gender == "f",
      res_entry_start_dateformat = as.Date("2000-01-01"),
      res_entry_end_dateformat = as.Date("2000-01-02"))]
    env$RESEBU_FOCAL <- env$RESEBU[is_focal == TRUE]
    env$RESEBU_COMPLEMENT <- env$RESEBU[is_focal == FALSE]
    env$days_dt <- data.table(thisday = as.Date(c("2000-01-01", "2000-01-02")))
    for (step in counting_steps) eval(step, env)
    expect_equal(env$DAILY_COUNTS$pol_all, c(2L, 2L))
    expect_equal(env$DAILY_COUNTS$pol_focal, c(1L, 1L))
    expect_equal(env$DAILY_COUNTS$proportion_focal, c(0.5, 0.5))
  }
  expect_true(any(grepl("_11$", fixture$political_function)))
})
