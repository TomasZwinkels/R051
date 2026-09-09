# Exercise R051's real membership filters without loading production files,
# rendering plots, or overwriting daily-count caches.
library(testthat)

test_that("both R051 filters retain delegates in the seated House population", {
  script <- parse("R051.R")
  membership_filters <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("RESE")) &&
      all(c("political_function", "%in%") %in% all.names(expr[[3]]))
  }, as.list(script))
  expect_length(membership_filters, 2L)
  env <- new.env(parent = environment())
  env$country_code <- "US"
  env$RESE <- data.frame(
    pers_id = c("US_voting", "US_delegate", "US_senator", "DE_berlin"),
    country_abb = c("US", "US", "US", "DE"),
    political_function = c("NT_LE-LH_T3_NA_01", "NT_LE-LH_T3_NA_11",
                           "NT_LE-UH_T3_NA_01", "NT_LE_T3_NA_11"),
    gender = c("m", "f", "m", "m"),
    stringsAsFactors = FALSE
  )
  eval(membership_filters[[1]], env)
  expect_setequal(env$RESE$pers_id, c("US_voting", "US_delegate", "DE_berlin"))
  eval(membership_filters[[2]], env)
  expect_identical(env$RESE$pers_id, c("US_voting", "US_delegate"))
  expect_equal(mean(env$RESE$gender == "f"), 0.5)
})
