library(testthat)
source("R051_input_functions.R")

write_import_fixture <- function(data, path) {
  writeLines("sep=,", path)
  suppressWarnings(write.table(data, path, sep = ",", row.names = FALSE,
                               col.names = TRUE, append = TRUE, na = "NA"))
}

find_script_calls <- function(expr, predicate) {
  if (!is.call(expr) && !is.expression(expr) && !is.pairlist(expr)) return(list())
  own <- if (is.call(expr) && predicate(expr)) list(expr) else list()
  c(own, unlist(lapply(as.list(expr), find_script_calls, predicate), recursive = FALSE))
}

test_that("the actual US loader resolves only import placeholders and preserves explicit IDs", {
  directory <- tempfile("r051-import-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "RESE_parlmem_import_ready.csv")
  original <- data.frame(
    res_entry_id = c("US_One__[incr]", "US_One__[incr]", "central_existing__42"),
    pers_id = c("US_One", "US_One", "US_Two"),
    res_entry_index = c(1L, 2L, 3L),
    political_function = c("NT_LE-LH_T3_NA_01", "NT_LE-LH_T3_NA_01", "NT_LE-LH_T3_NA_11")
  )
  write_import_fixture(original, path)
  before <- tools::md5sum(path)
  loader_calls <- find_script_calls(parse("R051.R"), function(expr) {
    as.character(expr[[1]])[1] %in% c("=", "<-") &&
      length(expr) == 3L && identical(expr[[2]], as.name("RESE")) &&
      "read_r052_rese_for_analysis" %in% all.names(expr[[3]])
  })
  expect_length(loader_calls, 1L)
  env <- new.env(parent = environment())
  env$r052_dir <- directory
  eval(loader_calls[[1]], env)
  expect_identical(env$RESE$res_entry_id, c("US_One__1", "US_One__2", "central_existing__42"))
  expect_equal(env$RESE[-1], original[-1])
  expect_identical(tools::md5sum(path), before)
  expect_false(anyDuplicated(env$RESE$res_entry_id) > 0L)
  write_import_fixture(env$RESE, path)
  expect_equal(read_r052_rese_for_analysis(path), env$RESE)
})

test_that("invalid or colliding placeholder indices fail instead of corrupting episode IDs", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  fixture <- data.frame(res_entry_id = "US_One__[incr]", pers_id = "US_One",
                        res_entry_index = 1L)
  for (bad_index in list(NA, 0, -1, 1.5)) {
    fixture$res_entry_index <- bad_index
    write_import_fixture(fixture, path)
    expect_error(read_r052_rese_for_analysis(path), "positive episode index")
  }
  fixture$res_entry_index <- 1L
  fixture$pers_id <- "US_Other"
  write_import_fixture(fixture, path)
  expect_error(read_r052_rese_for_analysis(path), "matching person")
  fixture$pers_id <- "US_One"
  fixture <- rbind(fixture, transform(fixture, res_entry_id = "US_One__1"))
  write_import_fixture(fixture, path)
  expect_error(read_r052_rese_for_analysis(path), "not unique")
})

test_that("the US cache branch versions source files independently of central PCC data", {
  directory <- tempfile("r051-cache-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  files <- file.path(directory, c("POLI_import_ready.csv", "RESE_parlmem_import_ready.csv",
                                  "PARL_import_ready.csv"))
  for (path in files) writeLines("original", path)
  script <- as.list(parse("R051.R"))
  version_block <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1]], as.name("if")) &&
      identical(expr[[2]], as.name("USE_SYNTHETIC")) &&
      "current_data_version" %in% all.names(expr)
  }, script)
  expect_length(version_block, 1L)
  env <- new.env(parent = environment())
  env$USE_SYNTHETIC <- FALSE
  env$country_code <- "US"
  env$r052_dir <- directory
  env$icpsr_poli_file <- file.path(directory, "optional_icpsr.csv")
  eval(version_block[[1]], env)
  original <- env$current_data_version
  expect_match(original, "^voting-members-v1\\|R052-BioGuide\\|")
  eval(version_block[[1]], env)
  expect_identical(env$current_data_version, original)
  for (path in files) {
    writeLines("changed", path)
    eval(version_block[[1]], env)
    expect_false(identical(env$current_data_version, original))
    writeLines("original", path)
  }
  writeLines("education data", env$icpsr_poli_file)
  eval(version_block[[1]], env)
  expect_false(identical(env$current_data_version, original))
  old_education <- env$current_data_version
  writeLines("changed education data", env$icpsr_poli_file)
  eval(version_block[[1]], env)
  expect_false(identical(env$current_data_version, old_education))
  expect_false(identical(r051_daily_cache_version("unchanged-PCC-version"), "unchanged-PCC-version"))
  expect_false(identical(r051_daily_cache_version("version1"), r051_daily_cache_version("version2")))
  expect_error(r051_daily_cache_version("version1", file.path(directory, "missing")), "missing")
})
