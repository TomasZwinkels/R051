# R052 import files use __[incr] for the central database to allocate IDs.
# Direct analysis needs stable, unique IDs without changing the import file or
# replacing IDs that have already been allocated by the central database.
read_r052_rese_for_analysis <- function(path) {
  rese <- read.csv(path, header = TRUE, skip = 1, stringsAsFactors = FALSE)
  required <- c("res_entry_id", "pers_id", "res_entry_index")
  if (!all(required %in% names(rese))) {
    stop("R052 membership export is missing episode identifier columns.", call. = FALSE)
  }
  placeholders <- !is.na(rese$res_entry_id) &
    endsWith(rese$res_entry_id, "__[incr]")
  if (any(placeholders)) {
    person <- rese$pers_id[placeholders]
    index <- as.character(rese$res_entry_index[placeholders])
    if (anyNA(person) || any(!nzchar(person)) || anyNA(index) ||
        any(!grepl("^[1-9][0-9]*$", index)) ||
        any(rese$res_entry_id[placeholders] != paste0(person, "__[incr]"))) {
      stop("R052 placeholder IDs require their matching person and a positive episode index.",
           call. = FALSE)
    }
    rese$res_entry_id[placeholders] <- paste0(person, "__", index)
  }
  if (anyDuplicated(rese$res_entry_id)) {
    stop("R052 analysis episode IDs are not unique after resolving import placeholders.",
         call. = FALSE)
  }
  rese
}

# Include the population definition in every country's cache version. US data
# comes directly from R052, so its actual input bytes must also invalidate the
# cache even when the central PCC data version has not changed.
r051_daily_cache_version <- function(data_version, source_files = character()) {
  if (length(data_version) != 1L || is.na(data_version) || !nzchar(data_version)) {
    stop("A single nonempty data version is required.", call. = FALSE)
  }
  if (any(!file.exists(source_files))) {
    stop("A daily-count cache input file is missing.", call. = FALSE)
  }
  fingerprints <- tools::md5sum(source_files)
  if (anyNA(fingerprints)) {
    stop("Could not fingerprint every daily-count cache input.", call. = FALSE)
  }
  paste(c("voting-members-v1", data_version, unname(fingerprints)), collapse = "|")
}
