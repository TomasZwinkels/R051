# Simplified version to generate the plot
library(sqldf); library(stringr); library(readr); library(dplyr); library(writexl); library(openxlsx); library(testthat); library(data.table); library(ggplot2)

setwd("/home/tomas/projects/ProjectR051_NewDaybyDay")

# Configuration: Set country code for analysis
USE_SYNTHETIC <- FALSE # Set to TRUE to load synthetic Slowjamistan data for testing
force_recalculate <- FALSE # Set to TRUE to force recalculation of daily counts (ignores cache)
show_mp_lines <- TRUE # Set to TRUE to show Total MPs and Parliament Size Baseline lines
country_code <- "NL"  # Options: "CA" (Canada), "CH" (Switzerland), "DE" (Germany), "NL" (Netherlands), "NO" (Norway), "US" (United States)

# Trait configuration: which binary characteristic to track over time
# The script splits all MPs into a "focal group" and its complement,
# then tracks the focal group's proportion daily.
# --- Option A: Gender ---
trait_column     <- "gender"
focal_value      <- "f"
focal_label      <- "Women"
complement_label <- "Men"
trait_name       <- "Gender"

# --- Option B: College education (requires ICPSR data, covers 1789-1996) ---
# trait_column     <- "has_college"
# focal_value      <- "no"
# focal_label      <- "No college"
# complement_label <- "College-educated"
# trait_name       <- "Education"

country_name <- switch(
  country_code,
  "CA" = "Canada",
  "CH" = "Switzerland",
  "DE" = "Germany",
  "NL" = "Netherlands",
  "NO" = "Norway",
  "US" = "United States (House of Representatives)",
  country_code
)
if (USE_SYNTHETIC) {
  country_code <- "SJ"
  country_name <- "Slowjamistan"
}

# Load data integrity functions 
pathtocheckerfunctions <- "/home/tomas/projects/ProjectR047_PCCIntegrity/"
source(paste0(pathtocheckerfunctions,"R047_RESE_functions.R"))
test_file(paste0(pathtocheckerfunctions,"R047_RESE_unittests.R"))

# Load custom R051 functions
source("R051_functions.R")
test_file("R051_unittests.R")

# Import data
if (USE_SYNTHETIC) {
  synthetic_dir <- "/home/tomas/projects/ProjectR054_PCCSyntheticData/output/case_equal_dropout_full_replacement_female_bias"
  POLI = read.csv(file.path(synthetic_dir, "POLI_slowjamistan.csv"), header = TRUE, sep = ";")
  RESE = read.csv(file.path(synthetic_dir, "RESE_membership_slowjamistan.csv"), header = TRUE, sep = ";")
  PARL = read.csv(file.path(synthetic_dir, "PARL_slowjamistan.csv"), header = TRUE, sep = ";")
  MEME = data.frame()
} else if (country_code == "US") {
  # Load US data directly from R052 BioGuide exports
  r052_dir <- "/home/tomas/projects/ProjectR052_DataFromExternalAPIs/USA/BioGuide/data_ready_for_IMPORT"
  POLI = read.csv(file.path(r052_dir, "POLI_import_ready.csv"), header = TRUE, skip = 1)
  RESE = read.csv(file.path(r052_dir, "RESE_parlmem_import_ready.csv"), header = TRUE, skip = 1)
  PARL = read.csv(file.path(r052_dir, "PARL_import_ready.csv"), header = TRUE, skip = 1)
  MEME = data.frame()

  # Enrich with ICPSR education data (college_v18)
  # ICPSR 7803 covers 1789-1996; post-1996 members will have NA.
  icpsr_poli_file <- "/home/tomas/projects/ProjectR052_DataFromExternalAPIs/USA/ICPSR_Congressional/data_ready_for_IMPORT/POLI_import_ready.csv"
  if (file.exists(icpsr_poli_file)) {
    icpsr_poli <- read.csv(icpsr_poli_file, header = TRUE, skip = 1)
    icpsr_edu <- icpsr_poli[, c("id_us_icpsr", "college_v18")]
    POLI <- merge(POLI, icpsr_edu, by = "id_us_icpsr", all.x = TRUE)
    # Derive binary has_college: yes/no/NA
    POLI$has_college <- ifelse(
      is.na(POLI$college_v18), NA_character_,
      ifelse(POLI$college_v18 == "none", "no", "yes")
    )
  }
} else {
  POLI = read.csv("/home/tomas/projects/PCCdata/POLI.csv", header = TRUE, sep = ";")
  RESE = read.csv("/home/tomas/projects/PCCdata/RESE.csv", header = TRUE, sep = ";")
  PARL = read.csv("/home/tomas/projects/PCCdata/PARL.csv", header = TRUE, sep = ";")
  MEME = read.csv("/home/tomas/projects/PCCdata/MEME.csv", header = TRUE, sep = ";")
}

# Data integrity checks
RESE <- RESE[which(RESE$country_abb == country_code),]
check_RESE_persid_in_POLI(RESE,POLI) # should return TRUE
check_RESE_resentryid_unique(RESE) # should return TRUE

# Focus on parliamentary membership
RESE <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09")),]
nrow(RESE)

check_RESE_parlmemeppisodes_anyfulloverlap(preprocess_RESEdates(RESE)) # should return FALSE
check_RESE_anynear_fulloverlap(preprocess_RESEdates(RESE)) # should return FALSE


# Data integrity checks with preprocessing
if( check_RESE_persid_in_POLI(RESE,POLI) == FALSE||
    check_RESE_resentryid_unique(RESE)==FALSE||
    check_RESE_parlmemeppisodes_anyfulloverlap(preprocess_RESEdates(RESE))==TRUE||
    check_RESE_anynear_fulloverlap(preprocess_RESEdates(RESE))==TRUE
  )
{
stop("One or more RESE integrity checks failed. RESE set to NULL.\n",
     "Run the R047 deepdive script for country '", country_code,
     "' to investigate the data quality issue.\n",
     "  persid_in_POLI: ", check_RESE_persid_in_POLI(RESE,POLI), "\n",
     "  resentryid_unique: ", check_RESE_resentryid_unique(RESE), "\n",
     "  no_full_overlaps: ", !check_RESE_parlmemeppisodes_anyfulloverlap(preprocess_RESEdates(RESE)), "\n",
     "  no_near_overlaps: ", !check_RESE_anynear_fulloverlap(preprocess_RESEdates(RESE)))
}
nrow(RESE)

# Date processing for RESE
RESE$res_entry_start <- gsub("[[rcen]]","",RESE$res_entry_start,fixed=TRUE)
RESE$res_entry_start <- gsub("[[lcen]]","",RESE$res_entry_start,fixed=TRUE)
RESE$res_entry_end <- gsub("[[rcen]]","",RESE$res_entry_end,fixed=TRUE)
RESE$res_entry_end <- gsub("[[lcen]]","",RESE$res_entry_end,fixed=TRUE)

RESE$res_entry_start_dateformat <- as.Date(as.character(RESE$res_entry_start),format=c("%d%b%Y"))
RESE$res_entry_end_dateformat <- as.Date(as.character(RESE$res_entry_end),format=c("%d%b%Y"))

# Date processing for PARL
PARL$leg_period_start <- gsub("[[rcen]]","",PARL$leg_period_start,fixed=TRUE)
PARL$leg_period_start <- gsub("[[lcen]]","",PARL$leg_period_start,fixed=TRUE)
PARL$leg_period_end <- gsub("[[rcen]]","",PARL$leg_period_end,fixed=TRUE)
PARL$leg_period_end <- gsub("[[lcen]]","",PARL$leg_period_end,fixed=TRUE)

PARL$leg_period_start_dateformat <- as.Date(as.character(PARL$leg_period_start),format=c("%d%b%Y"))
PARL$leg_period_end_dateformat <- as.Date(as.character(PARL$leg_period_end),format=c("%d%b%Y"))

# Focus on selected country and national level only (excludes regional Swiss data)
if (country_code == "CA") {
  PARL <- PARL[which(PARL$country_abb == country_code & PARL$level == "NT" & PARL$assembly_abb == "HC"),]
} else if (country_code == "CH") {
  PARL <- PARL[which(PARL$country_abb == country_code & PARL$level == "NT" & PARL$assembly_abb == "NR"),]
} else if (country_code == "DE") {
  PARL <- PARL[which(PARL$country_abb == country_code & PARL$level == "NT" & PARL$assembly_abb == "BT"),]
} else if (country_code == "NL") {
  PARL <- PARL[which(PARL$country_abb == country_code & PARL$level == "NT" & PARL$assembly_abb == "TK"),]
} else if (country_code == "NO") {
  PARL <- PARL[which(PARL$country_abb == country_code & PARL$level == "NT" & PARL$assembly_abb == "ST"),]
} else if (country_code == "US") {
  PARL <- PARL[which(PARL$country_abb == country_code & PARL$level == "NT" & PARL$assembly_abb == "HR"),]
} else if (country_code == "SJ") {
  PARL <- PARL[which(PARL$country_abb == country_code & PARL$level == "NT" & PARL$assembly_abb == "SA"),]
} else {
  stop(paste("Unsupported country_code for PARL filter:", country_code))
}

if (nrow(PARL) == 0) {
  stop(paste("No PARL rows found after filtering for country", country_code, "- check assembly_abb and level filters."))
}

# Filter again for parliamentary episodes in selected country
RESE <- RESE[which(RESE$country_abb == country_code & RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01","NT_LE_T3_NA_09")),]

# Merge with POLI to get trait info
RESEBU <- RESE %>%
  left_join(
    POLI %>% select(pers_id, all_of(trait_column)),
    by = "pers_id"
  )

# For gender trait, clean tf/tm values
if (trait_column == "gender") {
  RESEBU$gender[which(RESEBU$gender == "tf")] <- "f"
  RESEBU$gender[which(RESEBU$gender == "tm")] <- "m"
}

# Create binary focal group flag
RESEBU$is_focal <- RESEBU[[trait_column]] == focal_value

# Focus on relevant variables
RESEBU <- RESEBU %>%
            select(res_entry_id, pers_id, is_focal, res_entry_start_dateformat, res_entry_end_dateformat)
nrow(RESEBU)
head(RESEBU)

# Separate into focal group and complement
RESEBU_FOCAL <- RESEBU[which(RESEBU$is_focal == TRUE), ]
RESEBU_COMPLEMENT <- RESEBU[which(RESEBU$is_focal == FALSE), ]

# Convert to data.table
setDT(RESEBU)
setDT(RESEBU_FOCAL)
setDT(RESEBU_COMPLEMENT)

# Create sequence of all days - start from first parliamentary term we have data on
parl_start_date <- min(PARL$leg_period_start_dateformat, na.rm = TRUE)
all_days <- seq(
  from = parl_start_date,
  to   = max(RESEBU$res_entry_end_dateformat, na.rm = TRUE),
  by   = "day"
)
days_dt <- data.table(thisday = all_days)

print(paste("Created", length(all_days), "days from", min(all_days), "to", max(all_days)))

# Calculate daily counts with caching system
# Check if data has changed by comparing versions (country-specific caching)
if (USE_SYNTHETIC) {
  current_data_version <- trimws(readLines(file.path(synthetic_dir, "dataversion.txt"))[1])
} else {
  current_data_version <- trimws(readLines("/home/tomas/projects/PCCdata/dataversion.txt")[1])
}
trait_key <- paste0(tolower(gsub(" ", "_", trait_name)), "_", tolower(gsub(" ", "_", focal_value)))
country_dir <- file.path(country_code)
if (!dir.exists(country_dir)) dir.create(country_dir)
version_file <- file.path(country_dir, paste0("dataversion_latest_run_", country_code, "_", trait_key, ".txt"))
cache_file <- file.path(country_dir, paste0("daily_counts_cache_", country_code, "_", trait_key, ".RData"))

# Check if we need to recalculate or can load from cache
recalculate_needed <- FALSE  # Reset flag

if (force_recalculate) {
  cat("force_recalculate is TRUE — recalculating daily counts...\n")
  recalculate_needed <- TRUE
} else if (file.exists(version_file) && file.exists(cache_file)) {
  last_run_version <- trimws(readLines(version_file)[1])

  if (current_data_version == last_run_version) {
    cat("Cache hit: data version unchanged (", current_data_version, ") for country =", country_code, ", trait =", trait_name, "\n")
    cat("Loading from:", cache_file, "\n")
    load(cache_file)
    cat("Successfully loaded cached data.\n")
  } else {
    cat("Cache stale: data version changed from", last_run_version, "to", current_data_version, "for country =", country_code, ", trait =", trait_name, "\n")
    cat("Recalculating daily counts - this may take a while...\n")
    recalculate_needed <- TRUE
  }
} else {
  cat("No cache found for country =", country_code, ", trait =", trait_name, "\n")
  cat("Calculating daily counts for the first time - this may take a while...\n")
  recalculate_needed <- TRUE
}

# Only recalculate if needed
if (recalculate_needed) {
  DAILY_COUNTS_ALL <- days_dt[, .(
    pol_all = RESEBU[thisday >= res_entry_start_dateformat & thisday <= res_entry_end_dateformat,
                     uniqueN(pers_id)]
  ), by = thisday]

  DAILY_COUNTS_COMPLEMENT <- days_dt[, .(
    pol_complement = RESEBU_COMPLEMENT[thisday >= res_entry_start_dateformat & thisday <= res_entry_end_dateformat,
                        uniqueN(pers_id)]
  ), by = thisday]

  DAILY_COUNTS_FOCAL <- days_dt[, .(
    pol_focal = RESEBU_FOCAL[thisday >= res_entry_start_dateformat & thisday <= res_entry_end_dateformat,
                          uniqueN(pers_id)]
  ), by = thisday]

  # Merge all by thisday
  DAILY_COUNTS <- Reduce(function(x, y) merge(x, y, by = "thisday", all = TRUE),
                        list(DAILY_COUNTS_ALL, DAILY_COUNTS_COMPLEMENT, DAILY_COUNTS_FOCAL)
  )

  # Calculate proportions
  DAILY_COUNTS[, proportion_focal := pol_focal / pol_all]

  # Save cache and update version file
  save(DAILY_COUNTS_ALL, DAILY_COUNTS_COMPLEMENT, DAILY_COUNTS_FOCAL, DAILY_COUNTS, file = cache_file)
  writeLines(current_data_version, version_file)
  
  cat("Daily counts calculation completed and cached.\n")
}

# Add parliament session start lines
# Get unique parliament start dates for vertical lines
parl_starts <- unique(PARL$leg_period_start_dateformat)

# Check for NA values in parliament start dates
if(any(is.na(parl_starts))) {
  stop("ERROR: NA values found in parliament start dates (PARL$leg_period_start_dateformat). 
       This indicates date parsing issues in the PARL data that need to be fixed before proceeding.")
}

parl_starts <- sort(parl_starts)

# =============================================================================
# Calculate "election-only" fluctuations using data-driven cohort change days
#
# Goal: build the green "Election-Only Trend" line in the graph. This line
# shows what the focal group's representation would look like if only
# election-related changes mattered (ignoring mid-term changes).
#
# Approach:
#   1. For each parliament, find the "cohort change day" — the single day with
#      the most MP entries + departures. This is data-driven (no n_days window).
#   2. Measure % focal group the day before and day after the cohort change day.
#   3. The difference is the "election jump" — the change attributable to that
#      election.
#   4. Accumulate these jumps to build a running total (the green step line).
# =============================================================================

# Convert to data.table for efficient querying
setDT(PARL)
setDT(DAILY_COUNTS)

# Step 1: For each parliament, find the day with the most MP turnover.
# find_new_cohort_day() searches a window around each parliament (midpoint of
# previous term to midpoint of next term) and returns the peak-turnover date.
term_starts <- unique(PARL[, .(parliament_id, term_start = as.Date(leg_period_start_dateformat))])
term_starts[, new_cohort_day := as.Date(sapply(parliament_id, function(pid) {
  find_new_cohort_day(pid, RESE, PARL)
}), origin = "1970-01-01")]

cat("\n=== Cohort change days ===\n")
print(as.data.frame(term_starts))

# Step 2a: Get % focal group on the day BEFORE each cohort change day.
# This represents the composition of the outgoing parliament, just before
# the election turnover happens.
BEFORE <- term_starts[!is.na(new_cohort_day), .(
  parliament_id, term_start, new_cohort_day,
  target_day = new_cohort_day - 1
)]
BEFORE <- merge(BEFORE, DAILY_COUNTS[, .(thisday, pol_all, pol_focal, proportion_focal)],
                by.x = "target_day", by.y = "thisday", all.x = TRUE)
BEFORE[, pct_before := round(proportion_focal * 100, 3)]

# Step 2b: Get % focal group on the day AFTER each cohort change day.
# This represents the composition of the incoming parliament, just after
# the election turnover has settled.
AFTER <- term_starts[!is.na(new_cohort_day), .(
  parliament_id, new_cohort_day,
  target_day = new_cohort_day + 1
)]
AFTER <- merge(AFTER, DAILY_COUNTS[, .(thisday, pol_all, pol_focal, proportion_focal)],
               by.x = "target_day", by.y = "thisday", all.x = TRUE)
AFTER[, pct_after := round(proportion_focal * 100, 3)]

# Step 3: Calculate the "election jump" for each parliament.
# election_jumps = pct_after - pct_before: how much did this election change
# the focal group proportion? Positive = more after, negative = fewer.
DELTA <- merge(
  BEFORE[, .(parliament_id, term_start, new_cohort_day, pct_before)],
  AFTER[, .(parliament_id, pct_after)],
  by = "parliament_id"
)[order(term_start)][
  , election_jumps := round(pct_after - pct_before, 3)][]

# Safety check: each parliament should appear exactly once
if(!nrow(DELTA) == length(unique(DELTA$parliament_id))) {
  stop("ERROR: Non-unique parliament_ids in DELTA - data integrity issue")
}

# Step 4: Build the running total (the green step line).
# Start from the first parliament's "after" percentage, then add each
# subsequent election jump. This shows the cumulative effect of elections
# on focal group representation, stripping out all mid-term noise.
# Use the first non-NA pct_after as starting point (the first parliament's
# cohort day may fall before DAILY_COUNTS begins, giving NA)
startpercentage <- DELTA$pct_after[which(!is.na(DELTA$pct_after))[1]]

# Replace NA election_jumps with 0 so cumsum works (NA would propagate)
election_jumps_clean <- DELTA$election_jumps
election_jumps_clean[is.na(election_jumps_clean)] <- 0

DELTA$running_average_election_only <- startpercentage + cumsum(election_jumps_clean)

# Election-to-election trend: how are election outcomes themselves trending?
# Uses pct_after(this election) - pct_after(previous election), ignoring
# all mid-term dynamics. Shows whether successive elections are getting
# better or worse for the focal group.
election_to_election_jumps <- c(NA, diff(DELTA$pct_after))
election_to_election_jumps[is.na(election_to_election_jumps)] <- 0
DELTA$running_election_to_election <- startpercentage + cumsum(election_to_election_jumps)

tail(DELTA)

# Create year labels for parliament starts
parl_years <- data.frame(
  date = parl_starts,
  year = format(parl_starts, "%Y")
)

# Create parliament size baseline for integrity checking
# Convert parliament_size to numeric and create step function data
setDT(PARL)
parl_baseline <- PARL[, .(
  parliament_id,
  start_date = leg_period_start_dateformat,
  end_date = leg_period_end_dateformat,
  baseline_size = as.numeric(parliament_size)
)][order(start_date)]

# Get deviation periods for highlighting
deviation_periods <- detect_parliament_deviations(DAILY_COUNTS, parl_baseline, seat_threshold = 5, duration_threshold_days = 90, merge_gap_days = 7)

# Create segments data for red highlighting during deviation periods
if (nrow(deviation_periods) > 0) {
  deviation_segments <- data.table()
  for (i in seq_len(nrow(deviation_periods))) {
    period_data <- DAILY_COUNTS[thisday >= deviation_periods$start_date[i] & 
                               thisday <= deviation_periods$end_date[i]]
    if (nrow(period_data) > 0) {
      segment_data <- data.table(
        thisday = period_data$thisday,
        pol_all_normalized = period_data$pol_all / max(DAILY_COUNTS$pol_all, na.rm = TRUE),
        deviation_type = deviation_periods$deviation_type[i],
        period_id = i  # Add period ID to keep segments separate
      )
      deviation_segments <- rbind(deviation_segments, segment_data)
    }
  }
  
  # Create warning labels positioned relative to the actual data values
  warning_labels <- data.table()
  for (i in seq_len(nrow(deviation_periods))) {
    period_data <- DAILY_COUNTS[thisday >= deviation_periods$start_date[i] & 
                               thisday <= deviation_periods$end_date[i]]
    if (nrow(period_data) > 0) {
      mean_mp_count <- mean(period_data$pol_all, na.rm = TRUE)
      # Normalize to plot scale (0-1) with -10 offset to position just below the data
      y_position <- (mean_mp_count - 10) / max(DAILY_COUNTS$pol_all, na.rm = TRUE)
      
      label_row <- data.table(
        x = deviation_periods$start_date[i] + (deviation_periods$end_date[i] - deviation_periods$start_date[i]) / 2,
        y = y_position,
        label = paste0("WARNING!\nStructurally ", 
                       ifelse(deviation_periods$deviation_type[i] == "structurally_too_high", "too high", "too low"))
      )
      warning_labels <- rbind(warning_labels, label_row)
    }
  }
} else {
  deviation_segments <- data.table()
  warning_labels <- data.table()
}

# Per-parliament transition analysis
parl_transitions <- as.data.frame(term_starts[, list(parliament_id, new_cohort_day)])

# Add MP counts on the cohort change day from DAILY_COUNTS
parl_transitions <- merge(parl_transitions,
  as.data.frame(DAILY_COUNTS[, list(thisday, pol_all, pol_focal, pol_complement)]),
  by.x = "new_cohort_day", by.y = "thisday", all.x = TRUE)
names(parl_transitions)[names(parl_transitions) == "pol_all"] <- "seated_total"
names(parl_transitions)[names(parl_transitions) == "pol_focal"] <- "seated_focal"
names(parl_transitions)[names(parl_transitions) == "pol_complement"] <- "seated_complement"

# Count fresh entrants on the cohort day (functions from R051_functions.R)
parl_transitions$entered_at_election_focal <- sapply(parl_transitions$new_cohort_day,
  count_fresh_entrants, group_episodes = RESEBU_FOCAL)

parl_transitions$entered_at_election_complement <- sapply(parl_transitions$new_cohort_day,
  count_fresh_entrants, group_episodes = RESEBU_COMPLEMENT)

parl_transitions$entered_at_election_total = parl_transitions$entered_at_election_focal + parl_transitions$entered_at_election_complement

# Count mid-term attrition: seated after this election but gone before the next
next_cohort_days <- c(parl_transitions$new_cohort_day[-1], NA)

parl_transitions$attrition_focal <- mapply(count_midterm_attrition,
  parl_transitions$new_cohort_day, next_cohort_days,
  MoreArgs = list(group_episodes = RESEBU_FOCAL))

parl_transitions$attrition_complement <- mapply(count_midterm_attrition,
  parl_transitions$new_cohort_day, next_cohort_days,
  MoreArgs = list(group_episodes = RESEBU_COMPLEMENT))

parl_transitions$attrition_total = parl_transitions$attrition_focal + parl_transitions$attrition_complement

# Group-specific attrition rate: what % of focal/complement group seated at the start left mid-term
# e.g. 98 focal seated, 1 left → 1.0%; NA when no one of that group was seated
parl_transitions$attrition_pct_focal <- ifelse(
  is.na(parl_transitions$attrition_focal) | parl_transitions$seated_focal == 0,
  NA_real_,
  round(100 * parl_transitions$attrition_focal / parl_transitions$seated_focal, 1))

parl_transitions$attrition_pct_complement <- ifelse(
  is.na(parl_transitions$attrition_complement) | parl_transitions$seated_complement == 0,
  NA_real_,
  round(100 * parl_transitions$attrition_complement / parl_transitions$seated_complement, 1))

# Count mid-term reinforcements: MPs who were NOT seated after this election
# but ARE seated before the next election (e.g. by-election winners, etc.)
parl_transitions$reinforcements_focal <- mapply(count_midterm_reinforcements,
  parl_transitions$new_cohort_day, next_cohort_days,
  MoreArgs = list(group_episodes = RESEBU_FOCAL))

parl_transitions$reinforcements_complement <- mapply(count_midterm_reinforcements,
  parl_transitions$new_cohort_day, next_cohort_days,
  MoreArgs = list(group_episodes = RESEBU_COMPLEMENT))

parl_transitions$reinforcements_total = parl_transitions$reinforcements_focal + parl_transitions$reinforcements_complement

# Check: does every mid-term departure get replaced?
# attrition - reinforcements should equal the drop in parliament size between
# cohort_day+1 and next_cohort_day-1 (unfilled vacancies near end of term)
parl_transitions$unfilled_vacancies <- ifelse(
  is.na(parl_transitions$attrition_total) | is.na(parl_transitions$reinforcements_total),
  NA_integer_,
  parl_transitions$attrition_total - parl_transitions$reinforcements_total)

# Reinforcement bias: are by-elections disproportionately replacing with focal group?
# Compares the focal group share of reinforcements to the focal group share of seated MPs.
# Positive = by-elections favored focal group beyond their existing representation.
# Zero = neutral (replacements mirror existing composition).
# Negative = by-elections disfavored focal group.
parl_transitions$reinforcement_bias_focal <- ifelse(
  is.na(parl_transitions$reinforcements_total) | parl_transitions$reinforcements_total == 0 |
  is.na(parl_transitions$seated_total) | parl_transitions$seated_total == 0,
  NA_real_,
  round(
    100 * parl_transitions$reinforcements_focal / parl_transitions$reinforcements_total -
    100 * parl_transitions$seated_focal / parl_transitions$seated_total,
  1))

print(parl_transitions)

# Summary statistics for graph annotation
avg_attrition_focal <- round(mean(parl_transitions$attrition_pct_focal, na.rm = TRUE), 1)
sd_attrition_focal <- round(sd(parl_transitions$attrition_pct_focal, na.rm = TRUE), 1)
avg_attrition_complement <- round(mean(parl_transitions$attrition_pct_complement, na.rm = TRUE), 1)
sd_attrition_complement <- round(sd(parl_transitions$attrition_pct_complement, na.rm = TRUE), 1)
avg_reinforcement_bias_focal <- round(mean(parl_transitions$reinforcement_bias_focal, na.rm = TRUE), 1)
sd_reinforcement_bias_focal <- round(sd(parl_transitions$reinforcement_bias_focal, na.rm = TRUE), 1)

summary_text <- paste0(
  "Avg. mid-term attrition rate:\n",
  "  ", focal_label, ": ", avg_attrition_focal, "% (SD=", sd_attrition_focal, ")",
  "   ", complement_label, ": ", avg_attrition_complement, "% (SD=", sd_attrition_complement, ")\n",
  "Avg. reinforcement bias (", tolower(focal_label), "): ",
  avg_reinforcement_bias_focal, "% (SD=", sd_reinforcement_bias_focal, ")\n",
  "  (% ", tolower(focal_label), " among replacements minus\n",
  "   % ", tolower(focal_label), " seated at start of term;\n",
  "   positive = mid-term replacements favor ", tolower(focal_label), ")"
)
cat("\n", summary_text, "\n")

mean(parl_transitions$reinforcement_bias_focal,na.rm=TRUE)

# X-axis date limits (set to NULL for full range, or a Date vector to zoom)
x_date_limits <- as.Date(c("1945-01-01", "2027-12-31"))
x_start <- if (!is.null(x_date_limits)) x_date_limits[1] else min(DAILY_COUNTS$thisday)

# Create a triple-line plot
p_simple <- ggplot(DAILY_COUNTS, aes(x = thisday)) +
  geom_vline(xintercept = parl_starts, color = "gray70", alpha = 0.6, linewidth = 0.3) +
  geom_text(data = parl_years, 
            aes(x = date, y = 0.05, label = year), 
            angle = 90, 
            size = 3.5, 
            color = "gray50", 
            hjust = 0, 
            vjust = 0.5) +
  {if (show_mp_lines) geom_line(aes(y = pol_all / max(pol_all, na.rm = TRUE), color = "Total MPs"), linewidth = 0.8) else NULL} +
  {if (show_mp_lines) geom_step(data = parl_baseline,
            aes(x = start_date, y = baseline_size / max(DAILY_COUNTS$pol_all, na.rm = TRUE),
                color = "Parliament Size Baseline"),
            linewidth = 1.0) else NULL} +
  geom_line(aes(y = proportion_focal, color = paste0("Daily ", focal_label, " %")), linewidth = 0.8) +
  geom_step(data = DELTA,
            aes(x = new_cohort_day, y = running_average_election_only / 100,
                color = "Election-Only Trend"),
            linewidth = 1.0) +
  # Add red highlighting for deviation periods (thicker and more visible)
  {if (show_mp_lines && nrow(deviation_segments) > 0)
    geom_line(data = deviation_segments,
              aes(x = thisday, y = pol_all_normalized, group = period_id),
              color = "red", linewidth = 2.5, alpha = 0.8)
   else NULL} +
  # Add warning labels with background boxes
  {if (show_mp_lines && nrow(warning_labels) > 0)
    geom_label(data = warning_labels,
               aes(x = x, y = y, label = label),
               color = "red", size = 2.5, fontface = "bold",
               fill = "white", alpha = 0.8,
               hjust = 0.5, vjust = 0.5)
   else NULL} +
  # Add summary statistics in top-left corner
  annotate("label", x = x_start, y = 0.95,
           label = summary_text, hjust = 0, vjust = 1,
           size = 3.5, family = "mono",
           fill = "white", alpha = 0.85,
           label.size = 0.3) +
  scale_y_continuous(
    name = "Total Number of MPs",
    breaks = scales::pretty_breaks(n = 6),
    labels = function(x) round(x * max(DAILY_COUNTS$pol_all, na.rm = TRUE)),
    sec.axis = sec_axis(
      ~ ., 
      name = paste0("Proportion of ", focal_label),
      breaks = seq(0, 1, 0.2),
      labels = scales::percent_format()
    )
  ) +
  scale_x_date(
    name = "Time",
    limits = x_date_limits
  ) +
  scale_color_manual(
    values = {
      cols <- c("red", "green")
      nms <- c(paste0("Daily ", focal_label, " %"), "Election-Only Trend")
      if (show_mp_lines) { cols <- c(cols, "blue", "black"); nms <- c(nms, "Total MPs", "Parliament Size Baseline") }
      setNames(cols, nms)
    },
    name = "Measures"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.margin = margin(10, 30, 10, 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.caption = element_text(size = 8, family = "mono", hjust = 0)
  ) +
  ggtitle(paste0(focal_label, " Representation and Parliament Size in ", country_name, " Over Time")) +
  labs(caption = {
    caption_text <- paste0("Generated on: ",
      format(Sys.time(), "%Y-%m-%d at %H:%M:%S"))
    if (USE_SYNTHETIC) {
      config_file <- file.path(synthetic_dir, "config_summary.txt")
      if (file.exists(config_file)) {
        cfg_lines <- readLines(config_file)
        caption_text <- paste0(
          paste(cfg_lines, collapse = "\n"), "\n", caption_text)
      } else {
        caption_text <- paste0("Synthetic case: ",
          basename(synthetic_dir), "\n", caption_text)
      }
    }
    caption_text
  })

# Plot created successfully!

# Save the plot
plot_filename <- file.path(country_dir, paste0("representation_", tolower(gsub(" ", "_", trait_name)), "_", country_code, ".png"))
ggsave(plot_filename, plot = p_simple, width = 16, height = 8, dpi = 150, bg = "white")
cat("Plot saved as", plot_filename, "with double width!\n")

# Create final deviation dataframe for export/analysis
final_deviations <- detect_parliament_deviations(DAILY_COUNTS, parl_baseline, seat_threshold = 5, duration_threshold_days = 90, merge_gap_days = 7)

p_simple

# =============================================================================
# Plot: Mid-term attrition rate per parliamentary cohort
# =============================================================================

attrition_long <- parl_transitions |>
  select(new_cohort_day, attrition_pct_focal, attrition_pct_complement) |>
  tidyr::pivot_longer(
    cols = c(attrition_pct_focal, attrition_pct_complement),
    names_to = "group", values_to = "attrition_pct"
  ) |>
  mutate(group = ifelse(group == "attrition_pct_focal", focal_label, complement_label))

attrition_means <- attrition_long |>
  group_by(group) |>
  summarise(mean_attrition = mean(attrition_pct, na.rm = TRUE), .groups = "drop")

p_attrition <- ggplot(attrition_long, aes(x = new_cohort_day, y = attrition_pct, color = group)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.5) +
  geom_hline(data = attrition_means, aes(yintercept = mean_attrition, color = group),
             linetype = "dashed", linewidth = 0.5) +
  geom_label(data = attrition_means,
             aes(x = x_date_limits[2], y = mean_attrition, color = group,
                 label = paste0("mean: ", round(mean_attrition, 1), "%")),
             hjust = 1, size = 3.5, fill = "white", label.size = 0.2, show.legend = FALSE) +
  scale_color_manual(values = setNames(c("red", "blue"), c(focal_label, complement_label))) +
  scale_x_date(name = "Cohort start", limits = x_date_limits) +
  scale_y_continuous(name = "Mid-term attrition (%)", limits = c(0, NA)) +
  labs(
    title = paste0("Mid-Term Attrition Rate by Parliamentary Cohort in ", country_name),
    subtitle = "% of each group seated after the election who left before the next election",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "top")

attrition_filename <- file.path(country_dir, paste0("attrition_", tolower(gsub(" ", "_", trait_name)), "_", country_code, ".png"))
ggsave(attrition_filename, p_attrition, width = 14, height = 7, dpi = 150, bg = "white")
cat("Plot saved as", attrition_filename, "\n")

p_attrition

# =============================================================================
# Plot: Reinforcement bias per parliamentary cohort
# =============================================================================

reinf_data <- parl_transitions |>
  select(new_cohort_day, reinforcement_bias_focal) |>
  filter(!is.na(reinforcement_bias_focal))

reinf_mean <- mean(reinf_data$reinforcement_bias_focal, na.rm = TRUE)

p_reinforcement <- ggplot(reinf_data, aes(x = new_cohort_day, y = reinforcement_bias_focal)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_line(color = "purple", linewidth = 0.6) +
  geom_point(color = "purple", size = 1.5) +
  geom_hline(yintercept = reinf_mean, linetype = "dashed", color = "purple", linewidth = 0.5) +
  geom_label(aes(x = x_date_limits[2], y = reinf_mean,
                 label = paste0("mean: ", round(reinf_mean, 1), "%")),
             hjust = 1, size = 3.5, color = "purple", fill = "white", label.size = 0.2) +
  scale_x_date(name = "Cohort start", limits = x_date_limits) +
  scale_y_continuous(name = paste0("Reinforcement bias (", tolower(focal_label), ", pp)")) +
  labs(
    title = paste0("Mid-Term Reinforcement Bias (", focal_label, ") by Parliamentary Cohort in ", country_name),
    subtitle = paste0("% ", tolower(focal_label), " among mid-term replacements minus % ", tolower(focal_label), " seated at start of term; positive = replacements favor ", tolower(focal_label))
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

reinf_filename <- file.path(country_dir, paste0("reinforcement_bias_", tolower(gsub(" ", "_", trait_name)), "_", country_code, ".png"))
ggsave(reinf_filename, p_reinforcement, width = 14, height = 7, dpi = 150, bg = "white")
cat("Plot saved as", reinf_filename, "\n")

p_reinforcement
