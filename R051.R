# Simplified version to generate the plot
library(sqldf); library(stringr); library(readr); library(dplyr); library(writexl); library(openxlsx); library(testthat); library(data.table); library(ggplot2)

setwd("/home/tomas/projects/ProjectR051_NewDaybyDay")

# Configuration: Set country code for analysis
USE_SYNTHETIC <- TRUE # Set to TRUE to load synthetic Slowjamistan data for testing
country_code <- "CA"  # Options: "CA" (Canada), "CH" (Switzerland), "DE" (Germany), "NL" (Netherlands), "NO" (Norway)
country_name <- switch(
  country_code,
  "CA" = "Canada",
  "CH" = "Switzerland",
  "DE" = "Germany",
  "NL" = "Netherlands",
  "NO" = "Norway",
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
  synthetic_dir <- "/home/tomas/projects/ProjectR054_PCCSyntheticData/output/case_equal_dropout_partial_replacement"
  POLI = read.csv(file.path(synthetic_dir, "POLI_slowjamistan.csv"), header = TRUE, sep = ";")
  RESE = read.csv(file.path(synthetic_dir, "RESE_membership_slowjamistan.csv"), header = TRUE, sep = ";")
  PARL = read.csv(file.path(synthetic_dir, "PARL_slowjamistan.csv"), header = TRUE, sep = ";")
  MEME = data.frame()
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
RESE <- NULL
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

# Merge with POLI to get gender info
RESEBU <- RESE %>%
  left_join(
    POLI %>% select(pers_id, gender, birth_date),
    by = "pers_id"
  )

# Clean gender values
RESEBU$gender[which(RESEBU$gender == "tf")] <- "f"
RESEBU$gender[which(RESEBU$gender == "tm")] <- "m"

# Focus on relevant variables
RESEBU <- RESEBU %>% 
            select(res_entry_id, pers_id, gender, res_entry_start_dateformat, res_entry_end_dateformat)
nrow(RESEBU)
head(RESEBU)

# Separate by gender
RESEBU_MALE <- RESEBU[which(RESEBU$gender == "m"),]
RESEBU_FEMALE <- RESEBU[which(RESEBU$gender == "f"),]
RESEBU_NB <- RESEBU[which(RESEBU$gender == "nb"),]

# Convert to data.table
setDT(RESEBU)
setDT(RESEBU_MALE)
setDT(RESEBU_FEMALE)
setDT(RESEBU_NB)

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
version_file <- paste0("dataversion_latest_run_", country_code, ".txt")
cache_file <- paste0("daily_counts_cache_", country_code, ".RData")

# Check if we need to recalculate or can load from cache
recalculate_needed <- FALSE  # Reset flag

if (file.exists(version_file) && file.exists(cache_file)) {
  last_run_version <- trimws(readLines(version_file)[1])
  
  if (current_data_version == last_run_version) {
    cat("Data version unchanged (", current_data_version, ") for country", country_code, ", loading cached daily counts...\n")
    load(cache_file)
    cat("Successfully loaded cached data.\n")
  } else {
    cat("Data version changed from", last_run_version, "to", current_data_version, "for country", country_code, "\n")
    cat("Recalculating daily counts - this may take a while...\n")
    recalculate_needed <- TRUE
  }
} else {
  cat("No cache found for country", country_code, "or has not run for this country on this data version before.\n")
  cat("Calculating daily counts for the first time - this may take a while...\n")
  recalculate_needed <- TRUE
}

# Only recalculate if needed
if (recalculate_needed) {
  DAILY_COUNTS_ALL <- days_dt[, .(
    pol_all = RESEBU[thisday >= res_entry_start_dateformat & thisday <= res_entry_end_dateformat,
                     uniqueN(pers_id)]
  ), by = thisday]

  DAILY_COUNTS_MALE <- days_dt[, .(
    pol_m = RESEBU_MALE[thisday >= res_entry_start_dateformat & thisday <= res_entry_end_dateformat,
                        uniqueN(pers_id)]
  ), by = thisday]

  DAILY_COUNTS_FEMALE <- days_dt[, .(
    pol_f = RESEBU_FEMALE[thisday >= res_entry_start_dateformat & thisday <= res_entry_end_dateformat,
                          uniqueN(pers_id)]
  ), by = thisday]

  # Merge all by thisday
  DAILY_COUNTS <- Reduce(function(x, y) merge(x, y, by = "thisday", all = TRUE),
                        list(DAILY_COUNTS_ALL, DAILY_COUNTS_MALE, DAILY_COUNTS_FEMALE)
  )

  # Calculate proportions
  DAILY_COUNTS[, proportion_female := pol_f / pol_all]
  
  # Save cache and update version file
  save(DAILY_COUNTS_ALL, DAILY_COUNTS_MALE, DAILY_COUNTS_FEMALE, DAILY_COUNTS, file = cache_file)
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
# shows what women's representation would look like if only election-related
# changes mattered (ignoring mid-term resignations, deaths, by-elections).
#
# Approach:
#   1. For each parliament, find the "cohort change day" — the single day with
#      the most MP entries + departures. This is data-driven (no n_days window).
#   2. Measure % women the day before and day after the cohort change day.
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

# Step 2a: Get % women on the day BEFORE each cohort change day.
# This represents the composition of the outgoing parliament, just before
# the election turnover happens.
BEFORE <- term_starts[!is.na(new_cohort_day), .(
  parliament_id, term_start, new_cohort_day,
  target_day = new_cohort_day - 1
)]
BEFORE <- merge(BEFORE, DAILY_COUNTS[, .(thisday, pol_all, pol_f, proportion_female)],
                by.x = "target_day", by.y = "thisday", all.x = TRUE)
BEFORE[, pct_before := round(proportion_female * 100, 3)]

# Step 2b: Get % women on the day AFTER each cohort change day.
# This represents the composition of the incoming parliament, just after
# the election turnover has settled.
AFTER <- term_starts[!is.na(new_cohort_day), .(
  parliament_id, new_cohort_day,
  target_day = new_cohort_day + 1
)]
AFTER <- merge(AFTER, DAILY_COUNTS[, .(thisday, pol_all, pol_f, proportion_female)],
               by.x = "target_day", by.y = "thisday", all.x = TRUE)
AFTER[, pct_after := round(proportion_female * 100, 3)]

# Step 3: Calculate the "election jump" for each parliament.
# election_jumps = pct_after - pct_before: how much did this election change
# the proportion of women? Positive = more women after, negative = fewer.
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
# on women's representation, stripping out all mid-term noise.
# Use the first non-NA pct_after as starting point (the first parliament's
# cohort day may fall before DAILY_COUNTS begins, giving NA)
startpercentage <- DELTA$pct_after[which(!is.na(DELTA$pct_after))[1]]

# Replace NA election_jumps with 0 so cumsum works (NA would propagate)
election_jumps_clean <- DELTA$election_jumps
election_jumps_clean[is.na(election_jumps_clean)] <- 0

DELTA$running_average_election_only <- startpercentage + cumsum(election_jumps_clean)
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
  as.data.frame(DAILY_COUNTS[, list(thisday, pol_all, pol_f, pol_m)]),
  by.x = "new_cohort_day", by.y = "thisday", all.x = TRUE)
names(parl_transitions)[names(parl_transitions) == "pol_all"] <- "seated_total"
names(parl_transitions)[names(parl_transitions) == "pol_f"] <- "seated_f"
names(parl_transitions)[names(parl_transitions) == "pol_m"] <- "seated_m"

# Count fresh entrants on the cohort day (functions from R051_functions.R)
parl_transitions$entered_at_election_f <- sapply(parl_transitions$new_cohort_day,
  count_fresh_entrants, gender_episodes = RESEBU_FEMALE)

parl_transitions$entered_at_election_m <- sapply(parl_transitions$new_cohort_day,
  count_fresh_entrants, gender_episodes = RESEBU_MALE)

parl_transitions$entered_at_election_total = parl_transitions$entered_at_election_f + parl_transitions$entered_at_election_m

# Count mid-term attrition: seated after this election but gone before the next
next_cohort_days <- c(parl_transitions$new_cohort_day[-1], NA)

parl_transitions$attrition_f <- mapply(count_midterm_attrition,
  parl_transitions$new_cohort_day, next_cohort_days,
  MoreArgs = list(gender_episodes = RESEBU_FEMALE))

parl_transitions$attrition_m <- mapply(count_midterm_attrition,
  parl_transitions$new_cohort_day, next_cohort_days,
  MoreArgs = list(gender_episodes = RESEBU_MALE))

parl_transitions$attrition_total = parl_transitions$attrition_f + parl_transitions$attrition_m

# Gender-specific attrition rate: what % of women/men seated at the start left mid-term
# e.g. 98 women seated, 1 left → 1.0%; NA when no one of that gender was seated
parl_transitions$attrition_pct_f <- ifelse(
  is.na(parl_transitions$attrition_f) | parl_transitions$seated_f == 0,
  NA_real_,
  round(100 * parl_transitions$attrition_f / parl_transitions$seated_f, 1))

parl_transitions$attrition_pct_m <- ifelse(
  is.na(parl_transitions$attrition_m) | parl_transitions$seated_m == 0,
  NA_real_,
  round(100 * parl_transitions$attrition_m / parl_transitions$seated_m, 1))

# Count mid-term reinforcements: MPs who were NOT seated after this election
# but ARE seated before the next election (e.g. by-election winners, etc.)
parl_transitions$reinforcements_f <- mapply(count_midterm_reinforcements,
  parl_transitions$new_cohort_day, next_cohort_days,
  MoreArgs = list(gender_episodes = RESEBU_FEMALE))

parl_transitions$reinforcements_m <- mapply(count_midterm_reinforcements,
  parl_transitions$new_cohort_day, next_cohort_days,
  MoreArgs = list(gender_episodes = RESEBU_MALE))

parl_transitions$reinforcements_total = parl_transitions$reinforcements_f + parl_transitions$reinforcements_m

# Check: does every mid-term departure get replaced?
# attrition - reinforcements should equal the drop in parliament size between
# cohort_day+1 and next_cohort_day-1 (unfilled vacancies near end of term)
parl_transitions$unfilled_vacancies <- ifelse(
  is.na(parl_transitions$attrition_total) | is.na(parl_transitions$reinforcements_total),
  NA_integer_,
  parl_transitions$attrition_total - parl_transitions$reinforcements_total)

# Reinforcement bias: are by-elections disproportionately replacing with women?
# Compares the female share of reinforcements to the female share of seated MPs.
# Positive = by-elections favored women beyond their existing representation.
# Zero = neutral (replacements mirror existing composition).
# Negative = by-elections disfavored women.
parl_transitions$reinforcement_bias_f <- ifelse(
  is.na(parl_transitions$reinforcements_total) | parl_transitions$reinforcements_total == 0 |
  is.na(parl_transitions$seated_total) | parl_transitions$seated_total == 0,
  NA_real_,
  round(
    100 * parl_transitions$reinforcements_f / parl_transitions$reinforcements_total -
    100 * parl_transitions$seated_f / parl_transitions$seated_total,
  1))

print(parl_transitions)

# Summary statistics for graph annotation
avg_attrition_f <- round(mean(parl_transitions$attrition_pct_f, na.rm = TRUE), 1)
sd_attrition_f <- round(sd(parl_transitions$attrition_pct_f, na.rm = TRUE), 1)
avg_attrition_m <- round(mean(parl_transitions$attrition_pct_m, na.rm = TRUE), 1)
sd_attrition_m <- round(sd(parl_transitions$attrition_pct_m, na.rm = TRUE), 1)
avg_reinforcement_bias_f <- round(mean(parl_transitions$reinforcement_bias_f, na.rm = TRUE), 1)
sd_reinforcement_bias_f <- round(sd(parl_transitions$reinforcement_bias_f, na.rm = TRUE), 1)

summary_text <- sprintf(
  "Avg. mid-term attrition rate:\n  Women: %.1f%% (SD=%.1f)   Men: %.1f%% (SD=%.1f)\nAvg. reinforcement bias (women): %.1f%% (SD=%.1f)\n  (%% women among replacements minus\n   %% women seated at start of term;\n   positive = mid-term replacements favor women)",
  avg_attrition_f, sd_attrition_f, avg_attrition_m, sd_attrition_m,
  avg_reinforcement_bias_f, sd_reinforcement_bias_f
)
cat("\n", summary_text, "\n")

mean(parl_transitions$reinforcement_bias_f,na.rm=TRUE)

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
  geom_line(aes(y = pol_all / max(pol_all, na.rm = TRUE), color = "Total MPs"), linewidth = 0.8) +
  geom_step(data = parl_baseline, 
            aes(x = start_date, y = baseline_size / max(DAILY_COUNTS$pol_all, na.rm = TRUE), 
                color = "Parliament Size Baseline"), 
            linewidth = 1.0) +
  geom_line(aes(y = proportion_female, color = "Daily Women %"), linewidth = 0.8) +
  geom_step(data = DELTA,
            aes(x = new_cohort_day, y = running_average_election_only / 100,
                color = "Election-Only Trend"),
            linewidth = 1.0) +
  # Add red highlighting for deviation periods (thicker and more visible)
  {if (nrow(deviation_segments) > 0) 
    geom_line(data = deviation_segments, 
              aes(x = thisday, y = pol_all_normalized, group = period_id), 
              color = "red", linewidth = 2.5, alpha = 0.8) 
   else NULL} +
  # Add warning labels with background boxes
  {if (nrow(warning_labels) > 0)
    geom_label(data = warning_labels,
               aes(x = x, y = y, label = label),
               color = "red", size = 2.5, fontface = "bold",
               fill = "white", alpha = 0.8,
               hjust = 0.5, vjust = 0.5)
   else NULL} +
  # Add summary statistics in top-left corner
  annotate("label", x = min(DAILY_COUNTS$thisday), y = 0.95,
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
      name = "Proportion of Women",
      breaks = seq(0, 1, 0.2),
      labels = scales::percent_format()
    )
  ) +
  scale_x_date(
    name = "Time"
    # , limits = as.Date(c("1945-01-01", "2025-12-31"))
  ) +
  scale_color_manual(
    values = c("Daily Women %" = "red", "Total MPs" = "blue", "Parliament Size Baseline" = "black", "Election-Only Trend" = "green"),
    name = "Measures"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.margin = margin(10, 30, 10, 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  ggtitle(paste("Women's Representation and Parliament Size in", country_name, "Over Time")) +
  labs(caption = paste("Generated on:", format(Sys.time(), "%Y-%m-%d at %H:%M:%S")))

# Plot created successfully!

# Save the plot
plot_filename <- paste0("women_representation_simplified_", country_code, ".png")
ggsave(plot_filename, plot = p_simple, width = 16, height = 8, dpi = 150, bg = "white")
cat("Plot saved as", plot_filename, "with double width!\n")

# Create final deviation dataframe for export/analysis
final_deviations <- detect_parliament_deviations(DAILY_COUNTS, parl_baseline, seat_threshold = 5, duration_threshold_days = 90, merge_gap_days = 7)

p_simple
