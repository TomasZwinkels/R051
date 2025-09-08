# Simplified version to generate the plot
library(sqldf); library(stringr); library(readr); library(dplyr); library(writexl); library(openxlsx); library(testthat); library(data.table); library(ggplot2)

setwd("/home/tomas/projects/ProjectR051_NewDaybyDay")

# Configuration: Set country code for analysis
country_code <- "NL"  # Options: "NL" (Netherlands), "CH" (Switzerland)

# Load data integrity functions 
pathtocheckerfunctions <- "/home/tomas/projects/ProjectR047_PCCIntegrity/"
source(paste0(pathtocheckerfunctions,"R047_RESE_functions.R"))
test_file(paste0(pathtocheckerfunctions,"R047_RESE_unittests.R"))

# Load custom R051 functions
source("R051_functions.R")
test_file("R051_unittests.R")

# Import data
POLI = read.csv("PCC/POLI.csv", header = TRUE, sep = ";")
RESE = read.csv("PCC/RESE.csv", header = TRUE, sep = ";")
PARL = read.csv("PCC/PARL.csv", header = TRUE, sep = ";")
MEME = read.csv("PCC/MEME.csv", header = TRUE, sep = ";")

# Data integrity checks
RESE <- RESE[which(RESE$country_abb == country_code),]
check_RESE_persid_in_POLI(RESE,POLI) # should return TRUE
check_RESE_resentryid_unique(RESE) # should return TRUE

# Focus on parliamentary membership
RESE <- RESE[which(RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01")),]
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
PARL <- PARL[which(PARL$country_abb == country_code & PARL$level == "NT" & (PARL$assembly_abb == "TK" | PARL$assembly_abb == "NR")),]

# Filter again for parliamentary episodes in selected country
RESE <- RESE[which(RESE$country_abb == country_code & RESE$political_function %in% c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01")),]

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
current_data_version <- trimws(readLines("PCC/dataversion.txt")[1])
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

# Calculate fictional "election-only" fluctuations
# Parameter: how many days before/after elections to measure
n_days <- 60  # change to any integer you like

# Get unique parliament start dates with their IDs
setDT(PARL)
setDT(DAILY_COUNTS)
term_starts <- unique(PARL[, .(parliament_id, term_start = as.Date(leg_period_start_dateformat))])

# Functions loaded from R051_functions.R

# Get percentage before and after elections
BEFORE <- grab_pct_women(term_starts, -n_days)
setnames(BEFORE, "pct_women", "pct_women_before_election")

AFTER <- grab_pct_women(term_starts, n_days)
setnames(AFTER, "pct_women", "pct_women_after_election")

# Calculate election jumps
DELTA <- merge(
  BEFORE[, .(parliament_id, term_start, pct_before = pct_women_before_election)],
  AFTER[, .(parliament_id, pct_after = pct_women_after_election)],
  by = "parliament_id"
)[order(term_start)][
  , election_jumps := round(pct_after - pct_before, 3)][]

# Safety check
if(!nrow(DELTA) == length(unique(DELTA$parliament_id))) {
  stop("ERROR: Non-unique parliament_ids in DELTA - data integrity issue")
}

# Calculate running average with election fluctuations only
# Handle the first election which may have NA before value
startpercentage <- DELTA$pct_after[1]  # Use first "after" value as starting point

# Replace NA election_jumps with 0 for cumsum
election_jumps_clean <- DELTA$election_jumps
election_jumps_clean[is.na(election_jumps_clean)] <- 0

DELTA$running_average_election_only <- startpercentage + cumsum(election_jumps_clean)

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
            aes(x = term_start, y = running_average_election_only / 100, 
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
  scale_x_date(name = "Time") +
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
  ggtitle(paste("Women's Representation and Parliament Size in", 
                ifelse(country_code == "NL", "Netherlands", "Switzerland"), "Over Time")) +
  labs(caption = paste("Generated on:", format(Sys.time(), "%Y-%m-%d at %H:%M:%S")))

# Plot created successfully!

# Save the plot
plot_filename <- paste0("women_representation_simplified_", country_code, ".png")
ggsave(plot_filename, plot = p_simple, width = 16, height = 8, dpi = 150, bg = "white")
cat("Plot saved as", plot_filename, "with double width!\n")

# Create final deviation dataframe for export/analysis
final_deviations <- detect_parliament_deviations(DAILY_COUNTS, parl_baseline, seat_threshold = 5, duration_threshold_days = 90, merge_gap_days = 7)

p_simple