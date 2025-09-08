# Simplified version to generate the plot
library(sqldf); library(stringr); library(readr); library(dplyr); library(writexl); library(openxlsx); library(testthat); library(data.table); library(ggplot2)

setwd("/home/tomas/projects/ProjectR051_NewDaybyDay")

# Load data integrity functions 
pathtocheckerfunctions <- "/home/tomas/projects/ProjectR047_PCCIntegrity/"
source(paste0(pathtocheckerfunctions,"R047_RESE_functions.R"))
test_file(paste0(pathtocheckerfunctions,"R047_RESE_unittests.R"))

# Import data
POLI = read.csv("PCC/POLI.csv", header = TRUE, sep = ";")
RESE = read.csv("PCC/RESE.csv", header = TRUE, sep = ";")
PARL = read.csv("PCC/PARL.csv", header = TRUE, sep = ";")
MEME = read.csv("PCC/MEME.csv", header = TRUE, sep = ";")

# Data integrity checks
RESE <- RESE[which(RESE$country_abb == "NL"),]
check_RESE_persid_in_POLI(RESE,POLI)
check_RESE_resentryid_unique(RESE)

# Focus on parliamentary membership
RESE <- RESE[which(RESE$political_function == "NT_LE-LH_T3_NA_01"),]

# Data integrity checks with preprocessing
if( check_RESE_persid_in_POLI(RESE,POLI) == FALSE||
    check_RESE_resentryid_unique(RESE)==FALSE||
    check_RESE_parlmemeppisodes_anyfulloverlap(preprocess_RESEdates(RESE))==TRUE||
    check_RESE_anynear_fulloverlap(preprocess_RESEdates(RESE))==TRUE 
  )
{
RESE <- NULL
}

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

# Focus on NL
PARL <- PARL[which(PARL$country_abb == "NL"),]

# Filter again for parliamentary episodes in Netherlands
RESE <- RESE[which(RESE$country_abb == "NL" & RESE$political_function == "NT_LE-LH_T3_NA_01"),]

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

# Calculate daily counts (simplified version for faster execution)
# Calculating daily counts - this may take a while...

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

# Merging daily counts...

# Merge all by thisday
DAILY_COUNTS <- Reduce(function(x, y) merge(x, y, by = "thisday", all = TRUE),
                      list(DAILY_COUNTS_ALL, DAILY_COUNTS_MALE, DAILY_COUNTS_FEMALE)
)

# Calculate proportions
DAILY_COUNTS[, proportion_female := pol_f / pol_all]

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
n_days <- 30  # change to any integer you like

# Get unique parliament start dates with their IDs
setDT(PARL)
setDT(DAILY_COUNTS)
term_starts <- unique(PARL[, .(parliament_id, term_start = as.Date(leg_period_start_dateformat))])

# Helper function to grab % women at an offset relative to term start
grab_pct_women <- function(ts, offset_days, daily = DAILY_COUNTS) {
  ts2 <- copy(ts)[, target_day := as.Date(term_start + as.integer(offset_days))]
  out <- merge(
    ts2,
    daily[, .(thisday, pol_all, pol_f, proportion_female)],
    by.x = "target_day", by.y = "thisday",
    all.x = TRUE, sort = FALSE
  )
  out[, `:=`(
    offset_days = as.integer(offset_days),
    pct_women   = round(proportion_female * 100, 3)
  )]
  setorder(out, term_start)
  out[, .(parliament_id, term_start, target_day, offset_days,
          pol_all, pol_f, proportion_female, pct_women)]
}

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

# Creating simplified plot...

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
  scale_y_continuous(
    name = "Total Number of MPs", 
    breaks = scales::pretty_breaks(n = 6),
    labels = function(x) round(x * max(DAILY_COUNTS$pol_all, na.rm = TRUE)),
    sec.axis = sec_axis(
      ~ ., 
      name = "Proportion of Women",
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
  ggtitle("Women's Representation and Parliament Size in Netherlands Over Time") +
  labs(caption = paste("Generated on:", format(Sys.time(), "%Y-%m-%d at %H:%M:%S")))

# Plot created successfully!

# Save the plot
ggsave("women_representation_simplified.png", plot = p_simple, width = 16, height = 8, dpi = 150, bg = "white")
# Plot saved as women_representation_simplified.png with double width!

p_simple