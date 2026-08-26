# R051 Custom Functions
# Functions for parliament size analysis and data integrity checking

###############################################################################
# parliament_size may fluctuate within a term. Following the PCC convention for
# committee `seats` (codebook), a fluctuating seat count is written as the
# successive values separated by ';' in chronological order, e.g. "519;663" for
# DE_NT-BT_1987 (519 seated until German reunification on 03oct1990, 663 after).
#
# PARL stores only the term's start/end, NOT the intra-term changeover date(s),
# so those must be supplied manually here. A parliament_size with N values needs
# N-1 changeover dates, each the FIRST day the next size takes effect (midnight
# rule). Consumers that build a daily/step size series (dashboard daily counts,
# R051 baseline) STOP with an actionable error if a required date is missing.
#
# NOTE: this registry is intentionally duplicated in
# ProjectR047_PCCIntegrity/R047_PARL_functions.R (SIZE_CHANGE_DATES). Keep the
# two copies in sync.
###############################################################################
SIZE_CHANGE_DATES <- list(
  "DE_NT-BT_1949" = as.Date("1952-02-01"),  # West Berlin delegation 8 -> 19
  "DE_NT-BT_1953" = as.Date("1957-01-04"),  # Saarland accession: 509 -> 519
  "DE_NT-BT_1987" = as.Date("1990-10-03")   # reunification: 519 -> 663
)

###############################################################################
# Function: is_valid_parliament_size
# Description:
#   TRUE if x is a single positive integer, or ';'-separated positive integers
#   (the fluctuating-seats form). Vectorised. NA -> FALSE. Needs no date.
###############################################################################
is_valid_parliament_size <- function(x) {
  x <- as.character(x)
  ok <- !is.na(x) & grepl("^[0-9]{1,3}(;[0-9]{1,3})*$", x)
  # regex guarantees 1-3 digit segments; also reject any zero segment ("0")
  ok[ok] <- vapply(strsplit(x[ok], ";", fixed = TRUE), function(seg) {
    all(as.integer(seg) > 0)
  }, logical(1))
  ok
}

###############################################################################
# Function: parse_parliament_size_series
# Description:
#   Expand one parliament's (possibly ';'-separated) parliament_size into one
#   row per sub-period, splitting the term at the manually-registered changeover
#   date(s). Single-value sizes return one row spanning the whole term.
# Input:
#   - parliament_id : scalar character
#   - leg_start, leg_end : Date scalars (term bounds)
#   - parliament_size : scalar character, e.g. "518" or "519;663"
#   - registry : named list parliament_id -> Date (or Date vector) of changeovers
# Output:
#   - data.frame(parliament_id, seg_start, seg_end, size) with N rows for N values
# Stops:
#   - if N>1 values but the registry lacks exactly N-1 valid, ordered, in-range
#     changeover dates for this parliament_id.
#
# NOTE: kept identical to the copy in R047_PARL_functions.R.
###############################################################################
parse_parliament_size_series <- function(parliament_id, leg_start, leg_end,
                                         parliament_size,
                                         registry = SIZE_CHANGE_DATES) {
  leg_start <- as.Date(leg_start)
  leg_end   <- as.Date(leg_end)
  sizes <- as.integer(strsplit(as.character(parliament_size), ";", fixed = TRUE)[[1]])
  n <- length(sizes)

  if (n <= 1) {
    return(data.frame(parliament_id = parliament_id,
                      seg_start = leg_start, seg_end = leg_end,
                      size = sizes, stringsAsFactors = FALSE))
  }

  dates <- registry[[parliament_id]]
  need <- n - 1L
  example <- if (need > 1) {
    paste0("c(", paste(rep("as.Date(\"YYYY-MM-DD\")", need), collapse = ", "), ")")
  } else {
    "as.Date(\"YYYY-MM-DD\")"
  }
  hint <- paste0(
    "parliament_size \"", parliament_size, "\" for ", parliament_id,
    " changes mid-term; add its ", need, " changeover date",
    if (need > 1) "s" else "",
    " to SIZE_CHANGE_DATES in this script, e.g.\n",
    "  \"", parliament_id, "\" = ", example,
    "\nthen re-run."
  )
  if (is.null(dates)) stop(hint, call. = FALSE)
  dates <- as.Date(dates)
  if (length(dates) != need || any(is.na(dates)))
    stop(hint, call. = FALSE)
  if (is.unsorted(dates, strictly = TRUE))
    stop("Changeover dates for ", parliament_id,
         " must be strictly increasing.", call. = FALSE)
  if (dates[1] <= leg_start || dates[need] > leg_end)
    stop("Changeover dates for ", parliament_id,
         " must lie within (", format(leg_start), ", ", format(leg_end),
         "].", call. = FALSE)

  seg_start <- c(leg_start, dates)
  seg_end   <- c(dates - 1L, leg_end)
  data.frame(parliament_id = parliament_id,
             seg_start = seg_start, seg_end = seg_end,
             size = sizes, stringsAsFactors = FALSE)
}

###############################################################################
# Function: count_mp_transitions
# Description:
#   Count how many MPs of a given group entered or left parliament within a
#   specified date window. Operates on raw RESE + POLI data — does its own
#   filtering, gender join, and date parsing internally.
#
# Inputs:
#   - from_date: Date, start of the window (inclusive)
#   - to_date: Date, end of the window (inclusive)
#   - direction: "entered" (episode start in window) or "left" (episode end)
#   - gender: "f" or "m"
#   - country_code: e.g. "CA", "NO", "DE"
#   - RESE: raw RESE data.frame (as read from CSV)
#   - POLI: raw POLI data.frame (as read from CSV)
#
# Returns:
#   Integer count of unique persons matching the criteria.
###############################################################################
count_mp_transitions <- function(from_date, to_date, direction, gender,
                                 country_code, RESE, POLI) {
  if (!direction %in% c("entered", "left")) {
    stop("direction must be 'entered' or 'left'")
  }
  if (!gender %in% c("f", "m")) {
    stop("gender must be 'f' or 'm'")
  }

  # Filter to country and parliamentary membership functions
  parl_functions <- c("NT_LE-LH_T3_NA_01", "NT_LE_T3_NA_01", "NT_LE_T3_NA_09")
  rese <- RESE[RESE$country_abb == country_code &
               RESE$political_function %in% parl_functions, ]

  if (nrow(rese) == 0) return(0L)

  # Join gender from POLI
  poli_gender <- POLI[, c("pers_id", "gender")]
  rese <- merge(rese, poli_gender, by = "pers_id", all.x = TRUE)

  # Clean gender (tf→f, tm→m)
  rese$gender[rese$gender == "tf"] <- "f"
  rese$gender[rese$gender == "tm"] <- "m"

  # Filter to requested gender
  rese <- rese[!is.na(rese$gender) & rese$gender == gender, ]

  if (nrow(rese) == 0) return(0L)

  # Parse dates (strip censoring markers)
  clean_date <- function(x) {
    x <- gsub("\\[\\[rcen\\]\\]", "", x)
    x <- gsub("\\[\\[lcen\\]\\]", "", x)
    as.Date(x, format = "%d%b%Y")
  }

  rese$start_parsed <- clean_date(rese$res_entry_start)
  rese$end_parsed <- clean_date(rese$res_entry_end)

  # Count transitions in window
  from_date <- as.Date(from_date)
  to_date <- as.Date(to_date)

  if (direction == "entered") {
    matches <- rese[!is.na(rese$start_parsed) &
                    rese$start_parsed >= from_date &
                    rese$start_parsed <= to_date, ]
  } else {
    matches <- rese[!is.na(rese$end_parsed) &
                    rese$end_parsed >= from_date &
                    rese$end_parsed <= to_date, ]
  }

  length(unique(matches$pers_id))
}

###############################################################################
# Function: find_new_cohort_day
# Description:
#   For a given parliament, find the day with the most MP entries (inflow
#   only, ignoring departures). This is the data-driven "cohort change day"
#   — typically the day the new parliament is seated.
#
# Inputs:
#   - parliament_id: e.g. "CA_NT-HC_2019"
#   - RESE: raw RESE data.frame (country-filtered, parliamentary functions only)
#   - PARL: raw PARL data.frame (with leg_period_start/end_dateformat columns)
#
# Returns:
#   A Date: the day with the most MP entries within the search window.
#   NA if no transitions found.
###############################################################################
find_new_cohort_day <- function(parliament_id, RESE, PARL) {
  # Look up parliament start and end dates
  # Convert to data.frame to avoid data.table scoping issues
  # (data.table's [.data.table evaluates parliament_id as the column, not the argument)
  parl_df <- as.data.frame(PARL)
  parl_df <- parl_df[order(parl_df$leg_period_start_dateformat), ]
  parl_idx <- which(parl_df$parliament_id == parliament_id)
  if (length(parl_idx) == 0) {
    warning("parliament_id '", parliament_id, "' not found in PARL")
    return(as.Date(NA))
  }

  # Search window: from midpoint of previous parliament to midpoint of this one.
  # This ensures the election day (which falls between parliament end and session
  # start) is always captured, regardless of the gap size.
  term_start <- parl_df$leg_period_start_dateformat[parl_idx]
  term_end <- parl_df$leg_period_end_dateformat[parl_idx]
  if (is.na(term_end)) term_end <- Sys.Date()

  if (parl_idx > 1) {
    prev_start <- parl_df$leg_period_start_dateformat[parl_idx - 1]
    search_from <- prev_start + as.integer(difftime(term_start, prev_start, units = "days")) / 2
  } else {
    search_from <- term_start - 180  # first parliament: look 6 months before
  }

  if (parl_idx < nrow(parl_df)) {
    next_start <- parl_df$leg_period_start_dateformat[parl_idx + 1]
    search_to <- term_start + as.integer(difftime(next_start, term_start, units = "days")) / 2
  } else {
    search_to <- term_end  # last parliament: search to end of term
  }

  # Parse RESE dates (strip censoring markers)
  starts <- as.Date(gsub("\\[\\[.*\\]\\]", "", RESE$res_entry_start), format = "%d%b%Y")
  ends <- as.Date(gsub("\\[\\[.*\\]\\]", "", RESE$res_entry_end), format = "%d%b%Y")

  # Collect entry dates within the search window (inflow only)
  entry_dates <- starts[!is.na(starts) & starts >= search_from & starts <= search_to]
  if (length(entry_dates) == 0) return(as.Date(NA))

  # Find the date with the most MP entries
  date_counts <- table(entry_dates)
  peak_date <- as.Date(names(which.max(date_counts)))

  peak_date
}

###############################################################################
# Function: count_fresh_entrants
# Description:
#   Count MPs of a given group who are in parliament on target_day but were
#   NOT in parliament the day before. These are "fresh" entrants regardless
#   of whether they served in a previous parliament.
#
# Inputs:
#   - target_day: Date to check
#   - group_episodes: data.table with res_entry_start_dateformat,
#     res_entry_end_dateformat, pers_id (already filtered to one group)
#
# Returns:
#   Integer count of fresh entrants.
###############################################################################
count_fresh_entrants <- function(target_day, group_episodes) {
  if (is.na(target_day)) return(NA_integer_)
  day_before <- target_day - 1

  on_today <- group_episodes[res_entry_start_dateformat <= target_day &
                               res_entry_end_dateformat >= target_day, pers_id]
  on_yesterday <- group_episodes[res_entry_start_dateformat <= day_before &
                                   res_entry_end_dateformat >= day_before, pers_id]

  length(setdiff(on_today, on_yesterday))
}

###############################################################################
# Function: count_midterm_attrition
# Description:
#   Count MPs of a given group who were seated the day after this_cohort_day
#   but were no longer seated the day before next_cohort_day. These are
#   genuine between-election dropouts (deaths, resignations, appointments),
#   not MPs whose terms ended at dissolution.
#
# Inputs:
#   - this_cohort_day: Date of the current election's cohort change day
#   - next_cohort_day: Date of the next election's cohort change day
#   - group_episodes: data.table with res_entry_start_dateformat,
#     res_entry_end_dateformat, pers_id (already filtered to one group)
#
# Returns:
#   Integer count of mid-term dropouts. NA if either date is NA.
###############################################################################
count_midterm_attrition <- function(this_cohort_day, next_cohort_day, group_episodes) {
  if (is.na(this_cohort_day) || is.na(next_cohort_day)) return(NA_integer_)

  after_day <- this_cohort_day + 1
  before_next <- next_cohort_day - 1

  # Who was seated right after this election?
  seated_after <- group_episodes[res_entry_start_dateformat <= after_day &
                                   res_entry_end_dateformat >= after_day, pers_id]
  # Who was still seated right before the next election?
  seated_before_next <- group_episodes[res_entry_start_dateformat <= before_next &
                                         res_entry_end_dateformat >= before_next, pers_id]

  # Attrition = seated after this election but gone before the next
  length(setdiff(seated_after, seated_before_next))
}

###############################################################################
# Function: count_midterm_reinforcements
# Description:
#   Count MPs of a given group who were NOT seated the day after this election
#   but ARE seated the day before the next election. These are mid-term entrants
#   (by-election winners, appointees replacing vacancies, etc.)
#
# Inputs:
#   - this_cohort_day: Date of the current election's cohort change day
#   - next_cohort_day: Date of the next election's cohort change day
#   - group_episodes: data.table with res_entry_start_dateformat,
#     res_entry_end_dateformat, pers_id (already filtered to one group)
#
# Returns:
#   Integer count of mid-term entrants. NA if either date is NA.
###############################################################################
count_midterm_reinforcements <- function(this_cohort_day, next_cohort_day, group_episodes) {
  if (is.na(this_cohort_day) || is.na(next_cohort_day)) return(NA_integer_)

  after_day <- this_cohort_day + 1
  before_next <- next_cohort_day - 1

  # Who was seated right after this election?
  seated_after <- group_episodes[res_entry_start_dateformat <= after_day &
                                   res_entry_end_dateformat >= after_day, pers_id]
  # Who was seated right before the next election?
  seated_before_next <- group_episodes[res_entry_start_dateformat <= before_next &
                                         res_entry_end_dateformat >= before_next, pers_id]

  # Reinforcements = seated before the next election but NOT after this one
  length(setdiff(seated_before_next, seated_after))
}

# Helper function to grab % women at an offset relative to term start
grab_pct_focal <- function(ts, offset_days, daily = DAILY_COUNTS) {
  ts2 <- copy(ts)[, target_day := as.Date(term_start + as.integer(offset_days))]
  out <- merge(
    ts2,
    daily[, .(thisday, pol_all, pol_focal, proportion_focal)],
    by.x = "target_day", by.y = "thisday",
    all.x = TRUE, sort = FALSE
  )
  out[, `:=`(
    offset_days = as.integer(offset_days),
    pct_focal   = round(proportion_focal * 100, 3)
  )]
  setorder(out, term_start)
  out[, .(parliament_id, term_start, target_day, offset_days,
          pol_all, pol_focal, proportion_focal, pct_focal)]
}

# Function to detect problematic deviations from parliament size baseline
detect_parliament_deviations <- function(daily_counts, parl_baseline, 
                                       seat_threshold = 5, 
                                       duration_threshold_days = 90,
                                       merge_gap_days = 7) {
  
  # Merge daily counts with appropriate baseline for each day
  daily_with_baseline <- copy(daily_counts)
  
  # Add baseline size for each day using a simpler approach
  setkey(parl_baseline, start_date)
  daily_with_baseline[, baseline_size := NA_real_]
  
  # For each day, find the corresponding parliament baseline
  for (i in 1:nrow(parl_baseline)) {
    period_start <- parl_baseline$start_date[i]
    period_end <- parl_baseline$end_date[i]
    baseline_val <- parl_baseline$baseline_size[i]
    
    daily_with_baseline[thisday >= period_start & thisday <= period_end, 
                       baseline_size := baseline_val]
  }
  
  # Remove rows with NA baseline (before first parliament period)
  daily_with_baseline <- daily_with_baseline[!is.na(baseline_size)]
  
  # Calculate deviations
  daily_with_baseline[, deviation := pol_all - baseline_size]
  daily_with_baseline[, abs_deviation := abs(deviation)]
  daily_with_baseline[, deviation_type := fifelse(deviation > seat_threshold, "structurally_too_high",
                                                  fifelse(deviation < -seat_threshold, "structurally_too_low", "normal"))]
  
  # Find consecutive periods of problematic deviations
  daily_with_baseline[, group_id := cumsum(c(TRUE, deviation_type[-1] != deviation_type[-.N]))]
  
  # Summarize periods
  deviation_periods <- daily_with_baseline[deviation_type != "normal", .(
    start_date = min(thisday),
    end_date = max(thisday),
    duration_days = as.numeric(max(thisday) - min(thisday)) + 1,
    deviation_type = first(deviation_type),
    avg_deviation = round(mean(deviation), 1),
    max_deviation = max(abs_deviation)
  ), by = group_id]
  
  # Add parliament IDs for each period
  if (nrow(deviation_periods) > 0) {
    deviation_periods[, parliament_ids := ""]
    for (i in 1:nrow(deviation_periods)) {
      period_start <- deviation_periods$start_date[i]
      period_end <- deviation_periods$end_date[i]
      
      relevant_parliaments <- parl_baseline[
        (start_date <= period_end) & (end_date >= period_start),
        parliament_id
      ]
      
      deviation_periods$parliament_ids[i] <- paste(relevant_parliaments, collapse = ", ")
    }
  }
  
  # Merge closely spaced deviation periods of the same type
  if (nrow(deviation_periods) > 1 && merge_gap_days > 0) {
    deviation_periods <- deviation_periods[order(deviation_type, start_date)]
    merged_periods <- data.table()
    
    for (dev_type in unique(deviation_periods$deviation_type)) {
      type_periods <- deviation_periods[deviation_type == dev_type][order(start_date)]
      
      if (nrow(type_periods) > 0) {
        current_period <- type_periods[1]
        
        if (nrow(type_periods) > 1) {
          for (i in 2:nrow(type_periods)) {
            gap_days <- as.numeric(type_periods$start_date[i] - current_period$end_date) - 1
            
            if (gap_days <= merge_gap_days) {
              # Merge periods
              current_period$end_date <- type_periods$end_date[i]
              current_period$duration_days <- as.numeric(current_period$end_date - current_period$start_date) + 1
              current_period$avg_deviation <- round(
                (current_period$avg_deviation * current_period$duration_days + 
                 type_periods$avg_deviation[i] * type_periods$duration_days[i]) / 
                (current_period$duration_days + type_periods$duration_days[i]), 1
              )
              current_period$max_deviation <- max(current_period$max_deviation, type_periods$max_deviation[i])
              current_period$parliament_ids <- paste(
                unique(c(strsplit(current_period$parliament_ids, ", ")[[1]], 
                         strsplit(type_periods$parliament_ids[i], ", ")[[1]])), 
                collapse = ", "
              )
            } else {
              # Start new period
              merged_periods <- rbind(merged_periods, current_period)
              current_period <- type_periods[i]
            }
          }
        }
        merged_periods <- rbind(merged_periods, current_period)
      }
    }
    deviation_periods <- merged_periods[order(start_date)]
  }
  
  # Filter by duration threshold
  problematic_periods <- deviation_periods[duration_days >= duration_threshold_days]
  
  # Clean up and order
  if (nrow(problematic_periods) > 0) {
    problematic_periods[, group_id := NULL]
    problematic_periods <- problematic_periods[order(start_date)]
  }
  
  return(problematic_periods)
}