# R051 Custom Functions
# Functions for parliament size analysis and data integrity checking

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