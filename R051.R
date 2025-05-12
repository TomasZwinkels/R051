######################################################################################
#################################### SETUP ########################################### test
######################################################################################

	# INSTRUCTIONS
		# typically, one would run this script 'top to bottom' and fix issues higher up 
			# before going on to the subsequent lines of codes (e.g. fix fully overlapping
			# membership episodes before going into checking partially overlapping ones)


	# change the language and date formatting to English if it is not already
		Sys.setenv(LANG = "EN")
		Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct does not work
		Sys.getlocale(category = "LC_ALL")

		setwd("F:/PolCa/Analysis/R/ProjectR051_NewDaybyDay")
		getwd()
	
		# install.packages("sqldf")
		# install.packages("stringr")
		# install.packages("lubridate")
		# install.packages("readr")
		# install.packages("dplyr")
		# install.packages("writexl")
		# install.packages("testthat")

	# packages
		library(sqldf)
		library(stringr)
		library(lubridate)
		library(readr)
		library(dplyr)
		library(writexl)
		library(openxlsx)
		library(testthat)
		
	# Load some custom functions
		# source("R047_functions.R")
		
	# Run the unit tests from my test file
		# test_file("R047_unittests.R")
	
	# import and inspect all the PCC data-frames
				
			# core
			
				# import and inspect politician level information
				POLI = read.csv("PCC/POLI.csv", header = TRUE, sep = ";")
				summary(POLI)
				names(POLI)
				
				# import and inspect all the resume entries
				RESE = read.csv("PCC/RESE.csv", header = TRUE, sep = ";")
				summary(RESE)
				names(RESE)	

				# import and inspect parliamentary information
				PARL = read.csv("PCC/PARL.csv", header = TRUE, sep = ";")
				summary(PARL)
				names(PARL)
				
				# import and inspect faction episode level info
				FACT = read.csv("PCC/FACT.csv", header = TRUE, sep = ";")
				summary(FACT)
				names(FACT)
			
				# import and inspect party membership eppisodes
				MEME = read.csv("PCC/MEME.csv", header = TRUE, sep = ";")
				summary(MEME)
				names(MEME)
			
			# not core at the moment
			
				# import and inspect election list entries
				# ELEN = read.csv("PCC/ELEN.csv", header = TRUE, sep = ";")
				# summary(ELEN)
				# names(ELEN)

				# import and inspect election districts
				# ELDI = read.csv("PCC/ELDI.csv", header = TRUE, sep = ";")
				# summary(ELDI)
				# names(ELDI)
					
				# import and inspect election lists
				# ELLI = read.csv("PCC/ELLI.csv", header = TRUE, sep = ";")
				# summary(ELLI)
				# names(ELLI)
				
				
				
				
				
				# import and inspect election level information
				# ELEC = read.csv("PCC/ELEC.csv", header = TRUE, sep = ";")
				# summary(ELEC)
				# names(ELEC)
				
				# import and inspect party level information
				# PART = read.csv("PCC/PART.csv", header = TRUE, sep = ";")
				# summary(PART)
				# names(PART)
				
				# import and inspect quota level info
				# QUOT = read.csv("PCC/QUOT.csv", header = TRUE, sep = ";")
				# summary(QUOT)
				# names(QUOT)
		
## bunch of date cleaning e.t.c.

	# RESE
		names(RESE)
			
		# the RESE start dates
			# first do the standard cleaning by getting rid off left and right censored dates
				RESE$res_entry_start <- gsub("[[rcen]]","",RESE$res_entry_start,fixed=TRUE)
				RESE$res_entry_start <- gsub("[[lcen]]","",RESE$res_entry_start,fixed=TRUE)
				RESE$res_entry_end <- gsub("[[rcen]]","",RESE$res_entry_end,fixed=TRUE)
				RESE$res_entry_end <- gsub("[[lcen]]","",RESE$res_entry_end,fixed=TRUE)
		
		# transform to R date and check if all the dates make sense
			# transform
			RESE$res_entry_start_posoxctformat <- as.POSIXct(as.character(RESE$res_entry_start),format=c("%d%b%Y"))
			RESE$res_entry_end_posoxctformat <- as.POSIXct(as.character(RESE$res_entry_end),format=c("%d%b%Y"))
			
	# PARL
		names(PARL)
			
		# the RESE start dates
			# first do the standard cleaning by getting rid off left and right censored dates
				PARL$leg_period_start <- gsub("[[rcen]]","",PARL$leg_period_start,fixed=TRUE)
				PARL$leg_period_start <- gsub("[[lcen]]","",PARL$leg_period_start,fixed=TRUE)
				PARL$leg_period_end <- gsub("[[rcen]]","",PARL$leg_period_end,fixed=TRUE)
				PARL$leg_period_end <- gsub("[[lcen]]","",PARL$leg_period_end,fixed=TRUE)
		
		# transform to R date and check if all the dates make sense
			# transform
			PARL$leg_period_start_posoxctformat <- as.POSIXct(as.character(PARL$leg_period_start),format=c("%d%b%Y"))
			PARL$leg_period_end_posoxctformat <- as.POSIXct(as.character(PARL$leg_period_end),format=c("%d%b%Y"))
		
		# focus on NL
			nrow(PARL)
			PARL <- PARL[which(PARL$country_abb == "NL"),]	
			nrow(PARL)	
		
		# check the result
			table(is.na(PARL$leg_period_start_posoxctformat)) # should return all FALSE
			table(is.na(PARL$leg_period_end_posoxctformat)) # should return all FALSE
			
	# MEME
	
		# 1) strip left- and right-censor markers
		MEME$memep_startdate <- gsub("[[rcen]]","", MEME$memep_startdate, fixed = TRUE)
		MEME$memep_startdate <- gsub("[[lcen]]","", MEME$memep_startdate, fixed = TRUE)
		MEME$memep_enddate   <- gsub("[[rcen]]","", MEME$memep_enddate,   fixed = TRUE)
		MEME$memep_enddate   <- gsub("[[lcen]]","", MEME$memep_enddate,   fixed = TRUE)

		# 2) convert to POSIXct (day-month-abbr-year like “23mar2017”)
		MEME$memep_start_posixct <- as.POSIXct(as.character(MEME$memep_startdate),
											   format = "%d%b%Y")
		MEME$memep_end_posixct   <- as.POSIXct(as.character(MEME$memep_enddate),
											   format = "%d%b%Y")
		# 3) (optional) keep NL only
		MEME$country_abb <- substr(MEME$pers_id, 1, 2)
		
		if ("country_abb" %in% names(MEME)) {
		  MEME <- MEME[MEME$country_abb == "NL", ]
		}

		# 4) quick sanity check
		table(is.na(MEME$memep_start_posixct))  # should be all FALSE
		table(is.na(MEME$memep_end_posixct))    # should be all FALSE
		
		# inspect the problematic cases (all much earlier on it seems).
		MEME[ which(is.na(MEME$memep_start_posixct)), ]
		MEME[ which(is.na(MEME$memep_end_posixct)), ]
		
	
## SET ACTIVE FILTERS
	
	# parliamentary episodes in the Netherlands
	nrow(RESE)
	RESE <- RESE[which(RESE$country_abb == "NL" & RESE$political_function == "NT_LE-LH_T3_NA_01"),]	
	nrow(RESE)		
	
	# check the result in terms of date cleaning
		table(is.na(RESE$res_entry_start_posoxctformat)) # should return all FALSE
		table(is.na(RESE$res_entry_end_posoxctformat)) # should return all FALSE

##### GET DAY-BY-DAY totals #####


	## focus on the relevant variables
	RESEBU <- RESE %>% 
				select(res_entry_id, pers_id, res_entry_start_posoxctformat, res_entry_end_posoxctformat)
				
	head(RESEBU)
	

	# Load required libraries
		library(data.table)
		library(ggplot2)

		# Assume RESEBU has already been created and converted to a data.table.
		# For example, if not already done:
		setDT(RESEBU)

		# Create a sequence of all days from the earliest start date to the latest end date
		all_days <- seq(from = min(RESEBU$res_entry_start_posoxctformat, na.rm = TRUE),
						to = max(RESEBU$res_entry_end_posoxctformat, na.rm = TRUE),
						by = "day")
		days_dt <- data.table(day = all_days)

		# For each day, count unique politicians (based on pers_id) 
		# whose interval covers that day.
		daily_counts <- days_dt[, .(
		  pol_count = RESEBU[day >= res_entry_start_posoxctformat & day <= res_entry_end_posoxctformat,
							 uniqueN(pers_id)]
		), by = day]

		# Inspect the first few rows
		head(daily_counts)

# overall
	ggplot(daily_counts, aes(x = day, y = pol_count)) +
	  geom_line() +
	  labs(
		title = "Daily Number of Politicians in Parliament",
		x = "Date",
		y = "Number of Politicians"
	  ) +
	  theme_minimal()
	  
# focussed on a  specific daterange - wider resolution

	# todo: lets all add the election dates as vertical gridlines here!

	# daterange to use
	daterangestart <- "2005-01-01"
	daterangeend <- "2010-12-31"

# 1) compute your break dates
month_breaks <- seq(
  from = as.POSIXct(daterangestart),
  to   = as.POSIXct(daterangeend),
  by   = "1 month"
)
year_breaks <- seq(
  from = as.POSIXct(daterangestart),
  to   = as.POSIXct(daterangeend),
  by   = "1 year"
)

ggplot(daily_counts, aes(x = day, y = pol_count)) +
  # your main line
  geom_line() +

  # thin monthly grid‐lines:
  geom_vline(
    xintercept = as.numeric(month_breaks), 
    size       = 0.15,            # thin
    colour     = "grey80"
  ) +
  # thick yearly grid‐lines:
  geom_vline(
    xintercept = as.numeric(year_breaks),
    size       = 0.8,            # thicker
    colour     = "blue"
  ) +
  # and a line at 150
   geom_hline(
    yintercept = 150, 
    size       = 0.25,            # thin
    colour     = "green"
  ) +

  # Month labels on bottom:
  scale_x_datetime(
    limits = c(as.POSIXct(daterangestart), as.POSIXct(daterangeend)),
    breaks = month_breaks,
    labels = function(x) substr(month.abb[as.POSIXlt(x)$mon + 1], 1, 1),
    # secondary axis for the years:
    sec.axis = dup_axis(
      breaks = year_breaks,
      labels = function(x) format(x, "%Y"),
      name   = "Year"
    )
  ) +

  scale_y_continuous(
    limits = c(125, 160),
    breaks = seq(125, 160, by = 1),
	minor_breaks = NULL
  ) +

  labs(
    title = "Daily Number of Politicians in Parliament (2005–2010)",
    x     = NULL,
    y     = "Number of Politicians"
  ) +

  theme_minimal() +
  theme(
    # turn off the built‐in vertical grid:
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # keep horizontal grid
    panel.grid.major.y = element_line()
  )

# inspect suspisous mutations

	# 1) define your window
	start_date <- as.POSIXct("2008-04-01", tz = "UTC")
	end_date   <- as.POSIXct("2008-04-30", tz = "UTC")

	# 2) grab the IDs
	ids_in_window <- RESEBU[
	  (res_entry_start_posoxctformat >= start_date & res_entry_start_posoxctformat <= end_date)
	  | (res_entry_end_posoxctformat   >= start_date & res_entry_end_posoxctformat   <= end_date),
	  res_entry_id
	]

	ids_in_window
	
# lets start getting FACT info in, that should gives us more to hold on to.

	# FACT
	
	# first, some integrity checks on FACT
	
		# focus on NL for now
		
		nrow(FACT)
		FACT <- FACT[which(substr(as.character(FACT$parliament_id),0,2) == "NL"),]
		nrow(FACT)
		head(FACT)
		tail(FACT)
	
		# do all the parliament_ids sum up to the right amounts
			aggregate(seats ~ parliament_id,
			  data = FACT,
			  FUN  = sum,
			  na.rm = TRUE)
	
		# do the most dominant start and enddates match with PARL
		
			# the start dates
			table(FACT$faction_start)
			
			names(table(FACT$faction_start)) %in% names(table(PARL$leg_period_start))
			
			# the end dates
			table(FACT$faction_end)
	
	# now - although I know there are issues still! - lets merge MEME in, so we can compare totals!
	
	nrow(RESEBU)
	TEMP <- sqldf("
		  SELECT DISTINCT *
		  FROM ( 
					SELECT RESEBU.*,
					 MEME.party_id
			  FROM   RESEBU
			  LEFT JOIN MEME
					 ON RESEBU.pers_id       = MEME.pers_id
					AND RESEBU.res_entry_start_posoxctformat BETWEEN MEME.memep_start_posixct
										   AND MEME.memep_end_posixct
				)
		")
	nrow(TEMP)	# so we can see some duplicated where created (still, also after SELECT DISTINCT)
	
		# lets see for what cohorts?
		
		is.data.table(TEMP)
		setDT(TEMP)
		
		# ─────────────────────────────────────────────
		# 1.  Add a decade indicator based on the start date
		# ─────────────────────────────────────────────
		TEMP[, decade := floor(year(res_entry_start_posoxctformat) / 10) * 10]
		# optional: a nicer label, e.g. "1900s", "1910s", …
		TEMP[, decade_label := paste0(decade, "s")]
		
		table(TEMP$decade_label)
		
		head(TEMP)
		
		## one-liner, data.table
			dup_rows_by_decade <- TEMP[ ,
			  .(extra_rows = .N - uniqueN(res_entry_id)),
			  by = decade_label
			][order(decade_label)]

			dup_rows_by_decade
			
		# inspect the problematic cases (this is overseeable and should just be fixored manually)
			## duplicated IDs within each decade
			dup_ids <- TEMP[ ,
			  .N,                                 # count rows per ID
			  by = .(decade_label, res_entry_id)
			][N > 1]                              # keep only those with duplicates

			dup_ids[order(decade_label, -N)]
			
			
	# for example, lets troubleshoot the two one person we are low after the Nov 2006 elections
	
	#------------------------------------------------------------
	# Return the pers_id’s of everyone in parliament on a given day
	#------------------------------------------------------------
	whowashere <- function(localdate,
						   data   = RESEBU,
						   tz     = "UTC") {

	  # Accept either a character, Date, or POSIXct
	  if (inherits(localdate, "POSIXct")) {
		date_ct <- localdate
	  } else {
		date_ct <- as.POSIXct(localdate, tz = tz)
	  }

	  if (is.na(date_ct))
		stop("Could not convert 'localdate' to POSIXct. Check the format.")

	  # Fast interval filter and unique IDs
	  data[
		date_ct >= res_entry_start_posoxctformat &
		date_ct <= res_entry_end_posoxctformat,
		unique(pers_id)
	  ]
	}
	
	
	test <- whowashere("2007-12-13")
	length(test)
	
	