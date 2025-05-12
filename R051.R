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
				
				
				
				# import and inspect party membership eppisodes
				# MEME = read.csv("PCC/MEME.csv", header = TRUE, sep = ";")
				# summary(MEME)
				# names(MEME)
				
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

library(ggplot2)

# 1) compute your break dates
month_breaks <- seq(
  from = as.POSIXct("2000-01-01"),
  to   = as.POSIXct("2005-12-01"),
  by   = "1 month"
)
year_breaks <- seq(
  from = as.POSIXct("2000-01-01"),
  to   = as.POSIXct("2005-01-01"),
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

  # Month labels on bottom:
  scale_x_datetime(
    limits = c(as.POSIXct("2000-01-01"), as.POSIXct("2005-12-31")),
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
    breaks = seq(125, 160, by = 1)
  ) +

  labs(
    title = "Daily Number of Politicians in Parliament (2000–2005)",
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
	start_date <- as.POSIXct("2003-06-01", tz = "UTC")
	end_date   <- as.POSIXct("2003-07-01", tz = "UTC")

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