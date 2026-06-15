# =============================================================================
# R051 Education Coverage Analysis
# Examines missingness of ICPSR college_v18 variable across Congress cohorts
# =============================================================================

library(dplyr)
library(ggplot2)

# Load BioGuide POLI + RESE
r052_dir <- "/home/tomas/projects/ProjectR052_DataFromExternalAPIs/USA/BioGuide/data_ready_for_IMPORT"
POLI <- read.csv(file.path(r052_dir, "POLI_import_ready.csv"), header = TRUE, skip = 1)
RESE <- read.csv(file.path(r052_dir, "RESE_parlmem_import_ready.csv"), header = TRUE, skip = 1)
PARL <- read.csv(file.path(r052_dir, "PARL_import_ready.csv"), header = TRUE, skip = 1)

# Enrich with ICPSR education data
icpsr_poli_file <- "/home/tomas/projects/ProjectR052_DataFromExternalAPIs/USA/ICPSR_Congressional/data_ready_for_IMPORT/POLI_import_ready.csv"
icpsr_poli <- read.csv(icpsr_poli_file, header = TRUE, skip = 1)
icpsr_edu <- icpsr_poli[, c("id_us_icpsr", "college_v18")]
POLI <- merge(POLI, icpsr_edu, by = "id_us_icpsr", all.x = TRUE)

# Focus on House only
RESE <- RESE |> filter(pf_instdomain == "LE-LH")
PARL <- PARL |> filter(assembly_abb == "HR")

# Parse PARL dates to get congress start years
PARL$start_year <- as.integer(sub(".*_(\\d{4})$", "\\1", PARL$parliament_id))

# For each RESE episode, extract the first parliament_id (primary congress)
RESE$primary_parliament <- sub(";.*", "", RESE$parliament_id)

# Join person-level education data
RESE_EDU <- RESE |>
  left_join(POLI |> select(pers_id, college_v18), by = "pers_id") |>
  left_join(PARL |> select(parliament_id, start_year),
            by = c("primary_parliament" = "parliament_id"))

# For each congress, count unique persons and education coverage
cohort_coverage <- RESE_EDU |>
  group_by(start_year) |>
  summarise(
    n_persons = n_distinct(pers_id),
    n_has_edu = n_distinct(pers_id[!is.na(college_v18)]),
    n_missing = n_distinct(pers_id[is.na(college_v18)]),
    pct_coverage = round(100 * n_has_edu / n_persons, 1),
    # Breakdown by category
    n_none = n_distinct(pers_id[college_v18 == "none" & !is.na(college_v18)]),
    n_state = n_distinct(pers_id[college_v18 == "state_university" & !is.na(college_v18)]),
    n_ivy = n_distinct(pers_id[college_v18 == "ivy_league" & !is.na(college_v18)]),
    n_other = n_distinct(pers_id[college_v18 == "other_college" & !is.na(college_v18)]),
    .groups = "drop"
  ) |>
  arrange(start_year)

cat("=== Education data coverage by Congress (start year) ===\n\n")
print(as.data.frame(cohort_coverage), row.names = FALSE)

cat(sprintf("\n=== Summary ===\n"))
cat(sprintf("Congresses with 100%% coverage: %d\n",
            sum(cohort_coverage$pct_coverage == 100)))
cat(sprintf("Congresses with >95%% coverage: %d\n",
            sum(cohort_coverage$pct_coverage > 95)))
cat(sprintf("First Congress with <100%% coverage: %d\n",
            min(cohort_coverage$start_year[cohort_coverage$pct_coverage < 100])))
cat(sprintf("Last Congress with >95%% coverage: %d\n",
            max(cohort_coverage$start_year[cohort_coverage$pct_coverage > 95])))

# Plot: coverage over time
p_coverage <- ggplot(cohort_coverage, aes(x = start_year)) +
  geom_line(aes(y = pct_coverage), color = "blue", linewidth = 1) +
  geom_point(aes(y = pct_coverage), color = "blue", size = 1.5) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
  scale_x_continuous(breaks = seq(1790, 2030, 10), minor_breaks = seq(1790, 2030, 5)) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
  labs(
    title = "ICPSR Education Data Coverage by Congress",
    subtitle = "% of House members with college_v18 data per congressional cohort",
    x = "Congress start year",
    y = "% with education data"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

ggsave("education/education_coverage_US.png", p_coverage, width = 12, height = 6, dpi = 150, bg = "white")
cat("\nPlot saved as education_coverage_US.png\n")

# Plot: education composition over time (stacked)
cohort_long <- cohort_coverage |>
  select(start_year, n_none, n_state, n_ivy, n_other, n_missing) |>
  tidyr::pivot_longer(cols = c(n_none, n_state, n_ivy, n_other, n_missing),
                      names_to = "category", values_to = "count") |>
  mutate(category = factor(category,
    levels = c("n_ivy", "n_other", "n_state", "n_none", "n_missing"),
    labels = c("Ivy League", "Other college", "State university", "No college", "Missing")))

p_composition <- ggplot(cohort_long, aes(x = start_year, y = count, fill = category)) +
  geom_area(position = "stack") +
  scale_fill_manual(values = c(
    "Ivy League" = "#1b9e77",
    "Other college" = "#7570b3",
    "State university" = "#d95f02",
    "No college" = "#e7298a",
    "Missing" = "gray80"
  )) +
  labs(
    title = "Education Composition of US House of Representatives Over Time",
    subtitle = "Based on ICPSR 7803 V18 (college attended, collapsed). Missing = no ICPSR data.",
    x = "Congress start year",
    y = "Number of House members",
    fill = "Education"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

ggsave("education/education_composition_US.png", p_composition, width = 14, height = 7, dpi = 150, bg = "white")
cat("Plot saved as education_composition_US.png\n")
