# =============================================================================
# Project:     1EME Submission
# Script:      Excess Mortality Estimate 1 (The total number and rate of pandemic deaths in each state (Kentucky and rural Maryland))
# Description: This script produces the necessary estimates.
# =============================================================================
# Author(s):   Thelonious Goerz
#              Alvaro Padilla-Pozo
# Affiliation: Cornell University
# =============================================================================
# Created:     02-21-21
# Version:     1.0
# =============================================================================
# Input:        Data/Mortality data; Denominator data
# Output:       Estimates/estimate_1
# =============================================================================
# Notes:
#   - 
# =============================================================================
# Install 
# install.packages("excessmort")
# Packages 
rm(list = ls())
library(readr)
library(tidyverse)
library(magrittr)
library(excessmort)
library(modelsummary)
# =============================================================================
# Load Data 
# March 1918-May 1920
md = read_csv("Data/_Cleaned/md_data.csv")
ky = read_csv("Data/_Cleaned/ky_data.csv")
# =============================================================================
#
#
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Estimate Kentucky 


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# =============================================================================
# STEP 1: Define the pandemic interval to exclude from baseline fitting
#         and later estimate excess mortality over
# =============================================================================
pandemic_start <- as.Date("1918-03-01")
pandemic_end   <- as.Date("1920-05-01")

pandemic_interval <- list(
  pandemic = c(pandemic_start, pandemic_end)
)

# Sequence of all dates within the pandemic window to pass to exclude argument
exclude_dates <- seq(pandemic_start, pandemic_end, by = "month")

# =============================================================================
# STEP 2: Prepare counts table — aggregate to state level by year/month
# =============================================================================

ky_counts <-
  ky %>%
  filter(pandemic_death == "pandemic_death") %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         outcome = deaths) %>%
  arrange(date) %>%
  select(date, outcome, population)

# =============================================================================
# STEP 3: Compute expected counts
#
# The model fits an overdispersed Poisson with:
#   - A slow time trend (spline)
#   - A seasonal (Fourier harmonic) effect
# Pandemic dates are excluded from model fitting.
# =============================================================================

ky_counts_expect <- compute_expected(
  counts          = ky_counts,
  exclude         = exclude_dates,
  include.trend   = T,          # slow year-to-year trend
  harmonics       = 2,             # seasonal harmonics
  frequency       = 12,            # monthly data
  weekday.effect  = FALSE,         # not applicable for monthly
  keep.components = TRUE,
  verbose         = TRUE
)

# =============================================================================
# STEP 4: Fit the excess mortality model over the pandemic interval
# =============================================================================

# =============================================================================
# STEP 5: Cause-specific excess mortality — stratified by pandemic_death

# =============================================================================

cause_groups <- unique(ky$pandemic_death)

ky_excess_by_cause <- map(cause_groups, function(c) {
  
  message("Processing cause group: ", c)
  
  counts_c <- ky %>%
    filter(pandemic_death == c) %>%
    group_by(year, month) %>%
    summarise(
      outcome    = sum(deaths, na.rm = TRUE),
      population = sum(population, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    arrange(date) %>%
    select(date, outcome, population)
  
  if (mean(counts_c$outcome, na.rm = TRUE) < 1) {
    message("  Skipping — average monthly counts < 1 for cause group: ", c)
    return(NULL)
  }
  
  counts_c_expect <- compute_expected(
    counts         = counts_c,
    exclude        = exclude_dates,
    include.trend  = TRUE,
    harmonics      = 2,
    frequency      = 12,
    weekday.effect = FALSE,
    verbose        = FALSE
  )
  
  excess_c <- excess_model(
    counts    = counts_c_expect,
    start     = pandemic_start,
    end       = pandemic_end,
    intervals = pandemic_interval,
    verbose   = FALSE
  )
  
  excess_c$excess %>% mutate(pandemic_death = c)
  
}) %>%
  set_names(cause_groups) %>%
  compact()                # drop NULLs from skipped groups

# Combine results into one table
ky_excess_cause_summary <- bind_rows(ky_excess_by_cause)
datasummary_df(ky_excess_cause_summary)



# =============================================================================
# STEP 6: Flag pandemic vs non-pandemic rows in the counts object
#         (mirrors your pandemic_death variable in the raw data)
# =============================================================================

ky_counts_expect <- ky_counts_expect %>%
  mutate(
    pandemic_period = ifelse(date >= pandemic_start & date <= pandemic_end,
                             "pandemic", "non_pandemic")
  )
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Estimate Maryland 


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# =============================================================================
# STEP 2: Prepare counts table — aggregate to state level by year/month
# =============================================================================

md_counts <-
  md %>%
  filter(pandemic_death == "pandemic_death") %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         outcome = deaths) %>%
  arrange(date) %>%
  select(date, outcome, population)



# =============================================================================
# STEP 3: Compute expected counts
#
# The model fits an overdispersed Poisson with:
#   - A slow time trend (spline)
#   - A seasonal (Fourier harmonic) effect
# Pandemic dates are excluded from model fitting.
# =============================================================================

md_counts_expect <- compute_expected(
  counts          = md_counts,
  exclude         = exclude_dates,
  include.trend   = TRUE,          # slow year-to-year trend
  harmonics       = 2,             # seasonal harmonics
  frequency       = 12,            # monthly data
  weekday.effect  = FALSE,         # not applicable for monthly
  keep.components = TRUE,
  verbose         = TRUE
)

# =============================================================================
# STEP 4: Fit the excess mortality model over the pandemic interval
# =============================================================================


cause_groups <- unique(ky$pandemic_death)

md_excess_by_cause <- map(cause_groups, function(c) {
  
  message("Processing cause group: ", c)
  
  counts_c <- md %>%
    filter(pandemic_death == c) %>%
    group_by(year, month) %>%
    summarise(
      outcome    = sum(deaths, na.rm = TRUE),
      population = sum(population, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    arrange(date) %>%
    select(date, outcome, population)
  
  # Skip if average counts are very low (model will warn)
  if (mean(counts_c$outcome, na.rm = TRUE) < 1) {
    message("  Skipping — average monthly counts < 1 for cause group: ", c)
    return(NULL)
  }
  
  counts_c_expect <- compute_expected(
    counts         = counts_c,
    exclude        = exclude_dates,
    include.trend  = TRUE,
    harmonics      = 2,
    frequency      = 12,
    weekday.effect = FALSE,
    verbose        = FALSE
  )
  
  excess_c <- excess_model(
    counts    = counts_c_expect,
    start     = pandemic_start,
    end       = pandemic_end,
    intervals = pandemic_interval,
    verbose   = FALSE
  )
  
  excess_c$excess %>% mutate(pandemic_death = c)
  
}) %>%
  set_names(cause_groups) %>%
  compact()                # drop NULLs from skipped groups

# Combine results into one table
md_excess_cause_summary <- bind_rows(md_excess_by_cause)
datasummary_df(md_excess_cause_summary)


# =============================================================================
# STEP 5: Flag pandemic vs non-pandemic rows in the counts object
#         (mirrors your pandemic_death variable in the raw data)
# =============================================================================

md_counts_expect <- md_counts_expect %>%
  mutate(
    pandemic_period = ifelse(date >= pandemic_start & date <= pandemic_end,
                             "pandemic", "non_pandemic")
  )


# =============================================================================
# Finalize Estimates 

# =============================================================================
# Estimate 1
write_rds(ky_excess_cause_summary,"Estimates/ky_estimate_1.rds")

# Estimate 2
write_rds(md_excess_cause_summary,"Estimates/md_estimate_1.rds")




