# =============================================================================
# Project:     1EME Submission
# Script:      Excess Mortality Estimate 2 (The number and rate of pandemic deaths in each county (138 stable county units across two states))
# Description: This script produces the necessary estimates.
# =============================================================================
# Author(s):   Thelonious Goerz
#              Alvaro Padilla-Pozo
# Affiliation: Cornell University
# =============================================================================
# Created:     02-21-21
# Modified:    [YYYY-MM-DD]
# Version:     1.0
# =============================================================================
# Input:        Data/Mortality data; Denominator data
# Output:       Estimates/estimate_2
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
# =============================================================================

md <- read_csv("Data/_Cleaned/md_county_data.csv")
ky <- read_csv("Data/_Cleaned/ky_county_data.csv")

# =============================================================================
# Pandemic interval
# =============================================================================

pandemic_start    <- as.Date("1918-03-01")
pandemic_end      <- as.Date("1920-05-01")
pandemic_interval <- list(pandemic = c(pandemic_start, pandemic_end))
exclude_dates     <- seq(pandemic_start, pandemic_end, by = "month")

# =============================================================================
# Month
# =============================================================================
md %<>% mutate(month = as.integer(str_sub(date,6,7)))
ky %<>% mutate(month = as.integer(str_sub(date,6,7)))
# =============================================================================
# Helper: run excess model for a single county stratum
# Returns the excess summary row, or NULL if counts are too low to model
# =============================================================================
run_excess <- function(df, group_vars) {
  
  # explicitly reference global variables inside function scope
  excl   <- get("exclude_dates",   envir = .GlobalEnv)
  p_start <- get("pandemic_start", envir = .GlobalEnv)
  p_end   <- get("pandemic_end",   envir = .GlobalEnv)
  p_int   <- get("pandemic_interval", envir = .GlobalEnv)
  
  counts <- df %>%
    group_by(year, month) %>%
    summarise(
      outcome    = sum(deaths,     na.rm = TRUE),
      population = sum(population, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    arrange(date) %>%
    select(date, outcome, population)
  
  if (nrow(counts) == 0) return(NULL)
  
  counts_expect <- tryCatch(
    compute_expected(
      counts         = counts,
      exclude        = excl,
      include.trend  = TRUE,
      harmonics      = 2,
      frequency      = 12,
      weekday.effect = FALSE,
      verbose        = FALSE
    ),
    error = function(e) { message("compute_expected failed: ", e$message); NULL }
  )
  
  if (is.null(counts_expect)) return(NULL)
  
  excess <- tryCatch(
    excess_model(
      counts    = counts_expect,
      start     = p_start,
      end       = p_end,
      intervals = p_int,
      verbose   = FALSE
    ),
    error = function(e) { message("excess_model failed: ", e$message); NULL }
  )
  
  if (is.null(excess)) return(NULL)
  
  excess$excess %>% bind_cols(group_vars)
}
# =============================================================================
# Kentucky: excess mortality by county
# =============================================================================

ky_county_results <- ky %>%
  group_by(county) %>%
  group_split() %>%
  map(function(df) {
    grp <- df %>% distinct(county)
    message("KY — county: ", grp$county)
    run_excess(df, grp)
  }) %>%
  compact() %>%
  bind_rows()

# =============================================================================
# Maryland: excess mortality by county
# =============================================================================

md_county_results <- md %>%
  group_by(county) %>%
  group_split() %>%
  map(function(df) {
    grp <- df %>% distinct(county)
    message("MD — county: ", grp$county)
    run_excess(df, grp)
  }) %>%
  compact() %>%
  bind_rows()

# =============================================================================
# Final outputs
# =============================================================================
# Estimate 1
write_rds(ky_county_results,"Estimates/ky_estimate_2.rds")
# Estimate 2
write_rds(md_county_results,"Estimates/md_estimate_2.rds")

ky %>% distinct(county)
