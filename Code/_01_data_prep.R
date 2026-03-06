# =============================================================================
# Project:     1EME Submission
# Script:      Data Prep
# Description: This script imports necessary data and preps it for estimation.
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
# Output:       [Output file(s) or objects produced]
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
library(zoo)
# =============================================================================
# Load Data 
# =============================================================================
# Kentucky Files 
# =============================================================================
## Numerator 
ky_month = read_csv("Data/Mortality data/Aggregate cause-specific files/KY_aggregate_month-cause - v1.csv")
## Denominator 
ky_denom = read_csv("Data/Denominator data/Census counts/KY_population - v1.csv")
# =============================================================================
# Maryland Files 
# =============================================================================
## Numerator 
md_month = read_csv("Data/Mortality data/Aggregate cause-specific files/MD_aggregate_month-cause - v1.csv")
## Denominator
md_denom = read_csv("Data/Denominator data/Census counts/MD_population - v1.csv")
# =============================================================================

# =============================================================================
# STEP 1: Standardize state names in numerator files
# =============================================================================

# Maryland: collapse "Maryland (except Baltimore City)" -> "Maryland"
md_month_clean <- md_month %>%
  mutate(state = "Maryland")

# Kentucky: adjust pattern below if your numerator uses a different variant
ky_month_clean <- ky_month %>%
  mutate(state = "Kentucky")


# =============================================================================
# STEP 2: Aggregate denominator — sum population across age and gender,
#         keeping state, county, year, and race
# =============================================================================

md_denom_agg <- md_denom %>%
  mutate(state = "Maryland") %>%
  group_by(state, county, year, race) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

ky_denom_agg <- ky_denom %>%
  mutate(state = "Kentucky") %>%
  group_by(state, county, year) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

# =============================================================================
# STEP 2B: Interpolate decadal census counts to annual estimates
#
# The denominator only has population at census years (e.g. 1900, 1910, 1920).
# We linearly interpolate within each state/county/race group to produce
# a population estimate for every year in the numerator's range.
#
# Approach:
#   1. Expand each group to a full annual sequence spanning the data range
#   2. Place observed decadal counts at their census years (NA elsewhere)
#   3. Use zoo::na.approx() for linear interpolation between census years
#   4. Use zoo::na.approx() with rule = 2 to fill any leading/trailing NAs
#      (i.e. flat extrapolation beyond the first/last census year)
# =============================================================================

# Define the full year range needed (driven by your numerator coverage)
year_min <- 1900
year_max <- 1940  # adjust to match the last year in your numerator data

interpolate_population_md <- function(denom_agg) {
  denom_agg %>%
    group_by(state, county, race) %>%
    complete(year = year_min:year_max) %>%
    arrange(year) %>%
    mutate(
      population = na.approx(population, x = year, na.rm = FALSE, rule = 2)
    ) %>%
    ungroup()
}

interpolate_population_ky <- function(denom_agg) {
  denom_agg %>%
    group_by(state, county) %>%
    complete(year = year_min:year_max) %>%
    arrange(year) %>%
    mutate(
      population = na.approx(population, x = year, na.rm = FALSE, rule = 2)
    ) %>%
    ungroup()
}

md_denom_interp <- interpolate_population_md(md_denom_agg)
ky_denom_interp <- interpolate_population_ky(ky_denom_agg)

# Quick check: confirm census years are unchanged and interpolated years filled
md_denom_interp %>%
  filter(county == first(county), race == first(race)) %>%
  print(n = 20)

ky_denom_interp %>%
  filter(county == first(county)) %>%
  print(n = 20)

# =============================================================================
# STEP 3: Merge numerator and denominator separately for each state
#
# NOTE: The numerator is at state/year/month/race/cause level.
#       The denominator is now at state/county/year/race level (annual).
#       Merging on state/year/race will expand the numerator across all
#       counties for that state/year/race combination.
#       Each death count row will be repeated once per county.
#       This is expected given the differing granularities.
# =============================================================================

md_merged <- md_month_clean %>%
  left_join(md_denom_interp, by = c("state", "year", "race"),
            relationship = "many-to-many")

ky_merged <- ky_month_clean %>%
  left_join(ky_denom_interp, by = c("state", "year"),
            relationship = "many-to-many")

# =============================================================================
# STEP 4: Classify Pandemic deaths
# =============================================================================

ky_merged %<>% 
  mutate(pandemic_death = case_when(
    cause %in% c("Influenza","Tuberculosis of the lungs","Bronchitis") ~ "pandemic_death",
    TRUE ~ "non_pandemic_death"
  )) 

md_merged %<>% 
  mutate(pandemic_death = case_when(
     cause %in% c("Influenza","Tuberculosis of the lungs","Bronchitis") ~ "pandemic_death",
    TRUE ~ "non_pandemic_death"
  )) 
# =============================================================================
# STEP 5: Write Results for Estimation
# =============================================================================
# Filter out missing population
md_merged %<>% filter(!is.na(population)) 
ky_merged %<>% filter(!is.na(population)) 


ky_denom_agg
# == Save 
write_csv(md_merged,"Data/_Cleaned/md_data.csv")
write_csv(ky_merged,"Data/_Cleaned/ky_data.csv")
