# =============================================================================
# Project:     1EME Submission
# Script:      Data Prep
# Description: This script imports necessary data and preps it for estimation.
# =============================================================================
# Packages 
rm(list = ls())
# install.packages("excessmort")

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
ky_county = read_csv("Data/Mortality data/Granular death count files/KY_deaths - v1.csv")
## Denominator 
ky_denom = read_csv("Data/Denominator data/Census counts/KY_population - v1.csv")
# =============================================================================
# Maryland Files 
# =============================================================================
## Numerator 
md_month = read_csv("Data/Mortality data/Aggregate cause-specific files/MD_aggregate_month-cause - v1.csv")
md_county = read_csv("Data/Mortality data/Granular death count files/MD_deaths - v1.csv")
## Denominator
md_denom = read_csv("Data/Denominator data/Census counts/MD_population - v1.csv")
# =============================================================================

# =============================================================================
# STEP 1: Standardize state names in numerator files
# =============================================================================

# Maryland: collapse "Maryland (except Baltimore City)" -> "Maryland"
md_month_clean <- md_month %>%
  mutate(state = "Maryland")

md_county_clean <- md_county %>%
  mutate(state = "Maryland")

# Kentucky: adjust pattern below if your numerator uses a different variant
ky_month_clean <- ky_month %>%
  mutate(state = "Kentucky")

ky_county_clean <- 
ky_county %>%
  mutate(state = "Kentucky")

ky_denom %<>%
  mutate(
    # Rename LaRue to R capitalize. 
    county = ifelse(county == "Larue County","LaRue County",county)
  )

# =============================================================================
# STEP 2A: Aggregate denominator data
# =============================================================================

# --- Maryland: State-level aggregation 
md_denom_agg <- md_denom %>%
  mutate(state = "Maryland") %>%
  group_by(state, county, year, race) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

# --- Maryland: County-state-level aggregation 
md_denom_agg_county <- md_denom %>%
  mutate(state = "Maryland") %>%
  group_by(state, county, year, race) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")
# Note: For Maryland, county is already retained so this mirrors md_denom_agg.

# --- Kentucky: State-level aggregation
ky_denom_agg <- ky_denom %>%
  mutate(state = "Kentucky") %>%
  group_by(state, county, year) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

# --- Kentucky: County-state-level aggregation 
ky_denom_agg_county <- ky_denom %>%
  mutate(state = "Kentucky") %>%
  group_by(state, county, year) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")
# =============================================================================
# STEP 2B: Interpolate decadal census counts to annual estimates
# =============================================================================

year_min <- 1900
year_max <- 1940  # adjust to match the last year in your numerator data

# --- Maryland interpolation (groups on state/county/race) ---
interpolate_population_md <- function(denom_agg) {
  denom_agg %>%
    group_by(state, county, race) %>%
    complete(year = year_min:year_max) %>%
    arrange(year) %>%
    mutate(
      population = na.approx(population, x = year, na.rm = T, rule = 2)
    ) %>%
    ungroup()
}

# --- Kentucky interpolation (groups on state/county, no race) ---
interpolate_population_ky <- function(denom_agg) {
  denom_agg %>%
    group_by(state, county) %>%
    complete(year = year_min:year_max) %>%
    arrange(year) %>%
    mutate(
      population = na.approx(population, x = year, na.rm = T, rule = 2)
    ) %>%
    ungroup()
}

# State-level interpolated denominators (original)
md_denom_interp       <- interpolate_population_md(md_denom_agg)
ky_denom_interp       <- interpolate_population_ky(ky_denom_agg)

# County-level interpolated denominators (new)
md_denom_interp_county <- interpolate_population_md(md_denom_agg_county)
ky_denom_interp_county <- interpolate_population_ky(ky_denom_agg_county)

# =============================================================================
# STEP 3: Merge numerator and denominator separately for each state
# =============================================================================

md_merged <- md_month_clean %>%
  left_join(md_denom_interp, by = c("state", "year", "race"),
            relationship = "many-to-many")

ky_merged <- ky_month_clean %>%
  left_join(ky_denom_interp, by = c("state", "year"),
            relationship = "many-to-many")

md_merged_county <- 
  md_county_clean %>%
  mutate(year = as.integer(str_sub(date, 1, 4))) %>%
  group_by(date, county, year, race) %>%
    # need to add "County: to county names for county_clean. 
  summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  # fix county names 
  mutate(county = paste0(county," County")) %>%
  left_join(md_denom_interp_county, by = c("county", "year", "race")) %>% 
  ungroup() %>% 
  group_by(date, county, year) %>%
  # need to add "County: to county names for county_clean. 
  summarise(deaths = sum(deaths, na.rm = TRUE), 
            population = sum(population,na.rm = TRUE),.groups = "drop")

ky_merged_county <- 
  ky_county_clean %>%
  mutate(year = as.integer(str_sub(date, 1, 4))) %>%
  group_by(date, county, year) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  left_join(ky_denom_interp_county, by = c("county", "year"),
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
# Write Results for Estimation
# =============================================================================
# Filter out missing population
 md_merged %<>% filter(!is.na(population)) 
 ky_merged %<>% filter(!is.na(population)) 
 md_merged_county %<>% filter(!is.na(population)) 
 ky_merged_county %<>% filter(!is.na(population))


# == Save 
write_csv(md_merged,"Data/_Cleaned/md_data.csv")
write_csv(ky_merged,"Data/_Cleaned/ky_data.csv")

write_csv(md_merged_county,"Data/_Cleaned/md_county_data.csv")
write_csv(ky_merged_county,"Data/_Cleaned/ky_county_data.csv")
