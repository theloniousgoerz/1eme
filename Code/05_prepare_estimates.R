# =============================================================================
# Project:     1EME Submission
# Script:      Display Excess Mortality Estimates
# Description: 
# =============================================================================
# Author(s):   Thelonious Goerz
#              Alvaro Padilla-Pozo
# Affiliation: Cornell University
# =============================================================================
# Created:     03-11-26
# Modified:    [YYYY-MM-DD]
# Version:     1.0
# =============================================================================
# Input:        Estimates/
# Output:       Estimates/tables
# =============================================================================
# Notes:
#   - 
# =============================================================================
# Install 
# Packages 
rm(list = ls())
library(readr)
library(tidyverse)
library(magrittr)
library(excessmort)
library(modelsummary)
# =============================================================================
# Load Data 

e_md_1 = read_rds("Estimates/md_estimate_1.rds")
e_md_2 = read_rds("Estimates/md_estimate_2.rds")
e_ky_1 = read_rds("Estimates/ky_estimate_1.rds")
e_ky_2 = read_rds("Estimates/ky_estimate_2.rds")
e_md_3 = read_rds("Estimates/estimate_3.rds")


# =============================================================================
# Estimate 1 (Prepare)
e_ky_1 %<>% mutate(State = "Kentucky") %>% filter(pandemic_death == "pandemic_death")
e_md_1 %<>% mutate(State = "Maryland") %>% filter(pandemic_death == "pandemic_death")

rbind(e_ky_1,e_md_1) %>%
  mutate(int_n = (sd*1.96),
         int_r = (sd_death_rate*1.96),
         
    number_ci_low = observed - int_n,
    number_ci_hi =  observed + int_n,
    
    rate_ci_low = obs_death_rate- int_r,
    rate_ci_hi =  obs_death_rate+ int_r,
    ) %>% 
  select(
    State,
    `Pandemic Death Rate` =obs_death_rate, 
    `Pandemic Death Rate (95 CI Low)` =  rate_ci_low,
    `Pandemic Death Rate (95 CI High)` = rate_ci_hi,
    `Pandemic Death Number` = observed,
    `Pandemic Death Number (95 CI Low)` =  number_ci_low,
    `Pandemic Death Number (95 CI High)` = number_ci_hi
  ) %>% datasummary_df()

# =============================================================================
# Estimate 2 

# ------------------------------
# MD 
e_md_2 %>%
mutate(int_n = (sd*1.96),
       int_r = (sd_death_rate*1.96),
       
       number_ci_low = observed - int_n,
       number_ci_hi =  observed + int_n,
       
       rate_ci_low = obs_death_rate- int_r,
       rate_ci_hi =  obs_death_rate+ int_r,
) %>% 
  select(
    County = county,
    `Pandemic Death Rate` =obs_death_rate, 
    `Pandemic Death Rate (95 CI Low)` =  rate_ci_low,
    `Pandemic Death Rate (95 CI High)` = rate_ci_hi,
    `Pandemic Death Number` = observed,
    `Pandemic Death Number (95 CI Low)` =  number_ci_low,
    `Pandemic Death Number (95 CI High)` = number_ci_hi
  ) %>% datasummary_df()


# ------------------------------
# KY 
e_ky_2 %>%
  mutate(int_n = (sd*1.96),
         int_r = (sd_death_rate*1.96),
         
         number_ci_low = observed - int_n,
         number_ci_hi =  observed + int_n,
         
         rate_ci_low = obs_death_rate- int_r,
         rate_ci_hi =  obs_death_rate+ int_r,
  ) %>% 
  select(
    County = county,
    `Pandemic Death Rate` =obs_death_rate, 
    `Pandemic Death Rate (95 CI Low)` =  rate_ci_low,
    `Pandemic Death Rate (95 CI High)` = rate_ci_hi,
    `Pandemic Death Number` = observed,
    `Pandemic Death Number (95 CI Low)` =  number_ci_low,
    `Pandemic Death Number (95 CI High)` = number_ci_hi
  ) %>% datasummary_df()


# ------------------------------
# KY 
e_md_3 %>%
  mutate(int_n = (sd*1.96),
         int_r = (sd_death_rate*1.96),
         
         number_ci_low = observed - int_n,
         number_ci_hi =  observed + int_n,
         
         rate_ci_low = obs_death_rate- int_r,
         rate_ci_hi =  obs_death_rate+ int_r,
  ) %>% 
  select(
    Race = race,
    `Pandemic Death Rate` =obs_death_rate, 
    `Pandemic Death Rate (95 CI Low)` =  rate_ci_low,
    `Pandemic Death Rate (95 CI High)` = rate_ci_hi,
    `Pandemic Death Number` = observed,
    `Pandemic Death Number (95 CI Low)` =  number_ci_low,
    `Pandemic Death Number (95 CI High)` = number_ci_hi
  ) %>% datasummary_df()



