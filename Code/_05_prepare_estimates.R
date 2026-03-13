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
library(tinytable)
library(here)
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

e_1 = 
rbind(e_ky_1,e_md_1)  %>% mutate(
  int_n = sd * 1.96,
  int_r = sd_death_rate * 1.96,
  
  number_ci_low  = observed - int_n,
  number_ci_hi   = observed + int_n,
  rate_ci_low    = obs_death_rate - int_r,
  rate_ci_hi     = obs_death_rate + int_r,
  
  # Format: "value [low, high]"
  `Pandemic Death Rate (95 percent CI)`   = paste0(round(obs_death_rate, 2), 
                                                   " [", round(rate_ci_low, 2), 
                                                   ", ", round(rate_ci_hi, 2), "]"),
  
  `Pandemic Death Number (95 percent CI)` = paste0(round(observed, 2), 
                                                   " [", round(number_ci_low, 2), 
                                                   ", ", round(number_ci_hi, 2), "]")
)  %>%
  select(State,`Pandemic Death Rate (95 percent CI)`,`Pandemic Death Number (95 percent CI)`) %>%
  pivot_longer(cols = c(-State),
               names_to = " ",
               values_to = "Coefficient") %>% 
  pivot_wider(names_from = "State",
              values_from = "Coefficient") %>% 
  datasummary_df(title = "Estimand 1: The total number and rate of panedmic deaths in Maryland and Kentucky",
                       output = "tinytable",
                 align = "lcc")

# =============================================================================
# Estimate 2 

# ------------------------------
# MD 
e_2_1 = 
e_md_2 %>%
  mutate(
    int_n = sd * 1.96,
    int_r = sd_death_rate * 1.96,
    
    number_ci_low  = observed - int_n,
    number_ci_hi   = observed + int_n,
    rate_ci_low    = obs_death_rate - int_r,
    rate_ci_hi     = obs_death_rate + int_r,
    
    # Format: "value [low, high]"
    `Pandemic Death Rate (95 percent CI)`   = paste0(round(obs_death_rate, 2), 
                                     " [", round(rate_ci_low, 2), 
                                     ", ", round(rate_ci_hi, 2), "]"),
    
    `Pandemic Death Number (95 percent CI)` = paste0(round(observed, 2), 
                                     " [", round(number_ci_low, 2), 
                                     ", ", round(number_ci_hi, 2), "]")
  ) %>%
  select(
    County                  = county,
    `Pandemic Death Rate (95 percent CI)`,
    `Pandemic Death Number (95 percent CI)`
  ) %>% datasummary_df(title = "Estimand 2: The total number and rate of panedmic deaths in Maryland",
                       output = "tinytable",
                       align = "lcc")


# ------------------------------
# KY 
e_2_2 = e_ky_2 %>%
  mutate(
    int_n = sd * 1.96,
    int_r = sd_death_rate * 1.96,
    
    number_ci_low  = observed - int_n,
    number_ci_hi   = observed + int_n,
    rate_ci_low    = obs_death_rate - int_r,
    rate_ci_hi     = obs_death_rate + int_r,
    
    # Format: "value [low, high]"
    `Pandemic Death Rate (95 percent CI)`   = paste0(round(obs_death_rate, 2), 
                                                     " [", round(rate_ci_low, 2), 
                                                     ", ", round(rate_ci_hi, 2), "]"),
    
    `Pandemic Death Number (95 percent CI)` = paste0(round(observed, 2), 
                                                     " [", round(number_ci_low, 2), 
                                                     ", ", round(number_ci_hi, 2), "]")
  ) %>%
  select(
    County                  = county,
    `Pandemic Death Rate (95 percent CI)`,
    `Pandemic Death Number (95 percent CI)`
  )  %>% datasummary_df(title = "Estimand 2: The total number and rate of panedmic deaths in Kentucky",
                       output = "tinytable",
                       align = "lcc")


# ------------------------------
# MD
e_3 = e_md_3 %>%
  mutate(
    int_n = sd * 1.96,
    int_r = sd_death_rate * 1.96,
    
    number_ci_low  = observed - int_n,
    number_ci_hi   = observed + int_n,
    rate_ci_low    = obs_death_rate - int_r,
    rate_ci_hi     = obs_death_rate + int_r,
    
    # Format: "value [low, high]"
    `Pandemic Death Rate (95 percent CI)`   = paste0(round(obs_death_rate, 2), 
                                                     " [", round(rate_ci_low, 2), 
                                                     ", ", round(rate_ci_hi, 2), "]"),
    
    `Pandemic Death Number (95 percent CI)` = paste0(round(observed, 2), 
                                                     " [", round(number_ci_low, 2), 
                                                     ", ", round(number_ci_hi, 2), "]")
  ) %>%
  select(
    Race = race,
    `Pandemic Death Rate (95 percent CI)`,
    `Pandemic Death Number (95 percent CI)`
  ) %>% datasummary_df(title = "Estimand 3: The total number and rate of pandemic deaths by race in Maryland",
                       output = "tinytable",align = "lcc")

# ------------------------------
# Save 
# ------------------------------
e_1 %>% 
  save_tt(output = here("Submission","e1_table.tex"),overwrite = T)
e_2_1 %>% 
  save_tt(output = here("Submission","e2_1_table.tex"),overwrite = T)
e_2_2 %>% 
  save_tt(output = here("Submission","e2_2_table.tex"),overwrite = T)
e_3 %>% 
  save_tt(output = here("Submission","e3_table.tex"),overwrite = T)

