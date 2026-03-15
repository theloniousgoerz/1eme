# =============================================================================
# Project:     1EME Submission
# Script:      Display Excess Mortality Estimates
# Description: 
# =============================================================================
# Author(s):   Thelonious Goerz
#              Alvaro Padilla-Pozo
# Affiliation: Cornell University
# =============================================================================
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
) 

e_1 %>% select(observed,number_ci_low,number_ci_hi,obs_death_rate,rate_ci_low,rate_ci_hi,State) %>% 
  write_csv(.,here("Submission","e1.csv"))

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
  ) 

e_2_1 %>% select(observed,number_ci_low,number_ci_hi,obs_death_rate,rate_ci_low,rate_ci_hi,county) %>% 
  write_csv(.,here("Submission","e2_1.csv"))


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
  ) 
e_2_2 %>% select(observed,number_ci_low,number_ci_hi,obs_death_rate,rate_ci_low,rate_ci_hi,county) %>% 
  write_csv(.,here("Submission","e2_2.csv"))


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
  ) 

e_3 %>% select(observed,number_ci_low,number_ci_hi,obs_death_rate,rate_ci_low,rate_ci_hi,race) %>% 
  write_csv(.,here("Submission","e3.csv"))


