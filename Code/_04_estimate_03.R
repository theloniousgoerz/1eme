# =============================================================================
# Project:     1EME Submission
# Script:      Excess Mortality Estimate 3 (The number and rate of pandemic deaths in rural Maryland by race)
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
# Output:       Estimates/estimate_3
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