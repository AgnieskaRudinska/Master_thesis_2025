#-------------------------------------------------------------------------------
# This code performs association testing
#-------------------------------------------------------------------------------
rm(list = ls())

library(readxl)
library(openxlsx)
library(tibble)
library(tidyverse)
library(writexl)
library(psych)
library(broom)
library(purrr)

# dir
dir1 <- "../intermediate/"

source("functions.R")

dt <- read_excel(paste0(dir1, "02_unified_factors.xlsx"))

factors <- c("Sex", "SES", "Confession")
periods <- unique(dt$Period) %>% sort()

# dt without unknownt gender
dt_wout_unk <- dt %>%
  filter(is.na(Sex) | Sex %in% c("Male", "Female"))

#---------------------------- Dependency ---------------------------------------
# Chi-square test for categorical data
# (if expected value less than 5, fisher test)
chi_fisher_results <- chi_square_test_table(
  dt,
  c(
    "LEH", "(Osteo)periostitis",
    "Cribra orbitalia", "TB"
  ),
  factors,
  c("Total", periods)
)
chi_fisher_results_unk <- chi_square_test_table(
  dt_wout_unk,
  c(
    "LEH", "(Osteo)periostitis",
    "Cribra orbitalia", "TB"
  ),
  factors,
  c("Total", periods)
)

results <- list()
results[["Chi_Fisher results"]] <- chi_fisher_results
results[["Chi_Fisher results (unk)"]] <- chi_fisher_results_unk

write_xlsx(results, paste0(dir1, "results_summary.xlsx"))
