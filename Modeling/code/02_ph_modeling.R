#-------------------------------------------------------------------------------
# This code fits proportional hazard models
#-------------------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(readxl)
library(icenReg)
library(doParallel)
library(foreach)

source("./00_options.R")
source("./functions.R")

# import data
dt <- read_excel(paste0(dir1, "04_final_data.xlsx"))

# delete kids
dt_wout_unk <- dt %>%
  filter(is.na(Sex) | Sex %in% c("Male", "Female")) %>%
  filter(is.na(`Average age`) | `Average age` >= 20)

# delete Protestants as we have very small sample
dt_model <- dt_wout_unk %>%
  filter(Confession != "Protestants")

combinations <- generate_combinations(variables)

cox_results <- fit_cox_models(
  dt_model, setdiff(dependent, "TB"),
  variables, interval, combinations, cores
)

save(cox_results, file = paste0(dir2, "predicted_values.Rdata"))

final <- fix_names(cox_results)

write.csv(final,
  file = paste0(dir2, "all_ph_model_14_15.csv"),
  row.names = FALSE
)
