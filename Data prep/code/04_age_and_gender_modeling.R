#-------------------------------------------------------------------------------
# This code is used for missing age at death imputation
#-------------------------------------------------------------------------------
rm(list = ls())

library(dplyr)
library(readxl)
library(tidyr)
library(mice)
library(parallel)
library(future.apply)

# dir
dir1 <- "../data/"
dir2 <- "../intermediate/"

source("./functions.R")

# import data
dt <- read_excel(paste0(dir2, "02_unified_factors.xlsx"))

# prep for imputation
dt_predict <- dt %>%
  mutate(Age = ifelse(grepl(">", Age),
    paste0(gsub(">", "", Age), "+"), Age
  )) %>%
  mutate(Age = ifelse(grepl("<", Age),
    paste0(gsub("<", "", Age), "-"), Age
  )) %>%
  mutate(
    `Can Predict` =
      ifelse(is.na(`d13C‰ (bone)`) & is.na(`d13C‰ (dentine)`) &
               is.na(`d15N‰ (bone)`) & is.na(`d15N‰ (dentine)`) &
               is.na(`D+K humerus avg.`) & is.na(`D+K femur avg.`) &
               is.na(`Caries Freq`) & is.na(`AMTL Freq`) & is.na(Injuries),
             FALSE,
             TRUE))

# creating column with injuries count, as it can be good measure for age
injuries <- c(
  "Ribs", "Skull", "Spine", "Clavicle", "Humerus",
  "Forearm", "Hip bone", "Femur", "Tibia + Fibula"
)
dt_predict <- dt_predict %>%
  rowwise() %>%
  mutate(
    `Injuries count` = if (all(is.na(c_across(all_of(injuries))))) {
      NA
    } else if (sum(as.numeric(c_across(all_of(injuries))), na.rm = TRUE) == 0) {
      0
    } else {
      sum(as.numeric(c_across(all_of(injuries))) == 1, na.rm = TRUE)
    }
  ) %>%
  ungroup()


# divide into two groups, with fully known age interval and approximate age
to_predict <- dt_predict %>%
  filter(is.na(`Average age`)) %>%
  pull(Age) %>%
  unique()

# table with individuals we need to model, except if 60+, there is
# a lack of individuals
known_aprox_age_dt <- dt_predict %>%
  filter(Age %in% grep("\\+|\\-", to_predict, value = TRUE) & Age != "60+") %>%
  filter(`Can Predict` == TRUE)

table(known_aprox_age_dt$Age)

# data that will be use to fit the model
full_cases_dt <- dt_predict %>%
  filter(!is.na(`Average age`))

# regressions
predictors <- c(
  "d13C‰ (bone)", "d13C‰ (dentine)", "d15N‰ (bone)",
  "d15N‰ (dentine)", "D+K humerus avg.", "D+K femur avg.",
  "Caries Freq", "AMTL Freq", "Injuries count"
)

# as all combination of available predictors will be used,
# we need to use parallelization to speed up the process
num_cores <- detectCores() - 1
plan(multisession, workers = num_cores)


combined_res_list <-
  future_lapply(seq_len(nrow(known_aprox_age_dt)), function(row) {
    process_row(row, known_aprox_age_dt, full_cases_dt)
  })
combined_res <- do.call(rbind, combined_res_list)
plan(sequential)

# Formatting final results
final_results <- combined_res %>%
  group_by(across(-c(
    `Average age`, `Lower interval`,
    `Higher interval`, combination
  ))) %>%
  summarise(
    `Average age` = mean(`Average age`, na.rm = TRUE),
    `Lower interval` = mean(`Lower interval`, na.rm = TRUE),
    `Higher interval` = mean(`Higher interval`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(-`Can Predict`, -`Injuries count`) %>%
  mutate(age_predicted = "Predicted")

# Adding predicted data to the data set
dt <- dt %>%
  anti_join(
    final_results,
    setdiff(
      names(dt),
      c("Average age", "Lower interval", "Higher interval")
    )
  ) %>%
  bind_rows(final_results) %>%
  mutate(age_predicted = ifelse(is.na(age_predicted), "Actual", age_predicted))

# save data
save(final_results, dt, file = paste0(dir2, "predicted_values_test.Rdata"))
