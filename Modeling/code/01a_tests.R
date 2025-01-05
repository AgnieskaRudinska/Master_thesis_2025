#-------------------------------------------------------------------------------
# This code performs log-rank tests
#-------------------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(readxl)
library(interval)

source("./00_options.R")
source("./functions.R")

# import data
dt <- read_excel(paste0(dir1, "04_final_data.xlsx"))
dt_wout_unk <- dt %>%
  filter(is.na(Sex) | Sex %in% c("Male", "Female")) %>%
  filter(is.na(`Average age`) | `Average age` >= 20)

final_results <- data.frame() # Initialize outside the loop

for (y in dependent) {
  for (group in variables) {
    data_interval <- dt_wout_unk %>%
      mutate(
        !!sym(interval[1]) := ifelse(!!sym(y) == 0, `Average age`, 0),
        !!sym(interval[2]) := ifelse(!!sym(y) == 0,
          Inf,
          `Average age`
        )
      )

    group_surv_obj <- Surv(data_interval[[interval[1]]],
      data_interval[[interval[2]]],
      type = "interval2"
    )

    permut_test <- ictest(group_surv_obj ~ data_interval[[group]],
      data = data_interval,
      scores = "wmw"
    )
    mult_imput <- ictest(group_surv_obj ~ data_interval[[group]],
      data = data_interval,
      scores = "wmw",
      method = "wsr.HLY",
      mcontrol = mControl(nwsr = 100)
    )
    finkel <- ictest(group_surv_obj ~ data_interval[[group]],
      data = data_interval,
      scores = "logrank2"
    )

    temp <- data.frame(
      dependent = y,
      group = group,
      wp = permut_test$p.value,
      mi = mult_imput$p.value,
      fp = finkel$p.value
    )

    # Append the results
    final_results <- bind_rows(final_results, temp)
  }
}

# Save the final results
write.csv(final_results,
  paste0(dir2, "final_comparison_results.csv"),
  row.names = FALSE
)
