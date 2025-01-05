#-------------------------------------------------------------------------------
# This code used for survival curves estimation
#-------------------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(readxl)
library(survival)
library(survival)
library(survminer)
library(cowplot)

source("./00_options.R")
source("./functions.R")

# import data
dt <- read_excel(paste0(dir1, "04_final_data.xlsx"))
dt_wout_unk <- dt %>%
  filter(is.na(Sex) | Sex %in% c("Male", "Female")) %>%
  filter(is.na(`Average age`) | `Average age` >= 20)

pdf(file = paste0(dir2, "survival_curves_left.pdf"), width = 16, height = 12)

for (y in dependent) {
  gen_surv_obj <- Surv(
    time = dt_wout_unk[["Average age"]],
    event = as.numeric(dt_wout_unk[[y]]),
    type = "left"
  )
  gen_fit <- survfit(gen_surv_obj ~ 1, data = dt_wout_unk)

  gen_xlim <- c(min(gen_fit$time), max(gen_fit$time))

  gen_plot <- ggsurvplot(gen_fit,
    conf.int = TRUE,
    risk.table = TRUE,
    surv.median.line = "hv",
    break.time.by = 10,
    xlim = gen_xlim,
    title = paste0("Survival Curve for ", y),
    xlab = "Years", ylab = "Survival probability",
    legend = "none"
  )

  print(gen_plot)

  # Group-specific survival curves
  combined_plots <- list()
  for (group in variables) {
    group_surv_obj <- Surv(
      time = dt_wout_unk[["Average age"]],
      event = as.numeric(dt_wout_unk[[y]]),
      type = "left"
    )
    group_fit <- survfit(group_surv_obj ~ dt_wout_unk[[group]],
      data = dt_wout_unk
    )

    group_levels <- sort(unique(dt_wout_unk[[group]]))

    group_xlim <- c(min(group_fit$time), max(group_fit$time))

    group_plot <- ggsurvplot(group_fit,
      conf.int = TRUE,
      risk.table = TRUE,
      surv.median.line = "hv",
      break.time.by = 10,
      xlim = group_xlim,
      legend.title = group,
      legend.labs = group_levels,
      title = paste0(
        "Survival Curve for ", y,
        " by ", group
      ),
      xlab = "Years", ylab = "Survival probability"
    )

    print(group_plot)

    # for combined plot
    main_plot <- group_plot$plot
    risk_table <- group_plot$table

    combined_plot <- plot_grid(main_plot, risk_table,
      ncol = 1, rel_heights = c(3, 1)
    )
    combined_plots[[group]] <- combined_plot
  }
  final_combined_plot <- plot_grid(plotlist = combined_plots, ncol = 2)
  print(final_combined_plot)
}
dev.off()
