#-------------------------------------------------------------------------------
# This code create vizualisations of age at death intervals
#-------------------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(openxlsx)
library(cowplot)

# dir
dir1 <- "../data/"
dir2 <- "../intermediate/"
dir3 <- "../output/"

load(paste0(dir2, "predicted_values_test.Rdata"))

approx_age <- c("10+", "20+", "25+", "30+", "40+", "45+", "50+", "55+")
plots_list <- vector("list", length(approx_age))
for (i in seq_along(approx_age)) {
  aa <- approx_age[i]

  number_part <- as.numeric(substr(aa, 1, nchar(aa) - 1))

  dt_plot <- dt %>%
    filter(
      Age == aa | (`Lower interval` >= number_part & age_predicted == "Actual")
    )

  p <- ggplot(dt_plot, aes(
    x = `Lower interval`, xend = `Higher interval`,
    y = as.factor(ID), yend = as.factor(ID),
    color = age_predicted
  )) +
    geom_segment(size = 1) +
    labs(
      x = "Age at Death",
      y = "Individuals",
      color = "Age at Death Interval"
    ) +
    theme_classic() +
    ggtitle(aa) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 10),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      plot.title = element_text(size = 14),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )

  plots_list[[i]] <- p
}

# combine plots
legend_grob <- get_legend(plots_list[[2]] + theme(legend.position = "right"))

plots_no_legend <- lapply(plots_list, function(p) {
  p + theme(legend.position = "none")
})

plots_grid <- plot_grid(plotlist = plots_no_legend, ncol = 4)

final_plot <- plot_grid(plots_grid, legend_grob, ncol = 2, rel_widths = c(6, 1))

title_grob <- ggdraw() +
  draw_label("Actual Compared to Predicted Age at Death Intervals",
    fontface = "bold", x = 0.5, hjust = 0.5, size = 16
  )
final_plot_with_title <- plot_grid(title_grob, final_plot,
  ncol = 1,
  rel_heights = c(0.08, 1)
)

# summary of interval lengths
final_results[, "length_interval"] <- final_results[, "Higher interval"] -
  final_results[, "Lower interval"]
summary_table <- final_results %>%
  summarise(
    Count = n(), # Total number of values
    Mean = mean(length_interval, na.rm = TRUE),
    Median = median(length_interval, na.rm = TRUE),
    Min = min(length_interval, na.rm = TRUE),
    Max = max(length_interval, na.rm = TRUE),
    SD = sd(length_interval, na.rm = TRUE) # Standard deviation
  )
summary_table

# deleting bad predictions
final_dt <- dt %>%
  mutate(across(c(`Average age`, `Lower interval`, `Higher interval`), ~
                  ifelse(ID %in% c(1190, 33), NA, .)))

# save data
write.xlsx(final_dt, file = paste0(dir3, "04_final_data.xlsx"))
