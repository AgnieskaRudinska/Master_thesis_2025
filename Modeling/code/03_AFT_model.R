rm(list = ls())

library(DPpackage)
library(tidyverse)
library(survival)
library(censReg)
library(readxl)
library(coda)

source("./00_options.R")
source("./functions.R")

# import data
dt <- read_excel(paste0(dir1, "04_final_data.xlsx"))

# combinations
combinations <- generate_combinations(variables)

# delete kids
dt_wout_unk <- dt %>%
  filter(is.na(Sex) | Sex %in% c("Male", "Female")) %>%
  filter(is.na(`Average age`) | `Average age` >= 20)

# delete Protestants as we have very small sample
dt_model <- dt_wout_unk %>%
  filter(Confession != "Protestants")

# combinations
combinations <- generate_combinations(variables)

results <- list()
pdf(file = paste0(dir2, "AFT_plots.pdf"), width = 16, height = 12)
for (y in setdiff(dependent, "TB")) {
  # transform intervals
  data_interval <- dt_model %>%
    mutate(
      !!sym(interval[1]) := ifelse(!!sym(y) == 0, `Average age`, 0),
      !!sym(interval[2]) := ifelse(!!sym(y) == 0,
        -999,
        `Average age`
      )
    )

  # Prior information
  prior <- list(
    alpha = 20, m0 = 2.3, s0 = 10^2, tau1 = 0.1, tau2 = 0.1,
    beta0 = rep(0, 1), Sbeta0 = diag(10^2, 1)
  )

  # MCMC Configuration
  mcmc <- list(
    nburn = 15000, nsave = 15000, nskip = 10,
    ndisplay = 1000, tune = 0.25
  )

  i <- 1
  for (comb in combinations) {
    model_dt <- data_interval %>%
      select(
        all_of(interval),
        all_of(strsplit(gsub(
          "`", "",
          comb
        ), " \\+ ")[[1]])
      ) %>%
      na.omit()

    age <- as.matrix(model_dt[, interval])

    # drivers
    dummy_data <- model.matrix(as.formula(paste0("~ ", comb)),
      data = model_dt
    )
    dummy_data <- as.data.frame(dummy_data)

    num_covariates <- ncol(dummy_data) - 1 # Exclude the intercept
    current_prior <- prior
    current_prior$beta0 <- rep(0, num_covariates)
    current_prior$Sbeta0 <- diag(10^2, num_covariates)

    variables <- setdiff(colnames(dummy_data), "(Intercept)")
    formula <-
      as.formula(paste0(
        "age ~ ",
        paste(paste0("dummy_data$`", variables, "`"),
          collapse = " + "
        )
      ))

    # fiting the model
    fit <- DPsurvint(
      formula = formula,
      prior = current_prior, mcmc = mcmc,
      state = NULL, status = TRUE
    )

    save(fit, file = paste0(dir2, "aft_models/", y, "_model_", i, ".Rdata"))
    i <- i + 1

    # get information about fit
    model_summary <- summary(fit)

    # plot
    p <- plot(fit, ask = FALSE)
    print(p)

    theta_samples <- fit$save.state$thetasave
    colnames(theta_samples) <- c(
      variables,
      "Intercept (mu)", "Scale (sigma^2)",
      "Number of Clusters (ncluster)", "Alpha"
    )

    posterior_mean <- colMeans(theta_samples)
    posterior_sd <- apply(theta_samples, 2, sd)

    theta_mcmc <- as.mcmc(theta_samples)
    hpd_intervals <- HPDinterval(theta_mcmc, prob = 0.95)
    credible_intervals <- as.matrix(hpd_intervals)

    posterior_probability <- apply(theta_samples, 2, function(x) {
      max(mean(x > 0), mean(x < 0))
    })
    significance <- 1 - posterior_probability

    standard_errors <- posterior_sd / sqrt(nrow(theta_samples))

    # combine into a data table
    library(data.table)
    results_temp <- data.table(
      Dependent = y,
      Model_ID = paste0("model_", i),
      Parameter = colnames(theta_samples),
      Mean = posterior_mean,
      SD = posterior_sd,
      `Standard Error` = standard_errors,
      `Lower 95% CI` = credible_intervals[, 1],
      `Upper 95% CI` = credible_intervals[, 2],
      `Contour Probability` = significance
    )

    results[[length(results) + 1]] <- results_temp
  }
}
dev.off()

final_results <- do.call(rbind, results)

write.csv(final_results,
  file = paste0(dir2, "all_AFT_model.csv"), row.names = FALSE
)
