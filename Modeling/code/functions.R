#' Generate All Combinations of Variables
#'
#' This function generates all possible combinations of the input variables
#' and returns them as character formulas separated by " + ".
#' @param variables A character vector of variable names for which combinations
#' need to be generated.
#' @return A character vector containing all possible combinations of the
#' variables as formula strings.
#'
generate_combinations <- function(variables) {
  all_combinations <- c()
  for (i in seq(1, length(variables))) {
    combinations <- combn(variables, i, simplify = FALSE)
    formulas <- sapply(combinations, function(x) paste(x, collapse = " + "))
    all_combinations <- c(all_combinations, formulas)
  }

  return(all_combinations)
}

#' Fit Proportional Hazards Models for Interval-Censored Data
#'
#' @param data A data frame containing the input dataset.
#' @param dependent A character vector of dependent variable names to be modeled
#' @param variables A character vector of independent variable names
#' used for creating combinations.
#' @param interval A character vector of length two specifying the column names
#' for interval boundaries
#' @param combinations A character vector of formula strings representing
#' predictor combinations
#' @param cores An integer specifying the number of CPU cores to use
#' for parallel processing.
#'
#' @return A data frame containing model evaluation results,
#' including parameter estimates, log-likelihood, AIC, BIC
fit_cox_models <- function(data, dependent, variables, interval,
                           combinations, cores) {
  results <- list()
  for (y in dependent) {
    # re-code interval to include information about event
    data_interval <- data %>%
      mutate(
        !!sym(interval[1]) := ifelse(!!sym(y) == 0, `Average age`, 0),
        !!sym(interval[2]) := ifelse(!!sym(y) == 0,
          Inf,
          `Average age`
        )
      )

    for (i in seq(1, length(combinations))) {
      print(paste0("Evaluating model ", i, " for: ", y))

      model_data <- data_interval %>%
        select(
          all_of(interval),
          all_of(strsplit(gsub(
            "`", "",
            combinations[i]
          ), " \\+ ")[[1]])
        ) %>%
        na.omit()

      formula <- paste0(
        "cbind(`", interval[1], "`, `",
        interval[2], "`) ~ ", combinations[i]
      )

      myCluster <- makeCluster(cores - 1)
      registerDoParallel(myCluster)
      fit_ph <- ic_sp(as.formula(formula),
        model = "ph",
        bs_samples = 500, useMCores = TRUE,
        data = model_data
      )
      stopCluster(myCluster)

      save(fit_ph, file = paste0(dir2, "cox_model/", y, "_model_", i, ".Rdata"))

      model_summary <- summary(fit_ph)

      # Compute AIC, BIC
      n <- nrow(model_data)
      k <- length(coef(fit_ph))
      log_likelihood <- fit_ph$llk
      aic <- 2 * k - 2 * log_likelihood
      bic <- log(n) * k - 2 * log_likelihood

      # Create a data frame with all the useful information
      model_df <- data.frame(
        Dependent = y,
        Model_ID = paste0("model_", i)
      ) %>%
        bind_cols(as.data.frame(model_summary$summaryParameters)) %>%
        bind_cols(data.frame(
          Final_LogLikelihood = fit_ph$llk,
          Iterations = fit_ph$iterations,
          AIC = aic,
          BIC = bic
        ))
      model_df$Predictor <- rownames(model_df)
      rownames(model_df) <- NULL

      save(model_df, file = paste0(dir2, y, "_model_", i, ".Rdata"))
      # Append the results to the list
      results[[length(results) + 1]] <- model_df
    }
  }

  final_results <- do.call(rbind, results) %>%
    pivot_longer(!c(Dependent, Model_ID, Predictor),
      names_to = "Statistics",
      values_to = "value"
    ) %>%
    mutate(Statistics = paste0(Dependent, " - ", Statistics)) %>%
    select(-Dependent) %>%
    pivot_wider(
      names_from = "Statistics",
      values_from = "value"
    )

  return(final_results)
}


#' Fix Predictor Names in a Data Frame
#'
#' This function standardizes and reformats the names of predictors
#' for better readability and consistency.
#'
#' @param data A data frame containing a `Predictor` column that includes
#' predictor names to be standardized.
#' @return A data frame with the same structure as the input but with updated,
#' user-friendly names in the `Predictor` column.
#'
fix_names <- function(data) {
  final <- data %>%
    mutate(Predictor = case_when(
      Predictor == "(Intercept)" ~ "Intercept",
      Predictor == "`Average age`" ~ "Average age",
      Predictor == "SexMale" ~ "Sex - Male",
      Predictor == "SexFemale" ~ "Sex - Female",
      Predictor == "SESElite and clergy" ~
        "SES - Elite and clergy",
      Predictor == "ConfessionProtestants" ~
        "Confession - Protestantism",
      Predictor == "ConfessionOrthodox" ~
        "Confession - Orthodox",
      Predictor == "Period16 th." ~
        "Period - 16 th",
      Predictor == "Period17-18 th." ~
        "Period - 17-18 th",
      Predictor == "`d13C‰ (bone)`" ~
        "d13C‰ (bone)",
      Predictor == "`d13C‰ (dentine)`" ~
        "d13C‰ (dentine)",
      Predictor == "`d15N‰ (bone)`" ~
        "d15N‰ (bone)",
      Predictor == "`d15N‰ (dentine)`" ~
        "d15N‰ (dentine)",
      TRUE ~ Predictor
    ))
  return(final)
}
