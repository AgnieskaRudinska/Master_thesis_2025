#' This function generates all possible combinations of a given set of variables
#'
#' @param vars a vector of variables to generate combinations from
#' @return a list of vectors, where each vector is a unique combination
#'
generate_combinations <- function(vars) {
  unlist(
    lapply(
      1:length(vars),
      function(i) combn(vars, i, simplify = FALSE)
    ),
    recursive = FALSE
  )
}


#' This function goes through data frame with the values were approximate age
#' is known to generate imputed age values based on conditional age cutoffs and
#' available predictors.
#'
#' @param row An integer representing the index of the row
#' in `imput_data` to be processed.
#' @param imput_data A data frame with the data to be imputed
#' @param full_data A data frame with fully known age interval
#'
process_row <- function(row, imput_data, full_data) {
  cut_off <- imput_data$Age[row]
  number_part <- as.numeric(substr(cut_off, 1, nchar(cut_off) - 1))
  sign_part <- substring(cut_off, nchar(cut_off))

  to_predict_dt <- imput_data[row, ]
  id <- to_predict_dt$ID
  available_data <- !is.na(to_predict_dt[, predictors])
  var_to_use <- colnames(available_data)[available_data == TRUE]

  # Generate unique combinations for the current row
  combinations <- generate_combinations(var_to_use)

  # Define Age_cut_off conditionally based on sign
  data <- full_data %>%
    mutate(Age_cut_off = factor(ifelse(
      `Lower interval` >= number_part & sign_part == "+",
      1,
      ifelse(`Lower interval` <= number_part & sign_part == "-", 1, 0)
    ))) %>%
    bind_rows(to_predict_dt %>% mutate(Age_cut_off = as.factor(1)))

  predict_res <- list()

  # Loop over each combination
  for (j in seq(1, length(combinations))) {
    comb <- combinations[[j]]

    # Filter and select data for the specific combination
    temp_dt_l <- data %>%
      select(`Lower interval`, all_of(comb), Age_cut_off) %>%
      filter(if_all(-c(`Lower interval`), ~ !is.na(.)))
    temp_dt_h <- data %>%
      select(`Higher interval`, all_of(comb), Age_cut_off) %>%
      filter(if_all(-c(`Higher interval`), ~ !is.na(.)))
    names(temp_dt_l) <- make.names(names(temp_dt_l), unique = TRUE)
    names(temp_dt_h) <- make.names(names(temp_dt_h), unique = TRUE)

    if (nrow(temp_dt_h) >= 10) {
      # Define the method for mice and perform imputation
      method <- c("norm", rep("", ncol(temp_dt_h) - 1))

      # Lower interval
      imputed_data_l <- mice(temp_dt_l, method = method, m = 10, seed = 123)
      imputed_values_l <-
        as.numeric(unlist(imputed_data_l$imp[["Lower.interval"]]))
      mean_imputed_l <- mean(imputed_values_l, na.rm = TRUE)

      # Higher interval
      imputed_data_h <- mice(temp_dt_h, method = method, m = 10, seed = 123)
      imputed_values_h <-
        as.numeric(unlist(imputed_data_h$imp[["Higher.interval"]]))
      mean_imputed_h <- mean(imputed_values_h, na.rm = TRUE)

      # add imputed value to original data
      imputed <- to_predict_dt %>%
        mutate(
          `Average age` = (mean_imputed_l + mean_imputed_h) / 2,
          `Lower interval` = mean_imputed_l,
          `Higher interval` = mean_imputed_h,
          combination = paste(comb, collapse = " - ")
        )

      # Append result for each combination
      predict_res[[j]] <- imputed
    } else {
      imputed <- to_predict_dt %>%
        mutate(
          `Average age` = NA,
          `Lower interval` = NA,
          `Higher interval` = NA,
          combination = paste(comb, collapse = " - ")
        )
      # Add NA results if data is insufficient
      predict_res[[j]] <- imputed
    }
  }
  # Combine all results for the current row
  return(do.call(rbind, predict_res))
}


#' This function generates a summary table of chi-square and Fisher's exact test
#' results across multiple factors and periods
#'
#' @param data A data frame that needs to be analyzed.
#' @param dependent Variables the needs to be tested
#' @param factors Groups to analyze
#' @param periods A character vector of periods to use
#'
#' The output table includes frequency, counts, and p-values for each dependent
#' variable across specified factors and periods.
#'
#' @return A table summarizing the results.
#
chi_square_test_table <- function(data, dependent, factors, periods) {
  combined_results <- c()
  for (d in dependent) {
    general_statistics <- c()
    periods_results <- c()
    for (p in periods) {
      if (p == "Total") {
        period_data <- data
      } else {
        period_data <- data %>%
          filter(Period == p)
      }
      gs_temp <- period_data %>%
        pull(!!d) %>%
        na.omit() %>%
        {
          tibble(
            N = length(.),
            M = sum(. == "1")
          )
        } %>%
        mutate(freq. = M / N) %>%
        mutate(
          dependent = d,
          factor = "Total",
          level = "Total"
        ) %>%
        pivot_longer(!c(dependent, factor, level),
          names_to = "statistics",
          values_to = "value"
        ) %>%
        mutate(
          statistics = paste0(p, " - ", statistics),
          value = as.character(value)
        )

      # calculate p-value beetween for total
      # p_value (between) - chi or fisher
      between_test_data_tot <- data %>%
        select(Period, !!rlang::sym(d)) %>%
        na.omit()

      if (nrow(between_test_data_tot) > 0 &&
            length(unique(between_test_data_tot$Period)) > 1) {
        between_table_tot <- table(between_test_data_tot[[d]],
                                   between_test_data_tot[["Period"]])
        between_test_tot <- chisq.test(between_table_tot)
        between_expected_tot <- between_test_tot$expected

        if (any(between_expected_tot < 5)) {
          between_test_tot <- fisher.test(between_table_tot, workspace = 1e8)
          between_p_val_tot <- paste0(between_test_tot$p.value, " *")
        } else {
          between_p_val_tot <- paste0(between_test_tot$p.value)
        }
      } else {
        between_p_val_tot <- NA
      }

      gs_temp <- gs_temp %>%
        bind_rows(tibble(
          dependent = d,
          factor = "Total",
          level = "Total",
          statistics = paste0(p, " - P-value (between)"),
          value = between_p_val_tot
        ))

      general_statistics <- bind_rows(general_statistics, gs_temp)

      factor_statistics <- c()
      for (f in factors) {
        levels <- unique(na.omit(period_data[[f]])) %>% sort()

        levels_statistics <- c()
        for (l in levels) {
          ls_temp <- period_data %>%
            filter(!!rlang::sym(f) == l) %>%
            pull(!!d) %>%
            na.omit() %>%
            {
              tibble(
                N = as.character(length(.)),
                M = as.character(sum(. == "1"))
              )
            } %>%
            mutate(freq. = as.character(as.numeric(M) / as.numeric(N))) %>%
            mutate(
              dependent = d,
              factor = f,
              level = l
            ) %>%
            pivot_longer(!c(dependent, factor, level),
              names_to = "statistics",
              values_to = "value"
            ) %>%
            mutate(statistics = paste0(p, " - ", statistics))

          levels_statistics <- bind_rows(levels_statistics, ls_temp)

          # calculate P-value (between) across periods
          between_test_data <- data %>%
            filter(!!rlang::sym(f) == l) %>%
            select(Period, !!rlang::sym(d)) %>%
            na.omit()

          if (nrow(between_test_data) > 0 &&
                length(unique(between_test_data$Period)) > 1) {
            between_table <- table(
              between_test_data[[d]],
              between_test_data[["Period"]]
            )
            between_test <- chisq.test(between_table)
            between_expected <- between_test$expected

            if (any(between_expected < 5)) {
              between_test <- fisher.test(between_table, workspace = 1e8)
              between_p_val <- paste0(between_test$p.value, " *")
            } else {
              between_p_val <- paste0(between_test$p.value)
            }
          } else {
            between_p_val <- NA
          }

          levels_statistics <- levels_statistics %>%
            bind_rows(tibble(
              dependent = d,
              factor = f,
              level = l,
              statistics = paste0(p, " - P-value (between)"),
              value = between_p_val
            ))
        }

        # p_value (within) - chi or fisher
        within_test_data <- period_data %>%
          select(!!rlang::sym(d), !!rlang::sym(f)) %>%
          na.omit()

        if (nrow(within_test_data) > 0 &&
              length(unique(within_test_data[[f]])) > 1) {
          within_table <- table(within_test_data[[d]], within_test_data[[f]])
          within_test <- chisq.test(within_table)
          within_expected <- within_test$expected

          if (any(within_expected < 5)) {
            within_test <- fisher.test(within_table,
              workspace = 1e8,
              simulate.p.value = TRUE, B = 1e5
            )
            within_p_val <- paste0(within_test$p.value, "*")
          } else {
            within_p_val <- paste0(within_test$p.value)
          }
        } else {
          within_p_val <- NA # If not enough levels, assign NA
        }

        fs_temp <- levels_statistics %>%
          bind_rows(tibble(
            dependent = d,
            factor = f,
            level = "P - value (within)",
            statistics = paste0(p, " - ", "N"),
            value = within_p_val
          ))

        factor_statistics <- bind_rows(factor_statistics, fs_temp)
      }

      periods_results <- bind_rows(periods_results, factor_statistics)
    }
    combined_results <- bind_rows(combined_results, general_statistics) %>%
      bind_rows(periods_results)
  }

  results <- combined_results %>%
    pivot_wider(
      names_from = "statistics",
      values_from = "value"
    )

  cols <- grep("between", colnames(results), value = TRUE)

  results <- results %>%
    select(-all_of(cols[1:3])) %>%
    rename(`P-value (between)` = !!cols[4])

  return(results)
}
