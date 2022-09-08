single_variable <- function(df, y, x_pos) {

  Y <- unname(unlist(df[, y])) # nolint
  X_df <- df[, x_pos] # nolint

  names_X <- names(X_df) # nolint

  fit_list <- list()

  for (i in seq_len(ncol(X_df))) {

    X <- unname(unlist(X_df[, i])) # nolint

    fit <- glm(Y ~ X)
    fit_summary <- summary(fit)
    fit_coef <- coef(fit_summary)

    fit_df <- data.frame(DependentVariable = y,
                         IndependentVariable = names_X[i],
                         BetaEstimate = fit_coef[2, 1],
                         SE = fit_coef[2, 2],
                         t_value = fit_coef[2, 3],
                         WaldsP_Value = fit_coef[2, 4])

    fit_list[[i]] <- fit_df

  }

  results_df <- dplyr::bind_rows(fit_list)
  return(results_df)

}