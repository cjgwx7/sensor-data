require(MASS)

cross_val <- function(df, var, fit_class, fit_formula) {

  ### Create training/testing partitions
  train_df_list <- list()
  test_df_list <- list()

  fold_names <- unname(unlist(unique(df[, var])))

  for (i in fold_names) {

    train_df <- subset(df, get(var) != i)
    test_df <- subset(df, get(var) == i)

    train_df_list[[i]] <- train_df
    test_df_list[[i]] <- test_df

  }

  ### Fit models on training data
  train_fit_list <- list()

  for (i in fold_names) {

    train_df <- train_df_list[[i]]

    if (fit_class == "lm") {

      train_fit <- lm(fit_formula, data = train_df)

    } else if (fit_class == "robust") {

      train_fit <- rlm(fit_formula, data = train_df)

    }

    train_fit_list[[i]] <- train_fit

  }

  ### Predict using test data
  test_eval_df_list <- list()

  for (i in fold_names) {

    train_fit <- train_fit_list[[i]]
    test_eval_df <- test_df_list[[i]]
    test_eval_df$y_hat <- predict(train_fit, test_eval_df)

    test_eval_df_list[[i]] <- test_eval_df

  }

  ### Calculate metrics for each fold
  test_results_list <- list()

  for (i in fold_names) {

    train_fit <- train_fit_list[[i]]
    test_eval_df <- test_eval_df_list[[i]]
    y <- unname(unlist(test_eval_df[, ncol(test_eval_df) - 1]))
    y_hat <- unname(unlist(test_eval_df[, ncol(test_eval_df)]))

    res <- y - y_hat
    sse <- sum(res^2)
    mse <- mean(res^2)
    rmse <- sqrt(mse)
    sst <- (nrow(test_eval_df) - 1) * var(y)
    r2 <- 1 - (sse / sst)
    r2_adj_num <- (1 - r2) * (nrow(test_eval_df) - 1)
    r2_adj_den <- nrow(test_eval_df) - length(coef(train_fit)) - 1
    r2_adj <- 1 - (r2_adj_num / r2_adj_den)

    results <- data.frame(`HoldoutFold` = i,
                          SSE = sse,
                          SST = sst,
                          R2 = r2,
                          R2_adj = r2_adj,
                          MSE = mse,
                          RMSE = rmse,
                          fit = Reduce(paste, deparse(fit_formula)),
                          model = fit_class)

    test_results_list[[i]] <- results

  }

  eval_metrics <- do.call("rbind", test_results_list)

  return_list <- list("training_folds" = train_df_list,
                      "training_fits" = train_fit_list,
                      "testing_folds" = test_df_list,
                      "testing_eval_df" = test_eval_df_list,
                      "eval_metrics" = eval_metrics)
  return(return_list)

}