single_variable_negbin_ar1 <- function(df, y, x_pos, covariate, int, random_effect) { # nolint

  if (require(glmmTMB) == FALSE) {

    stop("Install package 'glmmTMB' in order to utilize function")

  }

  if (is.data.frame(df) == FALSE) {

    stop("The argument 'df' is not in proper format of data.frame")

  }

  COLUMN_NAMES <- names(df) # nolint
  COLUMN_POS <- c(1:ncol(df)) # nolint

  if (is.character(y) == FALSE | (y %in% COLUMN_NAMES) == FALSE | length(y) > 1) {

    stop("The argument 'y' is not a character string, is not a potential dependent variable, or is greater than length 1")

  }

  if (is.numeric(x_pos) == FALSE | all(x_pos %in% COLUMN_POS) == FALSE) {

    stop("The argument 'x_pos' is not numeric or is not within the positional bounds of the data frame")

  }

  if (is.null(covariate) == TRUE) {



  } else if (is.character(covariate) == FALSE | (covariate %in% COLUMN_NAMES) == FALSE | length(covariate) > 1) {

    stop("The argument 'covariate' is not a character string, is not a potential covariate, or is greater than length 1")

  }

  if (is.logical(int) == FALSE | length(int) > 1) {

    stop("The argument 'int' is not logical or is greater than length 1")

  }

  if (is.null(random_effect) == TRUE) {

    

  } else if (is.character(random_effect) == FALSE | (random_effect %in% COLUMN_NAMES) == FALSE | length(random_effect) > 1) {

    stop("The argument 'random_effect' is not a character string, is not a potential random effect, or is greater than length 1")

  }

  if (length(covariate) == 1 & int == TRUE & length(random_effect) == 1) {

    ### Extract Y
    Y <- unname(unlist(df[, y])) # nolint

    ### Extract X variables as data.frame
    X_df <- df[, x_pos] # nolint
    names_X <- names(X_df) # nolint

    ### Extract covariate
    cov <- unname(unlist(df[, covariate]))

    ### Extract random effect
    re <- unname(unlist(df[, random_effect]))

    ### Extract offset
    off <- unname(unlist(df[, "Inventory_Lag0"]))
    
    ### Extract time
    ar1_time <- factor(unname(unlist(df[, "Days_Lag0"])))
    
    ###Extract random effect ar(1)
    ar1_re <- unname(unlist(df[, "Site_Room_Turn"]))

    fit_list <- list()
    model_list <- list()
    vcov_list <- list()

    for (i in seq_len(ncol(X_df))) {

      X <- unname(unlist(X_df[, i])) # nolint

      fit <- glmmTMB(Y ~ X * cov + (1 | re) + ar1(ar1_time + 0 | ar1_re), offset = log(off), family = nbinom2())
      fit_summary <- summary(fit)
      fit_coef <- coef(fit_summary)$cond
      variance <- VarCorr(fit)
      variance <- variance[["cond"]][["re"]]
      variance <- unname(unlist(variance[1,1]))

      fit_df <- data.frame(DependentVariable = y,
                           IndependentVariable = names_X[i],
                           ConfoundingVariable = covariate,
                           Interaction = TRUE,
                           Beta_X = tryCatch({fit_coef[2, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_X = tryCatch({fit_coef[2, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_X = tryCatch({fit_coef[2, 3]}, error = function(e) {return(NA)}), # nolint
                           P_X = tryCatch({fit_coef[2, 4]}, error = function(e) {return(NA)}), # nolint
                           Intercept = tryCatch({fit_coef[1, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_Intercept = tryCatch({fit_coef[1, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_Intercept = tryCatch({fit_coef[1, 3]}, error = function(e) {return(NA)}), # nolint
                           P_Intercept = tryCatch({fit_coef[1, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_C2 = tryCatch({fit_coef[3, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_C2 = tryCatch({fit_coef[3, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_C2 = tryCatch({fit_coef[3, 3]}, error = function(e) {return(NA)}), # nolint
                           P_C2 = tryCatch({fit_coef[3, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_C3 = tryCatch({fit_coef[4, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_C3 = tryCatch({fit_coef[4, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_C3 = tryCatch({fit_coef[4, 3]}, error = function(e) {return(NA)}), # nolint
                           P_C3 = tryCatch({fit_coef[4, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_C4 = tryCatch({fit_coef[5, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_C4 = tryCatch({fit_coef[5, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_C4 = tryCatch({fit_coef[5, 3]}, error = function(e) {return(NA)}), # nolint
                           P_C4 = tryCatch({fit_coef[5, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_I1 = tryCatch({fit_coef[6, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_I1 = tryCatch({fit_coef[6, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_I1 = tryCatch({fit_coef[6, 3]}, error = function(e) {return(NA)}), # nolint
                           P_I1 = tryCatch({fit_coef[6, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_I2 = tryCatch({fit_coef[7, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_I2 = tryCatch({fit_coef[7, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_I2 = tryCatch({fit_coef[7, 3]}, error = function(e) {return(NA)}), # nolint
                           P_I2 = tryCatch({fit_coef[7, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_I3 = tryCatch({fit_coef[8, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_I3 = tryCatch({fit_coef[8, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_I3 = tryCatch({fit_coef[8, 3]}, error = function(e) {return(NA)}), # nolint
                           P_I3 = tryCatch({fit_coef[8, 4]}, error = function(e) {return(NA)}), # nolint
                           RandomEffect = random_effect,
                           RandomEffectVariance = variance,
                           AIC = AIC(fit),
                           BIC = BIC(fit))

      fit_list[[i]] <- fit_df
      model_list[[i]] <- fit
      vcov_list[[i]] <- vcov(fit)$cond
      print(names_X[i])

    }

  } else if (length(covariate) == 1 & int == FALSE & length(random_effect) == 1) {

    ### Extract Y
    Y <- unname(unlist(df[, y])) # nolint
    
    ### Extract X variables as data.frame
    X_df <- df[, x_pos] # nolint
    names_X <- names(X_df) # nolint
    
    ### Extract covariate
    cov <- unname(unlist(df[, covariate]))
    
    ### Extract random effect
    re <- unname(unlist(df[, random_effect]))
    
    ### Extract offset
    off <- unname(unlist(df[, "Inventory_Lag0"]))
    
    ### Extract time
    ar1_time <- factor(unname(unlist(df[, "Days_Lag0"])))
    
    ###Extract random effect ar(1)
    ar1_re <- unname(unlist(df[, "Site_Room_Turn"]))
    
    fit_list <- list()
    model_list <- list()
    vcov_list <- list()

    for (i in seq_len(ncol(X_df))) {

      X <- unname(unlist(X_df[, i])) # nolint

      fit <- glmmTMB(Y ~ X + cov + (1 | re) + ar1(ar1_time + 0 | ar1_re), offset = log(off), family = nbinom2())
      fit_summary <- summary(fit)
      fit_coef <- coef(fit_summary)$cond
      variance <- VarCorr(fit)
      variance <- variance[["cond"]][["re"]]
      variance <- unname(unlist(variance[1, 1]))

      fit_df <- data.frame(DependentVariable = y,
                           IndependentVariable = names_X[i],
                           ConfoundingVariable = covariate,
                           Interaction = FALSE,
                           Beta_X = tryCatch({fit_coef[2, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_X = tryCatch({fit_coef[2, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_X = tryCatch({fit_coef[2, 3]}, error = function(e) {return(NA)}), # nolint
                           P_X = tryCatch({fit_coef[2, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_C = tryCatch({fit_coef[3, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_C = tryCatch({fit_coef[3, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_C = tryCatch({fit_coef[3, 3]}, error = function(e) {return(NA)}), # nolint
                           P_C = tryCatch({fit_coef[3, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_I = NA,
                           SE_I = NA,
                           Z_I = NA,
                           P_I = NA,
                           RandomEffect = random_effect,
                           RandomEffectVariance = variance,
                           AIC = AIC(fit),
                           BIC = BIC(fit))

      fit_list[[i]] <- fit_df
      model_list[[i]] <- fit
      vcov_list[[i]] <- vcov(fit)$cond
      print(names_X[i])
      
    }

  } else if (length(covariate) == 1 & int == TRUE & length(random_effect) != 1) {

    ### Extract Y
    Y <- unname(unlist(df[, y])) # nolint
    
    ### Extract X variables as data.frame
    X_df <- df[, x_pos] # nolint
    names_X <- names(X_df) # nolint
    
    ### Extract covariate
    cov <- unname(unlist(df[, covariate]))
    
    ### Extract random effect
    re <- unname(unlist(df[, random_effect]))
    
    ### Extract offset
    off <- unname(unlist(df[, "Inventory_Lag0"]))
    
    ### Extract time
    ar1_time <- factor(unname(unlist(df[, "Days_Lag0"])))
    
    ###Extract random effect ar(1)
    ar1_re <- unname(unlist(df[, "Site_Room_Turn"]))
    
    fit_list <- list()

    for (i in seq_len(ncol(X_df))) {

      X <- unname(unlist(X_df[, i])) # nolint

      fit <- glmmTMB(Y ~ X * cov + ar1(ar1_time + 0 | ar1_re), offset = log(off), family = nbinom2())
      fit_summary <- summary(fit)
      fit_coef <- coef(fit_summary)$cond

      fit_df <- data.frame(DependentVariable = y,
                           IndependentVariable = names_X[i],
                           ConfoundingVariable = covariate,
                           Interaction = TRUE,
                           Beta_X = tryCatch({fit_coef[2, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_X = tryCatch({fit_coef[2, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_X = tryCatch({fit_coef[2, 3]}, error = function(e) {return(NA)}), # nolint
                           P_X = tryCatch({fit_coef[2, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_C = tryCatch({fit_coef[3, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_C = tryCatch({fit_coef[3, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_C = tryCatch({fit_coef[3, 3]}, error = function(e) {return(NA)}), # nolint
                           P_C = tryCatch({fit_coef[3, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_I = tryCatch({fit_coef[4, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_I = tryCatch({fit_coef[4, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_I = tryCatch({fit_coef[4, 3]}, error = function(e) {return(NA)}), # nolint
                           P_I = tryCatch({fit_coef[4, 4]}, error = function(e) {return(NA)}), # nolint
                           RandomEffect = NA,
                           RandomEffectVariance = NA,
                           AIC = AIC(fit),
                           BIC = BIC(fit))

      fit_list[[i]] <- fit_df

    }

  } else if (length(covariate) != 1 & int == FALSE & length(random_effect) == 1) {

    ### Extract Y
    Y <- unname(unlist(df[, y])) # nolint
    
    ### Extract X variables as data.frame
    X_df <- df[, x_pos] # nolint
    names_X <- names(X_df) # nolint
    
    ### Extract random effect
    re <- unname(unlist(df[, random_effect]))
    
    ### Extract offset
    off <- unname(unlist(df[, "Inventory_Lag0"]))
    
    ### Extract time
    ar1_time <- factor(unname(unlist(df[, "Days_Lag0"])))
    
    ###Extract random effect ar(1)
    ar1_re <- unname(unlist(df[, "Site_Room_Turn"]))
    
    fit_list <- list()

    for (i in seq_len(ncol(X_df))) {

      X <- unname(unlist(X_df[, i])) # nolint

      fit <- glmmTMB(Y ~ X + (1 | re) + ar1(ar1_time + 0 | ar1_re), offset = log(off), family = nbinom2())
      fit_summary <- summary(fit)
      fit_coef <- coef(fit_summary)$cond
      variance <- VarCorr(fit)
      variance <- variance[["cond"]][["re"]]
      variance <- unname(unlist(variance[1,1]))

      fit_df <- data.frame(DependentVariable = y,
                           IndependentVariable = names_X[i],
                           ConfoundingVariable = NA,
                           Interaction = NA,
                           Beta_X = tryCatch({fit_coef[2, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_X = tryCatch({fit_coef[2, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_X = tryCatch({fit_coef[2, 3]}, error = function(e) {return(NA)}), # nolint
                           P_X = tryCatch({fit_coef[2, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_C = NA,
                           SE_C = NA,
                           Z_C = NA,
                           P_C = NA,
                           Beta_I = NA,
                           SE_I = NA,
                           Z_I = NA,
                           P_I = NA,
                           RandomEffect = random_effect,
                           RandomEffectVariance = variance,
                           AIC = AIC(fit),
                           BIC = BIC(fit))

      fit_list[[i]] <- fit_df

    }

  } else if (length(covariate) == 1 & int == FALSE & length(random_effect) != 1) {

    ### Extract Y
    Y <- unname(unlist(df[, y])) # nolint
    
    ### Extract X variables as data.frame
    X_df <- df[, x_pos] # nolint
    names_X <- names(X_df) # nolint
    
    ### Extract covariate
    cov <- unname(unlist(df[, covariate]))
    
    ### Extract offset
    off <- unname(unlist(df[, "Inventory_Lag0"]))
    
    ### Extract time
    ar1_time <- factor(unname(unlist(df[, "Days_Lag0"])))
    
    ###Extract random effect ar(1)
    ar1_re <- unname(unlist(df[, "Site_Room_Turn"]))
    
    fit_list <- list()

    for (i in seq_len(ncol(X_df))) {

      X <- unname(unlist(X_df[, i])) # nolint

      fit <- glmmTMB(Y ~ X + cov + ar1(ar1_time + 0 | ar1_re), offset = log(off), family = nbinom2())
      fit_summary <- summary(fit)
      fit_coef <- coef(fit_summary)$cond

      fit_df <- data.frame(DependentVariable = y,
                           IndependentVariable = names_X[i],
                           ConfoundingVariable = covariate,
                           Interaction = FALSE,
                           Beta_X = tryCatch({fit_coef[2, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_X = tryCatch({fit_coef[2, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_X = tryCatch({fit_coef[2, 3]}, error = function(e) {return(NA)}), # nolint
                           P_X = tryCatch({fit_coef[2, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_C = tryCatch({fit_coef[3, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_C = tryCatch({fit_coef[3, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_C = tryCatch({fit_coef[3, 3]}, error = function(e) {return(NA)}), # nolint
                           P_C = tryCatch({fit_coef[3, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_I = NA,
                           SE_I = NA,
                           Z_I = NA,
                           P_I = NA,
                           RandomEffect = NA,
                           RandomEffectVariance = NA,
                           AIC = AIC(fit),
                           BIC = BIC(fit))

      fit_list[[i]] <- fit_df

    }

  } else {

    ### Extract Y
    Y <- unname(unlist(df[, y])) # nolint
    
    ### Extract X variables as data.frame
    X_df <- df[, x_pos] # nolint
    names_X <- names(X_df) # nolint
    
    ### Extract offset
    off <- unname(unlist(df[, "Inventory_Lag0"]))
    
    ### Extract time
    ar1_time <- factor(unname(unlist(df[, "Days_Lag0"])))
    
    ###Extract random effect ar(1)
    ar1_re <- unname(unlist(df[, "Site_Room_Turn"]))
    
    fit_list <- list()

    for (i in seq_len(ncol(X_df))) {

      X <- unname(unlist(X_df[, i])) # nolint

      fit <- glmmTMB(Y ~ X + ar1(ar1_time + 0 | ar1_re), offset = log(off), family = nbinom2())
      fit_summary <- summary(fit)
      fit_coef <- coef(fit_summary)$cond

      fit_df <- data.frame(DependentVariable = y,
                           IndependentVariable = names_X[i],
                           ConfoundingVariable = NA,
                           Interaction = NA,
                           Beta_X = tryCatch({fit_coef[2, 1]}, error = function(e) {return(NA)}), # nolint
                           SE_X = tryCatch({fit_coef[2, 2]}, error = function(e) {return(NA)}), # nolint
                           Z_X = tryCatch({fit_coef[2, 3]}, error = function(e) {return(NA)}), # nolint
                           P_X = tryCatch({fit_coef[2, 4]}, error = function(e) {return(NA)}), # nolint
                           Beta_C = NA,
                           SE_C = NA,
                           Z_C = NA,
                           P_C = NA,
                           Beta_I = NA,
                           SE_I = NA,
                           Z_I = NA,
                           P_I = NA,
                           RandomEffect = NA,
                           RandomEffectVariance = NA,
                           AIC = AIC(fit),
                           BIC = BIC(fit))

      fit_list[[i]] <- fit_df

    }

  }

  results_df <- dplyr::bind_rows(fit_list)
  results_list <- list("coefficients" = results_df, "models" = model_list, "vcov" = vcov_list)
  return(results_list)

}