single_variable_pois <- function(df, y, x_pos, covariate, int, random_effect) { # nolint

  if (require(lmerTest) == FALSE) {

    stop("Install package 'lmerTest' in order to utilize function")

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

    fit_list <- list()

    for (i in seq_len(ncol(X_df))) {

      X <- unname(unlist(X_df[, i])) # nolint

      fit <- glmer(Y ~ X * cov + (1 | re) + offset(log(off)), family = poisson())
      fit_summary <- summary(fit)
      fit_coef <- coef(fit_summary)
      variance <- VarCorr(fit)

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
                           RandomEffect = random_effect,
                           RandomEffectVariance = variance$re[1, 1],
                           AIC = AIC(fit),
                           BIC = BIC(fit))

      fit_list[[i]] <- fit_df

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

    fit_list <- list()

    for (i in seq_len(ncol(X_df))) {

      X <- unname(unlist(X_df[, i])) # nolint

      fit <- glmer(Y ~ X + cov + (1 | re) + offset(log(off)), family = poisson())
      print(names_X[i])
      fit_summary <- summary(fit)
      fit_coef <- coef(fit_summary)
      variance <- VarCorr(fit)

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
                           RandomEffectVariance = variance$re[1, 1],
                           AIC = AIC(fit),
                           BIC = BIC(fit))

      fit_list[[i]] <- fit_df

    }

  } else if (length(covariate) == 1 & int == TRUE & length(random_effect) != 1) {

    ### Extract Y
    Y <- unname(unlist(df[, y])) # nolint

    ### Extract X variables as data.frame
    X_df <- df[, x_pos] # nolint
    names_X <- names(X_df) # nolint

    ### Extract covariate
    cov <- unname(unlist(df[, covariate]))

    ### Extract offset
    off <- unname(unlist(df[, "Inventory_Lag0"]))

    fit_list <- list()

    for (i in seq_len(ncol(X_df))) {

      X <- unname(unlist(X_df[, i])) # nolint

      fit <- glm(Y ~ X * cov + offset(log(off)), family = poisson())
      fit_summary <- summary(fit)
      fit_coef <- coef(fit_summary)

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

    fit_list <- list()

    for (i in seq_len(ncol(X_df))) {

      X <- unname(unlist(X_df[, i])) # nolint

      fit <- glmer(Y ~ X + (1 | re) + offset(log(off)), family = poisson())
      fit_summary <- summary(fit)
      fit_coef <- coef(fit_summary)
      variance <- VarCorr(fit)

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
                           RandomEffectVariance = variance$re[1, 1],
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

    fit_list <- list()

    for (i in seq_len(ncol(X_df))) {

      X <- unname(unlist(X_df[, i])) # nolint

      fit <- glm(Y ~ X + cov + offset(log(off)), family = poisson())
      fit_summary <- summary(fit)
      fit_coef <- coef(fit_summary)

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

    fit_list <- list()

    for (i in seq_len(ncol(X_df))) {

      X <- unname(unlist(X_df[, i])) # nolint

      fit <- glm(Y ~ X + offset(log(off)), family = poisson())
      fit_summary <- summary(fit)
      fit_coef <- coef(fit_summary)

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
  return(results_df)

}