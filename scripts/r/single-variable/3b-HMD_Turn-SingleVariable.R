library(dplyr)
library(tidyr)

rm(list = ls())

load("/group/deckerlab/cjgwx7/sensor-data/data/2-master-file-clean-lagged.RData")

### Define lags to be included in single variable analysis
lags <- c(0:21)

### FOR loop 1: no confounding variable(s) included
df_list <- list()

for (i in lags) {

    df_fit <- df %>%
        dplyr::select(HMD_Turn_Lag0, ends_with(paste("Lag", i, sep = "")))

    df_list_by_lag <- list()

    for (j in c(2:ncol(df_fit))) {

        x_variable <- names(df_fit)[j]
        y_variable <- "HMD_Turn"
        y <- df_fit$HMD_Turn_Lag0
        x <- unlist(df_fit[, j])
        fit <- glm(y ~ x, family = binomial())
        summary_fit <- summary(fit)
        coef_fit <- coef(summary_fit)
        summary_fit_df <- tibble(`Dependent Variable` = y_variable,
                                 `Independent Variable` = x_variable,
                                 `Confounding Variable(s)` = "None",
                                 `Beta Estimate` = coef_fit[2, 1],
                                 `Standard Error` = coef_fit[2, 2],
                                 `Wald-Test Z-value` = coef_fit[2, 3],
                                 `Wald-Test P-value` = coef_fit[2, 4],
                                 `AIC` = AIC(fit),
                                 `BIC` = BIC(fit))
        df_list_by_lag[[x_variable]] <- summary_fit_df

    }

    df_list_by_lag <- bind_rows(df_list_by_lag)
    df_list[[i + 1]] <- df_list_by_lag

}

df_list1 <- bind_rows(df_list)

### FOR loop 2: Days variable(s) included
df_list <- list()

for (i in lags) {

    df_fit <- df %>%
        dplyr::select(HMD_Turn_Lag0, ends_with(paste("Lag", i, sep = "")))

    df_list_by_lag <- list()

    for (j in c(2:ncol(df_fit))) {

        x_variable <- names(df_fit)[j]
        y_variable <- "HMD_Turn"
        y <- df_fit$HMD_Turn_Lag0
        x <- unlist(df_fit[, j])
        days <- df_fit %>%
            dplyr::select(contains("StdDays")) %>%
            dplyr::pull(.)
        fit <- glm(y ~ x + days, family = binomial())
        summary_fit <- summary(fit)
        coef_fit <- coef(summary_fit)
        summary_fit_df <- tibble(`Dependent Variable` = y_variable,
                                 `Independent Variable` = x_variable,
                                 `Confounding Variable(s)` = "Days post placement",
                                 `Beta Estimate` = coef_fit[2, 1],
                                 `Standard Error` = coef_fit[2, 2],
                                 `Wald-Test Z-value` = coef_fit[2, 3],
                                 `Wald-Test P-value` = coef_fit[2, 4],
                                 `AIC` = AIC(fit),
                                 `BIC` = BIC(fit))
        df_list_by_lag[[x_variable]] <- summary_fit_df

    }

    df_list_by_lag <- bind_rows(df_list_by_lag)
    df_list[[i + 1]] <- df_list_by_lag

}

df_list2 <- bind_rows(df_list)

### FOR loop 3: Inventory variable(s) included
df_list <- list()

for (i in lags) {

    df_fit <- df %>%
        dplyr::select(HMD_Turn_Lag0, ends_with(paste("Lag", i, sep = "")))

    df_list_by_lag <- list()

    for (j in c(2:ncol(df_fit))) {

        x_variable <- names(df_fit)[j]
        y_variable <- "HMD_Turn"
        y <- df_fit$HMD_Turn_Lag0
        x <- unlist(df_fit[, j])
        inv <- df_fit %>%
            dplyr::select(contains("StdInventory")) %>%
            dplyr::pull(.)
        fit <- glm(y ~ x + inv, family = binomial())
        summary_fit <- summary(fit)
        coef_fit <- coef(summary_fit)
        summary_fit_df <- tibble(`Dependent Variable` = y_variable,
                                 `Independent Variable` = x_variable,
                                 `Confounding Variable(s)` = "Inventory",
                                 `Beta Estimate` = coef_fit[2, 1],
                                 `Standard Error` = coef_fit[2, 2],
                                 `Wald-Test Z-value` = coef_fit[2, 3],
                                 `Wald-Test P-value` = coef_fit[2, 4],
                                 `AIC` = AIC(fit),
                                 `BIC` = BIC(fit))
        df_list_by_lag[[x_variable]] <- summary_fit_df

    }

    df_list_by_lag <- bind_rows(df_list_by_lag)
    df_list[[i + 1]] <- df_list_by_lag

}

df_list3 <- bind_rows(df_list)

### FOR loop 4: Days and Inventory variable(s) included
df_list <- list()

for (i in lags) {

    df_fit <- df %>%
        dplyr::select(HMD_Turn_Lag0, ends_with(paste("Lag", i, sep = "")))

    df_list_by_lag <- list()

    for (j in c(2:ncol(df_fit))) {

        x_variable <- names(df_fit)[j]
        y_variable <- "HMD_Turn"
        y <- df_fit$HMD_Turn_Lag0
        x <- unlist(df_fit[, j])
        days <- df_fit %>%
            dplyr::select(contains("StdDays")) %>%
            dplyr::pull(.)
        inv <- df_fit %>%
            dplyr::select(contains("StdInventory")) %>%
            dplyr::pull(.)
        fit <- glm(y ~ x + days + inv, family = binomial())
        summary_fit <- summary(fit)
        coef_fit <- coef(summary_fit)
        summary_fit_df <- tibble(`Dependent Variable` = y_variable,
                                 `Independent Variable` = x_variable,
                                 `Confounding Variable(s)` = "Days post placement and Inventory",
                                 `Beta Estimate` = coef_fit[2, 1],
                                 `Standard Error` = coef_fit[2, 2],
                                 `Wald-Test Z-value` = coef_fit[2, 3],
                                 `Wald-Test P-value` = coef_fit[2, 4],
                                 `AIC` = AIC(fit),
                                 `BIC` = BIC(fit))
        df_list_by_lag[[x_variable]] <- summary_fit_df

    }

    df_list_by_lag <- bind_rows(df_list_by_lag)
    df_list[[i + 1]] <- df_list_by_lag

}

df_list4 <- bind_rows(df_list)

### FOR loop 5: Cubic Days and Cubic Inventory variable(s) included
df_list <- list()

for (i in lags) {

    df_fit <- df %>%
        dplyr::select(HMD_Turn_Lag0, ends_with(paste("Lag", i, sep = "")))

    df_list_by_lag <- list()

    for (j in c(2:ncol(df_fit))) {

        x_variable <- names(df_fit)[j]
        y_variable <- "HMD_Turn"
        y <- df_fit$HMD_Turn_Lag0
        x <- unlist(df_fit[, j])
        days <- df_fit %>%
            dplyr::select(contains("StdDays")) %>%
            dplyr::pull(.)
        inv <- df_fit %>%
            dplyr::select(contains("StdInventory")) %>%
            dplyr::pull(.)
        fit <- glm(y ~ x + days + I(days^2) + I(days^3) + inv + I(inv^2) + I(inv^3), family = binomial())
        summary_fit <- summary(fit)
        coef_fit <- coef(summary_fit)
        summary_fit_df <- tibble(`Dependent Variable` = y_variable,
                                 `Independent Variable` = x_variable,
                                 `Confounding Variable(s)` = "Days post placement and Cubic Inventory",
                                 `Beta Estimate` = coef_fit[2, 1],
                                 `Standard Error` = coef_fit[2, 2],
                                 `Wald-Test Z-value` = coef_fit[2, 3],
                                 `Wald-Test P-value` = coef_fit[2, 4],
                                 `AIC` = AIC(fit),
                                 `BIC` = BIC(fit))
        df_list_by_lag[[x_variable]] <- summary_fit_df

    }

    df_list_by_lag <- bind_rows(df_list_by_lag)
    df_list[[i + 1]] <- df_list_by_lag

}

df_list5 <- bind_rows(df_list)
df_out <- bind_rows(df_list1, df_list2, df_list3, df_list4, df_list5)

write.csv(df_out,
          file = "/group/deckerlab/cjgwx7/sensor-data/results/single-variable/HMD_Turn-SingleVariable.csv",
          sep = ",",
          row.names = FALSE,
          col.names = TRUE)