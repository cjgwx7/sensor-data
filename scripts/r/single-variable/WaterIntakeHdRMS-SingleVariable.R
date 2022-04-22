library(dplyr)
library(lmerTest)
library(lmtest)
library(tidyr)

rm(list = ls())

load("/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/master-file-clean-lagged.RData") # nolint

df2 <- df %>%
    dplyr::select(-c(GlobalID:Turn, CalendarDate:NumericDate,
                     Inventory, WaterMedicationType, Comments, WaterIntakeHdRMS)) # nolint

df3 <- df %>%
    dplyr::select(WaterIntakeHdRMS)

fit_list <- list()

for (i in c(4:ncol(df2))) {

    fit_df <- df2 %>%
        dplyr::select(Order, Site_Room_Turn, Days, i) %>%
        cbind(., df3) %>%
        tidyr::drop_na(.)

    null_fit <- lmer(WaterIntakeHdRMS ~ (1 + Days + I(Days^2) | Site_Room_Turn), data = fit_df, REML = FALSE) # nolint

    ind_var <- colnames(fit_df)[4]
    fit_formula <- formula(paste("WaterIntakeHdRMS ~ ", ind_var, " + (1 + Days + I(Days^2)| Site_Room_Turn)", sep = "")) # nolint
    fit <- lmer(fit_formula, data = fit_df, REML = FALSE)
    fit_summary <- coef(summary(fit))
    fit_lrt <- anova(null_fit, fit)
    summary_df <- data.frame(DepVar = "WaterIntakeHdRMS",
                             IndVar = ind_var,
                             Estimate = fit_summary[2, 1],
                             SE = fit_summary[2, 2],
                             df = fit_summary[2, 3],
                             T_Statistic = fit_summary[2, 4],
                             FTest_P = fit_summary[2, 5],
                             N_Parameters = fit_lrt[2, 1],
                             NullAIC = fit_lrt[1, 2],
                             TestAIC = fit_lrt[2, 2],
                             NullLikelihood = fit_lrt[1, 4],
                             FitLikelihood = fit_lrt[2, 4],
                             LRT_Statistic = fit_lrt[2, 6],
                             LRTdf = fit_lrt[2, 7],
                             LRT_P = fit_lrt[2, 8])
    fit_list[[i]] <- summary_df
    print(paste("The model for variable", ind_var, "was successfully fit.", sep = " ")) # nolint

}

df_out <- bind_rows(fit_list)

write.csv(df_out,
          file = "/group/deckerlab/cjgwx7/sensor-data/results/WaterIntakeHdRMS-SingleVariable.csv", # nolint
          sep = ",",
          row.names = FALSE,
          col.names = TRUE)