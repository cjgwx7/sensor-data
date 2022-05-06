library(dplyr)
library(lmerTest)
library(tidyr)

rm(list = ls())

load("/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/master-file-clean-lagged.RData") # nolint

df2 <- df %>%
    dplyr::select(-c(GlobalID:Turn, CalendarDate:NumericDate,
                     WaterMedicationType, Comments, TotalTreatments,
                     contains("WaterIntakeRMS"), contains("WaterIntakeVC"),
                     contains("Days_Lag"), contains("Inventory_Lag")))

df3 <- df %>%
    dplyr::select(TotalTreatments)

fit_list <- list()

for (i in c(5:ncol(df2))) {

    fit_df <- df2 %>%
        dplyr::select(Order, Site_Room_Turn, Days, Inventory, i) %>%
        cbind(., df3) %>%
        tidyr::drop_na(.)

    ind_var <- colnames(fit_df)[5]

    print(paste("The model for variable", ind_var, "was initiated at", Sys.time(), sep = " ")) # nolint

    fit_formula <- formula(paste("TotalTreatments ~ ", ind_var, " + (1 + Days + I(Days^2) | Site_Room_Turn) + offset(log(Inventory))", sep = "")) # nolint
    fit <- glmer.nb(fit_formula,
                    data = fit_df)
    fit_summary <- coef(summary(fit))
    summary_df <- data.frame(DepVar = "TotalTreatments",
                             IndVar = ind_var,
                             Estimate = fit_summary[2, 1],
                             SE = fit_summary[2, 2],
                             WaldZ_Statistic = fit_summary[2, 3],
                             WaldTest_P = fit_summary[2, 4])
    fit_list[[i]] <- summary_df

    print(paste("The model for variable", ind_var, "was successfully fit at", Sys.time(), sep = " ")) # nolint

}

df_out <- bind_rows(fit_list)

write.csv(df_out,
          file = "/group/deckerlab/cjgwx7/sensor-data/results/single-variable/TotalTreatments-SingleVariable.csv", # nolint
          sep = ",",
          row.names = FALSE,
          col.names = TRUE)