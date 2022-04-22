library(dplyr)
library(lmerTest)

load("/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/master-file-clean-lagged.RData") # nolint

df2 <- df %>%
     dplyr::select(-c(WaterMedicationType, Comments, Days, Inventory, TotalMortality)) # nolint

df3 <- df %>%
    dplyr::select(Days, Inventory, TotalMortality)

fit_list <- list()

for (i in c(11:ncol(df2))) {

    fit_df <- df2 %>%
        dplyr::select(Order, Site_Room_Turn, i) %>%
        cbind(., df3)
    ind_var <- colnames(fit_df)[3]
    fit_formula <- formula(paste("TotalMortality ~ ", ind_var, " + log(Inventory) + (1 + Days + I(Days^2)|Site_Room_Turn)", sep = "")) # nolint
    fit_summary <- coef(summary(glmer.nb(fit_formula, data = fit_df))) # nolint
    summary_df <- data.frame(DepVar = "TotalMortality",
                             IndVar = ind_var,
                             Estimate = fit_summary[2, 1],
                             SE = fit_summary[2, 2],
                             z = fit_summary[2, 3],
                             P = fit_summary[2, 4])
    fit_list[[i]] <- summary_df

}

df_out <- bind_rows(fit_list)

write.csv(df_out,
          file = "/group/deckerlab/cjgwx7/sensor-data/results/TotalMortality-SingleVariable.csv", # nolint
          sep = ",",
          row.names = FALSE,
          col.names = TRUE)