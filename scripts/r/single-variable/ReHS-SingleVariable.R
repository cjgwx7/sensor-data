library(dplyr)
library(lmerTest)

load("/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/master-file-clean-lagged.RData") # nolint

df2 <- df %>%
     dplyr::select(-c(WaterMedicationType, Comments, Days, ReHS))

df3 <- df %>%
    dplyr::select(Days, ReHS)

fit_list <- list()

for (i in c(11:ncol(df2))) {

    fit_df <- df2 %>%
        dplyr::select(Order, Site_Room_Turn, i) %>%
        cbind(., df3)
    ind_var <- colnames(fit_df)[3]
    fit_formula <- formula(paste("ReHS ~ ", ind_var, " + (1 + Days + I(Days^2) + I(Days^3) | Site_Room_Turn)", sep = "")) # nolint
    fit_summary <- coef(summary(lmer(fit_formula, data = fit_df)))
    summary_df <- data.frame(DepVar = "ReHS",
                             IndVar = ind_var,
                             Estimate = fit_summary[2, 1],
                             SE = fit_summary[2, 2],
                             df = fit_summary[2, 3],
                             t = fit_summary[2, 4],
                             P = fit_summary[2, 5])
    fit_list[[i]] <- summary_df

}

df_out <- bind_rows(fit_list)

write.csv(df_out,
          file = "/group/deckerlab/cjgwx7/sensor-data/results/ReHS-SingleVariable.csv", # nolint
          sep = ",",
          row.names = FALSE,
          col.names = TRUE)