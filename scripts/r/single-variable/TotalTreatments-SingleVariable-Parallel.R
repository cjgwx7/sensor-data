library(dplyr)
library(lmerTest)
library(parallel)

rm(list = ls())

load("/group/deckerlab/cjgwx7/sensor-data/data/master-file-clean-lagged.RData")

depvars <- "TotalTreatments"
depvars <- df %>%
    dplyr::select(all_of(depvars)) %>%
    dplyr::pull(.)

indvars <- df %>%
    dplyr::select(-c(Order:Inventory,
                     WaterMedicationType, Comments, TotalTreatments,
                     contains("WaterIntakeRMS"), contains("WaterIntakeVC"),
                     contains("Days_Lag"), contains("Inventory_Lag"))) # nolint

var_names <- colnames(indvars)

fit <- function(x) {

    results <- coef(summary(glmer.nb(depvars ~ x + (1 + Days + I(Days^2) | Site_Room_Turn) + offset(log(Inventory)), data = df))) # nolint

}

fit_results <- mclapply(indvars, fit, mc.cores = 56)
save(fit_results, file = "/group/deckerlab/cjgwx7/sensor-data/scripts/sbatch/slurm-out/TotalTreatments/TotalTreatments.RData") # nolint

extract <- function(x) {

  x[2, ]

}

fit_results <- lapply(fit_results, extract)
fit_results <- bind_rows(fit_results)
fit_results <- cbind(var_names, fit_results) %>%
    dplyr::mutate(DepVar = "TotalTreatments") %>%
    dplyr::rename(IndVar = var_names,
                  SE = `Std. Error`,
                  WaldZ_Statistic = `z value`,
                  WaldTest_P = `Pr(>|z|)`) %>%
    dplyr::select(DepVar, IndVar, Estimate, SE, WaldZ_Statistic, WaldTest_P) # nolint

write.csv(fit_results,
          file = "/group/deckerlab/cjgwx7/sensor-data/results/single-variable/TotalTreatments-SingleVariable-Parallel.csv", # nolint
          sep = ",",
          row.names = FALSE,
          col.names = TRUE)
