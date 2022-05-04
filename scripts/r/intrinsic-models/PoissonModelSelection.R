library(dplyr)
library(lmerTest)
library(tidyr)

rm(list = ls())

load("/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/master-file-clean.RData") # nolint

variable <- commandArgs(trailingOnly = TRUE)[1]

df <- df %>%
    dplyr::ungroup(.) %>%
    dplyr::select(Order, all_of(variable), Site, Site_Room_Turn, Days, Inventory) %>% # nolint
    tidyr::drop_na(.)
colnames(df)[2] <- "variable"

model_list <- list(Level1_glm = formula(variable ~ 1),
                   Level1_oinv_glm = formula(variable ~ offset(log(Inventory))),
                   Level1_inv_glm = formula(variable ~ log(Inventory)),
                   Level2_a_glm = formula(variable ~ Days),
                   Level2_a_oinv_glm = formula(variable ~ Days + offset(log(Inventory))), # nolint
                   Level2_a_inv_glm = formula(variable ~ Days + log(Inventory)),
                   Level2_b_glm = formula(variable ~ Days + I(Days^2)),
                   Level2_b_oinv_glm = formula(variable ~ Days + I(Days^2) + offset(log(Inventory))), # nolint
                   Level2_b_inv_glm = formula(variable ~ Days + I(Days^2) + log(Inventory)), # nolint
                   Level2_c_glm = formula(variable ~ Days + I(Days^2) + I(Days^3)), # nolint
                   Level2_c_oinv_glm = formula(variable ~ Days + I(Days^2) + I(Days^3) + offset(log(Inventory))), # nolint
                   Level2_c_inv_glm = formula(variable ~ Days + I(Days^2) + I(Days^3) + log(Inventory)), # nolint
                   Level3_a_glmer = formula(variable ~ (1 + Days | Site)),
                   Level3_a_oinv_glmer = formula(variable ~ (1 + Days | Site) + offset(log(Inventory))), # nolint
                   Level3_a_inv_glmer = formula(variable ~ (1 + Days | Site) + log(Inventory)), # nolint
                   Level3_b_glmer = formula(variable ~ (1 + Days + I(Days^2) | Site)), # nolint
                   Level3_b_oinv_glmer = formula(variable ~ (1 + Days + I(Days^2) | Site) + offset(log(Inventory))), # nolint
                   Level3_b_inv_glmer = formula(variable ~ (1 + Days + I(Days^2) | Site) + log(Inventory)), # nolint
                   Level3_c_glmer = formula(variable ~ (1 + Days + I(Days^2) + I(Days^3) | Site)), # nolint
                   Level3_c_oinv_glmer = formula(variable ~ (1 + Days + I(Days^2) + I(Days^3) | Site) + offset(log(Inventory))), # nolint
                   Level3_c_inv_glmer = formula(variable ~ (1 + Days + I(Days^2) + I(Days^3) | Site) + log(Inventory)), # nolint
                   Level4_a_glmer = formula(variable ~ (1 + Days | Site_Room_Turn)), # nolint
                   Level4_a_oinv_glmer = formula(variable ~ (1 + Days | Site_Room_Turn) + offset(log(Inventory))), # nolint
                   Level4_a_inv_glmer = formula(variable ~ (1 + Days | Site_Room_Turn) + log(Inventory)), # nolint
                   Level4_b_glmer = formula(variable ~ (1 + Days + I(Days^2) | Site_Room_Turn)), # nolint
                   Level4_b_oinv_glmer = formula(variable ~ (1 + Days + I(Days^2) | Site_Room_Turn) + offset(log(Inventory))), # nolint
                   Level4_b_inv_glmer = formula(variable ~ (1 + Days + I(Days^2) | Site_Room_Turn) + log(Inventory)), # nolint
                   Level4_c_glmer = formula(variable ~ (1 + Days + I(Days^2) + I(Days^3) | Site_Room_Turn)), # nolint
                   Level4_c_oinv_glmer = formula(variable ~ (1 + Days + I(Days^2) + I(Days^3) | Site_Room_Turn) + offset(log(Inventory))), # nolint
                   Level4_c_inv_glmer = formula(variable ~ (1 + Days + I(Days^2) + I(Days^3) | Site_Room_Turn) + log(Inventory))) # nolint

model_summary_list <- list()

for (i in seq_len(length(model_list))) {

    model_name <- names(model_list)[i]
    model_formula <- model_list[[i]]

    if (grepl("glmer", model_name) == FALSE) {

        start <- Sys.time()
        model_fit <- glm(model_formula,
                         family = poisson(),
                         data = df)
        aic <- AIC(model_fit)
        end <- Sys.time()

    } else {

        start <- Sys.time()
        model_fit <- glmer(model_formula,
                           family = poisson(),
                           data = df)
        aic <- AIC(model_fit)
        end <- Sys.time()

    }

    time <- end - start

    results <- data.frame(`ModelName` = model_name,
                          `ModelAIC` = aic,
                          `Time_to_Convergence` = time)

    model_summary_list[[i]] <- results

}

df_out <- bind_rows(model_summary_list)
path_out <- paste("/group/deckerlab/cjgwx7/sensor-data/results/intrinsic-models/", variable, "/Poisson.csv", sep = "") # nolint

write.csv(df_out,
          path_out,
          sep = ",",
          row.names = FALSE,
          col.names = TRUE)