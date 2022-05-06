library(dplyr)
library(lmerTest)
library(tidyr)

rm(list = ls())

load("/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/master-file-clean-lagged.RData") # nolint

variable <- commandArgs(trailingOnly = TRUE)[1]
print(variable)

df <- df %>%
    dplyr::ungroup(.) %>%
    dplyr::select(Order, all_of(variable), Site, Site_Room_Turn, Days, Inventory) %>% # nolint
    tidyr::drop_na(.)
colnames(df)[2] <- "variable"

model_list <- list(Level1_glm = formula(variable ~ 1),
                   Level2_a_glm = formula(variable ~ Days),
                   Level2_b_glm = formula(variable ~ Days + I(Days^2)),
                   Level2_c_glm = formula(variable ~ Days + I(Days^2) + I(Days^3)), # nolint
                   Level3_a_glmer = formula(variable ~ (1 + Days | Site)),
                   Level3_b_glmer = formula(variable ~ (1 + Days + I(Days^2) | Site)), # nolint
                   Level3_c_glmer = formula(variable ~ (1 + Days + I(Days^2) + I(Days^3) | Site)), # nolint
                   Level4_a_glmer = formula(variable ~ (1 + Days | Site_Room_Turn)), # nolint
                   Level4_b_glmer = formula(variable ~ (1 + Days + I(Days^2) | Site_Room_Turn)), # nolint
                   Level4_c_glmer = formula(variable ~ (1 + Days + I(Days^2) + I(Days^3) | Site_Room_Turn))) # nolint

model_summary_list <- list()

for (i in seq_len(length(model_list))) {

    model_name <- names(model_list)[i]

    print(paste("Model", model_name, "started"))

    model_formula <- model_list[[i]]

    if (grepl("glmer", model_name) == FALSE) {

        start <- Sys.time()
        model_fit <- glm(model_formula,
                         family = binomial(),
                         weights = Inventory,
                         data = df)
        aic <- AIC(model_fit)
        end <- Sys.time()

    } else {

        start <- Sys.time()
        model_fit <- glmer(model_formula,
                           family = binomial(),
                           weights = Inventory,
                           data = df)
        aic <- AIC(model_fit)
        end <- Sys.time()

    }

    time <- end - start

    results <- data.frame(`ModelName` = model_name,
                          `ModelAIC` = aic,
                          `Time_to_Convergence` = time)

    model_summary_list[[i]] <- results

    print(paste("Model", model_name, "finished"))

}

df_out <- bind_rows(model_summary_list)
path_out <- paste("/group/deckerlab/cjgwx7/sensor-data/results/intrinsic-models/", variable, "/Binomial.csv", sep = "") # nolint

write.csv(df_out,
          path_out,
          sep = ",",
          row.names = FALSE,
          col.names = TRUE)