library(dplyr)
library(lmerTest)
library(readr)

df <- read_csv("/group/deckerlab/cjgwx7/sensor-data/data/master/master-lagged-RUN=2023-01-12-21-07-38_MAXDATE=2022-07-20.csv",
               show_col_types = FALSE)

df <- df %>%
    select(Order:GrowthPeriod, Days_Lag0, Inventory_Lag0, TotalMortality_Lag0, contains("ReHS")) %>%
    select(-contains("Yellow"), -contains("Temp"))
glimpse(df)

### Extract Y
Y <- unname(unlist(df[, "TotalMortality_Lag0"])) # nolint

### Extract X variables as data.frame
X_df <- df[, 18:ncol(df)] # nolint
names_X <- names(X_df) # nolint

### Extract covariate
cov <- unname(unlist(df[, "GrowthPeriod"]))

### Extract random effect
re <- unname(unlist(df[, "Site_Turn"]))

### Extract offset
off <- unname(unlist(df[, "Inventory_Lag0"]))

fit_list <- list()

for (i in seq_len(ncol(X_df))) {

    X <- unname(unlist(X_df[, i])) # nolint

    fit <- glmer.nb(Y ~ X + cov + (1 | re) + offset(log(off)))
    fit_list[[names_X[i]]] <- fit
    print(names_X[i])

}

save(fit_list, file = "/group/deckerlab/cjgwx7/sensor-data/results/statistical-modeling/TotalMortality/TotalMortality-NB-ReHS_FitList.RData")