### SCRIPT SETUP
library(dplyr)
library(lmerTest)
library(MASS)

rm(list = ls())

source("/group/deckerlab/cjgwx7/sensor-data/scripts/r/source/functions.R")
load("/group/deckerlab/cjgwx7/sensor-data/data/1-master-file-clean-lagged.RData")

### MODEL FITTING
global_fit <- glm.nb(TotalMortality ~ StdDays + I(StdDays^2) + I(StdDays^3) + StdLogInventory,
                     na.action = na.exclude,
                     data = df)

turn_fit <- glmer.nb(TotalMortality ~ StdLogInventory +
                                      (1 + StdDays + I(StdDays^2) + I(StdDays^3) | Site_Room_Turn),
                     na.action = na.exclude,
                     data = df)

### EXTRACT RESIDUALS AND PERFORM THRESHOLDING
df <- df %>%
    dplyr::mutate(HMD_GlobalResid = residuals(global_fit, type = "response"),
                  HMD_Global = mad_thresholding(HMD_GlobalResid, 2, 1.4826),
                  HMD_TurnResid = residuals(turn_fit, type = "response"),
                  HMD_Turn = mad_thresholding(HMD_TurnResid, 2, 1.4826)) %>%
    dplyr::select(Order:TotalMortality, HMD_Global, HMD_GlobalResid,
                  HMD_Turn, HMD_TurnResid,
                  MortalityProportion:SetPointVC_Lag21)

### SAVE NEW FILE
save(df, file = "/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/2-master-file-clean-lagged.RData")
