### SCRIPT SETUP
library(dplyr)
library(lmerTest)
library(MASS)

rm(list = ls())

source("/group/deckerlab/cjgwx7/sensor-data/scripts/r/source/functions.R")
load("/group/deckerlab/cjgwx7/sensor-data/data/1-master-file-clean-lagged.RData")

### MODEL FITTING
global_fit <- glm.nb(TotalMortality_Lag0 ~ StdDays_Lag0 + I(StdDays_Lag0^2) + I(StdDays_Lag0^3) + StdLogInventory_Lag0,
                     na.action = na.exclude,
                     data = df)

turn_fit <- glmer.nb(TotalMortality_Lag0 ~ StdLogInventory_Lag0 +
                                      (1 + StdDays_Lag0 + I(StdDays_Lag0^2) + I(StdDays_Lag0^3) | Site_Room_Turn),
                     na.action = na.exclude,
                     data = df)

### EXTRACT RESIDUALS AND PERFORM THRESHOLDING
df <- df %>%
    dplyr::mutate(HMD_GlobalResid_Lag0 = residuals(global_fit, type = "response"),
                  HMD_Global_Lag0 = mad_thresholding(HMD_GlobalResid_Lag0, 2, 1.4826),
                  HMD_TurnResid_Lag0 = residuals(turn_fit, type = "response"),
                  HMD_Turn_Lag0 = mad_thresholding(HMD_TurnResid_Lag0, 2, 1.4826)) %>%
    dplyr::select(Order:TotalMortality_Lag0, HMD_Global_Lag0, HMD_GlobalResid_Lag0,
                  HMD_Turn_Lag0, HMD_TurnResid_Lag0,
                  MortalityProportion_Lag0:HiTempSetPointDeviationVC_Lag21)

### SAVE NEW FILE
save(df, file = "/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/2-master-file-clean-lagged.RData")
