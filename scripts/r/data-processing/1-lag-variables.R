### SCRIPT SETUP
library(dplyr)

rm(list = ls())

load("/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/master-file-clean.RData")

### STANDARDIZE DAYS AND (LOG)INVENTORY
df <- df %>%
    dplyr::ungroup(.) %>%
    dplyr::filter(Inventory > 0 & is.na(Inventory) == FALSE) %>% # filter non-informative observations
    dplyr::select(-c(Comments, WaterMedicationType)) %>% # remove Comments and WaterMedicationType columns
    dplyr::mutate(StdDays = (Days - mean(Days, na.rm = TRUE)) / sd(Days, na.rm = TRUE),
                  LogInventory = log(Inventory),
                  StdLogInventory = (LogInventory - mean(LogInventory, na.rm = TRUE)) / sd(LogInventory, na.rm = TRUE)) %>%
    dplyr::select(Order:Days, StdDays, Inventory, LogInventory, StdLogInventory, Euth:SetPointVC)

### CALCULATE TREATMENT CATEGORIES
df <- df %>%
    dplyr::mutate(PrimaryRespiratory = Ceftiofur + Enroflaxin + Tetracycline,
                  SecondaryRespiratory = Penicillin + Dexamethasone + Lincomycin) %>%
    dplyr::select(Order:OtherTreatments, PrimaryRespiratory:SecondaryRespiratory, TotalTreatments:SetPointVC)

### LAG VARIABLES
df <- df %>%
    dplyr::group_by(Site_Room_Turn) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 1), .names = "{.col}_Lag1")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 2), .names = "{.col}_Lag2")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 3), .names = "{.col}_Lag3")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 4), .names = "{.col}_Lag4")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 5), .names = "{.col}_Lag5")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 6), .names = "{.col}_Lag6")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 7), .names = "{.col}_Lag7")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 8), .names = "{.col}_Lag8")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 9), .names = "{.col}_Lag9")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 10), .names = "{.col}_Lag10")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 11), .names = "{.col}_Lag11")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 12), .names = "{.col}_Lag12")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 13), .names = "{.col}_Lag13")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 14), .names = "{.col}_Lag14")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 15), .names = "{.col}_Lag15")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 16), .names = "{.col}_Lag16")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 17), .names = "{.col}_Lag17")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 18), .names = "{.col}_Lag18")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 19), .names = "{.col}_Lag19")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 20), .names = "{.col}_Lag20")) %>%
    dplyr::mutate(across(c(Euth:SetPointVC), ~ dplyr::lag(.x, 21), .names = "{.col}_Lag21")) %>%
    dplyr::ungroup(.)

save(df, file = "/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/1-master-file-clean-lagged.RData")