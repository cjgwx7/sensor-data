library(dplyr)

rm(list = ls())

load("/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/master-file-clean.RData") # nolint

df <- df %>%
    dplyr::mutate(PrimaryRespiratory = Ceftiofur + Enroflaxin + Tetracycline,
                  SecondaryRespiratory = Penicillin + Dexamethasone + Lincomycin) %>% # nolint
    dplyr::select(Order:WaterMedicationType, PrimaryRespiratory:SecondaryRespiratory, ReHS:Comments) # nolint

df = df %>% # nolint
    dplyr::group_by(Site_Room_Turn) %>%
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 1), .names = "{.col}_Lag1")) %>% # nolint
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 2), .names = "{.col}_Lag2")) %>% # nolint
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 3), .names = "{.col}_Lag3")) %>% # nolint
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 4), .names = "{.col}_Lag4")) %>% # nolint
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 5), .names = "{.col}_Lag5")) %>% # nolint
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 6), .names = "{.col}_Lag6")) %>% # nolint
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 7), .names = "{.col}_Lag7")) %>% # nolint
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 8), .names = "{.col}_Lag8")) %>% # nolint
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 9), .names = "{.col}_Lag9")) %>% # nolint
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 10), .names = "{.col}_Lag10")) %>% # nolint
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 11), .names = "{.col}_Lag11")) %>% # nolint
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 12), .names = "{.col}_Lag12")) %>% # nolint
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 13), .names = "{.col}_Lag13")) %>% # nolint
    dplyr::mutate(across(c(Days:WaterMedications, ReHS:SetPointVC), ~ dplyr::lag(.x, 14), .names = "{.col}_Lag14")) %>% # nolint
    dplyr::ungroup(.) # nolint

save(df, file = "/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/master-file-clean-lagged.RData") # nolint