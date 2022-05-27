### SCRIPT SETUP
library(dplyr)
library(tidyr)

rm(list = ls())

load("/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/master-file-clean.RData")

### OUTLIER REMOVAL
df <- df %>%
    dplyr::ungroup(.) %>%
    # remove Comments and WaterMedicationType columns not needed for computations
    dplyr::select(-c(Comments, WaterMedicationType)) %>%
    # Set observations with zero Inventory to NA
    dplyr::mutate(across(c(Inventory:SetPointVC), ~ ifelse(Inventory == 0, NA, .x))) %>%
    # removed TreatmentProportion outlier that was caused by a high Dexamethasone observation;
    # consider 25% of the days inventory as a threshold for treatment outliers
    dplyr::mutate(TreatmentProportion = ifelse(TreatmentProportion > 0.25, NA, TreatmentProportion)) %>%
    # remove values of Euth, Dead, and TotalMortality if MortalityProportion was also removed
    dplyr::mutate(across(c(Euth:TotalMortality), ~ ifelse(is.na(MortalityProportion) == TRUE, NA, .x))) %>%
    # remove outlier values for water intake measures
    dplyr::mutate(across(c(WaterIntakeHdRMS, WaterIntakeHdVC), ~ ifelse(.x > 10, NA, .x))) %>%
    dplyr::mutate(WaterIntakeRMS = ifelse(is.na(WaterIntakeHdRMS) == TRUE, NA, WaterIntakeRMS),
                  WaterIntakeVC = ifelse(is.na(WaterIntakeHdVC) == TRUE, NA, WaterIntakeVC)) %>%
    # remove outliers for temperature variables
    dplyr::mutate(across(c(LowTempRMS:SetPointRMS), ~ ifelse(TempRangeRMS < 0, NA, .x))) %>%
    dplyr::mutate(across(c(LowTempVC:SetPointVC), ~ ifelse(TempRangeVC < 0, NA, .x)))

### VARIABLE STANDARDIZATION & CALCULATION
df <- df %>%
    # standardize Days and Inventory
    dplyr::mutate(StdDays = (Days - mean(Days, na.rm = TRUE)) / sd(Days, na.rm = TRUE),
                  StdInventory = (Inventory - mean(Inventory, na.rm = TRUE)) / sd(Inventory, na.rm = TRUE),
                  LogInventory = log(Inventory),
                  StdLogInventory = (LogInventory - mean(LogInventory, na.rm = TRUE)) / sd(LogInventory, na.rm = TRUE)) %>%
    # calculate treatment categories
    dplyr::mutate(PrimaryRespiratory = Ceftiofur + Enroflaxin + Tetracycline,
                  SecondaryRespiratory = Penicillin + Dexamethasone + Lincomycin) %>%
    # calculate set point deviations
    dplyr::mutate(LowTempSetPointDeviationRMS = LowTempRMS - SetPointRMS,
                  HiTempSetPointDeviationRMS = HiTempRMS - SetPointRMS,
                  LowTempSetPointDeviationVC = LowTempVC - SetPointVC,
                  AvgTempSetPointDeviationVC = AvgTempVC - SetPointVC,
                  HiTempSetPointDeviationVC = HiTempVC - SetPointVC) %>%
    # final rearrangement of variables
    dplyr::select(Order:Days, StdDays, Inventory, StdInventory, LogInventory, StdLogInventory,
                  Euth:OtherTreatments, PrimaryRespiratory, SecondaryRespiratory,
                  TotalTreatments:SetPointRMS, LowTempSetPointDeviationRMS, HiTempSetPointDeviationRMS,
                  LowTempVC:SetPointVC, LowTempSetPointDeviationVC:HiTempSetPointDeviationVC)

### SUMMARY STATISTICS
summary_stats <- df %>%
    tidyr::pivot_longer(cols = c(Days:HiTempSetPointDeviationVC),
                        names_to = "Variable",
                        values_to = "Value") %>%
    dplyr::group_by(Variable) %>%
    dplyr::summarise(N = sum(is.na(Value) == FALSE),
                     Mean = mean(Value, na.rm = TRUE),
                     SD = sd(Value, na.rm = TRUE),
                     Minimum = min(Value, na.rm = TRUE),
                     `First Quartile` = quantile(Value, probs = 0.25, names = FALSE, na.rm = TRUE),
                     Median = median(Value, na.rm = TRUE),
                     `Third Quartile` = quantile(Value, probs = 0.75, names = FALSE, na.rm = TRUE),
                     Maximum = max(Value, na.rm = TRUE))

write.csv(summary_stats,
          file = "/group/deckerlab/cjgwx7/sensor-data/results/exploratory-data-analysis/SummaryStatsAfterOutlierRemoval.csv",
          row.names = FALSE,
          col.names = TRUE)

### LAG VARIABLES
df <- df %>%
    # lags are calculated within unqiue turns
    dplyr::group_by(Site_Room_Turn) %>%
    # lag days 1 through 21
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 1), .names = "{.col}_Lag1")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 2), .names = "{.col}_Lag2")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 3), .names = "{.col}_Lag3")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 4), .names = "{.col}_Lag4")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 5), .names = "{.col}_Lag5")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 6), .names = "{.col}_Lag6")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 7), .names = "{.col}_Lag7")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 8), .names = "{.col}_Lag8")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 9), .names = "{.col}_Lag9")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 10), .names = "{.col}_Lag10")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 11), .names = "{.col}_Lag11")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 12), .names = "{.col}_Lag12")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 13), .names = "{.col}_Lag13")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 14), .names = "{.col}_Lag14")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 15), .names = "{.col}_Lag15")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 16), .names = "{.col}_Lag16")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 17), .names = "{.col}_Lag17")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 18), .names = "{.col}_Lag18")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 19), .names = "{.col}_Lag19")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 20), .names = "{.col}_Lag20")) %>%
    dplyr::mutate(across(c(Days:HiTempSetPointDeviationVC), ~ dplyr::lag(.x, 21), .names = "{.col}_Lag21")) %>%
    dplyr::ungroup(.)

save(df, file = "/storage/hpc/group/deckerlab/cjgwx7/sensor-data/data/1-master-file-clean-lagged.RData")