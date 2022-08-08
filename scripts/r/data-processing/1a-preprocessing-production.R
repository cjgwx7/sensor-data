###############################################################################
#                                                                             #
# Author: Caleb J. Grohmannn                                                  #
# Date: 6/16/2022                                                             #
# Purpose: data preprocessing for production data communicated by             #
# The Maschhoff's, LLC (attn: Dr. Caleb Shull), which includes mortality,     #
# treatment, temperature, and water disappearance records                     #
#                                                                             #
###############################################################################

print(paste("The current script was initialized on",
            format(Sys.Date(), "%A, %B %d, %Y"),
            "at",
            format(Sys.time(), "%I:%M %p")))

cat("\n")

cat(paste("########################################",
          "        BEGIN: ENVIRONMENT SETUP",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

cat("Loading package: data.table\n")
library(data.table)
cat("Loading package: dplyr\n")
library(dplyr)
cat("Loading package: readr\n")
library(readr)
cat("Loading package: stringr\n")
library(stringr)
cat("Loading package: tibble\n")
library(tibble)
cat("Loading package: tidyr\n")
library(tidyr)

rm(list = ls())

###############################################################################

cat("\n")

cat(paste("########################################",
          "         END: ENVIRONMENT SETUP",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

cat(paste("########################################",
          "         BEGIN: READ IN DATA",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

##### FILE INPUT/OUTPUT SPECS

infile <- commandArgs(trailingOnly = TRUE)[1]

file_version <- sub(".*%_", "", infile)
file_version <- sub(".csv$", "", file_version)

outfile1 <- paste("/group/deckerlab/cjgwx7/sensor-data/data/production/raw/production",
                  "-type-errors-",
                  file_version,
                  ".csv",
                  sep = "")

outfile2 <- paste("/group/deckerlab/cjgwx7/sensor-data/data/production/processed/production",
                  "-",
                  file_version,
                  ".csv",
                  sep = "")

outfile3 <- paste("/group/deckerlab/cjgwx7/sensor-data/results/exploratory-data-analysis/tables/production",
                  "-SummaryStats-PreQC-",
                  file_version,
                  ".csv",
                  sep = "")

outfile4 <- paste("/group/deckerlab/cjgwx7/sensor-data/results/exploratory-data-analysis/tables/production",
                  "-SiteSummaryStats-PreQC-",
                  file_version,
                  ".csv",
                  sep = "")

outfile5 <- paste("/group/deckerlab/cjgwx7/sensor-data/results/exploratory-data-analysis/tables/production",
                  "-SummaryStats-PostQC-",
                  file_version,
                  ".csv",
                  sep = "")

outfile6 <- paste("/group/deckerlab/cjgwx7/sensor-data/results/exploratory-data-analysis/tables/production",
                  "-SiteSummaryStats-PostQC-",
                  file_version,
                  ".csv",
                  sep = "")

outfile7 <- paste("/group/deckerlab/cjgwx7/sensor-data/results/exploratory-data-analysis/tables/production",
                  "-QC_Removals-",
                  file_version,
                  ".csv",
                  sep = "")

outfile8 <- paste("/group/deckerlab/cjgwx7/sensor-data/data/production/processed/production",
                  "-",
                  file_version,
                  ".RData",
                  sep = "")

col_names <- c("Site", "Room", "Date", "Days",
              "Inventory", "Euth", "Dead",
              "HiTempRMS", "LowTempRMS",
              "SetPointRMS", "WaterDispRMS",
              "AvgTempVC", "HiTempVC", "LowTempVC",
              "SetPointVC", "WaterDispVC",
              "Penicillin", "Dexamethasone",
              "Ceftiofur", "Enroflaxin", "Tetracycline",
              "Lincomycin", "OtherTreatments",
              "TotalTreatments", "WaterMedications",
              "WaterMedicationType", "Comments")
col_types <- c(rep("c", 3), rep("n", 22), rep("c", 2))

##### FILE INPUT
cat("Searching for variable type errors...\n")

production_prbs <- problems(read_csv(file = infile,
                                     col_types = col_types,
                                     na = "NA",
                                     trim_ws = TRUE))

if (nrow(production_prbs) == 0) {

  cat("There are no variable type errors in the input file\n")

} else {

  write_csv(production_prbs,
            file = outfile1)

  stop(paste("Variable type error(s) in input file.",
             "Check 'production-type-errors' log file and fix mistake."))

}

cat("Reading in clean data file...\n")

production <- suppressWarnings(read_csv(file = infile,
                                        col_names = col_names,
                                        col_types = col_types,
                                        na = "NA",
                                        trim_ws = TRUE,
                                        skip = 1))

cat("Printing raw dataset structure...\n")
glimpse(production)

###############################################################################

cat("\n")

cat(paste("########################################",
          "         END: READ IN DATA",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

cat(paste("########################################",
          "     BEGIN: VARIABLE CONFIGURATION",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

cat("Configuring Date variables...\n")

##### DATE CONFIGURATION
production_v2 <- production %>%
  mutate(Date = as.Date(Date, format = "%d-%b-%y")) %>%
  rename(CalendarDate = Date) %>%
  mutate(NumericDate = as.numeric(CalendarDate - as.Date("1899-12-30")))

cat("Configuring Site, Room, and Turn variables...\n")

##### SITE, ROOM, AND TURN CONFIGURATION

### Renumber room names and create Site_Room variable
production_v3 <- production_v2 %>%
  rename(OldRoom = Room) %>%
  group_by(Site) %>%
  mutate(Room = as.numeric(factor(OldRoom))) %>%
  ungroup(.) %>%
  mutate(Site_Room = paste(Site, "_", Room, sep = ""))

### Create Turn, Site_Room_Turn, and GlobalID variables
production_v4 <- production_v3 %>%
  mutate(Days = ifelse(is.na(Days) == TRUE, 0, Days),
         Turn = cumsum(c(-1, diff(Days)) < 0)) %>%
  filter(Days > 0) %>%
  group_by(Site_Room) %>%
  mutate(Turn = dense_rank(Turn)) %>%
  ungroup(.) %>%
  mutate(Site_Room_Turn = paste(Site, "_", Room, "_", Turn, sep = ""),
         GlobalID = paste(Site, "_", Room, "_", NumericDate, sep = ""),
         Site_NumericDate = paste(Site, "_", NumericDate, sep = ""))

cat("Arranging variables...\n")

##### VARIABLE ARRANGEMENT AND FINAL TYPE CONVERSION
production_v5 <- production_v4 %>%
  arrange(Site, Room, Turn, Days) %>%
  mutate(Site = factor(Site),
         OldRoom = factor(OldRoom),
         Room = factor(Room),
         Site_Room = factor(Site_Room),
         Turn = factor(Turn),
         Site_Room_Turn = factor(Site_Room_Turn)) %>%
  rowid_to_column(., var = "Order") %>%
  mutate(Order = as.numeric(Order)) %>%
  select(Order, GlobalID, Site, Site_NumericDate, Room, OldRoom,
         Site_Room, Turn, Site_Room_Turn, CalendarDate,
         NumericDate, Days, Inventory, Euth, Dead,
         Penicillin, Dexamethasone, Ceftiofur, Enroflaxin,
         Tetracycline, Lincomycin, OtherTreatments, TotalTreatments,
         WaterMedications, WaterDispRMS, WaterDispVC,
         LowTempRMS, HiTempRMS, SetPointRMS,
         LowTempVC, AvgTempVC, HiTempVC, SetPointVC)

###############################################################################

cat("\n")

cat(paste("########################################",
          "     END: VARIABLE CONFIGURATION",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

cat(paste("########################################",
          "     BEGIN: VARIABLE CALCULATION",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

cat("Calculating new variables...\n")
production_v6 <- production_v5 %>%
  ### Inventory
  group_by(Site_Room_Turn) %>%
  mutate(Inventory_Lag1 = lag(Inventory, 1, default = 0),
         InventoryChange = Inventory - Inventory_Lag1) %>%
  ungroup(.)

site_numeric_date <- production_v6 %>%
  select(Order:NumericDate, InventoryChange) %>%
  group_by(Site_NumericDate) %>%
  summarise(TotalInventoryChange = sum(InventoryChange)) %>%
  ungroup(.)

production_v7 <- left_join(production_v6, site_numeric_date,
                           by = "Site_NumericDate") %>%
  mutate(Marketing = ifelse(Days >= 125 & abs(TotalInventoryChange) >= 150,
                            1, NA)) %>%
  group_by(Site_Room_Turn) %>%
  fill(Marketing, .direction = "down") %>%
  ungroup(.) %>%
  mutate(Marketing = ifelse(is.na(Marketing) == TRUE, 0, 1)) %>%
  ### Mortality
  mutate(EuthProportion = Euth / Inventory,
         DeadProportion = Dead / Inventory,
         TotalMortality = Euth + Dead,
         TotalMortalityProportion = TotalMortality / Inventory) %>%
  ### Treatment
  mutate(PenicillinProportion =  Penicillin / Inventory,
         DexamethasoneProportion = Dexamethasone / Inventory,
         CeftiofurProportion = Ceftiofur / Inventory,
         EnroflaxinProportion = Enroflaxin / Inventory,
         TetracyclineProportion = Tetracycline / Inventory,
         LincomycinProportion = Lincomycin / Inventory,
         OtherTreatmentsProportion = OtherTreatments / Inventory,
         TotalTreatments = Penicillin + Dexamethasone + Ceftiofur +
           Enroflaxin + Tetracycline + Lincomycin + OtherTreatments,
         TotalTreatmentsProportion = TotalTreatments / Inventory,
         PrimaryRespiratory = Ceftiofur + Enroflaxin + Tetracycline,
         PrimaryRespiratoryProportion = PrimaryRespiratory / Inventory,
         SecondaryRespiratory = Penicillin + Dexamethasone + Lincomycin,
         SecondaryRespiratoryProportion = SecondaryRespiratory / Inventory) %>%
  ### Water disappearance
  mutate(WaterDispHdRMS = WaterDispRMS / Inventory,
         WaterDispHdVC = WaterDispVC / Inventory) %>%
  ### Temperature
  mutate(TempRangeRMS = HiTempRMS - LowTempRMS,
         TempRangeVC = HiTempVC - LowTempVC,
         LowTempSetPointDeviationRMS = SetPointRMS - LowTempRMS,
         HiTempSetPointDeviationRMS = SetPointRMS - HiTempRMS,
         LowTempSetPointDeviationVC = SetPointVC - LowTempVC,
         AvgTempSetPointDeviationVC = SetPointVC - AvgTempVC,
         HiTempSetPointDeviationVC = SetPointVC - HiTempVC) %>%
  ### Rearrange variables
  select(Order:Inventory, InventoryChange, TotalInventoryChange, Marketing,
         Euth, EuthProportion, Dead, DeadProportion,
         TotalMortality, TotalMortalityProportion,
         Penicillin, PenicillinProportion,
         Dexamethasone, DexamethasoneProportion,
         Ceftiofur, CeftiofurProportion,
         Enroflaxin, EnroflaxinProportion,
         Tetracycline, TetracyclineProportion,
         Lincomycin, LincomycinProportion,
         OtherTreatments, OtherTreatmentsProportion,
         PrimaryRespiratory, PrimaryRespiratoryProportion,
         SecondaryRespiratory, SecondaryRespiratoryProportion,
         TotalTreatments, TotalTreatmentsProportion, WaterMedications,
         WaterDispRMS, WaterDispHdRMS, WaterDispVC, WaterDispHdVC,
         SetPointRMS, LowTempRMS, HiTempRMS, TempRangeRMS,
         LowTempSetPointDeviationRMS, HiTempSetPointDeviationRMS,
         SetPointVC, LowTempVC, AvgTempVC, HiTempVC, TempRangeVC,
         LowTempSetPointDeviationVC, AvgTempSetPointDeviationVC,
         HiTempSetPointDeviationVC)

cat("Calculating pre-QC summary statistics...\n")
production_v7_long <- production_v7 %>%
  pivot_longer(Days:HiTempSetPointDeviationVC,
               names_to = "Variable",
               values_to = "Value") %>%
  mutate(Variable_Order = paste(Variable, "_", Order, sep = ""))

production_v7_long %>%
  group_by(Variable) %>%
  summarise(N = n(),
            N_missing = sum(is.na(Value)),
            Mean = round(mean(Value, na.rm = TRUE), 5),
            Median = round(median(Value, na.rm = TRUE), 5),
            SD = round(sd(Value, na.rm = TRUE), 6),
            Minimum = round(min(Value, na.rm = TRUE), 5),
            Maximum = round(max(Value, na.rm = TRUE), 5)) %>%
  write_csv(file = outfile3)

production_v7_long %>%
  group_by(Site, Variable) %>%
  summarise(N = n(),
            N_missing = sum(is.na(Value)),
            Mean = round(mean(Value, na.rm = TRUE), 5),
            Median = round(median(Value, na.rm = TRUE), 5),
            SD = round(sd(Value, na.rm = TRUE), 6),
            Minimum = round(min(Value, na.rm = TRUE), 5),
            Maximum = round(max(Value, na.rm = TRUE), 5)) %>%
  write_csv(file = outfile4)

###############################################################################

cat("\n")

cat(paste("########################################",
          "     END: VARIABLE CALCULATION",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

cat(paste("########################################",
          "        BEGIN: QUALITY CONTROL",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

cat("Removal of nonsensical values and outliers...\n")
production_v8 <- production_v7 %>%
  ### Inventory quality control
  mutate(Inventory = ifelse(Inventory < 1 | Inventory > 4500, NA, Inventory),
         across(c(Days:HiTempSetPointDeviationVC),
                ~ ifelse(is.na(Inventory) == TRUE, NA, .x)),
  ### Mortality quality control
         TotalMortalityProportion = ifelse(TotalMortalityProportion < 0 |
                                             TotalMortalityProportion > 0.075,
                                           NA, TotalMortalityProportion),
         across(c(Euth:TotalMortality),
                ~ ifelse(is.na(TotalMortalityProportion) == TRUE,
                          NA, .x)),
  ### Treatment quality control
         TotalTreatmentsProportion = ifelse(TotalTreatmentsProportion < 0 |
                                              TotalTreatmentsProportion > 0.25,
                                              NA,
                                              TotalTreatmentsProportion),
         across(c(Penicillin:TotalTreatments), ~ ifelse(is.na(TotalTreatmentsProportion),
                                                          NA,
                                                          .x)),
         WaterMedications = ifelse(!WaterMedications %in% c(0, 1),
                                   NA, WaterMedications),
  ### Water disappearance quality control
         across(c(WaterDispHdRMS, WaterDispHdVC), ~ ifelse(.x < 0.10 | .x > 7.5,
                                                           NA, .x)),
         WaterDispRMS = ifelse(is.na(WaterDispHdRMS) == TRUE, NA, WaterDispRMS),
         WaterDispVC = ifelse(is.na(WaterDispHdVC) == TRUE, NA, WaterDispVC),
  ### Temperature quality control
         across(c(SetPointRMS, SetPointVC), ~ ifelse(.x < 60 | .x > 90,
                                                     NA, .x)),
         across(c(LowTempRMS:HiTempRMS),
                ~ ifelse(.x < 45 | .x > 105 | TempRangeRMS < 0,
                         NA, .x)),
         across(c(LowTempVC, HiTempVC),
                ~ ifelse(.x < 45 | .x > 105 | TempRangeVC < 0,
                         NA, .x)),
         AvgTempVC = ifelse(AvgTempVC < 50 | AvgTempVC > 100 |
                              AvgTempVC < LowTempVC |
                              AvgTempVC > HiTempVC, NA, AvgTempVC),
         TempRangeRMS = ifelse(is.na(LowTempRMS) == TRUE |
                                 is.na(HiTempRMS) == TRUE,
                               NA, TempRangeRMS),
         TempRangeVC = ifelse(is.na(LowTempVC) == TRUE |
                                 is.na(HiTempVC) == TRUE,
                               NA, TempRangeVC),
         LowTempSetPointDeviationRMS = ifelse(is.na(SetPointRMS) == TRUE |
                                                is.na(LowTempRMS) == TRUE,
                                              NA, LowTempSetPointDeviationRMS),
         HiTempSetPointDeviationRMS = ifelse(is.na(SetPointRMS) == TRUE |
                                                is.na(HiTempRMS) == TRUE,
                                              NA, HiTempSetPointDeviationRMS),
         LowTempSetPointDeviationVC = ifelse(is.na(SetPointVC) == TRUE |
                                                is.na(LowTempVC) == TRUE,
                                              NA, LowTempSetPointDeviationVC),
         AvgTempSetPointDeviationVC = ifelse(is.na(SetPointVC) == TRUE |
                                               is.na(AvgTempVC) == TRUE,
                                             NA, AvgTempSetPointDeviationVC),
         HiTempSetPointDeviationVC = ifelse(is.na(SetPointVC) == TRUE |
                                               is.na(HiTempVC) == TRUE,
                                             NA, HiTempSetPointDeviationVC))

production_v9 <- production_v8 %>%
  mutate(WaterDispHdRMS_Residuals = unname(resid(lm(WaterDispHdRMS ~ Days + I(Days^2),
                                           na.action = na.exclude,
                                           data = production_v8))),
         WaterDispHdVC_Residuals = unname(resid(lm(WaterDispHdVC ~ Days + I(Days^2),
                                          na.action = na.exclude,
                                          data = production_v8))),
         WaterDispHdRMS_ResidualsStd = WaterDispHdRMS_Residuals /
           sd(WaterDispHdRMS_Residuals, na.rm = TRUE),
         WaterDispHdVC_ResidualsStd = WaterDispHdVC_Residuals /
           sd(WaterDispHdVC_Residuals, na.rm = TRUE),
         across(WaterDispRMS:WaterDispHdRMS,
         ~ ifelse(WaterDispHdRMS_ResidualsStd < -5 |
                    WaterDispHdRMS_ResidualsStd > 5,
                  NA, .x)),
         across(WaterDispVC:WaterDispHdVC,
         ~ ifelse(WaterDispHdVC_ResidualsStd < -5 |
                    WaterDispHdVC_ResidualsStd > 5,
                  NA, .x))) %>%
  select(-c(WaterDispHdRMS_Residuals:WaterDispHdVC_ResidualsStd))

cat("Calculating post-QC summary statistics...\n")
production_v9_long <- production_v9 %>%
  pivot_longer(Days:HiTempSetPointDeviationVC,
               names_to = "Variable",
               values_to = "Value") %>%
  mutate(Variable_Order = paste(Variable, "_", Order, sep = ""))

production_v9_long %>%
  group_by(Variable) %>%
  summarise(N = n(),
            N_missing = sum(is.na(Value)),
            Mean = round(mean(Value, na.rm = TRUE), 5),
            Median = round(median(Value, na.rm = TRUE), 5),
            SD = round(sd(Value, na.rm = TRUE), 6),
            Minimum = round(min(Value, na.rm = TRUE), 5),
            Maximum = round(max(Value, na.rm = TRUE), 5)) %>%
  write_csv(file = outfile5)

production_v9_long %>%
  group_by(Site, Variable) %>%
  summarise(N = n(),
            N_missing = sum(is.na(Value)),
            Mean = round(mean(Value, na.rm = TRUE), 5),
            Median = round(median(Value, na.rm = TRUE), 5),
            SD = round(sd(Value, na.rm = TRUE), 6),
            Minimum = round(min(Value, na.rm = TRUE), 5),
            Maximum = round(max(Value, na.rm = TRUE), 5)) %>%
  write_csv(file = outfile6)

removals <- left_join(production_v7_long,
                      production_v9_long %>%
                        select(Variable_Order, Value),
                      by = "Variable_Order") %>%
  mutate(FLAG = ifelse(is.na(Value.x) == FALSE & is.na(Value.y) == TRUE,
                       TRUE, FALSE)) %>%
  filter(FLAG == TRUE) %>%
  select(Order:Value.x) %>%
  rename(Value = Value.x) %>%
  write_csv(file = outfile7)

###############################################################################

cat("\n")

cat(paste("########################################",
          "        END: QUALITY CONTROL",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

cat(paste("########################################",
          "        BEGIN: FILE OUTPUT",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

cat("Printing clean dataset structure...\n")
glimpse(production_v9)

cat("Exporting clean dataset...\n")
write_csv(production_v9,
          file = outfile2)
save(production_v9,
     file = outfile8)

###############################################################################

cat("\n")

cat(paste("########################################",
          "        END: FILE OUTPUT",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

print(paste("The current script was completed on",
            format(Sys.Date(), "%A, %B %d, %Y"),
            "at",
            format(Sys.time(), "%I:%M %p")))