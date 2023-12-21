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
cat("Loading package: lubridate\n")
library(lubridate)
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

infile1 <- commandArgs(trailingOnly = TRUE)[1]

file_root <- str_extract(infile1, pattern = "(?<=%).*(?=%)")
file_root <- paste("%", file_root, "%", sep = "")

file_version <- sub(".*%_", "", infile1)
date_version <- as.Date(sub(".csv$", "", file_version))
run_version <- paste(format(Sys.Date(), "%Y-%m-%d"),
                     "-",
                     format(Sys.time(), "%H-%M-%S"),
                     sep = "")

infile2 <- "/group/deckerlab/cjgwx7/sensor-data/data/production/raw/%COLUMN_KEY%.csv"

outfile1 <- paste("/group/deckerlab/cjgwx7/sensor-data/data/production/raw/",
                  file_root,
                  "-name-errors-",
                  "DATE=", date_version, "_",
                  "RUN=", run_version,
                  ".csv",
                  sep = "")

outfile2 <- paste("/group/deckerlab/cjgwx7/sensor-data/data/production/raw/",
                  file_root,
                  "-type-errors-",
                  "DATE=", date_version, "_",
                  "RUN=", run_version,
                  ".csv",
                  sep = "")

outfile3 <- paste("/group/deckerlab/cjgwx7/sensor-data/data/production/raw/",
                  file_root,
                  "-date-errors-",
                  "DATE=", date_version, "_",
                  "RUN=", run_version,
                  ".csv",
                  sep = "")

outfile4 <- paste("/group/deckerlab/cjgwx7/sensor-data/results/exploratory-data-analysis/tables/production",
                  "-SummaryStats-PreQC-",
                  "DATE=", date_version, "_",
                  "RUN=", run_version,
                  ".csv",
                  sep = "")

outfile5 <- paste("/group/deckerlab/cjgwx7/sensor-data/results/exploratory-data-analysis/tables/production",
                  "-SiteSummaryStats-PreQC-",
                  "DATE=", date_version, "_",
                  "RUN=", run_version,
                  ".csv",
                  sep = "")

outfile6 <- paste("/group/deckerlab/cjgwx7/sensor-data/results/exploratory-data-analysis/tables/production",
                  "-SummaryStats-PostQC-",
                  "DATE=", date_version, "_",
                  "RUN=", run_version,
                  ".csv",
                  sep = "")

outfile7 <- paste("/group/deckerlab/cjgwx7/sensor-data/results/exploratory-data-analysis/tables/production",
                  "-SiteSummaryStats-PostQC-",
                  "DATE=", date_version, "_",
                  "RUN=", run_version,
                  ".csv",
                  sep = "")

outfile8 <- paste("/group/deckerlab/cjgwx7/sensor-data/results/exploratory-data-analysis/tables/production",
                  "-SummaryStatsByPeriod-PostQC-",
                  "DATE=", date_version, "_",
                  "RUN=", run_version,
                  ".csv",
                  sep = "")

outfile9 <- paste("/group/deckerlab/cjgwx7/sensor-data/results/exploratory-data-analysis/tables/production",
                  "-SiteSummaryStatsByPeriod-PostQC-",
                  "DATE=", date_version, "_",
                  "RUN=", run_version,
                  ".csv",
                  sep = "")

outfile10 <- paste("/group/deckerlab/cjgwx7/sensor-data/results/exploratory-data-analysis/tables/production",
                   "-QC-Removals-",
                   "DATE=", date_version, "_",
                   "RUN=", run_version,
                   ".csv",
                   sep = "")

cat("Printing column name and type specifications...\n")
column_specs <- read_csv(infile2,
                         col_names = TRUE,
                         col_types = rep("c", 3))
print(column_specs, n = Inf)
cat("\n")

column_names_old <- column_specs %>%
  pull(OriginalName)

column_names_new <- column_specs %>%
  pull(NewName)

column_types <- column_specs %>%
  pull(VariableType) %>%
  paste(collapse = "")

##### FILE INPUT
cat("Searching for column name specification errors...\n")
validation_file <- suppressMessages(read_csv(infile1,
                                    col_names = TRUE,
                                    n_max = 0,
                                    show_col_types = FALSE))

max_length <- max(c(length(colnames(validation_file)), length(column_names_old)))
name_check <- tibble(`Correct Name` = c(column_names_old, rep(NA, max_length - length(column_names_old))),
                     `Actual Name` = c(colnames(validation_file), rep(NA, max_length - length(colnames(validation_file))))) %>%
  mutate(`Are they the same?` = `Correct Name` == `Actual Name`,
         `Are they the same?` = ifelse(is.na(`Are they the same?`) == TRUE, FALSE, `Are they the same?`))
name_check_vector <- name_check %>%
  pull(`Are they the same?`)

if (all(name_check_vector) == TRUE) {

  cat("There are no variable name errors in the input file...\n")

} else {

  write_csv(name_check,
            file = outfile1)

  cat(paste("Variable name error(s) in input file.",
            "Check 'production-name-errors' log file and fix mistake...\n"))

}

cat("\n")

cat("Searching for variable type errors...\n")
production_prbs <- problems(read_csv(file = infile1,
                                     col_types = column_types,
                                     na = "NA",
                                     trim_ws = TRUE))

if (nrow(production_prbs) == 0) {

  cat("There are no variable type errors in the input file...\n")

} else {

  write_csv(production_prbs,
            file = outfile2)

  cat(paste("Variable type error(s) in input file.",
            "Check 'production-type-errors' log file and fix mistake...\n"))

}

cat("\n")

cat("Reading in clean data file...\n")

production <- suppressWarnings(read_csv(file = infile1,
                                        col_names = column_names_new,
                                        col_types = column_types,
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

#### DATE QUALITY CONTROL
date_qc <- tibble(OriginalDate = unname(unlist(production[, "Date"]))) %>%
  mutate(RowNumber = seq_len(nrow(.)),
         NewDate = as.Date(OriginalDate, tryFormats = c("%m/%d/%Y", "%Y-%m-%d")),
         NewDate = ifelse(year(NewDate) < 2020, NA, NewDate),
         QualityCheck = is.na(OriginalDate) == FALSE & is.na(NewDate) == TRUE)

date_qc_vector <- date_qc %>%
  pull(QualityCheck)

if (any(date_qc_vector) == TRUE) {

  write_csv(date_qc,
            outfile3)

  cat(paste("Date error(s) in input file.",
              "Check 'date-errors' log file and fix mistake...\n"))

} else {

  cat("There are no date errors in the input file.\n")

}

if (all(name_check_vector) != TRUE | nrow(production_prbs) != 0 | any(date_qc_vector) == TRUE) {

  stop("One or more errors during file input or type parsing. Check log files before proceeding.")

}

cat("\n")

##### DATE CONFIGURATION
production_v2 <- production %>%
  mutate(Date = as.Date(Date, tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))) %>%
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
  mutate(Site_Turn = paste(Site, "_", Turn, sep = ""),
         Site_Room_Turn = paste(Site, "_", Room, "_", Turn, sep = ""),
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
         Site_Turn = factor(Site_Turn),
         Site_Room_Turn = factor(Site_Room_Turn)) %>%
  rowid_to_column(., var = "Order") %>%
  mutate(Order = as.numeric(Order)) %>%
  select(Order, GlobalID, Site, Site_NumericDate, Room, OldRoom,
         Site_Room, Turn, Site_Turn, Site_Room_Turn, CalendarDate,
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
                            1, NA),
        GrowthPeriod = ifelse(Days <= 42, "Early",
                              ifelse(Days > 42 & Days <= 84, "Early Middle",
                                     ifelse(Days > 84 & Days <= 126, "Late Middle", "Late"))),
        GrowthPeriod = factor(GrowthPeriod,
                              levels = c("Early", "Early Middle", "Late Middle", "Late"))) %>%
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
  mutate(SetPointRMS = coalesce(SetPointRMS, SetPointVC),
         SetPointVC = coalesce(SetPointVC, SetPointRMS),
         MidPointTempRMS = (HiTempRMS + LowTempRMS) / 2,
         TempRangeRMS = HiTempRMS - LowTempRMS,
         TempRangeVC = HiTempVC - LowTempVC,
         LowTempSetPointDeviationRMS = LowTempRMS - SetPointRMS,
         MidPointTempSetPointDeviationRMS = MidPointTempRMS - SetPointRMS,
         HiTempSetPointDeviationRMS = HiTempRMS - SetPointRMS,
         LowTempSetPointDeviationVC = LowTempVC - SetPointVC,
         AvgTempSetPointDeviationVC = AvgTempVC - SetPointVC,
         HiTempSetPointDeviationVC = HiTempVC - SetPointVC) %>%
  ### Rearrange variables
  select(Order:NumericDate, GrowthPeriod, Days,
         Inventory, InventoryChange, TotalInventoryChange, Marketing,
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
         SetPointRMS, LowTempRMS, MidPointTempRMS,
         HiTempRMS, TempRangeRMS, LowTempSetPointDeviationRMS,
         HiTempSetPointDeviationRMS, MidPointTempSetPointDeviationRMS,
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
  write_csv(file = outfile4)

production_v7_long %>%
  group_by(Site, Variable) %>%
  summarise(N = n(),
            N_missing = sum(is.na(Value)),
            Mean = round(mean(Value, na.rm = TRUE), 5),
            Median = round(median(Value, na.rm = TRUE), 5),
            SD = round(sd(Value, na.rm = TRUE), 6),
            Minimum = round(min(Value, na.rm = TRUE), 5),
            Maximum = round(max(Value, na.rm = TRUE), 5)) %>%
  write_csv(file = outfile5)

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
         across(c(Inventory:HiTempSetPointDeviationVC),
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
         WaterMedications = ifelse(WaterMedications > 1, 1, WaterMedications),
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
         across(c(LowTempRMS, HiTempRMS),
                ~ ifelse(.x < 45 | .x > 105 | TempRangeRMS < 0,
                         NA, .x)),
         across(c(LowTempVC, HiTempVC),
                ~ ifelse(.x < 45 | .x > 105 | TempRangeVC < 0,
                         NA, .x)),
         MidPointTempRMS = ifelse(MidPointTempRMS < 50 | MidPointTempRMS > 100 |
                              MidPointTempRMS < LowTempRMS |
                              MidPointTempRMS > HiTempRMS, NA, MidPointTempRMS),
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
         MidPointTempSetPointDeviationRMS = ifelse(is.na(SetPointRMS) == TRUE |
                                                is.na(MidPointTempRMS) == TRUE,
                                              NA, MidPointTempSetPointDeviationRMS),
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
  write_csv(file = outfile6)

production_v9_long %>%
  group_by(Site, Variable) %>%
  summarise(N = n(),
            N_missing = sum(is.na(Value)),
            Mean = round(mean(Value, na.rm = TRUE), 5),
            Median = round(median(Value, na.rm = TRUE), 5),
            SD = round(sd(Value, na.rm = TRUE), 6),
            Minimum = round(min(Value, na.rm = TRUE), 5),
            Maximum = round(max(Value, na.rm = TRUE), 5)) %>%
  write_csv(file = outfile7)

production_v9_long %>%
  group_by(GrowthPeriod, Variable) %>%
  summarise(N = n(),
            N_missing = sum(is.na(Value)),
            Mean = round(mean(Value, na.rm = TRUE), 5),
            Median = round(median(Value, na.rm = TRUE), 5),
            SD = round(sd(Value, na.rm = TRUE), 6),
            Minimum = round(min(Value, na.rm = TRUE), 5),
            Maximum = round(max(Value, na.rm = TRUE), 5)) %>%
  write_csv(file = outfile8)

production_v9_long %>%
  group_by(Site, GrowthPeriod, Variable) %>%
  summarise(N = n(),
            N_missing = sum(is.na(Value)),
            Mean = round(mean(Value, na.rm = TRUE), 5),
            Median = round(median(Value, na.rm = TRUE), 5),
            SD = round(sd(Value, na.rm = TRUE), 6),
            Minimum = round(min(Value, na.rm = TRUE), 5),
            Maximum = round(max(Value, na.rm = TRUE), 5)) %>%
  write_csv(file = outfile9)

removals <- left_join(production_v7_long,
                      production_v9_long %>%
                        select(Variable_Order, Value),
                      by = "Variable_Order") %>%
  mutate(FLAG = ifelse(is.na(Value.x) == FALSE & is.na(Value.y) == TRUE,
                       TRUE, FALSE)) %>%
  filter(FLAG == TRUE) %>%
  select(Order:Value.x) %>%
  rename(Value = Value.x) %>%
  write_csv(file = outfile10)

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

maxdate_version <- max(production_v9$CalendarDate)

outfile_final_csv <- paste("/group/deckerlab/cjgwx7/sensor-data/data/production/processed/production",
                           "-",
                           "DATE=", date_version, "_",
                           "RUN=", run_version, "_",
                           "MAXDATE=", maxdate_version,
                           ".csv",
                           sep = "")

outfile_final_rdata <- paste("/group/deckerlab/cjgwx7/sensor-data/data/production/processed/production",
                             "-",
                             "DATE=", date_version, "_",
                             "RUN=", run_version, "_",
                             "MAXDATE=", maxdate_version,
                             ".RData",
                             sep = "")

cat("Printing clean dataset structure...\n")
glimpse(production_v9)

cat("Exporting clean dataset...\n")
write_csv(production_v9,
          file = outfile_final_csv)
save(production_v9,
     file = outfile_final_rdata)

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