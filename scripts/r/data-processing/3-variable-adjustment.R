###############################################################################
#                                                                             #
# Author: Caleb J. Grohmannn                                                  #
# Date: 8/10/2022                                                             #
# Purpose: model fitting/testing of inherent/environmental variation in       #
# water disappearance                                                         #
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

cat("Loading package: argparse\n")
library(argparse)
cat("Loading package: data.table\n")
library(data.table)
cat("Loading package: dplyr\n")
library(dplyr)
cat("Loading package: lmerTest\n")
library(lmerTest)
cat("Loading package: MASS\n")
library(MASS)
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
          "     BEGIN: FILE CONFIGURATION",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

parser <- ArgumentParser(description = "Specify file I/O")
parser$add_argument("--data",
                    help = "Data file for model fitting",
                    required = TRUE)
parser$add_argument("--data-export",
                    help = "Root path and file name for exporting CSV and RData files",
                    required = TRUE)
args <- parser$parse_args()

cat(paste("Importing data file from path:\n", args$data, sep = ""))
load(args$data)
cat("\n")
cat("\n")

cat("Preview of data file...\n")
glimpse(master)

###############################################################################

cat("\n")

cat(paste("########################################",
          "      END: FILE CONFIGURATION",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

cat(paste("########################################",
          "      BEGIN: VARIABLE ADJUSTMENT",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

master_std <- master %>%
    mutate(LogInventory = log(Inventory),
           StdDays = (Days - mean(Days, na.rm = TRUE)) / sd(Days, na.rm = TRUE),
           StdInventory = (Inventory - mean(Inventory, na.rm = TRUE)) / sd(Inventory, na.rm = TRUE),
           StdLogInventory = (LogInventory - mean(LogInventory, na.rm = TRUE)) / sd(LogInventory, na.rm = TRUE))

### WaterDispHdRMS
cat("Fitting statistical model for variable: WaterDispHdRMS...\n")
fit <- lmer(WaterDispHdRMS ~ MidPointTempSetPointDeviationRMS +
                             I(MidPointTempSetPointDeviationRMS^2) +
                             (1 | Site) +
                             (1 | Site:Marketing) +
                             (0 + StdInventory + I(StdInventory^2) + I(StdInventory^3) + StdDays + I(StdDays^2) + I(StdDays^3) | Site),
          data = master_std,
          na.action = na.exclude)

cat("Printing model fit summary for variable: WaterDispHdRMS...\n")
summary(fit)

cat("Extracting and saving residuals from model fit to new variable: ResidualWaterDispHdRMS...\n")
master_v2 <- master %>%
    mutate(ResidualWaterDispHdRMS = unname(residuals(fit))) %>%
    dplyr::select(Order:WaterDispHdRMS,
                  ResidualWaterDispHdRMS,
                  WaterDispVC:Color_ReHS)
cat("\n")

### WaterDispHdVC
cat("Fitting statistical model for variable: WaterDispHdVC...\n")
fit <- lmer(WaterDispHdVC ~  AvgTempSetPointDeviationVC +
                             I(AvgTempSetPointDeviationVC^2) +
                             (1 | Site) +
                             (1 | Site:Marketing) +
                             (0 + StdInventory + I(StdInventory^2) + I(StdInventory^3) + StdDays + I(StdDays^2) + I(StdDays^3) | Site),
          data = master_std,
          na.action = na.exclude)

cat("Printing model fit summary for variable: WaterDispHdVC...\n")
summary(fit)

cat("Extracting and saving residuals from model fit to new variable: ResidualWaterDispHdVC...\n")
master_v3 <- master_v2 %>%
    mutate(ResidualWaterDispHdVC = unname(residuals(fit))) %>%
    dplyr::select(Order:WaterDispHdVC,
                  ResidualWaterDispHdVC,
                  SetPointRMS:Color_ReHS)
cat("\n")

### Total Mortality
cat("Fitting statistical model for variable: TotalMortality...\n")
fit <- glmer.nb(TotalMortality ~ StdLogInventory +
                                 (1 + StdDays + I(StdDays^2) + I(StdDays^3) | Site_Turn),
                data = master_std,
                na.action = na.exclude)

cat("Printing model fit summary for variable: TotalMortality...\n")
summary(fit)

cat("Extracting and saving residuals from model fit to new variable: ResidualWaterDispHdVC...\n")
master_v4 <- master_v3 %>%
    mutate(ResidualTotalMortality = unname(residuals(fit, type = "response"))) %>%
    dplyr::select(Order:TotalMortalityProportion,
                  ResidualTotalMortality,
                  Penicillin:Color_ReHS)

MEDIAN <- median(master_v4$ResidualTotalMortality, na.rm = TRUE) # nolint
MAD <- mad(master_v4$ResidualTotalMortality, na.rm = TRUE) # nolint

master_v5 <- master_v4 %>%
    mutate(StdResidualTotalMortality = (ResidualTotalMortality - MEDIAN) / MAD,
           HighMortalityDay = ifelse(StdResidualTotalMortality >= 2, 1, 0),
           ExtremeHighMortalityDay = ifelse(StdResidualTotalMortality >= 3, 1, 0)) %>%
    dplyr::select(Order:ResidualTotalMortality,
                  StdResidualTotalMortality,
                  HighMortalityDay:ExtremeHighMortalityDay,
                  Penicillin:Color_ReHS)

cat("\n")

master <- master_v5

###############################################################################

cat("\n")

cat(paste("########################################",
          "         END: VARIABLE ADJUSTMENT",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

cat(paste("########################################",
          "          BEGIN: FILE EXPORT",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

root_export_path <- args$data_export
rdata_export <- paste(root_export_path, ".RData", sep = "")
csv_export <- paste(root_export_path, ".csv", sep = "")

cat(paste("Exporting .RData object to path:\n", rdata_export, sep = ""))
save(master, file = rdata_export)
cat("\n")
cat("\n")

cat(paste("Exporting .csv file to path:\n", csv_export, sep = ""))
write_csv(master,
          file = csv_export)
cat("\n")

###############################################################################

cat("\n")

cat(paste("########################################",
          "      END: FILE CONFIGURATION",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

print(paste("The current script was completed on",
            format(Sys.Date(), "%A, %B %d, %Y"),
            "at",
            format(Sys.time(), "%I:%M %p")))