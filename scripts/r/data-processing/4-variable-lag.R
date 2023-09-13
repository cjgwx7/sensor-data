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

cat("De-trending water disappearance variables...\n")
master <- master %>%
    group_by(Site_Room_Turn) %>%
    mutate(WaterDispHdRMS_Lag1 = lag(WaterDispHdRMS, 1),
           WaterDispHdVC_Lag1 = lag(WaterDispHdVC, 1)) %>%
    ungroup(.) %>%
    mutate(WaterDispHdRMSDifference = WaterDispHdRMS - WaterDispHdRMS_Lag1,
           WaterDispHdVCDifference = WaterDispHdVC - WaterDispHdVC_Lag1) %>%
    dplyr::select(Order:WaterDispHdRMS, WaterDispHdRMSDifference,
                  WaterDispVC:WaterDispHdVC, WaterDispHdVCDifference,
                  SetPointRMS:Color_ReHS_Yellow_Red)

cat("Creating a standardized copy...\n")
master_std <- master %>%
    mutate(across(c(Days:TotalMortalityProportion, Penicillin:TotalTreatmentsProportion, WaterDispRMS:ReHS),
                  ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)))

cat("Lagging variables...\n")
master <- master %>%
    # lags are calculated within unqiue turns
    dplyr::group_by(Site_Room_Turn) %>%
    # lag days 0 through 21
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), .names = "{.col}_Lag0")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 1), .names = "{.col}_Lag1")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 2), .names = "{.col}_Lag2")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 3), .names = "{.col}_Lag3")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 4), .names = "{.col}_Lag4")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 5), .names = "{.col}_Lag5")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 6), .names = "{.col}_Lag6")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 7), .names = "{.col}_Lag7")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 8), .names = "{.col}_Lag8")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 9), .names = "{.col}_Lag9")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 10), .names = "{.col}_Lag10")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 11), .names = "{.col}_Lag11")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 12), .names = "{.col}_Lag12")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 13), .names = "{.col}_Lag13")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 14), .names = "{.col}_Lag14")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 15), .names = "{.col}_Lag15")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 16), .names = "{.col}_Lag16")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 17), .names = "{.col}_Lag17")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 18), .names = "{.col}_Lag18")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 19), .names = "{.col}_Lag19")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 20), .names = "{.col}_Lag20")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 21), .names = "{.col}_Lag21")) %>%
    dplyr::select(-c(Days:Color_ReHS_Yellow_Red)) %>%
    dplyr::ungroup(.)

master_std <- master_std %>%
    # lags are calculated within unqiue turns
    dplyr::group_by(Site_Room_Turn) %>%
    # lag days 0 through 21
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), .names = "{.col}_Lag0")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 1), .names = "{.col}_Lag1")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 2), .names = "{.col}_Lag2")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 3), .names = "{.col}_Lag3")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 4), .names = "{.col}_Lag4")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 5), .names = "{.col}_Lag5")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 6), .names = "{.col}_Lag6")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 7), .names = "{.col}_Lag7")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 8), .names = "{.col}_Lag8")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 9), .names = "{.col}_Lag9")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 10), .names = "{.col}_Lag10")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 11), .names = "{.col}_Lag11")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 12), .names = "{.col}_Lag12")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 13), .names = "{.col}_Lag13")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 14), .names = "{.col}_Lag14")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 15), .names = "{.col}_Lag15")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 16), .names = "{.col}_Lag16")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 17), .names = "{.col}_Lag17")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 18), .names = "{.col}_Lag18")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 19), .names = "{.col}_Lag19")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 20), .names = "{.col}_Lag20")) %>%
    dplyr::mutate(across(c(Days:Color_ReHS_Yellow_Red), ~ dplyr::lag(.x, 21), .names = "{.col}_Lag21")) %>%
    dplyr::select(-c(Days:Color_ReHS_Yellow_Red)) %>%
    dplyr::ungroup(.)

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

rdata_export <- paste(root_export_path, "-standardized.RData", sep = "")
csv_export <- paste(root_export_path, "-standardized.csv", sep = "")

cat(paste("Exporting .RData object to path:\n", rdata_export, sep = ""))
save(master_std, file = rdata_export)
cat("\n")
cat("\n")

cat(paste("Exporting .csv file to path:\n", csv_export, sep = ""))
write_csv(master_std,
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