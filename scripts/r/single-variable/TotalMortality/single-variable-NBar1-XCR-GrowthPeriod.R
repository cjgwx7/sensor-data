###############################################################################
#                                                                             #
# Author: Caleb J. Grohmannn                                                  #
# Date: 8/25/2022                                                             #
# Purpose: single variable negative binomial regression for total mortality   #
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
df <- read.csv(args$data, header = TRUE)
cat("\n")
cat("\n")

cat("Preview of data file...\n")
glimpse(df)

cat("Loading single variable function...\n")
source("/group/deckerlab/cjgwx7/sensor-data/scripts/r/source/single_variable_negbin_ar1.R")

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

df <- df %>%
    rename(DepVar = TotalMortality_Lag0,
           Covariate = Days_Lag0,
           Offset = Inventory_Lag0) %>%
    select(-contains("Inventory"), -contains("Marketing"),
           -contains("Euth"), -contains("Dead"),
           -contains("TotalMortality"), -contains("Days"), -contains("GP"),
           -contains("WaterDispRMS"), -contains("WaterDispVC")) %>%
    rename(TotalMortality_Lag0 = DepVar,
           Days_Lag0 = Covariate,
           Inventory_Lag0 = Offset) %>%
    select(Order:GrowthPeriod, Days_Lag0, Inventory_Lag0, TotalMortality_Lag0,
           Penicillin_Lag0:Color_ReHS_Red_Lag21)

cat("Fitting regression models...\n")
fit_df <- single_variable_negbin_ar1(df = df, y = "TotalMortality_Lag0", x_pos = c(18:ncol(df)), covariate = "GrowthPeriod", int = FALSE, random_effect = "Site_Turn")

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
save(fit_df, file = rdata_export)
cat("\n")
cat("\n")

cat(paste("Exporting .csv file to path:\n", csv_export, sep = ""))
write_csv(fit_df,
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