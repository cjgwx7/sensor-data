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

source("/group/deckerlab/cjgwx7/sensor-data/scripts/r/source/find_high_mortality.R")

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

master <- master %>%
    mutate(StdDays = (Days - mean(Days, na.rm = TRUE)) / sd(Days, na.rm = TRUE))

master_e <- master %>%
    filter(GrowthPeriod == "Early")

master_em <- master %>%
     filter(GrowthPeriod == "Early Middle")

master_lm <- master %>%
    filter(GrowthPeriod == "Late Middle")

master_l <- master %>%
    filter(GrowthPeriod == "Late")

### WaterDispHdRMS
#cat("Fitting statistical model for variable: WaterDispHdRMS...\n")
#fit <- lmer(WaterDispHdRMS ~ MidPointTempSetPointDeviationRMS +
#                             I(MidPointTempSetPointDeviationRMS^2) +
#                             (1 | Site) +
#                             (1 | Site:Marketing) +
#                             (0 + StdInventory + I(StdInventory^2) + I(StdInventory^3) + StdDays + I(StdDays^2) + I(StdDays^3) | Site),
#          data = master_std,
#          na.action = na.exclude)

#cat("Printing model fit summary for variable: WaterDispHdRMS...\n")
#summary(fit)

#cat("Extracting and saving residuals from model fit to new variable: ResidualWaterDispHdRMS...\n")
#master_v2 <- master %>%
#    mutate(ResidualWaterDispHdRMS = unname(residuals(fit))) %>%
#    dplyr::select(Order:WaterDispHdRMS,
#                  ResidualWaterDispHdRMS,
#                  WaterDispVC:Color_ReHS)
#cat("\n")

### WaterDispHdVC
#cat("Fitting statistical model for variable: WaterDispHdVC...\n")
#fit <- lmer(WaterDispHdVC ~  AvgTempSetPointDeviationVC +
#                             I(AvgTempSetPointDeviationVC^2) +
#                             (1 | Site) +
#                             (1 | Site:Marketing) +
#                             (0 + StdInventory + I(StdInventory^2) + I(StdInventory^3) + StdDays + I(StdDays^2) + I(StdDays^3) | Site),
#          data = master_std,
#          na.action = na.exclude)

#cat("Printing model fit summary for variable: WaterDispHdVC...\n")
#summary(fit)

#cat("Extracting and saving residuals from model fit to new variable: ResidualWaterDispHdVC...\n")
#master_v3 <- master_v2 %>%
#    mutate(ResidualWaterDispHdVC = unname(residuals(fit))) %>%
#    dplyr::select(Order:WaterDispHdVC,
#                  ResidualWaterDispHdVC,
#                  SetPointRMS:Color_ReHS)
#cat("\n")

### Total Mortality
cat(paste("Fitting mixed (FE: offset(log(Inventory))) RE: Cubic Days and Site_Turn) ",
          "negative binomial model for variable: TotalMortality...\n",
          sep = ""))
fit <- glmer.nb(TotalMortality ~ offset(log(Inventory)) +
                                 (1 + StdDays + I(StdDays^2) + I(StdDays^3) | Site_Turn),
                data = master,
                na.action = na.exclude)
cat("\n")

cat("Printing model fit summary for variable: TotalMortality...\n")
summary(fit)
cat("\n")

cat("Extracting and saving residuals from model fit to new variable: ResidualTotalMortality_CubicDaysSiteTurn...\n")
master <- master %>%
    mutate(ResidualTotalMortality_CubicDaysSiteTurn = unname(residuals(fit, type = "response")))
cat("----------------------------------------------------------------------------------------------\n")

###################### NEW MODEL

cat(paste("Fitting mixed (FE: offset(log(Inventory)) & GrowthPeriod RE: Site_Turn) ",
          "negative binomial model for variable: TotalMortality...\n",
          sep = ""))
fit <- glmer.nb(TotalMortality ~ offset(log(Inventory)) + GrowthPeriod +
                                 (1 | Site_Turn),
                data = master,
                na.action = na.exclude)
cat("\n")

cat("Printing model fit summary for variable: TotalMortality...\n")
summary(fit)
cat("\n")

cat("Extracting and saving residuals from model fit to new variable: ResidualTotalMortality_FixedGrowthPeriodRandomSiteTurn...\n")
master <- master %>%
    mutate(ResidualTotalMortality_FixedGrowthPeriodRandomSiteTurn = unname(residuals(fit, type = "response")))
cat("----------------------------------------------------------------------------------------------\n")

###################### NEW MODEL

cat(paste("Fitting fixed [FE: offset(log(Inventory))] ",
          "negative binomial model for variable: TotalMortality...\n",
          sep = ""))
fit <- glm.nb(TotalMortality ~ offset(log(Inventory)) + GrowthPeriod,
              data = master,
              na.action = na.exclude)
cat("\n")

cat("Printing model fit summary for variable: TotalMortality...\n")
summary(fit)
cat("\n")

cat("Extracting and saving residuals from model fit to new variable: ResidualTotalMortality_FixedGrowthPeriod...\n")
master <- master %>%
    mutate(ResidualTotalMortality_FixedGrowthPeriod = unname(residuals(fit, type = "response")))
cat("----------------------------------------------------------------------------------------------\n")

###################### NEW MODEL

cat(paste("Fitting mixed [FE: offset(log(Inventory)) & RE: Site_Turn] ",
          "negative binomial model for variable: TotalMortality (Early)...\n",
          sep = ""))
fit <- glmer.nb(TotalMortality ~ offset(log(Inventory)) +
                    (1 | Site_Turn),
                data = master_e,
                na.action = na.exclude)
cat("\n")

cat("Printing model fit summary for variable: TotalMortality...\n")
summary(fit)
cat("\n")

cat("Extracting and saving residuals from model fit to new variable: ResidualTotalMortality_E...\n")
master_e <- master_e %>%
    mutate(ResidualTotalMortality_E = unname(residuals(fit, type = "response"))) %>%
    dplyr::select(Order, ResidualTotalMortality_E)
cat("----------------------------------------------------------------------------------------------\n")

###################### NEW MODEL

cat(paste("Fitting mixed [FE: offset(log(Inventory)) & RE: Site_Turn] ",
          "negative binomial model for variable: TotalMortality (Early Middle)...\n",
          sep = ""))
fit <- glmer.nb(TotalMortality ~ offset(log(Inventory)) +
                    (1 | Site_Turn),
                data = master_em,
                na.action = na.exclude)
cat("\n")

cat("Printing model fit summary for variable: TotalMortality...\n")
summary(fit)
cat("\n")

cat("Extracting and saving residuals from model fit to new variable: ResidualTotalMortality_EM...\n")
master_em <- master_em %>%
    mutate(ResidualTotalMortality_EM = unname(residuals(fit, type = "response"))) %>%
    dplyr::select(Order, ResidualTotalMortality_EM)
cat("----------------------------------------------------------------------------------------------\n")

###################### NEW MODEL

cat(paste("Fitting mixed [FE: offset(log(Inventory)) & RE: Site_Turn] ",
          "negative binomial model for variable: TotalMortality (Late Middle)...\n",
          sep = ""))
fit <- glmer.nb(TotalMortality ~ offset(log(Inventory)) +
                    (1 | Site_Turn),
                data = master_lm,
                na.action = na.exclude)
cat("\n")

cat("Printing model fit summary for variable: TotalMortality...\n")
summary(fit)
cat("\n")

cat("Extracting and saving residuals from model fit to new variable: ResidualTotalMortality_LM...\n")
master_lm <- master_lm %>%
    mutate(ResidualTotalMortality_LM = unname(residuals(fit, type = "response"))) %>%
    dplyr::select(Order, ResidualTotalMortality_LM)
cat("----------------------------------------------------------------------------------------------\n")

###################### NEW MODEL

cat(paste("Fitting mixed [FE: offset(log(Inventory)) & RE: Site_Turn] ",
          "negative binomial model for variable: TotalMortality (Late)...\n",
          sep = ""))
fit <- glmer.nb(TotalMortality ~ offset(log(Inventory)) +
                    (1 | Site_Turn),
                data = master_l,
                na.action = na.exclude)
cat("\n")

cat("Printing model fit summary for variable: TotalMortality...\n")
summary(fit)
cat("\n")

cat("Extracting and saving residuals from model fit to new variable: ResidualTotalMortality_L...\n")
master_l <- master_l %>%
    mutate(ResidualTotalMortality_L = unname(residuals(fit, type = "response"))) %>%
    dplyr::select(Order, ResidualTotalMortality_L)
cat("----------------------------------------------------------------------------------------------\n")

cat("Finding mortality events...\n")
master <- master %>%
    left_join(., master_e, by = "Order") %>%
    left_join(., master_em, by = "Order") %>%
    left_join(., master_lm, by = "Order") %>%
    left_join(., master_l, by = "Order") %>%
    mutate(EHMD_CubicDays = find_high_mortality(ResidualTotalMortality_CubicDaysSiteTurn, 3),
           EHMD_GPST = find_high_mortality(ResidualTotalMortality_FixedGrowthPeriodRandomSiteTurn, 3),
           EHMD_GP = find_high_mortality(ResidualTotalMortality_FixedGrowthPeriod, 3),
           EHMD_E = find_high_mortality(ResidualTotalMortality_E, 3),
           EHMD_EM = find_high_mortality(ResidualTotalMortality_EM, 3),
           EHMD_LM = find_high_mortality(ResidualTotalMortality_LM, 3),
           EHMD_L = find_high_mortality(ResidualTotalMortality_L, 3),
           EHMD_WGP = coalesce(EHMD_E, EHMD_EM, EHMD_LM, EHMD_L)) %>%
    dplyr::select(Order:TotalMortalityProportion, EHMD_CubicDays:EHMD_GP, EHMD_WGP, Penicillin:Color_ReHS) %>%
    mutate(Color_ReHS_Yellow = ifelse(Color_ReHS == "Yellow", 1, 0),
           Color_ReHS_Red = ifelse(Color_ReHS == "Red", 1, 0),
           Color_ReHS_Green_Red = ifelse(Color_ReHS == "Green", 1,
                                         ifelse(Color_ReHS == "Red", -1, 0)),
           Color_ReHS_Green_Yellow = ifelse(Color_ReHS == "Green", 1,
                                            ifelse(Color_ReHS == "Yellow", -1, 0)),
           Color_ReHS_Yellow_Red = ifelse(Color_ReHS == "Yellow", 1,
                                          ifelse(Color_ReHS == "Red", -1, 0))) %>%
    dplyr::select(Order:ReHS, Color_ReHS:Color_ReHS_Yellow_Red) %>%
    mutate(across(c(Days:ReHS, Color_ReHS_Yellow:Color_ReHS_Yellow_Red), as.double),
           Color_ReHS = factor(Color_ReHS,
                               levels = c("Green", "Yellow", "Red")))

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