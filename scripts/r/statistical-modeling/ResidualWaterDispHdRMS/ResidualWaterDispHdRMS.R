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
cat("Loading package: openxlsx\n")
library(openxlsx)
cat("Loading package: readr\n")
library(readr)
cat("Loading package: stringr\n")
library(stringr)
cat("Loading package: tibble\n")
library(tibble)
cat("Loading package: tidyr\n")
library(tidyr)

rm(list = ls())

cat("Loading source function: cross_val.R\n")
source("/group/deckerlab/cjgwx7/sensor-data/scripts/r/source/cross_val.R")

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
parser$add_argument("--fit-list",
                    help = "CSV file with list of model fit specifications",
                    required = TRUE)
parser$add_argument("--results",
                    help = "XLSX file with list of model fit CV results",
                    required = TRUE)
args <- parser$parse_args()

load(args$data)

master <- master %>%
    dplyr::select(GlobalID, Site, Days, Inventory,
                  MidPointTempSetPointDeviationRMS, WaterDispHdRMS) %>%
    mutate(Site = as.character(Site)) %>%
    drop_na()

cat("First five rows of configured data set...\n")
head(master, 5)
cat("\n")

fit_list <- read_csv(args$fit_list,
                     show_col_types = FALSE) %>%
    mutate(across(everything(), as.character))
fit_list <- fit_list %>%
    unite("fit", c(2:ncol(fit_list)), sep = " + ", na.rm = TRUE) %>%
    mutate(fit = paste(DependentVariable, " ~ ", fit, sep = "")) %>%
    pull(fit)
fit_list <- as.list(fit_list)
fit_list <- lapply(fit_list, as.formula)

cat("First five rows of fit specification list...\n")
head(fit_list, 5)
cat("\n")

###############################################################################

cat("\n")

cat(paste("########################################",
          "      END: FILE CONFIGURATION",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

cat(paste("########################################",
          "        BEGIN: MODEL EVALUTION",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

eval_list <- list()

for (i in seq_len(length(fit_list))) {

    for (j in c("lm", "robust")) {

        eval_object <- cross_val(df = master, var = "Site", fit_class = j, fit_formula = fit_list[[i]])
        i_j <- paste(i, "_", j, sep = "")
        eval_list[[i_j]] <- eval_object$eval_metrics

    }

}

results <- bind_rows(eval_list)

cat("First five rows of results file...\n")
head(results, 5)
cat("\n")

results <- results %>%
    group_by(fit, model) %>%
    summarise(`Mean RMSE` = mean(RMSE),
              `SD RMSE` = sd(RMSE))

cat("First five rows of sumarized results file...\n")
head(results, 5)
cat("\n")

write.xlsx(results,
           file = args$results)

###############################################################################

cat("\n")

cat(paste("########################################",
          "         END: MODEL EVALUTION",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

print(paste("The current script was completed on",
            format(Sys.Date(), "%A, %B %d, %Y"),
            "at",
            format(Sys.time(), "%I:%M %p")))