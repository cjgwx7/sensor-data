###############################################################################
#                                                                             #
# Author: Caleb J. Grohmannn                                                  #
# Date: 7/14/2022                                                             #
# Purpose: merge production and cough data files                              #
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

production_files <- list.files("/group/deckerlab/cjgwx7/sensor-data/data/production/processed",
                               pattern = "\\.RData$")
production_dates <- unlist(lapply(production_files, substring, 12))
production_dates <- as.Date(unlist(lapply(production_dates, substring, 1, 10)),
                            format = "%m-%d-%Y")
production_input <- production_files[which.max(production_dates)]
production_root <- "/group/deckerlab/cjgwx7/sensor-data/data/production/processed"
production_target <- file.path(production_root, production_input)


cough_files <- list.files("/group/deckerlab/cjgwx7/sensor-data/data/cough/processed",
                               pattern = "\\.RData$")
cough_dates <- unlist(lapply(cough_files, substring, 7))
cough_dates <- as.Date(unlist(lapply(cough_dates, substring, 1, 10)),
                       format = "%m-%d-%Y")
cough_input <- cough_files[which.max(cough_dates)]
cough_root <- "/group/deckerlab/cjgwx7/sensor-data/data/cough/processed"
cough_target <- file.path(cough_root, cough_input)

cat(paste("Loading production file from ",
          production_dates[which.max(production_dates)],
          "...\n",
          sep = ""))
load(production_target)

cat(paste("Loading cough file from ",
          cough_dates[which.max(cough_dates)],
          "...\n",
          sep = ""))
load(cough_target)

###############################################################################

cat("\n")

cat(paste("########################################",
          "          END: READ IN DATA",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

cat(paste("########################################",
          "          BEGIN: MERGE FILES",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

production <- production_v9
rm(production_v9)

cough <- cough_v3
rm(cough_v3)

production_v2 <- production %>%
      mutate(AltOldRoom = str_extract(OldRoom, "\\d+"),
             AltGlobalID = paste(Site, "_", AltOldRoom, "_", NumericDate,
                                 sep = ""))

cough_v2 <- cough %>%
      rename(AltGlobalID = GlobalID) %>%
      select(-Site)

cat("Merging files...\n")
master <- left_join(production_v2, cough_v2, by = "AltGlobalID") %>%
      select(Order, GlobalID, AltGlobalID,
             Site:HiTempSetPointDeviationVC,
             Temp_ReHS:Color_ReHS)

cat("Printing master data set...\n")
glimpse(master)

###############################################################################

cat("\n")

cat(paste("########################################",
          "          END: MERGE FILES",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

cat(paste("########################################",
          "          BEGIN: EXPORT DATA",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

production_dates <- production_dates[which.max(production_dates)]
cough_dates <- cough_dates[which.max(cough_dates)]

master_root <- "/group/deckerlab/cjgwx7/sensor-data/data/master"
master_dates <- c(production_dates, cough_dates)

master_filename_rdata <- paste("master-",
                               master_dates[which.min(master_dates)],
                               ".RData",
                               sep = "")
master_filename_rdata <- file.path(master_root, master_filename_rdata)

master_filename_csv <- paste("master-",
                             master_dates[which.min(master_dates)],
                             ".csv",
                             sep = "")
master_filename_csv <- file.path(master_root, master_filename_csv)

cat(paste("Exporting master file version ",
          master_dates[which.min(master_dates)],
          "...\n",
          sep = ""))

save(master,
     file = master_filename_rdata)

write_csv(master,
          file = master_filename_csv)

###############################################################################

cat("\n")

cat(paste("########################################",
          "          END: EXPORT DATA",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

print(paste("The current script was completed on",
            format(Sys.Date(), "%A, %B %d, %Y"),
            "at",
            format(Sys.time(), "%I:%M %p")))