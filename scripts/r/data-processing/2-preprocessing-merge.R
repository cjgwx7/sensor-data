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

#### Extract all possible file names: PRODUCTION
production_files <- list.files("/group/deckerlab/cjgwx7/sensor-data/data/production/processed",
                               pattern = "\\.RData$")
#### Find the maximum MAXDATE
production_dates <- str_extract(production_files, pattern = "(?<=MAXDATE=).*(?=.RData)")
production_dates <- as.Date(production_dates)
production_max_date <- max(production_dates)
#### Subset files with maximum MAXDATE
production_input <- grep(paste("MAXDATE=", production_max_date, sep = ""), production_files, value = TRUE)
#### Find maximum RUN date
production_rundates <- str_extract(production_input, pattern = "(?<=RUN=).*(?=_MAXDATE)")
production_rundates <- as.POSIXct(production_rundates, format = "%Y-%m-%d-%H-%M-%S")
production_max_rundate <- as.character(max(production_rundates))
production_max_rundate <- gsub(" ", "-", production_max_rundate)
production_max_rundate <- gsub(":", "-", production_max_rundate)
#### Subset files with maximum RUN datetime from production_input
production_input <- grep(paste("RUN=", production_max_rundate, sep = ""), production_input, value = TRUE)
#### Make target file path
production_root <- "/group/deckerlab/cjgwx7/sensor-data/data/production/processed"
production_target <- file.path(production_root, production_input)

#### Extract all possible file names: COUGH
cough_files <- list.files("/group/deckerlab/cjgwx7/sensor-data/data/cough/processed",
                          pattern = "\\.RData$")
#### Find the maximum MAXDATE
cough_dates <- str_extract(cough_files, pattern = "(?<=MAXDATE=).*(?=.RData)")
cough_dates <- as.Date(cough_dates)
cough_max_date <- max(cough_dates)
#### Subset files with maximum MAXDATE
cough_input <- grep(paste("MAXDATE=", cough_max_date, sep = ""), cough_files, value = TRUE)
#### Find maximum RUN date
cough_rundates <- str_extract(cough_input, pattern = "(?<=RUN=).*(?=_MAXDATE)")
cough_rundates <- as.POSIXct(cough_rundates, format = "%Y-%m-%d-%H-%M-%S")
cough_max_rundate <- as.character(max(cough_rundates))
cough_max_rundate <- gsub(" ", "-", cough_max_rundate)
cough_max_rundate <- gsub(":", "-", cough_max_rundate)
#### Subset files with maximum RUN datetime from cough_input
cough_input <- grep(paste("RUN=", cough_max_rundate, sep = ""), cough_input, value = TRUE)
#### Make target file path
cough_root <- "/group/deckerlab/cjgwx7/sensor-data/data/cough/processed"
cough_target <- file.path(cough_root, cough_input)

cat(paste("Loading production file run version ", production_max_rundate,
          " with records up to ",
          production_max_date,
          "...\n",
          sep = ""))
load(production_target)

cat(paste("Loading cough file run version ", cough_max_rundate,
          " with records up to ",
          cough_max_date,
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

master_root <- "/group/deckerlab/cjgwx7/sensor-data/data/master"
master_dates <- c(production_max_date, cough_max_date)
master_min_date <- min(master_dates)
run_date <- paste(format(Sys.Date(), "%Y-%m-%d"),
                  "-",
                  format(Sys.time(), "%H-%M-%S"),
                  sep = "")

master_filename_rdata <- paste("master-",
                               "RUN=", run_date, "_",
                               "MAXDATE=", master_min_date,
                               ".RData",
                               sep = "")
master_filename_rdata <- file.path(master_root, master_filename_rdata)

master_filename_csv <- paste("master-",
                               "RUN=", run_date, "_",
                               "MAXDATE=", master_min_date,
                               ".csv",
                               sep = "")
master_filename_csv <- file.path(master_root, master_filename_csv)

cat(paste("Exporting master file with dates up to ",
          master_min_date,
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