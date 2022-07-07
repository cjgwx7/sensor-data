###############################################################################
#                                                                             #
# Author: Caleb J. Grohmannn                                                  #
# Date: 7/7/2022                                                              #
# Purpose: data preprocessing for SoundTalks data communicated by             #
# Dr. Erin Lowe, which includes Respiratory Health Status and                 #
# temperature data                                                            #
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