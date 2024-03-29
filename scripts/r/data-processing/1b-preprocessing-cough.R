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
          "     BEGIN: DIRECTORY CONFIGURATION",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

target_file <- commandArgs(trailingOnly = TRUE)[1]
print(paste("Target file:", target_file, sep = " "))

file_name <- str_sub(target_file, -14, -1)
print(paste("File name:", file_name, sep = " "))

date <- str_sub(file_name, 1, 10)
print(paste("File date:", date, sep = " "))

root_dir <- str_remove(target_file, paste("/", file_name, sep = ""))
print(paste("Root directory:", root_dir, sep = " "))

dir_check <- file.path(root_dir, date)
print(paste("Directory check:", dir_check, sep = " "))

if (dir.exists(dir_check) == FALSE) {

      cat("Unzipping raw data files\n")
      dir.create(dir_check)
      unzip(target_file, exdir = dir_check)
      file_list <- list.files(file.path(dir_check, "input"))
      site_codes <- lapply(file_list, str_remove, "Maschhoff_")
      site_codes <- lapply(site_codes, str_trim, "left")
      site_codes <- unlist(lapply(site_codes, substring, 1, 4))

      for (i in seq_len(6)) {

            dir.create(file.path(dir_check, "input", site_codes[i]))
            unzip(file.path(dir_check, "input", file_list[i]),
                  exdir = file.path(dir_check, "input", site_codes[i]))

      }

      file.rename(from = target_file,
                  to = file.path(root_dir, date, file_name))

} else {

      cat("Raw data files already exist - moving onto data processing...\n")

}

###############################################################################

cat("\n")

cat(paste("########################################",
          "      END: DIRECTORY CONFIGURATION",
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

dir_list <- list.dirs(file.path(root_dir, date, "input"), recursive = FALSE)
site_codes <- list.dirs(file.path(root_dir, date, "input"),
                        full.names = FALSE,
                        recursive = FALSE)

cat("Reading in device files for each site...\n")
df_list <- list()

for (i in seq_len(6)) {

      df_tmp <- read_csv(file.path(dir_list[i], "devices_combined_list.csv"),
                         col_select = c(1:8),
                         col_names = FALSE,
                         col_types = c("dccccdcd"),
                         skip = 1,
                         trim_ws = TRUE) %>%
            rename(Device = X2,
                   CalendarDate = X4,
                   Temp_ReHS = X6,
                   Color_ReHS = X7,
                   ReHS = X8) %>%
            mutate(Site = site_codes[i])

      df_list[[i]] <- df_tmp

}

cat("Binding site device files into a single file...\n")
cough <- bind_rows(df_list)

cat("Printing raw data set structure...\n")
glimpse(cough)

###############################################################################

cat("\n")

cat(paste("########################################",
          "         END: READ IN DATA",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

cat(paste("########################################",
          "       BEGIN: DATA PROCESSING",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

cat("Processing data...\n")
### EXTRACT AND BUILD UNIQUE IDENTIFIER
cough_v2 <- cough %>%
      # Date converison
      mutate(CalendarDate = as.Date(CalendarDate, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")),
             NumericDate = as.numeric(CalendarDate - as.Date("1899-12-30"))) %>%
      # Extract room number
      mutate(Room = str_remove(Device, "Maschhoff_"),
             Room = str_remove(Room, "Masc_"),
             Room = substring(Room, 6),
             Room = str_extract(Room, "\\d+")) %>%
      # Build unique identifier
      mutate(GlobalID = paste(Site, "_", Room, "_", NumericDate, sep = "")) %>%
      # Calculate aggregated temperature and cough scores
      group_by(GlobalID) %>%
      summarise(Temp_ReHS = mean(Temp_ReHS, na.rm = TRUE),
                ReHS = mean(ReHS, na.rm = TRUE)) %>%
      mutate(Temp_ReHS = ifelse(is.nan(Temp_ReHS), NA, Temp_ReHS),
             Temp_ReHS = (Temp_ReHS * (9 / 5)) + 32,
             ReHS = ifelse(is.nan(ReHS), NA, ReHS),
             Color_ReHS = ifelse(ReHS < 40, "Red",
                                 ifelse(ReHS > 59, "Green", "Yellow")),
             Color_ReHS = factor(Color_ReHS),
             Site = substring(GlobalID, 1, 4)) %>%
             dplyr::select(GlobalID, Temp_ReHS, ReHS, Color_ReHS, Site)

cat("Printing overall summary statistics before QC...\n")
summary(cough_v2[, 2:4])

cat("Printing site summary statistics before QC...\n")
by(cough_v2[, 2:4], cough_v2$Site, summary)

cat("Performing quality control...\n")
cough_v3 <- cough_v2 %>%
      mutate(Temp_ReHS = ifelse(Temp_ReHS < 45 | Temp_ReHS > 105, NA, Temp_ReHS),
             ReHS = ifelse(ReHS < 1 | ReHS > 99, NA, ReHS))

cat("Printing overall summary statistics after QC...\n")
summary(cough_v3[, 2:4])

cat("Printing site summary statistics after QC...\n")
by(cough_v3[, 2:4], cough_v3$Site, summary)

cat("Printing clean dataset...\n")
glimpse(cough_v3)

###############################################################################

cat("\n")

cat(paste("########################################",
          "        END: DATA PROCESSING",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

cat(paste("########################################",
          "         BEGIN: FILE EXPORT",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

cat("Exporting cough data sets...\n")
received_date <- as.Date(date, format = "%m-%d-%Y")
run_date <- paste(format(Sys.Date(), "%Y-%m-%d"),
                  "-",
                  format(Sys.time(), "%H-%M-%S"),
                  sep = "")
max_date <- unname(unlist(cough_v3[, "GlobalID"]))
max_date <- max(as.integer(substring(max_date, 8, 12)))
max_date <- as.Date(max_date, origin = "1899-12-30")

export_root_dir <- "/group/deckerlab/cjgwx7/sensor-data/data/cough/processed-indiv"
export_filename <- paste("cough-",
                         "DATE=", received_date, "_",
                         "RUN=", run_date, "_",
                         "MAXDATE=", max_date,
                         ".csv", sep = "")
export_filename_rdata <- paste("cough-",
                         "DATE=", received_date, "_",
                         "RUN=", run_date, "_",
                         "MAXDATE=", max_date,
                         ".RData", sep = "")

write_csv(cough_v3,
          file = file.path(export_root_dir, export_filename))

save(cough_v3,
     file = file.path(export_root_dir, export_filename_rdata))

###############################################################################

cat("\n")

cat(paste("########################################",
          "           END: FILE EXPORT",
          "########################################",
          sep = "\n"),
    sep = "\n")
    
cat("\n")

cat(paste("########################################",
          "         BEGIN: DEDUPLICATION",
          "########################################",
          sep = "\n"),
    sep = "\n")

cat("\n")

###############################################################################

processed_files_path <- "/group/deckerlab/cjgwx7/sensor-data/data/cough/processed-indiv"
export_processed_file_root_dir <- "/group/deckerlab/cjgwx7/sensor-data/data/cough/processed-combined"

processed_files <- list.files(path = processed_files_path, full.names = TRUE)
processed_files <- grep(".RData", processed_files, value = TRUE)

output_processed_file <- processed_files %>%
  purrr::map_dfr(~ get(load(.))) %>%
  dplyr::mutate(Room = as.numeric(gsub(".*[_]([^.]+)[_].*", "\\1", GlobalID)),
                Date = as.numeric(substring(GlobalID, 8, 13))) %>%
  dplyr::arrange(Site, Room, Date) %>%
  dplyr::group_by(Site, Room, Date) %>%
  dplyr::summarise(Temp_ReHS = mean(Temp_ReHS, na.rm = TRUE),
                   ReHS = mean(ReHS, na.rm = TRUE)) %>%
  dplyr::ungroup(.) %>%
  dplyr::mutate(Color_ReHS = ifelse(ReHS <= 99 & ReHS >= 60, "Green",
                                    ifelse(ReHS < 60 & ReHS >= 40, "Yellow", "Red")),
                Temp_ReHS = ifelse(is.nan(Temp_ReHS) == TRUE, NA, Temp_ReHS),
                ReHS = ifelse(is.nan(ReHS) == TRUE, NA, ReHS),
                GlobalID = paste(Site, Room, Date, sep = "_")) %>%
  dplyr::select(GlobalID, Temp_ReHS, ReHS, Color_ReHS, Site)

write_csv(output_processed_file,
          file = file.path(export_processed_file_root_dir, export_filename))

save(output_processed_file,
     file = file.path(export_processed_file_root_dir, export_filename_rdata))

###############################################################################

cat("\n")

cat(paste("########################################",
          "           END: DEDUPLICATION",
          "########################################",
          sep = "\n"),
    sep = "\n")
    
cat("\n")

print(paste("The current script was completed on",
            format(Sys.Date(), "%A, %B %d, %Y"),
            "at",
            format(Sys.time(), "%I:%M %p")))