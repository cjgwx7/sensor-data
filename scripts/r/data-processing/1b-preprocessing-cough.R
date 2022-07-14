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

target_file <- "/group/deckerlab/cjgwx7/sensor-data/data/cough/06-16-2022.zip"
file_name <- str_sub(target_file, -14, -1)
date <- str_sub(file_name, 1, 10)
root_dir <- str_remove(target_file, paste("/", file_name, sep = ""))
dir_check <- file.path(root_dir, date)


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

df_list <- list()

for (i in seq_len(6)) {

      df_tmp <- read_csv(file.path(dir_list[i], "devices_combined_list.csv"),
                         col_select = c(3, 5, 7:9),
                         col_names = FALSE,
                         col_types = c("ddccccdcd"),
                         skip = 1,
                         trim_ws = TRUE) %>%
            rename(Device = X3,
                   CalendarDate = X5,
                   Temp_ReHS = X7,
                   Color_ReHS = X8,
                   ReHS = X9) %>%
            mutate(Site = site_codes[i])

      df_list[[i]] <- df_tmp

}

cough <- bind_rows(df_list)

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