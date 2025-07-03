# --------------------------------
# loading in libraries
# --------------------------------
invisible(
  c(
    "zeallot", # multi unpacker (I am lazy)
    "lubridate", # makes date handling easier
    "readxl", # excel import
    "readr", # read in files other than excel
    "dplyr", # data frame manipulation
    "tidyr",
    "purrr", # map/other functions for working with lists
    "stringr", # working with strings
    "ggplot2", # plotting
    "cowplot", # meta plot formatting
    "forcats", # working with factors
    "rlang", # string/var interchange (!!sym() stuff)
    "GGally", #TODO: really only want ggpairs from this (DO I?)
    "paletteer" #TODO actually use this or dump it
  ) |>
    lapply(function(x) {
      if (suppressMessages(!require(x, character.only = TRUE))) {
        install.packages(x)
        library(x, character.only = TRUE)
      }
    })
)

# --------------------------------
# setting paths and creating directories
# --------------------------------
common_path <- "~/Desktop/Soil_Work/"
output <- paste0(common_path, "cleaned_data/")
scripts_path <- paste0(common_path, "Soil_Analysis/analysis/R/")
setwd(scripts_path)
dir.create(output, showWarnings = FALSE)

# --------------------------------
# Importing Datasets
# --------------------------------
source(paste0(scripts_path, "file_handler.R"))

# grabs list of excel files + separates old vs newer format
c(old_files, new_files) %<-% import_excel_files(paste0(common_path, "drive_data"))

# separates pits and frost datasets in file lists
c(old_pits, old_frost) %<-% separate_datasets(old_files)
c(new_pits, new_frost) %<-% separate_datasets(new_files)
rm(old_files, new_files)

# handle oldest 2011-2022 format
oldest_files <- oldest_files_handler(paste0(common_path, "other_data"))
rm(import_excel_files, separate_datasets, oldest_files_handler)

# --------------------------------
# Process Pits Dataset
# --------------------------------
source(paste0(scripts_path, "pits.R"))
c(pits_data, pits_data_removed) %<-% full_handle_pits(new_pits, old_pits, oldest_files)

rm(
  create_pits_new,
  create_pits_old,
  create_pits_oldest,
  process_pits,
  old_pits,
  new_pits,
  oldest_files,
  full_handle_pits
)

# --------------------------------
# Process Frost Dataset
# --------------------------------
source(paste0(scripts_path, "frost.R"))
c(frost_data, frost_data_removed) %<-% full_handle_frost(new_frost, old_frost)

rm(
  create_frost_new,
  create_frost_old,
  process_frost,
  old_frost,
  new_frost,
  full_handle_frost
)

# --------------------------------
# Combine + Analyze Full Dataset
# --------------------------------
source(paste0(scripts_path, "analysis_helper.R"))
# exploratory factor/numeric data

full_explore_output(pits_data, paste0(output, "pits_exploratory.png"))
full_explore_output(frost_data, paste0(output, "frost_exploratory.png"))
rm(missing_plot, factor_bar, numeric_hist, full_explore_output, pairs_plots, theme_custom)

write_csv(frost_data, file = paste0(output, "lightly_cleaned_frost_data.csv"))
write_csv(pits_data, file = paste0(output, "lightly_cleaned_pits_data.csv"))

write_csv(pits_data_removed, file = paste0(output, "pits_removed_data.csv"))
write_csv(frost_data_removed, file = paste0(output, "frost_removed_data.csv"))

# --------------------------------
# Prepare Datasets for Upload
# --------------------------------

# subset data to what will be exported as well as remove calculated columns
source(paste0(scripts_path, "calculations.R"))
frost_data <- frost_data |>
  select(-c(initials, notes, source_file))

pits_data <- pits_data |>
  select(
    -c(
      initials,
      notes,
      source_file,
      photo_taken,
      scale_photo_taken,
      snow_density_kilograms_meters_cubed,
      snow_water_equivalent_millimeters,
      albedo
    )
  )

#TODO: make sure data does not vary significantly between old/new calcs
pits_data_1 <- pits_calculations(pits_data)

# --------------------------------
# Upload
# --------------------------------

# send to dashboard
dashboard_path <- paste0(common_path, "Soil_Analysis/dashboard/data/")
saveRDS(pits_data, paste0(dashboard_path, "pits_data.RData"))
saveRDS(frost_data, paste0(dashboard_path, "frost_data.RData"))

# save as csv for doi upload
