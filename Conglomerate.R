# --------------------------------
# loading in libraries
# --------------------------------
invisible(
  c(
    "lubridate",
    "readxl",
    "readr",
    "dplyr",
    "tidyr",
    "purrr",
    "stringr",
    "ggplot2",
    "cowplot",
    "rlang"
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
dir.create(output, showWarnings = FALSE)

# --------------------------------
# Importing Datasets
# --------------------------------
source(paste0(common_path, "Soil_Analysis/file_handler.R"))

# grabs list of excel files + separates old vs newer format
all_files <- import_excel_files(paste0(common_path, "drive_data"))
old_files <- unlist(all_files[1])
new_files <- unlist(all_files[2])
rm(all_files)

# separates pits and frost datasets in file lists
all_old_files <- separate_datasets(old_files)
old_pits <- unlist(all_old_files[1])
old_frost <- unlist(all_old_files[2])
all_new_files <- separate_datasets(new_files)
new_pits <- unlist(all_new_files[1])
new_frost <- unlist(all_new_files[2])
rm(old_files, new_files, all_old_files, all_new_files)

# handle oldest 2011-2022 format
oldest_files <- oldest_files_handler(paste0(common_path, "other_data"))
rm(import_excel_files, separate_datasets, oldest_files_handler)

# --------------------------------
# Process Pits Dataset
# --------------------------------
source(paste0(common_path, "Soil_Analysis/pits.R"))
new_pits <- create_pits_new(new_pits)
old_pits <- create_pits_old(old_pits)
oldest_pits <- create_pits_oldest(oldest_files)
pits_data <- full_join(new_pits, old_pits)
pits_data <- full_join(pits_data, oldest_pits) # this may be easier than bind_rows as that requires ordered rows
pits_data <- process_pits(pits_data)
rm(
  create_pits_new,
  create_pits_old,
  process_pits,
  old_pits,
  new_pits
)

# --------------------------------
# Process Frost Dataset
# --------------------------------
source(paste0(common_path, "Soil_Analysis/frost.R"))
new_frost <- create_frost_new(new_frost)
old_frost <- create_frost_old(old_frost)
frost_data <- full_join(new_frost, old_frost)
frost_data <- process_frost(frost_data)
rm(
  create_frost_new,
  create_frost_old,
  process_frost,
  old_frost,
  new_frost
)

# --------------------------------
# Combine + Analyze Full Dataset
# --------------------------------
source(paste0(common_path, "Soil_Analysis/analysis_helper.R"))
# exploratory factor/numeric data
full_explore_output(pits_data, paste0(output, "pits_exploratory.png"))
full_explore_output(frost_data, paste0(output, "frost_exploratory.png"))
rm(missing_plot, factor_bar, numeric_hist, full_explore_output)
