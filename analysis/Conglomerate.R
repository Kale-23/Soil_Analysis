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
    "forcats",
    "rlang",
    "GGally", #TODO: really only want ggpairs from this
    "paletteer"
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
source(paste0(scripts_path, "pits.R"))
new_pits <- create_pits_new(new_pits)
old_pits <- create_pits_old(old_pits)
oldest_pits <- create_pits_oldest(oldest_files)
pits_data <- full_join(new_pits, old_pits)
pits_data <- full_join(pits_data, oldest_pits) # this may be easier than bind_rows as that requires ordered rows
pits_data <- process_pits(pits_data)
rm(
  create_pits_new,
  create_pits_old,
  create_pits_oldest,
  process_pits,
  oldest_pits,
  old_pits,
  new_pits,
  oldest_files
)

# --------------------------------
# Process Frost Dataset
# --------------------------------
source(paste0(scripts_path, "frost.R"))
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
source(paste0(scripts_path, "analysis_helper.R"))
# exploratory factor/numeric data

full_explore_output(pits_data, paste0(output, "pits_exploratory.png"))
full_explore_output(frost_data, paste0(output, "frost_exploratory.png"))
rm(missing_plot, factor_bar, numeric_hist, full_explore_output)

write_csv(frost_data, file = paste0(output, "lightly_cleaned_frost_data.csv"))
write_csv(pits_data, file = paste0(output, "lightly_cleaned_pits_data.csv"))

rm(pairs_plots, theme_custom)
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
