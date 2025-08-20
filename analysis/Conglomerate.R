# --------------------------------
# loading in libraries
# --------------------------------
# --------------------------------
# setting paths and creating directories
# --------------------------------
common_path <- "~/Desktop/Soil_Work/"
output <- paste0(common_path, "cleaned_data/")
scripts_path <- paste0(common_path, "Soil_Analysis/analysis/R/")
setwd(scripts_path)

# create output directory
dir.create(output, showWarnings = FALSE)

# import libraries
source("globals.R")

# --------------------------------
# Importing Datasets
# --------------------------------
source("file_handler.R")

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
source("pits.R")
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
source("frost.R")
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
source("analysis_helper.R")
# exploratory factor/numeric data

full_explore_output(pits_data, paste0(output, "pits_exploratory.png"))
full_explore_output(frost_data, paste0(output, "frost_exploratory.png"))
rm(missing_plot, factor_bar, numeric_hist, pairs_plots, full_explore_output, theme_custom)

write_csv(frost_data, file = paste0(output, "lightly_cleaned_frost_data.csv"))
write_csv(pits_data, file = paste0(output, "lightly_cleaned_pits_data.csv"))

write_csv(pits_data_removed, file = paste0(output, "pits_removed_data.csv"))
write_csv(frost_data_removed, file = paste0(output, "frost_removed_data.csv"))

# --------------------------------
# Prepare Datasets for Upload
# --------------------------------

# subset data to what will be exported as well as remove calculated columns
source("calculations.R")

frost_data_filtered <- frost_data |>
  select(-c(initials, notes, source_file)) |> # get rid of unused columns
  filter(frost_tube_id %in% c("1", "2", "3")) # only keep tubes with ids 1/2/3 (others are for other experiments)

pits_data_filtered <- pits_data |>
  select(
    -c(
      # unused columns below
      initials,
      notes,
      source_file,
      photo_taken,
      scale_photo_taken,
      cma6_albedo,
      cocorahs_albedo,
      surface_skin_temp_celcius,
      Layer1Depth,
      Layer2Depth,
      Layer3Depth,
      LayerThickness,
      Layer2thickness,
      Layer3Thickness,
      layer_1_depth_centimeters,
      layer_2_depth_centimeters,
      layer_3_depth_centimeters,
      layer_1_density_kilograms_meters_cubed,
      layer_2_density_kilograms_meters_cubed,
      layer_3_density_kilograms_meters_cubed,
      # recalculating below
      snow_density_kilograms_meters_cubed,
      snow_water_equivalent_millimeters,
      albedo
    )
  ) |>
  filter(!is.na(date))

#TODO: make sure data does not vary significantly between old/new calcs
pits_data_filtered <- pits_calculations(pits_data_filtered)

source("analysis_helper.R")
full_explore_output(pits_data_filtered, paste0(output, "pits_filtered_exploratory.png"))
full_explore_output(frost_data_filtered, paste0(output, "frost_filtered_exploratory.png"))
rm(missing_plot, factor_bar, numeric_hist, pairs_plots, full_explore_output, theme_custom)

# manual comparison between excel and R column calculations.
# Outputs dfs showing different calculated values
# Does not show difference in 0,NaN,NA outputs! (ie one is 0, one is NaN)
#c(pits_diff, pits_recalc_diff) %<-%
#  compare_calcs(pits_data, pits_data_filtered, "snow_water_equivalent_millimeters")

# --------------------------------
# Upload
# --------------------------------

# send to dashboard

# create and send data to sqlite server
# will error if server already exists with same datasets
#db_connection <- dbConnect(RSQLite::SQLite(), paste0(common_path, "snow_soil_DB.db"))
#dbWriteTable(db_connection, "frost_data", frost_data_filtered)
#dbWriteTable(db_connection, "pits_data", pits_data_filtered)
## test server has data
#dbListTables(db_connection)
#dbGetQuery(db_connection, "SELECT snow_depth_centimeters FROM frost_data LIMIT 10")
#dbGetQuery(db_connection, "SELECT albedo FROM pits_data LIMIT 10")

# backup RData files
dashboard_path <- paste0(common_path, "Soil_Analysis/dashboard/data/")
saveRDS(pits_data_filtered, paste0(dashboard_path, "pits_data.RData"))
saveRDS(frost_data_filtered, paste0(dashboard_path, "frost_data.RData"))

# save as csv for doi upload
write.csv(pits_data_filtered, paste0(output, "pits_final_data.csv"))
write.csv(frost_data_filtered, paste0(output, "frost_final_data.csv"))
