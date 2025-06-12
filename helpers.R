# values in Excel to set to NA #TODO: figure out consistant NA value
na_import_list = c(
  "",
  "NO DATA",
  "no data",
  "NO data",
  "No Data",
  "NaN",
  "NA",
  "na",
  "N/A",
  "n/a",
  "-9999",
  "9999",
  "-999",
  "999",
  "-999.00",
  "-9999.0",
  "-"
)

rename_map <- list(
  #Change column names to be friendlier
  # (follow https://edirepository.org/resources/cleaning-data-and-quality-control)
  # general (everything should have these)
  site_name = c("Site"),
  water_year = c("WaterYear"),
  date = c("Date"),
  time = c("Time"),
  initials = c("Initials"),
  notes = c("Notes"),
  # pits specific
  cloud_cover = c("CloudCover", "Cloud"),
  incoming_radiation_1 = c("IncomingRadiation1"),
  incoming_radiation_2 = c("IncomingRadiation2"),
  incoming_radiation_3 = c("IncomingRadiation3"),
  outgoing_radiation_1 = c("OutgoingRadiation1"),
  outgoing_radiation_2 = c("OutgoingRadiation2"),
  outgoing_radiation_3 = c("OutgoingRadiation3"),
  surface_temperature_fahrenheit = c("SurfaceTemp(F)"),
  snow_depth_centimeters = c("SnowDepth(cm)", "Depth(cm)"),
  tube_tare_weight_pounds = c("TubeTareWeight(lb)", "Tare(lb)"),
  tube_and_snow_weight_pounds = c("Tube+SnowWeight(lb)", "Weight(lb)"),
  snowing = c("Snowing?", "Snowing(y/n)"),
  snowing_past_24_hours = c("SnowinPast24Hours?", "Snowlast24h(y/n)"),
  melt = c("Melt?", "Melt(y/n)"),
  grain_size_millimeters = c("GrainSize(mm)", "Grainsize(mm)"),
  photo_taken = c("Photo?"),
  snow_depth_inches = c("SnowDepth(in)", "depth(in)"),
  snow_weight_kilograms = c("SnowWeight(kg)"),
  snow_density_kilograms_meters_cubed = c("SnowDensity(kgm-3)", "SnowDensity(kg/m3)"),
  snow_water_equivalent_millimeters = c("SWE(mm)"),
  albedo = c("Albedo"),
  surface_temperature_celcius = c("SurfaceTemp(C)"),
  # old pits specific
  pits_tube_id = c("Tube#"),
  scale_photo_taken = c("PhotoofSnowScale(y/n)"),
  # frost specific
  frost_tube_id = c("FrostTubeID"),
  max_frost_depth_centimeters = c("MaxFrostDepth(cm)", "MaxSoilFrostDepth(cm)"),
  layers_present = c("LayersPresent?(y/n)", "Layerspresent(y/n)"),
  thaw_depth_centimeters = c("ThawDepth(cm)", "ThawDepth1(cm)", "ThawDepth2(cm)"),
  shallow_frost_depth_centimeters = c("ShallowFrostDepth(cm)"),
  # old frost specific
  frost_depth_1_centimeters = c("FrostDepth1(cm)"),
  frost_depth_2_centimeters = c("FrostDepth2(cm)")
)

assign_site <- function(df) {
  # requires df to have `source_file` which should be the case for all
  df |>
    mutate(
      # add site_name column to data
      site_name = dplyr::case_when(
        stringr::str_detect(str_to_lower(source_file), "kingman") ~ "kingman",
        stringr::str_detect(str_to_lower(source_file), "field") ~ "thompson field",
        stringr::str_detect(str_to_lower(source_file), "canopy") ~ "thompson canopy",
      ),
    )
}

excel_import_from_file_list <- function(file_list, range = NULL) {
  # excel import and basic cleaning of data
  map(
    file_list,
    function(file) {
      # this allows to optionally provide a range to readxl
      args <- list(path = file, na = na_import_list)
      if (!is_null(range)) {
        args$range <- readxl::cell_cols(range)
      }

      rlang::exec(readxl::read_xlsx, !!!args) |>
        dplyr::mutate(source_file = basename(file)) |> # add column for sorce file
        dplyr::rename_all(~ str_replace_all(., "\\s+", "")) # removes all whitespace from column names
    }
  )
}

reasign_names <- function(df) {
  # safely renames df columns even if column exists in list but not in df
  # takes a dataframe and map between old/new names for columns (see above in file)
  # returns dataframe with new column names
  for (new_col in names(rename_map)) {
    old_col_matches <- rename_map[[new_col]]
    # if mulptiple old names, only pull out the correct one
    match_col <- intersect(names(df), old_col_matches)
    if (length(match_col) > 0) {
      df <- df |> rename(!!new_col := !!sym(match_col[1]))
    }
  }
  df
}

remove_carriage_returns <- function(df) {
  df |>
    mutate(across(
      # in all cells
      everything(),
      ~ {
        # if column is a string/character, remove all windows style(?) line ends
        if (is.character(.)) {
          str_replace_all(., "[\r\n]", "")
        } else {
          .
        }
      }
    ))
}

add_water_year <- function(df) {
  # add water_year column
  all_water_year <- max(year(df$date))
  df |> mutate(water_year = all_water_year)
}
