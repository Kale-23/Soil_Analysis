create_pits_new <- function(pits_files) {
  source("./lists.R")
  # Data Aggregation
  # for every file in pits_data, read it into a dataframe
  pits_data <- map(
    pits_files,
    function(file) {
      readxl::read_xlsx(
        file,
        #col_types = "text", # set all column types to strings, changed later in script
        na = na_import_list
      ) |>
        dplyr::mutate(source_file = basename(file)) |> # add column for sorce file
        dplyr::rename_all(~ str_replace_all(., "\\s+", "")) # removes all whitespace from column names
    }
  )
  rm(na_import_list)

  #Change column names to be friendlier
  # (follow https://edirepository.org/resources/cleaning-data-and-quality-control)
  rename_map <- c(
    site_name = "Site",
    water_year = "WaterYear",
    date = "Date",
    time = "Time",
    cloud_cover = "CloudCover",
    incoming_radiation_1 = "IncomingRadiation1",
    incoming_radiation_2 = "IncomingRadiation2",
    incoming_radiation_3 = "IncomingRadiation3",
    outgoing_radiation_1 = "OutgoingRadiation1",
    outgoing_radiation_2 = "OutgoingRadiation2",
    outgoing_radiation_3 = "OutgoingRadiation3",
    surface_temperature_fahrenheit = "SurfaceTemp(F)",
    snow_depth_centimeters = "SnowDepth(cm)",
    tube_tare_weight_pounds = "TubeTareWeight(lb)",
    tube_and_snow_weight_pounds = "Tube+SnowWeight(lb)",
    snowing = "Snowing?",
    snowing_past_24_hours = "SnowinPast24Hours?",
    melt = "Melt?", #TODO: melting?
    grain_size_millimeters = "GrainSize(mm)",
    initials = "Initials", #TODO: person who took measurements?
    photo_taken = "Photo?", #TODO: are these stored anywhere?
    snow_depth_inches = "SnowDepth(in)",
    snow_weight_kilograms = "SnowWeight(kg)",
    snow_density_kilograms_meters_cubed = "SnowDensity(kgm-3)",
    snow_water_equivalent_millimeters = "SWE(mm)", #TODO: SWE means this right?
    albedo = "Albedo",
    notes = "Notes",
    surface_temperature_celcius = "SurfaceTemp(C)"
  )
  # rename will error out if column name doesn't exist
  # this checks to make sure column exists before rename
  pits_data <- map(pits_data, function(df) {
    for (new_col in names(rename_map)) {
      old_col <- rename_map[[new_col]]
      if (old_col %in% names(df)) {
        df <- df |> rename(!!new_col := !!sym(old_col))
      }
    }
    df
  })
  rm(rename_map)

  # this moves some categorical stuff from numerical data
  pits_data <- map(
    pits_data,
    ~ .x |>
      mutate(
        # "trace" in depth calcs to its own column
        snow_depth_trace = ifelse(
          str_to_lower(snow_depth_centimeters) == "trace" |
            str_to_lower(tube_tare_weight_pounds) == "trace" |
            str_to_lower(tube_and_snow_weight_pounds) == "trace",
          "Y",
          "N"
        ),
        snow_depth_centimeters = as.numeric(ifelse(
          str_to_lower(snow_depth_centimeters) == "trace",
          NA,
          snow_depth_centimeters
        )),
        tube_tare_weight_pounds = as.numeric(ifelse(
          str_to_lower(tube_tare_weight_pounds) == "trace",
          NA,
          tube_tare_weight_pounds
        )),
        tube_and_snow_weight_pounds = as.numeric(ifelse(
          str_to_lower(tube_and_snow_weight_pounds) == "trace",
          NA,
          tube_and_snow_weight_pounds
        ))
      )
  )
  # "AL" in surface temp
  #TODO: na introduced, could handle better to make sure its just AL -> na
  pits_data <- map(pits_data, function(df) {
    if ("surface_temperature_fahrenheit" %in% names(df)) {
      df <- df |>
        mutate(
          surface_temperature_AL = ifelse(
            str_to_upper(surface_temperature_fahrenheit) == "AL",
            "Y",
            "N"
          ),
          surface_temperature_fahrenheit = as.numeric(
            surface_temperature_fahrenheit
          )
        )
    }
    df
  })

  # testing to make sure no na values are introduced do to conversion between str and numeric
  #map(
  #  pits_data,
  #  ~ mutate(
  #    .x,
  #    surface_temperature_fahrenheit = ifelse(
  #      "surface_temperature_fahrenheit" %in% names(.x),
  #      surface_temperature_fahrenheit,
  #      NA
  #    )
  #  ),
  #  select(surface_temperature_fahrenheit) |>
  #    filter(is.na(
  #      as.numeric(surface_temperature_fahrenheit) !=
  #        is.na(surface_temperature_fahrenheit)
  #    )) |>
  #    select(surface_temperature_fahrenheit)
  #)

  # combine into one dataframe
  pits_data <- reduce(pits_data, full_join)

  # return dataframe
  pits_data
}

create_pits_old <- function(old_pits_files) {
  # import global sorting lists
  source("./lists.R")

  # import excel data
  pits_data <- map(
    old_pits_files,
    function(file) {
      readxl::read_xlsx(
        file,
        na = na_import_list
      ) |>
        dplyr::mutate(source_file = basename(file)) |> # add column for sorce file
        dplyr::rename_all(~ str_replace_all(., "\\s+", "")) # removes all whitespace from column names
    }
  )
  rm(na_import_list)

  # rename columns to fit with newer format
  rename_map <- c(
    date = "Date",
    time = "Time",
    cloud_cover = "Cloud",
    pits_tube_id = "Tube#",
    snow_depth_centimeters = "Depth(cm)",
    snow_depth_inches = "depth(in)",
    tube_tare_weight_pounds = "Tare(lb)",
    tube_and_snow_weight_pounds = "Weight(lb)",
    snow_weight_kilograms = "SnowWeight(kg)",
    snow_density_kilograms_meters_cubed = "SnowDensity(kg/m3)",
    snow_water_equivalent_millimeters = "SWE(mm)", #TODO: SWE means this right?
    scale_photo_taken = "PhotoofSnowScale(y/n)", #TODO: are these stored anywhere?
    snowing = "Snowing(y/n)",
    snowing_past_24_hours = "Snowlast24h(y/n)",
    melt = "Melt(y/n)",
    grain_size_millimeters = "Grainsize(mm)",
    initials = "Initials",
    notes = "Notes"
  )
  pits_data <- map(pits_data, function(df) {
    for (new_col in names(rename_map)) {
      old_col <- rename_map[[new_col]]
      if (old_col %in% names(df)) {
        df <- df |> rename(!!new_col := !!sym(old_col))
      }
    }
    df
  })
  rm(rename_map)

  # combine dataframes

  # make columns the same datatypes so we can join together
  pits_data <- map(
    pits_data,
    ~ .x |>
      select(-contains("Layer")) |>
      mutate(
        # remove "<{num}mm" annotation from number
        #TODO: revisit and determine if "<1MM" should be 1 or NA or something else
        grain_size_millimeters = as.numeric(str_extract(
          grain_size_millimeters,
          "\\d*\\.?\\d+"
        ))
      )
  )

  # add water_year column
  pits_data <- map(
    pits_data,
    function(df) {
      all_water_year <- max(year(df$date))
      df |> mutate(water_year = all_water_year)
    }
  )

  # join dataframes together
  pits_data <- reduce(pits_data, full_join)

  pits_data <- pits_data |>
    mutate(
      # add site_name column to data
      site_name = case_when(
        str_detect(str_to_lower(source_file), "kingman") ~ "kingman",
        str_detect(str_to_lower(source_file), "field") ~ "thompson field",
        str_detect(str_to_lower(source_file), "canopy") ~ "thompson canopy",
      ),
    )

  # return full dataframe
  pits_data
}


process_pits <- function(df) {
  # remove rows where all data is missing
  orig_row_count <- nrow(pits_data)
  full_columns <- c("site_name", "water_year", "date", "source_file")
  pits_data <- pits_data |>
    # these columns have data even if the rest dont, ignore these when filtering
    filter(!if_all(-all_of(full_columns), ~ is.na(.) | . == 0))
  print(paste("filtered out ", orig_row_count - nrow(pits_data), " columns"))
  rm(orig_row_count, full_columns)

  # some cells had carrage returns? this removes them
  pits_data <- pits_data |>
    mutate(across(
      everything(),
      ~ {
        if (is.character(.)) {
          str_replace_all(., "[\r\n]", "")
        } else {
          .
        }
      }
    ))

  # change some str to factors + adjust date formating (dates are weird in excel)
  pits_data <- pits_data |>
    mutate(
      # columns to factors
      site_name = as.factor(str_to_lower(site_name)),
      cloud_cover = as.factor(str_to_upper(cloud_cover)),
      snowing = as.factor(str_to_lower(snowing)),
      snowing_past_24_hours = as.factor(str_to_lower(snowing_past_24_hours)),
      melt = as.factor(str_to_lower(melt)),
      initials = as.factor(str_to_upper(initials)),
      photo_taken = as.factor(str_to_lower(photo_taken)),
    ) |>
    mutate(
      # factor recoding (ie n into no, y into yes)
      snowing = recode(
        snowing,
        "n" = "no",
        "y" = "yes"
      ),
      snowing_past_24_hours = recode(
        snowing_past_24_hours,
        "n" = "no",
        "y" = "yes"
      ),
      melt = recode(
        melt,
        "n" = "no",
        "y" = "yes",
        "ys" = "yes"
      ),
      photo_taken = recode(
        photo_taken,
        "n" = "no",
        "y" = "yes",
        "ys" = "yes"
      ),
    ) |>
    mutate(
      # dates
      year = year(date),
      month = month(date),
      day = day(date),
      hour = hour(time),
      minute = minute(time)
    ) |>
    select(!time)

  # compare to determine if NA are introduced between functions
  #na_summary <- pits_data |>
  #  summarise(across(everything(), ~ sum(is.na(.))))
  #na_summary1 <- pits_data1 |>
  #  summarise(across(everything(), ~ sum(is.na(.))))

  # return dataframe of all pits data
  pits_data
}
