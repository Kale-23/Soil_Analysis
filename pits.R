create_pits_new <- function(pits_files) {
  source("./helpers.R", local = TRUE)
  # Data Aggregation
  pits_data <- excel_import_from_file_list(pits_files)

  # renames all columns of all dataframes (check helpers.R for name map)
  pits_data <- map(pits_data, reasign_names)

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

  # combine into one dataframe
  pits_data <- reduce(pits_data, full_join)

  # return dataframe
  pits_data
}

create_pits_old <- function(old_pits_files) {
  source("./helpers.R", local = TRUE)
  # Data Aggregation
  pits_data <- excel_import_from_file_list(old_pits_files)

  # rename columns to fit with newer format
  pits_data <- map(pits_data, reasign_names)

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
  pits_data <- map(pits_data, add_water_year)

  # join dataframes together + use filename col to assign site
  pits_data <- reduce(pits_data, full_join)
  pits_data <- assign_site(pits_data)

  # return full dataframe
  pits_data
}


process_pits <- function(df) {
  source("./helpers.R", local = TRUE)
  # remove rows where all data is missing
  orig_row_count <- nrow(pits_data)
  full_columns <- c("site_name", "water_year", "date", "source_file")
  pits_data <- pits_data |>
    # these columns have data even if the rest dont, ignore these when filtering
    filter(!if_all(-all_of(full_columns), ~ is.na(.) | . == 0))
  print(paste("filtered out ", orig_row_count - nrow(pits_data), " columns"))
  rm(orig_row_count, full_columns)

  # some cells had carrage returns? this removes them
  pits_data <- remove_carriage_returns(pits_data)

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
      minute = minute(time),
      hour = case_when(
        hour <= 5 ~ hour + 12,
        .default = hour
      )
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
