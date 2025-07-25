# returns a dataframe of pits data as well as a dataframe of all removed data
# returned df: properly formatted df for combination with other era frost data
# returned removed_df: df of all rows removed due to one reason or another (see specifics below)
create_pits_new <- function(pits_files) {
  # import helper functions
  source("helpers.R", local = TRUE)

  # Data Aggregation (no removals)
  pits_data <- excel_import_from_file_list(pits_files)
  pits_data <- map(pits_data, reasign_names)

  # this will collect any data that would be removed by the following code
  # ideally all dfs will be empty but this is reasurance
  pits_data_removed <- find_removed_columns(
    pits_data,
    suppressWarnings(
      ~ is.na(as.numeric(.)) & !is.na(.) & str_to_lower(.) != "trace"
    ),
    c(
      "snow_depth_centimeters",
      "tube_tare_weight_pounds",
      "tube_and_snow_weight_pounds"
    )
  )

  # this moves some categorical stuff from numerical data
  # will coerce to NA if character data other than "trace" is found.
  # data will be collected by pits_data_removed for checking
  pits_data <- map(
    pits_data,
    ~ .x |>
      mutate(
        # "trace" in depth calcs to its own column
        snow_depth_trace = if_else(
          str_to_lower(snow_depth_centimeters) == "trace" |
            str_to_lower(tube_tare_weight_pounds) == "trace" |
            str_to_lower(tube_and_snow_weight_pounds) == "trace",
          "y",
          "n"
        ),
        snow_depth_centimeters = as.numeric(if_else(
          str_to_lower(snow_depth_centimeters) == "trace",
          NA,
          snow_depth_centimeters
        )),
        tube_tare_weight_pounds = as.numeric(if_else(
          str_to_lower(tube_tare_weight_pounds) == "trace",
          NA,
          tube_tare_weight_pounds
        )),
        tube_and_snow_weight_pounds = as.numeric(if_else(
          str_to_lower(tube_and_snow_weight_pounds) == "trace",
          NA,
          tube_and_snow_weight_pounds
        ))
      )
  )

  #! (determined to be unknown, AL data will be removed)
  # "AL" in surface temp
  #pits_data <- map(pits_data, function(df) {
  #  if ("surface_temperature_fahrenheit" %in% names(df)) {
  #    df <- df |>
  #      mutate(
  #        surface_temperature_AL = if_else(
  #          str_to_upper(surface_temperature_fahrenheit) == "AL",
  #          "Y",
  #          "N"
  #        ),
  #        surface_temperature_fahrenheit = as.numeric(if_else(
  #          str_to_upper(surface_temperature_fahrenheit) == "AL",
  #          NA,
  #          surface_temperature_fahrenheit
  #        ))
  #      )
  #  }
  #  df
  #})

  # shows removed "AL" values in removed dfs
  temp_removed <- find_removed_columns(
    pits_data,
    suppressWarnings(~ is.na(as.numeric(.)) & !is.na(.)),
    c("surface_temperature_fahrenheit")
  )
  for (i in 1:length(pits_data_removed)) {
    pits_data_removed[[i]] <- full_join(pits_data_removed[[i]], temp_removed[[i]])
  }
  rm(temp_removed)

  # actually removes "AL" values
  # done this way so that warnings dont show unless more than just "AL" exists
  pits_data <- map(
    pits_data,
    function(df) {
      if ("surface_temperature_fahrenheit" %in% names(df)) {
        df |>
          mutate(
            surface_temperature_fahrenheit = as.numeric(if_else(
              str_to_upper(surface_temperature_fahrenheit) == "AL",
              NA,
              surface_temperature_fahrenheit
            ))
          )
      } else {
        df
      }
    }
  )

  # combine into one dataframe
  pits_data <- reduce(pits_data, full_join)
  pits_data_removed <- reduce(pits_data_removed, full_join)

  # return dataframe
  list(pits_data, pits_data_removed)
}

create_pits_old <- function(old_pits_files) {
  source("./helpers.R", local = TRUE)
  # Data Aggregation
  pits_data <- excel_import_from_file_list(old_pits_files)

  # rename columns to fit with newer format + add identifier columns
  pits_data <- map(pits_data, reasign_names)
  pits_data <- map(pits_data, assign_site)
  pits_data <- map(pits_data, add_water_year)

  # remove mm/MM at end of grain_size
  pits_data <- map(
    pits_data,
    ~ .x |>
      mutate(grain_size_millimeters = str_remove(grain_size_millimeters, "[mM]*$"))
  )

  # shows what values will be removed from dataset in grain_size due to not knowing exact mm (ie has "<")
  pits_data_removed <- find_removed_columns(
    pits_data,
    ~ is.na(as.numeric(.)) & !is.na(.),
    c("grain_size_millimeters")
  )

  # remove the "<" values in grain_size
  pits_data <- map(
    pits_data,
    ~ .x |>
      mutate(
        grain_size_millimeters = as.numeric(if_else(
          str_detect(grain_size_millimeters, "<"),
          NA,
          grain_size_millimeters
        ))
      )
  )

  #! removing the "<" values instead of extracting value
  ## make columns the same datatypes so we can join together
  #pits_data <- map(
  #  pits_data,
  #  ~ .x |>
  #    select(-contains("Layer")) |>
  #    mutate(
  #      # remove "<{num}mm" annotation from number
  #      #TODO: revisit and determine if "<1MM" should be 1 or NA or something else
  #      grain_size_millimeters = as.numeric(str_extract(
  #        grain_size_millimeters,
  #        "\\d*\\.?\\d+"
  #      ))
  #    )
  #)

  # join dataframes together
  pits_data <- reduce(pits_data, full_join)
  pits_data_removed <- reduce(pits_data_removed, full_join)

  # return full dataframe
  list(pits_data, pits_data_removed)
}

create_pits_oldest <- function(oldest_files) {
  source("./helpers.R", local = TRUE)
  # Data Aggregation
  pits_data <- excel_import_from_file_list(oldest_files)

  # renames all columns of all dataframes (check helpers.R for name map)
  pits_data <- map(pits_data, reasign_names)

  # the notes are spread across a couple columns, this combines those columns
  pits_data <- map(
    pits_data,
    function(x) {
      start_col <- which(names(x) == "notes")
      if (start_col < ncol(x)) {
        combined_cols <- names(x)[start_col:(ncol(x) - 1)]
      } else {
        combined_cols <- names(x)[start_col]
      }
      print(combined_cols)

      x |>
        unite(col = notes, all_of(combined_cols), sep = ", ", remove = TRUE) |>
        mutate(
          notes = str_remove_all(notes, "NA, "),
          notes = if_else(notes == "NA", NA, notes)
        )
    }
  )

  # give the site name column
  pits_data <- map(pits_data, assign_site)

  # checks what is being removed below
  pits_data_removed <- find_removed_columns(
    pits_data,
    suppressWarnings(
      ~ is.na(as.numeric(.)) & !is.na(.) & str_to_lower(.) != "trace"
    ),
    c("snow_depth_centimeters")
  )

  # removes character cells from snow_depth_centimeters and puts "trace" annotation in its own column
  pits_data <- map(
    pits_data,
    ~ .x |>
      mutate(
        snow_depth_trace = if_else(
          str_to_lower(snow_depth_centimeters) == "trace",
          "y",
          "n"
        ),
        snow_depth_centimeters = as.numeric(if_else(
          str_to_lower(snow_depth_centimeters) == "trace",
          NA,
          snow_depth_centimeters
        ))
      )
  )

  # join dataframes together
  pits_data <- reduce(pits_data, full_join)
  pits_data_removed <- reduce(pits_data_removed, full_join)

  list(pits_data, pits_data_removed)
}


process_pits <- function(pits_data, pits_data_removed) {
  source("helpers.R", local = TRUE)
  # remove rows where all data is missing
  full_columns <- c("site_name", "water_year", "date", "source_file")

  # add removed rows to removed df first
  pits_data_removed <- pits_data |>
    filter(if_all(-all_of(full_columns), ~ is.na(.) | . == 0)) |>
    mutate(across(everything(), ~ as.character(.x))) |>
    full_join(pits_data_removed)

  # actually removing them now
  pits_data <- pits_data |>
    filter(!if_all(-all_of(full_columns), ~ is.na(.) | . == 0))
  rm(full_columns)

  # some cells had windows style eol? this removes them (does not remove rows)
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
      pits_tube_id = as.factor(pits_tube_id)
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

  # reporting/removing impossible values (tared weight less than tare)
  pits_data_removed <- pits_data |>
    filter(tube_and_snow_weight_pounds < tube_tare_weight_pounds) |>
    mutate(across(
      everything(),
      ~ {
        as.character(.x)
      }
    )) |>
    mutate(across(
      .cols = !c(
        any_of(c("site_name", "water_year", "date", "time", "source_file")),
        all_of(c("tube_and_snow_weight_pounds", "tube_tare_weight_pounds"))
      ),
      .fns = ~NA
    )) |>
    full_join(pits_data_removed)

  pits_data <- pits_data |>
    mutate(
      tube_and_snow_weight_pounds = if_else(
        tube_and_snow_weight_pounds >= tube_tare_weight_pounds,
        tube_and_snow_weight_pounds,
        NA
      ),
      tube_tare_weight_pounds = if_else(
        tube_and_snow_weight_pounds >= tube_tare_weight_pounds,
        tube_tare_weight_pounds,
        NA
      ),
      tube_and_snow_weight_pounds
    )

  # return dataframe of all pits data
  list(pits_data, pits_data_removed)
}

full_handle_pits <- function(new_pits, old_pits, oldest_files) {
  # creates seperate pits dataframes from each era of data collection files (new/old/oldest)
  # each era is slightly different so it was easier to do it this way than to make one function for all 3
  c(new_pits_temp, new_pits_removed_temp) %<-% create_pits_new(new_pits)
  c(old_pits_temp, old_pits_removed_temp) %<-% create_pits_old(old_pits)
  c(oldest_pits_temp, oldest_pits_removed_temp) %<-% create_pits_oldest(oldest_files)

  # combines all pits data together
  #TODO bind rows may be more correct but this does not require columns to be the same/in the same order
  pits_data_temp <- full_join(new_pits_temp, old_pits_temp)
  pits_data_combined <- full_join(pits_data_temp, oldest_pits_temp)
  rm(new_pits_temp, old_pits_temp, oldest_pits_temp, pits_data_temp)

  # combine removed data together for easier reporting
  pits_data_removed_temp <- full_join(new_pits_removed_temp, old_pits_removed_temp)
  pits_data_removed_combined <- full_join(pits_data_removed_temp, oldest_pits_removed_temp)
  rm(new_pits_removed_temp, old_pits_removed_temp, oldest_pits_removed_temp, pits_data_removed_temp)

  # once combined, all pits data is processed and returned
  c(pits_data, pits_data_removed) %<-% process_pits(pits_data_combined, pits_data_removed_combined)

  list(pits_data, pits_data_removed)
}
