# returns a dataframe of frost data as well as a dataframe of all removed data
# returned df: properly formatted df for combination with other era frost data
# returned removed_df: df of all rows removed due to one reason or another (see specifics below)
create_frost_new <- function(frost_files) {
  # import helper functions
  source("helpers.R", local = TRUE)

  # Data Aggregation (no removals)
  frost_data <- excel_import_from_file_list(frost_files, range = "A:L")
  frost_data <- map(frost_data, reasign_names)

  # move any alpha data from numeric data into notes (this does not remove, just moves)
  frost_data <- map(
    frost_data,
    ~ .x |>
      mutate(
        # "trace" in depth calcs to its own column (blocks numeric conversion)
        snow_depth_trace = ifelse(
          str_to_lower(snow_depth_centimeters) == "trace",
          "y",
          "n"
        ),
        # any annotation other than "trace" belongs in notes and not numeric data
        notes = ifelse(
          str_detect(snow_depth_centimeters, "^[:alpha:]+$") &
            str_to_lower(snow_depth_centimeters) != "trace",
          paste(snow_depth_centimeters, notes, sep = ", "),
          notes
        ),
        # any annotation other than "trace" belongs in notes and not numeric data
        notes = ifelse(
          str_detect(max_frost_depth_centimeters, "^[:alpha:]+$") &
            str_to_lower(max_frost_depth_centimeters) != "trace",
          paste(max_frost_depth_centimeters, notes, sep = " "),
          notes
        )
      )
  )

  # reports data removed/moved to notes
  frost_data_removed <- find_removed_columns(
    frost_data,
    suppressWarnings(
      ~ is.na(as.numeric(.)) &
        !is.na(.) &
        str_to_lower(.) != "trace"
    ),
    c(
      "snow_depth_centimeters",
      "max_frost_depth_centimeters"
    )
  )
  frost_data_removed <- map(
    frost_data_removed,
    ~ .x |> mutate(across(everything(), ~ as.character(.x)))
  )

  # actually removes character data from within numeric columns
  # done this way to reduce the warnings reported unless something other than
  # what is being tested for is seen (will be able to tell in frost_data_removed)
  frost_data <- map(
    frost_data,
    ~ .x |>
      mutate(
        snow_depth_centimeters = as.numeric(ifelse(
          str_to_lower(snow_depth_centimeters) == "trace" |
            str_to_lower(snow_depth_centimeters) == "broken",
          NA,
          snow_depth_centimeters
        )),
        max_frost_depth_centimeters = as.numeric(ifelse(
          str_to_lower(max_frost_depth_centimeters) == "trace" |
            str_to_lower(max_frost_depth_centimeters) == "broken" |
            str_to_lower(max_frost_depth_centimeters) == "stuck",
          NA,
          max_frost_depth_centimeters
        ))
      )
  )

  # make all tube ids into factors as there are both numbers and letters
  frost_data <- map(
    frost_data,
    ~ .x |>
      mutate(
        frost_tube_id = as.factor(frost_tube_id),
      )
  )

  # combine into one dataframe
  frost_data <- reduce(frost_data, full_join)
  frost_data_removed <- reduce(frost_data_removed, full_join)

  # filter out empty rows
  filter_columns <- c(
    "snow_depth_centimeters",
    "max_frost_depth_centimeters",
    "layers_present",
    "thaw_depth_centimeters",
    "shallow_frost_depth_centimeters",
    "initials",
    "notes"
  )

  # add empty rows to removed df
  frost_data_removed <- frost_data |>
    filter(if_all(all_of(filter_columns), ~ is.na(.) | . == 0)) |>
    mutate(across(everything(), ~ as.character(.x))) |>
    full_join(frost_data_removed)

  # actually remove empty rows
  frost_data <- frost_data |>
    # these columns have data even if the rest dont, ignore these when filtering
    filter(!if_all(all_of(filter_columns), ~ is.na(.) | . == 0))
  rm(filter_columns)

  # remove carrage returns
  frost_data <- remove_carriage_returns(frost_data)

  # return final dataframe for frost data
  list(frost_data, frost_data_removed)
}

create_frost_old <- function(old_frost_files) {
  source("helpers.R", local = TRUE)
  # import and assign correct names to columns
  frost_data <- excel_import_from_file_list(old_frost_files)
  frost_data <- map(frost_data, reasign_names)
  frost_data <- map(frost_data, ~ .x |> rename("frost_tube_id" = "pits_tube_id"))

  # these only put in dates once for every 3 tube measurements, this corrects that
  # this is done before combining to make sure order stays the same
  frost_data <- map(frost_data, function(df) {
    date <- NA
    time <- NA
    initials <- NA
    for (i in seq_len(nrow(df))) {
      # modulo selects once every 3 lines to save for next 2 after
      if (i %% 3 == 1) {
        date <- df$date[i]
        time <- df$time[i]
        initials <- df$initials[i]
      } else {
        df$date[i] <- date
        df$time[i] <- time
        df$initials[i] <- initials
      }
    }
    df
  })
  # this doesnt work as if a section is missing, the previous section will get filled in (ie 6 of same instead of 3)
  #frost_data <- map(frost_data, ~ .x |> fill(date, time, .direction = "down"))

  # make all tube ids into factors as there are both numbers and letters
  frost_data <- map(
    frost_data,
    ~ .x |>
      mutate(
        frost_tube_id = as.factor(frost_tube_id),
      )
  )

  # add water year column
  frost_data <- map(frost_data, add_water_year)

  # combine to one dataframe
  frost_data <- reduce(frost_data, full_join)

  # assign site column
  frost_data <- assign_site(frost_data)

  # filter out empty rows
  filter_columns <- c(
    "frost_depth_1_centimeters",
    "snow_depth_centimeters",
    "thaw_depth_centimeters",
    "frost_depth_2_centimeters",
    "initials",
    "notes"
  )

  # add empty rows to removed df
  frost_data_removed <- frost_data |>
    filter(if_all(all_of(filter_columns), ~ is.na(.) | . == 0)) |>
    mutate(across(everything(), ~ as.character(.x)))

  # actually remove empty rows
  frost_data <- frost_data |>
    # these columns have data even if the rest dont, ignore these when filtering
    filter(!if_all(all_of(filter_columns), ~ is.na(.) | . == 0))
  rm(filter_columns)

  # return dataframe
  list(frost_data, frost_data_removed)
}

process_frost <- function(frost_data, frost_data_removed) {
  #TODO: finish this step and be done wiht better removed reporting
  frost_data <- frost_data |>
    mutate(
      # change some formats to better reflect data (str to factor etc)
      site_name = as.factor(str_to_lower(site_name)),
      layers_present = as.factor(layers_present),
      initials = as.factor(initials),
      source_file = as.factor(source_file),
      snow_depth_trace = as.factor(snow_depth_trace)
    ) |>
    mutate(
      # recode factors (ie "n" to "no")
      # no vs n etc to be consistant with pits data
      layers_present = recode(
        layers_present,
        "n" = "no",
        "N" = "no",
        "No" = "no",
        "y" = "yes",
        "Y" = "yes",
        "Yes" = "yes"
      ),
    ) |>
    mutate(
      layers_present = na_if(layers_present, "0"),
      initials = na_if(initials, "0")
    ) |>
    mutate(
      # dates
      year = year(date),
      month = month(date),
      day = day(date),
      hour = hour(time),
      minute = minute(time)
    ) |>
    mutate(
      # fixing non 24 hour time input (if at/before 5am, change to pm)
      hour = ifelse(hour == 0, NA, hour),
      hour = ifelse(hour <= 5, hour + 12, hour),
      minute = ifelse(hour == 0 & minute == 0, NA, minute)
    ) |>
    select(-time)
}

full_handle_frost <- function(new_frost, old_frost) {
  # read in and process each era (new/older) of frost data seperately
  #! most processing should be completed in "process_frost" step
  c(new_frost_temp, new_frost_removed_temp) %<-% create_frost_new(new_frost)
  c(old_frost_temp, old_frost_removed_temp) %<-% create_frost_old(old_frost)

  # combine frost data into one dataframe
  frost_data <- full_join(new_frost_temp, old_frost_temp)
  frost_data_removed <- full_join(new_frost_removed_temp, old_frost_removed_temp)
  rm(new_frost_temp, old_frost_temp, new_frost_removed_temp, old_frost_removed_temp)

  # process all frost data together
  c(frost_data, frost_data_removed) %<-% process_frost(frost_data_temp, frost_data_removed)
}
