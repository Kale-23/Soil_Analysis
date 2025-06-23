create_frost_new <- function(frost_files) {
  source("helpers.R", local = TRUE)

  # Data Aggregation
  frost_data <- excel_import_from_file_list(frost_files, range = "A:L")
  frost_data <- map(frost_data, reasign_names)

  frost_data <- map(
    frost_data,
    ~ .x |>
      mutate(
        # "trace" in depth calcs to its own column (blocks numeric conversion)
        snow_depth_trace = ifelse(
          str_to_lower(snow_depth_centimeters) == "trace",
          "Y",
          "N"
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
        ),
        #TODO: will show warnigns, give user some way of knowing what was removed
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
        )),
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
  filter_columns <- c(
    "snow_depth_centimeters",
    "max_frost_depth_centimeters",
    "layers_present",
    "thaw_depth_centimeters",
    "shallow_frost_depth_centimeters",
    "initials",
    "notes"
  )
  full_columns <- c("site_name", "water_year", "date", "source_file")
  orig_row_count <- nrow(frost_data)
  frost_data <- frost_data |>
    # these columns have data even if the rest dont, ignore these when filtering
    filter(!if_all(all_of(filter_columns), ~ is.na(.) | . == 0))
  print(paste("filtered out ", orig_row_count - nrow(frost_data), " columns"))
  rm(orig_row_count, full_columns, filter_columns)

  # remove carrage returns
  frost_data <- remove_carriage_returns(frost_data)

  # return final dataframe for frost data
  frost_data
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

  # return dataframe
  frost_data
}

process_frost <- function(frost_data) {
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
