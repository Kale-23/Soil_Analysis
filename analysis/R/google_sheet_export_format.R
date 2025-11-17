format_frost <- function(df) {
  frost_data_goog <- df |>
    dplyr::select(-c(source_file)) |> # get rid of unused columns
    dplyr::filter(frost_tube_id %in% c("1", "2", "3")) |> # only keep tubes with ids 1/2/3 (others are for other experiments)
    dplyr::select(c(
      site_name,
      date,
      hour,
      minute,
      frost_tube_id,
      snow_depth_centimeters,
      shallow_frost_depth_centimeters,
      max_frost_depth_centimeters,
      thaw_depth_centimeters,
      initials,
      notes
    )) |>
    # add time of day back in
    dplyr::mutate(
      #time_of_day = sprintf("%02d:%02d:00", hour, minute),
      time_of_day = make_datetime(
        year = year(date),
        month = month(date),
        day = day(date),
        hour = hour,
        min = minute,
        sec = 0
      ),
      time_of_day = format(time_of_day, "%I:%M:%S %p")
      #time_of_day = format(hms(time_of_day), "%I:%M:%S %p"),
    ) |>
    dplyr::select(-hour, -minute) |>
    tidyr::pivot_wider(
      id_cols = c(site_name, date),
      names_from = frost_tube_id,
      values_from = c(
        snow_depth_centimeters,
        shallow_frost_depth_centimeters,
        max_frost_depth_centimeters,
        thaw_depth_centimeters
      ),
      unused_fn = list, # turn rest of columns into lists of values
      names_glue = "{frost_tube_id}_{.value}"
    ) |>
    dplyr::mutate(
      initials = purrr::map_chr(initials, function(x) {
        ux <- unique(x)

        if (length(ux) == 1) {
          as.character(ux)
        } else {
          str_c(ux, collapse = ",")
        }
      }),
      notes = purrr::map_chr(notes, function(x) {
        ux <- unique(x)

        if (length(ux) == 1) {
          as.character(ux)
        } else {
          str_c(ux, collapse = ",")
        }
      }),
      time_of_day = purrr::map_chr(time_of_day, function(x) {
        ux <- unique(x)

        if (length(ux) == 1) {
          as.character(ux)
        } else {
          # takes the most common time (this would be in the case of one value is mistyped in original data)
          tab <- table(x)
          return(names(tab)[which.max(tab)])
        }
      })
    )

  # columns to rename with site prefixes
  rename_cols <- c(
    "time_of_day",
    "snow_depth_centimeters",
    "shallow_frost_depth_centimeters",
    "max_frost_depth_centimeters",
    "thaw_depth_centimeters",
    "initials",
    "notes"
  )

  # separate dataframes for each site, renaming columns to indicate site
  frost_data_goog_kingman <- frost_data_goog |>
    dplyr::filter(site_name == "kingman") |>
    dplyr::select(-c(site_name)) |>
    dplyr::rename_with(
      .cols = dplyr::contains(rename_cols),
      .fn = ~ paste0("kingman_", .x)
    )
  frost_data_goog_field <- frost_data_goog |>
    dplyr::filter(site_name == "thompson field") |>
    dplyr::select(-c(site_name)) |>
    dplyr::rename_with(
      .cols = dplyr::contains(rename_cols),
      .fn = ~ paste0("field_", .x)
    )
  frost_data_goog_canopy <- frost_data_goog |>
    dplyr::filter(site_name == "thompson canopy") |>
    dplyr::select(-c(site_name)) |>
    dplyr::rename_with(
      .cols = dplyr::contains(rename_cols),
      .fn = ~ paste0("canopy_", .x)
    )

  # list of dataframes to combine
  dfs <- list(
    #frost_data_goog_kingman, # uncomment this to add kingman data back into google sheet export
    frost_data_goog_field,
    frost_data_goog_canopy
  )

  # this tests for duplicate date entries in any of the datasets
  #lapply(dfs, function(df) df %>% count(date) %>% filter(n > 1))

  # reduce to one dataframe
  frost <- purrr::reduce(dfs, dplyr::full_join, by = "date")

  # combine columns that are universal into one
  frost_goog_final <- frost |>
    tidyr::unite(
      "combined_initials",
      dplyr::contains("initials"),
      sep = ", "
    ) |>
    dplyr::select(
      -c(
        dplyr::starts_with("initials")
      )
    ) |>
    dplyr::mutate(
      combined_initials = simplify_combined_vec(combined_initials)
    ) |>
    dplyr::rename(
      frost_field_notes = field_notes,
      frost_canopy_notes = canopy_notes,
      frost_field_time = field_time_of_day,
      frost_canopy_time = canopy_time_of_day
    )

  return(frost_goog_final)
}

format_pits <- function(df) {
  df <- pits_data
  pits_data_goog <- df |>
    dplyr::select(c(
      site_name,
      date,
      hour,
      minute,
      cloud_cover,
      snowing,
      snowing_past_24_hours,
      melt,
      grain_size_millimeters,
      initials,
      incoming_radiation_1,
      incoming_radiation_2,
      incoming_radiation_3,
      outgoing_radiation_1,
      outgoing_radiation_2,
      outgoing_radiation_3,
      surface_temperature_celcius,
      snow_depth_centimeters,
      tube_tare_weight_pounds,
      tube_and_snow_weight_pounds,
      notes
    )) |>
    dplyr::mutate(
      #time_of_day = sprintf("%02d:%02d:00", hour, minute),
      time_of_day = make_datetime(
        year = year(date),
        month = month(date),
        day = day(date),
        hour = hour,
        min = minute,
        sec = 0
      ),
      time_of_day = format(time_of_day, "%I:%M:%S %p")
      #time_of_day = format(hms(time_of_day), "%I:%M:%S %p"),
    ) |>
    dplyr::select(-hour, -minute) |>
    dplyr::mutate(
      time_of_day = purrr::map_chr(time_of_day, function(x) {
        ux <- unique(x)

        if (length(ux) == 1) {
          as.character(ux)
        } else {
          # takes the most common time (this would be in the case of one value is mistyped in original data)
          tab <- table(x)
          return(names(tab)[which.max(tab)])
        }
      })
    )

  rename_cols <- c(
    "time_of_day",
    "cloud_cover",
    "snowing",
    "snowing_past_24_hours",
    "melt",
    "grain_size_millimeters",
    "initials",
    "incoming_radiation_",
    "outgoing_radiation_",
    "surface_temperature_",
    "snow_depth_",
    "tube_tare_weight_",
    "tube_and_snow_weight_",
    "notes"
  )

  pits_data_goog_kingman <- pits_data_goog |>
    dplyr::filter(site_name == "kingman") |>
    dplyr::select(-c(site_name)) |>
    dplyr::rename_with(
      .cols = dplyr::starts_with(rename_cols),
      .fn = ~ paste0("kingman_", .x)
    )
  pits_data_goog_field <- pits_data_goog |>
    dplyr::filter(site_name == "thompson field") |>
    dplyr::select(-c(site_name)) |>
    dplyr::rename_with(
      .cols = dplyr::starts_with(rename_cols),
      .fn = ~ paste0("field_", .x)
    )
  pits_data_goog_canopy <- pits_data_goog |>
    dplyr::filter(site_name == "thompson canopy") |>
    dplyr::select(-c(site_name)) |>
    dplyr::rename_with(
      .cols = dplyr::starts_with(rename_cols),
      .fn = ~ paste0("canopy_", .x)
    )

  dfs <- list(
    #pits_data_goog_kingman, # uncomment this to add kingman data back into google sheet export
    pits_data_goog_field,
    pits_data_goog_canopy
  )

  # this tests for duplicate date entries in any of the datasets
  #lapply(dfs, function(df) df %>% count(date) %>% filter(n > 1))

  pits <- purrr::reduce(dfs, dplyr::full_join, by = "date")
  rm(
    dfs,
    pits_data_goog_kingman,
    pits_data_goog_field,
    pits_data_goog_canopy,
    rename_cols,
    pits_data_goog
  )

  pits_goog_final <- pits |>
    tidyr::unite(
      "combined_cloud_cover",
      dplyr::contains("cloud_cover"),
      sep = ", "
    ) |>
    tidyr::unite(
      "combined_snowing",
      c("field_snowing", "canopy_snowing"),
      sep = ", "
    ) |>
    tidyr::unite(
      "combined_snowing_past_24_hours",
      dplyr::contains("snowing_past_24_hours"),
      sep = ", "
    ) |>
    tidyr::unite(
      "combined_melt",
      dplyr::contains("melt"),
      sep = ", "
    ) |>
    tidyr::unite(
      "combined_grain_size_millimeters",
      dplyr::contains("grain_size_millimeters"),
      sep = ", "
    ) |>
    tidyr::unite(
      "combined_initials",
      dplyr::contains("initials"),
      sep = ", "
    ) |>
    dplyr::select(
      -c(
        dplyr::starts_with("cloud_cover"),
        dplyr::starts_with("snowing"),
        dplyr::starts_with("melt"),
        dplyr::starts_with("grain_size_millimeters"),
        dplyr::starts_with("initials")
      )
    ) |>
    dplyr::mutate(
      combined_cloud_cover = simplify_combined_vec(combined_cloud_cover),
      combined_snowing = simplify_combined_vec(combined_snowing),
      combined_snowing_past_24_hours = simplify_combined_vec(combined_snowing_past_24_hours),
      combined_melt = simplify_combined_vec(combined_melt),
      combined_grain_size_millimeters = simplify_combined_vec(combined_grain_size_millimeters),
      combined_initials = simplify_combined_vec(combined_initials)
    ) |>
    dplyr::rename(
      pits_field_notes = field_notes,
      pits_canopy_notes = canopy_notes,
      pits_field_time = field_time_of_day,
      pits_canopy_time = canopy_time_of_day
    )

  return(pits_goog_final)
}

simplify_combined_vec <- function(v) {
  vapply(
    v,
    function(x) {
      parts <- strsplit(x, ",\\s*")[[1]]

      # normalize NA-like values
      parts_clean <- ifelse(parts %in% c("NA", "", NA), NA, parts)

      # all non-NA values
      non_na <- parts_clean[!is.na(parts_clean)]

      if (length(non_na) == 0) {
        # all NA
        return("NA")
      }

      if (length(unique(non_na)) == 1) {
        # all equal or equal except for NA
        return(unique(non_na))
      }

      # otherwise unchanged
      x
    },
    character(1)
  )
}

combine_datasets <- function(frost_df, pits_df) {
  # join the two datasets together
  combined_df <- dplyr::full_join(frost_df, pits_df, by = "date") |>
    # combine initials columns
    tidyr::unite(
      initials,
      c("combined_initials.x", "combined_initials.y"),
      sep = ", "
    ) |>
    # rename combined columns to final names
    dplyr::rename(
      cloud_cover = combined_cloud_cover,
      snowing = combined_snowing,
      snowing_past_24_hours = combined_snowing_past_24_hours,
      melt = combined_melt,
      grain_size_millimeters = combined_grain_size_millimeters
    ) |>
    # add in empty columns for google sheet upload format
    dplyr::mutate(
      overall_notes = "",
      timestamp = "",
      email = ""
    )

  final_order <- c(
    # --- combined global columns ---
    "timestamp",
    "email",
    "initials",
    "date",
    "cloud_cover",
    "snowing",
    "snowing_past_24_hours",
    "melt",
    "grain_size_millimeters",
    "overall_notes",

    # --- field tubes (1–3 for each measurement) ---
    "frost_field_time",
    "field_1_snow_depth_centimeters",
    "field_2_snow_depth_centimeters",
    "field_3_snow_depth_centimeters",

    "field_1_shallow_frost_depth_centimeters",
    "field_2_shallow_frost_depth_centimeters",
    "field_3_shallow_frost_depth_centimeters",

    "field_1_max_frost_depth_centimeters",
    "field_2_max_frost_depth_centimeters",
    "field_3_max_frost_depth_centimeters",

    "field_1_thaw_depth_centimeters",
    "field_2_thaw_depth_centimeters",
    "field_3_thaw_depth_centimeters",

    "frost_field_notes",

    # --- canopy tubes (1–3 for each measurement) ---
    "frost_canopy_time",
    "canopy_1_snow_depth_centimeters",
    "canopy_2_snow_depth_centimeters",
    "canopy_3_snow_depth_centimeters",

    "canopy_1_shallow_frost_depth_centimeters",
    "canopy_2_shallow_frost_depth_centimeters",
    "canopy_3_shallow_frost_depth_centimeters",

    "canopy_1_max_frost_depth_centimeters",
    "canopy_2_max_frost_depth_centimeters",
    "canopy_3_max_frost_depth_centimeters",

    "canopy_1_thaw_depth_centimeters",
    "canopy_2_thaw_depth_centimeters",
    "canopy_3_thaw_depth_centimeters",

    "frost_canopy_notes",

    # --- field pits instrumentation ---
    "pits_field_time",
    "field_incoming_radiation_1",
    "field_incoming_radiation_2",
    "field_incoming_radiation_3",

    "field_outgoing_radiation_1",
    "field_outgoing_radiation_2",
    "field_outgoing_radiation_3",

    "field_surface_temperature_celcius",
    "field_snow_depth_centimeters",

    "field_tube_tare_weight_pounds",
    "field_tube_and_snow_weight_pounds",

    "pits_field_notes",

    # --- canopy pits instrumentation ---
    "pits_canopy_time",
    "canopy_incoming_radiation_1",
    "canopy_incoming_radiation_2",
    "canopy_incoming_radiation_3",

    "canopy_outgoing_radiation_1",
    "canopy_outgoing_radiation_2",
    "canopy_outgoing_radiation_3",

    "canopy_surface_temperature_celcius",
    "canopy_snow_depth_centimeters",

    "canopy_tube_tare_weight_pounds",
    "canopy_tube_and_snow_weight_pounds",

    "pits_canopy_notes"
  )

  # reorder columns to final order
  combined_ordered <- combined_df |>
    dplyr::select(any_of(final_order), dplyr::everything())

  return(combined_ordered)
}
