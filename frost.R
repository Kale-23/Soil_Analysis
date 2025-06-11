process_frost <- function(frost_files) {
  source("./lists.R")
  # Data Aggregation
  # for every file in pits_data, read it into a dataframe
  frost_data <- map(
    frost_files,
    function(file) {
      readxl::read_xlsx(
        file,
        range = cell_cols("A:L"),
        #col_types = "text", # set all column types to strings, changed later in script
        na = na_import_list
      ) |>
        dplyr::mutate(source_file = basename(file)) |> # add column for sorce file
        dplyr::rename_all(~ str_replace_all(., "\\s+", "")) # removes all whitespace from column names
    }
  )
  rm(na_import_list)

  # current column names vs new names
  rename_map <- c(
    site_name = c("Site"),
    water_year = c("WaterYear"),
    date = c("Date"),
    time = c("Time"),
    frost_tube_id = c("FrostTubeID"),
    snow_depth_centimeters = c("SnowDepth(cm)"),
    max_frost_depth_centimeters = c(
      "MaxFrostDepth(cm)",
      "MaxSoilFrostDepth(cm)"
    ),
    layers_present = c(
      "LayersPresent?(y/n)",
      "Layerspresent(y/n)"
    ),
    thaw_depth_centimeters = c("ThawDepth(cm)"),
    shallow_frost_depth_centimeters = c("ShallowFrostDepth(cm)"),
    initials = c("Initials"),
    notes = c("Notes")
  )

  # maps new names onto columns
  frost_data <- map(frost_data, function(df) {
    for (new_col in names(rename_map)) {
      old_col <- rename_map[[new_col]]
      # if multiple possible column names, this removes the subscripted number
      new_col <- str_remove(new_col, "[0-9]")
      if (old_col %in% names(df)) {
        df <- df |> rename(!!new_col := !!sym(old_col))
      }
    }
    df
  })
  rm(rename_map)

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
        # making sure nothing other than "trace" exists
        #testing = ifelse(
        #  str_detect(max_frost_depth_centimeters, "^[:alpha:]+$"),
        #  TRUE,
        #  FALSE
        #),

        # this is the conversion to numeric
        #TODO: will show warnigns but due to check above + below can be ignored for now
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
  ## making sure not to remove anything other than trace
  #test = c()
  #for (i in 1:13) {
  #  testing <- frost_data[[i]] |>
  #    filter(frost_data[[i]]$testing) |>
  #    select(snow_depth_centimeters)
  #  str(testing)
  #  test <- append(test, as.list(testing$snow_depth_centimeters))
  #}
  #unique(test)
  #rm(testing, test, i)

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
  #empty_frost_data <- frost_data |>
  #  # these columns have data even if the rest dont, ignore these when filtering
  #  filter(if_all(all_of(filter_columns), ~ is.na(.) | . == 0))
  frost_data <- frost_data |>
    # these columns have data even if the rest dont, ignore these when filtering
    filter(!if_all(all_of(filter_columns), ~ is.na(.) | . == 0))
  print(paste("filtered out ", orig_row_count - nrow(frost_data), " columns"))
  rm(orig_row_count, full_columns, filter_columns)

  # remove carrage returns
  frost_data <- frost_data |>
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

  frost_data <- frost_data |>
    mutate(
      # change some formats to better reflect data (str to factor etc)
      site_name = as.factor(site_name),
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
    )

  # return final dataframe for frost data
  frost_data
}
