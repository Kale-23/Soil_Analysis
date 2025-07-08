pits_calculations <- function(df) {
  df |>
    mutate(
      # change all fahrenheit to celcius and remove fahrenheit column
      surface_temperature_celcius = if_else(
        is.na(surface_temperature_celcius) & !is.na(surface_temperature_fahrenheit),
        (surface_temperature_fahrenheit - 32) / 1.8, # fahrenheit to celcius
        surface_temperature_celcius
      ),
      # change all inches to centimeters and remove inches column
      snow_depth_centimeters = if_else(
        is.na(snow_depth_centimeters) & !is.na(snow_depth_inches),
        snow_depth_inches * 2.54, # inches to centimeters
        snow_depth_centimeters
      ),
      #TODO: go through all pits files and make sure numbers for calculations are correct
      # recalculate snow weight (and pounds to kilogram conversion)
      snow_weight_kilograms = if_else(
        snow_depth_trace == "n" &
          (is.na(tube_and_snow_weight_pounds) | tube_and_snow_weight_pounds == 0) &
          (is.na(tube_tare_weight_pounds) | tube_tare_weight_pounds == 0),
        0, #TODO: do I want to do something with trace?
        round((tube_and_snow_weight_pounds - tube_tare_weight_pounds) * 0.4535924, 2)
      ),
      #TODO: round or not?, also two different numbers
      #0.453592
      #0.4535924
      # recalculate snow density
      #TODO: fix if trace changes stuff (0 depth will cause INF)
      snow_density_kilograms_meters_cubed = snow_weight_kilograms /
        (pi * ((0.04653836 / 2)^2) * (snow_depth_centimeters * 0.01)),
      #TODO two different dimensions of tubes?
      #0.04653836
      #0.046736
      # recalculate SWE (if trace, assume 0.1cm depth)
      snow_water_equivalent_millimeters = if_else(
        snow_depth_trace == "y",
        (0.1 * (snow_density_kilograms_meters_cubed / 100)),
        (snow_depth_centimeters * (snow_density_kilograms_meters_cubed / 100))
      ),
      # recalculate albedo
      albedo = (outgoing_radiation_1 + outgoing_radiation_2 + outgoing_radiation_3) /
        (incoming_radiation_1 + incoming_radiation_2 + incoming_radiation_3),
    ) |>
    select(-c(surface_temperature_fahrenheit, snow_depth_inches))
}


compare_calcs <- function(orig_df, recalc_df, column_to_compare) {
  save_list <- c()
  for (i in 1:nrow(orig_df)) {
    if (
      isTRUE(round(orig_df[i, column_to_compare], 2) != round(recalc_df[i, column_to_compare], 2))
    ) {
      save_list[i] <- FALSE
    } else {
      save_list[i] <- TRUE
    }
  }
  orig_df <- orig_df[!save_list, ]
  recalc_df <- recalc_df[!save_list, ]

  list(orig_df, recalc_df)
}
