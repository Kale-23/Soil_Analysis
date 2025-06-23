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
        snow_depth_inches * 0.3937007874, # inches to centimeters
        snow_depth_centimeters
      ),
      #TODO: go through all pits files and make sure numbers for calculations are correct
      # recalculate snow weight (and pounds to kilogram conversion)
      snow_weight_kilograms = if_else(
        snow_depth_trace == "Y" &
          (is.na(tube_and_snow_weight_pounds) | tube_and_snow_weight_pounds == 0) &
          (is.na(tube_tare_weight_pounds) | tube_tare_weight_pounds == 0),
        0, #TODO: do I want to do something with trace?
        (tube_and_snow_weight_pounds - tube_tare_weight_pounds) * 0.4535924
      ),
      # recalculate snow density
      #TODO: fix if trace changes stuff
      snow_density_kilograms_meters_cubed = snow_weight_kilograms /
        (pi * ((0.046736 / 2)^2) * (snow_depth_centimeters * 0.1)),
      # recalculate SWE (if trace, assume 0.1cm depth)
      snow_water_equivalent_millimeters = if_else(
        snow_depth_trace == "Y",
        (0.1 * (snow_density_kilograms_meters_cubed / 20)),
        (snow_depth_centimeters * (snow_density_kilograms_meters_cubed / 20))
      ),
      # recalculate albedo
      albedo = (outgoing_radiation_1 + outgoing_radiation_2 + outgoing_radiation_3) /
        (incoming_radiation_1 + incoming_radiation_2 + incoming_radiation_3),
    ) |>
    select(-c(surface_temperature_fahrenheit, snow_depth_inches))
}
