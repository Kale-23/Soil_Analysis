invisible(
  c(
    "zeallot", # multi unpacker (I am lazy)
    "readr", # read in files other than excel
    "dplyr", # data frame manipulation
    "tidyr",
    "purrr", # map/other functions for working with lists
    "stringr", # working with strings
    "forcats" # working with factors
  ) |>
    lapply(function(x) {
      if (suppressMessages(!require(x, character.only = TRUE))) {
        install.packages(x)
        library(x, character.only = TRUE)
      }
    })
)

# PITS
pits_data <- read_csv("~/Desktop/Soil_Work/cleaned_data/pits_final_data.csv")
pits_filtered <- pits_data |>
  filter(
    albedo >= 1 |
      incoming_radiation_1 >= 1000 |
      incoming_radiation_2 >= 1000 |
      incoming_radiation_3 >= 1000 |
      outgoing_radiation_1 >= 750 |
      outgoing_radiation_2 >= 750 |
      outgoing_radiation_3 >= 750 |
      snow_density_kilograms_meters_cubed >= 1000 |
      snow_water_equivalent_millimeters >= 80 |
      snow_weight_kilograms >= 0.2 |
      tube_tare_weight_pounds >= 1.5 |
      hour >= 16
  )
write_csv(pits_filtered, "~/Desktop/Soil_Work/cleaned_data/pits_check_values.csv")

# FROST
frost_data <- read_csv("~/Desktop/Soil_Work/cleaned_data/frost_final_data.csv")
problems(frost_data)
frost_filtered <- frost_data |>
  filter(
    frost_depth_1_centimeters >= 20 |
      hour >= 17 |
      shallow_frost_depth_centimeters < 0 |
      shallow_frost_depth_centimeters >= 7 |
      thaw_depth_centimeters < 0
  )
write_csv(frost_filtered, "~/Desktop/Soil_Work/cleaned_data/frost_check_values.csv")
