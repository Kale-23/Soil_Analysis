invisible(
  c(
    "zeallot", # multi unpacker (I am lazy)
    "lubridate", # makes date handling easier
    "readxl", # excel import
    "readr", # read in files other than excel
    "dplyr", # data frame manipulation
    "tidyr",
    "purrr", # functional programming helpers
    "stringr", # working with strings
    "ggplot2", # plotting
    "cowplot", # meta plot formatting
    "forcats", # working with factors
    "rlang", # string/var interchange (!!sym() stuff)
    "GGally", #TODO: really only want ggpairs from this (DO I?)
    "paletteer", #TODO actually use this or dump it
    "RSQLite"
  ) |>
    lapply(function(x) {
      if (suppressMessages(!require(x, character.only = TRUE))) {
        install.packages(x)
        library(x, character.only = TRUE)
      }
    })
)
