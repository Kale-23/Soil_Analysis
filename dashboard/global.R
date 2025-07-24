invisible(
  c(
    "shiny",
    "bslib",
    "dplyr",
    "purrr"
  ) |>
    lapply(function(x) {
      if (suppressMessages(!require(x, character.only = TRUE))) {
        install.packages(x)
        library(x, character.only = TRUE)
      }
    })
)

# load in datasets created in analysis scripts
frost_data_df <- readRDS("data/frost_data.RData")
pits_data_df <- readRDS("data/pits_data.RData")

# simple ui elements (no servers needed)
source("R/title_panel.R")
source("R/footer.R")
source("R/theme.R")

# ui/server modules for specific datasets
source("modules/global_module.R")
source("modules/frost_module.R")
source("modules/pits_module.R")
