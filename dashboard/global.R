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
