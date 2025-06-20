# --------------------------------
# Snow and Soil Frost Sampling App
# --------------------------------

setwd("~/Desktop/Soil_Work/Soil_Analysis/dashboard")

source("global.R")
source("data/import.R")

source("modules/frost_module.R")
source("modules/pits_module.R")
# top bar info
title_panel <- titlePanel(
  title = div(
    img(src = "snow.png", height = "30px", style = "margin-right: 10px;"),
    "Snow and Frost Data Explorer"
  ),
  windowTitle = "Snow and Frost Data Explorer"
)

# global controls
global_controls <- card(
  card_header(
    "Global",
  ),
  sliderInput(
    "year",
    "Water Year",
    min = 2011,
    max = 2025,
    value = 2025,
  )
)

# --------------------------------
# UI Elements
# --------------------------------
ui <- page_fluid(
  title_panel,
  global_controls,
  accordion(
    id = "main_sections",
    accordion_panel(
      "Frost Data",
      value = "frost",
      frost_ui("frost_1")
    ),
    accordion_panel(
      "Pits Data",
      value = "pits",
      pits_ui("pits_1")
    )
  )
)

# --------------------------------
# Server
# --------------------------------
server <- function(input, output) {
  frost_server(
    "frost_1",
    reactive({
      rnorm(100)
    })
  )
  frost_server(
    "pits_1",
    reactive({
      rnorm(100)
    })
  )
}

# --------------------------------
# Run App
# --------------------------------
shinyApp(ui = ui, server = server)
