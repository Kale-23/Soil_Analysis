# --------------------------------
# Snow and Soil Frost Sampling App
# --------------------------------

# load in required libraries
invisible(
  c(
    "shiny",
    "bslib"
  ) |>
    lapply(function(x) {
      if (suppressMessages(!require(x, character.only = TRUE))) {
        install.packages(x)
        library(x, character.only = TRUE)
      }
    })
)

# --------------------------------
# Load in Datasets
# --------------------------------

# --------------------------------
# Create Elements
# --------------------------------

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

# frost dataset
frost <- layout_sidebar(
  sidebar = accordion(
    accordion_panel(
      "Categories",
    ),
    accordion_panel(
      "Numeric",
    ),
  ),
  card(
    card_header(
      "DistPlot 1"
    ),
    plotOutput("distPlot")
  )
)

pits <- layout_sidebar(
  sidebar = sidebar(
    title = "Pits"
  ),
  card(
    card_header(
      "DistPlot 2"
    ),
    plotOutput("distPlot2")
  ),
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
      frost,
    ),
    accordion_panel(
      "Pits Data",
      value = "pits",
      pits,
    )
  )
)

# --------------------------------
# Server
# --------------------------------
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$year))
  })
  output$distPlot2 <- renderPlot({
    hist(rnorm(input$year))
  })
}

# --------------------------------
# Run App
# --------------------------------
shinyApp(ui = ui, server = server)
