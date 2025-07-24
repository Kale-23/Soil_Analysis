# --------------------------------
# Snow and Soil Frost Sampling App
# --------------------------------

# --------------------------------
# Importing Datasets and Helpers
# --------------------------------
setwd("~/Desktop/Soil_Work/Soil_Analysis/dashboard")
source("global.R")

# --------------------------------
# UI Elements
# --------------------------------
ui <- page_fluid(
  #theme = custom_theme,
  #theme = bs_theme(bootswatch = "morph"),
  tags$style(HTML("html, body { height: 100%; margin: 0; }")),
  div(
    style = "
    display: flex;
    flex-direction: column;
    height: 100vh;
    ",

    title_panel(total_height = "5vh"),

    global_ui(id = "global_1", total_height = "20vh"),

    navset_card_pill(
      height = "75vh",
      placement = "above",
      nav_panel(
        title = "Frost",
        frost_ui("frost_1", frost_data_df)
      ),
      nav_panel(
        title = "Pits",
        pits_ui("pits_1", pits_data_df)
      )
    )
  )

  #footer,
)

# --------------------------------
# Server
# --------------------------------
server <- function(input, output) {
  # load in datasets
  frost_data <- reactive({
    frost_data_df
  })
  pits_data <- reactive({
    pits_data_df
  })

  # global filters for use within datasets
  global_inputs <- global_server("global_1")

  # specific dataset servers
  frost_server(
    "frost_1",
    frost_data,
    global_inputs
  )
  pits_server(
    "pits_1",
    pits_data,
    global_inputs
  )
}

# --------------------------------
# Run App
# --------------------------------
shinyApp(ui = ui, server = server)
