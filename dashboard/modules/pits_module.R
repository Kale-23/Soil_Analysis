pits_ui <- function(id, df) {
  source("R/generic_checkbox.R")
  ns <- NS(id)
  card(
    #card_title(
    #  "Pits Data"
    #),
    card_body(
      layout_sidebar(
        sidebar = accordion(
          accordion_panel(
            "Numeric",
            custom_checkbox(ns("pits_numeric"), df, "numeric")
          ),
          accordion_panel(
            "Categories",
            custom_checkbox(ns("pits_factor"), df, "factor")
          ),
        ),
        card(
          card_header(
            "pits plot"
          ),
          tableOutput(ns("pits_plot"))
        ),
      )
    )
  )
}

pits_server <- function(id, data, global_inputs) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- reactive({
      selected_cols <- c(input$pits_numeric, input$pits_factor)
      req(length(selected_cols) > 0)

      req(data(), global_inputs())
      data() |>
        dplyr::filter(
          year %in% global_inputs()$year,
          water_year %in% global_inputs()$water_year,
          site_name %in% global_inputs()$site_name
        ) |>
        dplyr::select(
          all_of(selected_cols)
        )
    })

    output$pits_plot <- renderTable({
      filtered_data()
    })
  })
}
