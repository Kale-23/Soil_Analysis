pits_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        title = "Pits"
      ),
      card(
        card_header(
          "DistPlot 2"
        ),
        tableOutput(ns("pits_plot"))
      ),
    )
  )
}

pits_server <- function(id, data, global_inputs) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- reactive({
      req(data(), global_inputs())
      data() |>
        dplyr::filter(
          year %in% global_inputs()$year,
          water_year %in% global_inputs()$water_year
        )
    })

    output$pits_plot <- renderTable({
      filtered_data()
    })
  })
}
