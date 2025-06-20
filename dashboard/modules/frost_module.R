frost_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
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
        plotOutput(ns("frostPlot"))
      )
    )
  )
}

frost_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$frostPlot <- renderPlot({
      hist(data())
    })
  })
}
