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
        plotOutput(ns("pitsPlot"))
      ),
    )
  )
}

pits_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$pitsPlot <- renderPlot({
      hist(data())
    })
  })
}
