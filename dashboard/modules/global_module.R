# global controls
global_ui <- function(id) {
  ns <- NS(id)
  navset_card_pill(
    placement = "above",
    nav_panel(
      title = "Year",
      layout_columns(
        col_widths = c(8, 4),
        sliderInput(
          inputId = ns("year"),
          label = "Year",
          min = 2011,
          max = 2025,
          value = c(2011, 2025),
        ),
        actionButton(
          inputId = ns("year_reset"),
          label = "Reset Filter"
        )
      )
    ),
    nav_panel(
      title = "Water Year",
      layout_columns(
        col_widths = c(8, 4),
        sliderInput(
          inputId = ns("water_year"),
          label = "Water Year",
          min = 2011,
          max = 2025,
          value = c(2011, 2025),
        ),
        actionButton(
          inputId = ns("water_year_reset"),
          label = "Reset Filter"
        )
      )
    )
  )
}

global_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$year_reset, {
      updateSliderInput(
        session = session,
        inputId = "year",
        value = c(2011, 2025)
      )
    })

    observeEvent(input$water_year_reset, {
      updateSliderInput(
        session = session,
        inputId = "water_year",
        value = c(2011, 2025)
      )
    })

    reactive({
      list(
        year = seq(input$year[1], input$year[2]),
        water_year = seq(input$water_year[1], input$water_year[2])
      )
    })
  })
}
