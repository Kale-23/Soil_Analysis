# global controls
global_ui <- function(id, total_height) {
  ns <- NS(id)
  navset_card_pill(
    placement = "above",
    height = total_height,
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
    ),
    nav_panel(
      title = "Site",
      layout_columns(
        col_widths = c(8, 4),
        selectInput(
          inputId = ns("site_name"),
          label = "Site Name",
          multiple = TRUE,
          list(
            "Kingman Farm" = "kingman",
            "Thompson Farm Canopy" = "thompson canopy",
            "Thompson Farm Field" = "thompson field"
          ),
          selected = c("kingman", "thompson canopy", "thompson field") # auto select these
        ),
        actionButton(
          inputId = ns("site_name_reset"),
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

    observeEvent(input$site_name_reset, {
      updateSelectInput(
        session = session,
        inputId = "site_name",
        selected = c("kingman", "thompson canopy", "thompson field")
      )
    })

    reactive({
      list(
        year = seq(input$year[1], input$year[2]),
        water_year = seq(input$water_year[1], input$water_year[2]),
        site_name = input$site_name
      )
    })
  })
}
