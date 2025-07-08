frost_ui <- function(id, df) {
  source("R/generic_checkbox.R")
  source("R/dynamic_filter.R")
  ns <- NS(id)
  card(
    #card_title(
    #  "Frost Data"
    #),
    card_body(
      layout_sidebar(
        sidebar = map(names(df), ~ make_ui(df[[.x]], .x, id)),
        #sidebar = accordion(
        #  accordion_panel(
        #    "Numeric",
        #    custom_checkbox(ns("frost_numeric"), df, "numeric")
        #  ),
        #  accordion_panel(
        #    "Categories",
        #    custom_checkbox(ns("frost_factor"), df, "factor")
        #  ),
        #),
        card(
          card_header(
            "frost 1"
          ),
          tableOutput(ns("frost_plot"))
        )
      )
    )
  )
}

frost_server <- function(id, react_data, global_inputs) {
  moduleServer(id, function(input, output, session) {
    #filtered_react_data <- reactive({
    #  selected_cols <- c(input$frost_numeric, input$frost_factor)
    #  req(length(selected_cols) > 0)

    #  req(react_data(), global_inputs())
    #  react_data() |>
    #    dplyr::filter(
    #      year %in% global_inputs()$year,
    #      water_year %in% global_inputs()$water_year,
    #      site_name %in% global_inputs()$site_name
    #    ) |>
    #    dplyr::select(
    #      all_of(
    #        selected_cols
    #      )
    #    )
    #})

    selected <- reactive({
      # make sure we have the react_data
      req(react_data())

      # filtering based on dynamic ui input
      filters <- map(
        names(react_data()),
        ~ {
          input_value <- input[[.x]] # dynamic ui input
          print(input_value)
          print(react_data()[[.x]])
          filter_var(react_data()[[.x]], input_value)
        }
      )
      #print(head(filters))
      reduce(filters, `&`) # if all values across a row are TRUE, will show that row
    })

    output$frost_plot <- renderTable({
      #print(head(react_data()[selected(), ]))
      react_data()[selected(), ]
    })
  })
}
