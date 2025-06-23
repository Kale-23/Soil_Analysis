custom_checkbox <- function(id, data, data_type) {
  type_func <- switch(
    data_type,
    "numeric" = is.numeric,
    "factor" = is.factor,
  )

  col_names <- data |>
    select(where(type_func)) |>
    names()

  checkboxGroupInput(
    inputId = id,
    label = paste("Select", data_type, "columns"),
    choices = col_names,
    selected = col_names
  )
}
