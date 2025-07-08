# taken from https://mastering-shiny.org/action-dynamic.html

# ui side
make_ui <- function(x, var, id) {
  ns <- NS(id)
  if (var %in% c("site_name", "water_year", "date")) {
    # global control columns
    NULL
  } else if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(ns(var), var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(ns(var), var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

# server side
filter_var <- function(x, val) {
  #print(str(x))
  #print(names(x))
  #print(val)
  if (names(x) %in% c("site_name", "water_year", "date")) {
    TRUE
  } else if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}
