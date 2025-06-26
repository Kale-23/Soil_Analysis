title_panel <- function(total_height) {
  card(
    height = total_height,
    titlePanel(
      title = div(
        img(src = "www/snow.png", height = "20px", style = "margin-right: 10px;"),
        "Snow and Frost Data Explorer"
      ),
      windowTitle = "Snow and Frost Data Explorer"
    )
  )
}
