numeric_hist <- function(df) {
  df |>
    select(where(is.numeric)) |>
    pivot_longer(everything()) |>
    ggplot(aes(x = value)) +
    geom_histogram() +
    facet_wrap(~name, scales = "free")
}

factor_bar <- function(df) {
  df |>
    select(where(is.factor)) |>
    pivot_longer(everything()) |>
    count(name, value) |>
    ggplot(aes(x = value, y = n)) +
    geom_bar(stat = "identity") +
    facet_wrap(~name, scales = "free") +
    theme(axis.text.x = element_text(size = 5, angle = 45))
}

full_explore <- function(df) {
  df_hist <- numeric_hist(df)
  df_bar <- factor_bar(df)
  cowplot::plot_grid(df_hist, df_bar, ncol = 1)
}
