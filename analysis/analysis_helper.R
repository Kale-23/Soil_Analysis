theme_custom <- function(
  aesthetic = c("fill", "color", "none"),
  grid = c("yes", "no")
) {
  aesthetic <- match.arg(aesthetic)
  scale_layer <- switch(
    aesthetic,
    fill = scale_fill_paletteer_d("MetBrewer::Austria"),
    color = scale_color_paletteer_d("MetBrewer::Austria"),
    none = NULL
  )
  grid <- match.arg(grid)
  grid <- switch(
    grid,
    yes = element_line(color = "gray80"),
    no = element_blank()
  )

  list(
    scale_layer,
    theme_minimal(),
    theme(
      # global changes
      text = element_text(family = "Work Sans", color = "black"),
      #plot.background = element_rect(fill = "white", color = NA)
      panel.grid.major = grid,
      panel.grid.minor = element_blank(),
      #panel.background = element_rect(fill = "white", color = NA),

      # axis elements
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10, color = "black"),
      axis.ticks = element_line(color = "gray80"),
      axis.ticks.length = unit(0.01, "cm"),

      # legend elements
      legend.position = "right",
      legend.key.size = unit(0.35, "cm"),
      legend.key.spacing = unit(0.2, "cm"),
    )
  )
}


numeric_hist <- function(df) {
  # takes all numeric columns of df and creates a histogram for each column
  df |>
    select(where(is.numeric)) |>
    pivot_longer(everything()) |>
    ggplot(aes(x = value)) +
    geom_histogram(na.rm = TRUE) +
    facet_wrap(~name, scales = "free")
}

factor_bar <- function(df) {
  # takes all factor columns of df and creates a bar plot for each column
  df |>
    select(where(is.factor)) |>
    pivot_longer(everything()) |>
    count(name, value) |>
    ggplot(aes(x = value, y = n)) +
    geom_bar(stat = "identity", na.rm = TRUE) +
    facet_wrap(~name, scales = "free") +
    theme(axis.text.x = element_text(size = 5, angle = 45))
}

missing_plot <- function(df) {
  df |>
    mutate(across(everything(), as.character)) |>
    pivot_longer(everything()) |>
    mutate(
      is_missing = is.na(value),
      name = as.factor(name)
    ) |>
    group_by(name, is_missing) |>
    summarise(missing = n(), .groups = "drop") |>
    group_by(name) |>
    mutate(total_missing = sum(missing[is_missing])) |> # extract count of missing values
    ungroup() |>
    mutate(
      name = forcats::fct_reorder(name, total_missing),
      is_missing = factor(is_missing, levels = c(TRUE, FALSE))
    ) |>
    ggplot(aes(x = name, y = missing, fill = is_missing)) +
    geom_col() +
    labs(x = "Column", y = "Proportion of Total Columns", fill = "Is Missing") +
    scale_fill_manual(values = c("FALSE" = "green", "TRUE" = "red")) +
    theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1))
}

pairs_plots <- function(df) {
  df |>
    ggpairs(lower = list(continuous = wrap("points")), upper = "blank", diag = "blankDiag") +
    theme_cowplot() +
    theme_custom()
}

full_explore_output <- function(df, output_file) {
  # nicely formats exploratory plots
  df_hist <- numeric_hist(df)
  df_bar <- factor_bar(df)
  df_missing <- missing_plot(df)
  output_plot <- cowplot::plot_grid(df_missing, df_hist, df_bar, ncol = 1)
  ggsave(
    filename = output_file,
    plot = output_plot,
    width = 20,
    height = 30
  )
}
