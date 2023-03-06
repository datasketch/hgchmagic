test_that("multiplication works", {
  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  data <- data |> select(game_type, date, x)
  hgch_scatter(data, var_cat = "game_type", var_dat = "date",var_num = "x")
  hgch_scatter(data, var_cat = "game_type", var_dat = "date",var_num = "x",
               palette_colors = c("#ffa92a", "#f06142"))
  hgch_scatter_CatDatNum(data)


  data <- ggplot2::diamonds
  data <- data |> select(carat, x, everything())
  hgch_scatter(data, var_num = c("x", "carat"))

  data <- ggplot2::diamonds
  data <- data |>
    group_by(clarity) |>
    summarise(x = sum(x, na.rm = T), y = sum(carat, na.rm = T))
  hgch_scatter(data, var_num = c("x", "y"), var_cat = "clarity")

  data <- ggplot2::diamonds
  data <- data |> select(clarity, x, y)
  hgch_scatter(data, var_num = c("x", "y"), var_cat = "clarity")

})
