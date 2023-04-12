test_that("multiplication works", {

  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  data <- data |> select(game_type, date, x)
  h_scatter <- hgch_scatter(data, var_cat = "game_type", var_dat = "date",var_num = "x")

  expect_equal(h_scatter$x$hc_opts$chart$type, "scatter")
  expect_equal(h_scatter$x$hc_opts$xAxis$type, "datetime")


  h <- hgch_scatter(data, var_cat = "game_type", var_dat = "date",var_num = "x",
                    palette_colors = c("#ffa92a", "#f06142"))

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))

  h <- hgch_scatter_CatDatNum(data)

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))


  data <- ggplot2::diamonds
  data <- data |> select(carat, x, everything())
  h <- hgch_scatter(data, var_num = c("x", "carat"))

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))

  h <- hgch_scatter_NumNum(data)

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))


  data <- ggplot2::diamonds
  data <- data |>
    group_by(clarity) |>
    summarise(x = sum(x, na.rm = T), y = sum(carat, na.rm = T))

  h <- hgch_scatter(data, var_num = c("x", "y"), var_cat = "clarity")

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))


  data <- ggplot2::diamonds
  data <- data |> select(clarity, x, y)

  h <- hgch_scatter(data, var_num = c("x", "y"), var_cat = "clarity")

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))

})
