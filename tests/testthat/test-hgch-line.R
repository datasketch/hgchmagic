test_that("Line", {
  dates <- seq(as.POSIXct("2022-01-01"), as.POSIXct("2022-01-10"), by = "day")
  values <- rnorm(length(dates))
  df <- data.frame(date = dates, value = values)

  h_line <- hgch_line(df, var_dat = "date", var_num = "value")

  expect_equal(h_line$x$hc_opts$chart$type, "line")
  expect_equal(h_line$x$hc_opts$xAxis$type, "datetime")



  #                          hgch_line                          ~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  data <- data |>
    tidyr::drop_na(x) |>
    group_by(date) |>
    summarise(x = sum(x)) |> dplyr::arrange(date)
  h <- hgch_line(data, var_dat = "date", var_num = "x", hor_title = "fecha", ver_title = "valor")

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))


  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  h <- hgch_line_Dat(data)

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))



  data <- data |> select(date, x, everything())
  h <- hgch_line_DatNum(data)

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))


  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  data <- data |> group_by(game_type, date) |> summarise(x = sum(x, na.rm = T))
  data$aver <- "hola"
  h <- hgch_line(data, var_cat = "game_type", var_dat = "date", var_num = "x",
            palette_colors = c("#ffa92a"))

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))

  h <- hgch_line(data, var_cat = "game_type", var_dat = "date", var_num = "x",
            palette_colors = c("#ffa92a", "#f06142"),  hor_title = "fecha", ver_title = "valor")

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))


  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  data <- data |> select(game_type, date, everything())
  h <- hgch_line_CatDat(data, hor_title = "fecha", ver_title = "valor")

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))


  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  data <- data |> select(game_type, date, x, everything())

  h <- hgch_line_CatDatNum(data, agg = "mean")

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))


  data <- lubridate::lakers[1:100,]
  data$date <- lubridate::ymd(data$date)
  data$valor <- runif(nrow(data), 0, 1)
  data$indicador <- runif(nrow(data), 1000, 5000)
  data <- data |> select(date, valor, indicador)

  h <- hgch_line(data, var_dat = "date", var_num = c("valor", "indicador"))

  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))


  # data <- lubridate::lakers
  # data <- data |> select(date, `nume 1` = x,  `nume 2` = y) |> tidyr::drop_na()
  # #data$..labels <- " "
  # h <- hgch_line_DatNumNum(data)

})
