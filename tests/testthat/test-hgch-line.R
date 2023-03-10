test_that("Line", {
  dates <- seq(as.POSIXct("2022-01-01"), as.POSIXct("2022-01-10"), by = "day")
  values <- rnorm(length(dates))
  df <- data.frame(date = dates, value = values)
  hgch_line(df, var_dat = "date", var_num = "value")

  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  data <- data |>
    tidyr::drop_na(x) |>
    group_by(date) |>
    summarise(x = sum(x)) |> dplyr::arrange(date)
  hgch_line(data, var_dat = "date", var_num = "x")

  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  hgch_line_Dat(data)
  data <- data |> select(date, x, everything())
  hgch_line_DatNum(data)

  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  data <- data |> group_by(game_type, date) |> summarise(x = sum(x, na.rm = T))
  hgch_line(data, var_cat = "game_type", var_dat = "date", var_num = "x",
            palette_colors = c("#ffa92a"))
  hgch_line(data, var_cat = "game_type", var_dat = "date", var_num = "x",
            palette_colors = c("#ffa92a", "#f06142"))

  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  data <- data |> select(game_type, date, everything())
  hgch_line_CatDat(data)

  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  data <- data |> select(game_type, date, x, everything())
  hgch_line_CatDatNum(data, agg = "mean")


  data <- lubridate::lakers[1:100,]
  data$date <- lubridate::ymd(data$date)
  data$valor <- runif(nrow(data), 0, 1)
  data$indicador <- runif(nrow(data), 1000, 5000)
  data <- data |> select(date, valor, indicador)
  hgch_line(data, var_dat = "date", var_num = c("valor", "indicador"))

})
