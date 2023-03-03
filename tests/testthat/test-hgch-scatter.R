test_that("multiplication works", {
  data <- lubridate::lakers
  data$date <- lubridate::ymd(data$date)
  data <- data |> select(game_type, date, x)
  hgch_scatter(data, var_cat = "game_type", var_dat = "date",var_num = "x")
  hgch_scatter(data, var_cat = "game_type", var_dat = "date",var_num = "x",
               palette_colors = c("#ffa92a", "#f06142"))
  hgch_scatter_CatDatNum(data)
})
