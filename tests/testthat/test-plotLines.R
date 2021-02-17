test_that("Plot lines", {

  # Data Labels format to Cat-Num
  data <- sample_data("Cat-Num", n = 30, rep = FALSE)
  opts <- dsvizopts::dsviz_defaults()
  l <- hgchmagic_prep(data, opts)
  hgch_bar_CatNum(data)
  hgch_bar_CatNum(data, plotLine_value_x = 3)
  hgch_bar_CatNum(data, plotLine_value_y = 1000)
  hgch_bar_CatNum(data, plotLine_value_y = 1000, plotLine_value_x = 3)
  hgch_bar_CatNum(data, plotLine_value_y = 1000, plotLine_value_x = 3, orientation = "hor")


  #opts$extra$plotLine_value_x <- 3
  #opts$extra$plotLine_value_y <- mean(data[[2]], na.rm = TRUE)
})
