test_that("hgch bar CatNum", {


  data <- sample_data("Cat-Num", n = 30, rep = FALSE)

  opts <- dsvizopts::dsviz_defaults()

  hgch_bar_CatNum(data, format_sample_num = "1,231.1")
  hgch_bar_CatNum(data, format_sample_num = "1,231.1", dataLabels_show = T)
})
