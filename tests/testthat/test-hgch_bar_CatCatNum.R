test_that("hgch bar CatCatNum", {


  data <- sample_data("Cat-Cat-Num", n = 30, rep = FALSE)

  opts <- dsvizopts::dsviz_defaults()

  hgch_bar_CatCatNum(data, format_sample_num = "1,231.1")
  hgch_bar_CatCatNum(data, format_sample_num = "1231", dataLabels_show = T)
})
