test_that("hgch bar CatNum", {

  library(homodatum)
  data <- sample_data("Cat-Num", n = 30, nlevels = 20, rep = FALSE)

  opts <- dsvizopts::dsviz_defaults()

  hgch_treemap_CatNum(data, treemap_layout = "squarified")

  hgch_treemap_CatNum(data, treemap_layout = "stripes")
  hgch_treemap_CatNum(data, treemap_layout = "stripes", treemap_direction = "horizontal")

  hgch_treemap_CatNum(data, treemap_layout = "strip")
  hgch_treemap_CatNum(data, treemap_layout = "sliceAndDice")



})
