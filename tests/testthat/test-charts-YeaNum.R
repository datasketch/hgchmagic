test_that("hgch bar YeaNum", {

  data <- sample_data("Yea-Num", n = 30, rep = FALSE)
  hgch_bar_YeaNum(data)
  hgch_bar_YeaNum(data, palette_colors = "#FEAFEA")

})


test_that("hgch line YeaNum", {

  data <- sample_data("Yea-Num", n = 30, rep = FALSE)
  hgch_line_YeaNum(data)
  hgch_line_YeaNum(data, palette_colors = "#FEAFEA", drop_na = TRUE)
  hgch_line_YeaNum(data, order = c("(NA)"), marker_enabled = FALSE)
})


test_that("hgch area YeaNum", {

  data <- sample_data("Yea-Num", n = 30, rep = FALSE)
  hgch_area_YeaNum(data)
  hgch_area_YeaNum(data, palette_colors = "#FEAFEA")
  hgch_area_YeaNum(data, order = c("(NA)"), marker_enabled = FALSE)

})
