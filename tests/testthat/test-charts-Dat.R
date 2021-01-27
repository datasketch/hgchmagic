test_that("hgch line Dat", {
  data <- sample_data("Dat-Cat-Dat", n = 30, rep = TRUE)
  hgch_line_Dat(data)
  hgch_line_Dat(data, tooltip = paste0("{", names(data)[3],"}"))
  hgch_line_Dat(data, palette_colors = "#FEAFEA")

})

test_that("hgch scatter Dat", {
  data <- sample_data("Dat", 300, rep = TRUE)
  hgch_scatter_Dat(data)
  })

test_that("hgch area Dat", {
  data <- sample_data("Dat", 300, rep = TRUE)
  hgch_area_Dat(data, palette_colors = "#0FC333", format_dat = "%B %d")
})
