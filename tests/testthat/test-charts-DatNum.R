test_that("hgch line DatNum", {


  data <- data.frame(fecha = c("2020/10/03","2020/10/04"),
                   vals = 1:2)

  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts)

  #expect_equal(lapply(l$d, class), list(a = "Date", b = "numeric", ..colors = "character"))

  hgch_line_DatNum(data)
  hgch_line_DatNum(data, format_dat = "%b %d %Y")
  hgch_line_DatNum(data, locale = "ru-RU")
 # hgch_line_DatNum(data, locale = "ru-RU", format_dat = "%b %d %Y")
  hgch_line_DatNum(data, locale = "es-CO")
  hgch_line_DatNum(data, locale = "es-CO", format_dat = "%B %d %Y")
  hgch_line_DatNum(data, locale = "de-DE")
  #hgch_line_DatNum(data, locale = "de-DE", format_dat = "%B %d %Y")


  data <- sample_data("Dat-Num-Cat-Dat", n = 30, rep = FALSE)
  hgch_line_DatNum(data)
  hgch_line_DatNum(data, tooltip = paste0("{", names(data)[4],"}"))
  hgch_line_DatNum(data, palette_colors = "#FEAFEA")

})

test_that("hgch scatter DatNum", {
  data <- sample_data("Dat-Num")
  hgch_scatter_DatNum(data)
  })

test_that("hgch area DatNum", {
  data <- sample_data("Dat-Num")
  hgch_area_DatNum(data, palette_colors = "#0FC333", format_dat = "%Y %B %d")
})
