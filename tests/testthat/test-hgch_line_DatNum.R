test_that("gg bar DatNum", {


  data <- sample_data("Dat-Num", n = 30, rep = FALSE)

  data <- data.frame(fecha = c("2020/05/03","2020/05/04"),
                   vals = 1:2)

  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts)

  #expect_equal(lapply(l$d, class), list(a = "Date", b = "numeric", ..colors = "character"))

  hgch_line_DatNum(data)
  hgch_line_DatNum(data, format_dat = "%b %d %Y")

  hgch_line_DatNum(data, locale = "ru-RU", format_dat = "%b %d %Y")
  hgch_line_DatNum(data, locale = "es-CO", format_dat = "%B %d %Y")


})
