test_that("Data Labels Format", {

  # Data Labels format to Cat-Num
  data <- sample_data("Cat-Num", n = 30, rep = FALSE)
  opts <- dsvizopts::dsviz_defaults()
  l <- hgchmagic_prep(data, opts)
  expect_equal( l$theme$format_dataLabels, "{point.y:,.2f}%")

  data <- sample_data("Cat-Yea-Num", n = 30, rep = FALSE)
  opts <- dsvizopts::dsviz_defaults()
  l <- hgchmagic_prep(data, opts)
  expect_equal( l$theme$format_dataLabels, "{point.y:,.2f}%")


  data <- sample_data("Cat-Cat-Num", n = 30)
  opts <- dsvizopts::dsviz_defaults()
  l <- hgchmagic_prep(data, opts,  plot = "treemap")

})
