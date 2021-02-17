test_that("Data Labels Format", {

  # Data Labels format to Cat-Num
  data <- sample_data("Dat-Num", n = 30, rep = FALSE)
  opts <- dsvizopts::dsviz_defaults()
  l <- hgchmagic_prep(data, opts, ftype = "Dat-Num")
  # title x axis
  expect_equal(l$titles$x, names(data)[1])
  # title y axis
  expect_equal(l$titles$y, names(data)[2])

})
