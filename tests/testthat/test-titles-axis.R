
test_that("Axes names", {

  # data Cat
  data <- sample_data("Cat", 50)
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Cat")
  axis_x <- l$titles$x
  axis_y <- l$titles$y
  expect_equal(names(data), axis_x)
  expect_equal("Count", axis_y)

  # data Cat-Num
  data <- sample_data("Cat-Num-Dat-Cat", 50)
  opts <- dsvizopts::dsviz_defaults()
  opts$chart$orientation <- "hor"

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Num")
  axis_x <- l$titles$x
  axis_y <- l$titles$y
  expect_equal(axis_x, names(data)[2])
  expect_equal(axis_y, names(data)[1])


  # data Dat
  data <- sample_data("Dat-Num-Dat-Cat", 50)
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Dat-Num")
  axis_x <- l$titles$x
  axis_y <- l$titles$y
  expect_equal(axis_x, names(data)[1])
  expect_equal(axis_y, names(data)[2])

  # data Cat-Cat-Num
  data <- sample_data("Cat-Cat-Num", 50)
  opts <- dsvizopts::dsviz_defaults()
  opts$chart$orientation <- "hor"
  l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat-Num")
  axis_x <- l$titles$x
  axis_y <- l$titles$y
  expect_equal(axis_x, names(data)[3])
  expect_equal(axis_y, names(data)[2])


  # data Cat-Cat-Num
  data <- sample_data("Cat-Cat-Num", 50)
  opts <- dsvizopts::dsviz_defaults()
  opts$chart$orientation <- "hor"
  opts$title$ver_title <- "Ver title"
  opts$title$hor_title <- "Hor title"
  l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat-Num")

  expect_equal(l$titles$x, "Ver title")
  expect_equal(l$titles$y, "Hor title")


  # data Num-Num
  data <- sample_data("Num-Num", 50)
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Num-Num")
  axis_x <- l$titles$x
  axis_y <- l$titles$y
  expect_equal(axis_x, names(data)[1])
  expect_equal(axis_y, names(data)[2])

  # data Cat-Num-Num
  data <- sample_data("Cat-Num-Num", 50)
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Num-Num")
  axis_x <- l$titles$x
  axis_y <- l$titles$y
  expect_equal(axis_x, names(data)[2])
  expect_equal(axis_y, names(data)[3])

})
