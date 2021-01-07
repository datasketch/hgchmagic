test_that("Aggregations", {

  # data Cat
  data <- sample_data("Cat")
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Cat")
  data_test <- l$d
  expect_equal(names(data_test), c("a", "..count"))

  data <- sample_data("Cat-Num-Num-Num-Cat-Dat")
  opts <- dsvizopts::dsviz_defaults()
  opts$preprocess$drop_na <- TRUE

  l <- hgchmagic_prep(data, opts, ftype = "Cat")
  data_test <- l$d
  expect_equal(names(data_test) , c(c("a", "..count"), names(data_test)[-1:-2]))

  # data Cat-Num
  data <- sample_data("Cat-Num")
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Num")
  data_test <- l$d
  expect_equal(names(data_test), c("a", "b"))
  expect_equal(nrow(data_test), length(unique(data[[1]])))

  data <- sample_data("Cat-Num-Num-Num-Cat-Dat")
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Num")
  data_test <- l$d
  expect_equal(names(data_test) , c("a", "b", "c", "d", "e", "f"))
  expect_equal(nrow(data_test), length(unique(data[[1]])))

  # data Cat-Cat
  data <- sample_data("Cat-Cat")
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat")
  data_test <- l$d
  expect_equal(names(data_test), c("a", "b", "..count"))

  data <- sample_data("Cat-Cat-Num-Num-Num-Cat-Dat")
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat")
  data_test <- l$d
  expect_equal(names(data_test) , c(c("a", "b", "..count"), names(data_test)[-1:-3]))

  # data Cat-Cat-Num
  data <- sample_data("Cat-Cat-Num")
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat-Num")
  data_test <- l$d
  expect_equal(names(data_test), c("a", "b", "c"))

  data <- sample_data("Cat-Cat-Num-Num-Num-Cat-Dat")
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat")
  data_test <- l$d
  expect_equal(names(data_test) , c(c("a", "b", "..count"), names(data_test)[-1:-3]))

  # data Dat
  data <- sample_data("Dat")
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Dat")
  data_test <- l$d
  expect_equal(names(data_test), c("a", "..count"))

  data <- sample_data("Dat-Cat-Num-Num-Num-Cat-Dat")
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Dat")
  data_test <- l$d
  expect_equal(names(data_test) , c(c("a", "..count"), names(data_test)[-1:-2]))

})
