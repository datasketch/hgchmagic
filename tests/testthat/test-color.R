test_that("Color test", {


  # data Cat-Num
  data <- sample_data("Cat-Num")
  opts <- dsvizopts::dsviz_defaults()
  opts$style$color_by <- names(data)[1]

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Num")
  data_test <- l$d
  expect_equal(unique(data_test$..colors), c("#385573", "#ffa92a", "#f06142", "#99e8b3", "#32a8ce", "#996295"))

  data <- sample_data("Cat-Num")
  opts <- dsvizopts::dsviz_defaults()
  opts$style$color_by <- names(data)[1]
  opts$theme$palette_type <- "sequential"

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Num")
  data_test <- l$d
  expect_equal(unique(data_test$..colors), c("#b8f3ca", "#add79c", "#8dbd93", "#70a38e", "#568989", "#3f6f83"))

  data <- sample_data("Cat-Num")
  opts <- dsvizopts::dsviz_defaults()
  opts$style$color_by <- names(data)[2]

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Num")
  data_test <- l$d
  #expect_equal(unique(data_test$..colors), c("#b8f3ca", "#add79c", "#8dbd93", "#70a38e", "#568989", "#3f6f83"))


  # data Dat-Num
  data <- sample_data("Dat-Num")
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Dat-Num")
  data_test <- l$d
  expect_equal(unique(data_test$..colors), c("#385573"))


  # data Cat-Dat-Num
  data <- sample_data("Cat-Dat-Num")
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Dat-Num")
  data_test <- l$d
  expect_equal(unique(data_test$..colors), c("#385573"))

  opts <- dsvizopts::dsviz_defaults()
  opts$style$color_by <- names(data)[1]
  l <- hgchmagic_prep(data, opts, ftype = "Cat-Dat-Num")
  data_test <- l$d
  expect_equal(unique(data_test$..colors), c("#385573", "#ffa92a", "#f06142", "#99e8b3", "#32a8ce", "#996295"))

})




test_that("Color in highlight value", {


  # data Cat-Num
  data <- data.frame(thinks = c("Rocks", "Papers", "Scissors", NA),
                     total = c(23, 45, -10, 1),
                     otros = c("One", "Two", "Three", "BLA"),
                     nature = c("Cat", "Fox", "Spider", "BLU"))
  opts <- dsvizopts::dsviz_defaults()
  opts$chart$highlight_value <- "Rocks"

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Num")
  data_test <- l$d
  expect_equal(data_test$..colors[data_test$a == "Rocks"], "#50c5b7")

  opts$chart$highlight_value_color <- "#FEAFEA"
  l <- hgchmagic_prep(data, opts, ftype = "Cat-Num")
  data_test <- l$d
  expect_equal(data_test$..colors[data_test$a == "Rocks"], "#FEAFEA")


  # data Cat-Cat-Num
  data <- data.frame(thinks = c("Rocks", "Papers", "Scissors", NA),
                     otros = c("One", "Two", "Three", "BLA"),
                     total = c(23, 45, -10, 1),
                     nature = c("Cat", "Fox", "Spider", "BLU"))
  opts <- dsvizopts::dsviz_defaults()
  opts$chart$highlight_value <- "Rocks"

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat-Num")
  data_test <- l$d
  expect_equal(unique(data_test$..colors[data_test$a == "Rocks"]), "#50c5b7")

})
