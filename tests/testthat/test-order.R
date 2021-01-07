test_that("Order in categorical variables", {


  # data Cat-Num
  data <- data.frame(thinks = c("Rocks", "Papers", "Scissors", NA),
                     total = c(23, 45, -10, 1),
                     otros = c("One", "Two", "Three", "BLA"),
                     nature = c("Cat", "Fox", "Spider", "BLU"))
  opts <- dsvizopts::dsviz_defaults()
  opts$postprocess$order <- c("Scissors", "Papers", "Rocks")
  opts$preprocess$drop_na <- F

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Num")
  data_test <- l$d
  expect_equal(names(data_test), c("a", "..count"))


})
