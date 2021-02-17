test_that("Order in categorical variables", {


  # data Cat-Num
  data <- data.frame(thinks = c("Rocks", "Papers", "Scissors", NA),
                     total = c(23, 45, -10, 1),
                     otros = c("One", "Two", "Three", "BLA"),
                     nature = c("Cat", "Fox", "Spider", "BLU"))
  opts <- dsvizopts::dsviz_defaults()
  opts$postprocess$order <- c("Scissors", "Papers", "Rocks")
  opts$preprocess$drop_na <- T

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Num")
  data_test <- l$d
  expect_equal(unique(data_test$a), c("Scissors", "Papers", "Rocks"))



  # data Cat-Cat-Num
  data <- data.frame(thinks = c("Rocks", "Papers", "Scissors", NA),
                     otros = c("One", "Two", "Three", "BLA"),
                     total = c(23, 45, -10, 1),
                     nature = c("Cat", "Fox", "Spider", "BLU"))
  opts <- dsvizopts::dsviz_defaults()
  opts$postprocess$order_legend <- c("Scissors", "Papers", "Rocks")

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat-Num")
  data_test <- l$d
  expect_equal(unique(data_test$a), c("Scissors", "Papers", "Rocks", "(NA)"))

  opts <- dsvizopts::dsviz_defaults()
  opts$postprocess$order <- c("Three", "BLA")

  l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat-Num")
  data_test <- l$d
  expect_equal(unique(data_test$b), c("Three", "BLA","One", "Two"))

})
