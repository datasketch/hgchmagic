test_that("Aggregations", {

  # data Cat
  data <- sample_data("Cat")
  opts <- dsvizopts::dsviz_defaults()

  l <- hgchmagic_prep(data, opts, ftype = "Cat")
  data_test <- l$d
  expect_equal(sort(names(data_test)), sort(c("a", "..count", "labels", "..colors")))

  data <- sample_data("Cat-Num-Num-Num-Cat-Dat")
  opts <- dsvizopts::dsviz_defaults()
  opts$preprocess$drop_na <- TRUE
  opts$postprocess$percentage <- TRUE
  l <- hgchmagic_prep(data, opts, ftype = "Cat")
  data_test <- l$d
  expect_equal(sort(names(data_test)) ,sort(c("a", "..percentage", "labels", "..colors")))

  # data Cat-Num
  data <- sample_data("Cat-Num")
  opts <- dsvizopts::dsviz_defaults()
  opts$summarize$agg <- "mean"
  l <- hgchmagic_prep(data, opts, ftype = "Cat-Num")
  data_test <- l$d
  data_test$b <- round(data_test$b)
  data_expe <- data %>%
    group_by(!!sym(names(data)[1])) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  names(data_expe) <- c("a", "b")
  data_expe$b <- round(data_expe$b)
  data_expe[[names(data_expe[1])]][is.na(data_expe[[names(data_expe[1])]])] <- "(NA)"

  expect_equal(names(data_test), c("a", "b", "labels", "..colors"))
  expect_equal(nrow(data_test), length(unique(data[[1]])))
  expect_equal(sum(data_test$b), as.numeric(sum(data_expe$b)))
  #
  # data <- sample_data("Cat-Num-Num-Num-Cat-Dat")
  # opts <- dsvizopts::dsviz_defaults()
  #
  # l <- hgchmagic_prep(data, opts, ftype = "Cat-Num")
  # data_test <- l$d
  # expect_equal(names(data_test) , c("a", "b", "..percentage", "c", "d", "e", "f", "..colors"))
  # expect_equal(nrow(data_test), length(unique(data[[1]])))
  #
  # # data Cat-Cat
  # data <- sample_data("Cat-Cat")
  # opts <- dsvizopts::dsviz_defaults()
  #
  # l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat")
  # data_test <- l$d
  # expect_equal(names(data_test), c("a", "b", "..count", "..percentage", "..colors"))
  #
  # data <- sample_data("Cat-Cat-Num-Num-Num-Cat-Dat")
  # opts <- dsvizopts::dsviz_defaults()
  #
  # l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat")
  # data_test <- l$d
  # expect_equal(names(data_test) , c(c("a", "b", "..count"), names(data_test)[-1:-3]))
  #
  # # data Cat-Cat-Num
  # data <- sample_data("Cat-Cat-Num")
  # opts <- dsvizopts::dsviz_defaults()
  #
  # l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat-Num")
  # data_test <- l$d
  # expect_equal(names(data_test), c("a", "b", "c",  "..percentage", "..colors"))
  #
  # data <- sample_data("Cat-Cat-Num-Num-Num-Cat-Dat")
  # opts <- dsvizopts::dsviz_defaults()
  #
  # l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat")
  # data_test <- l$d
  # expect_equal(names(data_test) , c(c("a", "b", "..count"), names(data_test)[-1:-3]))
  #
  # # data Dat
  # data <- sample_data("Dat")
  # opts <- dsvizopts::dsviz_defaults()
  #
  # l <- hgchmagic_prep(data, opts, ftype = "Dat")
  # data_test <- l$d
  # expect_equal(names(data_test), c("a", "..count", "..percentage", "group", "..date", "..colors", "..date_label"))
  #
  # data <- sample_data("Dat-Cat-Num-Num-Num-Cat-Dat")
  # opts <- dsvizopts::dsviz_defaults()
  #
  # l <- hgchmagic_prep(data, opts, ftype = "Dat")
  # data_test <- l$d
  # expect_equal(names(data_test) , c(c("a", "..count"), names(data_test)[-1:-2]))

})
