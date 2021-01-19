test_that("Percentage", {


  # percentage Cat
  data <- sample_data("Cat", n = 30, rep = FALSE)
  opts <- dsvizopts::dsviz_defaults()
  opts$postprocess$percentage <- TRUE
  l <- hgchmagic_prep(data, opts, ftype = "Cat")
  expect_equal(sum(l$d$..percentage), 100)

  # percentage Cat-Num
  data <- sample_data("Cat-Num", n = 30, rep = FALSE)
  opts <- dsvizopts::dsviz_defaults()
  opts$postprocess$percentage <- TRUE
  l <- hgchmagic_prep(data, opts)
  expect_equal(sum(l$d$..percentage), 100)


  # percentage Cat-Cat-Num by first column
  data <- data.frame(Cosas = c("Piedra", "Piedra", "Hoja", "Hoja", NA, "Tijera", "Tijera", "Hoja", "Piedra"),
                     Seres = c("Elfo", "Enano", "Elfo", "Gigante", "Golondrina", "Mago", "Enano", "Mago", "Mago"),
                     Poder = runif(9, 1, 1000))

  opts <- dsvizopts::dsviz_defaults()
  opts$postprocess$percentage <- TRUE
  opts$postprocess$percentage_col <- "Cosas" # columna a
  l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat-Num")
  d <- l$d
  per_a <- d %>% group_by(a) %>% summarise(Total = sum(..percentage, na.rm = T))
  expect_equal(unique(per_a$Total)[1], 100)


  # percentage Cat-Cat-Num by second column
  opts <- dsvizopts::dsviz_defaults()
  opts$postprocess$percentage <- TRUE
  opts$postprocess$percentage_col <- "Seres" # columna a
  l <- hgchmagic_prep(data, opts, ftype = "Cat-Cat-Num")
  d <- l$d
  per_b <- d %>% group_by(b) %>% summarise(Total = sum(..percentage, na.rm = T))
  expect_equal(unique(per_b$Total), 100)



  data <- sample_data("Dat-Num", n = 30, rep = FALSE)
  opts <- dsvizopts::dsviz_defaults()
  opts$postprocess$percentage <- TRUE
  l <- hgchmagic_prep(data, opts, ftype = "Dat-Num")
  expect_equal(sum(l$d$..percentage), 100)



  # Percentage Cat-Dat-Num
  data <- sample_data("Cat-Dat-Num")
  opts <- dsvizopts::dsviz_defaults()
  opts$postprocess$percentage <- TRUE
  opts$postprocess$percentage_col <- names(data)[1] # columna a
  l <- hgchmagic_prep(data, opts, ftype = "Cat-Dat-Num")
  d <- l$d
  per_a <- d %>% group_by(a) %>% summarise(Total = sum(..percentage, na.rm = T))
  expect_equal(unique(per_a$Total), 100)

})
