test_that("tooltip", {

  # percentage Cat-Num
  data <- data.frame(thinks = c("Rocks", "Papers", "Scissors"), total = c(23, 45, -10))
  opts <- dsvizopts::dsviz_defaults()

  #tooltip without variables
  opts$chart$tooltip <- "This is a test"
  l <- hgchmagic_prep(data, opts)
  d_test <- unique(l$d$labels) %>% unlist()
  expect_equal(d_test, "This is a test")

  #tooltip with variables
  opts$chart$tooltip <- "Pepite have {total} {thinks}"
  l <- hgchmagic_prep(data, opts)
  d_test <- unique(l$d$labels) %>% unlist()
  expect_equal(d_test, c("Pepite have 45 Papers", "Pepite have 23 Rocks", "Pepite have -10 Scissors"))

  #tooltip with percentage and count
  data <- data.frame(letter = sample(letters, 333, replace = T))
  opts$chart$tooltip <- "letters: {Count} ({%}%)"
  opts$style$format_sample_num <- "1,234.1"
  l <- hgchmagic_prep(data, opts, ftype = "Cat")
  d_test <- unique(l$d$labels) %>% unlist()
  d_expe <- data %>%
             group_by_all() %>%
              summarise(count = n())
  d_expe$pc <- round((d_expe$count/sum(d_expe$count)) * 100, 1)
  d_expe$label <- paste0("letters: ", d_expe$count, " (",d_expe$pc, "%)")
  expect_equal(d_test, unique(d_expe$label))

})
