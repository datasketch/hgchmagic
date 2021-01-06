test_that("tooltip", {

  # percentage Cat-Num
  data <- data.frame(thinks = c("Rocks", "Papers", "Scissors"), total = c(23, 45, -10))
  opts <- dsvizopts::dsviz_defaults()

  #tooltip without variables
  opts$chart$tooltip <- "This is a test"
  l <- hgchmagic_prep(data, opts)
  expect_equal(l$tooltip, "This is a test")

  #tooltip with variables
  opts$chart$tooltip <- "Pepite have {total} {thinks}"
  l <- hgchmagic_prep(data, opts)
  expect_equal(l$tooltip, "Pepite have {point.y:,.2f} {point.name}")


})
