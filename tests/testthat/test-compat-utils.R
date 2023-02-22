test_that("Internal functions", {
  string_result <- frtype_viz(var_cat = "a")
  string_expect <- "Cat"
  expect_equal(string_result, string_expect)

  string_result <- frtype_viz(var_cat = "a", var_num = "y")
  string_expect <- "CatNum"
  expect_equal(string_result, string_expect)

  string_result <- frtype_viz(var_cat = "a", var_num = "y", var_date = "date")
  string_expect <- "CatDatNum"
  expect_equal(string_result, string_expect)


  string_result <- frtype_viz(var_cat = c("a", "b"), var_num = "y")
  string_expect <- "CatCatNum"
  expect_equal(string_result, string_expect)

  string_result <- frtype_viz(var_cat = c("a", "b"), var_num = c("y", "x"))
  string_expect <- "CatCatNumNum"
  expect_equal(string_result, string_expect)

  string_result <- frtype_viz(var_cat = c("a"), var_num = c("y", "x"))
  string_expect <- "CatNumNum"
  expect_equal(string_result, string_expect)


  string_result <- frtype_viz(var_num = c("y", "x"))
  string_expect <- "NumNum"
  expect_equal(string_result, string_expect)

})
