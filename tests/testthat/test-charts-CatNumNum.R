test_that("hgch scatter Cat-Num-Num", {

  data <- sample_data("Cat-Num-Num")
  hgch_scatter_CatNumNum(data)
  hgch_scatter_CatNumNum(data, color_by = names(data)[1])

})


test_that("hgch dumbbell Cat-Num-Num", {

  data <- sample_data("Cat-Num-Num")
  hgch_dumbbell_CatNumNum(data)
  hgch_dumbbell_CatNumNum(data, color_by = names(data)[1])

})
