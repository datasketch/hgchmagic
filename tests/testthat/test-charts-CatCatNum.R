test_that("hgch bar CatCatNum", {


  data <- sample_data("Cat-Cat-Num", n = 30, rep = FALSE)

  hgch_bar_CatCatNum(data, format_sample_num = "1,231.1")
  hgch_bar_CatCatNum(data, format_sample_num = "1,231.1", dataLabels_show = T)
  hgch_bar_CatCatNum(data, percentage = T, dataLabels_show = T, suffix = "%")
  hgch_bar_CatCatNum(data, percentage = T, dataLabels_show = T,
                  tooltip = paste0("<p style='color:red;'>", names(data)[1],"</p>Percentage: {%}% <br/> Sum: {", names(data)[2],"}"))


  data <- sample_data("Cat-Cat-Num-Cat-Cat-Num", n = 30, rep = FALSE)
  hgch_bar_CatCatNum(data, dataLabels_show = T,
                  tooltip = paste0("<p style='color:red;'>", names(data)[1],
                                   "</p>Percentage: {%}%
                                   <br/> Sum ", names(data)[5],": {", names(data)[5],"}"))


})



test_that("hgch bubbles CatCatNum", {


  data <- sample_data("Cat-Cat-Num", n = 30, rep = FALSE)
  hgch_bubbles_CatCatNum(data, format_sample_num = "1,231.1",
                      dataLabels_show = T)
  data <- sample_data("Cat-Cat-Num", n = 30, rep = FALSE)
  data$my_label <- "This is a generic label"
  hgch_bubbles_CatCatNum(data,
                      tooltip = "{my_label}")

})

test_that("hgch treemap CatCatNum", {

  data <- sample_data("Cat-Cat-Num", n = 30, rep = FALSE)

  opts <- dsvizopts::dsviz_defaults()

  hgch_treemap_CatCatNum(data,
                      format_sample_num = "1,231.",
                      dataLabels_show = T,
                      color_by = names(data)[2],
                      palette_type = "sequential")

  data <- sample_data("Cat-Cat-Num-Cat-Cat", n = 30, nlevels = 20, rep = FALSE)
  hgch_treemap_CatCatNum(data, treemap_layout = "squarified", dataLabels_show = T)

  hgch_treemap_CatCatNum(data, treemap_layout = "stripes")
  hgch_treemap_CatCatNum(data, treemap_layout = "stripes", treemap_direction = "horizontal")

  hgch_treemap_CatCatNum(data, treemap_layout = "strip")
  hgch_treemap_CatCatNum(data, treemap_layout = "sliceAndDice")

})
