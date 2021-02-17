test_that("hgch bar CatNum", {


  data <- sample_data("Cat-Num", n = 30, rep = FALSE)

  hgch_bar_CatNum(data, format_sample_num = "1,231.1")
  hgch_bar_CatNum(data, format_sample_num = "1,231.1", dataLabels_show = T)
  hgch_bar_CatNum(data, percentage = T, dataLabels_show = T, suffix = "%")
  hgch_bar_CatNum(data, percentage = T, dataLabels_show = T,
                  tooltip = paste0("<p style='color:red;'>", names(data)[1],"</p>Percentage: {%}% <br/> Sum: {", names(data)[2],"}"))


  data <- sample_data("Cat-Num-Cat-Cat-Num", n = 30, rep = FALSE)
  hgch_bar_CatNum(data, percentage = T, dataLabels_show = T,
                  tooltip = paste0("<p style='color:red;'>", names(data)[1],
                                   "</p>Percentage: {%}%
                                   <br/> Sum ", names(data)[5],": {", names(data)[5],"}"))


})


test_that("hgch pie CatNum", {


  data <- sample_data("Cat-Num", n = 30, rep = FALSE)
  hgch_pie_CatNum(data, format_sample_num = "1,231.1")
  hgch_pie_CatNum(data,
                  format_sample_num = "1,231.",
                  dataLabels_show = T,
                  color_by = names(data)[1],
                  legend_show = FALSE,
                  percentage = TRUE,
                  suffix = "%",
                  tooltip = paste0("<span style='color:#feafea;'>", names(data)[1],"</span><br/>
                                   Percentage: {%}% <br/> Total: {", names(data)[2],"}"))

})


test_that("hgch donut CatNum", {


  data <- sample_data("Cat-Num", n = 30, rep = FALSE)
  hgch_donut_CatNum(data, format_sample_num = "1,231.1")
  hgch_donut_CatNum(data,
                    color_by = names(data)[1],
                    palette_type = "divergent"
                    )

})


test_that("hgch bubbles CatNum", {


  data <- sample_data("Cat-Num", n = 30, rep = FALSE)
  hgch_bubbles_CatNum(data, format_sample_num = "1,231.1",
                      dataLabels_show = T)
  data <- sample_data("Cat-Num", n = 30, rep = FALSE)
  data$my_label <- "This is a generic label"
  hgch_bubbles_CatNum(data,
                      tooltip = "{my_label}")

})

test_that("hgch treemap CatNum", {

  library(homodatum)
  data <- sample_data("Cat-Num", n = 30, rep = FALSE)

  opts <- dsvizopts::dsviz_defaults()

  hgch_treemap_CatNum(data,
                      format_sample_num = "1,231.",
                      dataLabels_show = T,
                      color_by = names(data)[2],
                      palette_type = "sequential")

  data <- sample_data("Cat-Num", n = 30, nlevels = 20, rep = FALSE)
  hgch_treemap_CatNum(data, treemap_layout = "squarified")

  hgch_treemap_CatNum(data, treemap_layout = "stripes")
  hgch_treemap_CatNum(data, treemap_layout = "stripes", treemap_direction = "horizontal")

  hgch_treemap_CatNum(data, treemap_layout = "strip")
  hgch_treemap_CatNum(data, treemap_layout = "sliceAndDice")

})
