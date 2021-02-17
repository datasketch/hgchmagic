test_that("hgch bar Cat", {


  data <- sample_data("Cat", n = 30, rep = FALSE)

  hgch_bar_Cat(data, format_sample_num = "1,231.1")
  hgch_bar_Cat(data, format_sample_num = "1,231.1", dataLabels_show = T)
  hgch_bar_Cat(data, percentage = T, dataLabels_show = T, suffix = "%")
  hgch_bar_Cat(data, percentage = T, dataLabels_show = T,
                  tooltip = paste0("<b>{",names(data)[1],"}</b><br/>Percentage: {%}% <br/> Total: {Count}"))


  data <- sample_data("Cat-Cat-Cat", n = 30, rep = FALSE)
  hgch_bar_Cat(data, percentage = T, dataLabels_show = T,
                  tooltip = paste0("<b>{", names(data)[1],
                                   "}</b><br/>Percentage: {%}%
                                   <br/> other cats: {", names(data)[3],"}"))


})


test_that("hgch pie Cat", {


  data <- sample_data("Cat", n = 30, rep = FALSE)
  hgch_pie_Cat(data, format_sample_num = "1,231.1")
  data$my_label <- "My label"
  hgch_pie_Cat(data,
                  format_sample_num = "1,231.",
                  dataLabels_show = T,
                  color_by = names(data)[1],
                  legend_show = FALSE,
                  percentage = TRUE,
                  suffix = "%",
                  tooltip = paste0("<span style='color:#feafea;'>", names(data)[1],"</span><br/>
                                   Percentage: {%}% <br/> Total: {", names(data)[2],"}"))

})


test_that("hgch donut Cat", {


  data <- sample_data("Cat", n = 30, rep = FALSE)
  hgch_donut_Cat(data, format_sample_num = "1,231.1")
  hgch_donut_Cat(data,
                    color_by = names(data)[1],
                    palette_type = "divergent"
  )

})


test_that("hgch bubbles Cat", {


  data <- sample_data("Cat", n = 30, rep = FALSE)
  hgch_bubbles_Cat(data, format_sample_num = "1,231.1",
                      dataLabels_show = T)
  data <- sample_data("Cat", n = 30, rep = FALSE)
  data$my_label <- "This is a generic label"
  hgch_bubbles_Cat(data,
                      tooltip = "{my_label}")

})

test_that("hgch treemap Cat", {

  library(homodatum)
  data <- sample_data("Cat", n = 30, rep = FALSE)

  opts <- dsvizopts::dsviz_defaults()

  hgch_treemap_Cat(data,
                      format_sample_num = "1,231.",
                      dataLabels_show = T,
                      color_by = names(data)[1],
                      palette_type = "sequential")

  data <- sample_data("Cat", n = 30, nlevels = 20, rep = FALSE)
  hgch_treemap_Cat(data, treemap_layout = "squarified")

  hgch_treemap_Cat(data, treemap_layout = "stripes")
  hgch_treemap_Cat(data, treemap_layout = "stripes", treemap_direction = "horizontal")

  hgch_treemap_Cat(data, treemap_layout = "strip")
  hgch_treemap_Cat(data, treemap_layout = "sliceAndDice")

})
