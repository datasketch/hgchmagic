test_that("hgch bar Yea", {


  data <- sample_data("Yea", n = 30, rep = TRUE)

  hgch_bar_Yea(data, format_sample_num = "1,231.1")
  hgch_bar_Yea(data, format_sample_num = "1,231.1", dataLabels_show = T)
  hgch_bar_Yea(data, percentage = T, dataLabels_show = T, suffix = "%")
  hgch_bar_Yea(data, percentage = T, dataLabels_show = T,
                  tooltip = paste0("<b>{",names(data)[1],"}</b><br/>Percentage: {%}% <br/> Total: {Count}"))


  data <- sample_data("Yea-Yea-Cat", n = 30, rep = TRUE)
  hgch_bar_Yea(data, percentage = T, dataLabels_show = T,
                  tooltip = paste0("<b>{", names(data)[1],
                                   "}</b><br/>Percentage: {%}%
                                   <br/> Add categories: {", names(data)[3],"}"))


})


test_that("hgch pie Yea", {


  data <- sample_data("Yea", n = 30, rep = TRUE)
  hgch_pie_Yea(data, format_sample_num = "1,231.1")
  data$my_label <- "My label"
  hgch_pie_Yea(data,
                  format_sample_num = "1,231.",
                  dataLabels_show = T,
                  color_by = names(data)[1],
                  legend_show = FALSE,
                  percentage = TRUE,
                  suffix = "%",
                  tooltip = paste0("<span style='color:#feafea;'>", names(data)[1],"</span><br/>
                                   Percentage: {%}% <br/> Total: {", names(data)[2],"}"))

})


test_that("hgch donut Yea", {


  data <- sample_data("Yea", n = 30, rep = FALSE)
  hgch_donut_Yea(data, format_sample_num = "1,231.1")
  hgch_donut_Yea(data,
                    color_by = names(data)[1],
                    palette_type = "divergent"
  )

})


test_that("hgch bubbles Yea", {


  data <- sample_data("Yea", n = 30, rep = FALSE)
  hgch_bubbles_Yea(data, format_sample_num = "1,231.1",
                      dataLabels_show = T)
  data <- sample_data("Yea", n = 30, rep = FALSE)
  data$my_label <- "This is a generic label"
  hgch_bubbles_Yea(data,
                      tooltip = "{my_label}")

})

test_that("hgch treemap Yea", {


  data <- sample_data("Yea", n = 30, rep = FALSE)
  hgch_treemap_Yea(data,
                      format_sample_num = "1,231.",
                      dataLabels_show = T,
                      color_by = names(data)[1],
                      palette_type = "sequential")

  data <- sample_data("Yea", n = 30, nlevels = 20, rep = FALSE)
  hgch_treemap_Yea(data, treemap_layout = "squarified")

  hgch_treemap_Yea(data, treemap_layout = "stripes")
  hgch_treemap_Yea(data, treemap_layout = "stripes", treemap_direction = "horizontal")

  hgch_treemap_Yea(data, treemap_layout = "strip")
  hgch_treemap_Yea(data, treemap_layout = "sliceAndDice")

})
