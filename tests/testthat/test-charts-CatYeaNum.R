test_that("hgch area CatYeaNum", {


  data <- data.frame(continente = c(rep("Asia", 7), rep("Africa", 7), rep("Europe", 7), rep("America", 7), rep("Oceania", 7)),
                     year = c(2001:2007, 2001:2007, 2001:2007, 2001:2007, 2001:2007),
                     info_value = c(502, 635, 809, 947, 1402, 3634, 5268, 106, 107, 111, 133, 221, 767, 1766,
                                    163, 203, 276, 408, 547, 729, 628, 18, 31, 54, 156, 339, 818, 1201,
                                    2, 2, 2, 6, 13, 30, 46))

  hgch_area_CatYeaNum(data, format_sample_num = "1,231.1")
  hgch_area_CatYeaNum(data, format_sample_num = "1,231.1", dataLabels_show = T)
  hgch_area_CatYeaNum(data, percentage = T, dataLabels_show = T, suffix = "%")
  hgch_area_CatYeaNum(data, percentage = T, dataLabels_show = T,
                     tooltip = paste0("<p style='color:red;'>", names(data)[1],"</p>Percentage: {%}% <br/> Sum: {", names(data)[2],"}"))


  data <- sample_data("Cat-Yea-Num-Cat-Yea-Num", n = 30, rep = FALSE)
  hgch_area_CatYeaNum(data, dataLabels_show = T,
                     tooltip = paste0("<p style='color:red;'>", names(data)[1],
                                      "</p>Percentage: {%}%
                                   <br/> Sum ", names(data)[5],": {", names(data)[5],"}"))


})


test_that("hgch bar CatYeaNum", {


  data <- sample_data("Cat-Yea-Num", n = 30, rep = FALSE)

  hgch_bar_CatYeaNum(data, format_sample_num = "1,231.1")
  hgch_bar_CatYeaNum(data, format_sample_num = "1,231.", graph_type = "stacked")
  hgch_bar_CatYeaNum(data, format_sample_num = "1,231.", graph_type = "stacked", percentage = TRUE)
  hgch_bar_CatYeaNum(data, format_sample_num = "1,231.",
                     graph_type = "stacked", percentage = TRUE,
                     tooltip = paste0(names(data)[1], ": {", names(data)[1], "}<br/>
                                      ", names(data)[2], ": {", names(data)[2], "}<br/>
                                      ", names(data)[3], ": {", names(data)[3], "}<br/>
                                      Percentage: {%}%" ))
  hgch_bar_CatYeaNum(data, format_sample_num = "1,231.1", dataLabels_show = T)
  hgch_bar_CatYeaNum(data, percentage = T, dataLabels_show = T, suffix = "%")
  hgch_bar_CatYeaNum(data, percentage = T, dataLabels_show = T,
                     tooltip = paste0("<p style='color:red;'>", names(data)[1],"</p>Percentage: {%}% <br/> Sum: {", names(data)[2],"}"))


  data <- data.frame(continente = c(rep("Asia", 7), rep("Africa", 7), rep("Europe", 7), rep("America", 7), rep("Oceania", 7)),
                     year = c(2001:2007, 2001:2007, 2001:2007, 2001:2007, 2001:2007),
                     info_value = c(502, 635, 809, 947, 1402, 3634, 5268, 106, 107, 111, 133, 221, 767, 1766,
                                    163, 203, 276, 408, 547, 729, 628, 18, 31, 54, 156, 339, 818, 1201,
                                    2, 2, 2, 6, 13, 30, 46))
  data$cat_add <- "my label"
  data$year_add <- c(1990:1996, 1990:1996, 1990:1996, 1990:1996, 1990:1996)
  hgch_bar_CatYeaNum(data, dataLabels_show = T,
                     tooltip = paste0("<b>", names(data)[1],":</b> {", names(data)[1],"}<br/>Percentage: {%}% <br/> ", names(data)[5],": {", names(data)[5],"}"))


})



test_that("hgch bubbles CatYeaNum", {


  data <- sample_data("Cat-Yea-Num", n = 30, rep = FALSE)
  hgch_bubbles_CatYeaNum(data, format_sample_num = "1,231.1",
                         dataLabels_show = T)
  data <- sample_data("Cat-Yea-Num", n = 30, rep = FALSE)
  data$my_label <- "This is a generic label"
  hgch_bubbles_CatYeaNum(data,
                         tooltip = "{my_label}")

})

test_that("hgch treemap CatYeaNum", {

  data <- sample_data("Cat-Yea-Num", n = 30, rep = FALSE)

  opts <- dsvizopts::dsviz_defaults()

  hgch_treemap_CatYeaNum(data,
                         format_sample_num = "1,231.",
                         dataLabels_show = T,
                         color_by = names(data)[2],
                         palette_type = "sequential")

  data <- sample_data("Cat-Yea-Num-Cat-Cat", n = 30, nlevels = 20, rep = FALSE)
  hgch_treemap_CatYeaNum(data, treemap_layout = "squarified", dataLabels_show = T)

  hgch_treemap_CatYeaNum(data, treemap_layout = "stripes")
  hgch_treemap_CatYeaNum(data, treemap_layout = "stripes", treemap_direction = "horizontal")

  hgch_treemap_CatYeaNum(data, treemap_layout = "strip")
  hgch_treemap_CatYeaNum(data, treemap_layout = "sliceAndDice")

})


test_that("hgch line CatYeaNum", {


  data <- data.frame(continente = c(rep("Asia", 7), rep("Africa", 7), rep("Europe", 7), rep("America", 7), rep("Oceania", 7)),
                     year = c(2001:2007, 2001:2007, 2001:2007, 2001:2007, 2001:2007),
                     info_value = c(502, 635, 809, 947, 1402, 3634, 5268, 106, 107, 111, 133, 221, 767, 1766,
                                    163, 203, 276, 408, 547, 729, 628, 18, 31, 54, 156, 339, 818, 1201,
                                    2, 2, 2, 6, 13, 30, 46))

  hgch_line_CatYeaNum(data, format_sample_num = "1,231.1")
  hgch_line_CatYeaNum(data, format_sample_num = "1,231.1", dataLabels_show = T)
  hgch_line_CatYeaNum(data, percentage = T, dataLabels_show = T, suffix = "%")
  hgch_line_CatYeaNum(data, percentage = T, dataLabels_show = T,
                      tooltip = paste0("<p style='color:red;'>", names(data)[1],"</p>Percentage: {%}% <br/> Sum: {", names(data)[2],"}"))


  data <- sample_data("Cat-Yea-Num-Cat-Yea-Num", n = 30, rep = FALSE)
  hgch_line_CatYeaNum(data, dataLabels_show = T,
                      tooltip = paste0("<p style='color:red;'>", names(data)[1],
                                       "</p>Percentage: {%}%
                                   <br/> Sum ", names(data)[5],": {", names(data)[5],"}"))


})
