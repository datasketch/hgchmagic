test_that("hgch area Cat-Dat-Num", {


  data <- sample_data("Cat-Dat-Num", n = 30, rep = FALSE)

  hgch_area_CatDatNum(data, format_sample_num = "1,231.1")
  hgch_area_CatDatNum(data, format_sample_num = "1,231.",
                      connect_lines_nulls = TRUE)
  hgch_area_CatDatNum(data, format_sample_num = "1,231.1", dataLabels_show = T)
  hgch_area_CatDatNum(data, percentage = T, dataLabels_show = T, suffix = "%")
  hgch_area_CatDatNum(data, percentage = T, dataLabels_show = T, format_sample_num = "1,231.1",
                      tooltip = paste0("<p style='color:red;'>", names(data)[1],"</p>Percentage: {%}% <br/> Sum: {", names(data)[2],"}"))


  data <- sample_data("Cat-Dat-Num-Cat-Cat-Num", n = 30, rep = FALSE)
  hgch_area_CatDatNum(data, dataLabels_show = T,
                      tooltip = paste0("<p style='color:red;'>", names(data)[1],
                                       "</p>Percentage: {%}%
                                   <br/> Sum ", names(data)[5],": {", names(data)[5],"}"))


})



test_that("hgch line CatDatNum", {


  data <- sample_data("Cat-Dat-Num", n = 300, rep = FALSE)
  hgch_line_CatDatNum(data)
  hgch_line_CatDatNum(data, date_intervals = "week")
  data <- sample_data("Cat-Dat-Num", n = 30, rep = FALSE)
  hgch_line_CatDatNum(data, format_sample_num = "1,231.1",
                      dataLabels_show = T)
  hgch_line_CatDatNum(data, connect_lines_nulls = TRUE)
  hgch_line_CatDatNum(data, connect_lines_nulls = TRUE, dataLabels_show = T)
  data <- data.frame(
    cat = c("one", "two", "one", "two"),
    fecha = c("2020/10/03","2020/10/04", "2020/10/04", "2020/10/03"),
    vals = 1:4
    )
  data$my_label <- "This is a generic label"
  hgch_line_CatDatNum(data)
  hgch_line_CatDatNum(data, tooltip = "{my_label}",
                      format_sample_dat = "20 enero 2019",
                      hor_title = " ")


})

test_that("hgch scatter CatDatNum", {

  data <- sample_data("Cat-Dat-Num", n = 30, rep = FALSE)

  opts <- dsvizopts::dsviz_defaults()
  hgch_scatter_CatDatNum(data)
  hgch_scatter_CatDatNum(data,
                         format_sample_num = "1,231.",
                         dataLabels_show = T,
                         color_by = names(data)[2],
                         palette_type = "sequential")

  data <- sample_data("Cat-Dat-Num-Cat-Cat", n = 30, rep = FALSE)
  hgch_scatter_CatDatNum(data, tooltip = paste0(names(data)[4], ": {", names(data)[4], "}"))


})
