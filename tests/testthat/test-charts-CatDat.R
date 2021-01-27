test_that("hgch area Cat-Dat", {


  data <- sample_data("Cat-Dat", n = 30, rep = FALSE)

  hgch_area_CatDat(data, format_sample_num = "1,231.1")
  hgch_area_CatDat(data, format_sample_num = "1,231.",
                      connect_lines_nulls = TRUE)
  hgch_area_CatDat(data, format_sample_num = "1,231.1", dataLabels_show = T)
  hgch_area_CatDat(data, percentage = T, dataLabels_show = T, suffix = "%")
  hgch_area_CatDat(data, percentage = T, dataLabels_show = T, format_sample_num = "1,231.1",
                      tooltip = paste0("<p style='color:red;'>", names(data)[1],"</p>Percentage: {%}% <br/> Sum: {", names(data)[2],"}"))


  data <- sample_data("Cat-Dat-Cat-Cat-Num", n = 30, rep = FALSE)
  hgch_area_CatDat(data, dataLabels_show = T,
                      tooltip = paste0("<p style='color:red;'>", names(data)[1],
                                       "</p>Percentage: {%}%
                                   <br/> Sum ", names(data)[5],": {", names(data)[5],"}"))


})



test_that("hgch line CatDat", {


  data <- data.frame(
    cat = c("one", "one", "one", "two", "two", "two", "one", "two", "one", "two", "one", "two", "one", "two",  "one", "two"),
    fecha = c("2020/10/03", "2020/10/03", "2020/10/03", "2020/10/03", "2020/10/03","2020/10/04", "2020/10/04", "2020/10/03", "2020/10/04", "2020/10/03", "2020/10/04", "2020/10/03", "2020/10/04", "2020/10/03", "2020/10/04", "2020/10/03"),
    vals = 1:16
  )
  hgch_line_CatDat(data)
  hgch_line_CatDat(data, dataLabels_show = TRUE)
  hgch_line_CatDat(data, format_sample_num = "1,231.1",
                      dataLabels_show = T)

  data$my_label <- "This is a generic label"
  hgch_line_CatDat(data)
  hgch_line_CatDat(data, tooltip = "{my_label}",
                      format_sample_dat = "20 enero 2019",
                      hor_title = " ")


})

test_that("hgch scatter CatDat", {

  data <- sample_data("Cat-Dat", n = 100, rep = TRUE)
  hgch_scatter_CatDat(data)
  hgch_scatter_CatDat(data,
                         format_sample_num = "1,231.",
                         dataLabels_show = T,
                         color_by = names(data)[2],
                         palette_type = "sequential")

  data <- sample_data("Cat-Dat-Cat-Cat", n = 30, rep = T)
  hgch_scatter_CatDat(data, tooltip = paste0(names(data)[4], ": {", names(data)[4], "}"))


})
