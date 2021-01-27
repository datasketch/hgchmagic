test_that("hgch scatter NumNum", {


  data <- sample_data("Num-Num", n = 30, rep = FALSE)

  hgch_scatter_NumNum(data)
  hgch_scatter_NumNum(data, scatter_format_num_sample_x = "1,231.1", scatter_regression = T)
  hgch_scatter_NumNum(data, scatter_format_num_sample_x= "1,231.", scatter_format_num_sample_y = "1,231.1", scatter_prefix_y = "$")
  hgch_scatter_NumNum(data,
                  tooltip = paste0("<b>{", names(data)[1],"} - {",names(data)[2] ,"}</b>"))


  data <- sample_data("Num-Num-Cat-Num-Num", n = 30, rep = FALSE)
  hgch_scatter_NumNum(data,
                  tooltip = paste0("<p style='color:red;'>", names(data)[1], "<br/> Sum ", names(data)[5],": {", names(data)[5],"}</p>"))


})
