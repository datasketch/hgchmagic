test_that("Donut", {
  data <- ggplot2::diamonds
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "sum",
                                       group_var = "cut",
                                       to_agg = "price")
  hgch_donut(data = data, var_cat = "cut", var_num = "price")

  data1 <- data.frame(name = c("A", "B", "C"), y = c(10, 20, 30))
  data2 <- data.frame(name = c("D", "E", "F"), y = c(15, 25, 35))
  data3 <- data.frame(name = c("G", "H", "I"), y = c(18, 22, 28))

  # Create Highcharts donut charts
  hgch_donut_Cat(data1 )
  hgch_donut_CatNum(data2)
  hgch_donut(data3, var_cat = "name", var_num = "y" )

  # test theme (titles)

  test_theme <- list(
    theme = list(
      title_align = "center",
      title_family = "Roboto",
      title_size = 15,
      title_color = "#3b83b8",
      title_weight = 700
    )
  )

  data <- data.frame(name = c("G", "H", "I"), y = c(18, 22, 28))
  hgch_donut(data, var_cat = "name", var_num = "y",
             title = "Theme test",
             opts = test_theme )

   hgch_donut_CatNum(data)


})
