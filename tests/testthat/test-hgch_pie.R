test_that("Pie", {
  data <- ggplot2::diamonds
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "sum",
                                       group_var = "cut",
                                       to_agg = "price")
  hgch_pie(data = data, var_cat = "cut", var_num = "price")

  data1 <- data.frame(name = c("A", "B", "C"), y = c(10, 20, 30))
  data2 <- data.frame(name = c("D", "E", "F"), y = c(15, 25, 35))
  data3 <- data.frame(name = c("G", "H", "I"), y = c(18, 22, 28))

  # Create Highcharts pie charts
  hgch_pie_Cat(data1 )
  hgch_pie_CatNum(data2)
  hgch_pie(data3, var_cat = "name", var_num = "y" )


  data <- ggplot2::diamonds
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "sum",
                                       group_var = "cut",
                                       to_agg = "price")

  ops <- list(title = "title",
              subtitle = "subtitle",
              caption = "caption",
              hor_title = "Categorias",
              ver_title = "Numeros",
              bar_orientation = "hor")
  hgch_pie(data, var_cat = "cut", var_num = "price", opts =  ops)
  hgch_pie(data,
           var_cat = "cut",
           var_num = "price",
           palette_colors = c("#ffa92a"))


})