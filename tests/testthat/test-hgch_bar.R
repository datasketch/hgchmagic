test_that("Bar", {
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
  hgch_bar(data, var_cat = "cut", var_num = "price", opts =  ops)

  data <- ggplot2::diamonds
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "sum",
                                       group_var = c("cut", "color"),
                                       to_agg = "price")

  ops <- list(titles = list(title = "title", subtitle = "subtitle", caption = "caption"))
  hgch_bar(data, var_cat = c("cut", "color"), var_num = "price", opts =  ops)




})
