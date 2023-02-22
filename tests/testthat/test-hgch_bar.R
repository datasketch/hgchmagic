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
  hgch_bar(data, var_cat = "cut", var_num = "price", palette_colors = "#ffa92a",
           order = c("Very Good"))


  data <- ggplot2::diamonds |> select(cut, everything())
  hgch_bar_Cat(data, opts = ops)

  data <- ggplot2::diamonds |> select(cut, price, everything())
   hgch_bar_CatNum(data, opts = ops)
  hgch_bar_CatNum(data, opts = ops, collapse_rows = TRUE)


  data <- ggplot2::diamonds
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "sum",
                                       group_var = c("cut", "color"),
                                       to_agg = "price")

  ops <- list(titles = list(title = "title", subtitle = "subtitle", caption = "caption"))
  hgch_bar(data, var_cat = c("cut", "color"), var_num = "price", opts =  ops)
  hgch_bar(data, var_cat = c("cut", "color"), var_num = "price", order = c("H", "F"))
  hgch_bar(data, var_cat = c("cut", "color"), var_num = "price", order_legend = c("Ideal", "Premium"))
  hgch_bar(data, var_cat = c("cut", "color"), var_num = "price",
           order_legend = c("Ideal", "Premium"), order = c("H", "F"))

  data <- ggplot2::diamonds |> select(cut, color, price, everything())
  hgch_bar_CatCat(data, opts = ops)
  hgch_bar_CatCatNum(data, opts = ops)


})
