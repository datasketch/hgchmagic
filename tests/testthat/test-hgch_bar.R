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
  hgch_bar(data, var_cat = "cut", var_num = "price", palette_colors = c("#ffa92a"),
           order = c("Very Good"))


  data <- ggplot2::diamonds |> select(cut, everything())
  hgch_bar_Cat(data, opts = ops, palette_colors = "#ffa92a")

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


  # opts from theme (canvas)
  test_theme <- list(
    theme = list(
    background_color = "#2f2f2f",
    plot_margin_bottom = 30,
    plot_margin_left = 30,
    plot_margin_right = 30,
    plot_margin_top = 30,
    plot_background_color = "#f2f2f2",
    plot_border_color = "#ff2c2f",
    plot_border_size = 3,
    text_family = "ubuntu",
    text_size = 15
    )
  )
  data <- ggplot2::diamonds |> select(cut, everything())
  hgch_bar_Cat(data, opts = test_theme)


  data <- data.frame(var_cat = c("a",  "b", "d"),
                     var_num_one = runif(3),
                     var_num_two = runif(3, 100, 1000))
  hgch_bar(data, var_cat = "var_cat", var_num = c("var_num_one", "var_num_two"))
  hgch_bar_CatNumNum(data)
  data <- tibble(var_cat = c("arroz", "SAL", "agua"),
                     `var num one` = runif(3),
                     `var num two` = runif(3, 100, 1000))
  hgch_bar(data, var_cat = "var_cat", var_num = c("var num one", "var num two"))



})
