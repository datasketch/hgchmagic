test_that("Bar", {


  ### hgch_bar()
  data <- ggplot2::diamonds
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "sum",
                                       group_var = "cut",
                                       to_agg = "price")
  h <- hgch_bar(data, var_cat = "cut", var_num = "price")

  expect_equal(h_bar_Cat$x$hc_opts$chart$type, "column")
  expect_equal(length(h$x$hc_opts$series[[1]]$data), 5)
  for (i in 1:length(h$x$hc_opts$series[[1]]$data)) {
    expect_equal(h$x$hc_opts$series[[1]]$data[[i]]$name, as.character(data$cut[i]))
  }



  ## Ops list
  ops <- list(title = "This is a title",
              subtitle = "This is a subtitle",
              caption = "A caption? Yes, this is a caption",
              hor_title = "Categories",
              ver_title = "Numbers",
              bar_orientation = "hor")

  h <- hgch_bar(data, var_cat = "cut", var_num = "price", opts =  ops)
  expect_equal(h$x$hc_opts$subtitle$text, ops$subtitle)

  # Color setting
  h <- hgch_bar(data, var_cat = "cut", var_num = "price", palette_colors = c("#ffa92a"),
                order = c("Very Good"))
  expect_equal(h$x$theme$colors, "#ffa92a")
  expect_error(hgch_bar(data, opts = ops, palette_colors = "#ffa92a"))


  ### hgch_bar_Cat()
  data <- ggplot2::diamonds |> dplyr::select(cut, dplyr::everything())
  hgch_bar_Cat(data, collapse_rows = TRUE, tooltip_template = "Color: {color}")



  h_bar <- hgch_bar(data, var_cat = "cut", opts = ops, palette_colors = "#ffa92a")
  expect_null(h_bar$x$hc_opts$xAxis$type)

  h_bar_Cat <- hgch_bar_Cat(data, opts = ops, palette_colors = "#ffa92a")
  expect_equal(h_bar_Cat$x$hc_opts$xAxis$type, "category")




  # data <- ggplot2::diamonds |> dplyr::select(cut, price, dplyr::everything())
  # h_bar_CatNum <- hgch_bar_CatNum(data, opts = ops)
  # hgch_bar_CatNum(data, opts = ops, collapse_rows = TRUE)
  #
  #
  # data <- ggplot2::diamonds
  # data <- dsdataprep::aggregation_data(data = data,
  #                                      agg = "sum",
  #                                      group_var = c("cut", "color"),
  #                                      to_agg = "price")
  #
  # ops <- list(titles = list(title = "title", subtitle = "subtitle", caption = "caption"))
  # hgch_bar(data, var_cat = c("cut", "color"), var_num = "price", opts =  ops)
  # hgch_bar(data, var_cat = c("cut", "color"), var_num = "price", order = c("H", "F"))
  # hgch_bar(data, var_cat = c("cut", "color"), var_num = "price", order_legend = c("Ideal", "Premium"))
  # hgch_bar(data, var_cat = c("cut", "color"), var_num = "price",
  #          order_legend = c("Ideal", "Premium"), order = c("H", "F"))
  #
  # data <- ggplot2::diamonds |> select(cut, color, price, everything())
  # hgch_bar_CatCat(data, opts = ops)
  # hgch_bar_CatCatNum(data, opts = ops)
  #
  #
  # # opts from theme (canvas)
  # test_theme <- list(
  #   theme = list(
  #   background_color = "#2f2f2f",
  #   plot_margin_bottom = 30,
  #   plot_margin_left = 30,
  #   plot_margin_right = 30,
  #   plot_margin_top = 30,
  #   plot_background_color = "#f2f2f2",
  #   plot_border_color = "#ff2c2f",
  #   plot_border_size = 3,
  #   text_family = "ubuntu",
  #   text_size = 15
  #   )
  # )
  # data <- ggplot2::diamonds |> select(cut, everything())
  # hgch_bar_Cat(data, opts = test_theme)


  data <- data.frame(var_cat = c("a",  "b", "d"),
                     var_num_one = runif(3),
                     var_num_two = runif(3, 100, 1000))
  hgch_bar(data, var_cat = "var_cat", var_num = c("var_num_one", "var_num_two"))
  hgch_bar_CatNumNum(data)
  data <- tibble(var_cat = c("arroz", "SAL", "agua"),
                     `var num one` = c(runif(2), NA),
                     `var num two` = runif(3, 100, 1000))
  hgch_bar(data, var_cat = "var_cat", var_num = c("var num one", "var num two"))
  data <- tibble(var_cat = c("arroz"),
                 `var num one` = 10,
                 `var num two` = 3)
  hgch_bar(data, var_cat = "var_cat",
           var_num = c("var num one", "var num two"), bar_orientation = "ver")



})
