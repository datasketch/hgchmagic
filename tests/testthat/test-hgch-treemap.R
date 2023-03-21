test_that("Treemap", {
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
  hgch_treemap(data, var_cat = "cut", var_num = "price", opts =  ops)

  data <- ggplot2::diamonds |> select(cut, everything())
  hgch_treemap_Cat(data)

  hgch_treemap_CatNum(data)

  data <- ggplot2::diamonds
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "sum",
                                       group_var = c("cut", "clarity"),
                                       to_agg = "price")

  hgch_treemap(data, var_cat = c("cut", "clarity"), var_num = "price")
  hgch_treemap_CatCatNum(data)

  data <- ggplot2::diamonds |> select(cut, clarity)
  hgch_treemap_CatCat(data)

})
