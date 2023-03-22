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
  h_treemap <- hgch_treemap(data, var_cat = "cut", var_num = "price", opts =  ops)

  expect_equal(h_treemap$x$hc_opts$series[[1]]$type, "treemap")


  data <- ggplot2::diamonds |> select(cut, everything())
  h_treemap_cat <- hgch_treemap_Cat(data)
  expect_equal(h_treemap_cat$x$hc_opts$series[[1]]$type, "treemap")

  h_treemap_cat_num <- hgch_treemap_CatNum(data)
  expect_equal(h_treemap_cat_num$x$hc_opts$series[[1]]$type, "treemap")

})
