test_that("Sankey", {
  data <- dplyr::starwars |>
    tidyr::drop_na(sex, gender, hair_color, skin_color)
  h_sankey <- hgch_sankey(data, var_cat = c("species", "sex", "gender"))
  expect_equal(h_sankey$x$hc_opts$chart$type, "sankey")

  h <- hgch_sankey(data, var_cat = c( "sex", "gender", "hair_color", "skin_color"))
  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))

  data_catcat <- data |> dplyr::select(sex, gender)
  h <- hgch_sankey_CatCat(data_catcat)
  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))

  data_catcatcat <- data |> dplyr::select(sex, gender, species)
  h <- hgch_sankey_CatCatCat(data_catcatcat)
  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))

  data_cat4 <- data |> dplyr::select(hair_color, sex, gender, species)
  h <- hgch_sankey_CatCatCatCat(data_cat4)

  test_theme <- list(
    theme = list(
      palette_colors = "#ffa92a",
      caption_align = "right",
      caption_family = "Roboto",
      caption_size = 15,
      caption_color = "#3b83b8",
      caption_weight = 700
    )
  )
  data <- dplyr::starwars
  data <- data |> dplyr::select(hair_color, sex) |> tidyr::drop_na()
  h <- hgch_sankey_CatCat(data, opts = test_theme, caption = "theme caption")
  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))

  data <- ggplot2::diamonds
  data <- data |> dplyr::group_by(cut, clarity) |> dplyr::summarise(total = sum(z, na.rm = T))
  h <- hgch_sankey(data, var_cat = c("cut", "clarity"), var_num = "total")
  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))

  h <- hgch_sankey_CatCatNum(data)
  expect_true(all(class(h) %in% c("highchart", "htmlwidget")))
})
