test_that("Sankey", {
  data <- dplyr::starwars |>
    tidyr::drop_na(sex, gender, hair_color, skin_color)
  hgch_sankey(data, var_cat = c("species", "sex", "gender"))
  hgch_sankey(data, var_cat = c( "sex", "gender", "hair_color", "skin_color"))
})
