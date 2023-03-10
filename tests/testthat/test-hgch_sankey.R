test_that("Sankey", {
  data <- dplyr::starwars |>
    tidyr::drop_na(sex, gender, hair_color, skin_color)
  hgch_sankey(data, var_cat = c("species", "sex", "gender"))
  hgch_sankey(data, var_cat = c( "sex", "gender", "hair_color", "skin_color"))


  data_catcat <- data |> select(sex, gender)
  hgch_sankey_CatCat(data_catcat)

  data_catcatcat <- data |> select(sex, gender, species)
  hgch_sankey_CatCatCat(data_catcatcat)


  data_cat4 <- data |> select(hair_color, sex, gender, species)
  hgch_sankey_CatCatCatCat(data_cat4)


})
