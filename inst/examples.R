
hgch_bar_CatNum(sampleData("Cat-Num"))
hgch_bar_CatNum(sampleData("Cat-Num"),
                opts = list(title = "titulo"))

hgch_bar_CatNum(sampleData("Cat-Num"),
                opts = list(title = "titulo"))

hgch_bar_CatNum(sampleData("Cat-Num"),
                title = "esto es un titulo",
                subtitle = "esto es un subitu",
                caption = "esto es un caption",
                branding_include = TRUE,
                plot_margin_top = 10)

d <- data.frame(cosas = c("Piedra", "Papel", "Tijera"), total = c(23, 45, -1))
hgch_bar_CatNum(d)
hgch_bar_CatNum(d,
                text_show = FALSE, highlight_value = "Papel")
hgch_bar_CatNum(d,
                text_show = FALSE, order = c("Piedra"))
hgch_bar_CatNum(d,
                theme = list(label_color = "red",
                            label_text_outline = "none",
                            background_color = "00C0C0"))

hgch_bar_CatNum(d, branding_include = TRUE, background_color = "#000000")

hgch_bubbles_CatNum(d, prefix = "$",
                    color_by = "cosas",
                    theme = list(label_text_outline = "none"))

hgch_treemap_CatNum(sampleData('Cat-Num'))

d <- data.frame(cosas = c("Piedra", "Papel", "Tijera"), total = c(23, 45, 111))
hgch_treemap_CatNum(d, color_by = "cosas")

hgch_pie_CatNum(d)
hgch_donut_CatNum(d, color_by = "cosas") %>% hc_plotOptions(pie = list(dataLabels = list(distance = -100),
                                                                     showInLegend = TRUE))

hgch_donut_CatNum(d)
hgch_donut_CatNum(d, color_by = "cosas")


hgch_line_DatNum(sampleData("Dat-Num"), background_color = "#000000")


# Cat-Cat-Num -------------------------------------------------------------

hgch_bar_CatCatNum(sampleData("Cat-Cat-Num"))
hgch_bar_CatCatNum(sampleData("Cat-Cat-Num"), graph_type = "stacked")
hgch_bar_CatCatNum(sampleData("Cat-Cat-Num"), graph_type = "stacked", percentage = TRUE)








