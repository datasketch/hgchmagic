
# test cat plots ----------------------------------------------------------

hgch_bar_Cat(sampleData("Cat"))
hgch_bubbles_Cat(sampleData("Cat", 100), dataLabels_show = TRUE)
hgch_donut_Cat(sampleData("Cat", nrow = 10), dataLabels_show = TRUE)
hgch_pie_Cat(sampleData("Cat", nrow = 10), dataLabels_show = TRUE, inner_dataLabels = TRUE)
hgch_treemap_Cat(sampleData("Cat", nrow = 10), dataLabels_show = TRUE)

# bar cat num examples ----------------------------------------------------

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
hgch_bar_CatNum(d, dataLabels_show = TRUE, highlight_value = "Papel")
hgch_bar_CatNum(d,
                dataLabels_show = FALSE, order = c("Piedra"))
hgch_bar_CatNum(d,
                theme = list(dataLabels_color = "red",
                             dataLabels_show = TRUE,
                             label_text_outline = "none",
                             background_color = "00C0C0"))

hgch_bar_CatNum(d, branding_include = TRUE, background_color = "#000000")


# Pie and donut cat num examples ------------------------------------------

hgch_pie_CatNum(sampleData("Cat-Num"), branding_include = TRUE)
hgch_pie_CatNum(sampleData("Cat-Num"), dataLabels_show = TRUE)
hgch_pie_CatNum(sampleData("Cat-Num"), dataLabels_show = TRUE, inner_dataLabels = TRUE)
hgch_pie_CatNum(sampleData("Cat-Num"),
                background_color = "#000000",
                legend_show = TRUE,
                legend_color = "#feafea",
                dataLabels_show = TRUE,
                dataLabels_color = "#feafea")

hgch_pie_CatNum(d)
hgch_donut_CatNum(d, color_by = "cosas")

hgch_donut_CatNum(d)
hgch_donut_CatNum(d, color_by = "cosas", legend_show = TRUE)


if (is.null(data)) stop(" dataset to visualize")

opts <- dsvizopts::merge_dsviz_options(...)
l <- hgchmagic_prep(data, opts = opts)

d <- l$d


# treemap cat num example -------------------------------------------------

hgch_treemap_CatNum(sampleData('Cat-Num'), dataLabels_show = TRUE)

d <- data.frame(cosas = c("Piedra", "Papel", "Tijera"), total = c(23, 45, 111))
hgch_treemap_CatNum(d, color_by = "cosas")



# bubbles cat nun example -------------------------------------------------

hgch_bubbles_CatNum(d)
hgch_bubbles_CatNum(d, dataLabels_show = TRUE)
hgch_bubbles_CatNum(d, prefix = "$",
                    color_by = "cosas",
                    dataLabels_show = TRUE,
                    theme = list(label_text_outline = "none"))


hgch_line_DatNum(sampleData("Dat-Num"), background_color = "#000000")




# Cat-Cat-Num -------------------------------------------------------------

hgch_bar_CatCatNum(sampleData("Cat-Cat-Num"))
hgch_bar_CatCatNum(sampleData("Cat-Cat-Num"), graph_type = "stacked")
hgch_bar_CatCatNum(sampleData("Cat-Cat-Num"), graph_type = "stacked", percentage = TRUE)








