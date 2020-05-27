
# test cat plots ----------------------------------------------------------

hgch_bar_Cat(sample_data("Cat"))
hgch_bubbles_Cat(sample_data("Cat", 100), dataLabels_show = TRUE)
hgch_donut_Cat(sample_data("Cat", nrow = 10), dataLabels_show = TRUE)
hgch_pie_Cat(sample_data("Cat", nrow = 10), dataLabels_show = TRUE, inner_dataLabels = TRUE)
hgch_treemap_Cat(sample_data("Cat", nrow = 10), dataLabels_show = TRUE)

# bar cat num examples ----------------------------------------------------

hgch_bar_CatNum(sample_data("Cat-Num"))
hgch_bar_CatNum(sample_data("Cat-Num"),
                opts = list(title = "titulo"))

hgch_bar_CatNum(sample_data("Cat-Num"),
                opts = list(title = "titulo"))

hgch_bar_CatNum(sample_data("Cat-Num"),
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

hgch_pie_CatNum(sample_data("Cat-Num"), branding_include = TRUE)
hgch_pie_CatNum(sample_data("Cat-Num"), dataLabels_show = TRUE)
hgch_pie_CatNum(sample_data("Cat-Num"), dataLabels_show = TRUE, inner_dataLabels = TRUE)
hgch_pie_CatNum(sample_data("Cat-Num"),
                background_color = "#000000",
                legend_show = TRUE,
                legend_color = "#feafea",
                dataLabels_show = TRUE,
                dataLabels_color = "#feafea")

d <- data.frame(cosas = c("Piedra", "Papel", "Tijera"), total = c(23, 45, 111))
hgch_pie_CatNum(d)
hgch_donut_CatNum(d, color_by = "cosas")

hgch_donut_CatNum(d)
hgch_donut_CatNum(d, color_by = "cosas", legend_show = TRUE)


if (is.null(data)) stop(" dataset to visualize")

opts <- dsvizopts::merge_dsviz_options(...)
l <- hgchmagic_prep(data, opts = opts)

d <- l$d


# treemap cat num example -------------------------------------------------

hgch_treemap_CatNum(sample_data('Cat-Num'), dataLabels_show = TRUE)

hgch_treemap_CatNum(d, color_by = "cosas")



# bubbles cat nun example -------------------------------------------------

hgch_bubbles_CatNum(d)
hgch_bubbles_CatNum(d, dataLabels_show = TRUE)
hgch_bubbles_CatNum(d, prefix = "$",
                    color_by = "cosas",
                    dataLabels_show = TRUE,
                    theme = list(label_text_outline = "none"))


hgch_line_DatNum(sample_data("Dat-Num"), background_color = "#000000")



# Bar cat cat num examples ------------------------------------------------
data <- sample_data("Cat-Cat-Num")
hgch_bar_CatCatNum(data)
hgch_bar_CatCatNum(data, drop_na_legend = TRUE)
hgch_bar_CatCatNum(data, drop_na = TRUE)

data <- data.frame(Cosas = c("Piedra", "Piedra", "Hoja", "Hoja", NA, "Tijera", "Tijera", "Hoja", "Piedra"),
                   Seres = c("Elfo", "Enano", "Elfo", "Gigante", "Golondrina", "Mago", "Enano", "Mago", "Mago"),
                   Poder = runif(9, 1, 1000))
hgch_bar_CatCatNum(data, order = c("Gigante"), order_legend = c("Piedra", "Hoja"))
hgch_bar_CatCatNum(data, graph_type = "stacked", percentage = TRUE)


# Bubble cat cat num examples ---------------------------------------------

hgch_bubbles_CatCatNum(sample_data("Cat-Cat-Num"))
hgch_bubbles_CatCatNum(sample_data("Cat-Cat-Num"), dataLabels_show = TRUE)
hgch_bubbles_CatCatNum(sample_data("Cat-Cat-Num"), percentage = TRUE, dataLabels_show = TRUE)



# Treemap cat cat num examples --------------------------------------------

hgch_treemap_CatCatNum(sample_data("Cat-Cat-Num"))
hgch_treemap_CatCatNum(sample_data("Cat-Cat-Num"), dataLabels_show = TRUE)



# Line dat num examples ---------------------------------------------------
data <- sample_data("Dat-Num")
hgch_line_DatNum(data)

