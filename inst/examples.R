
# test cat plots ----------------------------------------------------------

hgch_bar_Cat(sample_data("Cat"), dataLabels_show = TRUE)
hgch_bubbles_Cat(sample_data("Cat", 100, addNA = F), dataLabels_show = TRUE)
hgch_donut_Cat(sample_data("Cat", nrow = 10), dataLabels_show = TRUE)
hgch_pie_Cat(sample_data("Cat", nrow = 10), dataLabels_show = TRUE, inner_dataLabels = TRUE)
hgch_treemap_Cat(sample_data("Cat", nrow = 10), dataLabels_show = TRUE)

# bar cat num examples ----------------------------------------------------

hgch_bar_CatNum(sample_data("Cat-Num"))

hgch_bar_CatNum(sample_data("Cat-Num"), dataLabels_show = T)

data <- data.frame(cat = c("panda", "isis", "hueco"), val = runif(3, 0,1))
hgch_bar_CatNum(data, dataLabels_show = T)


hgch_bar_CatNum(sample_data("Cat-Num"),
                opts = list(title = "titulo"))

hgch_bar_CatNum(sample_data("Cat-Num"),
                opts = list(title = "titulo"))

hgch_bar_CatNum(sample_data("Cat-Num"),
                title = "Esto es un titulo <br/> muito lindo muito agradavel",
                subtitle = "esto es un subititulo",
                caption = "esto es un caption <br/> lalal lala saldk alskdmf<br/>sdlaksnla kamslkasnd",
                branding_include = TRUE)

d <- data.frame(cosas = c("Piedra", "Papel", "Tijera"), total = c(23, 45, -10))
hgch_bar_CatNum(d, tooltip = "<b>{cosas}</b>", caption = "esto es un caption")
hgch_bar_CatNum(d, dataLabels_show = TRUE, highlight_value = "Papel")
hgch_bar_CatNum(d,
                dataLabels_show = FALSE, order = c("Piedra"))
hgch_bar_CatNum(d,
                theme = list(dataLabels_color = "red",
                             dataLabels_show = TRUE,
                             dataLabels_text_outline = "none",
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



# treemap cat num example -------------------------------------------------

hgch_treemap_CatNum(sample_data('Cat-Num'), dataLabels_show = TRUE)

hgch_treemap_CatNum(d, color_by = "cosas")



# bubbles cat nun example -------------------------------------------------

hgch_bubbles_CatNum(d)
hgch_bubbles_CatNum(d, dataLabels_show = TRUE)
hgch_bubbles_CatNum(d, prefix = "$",
                    color_by = "cosas",
                    dataLabels_show = TRUE,
                    theme = list(dataLabels_text_outline = "none"))


hgch_line_DatNum(sample_data("Dat-Num"), background_color = "#000000")




# Bar cat cat num examples ------------------------------------------------
data <- sample_data("Cat-Cat-Num")
hgch_bar_CatCatNum(data, hor_title = " ", dataLabels_show = TRUE, dataLabels_size = 11, dataLabels_format_sample = "1.234,1")
hgch_bar_CatCatNum(data, title = "Esto es un titulo",
                   subtitle = "Esto es un subtitulo<br/><br/>",
                   grid_color = "red",
                   axis_line_width = 1,
                   grid_y_width = 1,
                   grid_y_color = "red")
hgch_bar_CatCatNum(data, branding_include = T, percentage = T, sort = "desc")
hgch_bar_CatCatNum(data, branding_include = T, percentage = T, percentage_col = "Elit (Cat)")
hgch_bar_CatCatNum(data, drop_na_legend = TRUE)
hgch_bar_CatCatNum(data, drop_na = TRUE)

data <- data.frame(Cosas = c("Piedra", "Piedra", "Hoja", "Hoja", NA, "Tijera", "Tijera", "Hoja", "Piedra"),
                   Seres = c("Elfo", "Enano", "Elfo", "Gigante", "Golondrina", "Mago", "Enano", "Mago", "Mago"),
                   Poder = runif(9, 1, 1000))
hgch_bar_CatCatNum(data, order = c("Gigante"), order_legend = c("Piedra", "Hoja"))
hgch_bar_CatCatNum(data, graph_type = "stacked", percentage = TRUE, dataLabels_show = TRUE)


# Bubble cat cat num examples ---------------------------------------------
hgch_bubbles_CatNum(sample_data("Cat-Num"),
                       plot_border_color = 'red',
                    plot_border_width=10, branding_include = T)

hgch_bubbles_CatCatNum(sample_data("Cat-Cat-Num"))
hgch_bubbles_CatCatNum(sample_data("Cat-Cat-Num"), dataLabels_show = TRUE)
hgch_bubbles_CatCatNum(sample_data("Cat-Cat-Num"), percentage = TRUE, dataLabels_show = TRUE, format_sample_num = "1.234,1")



# Treemap cat cat num examples --------------------------------------------

hgch_treemap_CatCatNum(sample_data("Cat-Cat-Num"))
hgch_treemap_CatCatNum(sample_data("Cat-Cat-Num"), dataLabels_show = TRUE)



# Line dat num examples ---------------------------------------------------
data <- sample_data("Dat-Num",5000)
hgch_line_DatNum(data)
hgch_line_DatNum(data, date_intervals = "day")
hgch_line_DatNum(data, date_intervals = "month")
hgch_line_DatNum(data, date_intervals = "week")
hgch_line_DatNum(data, date_intervals = "year")

# Line Cat Dat Num examples -----------------------------------------------

hgch_line_CatDatNum(sample_data("Cat-Dat-Num"))



# Map
data <- data.frame(pais = c( "San Andrés y Providencia", "Cauca", "Nariño","Chocó", "Tolima", "Caquetá", "Norte de Santander"),
                   random = runif(7, 10, 1000))
hgch_choropleth_GnmNum(data = data, legend_layout = "vertical")


hgch_mapbubbles_GltGlnNum(sample_data("Num-Num-Num"))



# Year --------------------------------------------------------------------

data <- data.frame(anio = 2020:2024, vlor = c(50, 20, 30, 14, 55))

hgch_bar_YeaNum(data)

data <- data.frame(date = c("2020-05-03","2020-05-04","2020-05-05","2020-05-06", "2020-05-07"),
                   vlor = c(50, 20, 30, 14, 55))
hgch_line_DatNum(data)

