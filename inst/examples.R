
source("inst/meta.R")
devtools::load_all()
devtools::document()
devtools::install()

library(hgchmagic)

meta <- hgchMeta()
hgchWhich()
hgchList()
hgchFtype()


# Available

dir <- find.package("hgchmagic", lib.loc = NULL)
list.files(dir)
file.info(system.file("meta.csv",package = "hgchmagic"))

funsMeta <- hgchMeta()




# Pie and Donut

availableCtypeIds()

data <- sampleData("Cat", nrow = 10)
hgch_pie_Cat(data, title = "TITLE", subtitle = "Subtitle")



hgch_donut_Cat(data)
hgch_donut_Cat(data, title = "TITLE", subtitle = "Subtitle")

data <- sampleData("Cat-Num", nrow = 10)
hgch_pie_CatNum(data, export = TRUE, font_size = '17px')
hgch_donut_CatNum(data)


# Lines

data <- sampleData("Dat-Num",gt0 = FALSE)
hgch_line_DatNum(data)
hgch_line_DatNum(data, export = TRUE)

d <- sampleData("Cat-Num",gt0 = FALSE)
hgch_line_CatNum(d)

data <- sampleData("Cat-Yea-Num",nrow = 40)
hgch_line_CatYeaNum(data)
hgch_line_CatYeaNum(data, symbol = "square")

data <- sampleData("Cat-Yea-Num",nrow = 40, asFringe = TRUE)
hgch_line_CatYeaNum(data)
hgch_line_CatYeaNum(data, symbol = "square")

data <- sampleData("Cat-Dat-Num", nrow = 100)
hgch_line_CatDatNum(data)
hgch_line_CatDatNum(data, symbol = "square")

data <- read_csv("inst/data/trends.csv")
data$start <- as.Date(data$start)
hgch_line_CatDatNum(data)

data <- sampleData("Cat-Cat-Num", nrow = 100)
hgch_line_CatCatNum(data, symbol = "square")

data <- sampleData("Yea-Num", nrow = 20)
hgch_line_YeaNum(data, symbol = "square")

data <- sampleData("Yea-Num", nrow = 20)
data <- sample_n(data,size = 20)
hgch_line_YeaNum(data, symbol = "square")


# Two axis lines

data <- sampleData("Yea-Num-Num",nrow = 10)
data <- sample_n(data,size = 10)
hgch_2yline_YeaNumNum(data)

# Multilines

data <- sampleData("Yea-Num-Num",nrow = 70)
hgch_multilines_YeaNumNum(data)

data <- sampleData("Yea-Num",nrow = 70)
hgch_multilines_YeaNum(data)

data <- sample_n(data,size = 20)
hgch_multilines_YeaNum(data)

data <- sampleData("Yea-Num-Num-Num-Num-Num",nrow = 11)
hgch_multilines_YeaNumP(data)


#slope

data <- data.frame(hechos = c("secuestro", "secuestro", "delito", "delito", "ex", "ex"),
                   year = c(2007,2015,2007,2015,2007,2015),
                   pop = c(230, 123, 7139, 21597, 1082, 5480))

hgch_slope_CatYeaNum(data)

# Bars

# other_theme <- hc_theme(
#   colors = rainbow(8)
# )

tmp_theme <- hc_theme(
  colors = c('#0b356D', '#3F8909', '#ACA9A9','#CD7031','#1670D2'),
  chart = list(
    backgroundColor = NULL,
    divBackgroundImage = "http://www.comunidadandina.org/Upload/2011121103437caf800.jpg"
  ),
  title = list(
    style = list(
      color = '#333333',
      fontFamily = "Lato"
    )
  ),
  subtitle = list(
    style = list(
      color = '#666666',
      fontFamily = ""
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = '',
      color = 'black'
    ),
    itemHoverStyle = list(
      color = 'gray'
    )
  )
)





data <- sampleData("Cat", nrow = 500000)
hgch_bar_Cat(data, percentage = TRUE)

data <- sampleData("Cat-Num", nrow = 20)
hgch_bar_CatNum(data, marks = c(",", "."), nDigits = 0, percentage = TRUE)

data <- sampleData("Dat-Num", nrow = 100)
hgch_bar_ver_DatNum(data)

data <- sampleData("Yea-Num", nrow = 10)
hgch_bar_ver_YeaNum(data)
hgch_bar_ver_YeaNum(data, theme = tmp_theme)

data <- sampleData("Cat-Cat-Num", nrow = 100)
hgch_bar_grouped_ver_CatCatNum(data)
hgch_bar_grouped_hor_CatCatNum(data)

data <- sampleData("Cat-Yea-Num",nrow = 100, rep = TRUE, nlevels = 3)
hgch_bar_grouped_ver_CatYeaNum(data)

data <- sampleData("Cat-Cat-Num", nrow = 100)
hgch_bar_stacked_ver_CatCatNum(data, theme = tmp_theme)
hgch_bar_stacked_hor_CatCatNum(data)
hgch_bar_stacked_100_ver_CatCatNum(data)
hgch_bar_stacked_100_hor_CatCatNum(data)

data <- sampleData("Cat-Dat-Num", nrow = 100)
hgch_bar_stacked_ver_CatDatNum(data)
hgch_bar_stacked_100_ver_CatDatNum(data)

data <- sampleData("Cat-NumP", nrow = 5)
hgch_bar_grouped_ver_CatNumP(data)
hgch_bar_grouped_hor_CatNumP(data)

data <- sampleData("Cat-NumP",nrow = 10)
hgch_bar_grouped_hor_CatNumP(data)


hgch_bar_grouped_ver_CatCat(sampleData("Cat-Cat",nrow = 10))
hgch_bar_grouped_hor_CatCat(sampleData("Cat-Cat",nrow = 10))


data <- sampleData('Cat-Num',nrow = 100)
hgch_circular_bar_CatNum(data)
df <- data.frame(name = c('bum','gtos'), y = c(34,90))
hgch_circular_bar_CatNum(df, caption = 'ajknsa', title = 'ndjsdd', subtitle = 'njaks')

data <- sampleData("NumP")

hgch_waterfall_CatNum(sampleData("Cat-Num",nrow = 10), title = "hola")

# Area

data <- sampleData("Dat-Num")
data <- sampleData("Dat-Num",gt0 = FALSE)
hgch_area_DatNum(data)
hgch_area_DatNum(data, startAtZero = TRUE)


d <- sampleData("Cat-Num",gt0 = FALSE)
hgch_area_CatNum(d)

data <- sampleData("Cat-Cat-Num", nrow = 100)
hgch_area_CatCatNum(data)
hgch_area_CatCatNum(data, symbol = "square")
hgch_area_CatCatNum(data, symbol = "square", startAtZero = TRUE)
hgch_area_stacked_CatCatNum(data)
hgch_area_stacked_CatCatNum(data, symbol = "square")
hgch_area_stacked_100_CatCatNum(data, symbol = "square")

data <- sampleData("Cat-Yea-Num",nrow = 10)
hgch_area_CatYeaNum(data)
hgch_area_CatYeaNum(data, symbol = "square")

data <- sampleData("Cat-Yea-Num",nrow = 100, gt0 = TRUE)
hgch_area_CatYeaNum(data)
hgch_area_stacked_CatYeaNum(data, symbol = "square")
hgch_area_stacked_100_CatYeaNum(data, symbol = "square")

data <- sampleData("Cat-Dat-Num", nrow = 100)
hgch_area_CatDatNum(data, title = "hola", yAxisTitle = "Ventas")
hgch_area_CatDatNum(data, startAtZero = TRUE)
hgch_area_stacked_CatDatNum(data)
hgch_area_stacked_100_CatDatNum(data)

data <- sampleData("Yea-Num", nrow = 20)
hgch_area_YeaNum(data, symbol = "square")


# Heatmap

data <- sampleData("Cat-Cat-Num", nrow = 40)
hgch_heatmap_matrix_CatCatNum(data)


# Stream

data <- sampleData("Cat-Cat-Num", nrow = 100)
#hgch_stream_CatCatNum(data)

hgch_area_stacked_CatCatNum(data, symbol = "square")
hgch_area_stacked_100_CatCatNum(data, symbol = "square")


# Treemap
data <- sampleData("Cat-Num",nrow = 10)
hgch_treemap_CatNum(data, color_title = 'blue')
hgch_treemap_CatNum(data, export = TRUE)


hgch_treemap_discrete_color_CatNum(data, export = TRUE)


## Treemap 2Vars
## Nested Treemap

data <- sampleData("Cat-Cat-Num",nrow = 10, gt0 = TRUE)
hgch_treemap_CatCatNum(data)


# Spider
data <- sampleData("Cat-Num", nrow = 10)
hgch_spider_CatNum(data)

data <- sampleData("Cat-Num-Num", nrow = 10)
hgch_spider_CatNumNum(data)

hgch_polarcolumn_CatNum(sampleData("Cat-Num",nrow = 10))

# Scatter

hgch_bubble_CatNum(sampleData("Cat-Num",nrow = 10))

data <- sampleData("Cat-Num-Num", nrow = 20)
hgch_scatter_CatNumNum(data)

data <- sampleData("Cat-Num-Num-Num", nrow = 10)
hgch_scatter_CatNumNumNum(data)

data <- sampleData("Cat-Cat-Num-Num", nrow = 20)
hgch_scatter_CatCatNumNum(data)

data <- sampleData("Cat-Cat-Num-Num-Num", nrow = 20)
hgch_scatter_CatCatNumNumNum(data)

# Maps
#
# geo <- read_csv(system.file("aux/world-geo.csv",package = "hgchmagic"))
# data <- data_frame(country = sample(geo$code))
# data$valor <- runif(6)
#
# hgch_map_choro_world_GcdNum(data)
#
#
#
# pobl <- data.frame(pais = c("US", "ZW", "FR", "CO"), pob = c(316129,14150, 7978979, 7979))
# hgch_map_bubbles_world_GcdNum(pobl, geoinfoPath = "inst/aux/world-cod.csv", geoCodeVar='iso2', geoNameVar = "name")
#
#
# f <-  data.frame(
#   code = c('BRA-MGE', "ARG-DFD"),
#   vartwteuywgdbskjbskdcbskdjf = c(12743845843.7989,3345)
# )
#
#
#
#
# hgch_map_bubbles_latinAmerican_GcdNum(f,geoinfoPath = "inst/aux/latam-geo.csv",
#                   geoCodeVar = "code",
#                   geoNameVar = "name", export = TRUE, col_bur = 'black')
#
#
#
# f2 <-  data.frame(
#   code = c('ARG-DFD','BRA-MGE', NA),
#   xansdjsa = c(12, NA,12),
#   yasksas = c(3345, 56.08089,NA)
# )
#
# hgch_map_bubbles_latinAmerican_GcdNumNum(f2,geoinfoPath = "inst/aux/latam-geo.csv",
#                                     geoCodeVar = "code",
#                                     geoNameVar = "name", export = TRUE, col_bone = 'orange', col_btwo = 'red')
#
#
# Pyramid
data <- sampleData('Cat-Num')
hgch_pyramid_CatNum(data)

hgch_funnel_CatNum(data)


# Heatmap
