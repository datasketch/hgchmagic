
#source("inst/meta.R")
devtools::load_all()
devtools::document()
devtools::install()


library(hgchmagic)


# Barras ------------------------------------------------------------------

# Categoricas

datCat <- sampleData('Cat', nrow = 300)
hgch_bar_Cat(datCat)
hgch_bar_Cat(datCat, title = 'Esto es un título',
             subtitle = 'Esto es un subtitulo',
             caption = 'Esto es un caption'
            )
hgch_bar_Cat(datCat, orientation = 'hor', verLabel = 'Texto vertical',
             horLabel = 'Texto horizontal')
hgch_bar_Cat(datCat, orientation = 'ver', verLabel = 'Texto vertical',
             horLabel = 'Texto horizontal')
hgch_bar_Cat(datCat, horLine = 40, percentage = TRUE)
hgch_bar_Cat(datCat, orientation = 'hor', horLine = 3)
hgch_bar_Cat(datCat, colorScale = 'discrete', theme = tma(showText = FALSE))

# Categorica-Numerica

datCatNum <- sampleData('Cat-Num')
hgch_bar_CatNum(datCatNum)
hgch_bar_CatNum(datCatNum, sort = 'asc')
hgch_bar_CatNum(datCatNum, sort = 'desc', colorScale = 'discrete')
hgch_bar_CatNum(datCatNum, export = TRUE, nDigits = 2, theme = tma(showText = F),
                marks = c(',', '.'), format = c('$', ''))

# Data-Numerica
datDatNum <- sampleData('Dat-Num')
hgch_bar_DatNum(datDatNum)
hgch_bar_DatNum(datDatNum, dropNa = TRUE)


#Años - Númerico
datYeaNum <- sampleData('Yea-Num', 250)
hgch_bar_YeaNum(datYeaNum, agg = 'mean')


# Categoricos - categoricos
dfCC <- sampleData('Cat-Cat', 999)
hgch_bar_grouped_CatCat(dfCC)
hgch_bar_grouped_CatCat(dfCC, percentage = TRUE)
hgch_bar_stacked_CatCat(dfCC)
hgch_bar_stacked_CatCat(dfCC, percentage = T)

# Categoricos - categoricos - Numericos
dfCCN <- sampleData('Cat-Cat-Num', 500)
hgch_bar_grouped_CatCatNum(dfCCN)
hgch_bar_grouped_CatCatNum(dfCCN, colors = c('#FF0AC2', '#FFCDDD', '#FDFD0D', '#FDACDC'))
hgch_bar_stacked_CatCatNum(dfCCN)

# Categoricos - Fecha - numericos
dfCDN <- sampleData('Cat-Dat-Num')
hgch_bar_grouped_CatDatNum(dfCDN)
hgch_bar_stacked_CatDatNum(dfCDN)

# Categoricos - Años - Numericos
dfCAN <- sampleData('Cat-Yea-Num')
hgch_bar_grouped_CatYeaNum(dfCAN)
hgch_bar_stacked_CatYeaNum(dfCAN, percentage = TRUE)


# Categoricos y P columnas numericas
dfCNp <- sampleData('Cat-NumP')
hgch_bar_grouped_CatNumP(dfCNp)
# Líneas ------------------------------------------------------------------


# Categoricas

datCat <- sampleData('Cat', nrow = 300)
hgch_line_Cat(datCat)
hgch_line_Cat(datCat, spline = T)
hgch_line_Cat(datCat, startAtZero = F)
hgch_line_Cat(datCat, title = 'Esto es un título',
             subtitle = 'Esto es un subtitulo',
             caption = 'Esto es un caption'
)

# Categorica-Numerica

datCatNum <- sampleData('Cat-Num')
hgch_line_CatNum(datCatNum)
hgch_line_CatNum(datCatNum, sort = 'asc', dropNa = TRUE)
hgch_line_CatNum(datCatNum, sort = 'desc', colors = c('orange'))
hgch_line_CatNum(datCatNum, export = TRUE, nDigits = 2, theme = tma(showText = F),
                marks = c(',', '.'), format = c('', ' <b>cosas</b>'), order = c('FormE'))

# Data-Numerica
datDatNum <- sampleData('Dat-Num', 100)
hgch_line_DatNum(datDatNum)
hgch_line_DatNum(datDatNum, dropNa = TRUE, colors = '#8B0000')


#Años - Númerico
datYeaNum <- sampleData('Yea-Num', 250)
hgch_line_YeaNum(datYeaNum, agg = 'mean', startAtZero = F)


# Categoricos - categoricos
dfCC <- sampleData('Cat-Cat', 999)
hgch_line_CatCat(dfCC)
hgch_line_CatCat(dfCC, percentage = TRUE)

# Categoricos - categoricos - Numericos
dfCCN <- sampleData('Cat-Cat-Num', 500)
hgch_line_CatCatNum(dfCCN, spline = T)
hgch_line_CatCatNum(dfCCN, colors = c('#FF0AC2', '#FFCDDD', '#FDFD0D', '#FDACDC'))


# Categoricos - Fecha - numericos
dfCDN <- sampleData('Cat-Dat-Num')
hgch_line_CatDatNum(dfCDN)

# Categoricos - Años - Numericos
dfCAN <- sampleData('Cat-Yea-Num')
hgch_line_CatYeaNum(dfCAN, startAtZero = F)

# Categoricos y P columnas numericas
dfCNp <- sampleData('Cat-NumP')
hgch_line_CatNumP(dfCNp)

# # Available
#
# dir <- find.package("hgchmagic", lib.loc = NULL)
# list.files(dir)
# file.info(system.file("meta.csv",package = "hgchmagic"))
#
# funsMeta <- hgchMeta()
#
#
#
#
# # Pie and Donut
#
# availableCtypeIds()
#
# data <- sampleData("Cat", nrow = 10)
# hgch_pie_Cat(data, title = "TITLE", subtitle = "Subtitle")
#
#
#
# hgch_donut_Cat(data)
# hgch_donut_Cat(data, title = "TITLE", subtitle = "Subtitle")
#
# data <- sampleData("Cat-Num", nrow = 10)
# hgch_pie_CatNum(data, export = TRUE, font_size = '17px')
# hgch_donut_CatNum(data)
#
#
# # Lines
#
# data <- sampleData("Dat-Num",gt0 = FALSE)
# hgch_line_DatNum(data)
# hgch_line_DatNum(data, export = TRUE)
#
# dc <- sampleData('Cat', nrow = 1000)
# hgch_line_Cat(dc)
# d <- sampleData("Cat-Num",gt0 = FALSE)
# hgch_line_CatNum(d, theme = tma(), percentage = T, startAtZero = F, plotBandsFromX = 1, plotBandsToX = 2)
# hgch_line_CatNum(d, theme = tma(), verLine = 5, horLine = 3000, spline = T, colors = c('orange'), sort = 'asc')
# data <- sampleData("Cat-Yea-Num",nrow = 40)
# hgch_line_CatYeaNum(data)
# hgch_line_CatYeaNum(data, symbol = "square")
#
# data <- sampleData("Cat-Yea-Num",nrow = 40, asFringe = TRUE)
# hgch_line_CatYeaNum(data)
# hgch_line_CatYeaNum(data, symbol = "square")
#
# data <- sampleData("Cat-Dat-Num", nrow = 100)
# hgch_line_CatDatNum(data)
# hgch_line_CatDatNum(data, symbol = "square")
#
# data <- read_csv("inst/data/trends.csv")
# data$start <- as.Date(data$start)
# hgch_line_CatDatNum(data)
#
# data <- sampleData("Cat-Cat-Num", nrow = 100)
# hgch_line_CatCatNum(data, horLine = 300, horLineLabel = 'hola',
#                     agg = 'mean', colors = c('red'), dropNa = c(F, T))
#
#
#
# data <- sampleData("Yea-Num", nrow = 20)
# hgch_line_YeaNum(data, symbol = "square")
#
# data <- sampleData("Yea-Num", nrow = 20)
# data <- sample_n(data,size = 20)
# hgch_line_YeaNum(data, symbol = "square")
#
#
# # Two axis lines
#
# data <- sampleData("Yea-Num-Num",nrow = 10)
# data <- sample_n(data,size = 10)
# hgch_2yline_YeaNumNum(data)
#
# # Multilines
#
# data <- sampleData("Yea-Num-Num",nrow = 70)
# hgch_multilines_YeaNumNum(data)
#
# data <- sampleData("Yea-Num",nrow = 70)
# hgch_multilines_YeaNum(data)
#
# data <- sample_n(data,size = 20)
# hgch_multilines_YeaNum(data)
#
# data <- sampleData("Yea-Num-Num-Num-Num-Num",nrow = 11)
# hgch_multilines_YeaNumP(data)
#
#
# #slope
#
# data <- data.frame(hechos = c("secuestro", "secuestro", "delito", "delito", "ex", "ex"),
#                    year = c(2007,2015,2007,2015,2007,2015),
#                    pop = c(230, 123, 7139, 21597, 1082, 5480))
#
# hgch_slope_CatYeaNum(data)
#
# # Bars
#
# # other_theme <- hc_theme(
# #   colors = rainbow(8)
# # )
#
# tmp_theme <- hc_theme(
#   colors = c('#0b356D', '#3F8909', '#ACA9A9','#CD7031','#1670D2'),
#   chart = list(
#     backgroundColor = NULL,
#     divBackgroundImage = "http://www.comunidadandina.org/Upload/2011121103437caf800.jpg"
#   ),
#   title = list(
#     style = list(
#       color = '#333333',
#       fontFamily = "Lato"
#     )
#   ),
#   subtitle = list(
#     style = list(
#       color = '#666666',
#       fontFamily = ""
#     )
#   ),
#   legend = list(
#     itemStyle = list(
#       fontFamily = '',
#       color = 'black'
#     ),
#     itemHoverStyle = list(
#       color = 'gray'
#     )
#   )
# )
#
#
#
#
#
# data <- sampleData("Cat", nrow = 500000)
# hgch_bar_Cat(data, labelWrap = NULL, title = 'titulo',
#              subtitle = 'subtitulo', caption = 'creditos',
#              horLabel = 'titulo horizontal', verLabel = 'titulo vertical', horLine = 10000, verLine = 4,
#              horLineLabel = 'linea horizontal', verLineLabel = 'linea vertical', colors = c('darkred', 'gray', 'orange', '#F3D3D3', '#FA4B4C'),
#              colorScale = 'discrete', orientation = 'ver', marks = c(',', '.'), nDigits = 2, dropNa = TRUE)
# hgch_bar_Cat(data, colorScale = 'continuous')
# hgch_bar_Cat(data, percentage = TRUE,
#              colors = c('orange'), colorScale = 'discrete',
#              highlightValue = 'NA', highlightValueColor = 'red')
#
# data <- sampleData("Cat-Num", nrow = 20)
# hgch_bar_CatNum(data, marks = c(",", "."))
# hgch_bar_CatNum(data, horLineLabel = 'hola', horLine = 2000)
# hgch_bar_CatNum(data, orientation = 'hor',  horLine = 4)
# data <- sampleData("Dat-Num", nrow = 100)
# hgch_bar_DatNum(data)
#
# data <- sampleData("Yea-Num", nrow = 100)
# hgch_bar_YeaNum(data, order = c('1909'))
# hgch_bar_YeaNum(data, horLine = 500)
#
# data <- sampleData("Cat-Cat-Num", nrow = 100)
# hgch_bar_grouped_CatCatNum(data)
# hgch_bar_grouped_CatCatNum(data, order1 = c('CatB'), order2 = c('X_C', 'X_B'), labelWrap = c(NULL, NULL))
# hgch_bar_grouped_CatCatNum(data, percentage = TRUE, format = c('','v'))
# hgch_bar_grouped_CatCatNum(data, title = 'título', subtitle = 'subtitulo', caption = 'caption',
#                            verLabel = 'título vertical',  horLabel = 'título horizontal', verLine = 5, verLineLabel = 'hoa')
#
# dyear <- sampleData('Cat-Yea-Num', nrow = 100)
# hgch_bar_stacked_CatYeaNum(dyear)
# hgch_bar_grouped_CatYeaNum(dyear, title = 'título', subtitle = 'subtitulo', caption = 'caption',
#                            verLabel = 'título vertical',  horLabel = 'título horizontal', verLine = 5, verLineLabel = 'hoa')
#
# df <- sampleData("Cat-Cat-Num", nrow = 100)
# hgch_bar_stacked_CatCatNum(df, orientation = 'hor', title = 'título', subtitle = 'subtitulo', caption = 'caption',
#                            verLabel = 'título vertical',  horLabel = 'título horizontal', verLine = 4000,
#                            marks = c(',', '.'), nDigits = 3, percentage = TRUE)
#
# #
#
#
#
# data <- sampleData('Cat-Num',nrow = 100)
# hgch_circular_bar_CatNum(data)
# df <- data.frame(name = c('bum','gtos'), y = c(34,90))
# hgch_circular_bar_CatNum(df, caption = 'ajknsa', title = 'ndjsdd', subtitle = 'njaks')
#
# data <- sampleData("NumP")
#
# hgch_waterfall_CatNum(sampleData("Cat-Num",nrow = 10), title = "hola")
#
# # Area
#
# data <- sampleData("Dat-Num")
# data <- sampleData("Dat-Num",gt0 = FALSE)
# hgch_area_DatNum(data)
# hgch_area_DatNum(data, startAtZero = TRUE)
#
#
# d <- sampleData("Cat-Num",gt0 = FALSE)
# hgch_area_CatNum(d)
#
# data <- sampleData("Cat-Cat-Num", nrow = 100)
# hgch_area_CatCatNum(data)
# hgch_area_CatCatNum(data, symbol = "square")
# hgch_area_CatCatNum(data, symbol = "square", startAtZero = TRUE)
# hgch_area_stacked_CatCatNum(data)
# hgch_area_stacked_CatCatNum(data, symbol = "square")
# hgch_area_stacked_100_CatCatNum(data, symbol = "square")
#
# data <- sampleData("Cat-Yea-Num",nrow = 10)
# hgch_area_CatYeaNum(data)
# hgch_area_CatYeaNum(data, symbol = "square")
#
# data <- sampleData("Cat-Yea-Num",nrow = 100, gt0 = TRUE)
# hgch_area_CatYeaNum(data)
# hgch_area_stacked_CatYeaNum(data, symbol = "square")
# hgch_area_stacked_100_CatYeaNum(data, symbol = "square")
#
# data <- sampleData("Cat-Dat-Num", nrow = 100)
# hgch_area_CatDatNum(data, title = "hola", yAxisTitle = "Ventas")
# hgch_area_CatDatNum(data, startAtZero = TRUE)
# hgch_area_stacked_CatDatNum(data)
# hgch_area_stacked_100_CatDatNum(data)
#
# data <- sampleData("Yea-Num", nrow = 20)
# hgch_area_YeaNum(data, symbol = "square")
#
#
# # Heatmap
#
# data <- sampleData("Cat-Cat-Num", nrow = 40)
# hgch_heatmap_matrix_CatCatNum(data)
#
#
# # Stream
#
# data <- sampleData("Cat-Cat-Num", nrow = 100)
# #hgch_stream_CatCatNum(data)
#
# hgch_area_stacked_CatCatNum(data, symbol = "square")
# hgch_area_stacked_100_CatCatNum(data, symbol = "square")
#
#
# # Treemap
# data <- sampleData("Cat-Num",nrow = 10)
# hgch_treemap_CatNum(data, color_title = 'blue')
# hgch_treemap_CatNum(data, export = TRUE)
#
#
# hgch_treemap_discrete_color_CatNum(data, export = TRUE)
#
#
# ## Treemap 2Vars
# ## Nested Treemap
#
# data <- sampleData("Cat-Cat-Num",nrow = 10, gt0 = TRUE)
# hgch_treemap_CatCatNum(data)
#
#
# # Spider
# data <- sampleData("Cat-Num", nrow = 10)
# hgch_spider_CatNum(data)
#
# data <- sampleData("Cat-Num-Num", nrow = 10)
# hgch_spider_CatNumNum(data)
#
# hgch_polarcolumn_CatNum(sampleData("Cat-Num",nrow = 10))
#
# # Scatter
#
# hgch_bubble_CatNum(sampleData("Cat-Num",nrow = 10))
#
# data <- sampleData("Cat-Num-Num", nrow = 20)
# hgch_scatter_CatNumNum(data)
#
# data <- sampleData("Cat-Num-Num-Num", nrow = 10)
# hgch_scatter_CatNumNumNum(data)
#
# data <- sampleData("Cat-Cat-Num-Num", nrow = 20)
# hgch_scatter_CatCatNumNum(data)
#
# data <- sampleData("Cat-Cat-Num-Num-Num", nrow = 20)
# hgch_scatter_CatCatNumNumNum(data)
#
# # Maps
# #
# # geo <- read_csv(system.file("aux/world-geo.csv",package = "hgchmagic"))
# # data <- data_frame(country = sample(geo$code))
# # data$valor <- runif(6)
# #
# # hgch_map_choro_world_GcdNum(data)
# #
# #
# #
# # pobl <- data.frame(pais = c("US", "ZW", "FR", "CO"), pob = c(316129,14150, 7978979, 7979))
# # hgch_map_bubbles_world_GcdNum(pobl, geoinfoPath = "inst/aux/world-cod.csv", geoCodeVar='iso2', geoNameVar = "name")
# #
# #
# # f <-  data.frame(
# #   code = c('BRA-MGE', "ARG-DFD"),
# #   vartwteuywgdbskjbskdcbskdjf = c(12743845843.7989,3345)
# # )
# #
# #
# #
# #
# # hgch_map_bubbles_latinAmerican_GcdNum(f,geoinfoPath = "inst/aux/latam-geo.csv",
# #                   geoCodeVar = "code",
# #                   geoNameVar = "name", export = TRUE, col_bur = 'black')
# #
# #
# #
# # f2 <-  data.frame(
# #   code = c('ARG-DFD','BRA-MGE', NA),
# #   xansdjsa = c(12, NA,12),
# #   yasksas = c(3345, 56.08089,NA)
# # )
# #
# # hgch_map_bubbles_latinAmerican_GcdNumNum(f2,geoinfoPath = "inst/aux/latam-geo.csv",
# #                                     geoCodeVar = "code",
# #                                     geoNameVar = "name", export = TRUE, col_bone = 'orange', col_btwo = 'red')
# #
# #
# # Pyramid
# data <- sampleData('Cat-Num')
# hgch_pyramid_CatNum(data)
#
# hgch_funnel_CatNum(data)
#
#
# # Heatmap
