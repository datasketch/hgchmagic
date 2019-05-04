
#source("inst/meta.R")
devtools::load_all()
devtools::document()
devtools::install()


library(hgchmagic)


# Barras ------------------------------------------------------------------

# Categoricas

datCat <- sampleData('Cat', nrow = 300)

hgch_bar_Cat(datCat,
             opts = list(color_scale = 'no'))


opts <- list (
  title = 'Esto es un título',
  subtitle = 'Esto es un subtitulo',
  caption = 'Esto es un caption',
  export = TRUE,
  agg_text = "hola",
  lang = 'en'
)

hgch_bar_Cat(datCat, opts = opts)

opts <- list(
  orientation = 'hor',
  verLabel = 'Texto vertical',
  horLabel = 'Texto horizontal'
)

hgch_bar_Cat(datCat, opts = opts)

opts <- list(
  horLine = 40,
  percentage = TRUE
)
hgch_bar_Cat(datCat, opts = opts)

opts <- list(
  color_scale = "no",
  highlight_valueColor = '#FDAACC',
  highlight_value = 'X_A',
  order = c("X_A", "X_D")
)

hgch_bar_Cat(datCat, opts = opts)

# Categorica-Numerica

datCatNum <- sampleData('Cat-Num')
hgch_bar_CatNum(datCatNum)

opts <- list(export = TRUE)
hgch_bar_CatNum(datCatNum, opts = opts)

opts <- list(export = TRUE,
             lang = 'en')
hgch_bar_CatNum(datCatNum,
                opts = opts)

opts <- list(sort = 'asc',
             showText = FALSE)
hgch_bar_CatNum(datCatNum, opts = opts)

opts <- list(
  export = TRUE,
  nDigits = 2,
  #color_scale = "discrete",
  theme = tma(custom = list(#colors = c('#FDC123'),
                            showText = T,
                            colors_diff = F)),
  marks = c(',', '.'),
  prefix = "$"
)

hgch_bar_CatNum(datCatNum, opts = opts)


# Data-Numerica
datDatNum <- sampleData('Dat-Num')
hgch_bar_CatNum(datDatNum)
hgch_bar_CatNum(datDatNum, dropNa = TRUE)


#Años - Númerico
datYeaNum <- sampleData('Yea-Num', 250)
hgch_bar_CatNum(datYeaNum, agg = 'mean', showText = F)


# Categoricos - hgch_bar_CatCat(dfCC, theme = tma(background = '#ccc'), colors = c('orange', 'darkred'))
dfCC <- sampleData('Cat-Cat', 999)
arg <- list(
  export = TRUE
)
hgch_bar_CatCat(dfCC, opts = arg)

arg <- list(
  percentage = TRUE,
  theme = tma(colors_diff= TRUE)
)
hgch_bar_CatCat(dfCC, opts = arg)
hgch_bar_CatCat(dfCC, opts = list(graphType = "stacked"))
hgch_bar_CatCat(dfCC, opts = list(theme = tma(custom = list(background = '#FDCFFF', colors_diff= F))))
hgch_bar_CatCat(dfCC, opts = list(percentage = T, graphType = "stacked"))
hgch_bar_CatCat(dfCC, opts = list(percentage = T, graphType = "stacked", order1 = c("TypeA", "TypeB", "TypeE"), showText = F))
hgch_bar_CatCat(dfCC, opts = list(percentage = T, graphType = "stacked", order2 = "X_B"))
# Categoricos - categoricos - Numericos
dfCCN <- sampleData('Cat-Cat-Num', 500)
hgch_bar_CatCatNum(dfCCN)
opts = list(
  color_scale = "countinuos"
)
hgch_bar_CatCatNum(dfCCN, opts = opts)

opts = list(
  percentage = T,
  graphType = "stack",
  order1 = c("CatB", "CatC")
)
hgch_bar_CatCatNum(dfCCN, opts = opts)

opts <- list(
  percentage = T,
  orientation = "ver",
  horLine = 20,
  verLine_label  = 'linea',
  verLine = 3,
  allow_point = T
)
hgch_bar_CatCatNum(dfCCN, opts = opts)


# Categoricos - Fecha - numericos
dfCDN <- sampleData('Cat-Dat-Num')
hgch_bar_CatCatNum(dfCDN)
hgch_bar_CatCatNum(dfCDN,graphType = "stack")
hgch_bar_CatCatNum(dfCDN,graphType = "stack", dropNa = c(TRUE, FALSE))
hgch_bar_CatCatNum(dfCDN,graphType = "stack", dropNa = c(TRUE, TRUE))
hgch_bar_CatCatNum(dfCDN,graphType = "stack", percentage = T)
hgch_bar_CatCatNum(dfCDN, showText = F)

# Categoricos - Años - Numericos
dfCAN <- sampleData('Cat-Yea-Num', 100)
hgch_bar_CatCatNum(dfCAN)
hgch_bar_CatCatNum(dfCAN, percentage = TRUE)
hgch_bar_CatCatNum(dfCAN, percentage = TRUE, graphType = "stack")
hgch_bar_CatCatNum(dfCAN, percentage = TRUE, graphType = "stack", orientation = "hor")

# Categoricos y P columnas numericas
dfCNp <- sampleData('Cat-NumP')
hgch_bar_CatNumP(dfCNp)
hgch_bar_CatNumP(dfCNp, orientation = "hor")
hgch_bar_CatNumP(dfCNp, graphType = "stacked")
hgch_bar_CatNumP(dfCNp, graphType = "stacked", orientation = "hor", percentage = T)
# Líneas ------------------------------------------------------------------
# Categoricas

datCat <- sampleData('Cat', nrow = 300)
opts <- list(
  horLine = 40,
  theme = tma(list(showText = FALSE))
)
hgch_line_Cat(datCat, opts = opts)
hgch_line_Cat(datCat, opts = list(orientation = 'hor', horLine = 5, export = TRUE))
hgch_line_Cat(datCat,
              opts = list(spline = T,
                          showText = F))
hgch_line_Cat(datCat,
              opts = list(startAtZero = F,
                          sort = 'desc',
                          order = c('IlkD'),
                          theme = tma(list(colors = "darkred")))
)
hgch_line_Cat(datCat,
              opts = list(title = 'Esto es un título',
             subtitle = 'Esto es un subtitulo',
             caption = 'Esto es un caption')
)
hgch_line_Cat(datCat,
              opts = list(spline = TRUE,
                          orientation = "hor")
)

# Categorica-Numerica

datCatNum <- sampleData('Cat-Num')
hgch_line_CatNum(datCatNum,
                 opts = list(export = T,
                             lang = 'en'))
hgch_line_CatNum(datCatNum, opts = list(sort = 'asc', dropNa = TRUE, showText = F))
hgch_line_CatNum(datCatNum, opts = list(sort = 'desc', colors = c('orange'), order = 'TypeB', showText = F))
hgch_line_CatNum(datCatNum,
                 opts = list(export = TRUE,
                             nDigits = 2,
                             theme = tma(showText = F),
                             marks = c(',', '.'),
                             suffix =  '<b>cosas</b>',
                             order = c('FormE')))

# Data-Numerica
datDatNum <- sampleData('Dat-Num', 100)
hgch_line_CatNum(datDatNum)
hgch_line_CatNum(datDatNum,
                 opts = list(dropNa = TRUE,
                             colors = '#8B0000'))


#Años - Númerico
datYeaNum <- sampleData('Yea-Num', 250)
hgch_line_CatNum(datYeaNum, opts = list(agg = 'mean', startAtZero = F))
hgch_line_CatNum(datYeaNum, opts = list(agg = 'mean', startAtZero = T, dropNa = T))

# Categoricos - categoricos
dfCC <- sampleData('Cat-Cat', 999)
hgch_line_CatCat(dfCC, opts = list(export = T))
hgch_line_CatCat(dfCC, opts = list(showText = F))
hgch_line_CatCat(dfCC, opts = list(percentage = TRUE, horLine = 15, verLine = 3))

# Categoricos - categoricos - Numericos
dfCCN <- sampleData('Cat-Cat-Num', 500)
hgch_line_CatCatNum(dfCCN,
                    opts = list(spline = T,
                                export = T,
                                lang = 'en'))
hgch_line_CatCatNum(dfCCN, opts = list(spline = T, orientation = "hor", agg = "mean", startAtZero = F))
hgch_line_CatCatNum(dfCCN, opts = list(spline = T,  theme = tma(symbLine = F, showText = F)))
hgch_line_CatCatNum(dfCCN, opts = list(colors = c('#FF0AC2', '#FFCDDD', '#FDFD0D', '#FDACDC')))


# Categoricos - Fecha - numericos
dfCDN <- sampleData('Cat-Dat-Num')
hgch_line_CatCatNum(dfCDN)

# Categoricos - Años - Numericos
dfCAN <- sampleData('Cat-Yea-Num')
hgch_line_CatCatNum(dfCAN, startAtZero = F)

# Categoricos y P columnas numericas
dfCNp <- sampleData('Cat-NumP')
hgch_line_CatNumP(dfCNp)
hgch_line_CatNumP(dfCNp, showText = F)

# Areas -------------------------------------------------------------------

# Categorico
dfC <- sampleData('Cat')
hgch_area_Cat(dfC, export = T)
hgch_area_Cat(dfC, colorOpacity = 1, showText = F)
hgch_area_Cat(dfC, horLabel = 'titulo horizontal', verLabel = 'titulo vertical')
hgch_area_Cat(dfC, horLabel = 'titulo horizontal', verLabel = 'titulo vertical', spline = TRUE, orientation = 'hor')
hgch_area_Cat(dfC, spline = TRUE, tooltip = list(pointFormat = 'Acá se personaliza el tooltip'))

dfCN <- sampleData('Cat-Num')
hgch_area_CatNum(dfCN, export = T)
hgch_area_CatNum(dfCN, export = T, lang = 'en')
hgch_area_CatNum(dfCN, export = T, lang = 'pt')
hgch_area_CatNum(dfCN, colors = 'orange', agg = 'mean', colorOpacity = 0.7)
hgch_area_CatNum(dfCN, colors = 'orange', agg = 'median', spline = TRUE)

dfCCN <- sampleData('Cat-Cat-Num', 1000)
hgch_area_CatCatNum(dfCCN, export = T)
hgch_area_CatCatNum(dfCCN, export = T, lang = 'pt')
hgch_area_CatCatNum(dfCCN, showText = F)
hgch_area_CatCatNum(dfCCN, horLabel = 'titulo horizonal',
                    verLabel = 'titulo vertical', spline = TRUE,
                    percentage = TRUE, colorOpacity = 0.1)
hgch_area_CatCatNum(dfCCN, horLabel = 'titulo horizonal',
                    verLabel = 'titulo vertical', spline = TRUE,
                    percentage = TRUE, graphType = 'stack')
hgch_area_CatCatNum(dfCCN, horLabel = 'titulo horizonal',
                    verLabel = 'titulo vertical', spline = TRUE,
                    graphType = 'stack')
hgch_area_CatCatNum(dfCCN, horLabel = 'titulo horizonal',
                    verLabel = 'titulo vertical', spline = TRUE,
                    graphType = 'stack', theme = tma(showText = F))
hgch_area_CatCatNum(dfCCN, horLabel = 'titulo horizonal',
                    verLabel = 'titulo vertical', spline = TRUE,
                    graphType = 'stack', theme = tma(showText = F),
                    horLine = 100000, horLineLabel = 'linea horizontal')
hgch_area_CatCatNum(dfCCN, horLabel = 'titulo horizonal',
                    verLabel = 'titulo vertical', spline = TRUE,
                    graphType = 'stack', theme = tma(showText = F),
                    verLine = 3, verLineLabel = 'linea vertical')
hgch_area_CatCatNum(dfCCN, horLabel = 'titulo horizonal',
                    verLabel = 'titulo vertical', spline = TRUE,
                    graphType = 'stack', theme = tma(showText = F),
                    verLine = 3, verLineLabel = 'linea vertical',
                    horLine = 100000, horLineLabel = 'linea horizontal')

hgch_area_CatCatNum(dfCCN, horLabel = 'titulo horizonal',
                    orientation = 'hor',
                    format = c('$', ' en cositas'),
                    marks = c(',', '.'),
                    verLabel = 'titulo vertical', spline = TRUE,
                    graphType = 'stack', theme = tma(showText = F),
                    verLine = 100000, verLineLabel = 'linea vertical',
                    horLine = 3, horLineLabel = 'linea horizontal')

dfCNp <- sampleData('Cat-Num-Num-Num-Num')

hgch_area_CatNumP(dfCNp)
hgch_area_CatNumP(dfCNp, colorOpacity = 0.3)


# Pie ---------------------------------------------------------------------

# Categorico
dfC <- sampleData("Cat", nrow = 10)
hgch_pie_Cat(dfC, title = "TITLE", subtitle = "Subtitle")


# Categorico - Numerico
data <- sampleData("Cat-Num", nrow = 100)
hgch_pie_CatNum(data)
hgch_pie_CatNum(data, opts = list(export = TRUE, allow_point = TRUE, cursor = "pointer"))
hgch_pie_CatNum(data, colors = c("darkred", "#FFFDDD"))
hgch_pie_CatNum(data, colors = c("darkred"), colorScale = "no", highlightValue = "CatD", highlightValueColor = "orange")
hgch_pie_CatNum(data,
                title = "esto es un título",
                subtitle = "Subtitulo",
                caption = "estos son los creditos")
hgch_pie_CatNum(data, percentage = TRUE, nDigits = 3, marks = c(",", "x"))
hgch_pie_CatNum(data, export = TRUE, theme = tma(showText = F, showLegend = T))
hgch_pie_CatNum(data,
                opts = list(percentage = TRUE,
                            nDigits = 2,
                            theme = tma(custom = list(showText = F, legend_show = T))
                ))


# Donut -------------------------------------------------------------------

hgch_donut_CatNum(data)
hgch_donut_CatNum(data, opts = list(export = T, lang = 'en'))

# treemap
dataC <- sampleData('Cat')
hgch_treemap_Cat(dataC)
hgch_treemap_Cat(dataC, colors = c('#DAF311', '#11FDA0'))

dataCN <- sampleData('Cat-Num')

#discrete color
hgch_treemap_CatNum(data = dataCN, colorScale = 'discrete')
hgch_treemap_CatNum(data = dataCN,
                    colors = c('#12da3a', '#D3daf0'),
                    colorScale = 'discrete',
                    title = 'Esto es un título',
                    subtitle = 'Esto es un subtitulo',
                    caption = 'Esto es un caption',
                    export = TRUE, lang = 'en',
                    showText = F)
hgch_treemap_CatNum(data = dataCN,
                    agg = "mean",
                    marks = c(".", ","),
                    format = c("$"))
hgch_treemap_CatNum(data = dataCN,
                    percentage = TRUE,
                    nDigits = 2,
                    dropNa = T,
                    colors = c("black", "black", "black"),
                    highlightValueColor = "red",
                    highlightValue = "FormA",
                    showLegend = F,
                    theme = tma(labsData = list(colLabel = "#ffffff")))


# Treemap CatCatNum
dCCN <- sampleData('Cat-Cat-Num')
hgch_treemap_CatCatNum(dCCN)
hgch_treemap_CatCatNum(dCCN,

                        colorScale = "continuous",
                        nDigits = 2, format = c("$"),
                        marks = c("<", ">"))
hgch_treemap_CatCatNum(dCCN, showText = F)


dfCC <- sampleData('Cat-Cat', 999)
hgch_treemap_CatCat(dfCC, export = T)
hgch_treemap_CatCat(dfCC, showText = F)


dfCC <- sampleData('Cat-NumP', 999)
hgch_treemap_CatNumP(dfCC, export = T)
hgch_treemap_CatNumP(dfCC, showText = F)

# # Two axis lines
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
