
#source("inst/meta.R")
devtools::load_all()
devtools::document()
devtools::install()


library(hgchmagic)


# Barras ------------------------------------------------------------------

# Categoricas

datCat <- sampleData('Cat', nrow = 300)
hgch_bar_Cat(datCat,
             agg_text = "mean",
             allow_point = FALSE,
             theme = list(stylesY_gridLineWidth = 0,
                              colors = "black",
                              color_scale = "no",
                              background = "#FEAFEA"))

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
  ver_label = 'Texto vertical',
  hor_label = 'Texto horizontal'
)

hgch_bar_Cat(datCat, opts = opts)

opts <- list(
  hor_line = 40,
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
hgch_bar_CatNum(datCatNum, tooltip = "Categoria: {point.name}")

opts <- list(export = TRUE)
hgch_bar_CatNum(datCatNum, opts = opts)

opts <- list(export = TRUE,
             lang = 'en')
hgch_bar_CatNum(datCatNum,
                opts = opts)

opts <- list(sort = 'asc',
             text_show = FALSE)
hgch_bar_CatNum(datCatNum, opts = opts)

#Años - Númerico
datYeaNum <- sampleData('Yea-Num', 250)
hgch_bar_CatNum(datYeaNum, agg = 'mean', text_show = F)


# Categoricos - hgch_bar_CatCat(dfCC, theme = tma(background = '#ccc'), colors = c('orange', 'darkred'))
dfCC <- sampleData('Cat-Cat', 999)
arg <- list(
  export = TRUE
)
hgch_bar_CatCat(dfCC, opts = arg)

arg <- list(
  percentage = TRUE,
  theme = list(stylesY_gridLineWidth = 0,
               stylesY_lineWidth = 1,
               background = "#FEAFEA")
)
hgch_bar_CatCat(dfCC, opts = arg)
hgch_bar_CatCat(dfCC, graph_type = "stacked")
hgch_bar_CatCat(dfCC, opts = list(percentage = T, graph_type = "stacked"))
hgch_bar_CatCat(dfCC, opts = list(percentage = T, graph_type = "stacked", order1 = c("TypeA", "TypeB", "TypeE"), text_show = F))
hgch_bar_CatCat(dfCC,  percentage = T, graph_type = "stacked", order2 = "X_B")
# Categoricos - categoricos - Numericos
dfCCN <- sampleData('Cat-Cat-Num', 500)
hgch_bar_CatCatNum(dfCCN)
opts = list(
  color_scale = "countinuos"
)
hgch_bar_CatCatNum(dfCCN, opts = opts)

opts = list(
  percentage = T,
  graph_type = "stack",
  order1 = c("CatB", "CatC")
)
hgch_bar_CatCatNum(dfCCN, opts = opts)

opts <- list(
  percentage = T,
  orientation = "ver",
  hor_line = 20,
  ver_line_label  = 'linea',
  ver_line = 3,
  allow_point = T
)
hgch_bar_CatCatNum(dfCCN, opts = opts)


# Categoricos - Fecha - numericos
dfCDN <- sampleData('Cat-Dat-Num')
hgch_bar_CatCatNum(dfCDN)
hgch_bar_CatCatNum(dfCDN,graph_type = "stack")
hgch_bar_CatCatNum(dfCDN,graph_type = "stack", drop_na = TRUE, drop_na_legend = FALSE)
hgch_bar_CatCatNum(dfCDN,graph_type = "stack",  drop_na = TRUE, drop_na_legend = TRUE)
hgch_bar_CatCatNum(dfCDN,graph_type = "stack", percentage = T, tooltip = "{series.name}")
hgch_bar_CatCatNum(dfCDN, text_show = F)

# Categoricos - Años - Numericos
dfCAN <- sampleData('Cat-Yea-Num', 100)
hgch_bar_CatCatNum(dfCAN)
hgch_bar_CatCatNum(dfCAN, percentage = TRUE)
hgch_bar_CatCatNum(dfCAN, percentage = TRUE, graph_type = "stack")
hgch_bar_CatCatNum(dfCAN, percentage = TRUE, graph_type = "stack", orientation = "hor")

# Líneas ------------------------------------------------------------------
# Categoricas

datCat <- sampleData('Cat', nrow = 1300)
opts <- list(
  hor_line = 40,
  theme = list(text_show = FALSE, background = '#FEAFEA')
)
hgch_line_Cat(datCat, tooltip = "{point.y}")
hgch_line_Cat(datCat, opt)
hgch_line_Cat(datCat,
              opts = list(spline = T,
                          text_show = F))
hgch_line_Cat(datCat,
              start_zero = F,
                          sort = 'desc',
                          order = c('IlkD'),
                          theme = list(colors = "darkred", background="#FEAFEA")
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
hgch_line_CatNum(datCatNum, opts = list(sort = 'asc', drop_na = TRUE, text_show = F))
hgch_line_CatNum(datCatNum, opts = list(sort = 'desc', colors = c('orange'), order = 'TypeB', text_show = F))
hgch_line_CatNum(datCatNum,
                 opts = list(export = TRUE,
                             n_digits = 2,
                             theme = tma(text_show = F),
                             marks = c(',', '.'),
                             suffix =  '<b>cosas</b>',
                             order = c('FormE')))

# Data-Numerica
datDatNum <- sampleData('Dat-Num', 100)
hgch_line_CatNum(datDatNum)
hgch_line_CatNum(datDatNum,
                 opts = list(drop_na = TRUE,
                             colors = '#8B0000'))


#Años - Númerico
datYeaNum <- sampleData('Yea-Num', 250)
hgch_line_CatNum(datYeaNum, opts = list(agg = 'mean', start_zero = F))
hgch_line_CatNum(datYeaNum, opts = list(agg = 'mean', start_zero = T, drop_na = T))

# Categoricos - categoricos
dfCC <- sampleData('Cat-Cat', 999)
hgch_line_CatCat(dfCC, opts = list(export = T))
hgch_line_CatCat(dfCC, opts = list(text_show = F))
hgch_line_CatCat(dfCC, opts = list(percentage = TRUE, hor_line = 15, ver_line = 3))

# Categoricos - categoricos - Numericos
dfCCN <- sampleData('Cat-Cat-Num', 500)
hgch_line_CatCatNum(dfCCN,
                    opts = list(spline = T,
                                export = T,
                                lang = 'en'))
hgch_line_CatCatNum(dfCCN, opts = list(spline = T, orientation = "hor", agg = "mean", start_zero = F))
hgch_line_CatCatNum(dfCCN, opts = list(spline = T,  theme = tma(symbLine = F, text_show = F)))
hgch_line_CatCatNum(dfCCN, opts = list(colors = c('#FF0AC2', '#FFCDDD', '#FDFD0D', '#FDACDC')))


# Categoricos - Fecha - numericos
dfCDN <- sampleData('Cat-Dat-Num')
hgch_line_CatCatNum(dfCDN)

# Categoricos - Años - Numericos
dfCAN <- sampleData('Cat-Yea-Num')
hgch_line_CatCatNum(dfCAN, start_zero = F)


# Areas -------------------------------------------------------------------

# Categorico
dfC <- sampleData('Cat')
hgch_area_Cat(dfC, export = T)
hgch_area_Cat(dfC, hor_label = 'titulo horizontal', ver_label = 'titulo vertical')
hgch_area_Cat(dfC, hor_label = 'titulo horizontal', ver_label = 'titulo vertical', spline = TRUE, orientation = 'hor')
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
hgch_area_CatCatNum(dfCCN, text_show = F)
hgch_area_CatCatNum(dfCCN, hor_label = 'titulo horizonal',
                    ver_label = 'titulo vertical', spline = TRUE,
                    percentage = TRUE, colorOpacity = 0.1)
hgch_area_CatCatNum(dfCCN, hor_label = 'titulo horizonal',
                    ver_label = 'titulo vertical', spline = TRUE,
                    percentage = TRUE, graph_type = 'stack')
hgch_area_CatCatNum(dfCCN, hor_label = 'titulo horizonal',
                    ver_label = 'titulo vertical', spline = TRUE,
                    graph_type = 'stack')
hgch_area_CatCatNum(dfCCN, hor_label = 'titulo horizonal',
                    ver_label = 'titulo vertical', spline = TRUE,
                    graph_type = 'stack', theme = tma(text_show = F))
hgch_area_CatCatNum(dfCCN, hor_label = 'titulo horizonal',
                    ver_label = 'titulo vertical', spline = TRUE,
                    graph_type = 'stack', theme = tma(text_show = F),
                    hor_line = 100000, hor_lineLabel = 'linea horizontal')
hgch_area_CatCatNum(dfCCN, hor_label = 'titulo horizonal',
                    ver_label = 'titulo vertical', spline = TRUE,
                    graph_type = 'stack', theme = tma(text_show = F),
                    ver_line = 3, ver_lineLabel = 'linea vertical')
hgch_area_CatCatNum(dfCCN, hor_label = 'titulo horizonal',
                    ver_label = 'titulo vertical', spline = TRUE,
                    graph_type = 'stack', theme = tma(text_show = F),
                    ver_line = 3, ver_lineLabel = 'linea vertical',
                    hor_line = 100000, hor_lineLabel = 'linea horizontal')

hgch_area_CatCatNum(dfCCN, hor_label = 'titulo horizonal',
                    orientation = 'hor',
                    format = c('$', ' en cositas'),
                    marks = c(',', '.'),
                    ver_label = 'titulo vertical', spline = TRUE,
                    graph_type = 'stack', theme = tma(text_show = F),
                    ver_line = 100000, ver_lineLabel = 'linea vertical',
                    hor_line = 3, hor_lineLabel = 'linea horizontal')

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
hgch_pie_CatNum(data, colors = c("darkred"), color_scale = "no", highlightValue = "CatD", highlightValueColor = "orange")
hgch_pie_CatNum(data,
                title = "esto es un título",
                subtitle = "Subtitulo",
                caption = "estos son los creditos")
hgch_pie_CatNum(data, percentage = TRUE, n_digits = 3, marks = c(",", "x"))
hgch_pie_CatNum(data, export = TRUE, theme = tma(text_show = F, legend_show = T))
hgch_pie_CatNum(data,
                opts = list(percentage = TRUE,
                            n_digits = 2,
                            theme = tma(custom = list(text_show = F, legend_show = T))
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
hgch_treemap_CatNum(data = dataCN, color_scale = 'discrete')
hgch_treemap_CatNum(data = dataCN,
                    colors = c('#12da3a', '#D3daf0'),
                    color_scale = 'discrete',
                    title = 'Esto es un título',
                    subtitle = 'Esto es un subtitulo',
                    caption = 'Esto es un caption',
                    export = TRUE, lang = 'en',
                    text_show = F, theme = list(border_widthBar = 6))
hgch_treemap_CatNum(data = dataCN,
                    agg = "mean",
                    marks = c(".", ","),
                    format = c("$"))
hgch_treemap_CatNum(data = dataCN,
                    percentage = TRUE,
                    n_digits = 2,
                    drop_na = T,
                    colors = c("black", "black", "black"),
                    highlightValueColor = "red",
                    highlightValue = "FormA",
                    legend_show = F,
                    theme = tma(labsData = list(colLabel = "#ffffff")))


# Treemap CatCatNum
dCCN <- sampleData('Cat-Cat-Num')
hgch_treemap_CatCatNum(dCCN, tooltip = "esto es un tooltip")
hgch_treemap_CatCatNum(dCCN,

                        color_scale = "continuous",
                        n_digits = 2, format = c("$"),
                        marks = c("<", ">"))
hgch_treemap_CatCatNum(dCCN, text_show = F)


dfCC <- sampleData('Cat-Cat', 999)
hgch_treemap_CatCat(dfCC, export = T)
hgch_treemap_CatCat(dfCC, text_show = F)


dfCC <- sampleData('Cat-NumP', 999)
hgch_treemap_CatNumP(dfCC, export = T)
hgch_treemap_CatNumP(dfCC, text_show = F)



# scatter -----------------------------------------------------------------

dataNumNum <- sampleData('Num-Num')
hgch_scatter_NumNum(dataNumNum)
opts_s <- list(
  marks  = c('*', '.')
)
hgch_scatter_NumNum(dataNumNum, opts = opts_s)
#



