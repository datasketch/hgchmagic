
library(devtools)
load_all()
document()
install()

library(hgchmagic)

# Pie and Donut

data <- sampleData("Ca", nrow = 10)
hgch_pie_Ca(data)
hgch_donut_Ca(data)

data <- sampleData("Ca-Nu", nrow = 10)
hgch_pie_CaNu(data)
hgch_donut_CaNu(data)


# Lines

data <- sampleData("Da-Nu",gt0 = FALSE)
hgch_line_DaNu(data)

d <- sampleData("Ca-Nu",gt0 = FALSE)
hgch_line_CaNu(d)

data <- sampleData("Ca-Ye-Nu",nrow = 10)
hgch_line_CaYeNu(data)
hgch_line_CaYeNu(data, symbol = "square")

data <- sampleData("Ca-Da-Nu", nrow = 100)
data <- read_csv("inst/data/trends.csv")
data$start <- as.Date(data$start)
hgch_line_CaDaNu(data)

data <- sampleData("Ca-Ca-Nu", nrow = 100)
hgch_line_CaCaNu(data, symbol = "square")

data <- sampleData("Ye-Nu", nrow = 20)
hgch_line_YeNu(data, symbol = "square")


# Two axis lines

data <- sampleData("Ye-Nu-Nu",nrow = 10)
hgch_2yline_YeNuNu(data)

# Multilines

data <- sampleData("Ye-Nu-Nu",nrow = 11)
hgch_multilines_YeNuP(data)
data <- sampleData("Ye-Nu-Nu-Nu-Nu-Nu",nrow = 11)
hgch_multilines_YeNuP(data)

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





data <- sampleData("Ca", nrow = 50)
hgch_bar_ver_Ca(data)
hgch_bar_ver_Ca(data, theme = tmp_theme)
hgch_bar_hor_Ca(data)
hgch_bar_hor_top_Ca(data)
hgch_bar_ver_top_Ca(data)

data <- sampleData("Ca-Nu", nrow = 20)
hgch_bar_ver_CaNu(data)
hgch_bar_ver_top_CaNu(data)
hgch_bar_hor_CaNu(data)
hgch_bar_hor_top_CaNu(data)

data <- sampleData("Da-Nu", nrow = 100)
hgch_bar_ver_DaNu(data)

data <- sampleData("Ye-Nu", nrow = 10)
hgch_bar_ver_YeNu(data)
hgch_bar_ver_YeNu(data, theme = tmp_theme)

data <- sampleData("Ca-Ca-Nu", nrow = 100)
hgch_bar_grouped_ver_CaCaNu(data)
hgch_bar_grouped_hor_CaCaNu(data)

data <- sampleData("Ca-Ye-Nu",nrow = 100, rep = TRUE, nlevels = 3)
hgch_bar_grouped_ver_CaYeNu(data)

data <- sampleData("Ca-Ca-Nu", nrow = 100)
hgch_bar_stacked_ver_CaCaNu(data, theme = tmp_theme)
hgch_bar_stacked_hor_CaCaNu(data)
hgch_bar_stacked_100_ver_CaCaNu(data)
hgch_bar_stacked_100_hor_CaCaNu(data)

data <- sampleData("Ca-Da-Nu", nrow = 100)
hgch_bar_stacked_ver_CaDaNu(data)
hgch_bar_stacked_100_ver_CaDaNu(data)

data <- sampleData("Ca-NuP", nrow = 5)
hgch_bar_grouped_ver_CaNuP(data)
hgch_bar_grouped_hor_CaNuP(data)


# Area

data <- sampleData("Da-Nu")
data <- sampleData("Da-Nu",gt0 = FALSE)
hgch_area_DaNu(data)
hgch_area_DaNu(data, startAtZero = TRUE)


d <- sampleData("Ca-Nu",gt0 = FALSE)
hgch_area_CaNu(d)

data <- sampleData("Ca-Ca-Nu", nrow = 100)
hgch_area_CaCaNu(data, symbol = "square")
hgch_area_CaCaNu(data, symbol = "square", startAtZero = TRUE)
hgch_area_stacked_CaCaNu(data, symbol = "square")
hgch_area_stacked_100_CaCaNu(data, symbol = "square")

data <- sampleData("Ca-Ye-Nu",nrow = 10)
hgch_area_CaYeNu(data)
hgch_area_CaYeNu(data, symbol = "square")

data <- sampleData("Ca-Ye-Nu",nrow = 100, gt0 = TRUE)
hgch_area_CaYeNu(data)
hgch_area_stacked_CaYeNu(data, symbol = "square")
hgch_area_stacked_100_CaYeNu(data, symbol = "square")

data <- sampleData("Ca-Da-Nu", nrow = 100)
hgch_area_CaDaNu(data, title = "hola", yAxisTitle = "Ventas")
hgch_area_CaDaNu(data, startAtZero = TRUE)
hgch_area_stacked_CaDaNu(data)
hgch_area_stacked_100_CaDaNu(data)

data <- sampleData("Ye-Nu", nrow = 20)
hgch_area_YeNu(data, symbol = "square")


# Stream

data <- sampleData("Ca-Ca-Nu", nrow = 100)
hgch_stream_CaCaNu(data)

hgch_area_stacked_CaCaNu(data, symbol = "square")
hgch_area_stacked_100_CaCaNu(data, symbol = "square")


# Treemap
data <- sampleData("Ca-Nu",nrow = 10)
names(data) <- c(".rw54", "grgs")
hgch_treemap_CaNu(data)

## Treemap 2Vars
## Nested Treemap

data <- sampleData("Ca-Ca-Nu",nrow = 10, gt0 = TRUE)
hgch_treemap_CaCaNu(data)


# Spider
data <- sampleData("Ca-Nu", nrow = 10)
hgch_spider_CaNu(data)

data <- sampleData("Ca-Nu-Nu", nrow = 10)
hgch_spider_CaNuNu(data)

# Scatter

data <- sampleData("Ca-Nu-Nu", nrow = 20)
hgch_scatter_CaNuNu(data)

data <- sampleData("Ca-Nu-Nu-Nu", nrow = 10)
hgch_scatter_CaNuNuNu(data)

data <- sampleData("Ca-Ca-Nu-Nu", nrow = 20)
hgch_scatter_CaCaNuNu(data)

data <- sampleData("Ca-Ca-Nu-Nu-Nu", nrow = 20)
hgch_scatter_CaCaNuNuNu(data)





# Heatmap

