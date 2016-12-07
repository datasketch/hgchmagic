
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

data <- sampleData("Ca-Nu",gt0 = FALSE)
hgch_line_CaNu(data)

data <- sampleData("Ca-Ye-Nu",nrow = 10)
hgch_line_CaYeNu(data)
hgch_line_CaYeNu(data, symbol = "square")

data <- sampleData("Ca-Da-Nu", nrow = 100)
data <- read_csv("inst/data/trends.csv")
data$start <- as.Date(data$start)
hgch_line_CaDaNu(data)

data <- sampleData("Ca-Ca-Nu", nrow = 100)
hgch_line_CaCaNu(data, symbol = "square")


# Multilines

data <- sampleData("Ye-Nu-Nu",nrow = 11)
hgch_multilines_YeNuP(data)
data <- sampleData("Ye-Nu-Nu-Nu-Nu-Nu",nrow = 11)
hgch_multilines_YeNuP(data)

# Bars
data <- sampleData("Ca-Ye-Nu",nrow = 10)
hgch_bar_CaYeNu(data)


data <- sampleData("Ca-Ca-Nu", nrow = 100)
hgch_bar_CaCaNu(data)

# Bar hor

data <- sampleData("Ca", nrow = 10)
hgch_bar_hor_Ca(data)
hgch_bar_hor_top_Ca(data)

data <- sampleData("Ca-Nu", nrow = 10)
hgch_bar_hor_CaNu(data)
hgch_bar_hor_top_CaNu(data)




# Treemap
data <- sampleData("Ca-Nu",nrow = 10)
names(data) <- c(".rw54", "grgs")
hgch_treemap_CaNu(data)

## Treemap 2Vars
## Nested Treemap



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

data <- sampleData("Ca-Ca-Nu-Nu-Nu", nrow = 10)
hgch_scatter_CaCaNuNuNu(data)





# Heatmap

