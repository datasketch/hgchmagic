
library(devtools)
load_all()
document()
install()

library(hgchmagic)

# Multilines

data <- sampleData("Ye-Nu-Nu",nrow = 11)
hgch_multilines_ynp(data)
data <- sampleData("Ye-Nu-Nu-Nu-Nu-Nu",nrow = 11)
hgch_multilines_ynp(data)

# Bars
data <- sampleData("Ca-Ye-Nu",nrow = 10)
hgch_bar_cyn(data)

# Lines
data <- sampleData("Ca-Ye-Nu",nrow = 10)
hgch_line_cyn(data)
hgch_line_cyn(data, symbol = "square")

# Treemap
data <- sampleData("Ca-Nu",nrow = 10)
hgch_treemap_cn(data)

## Treemap 2Vars
## Nested Treemap

# Bar hor
data <- sampleData("Ca-Nu", nrow = 10)
hgch_bar_hor_cn(data)
hgch_bar_hor_top_cn(data)

data <- sampleData("Ca", nrow = 10)
hgch_bar_hor_c(data)
hgch_bar_hor_top_c(data)


# Spider
data <- sampleData("Ca-Nu", nrow = 10)
hgch_spider_cn(data)

data <- sampleData("Ca-Nu-Nu", nrow = 10)
hgch_spider_cnn(data)

# Scatter

# Pie

# Heatmap
