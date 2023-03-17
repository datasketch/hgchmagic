
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hgchmagic

<!-- badges: start -->
<!-- badges: end -->

hgchmagic is a wrapper for highcharter R package. With hgchmagic you can
provide more info in form of list and improve your visualizations.

## Installation

You can install the development version of hgchmagic like so:

``` r
# install.packages("devtools")
remotes::install_github("datasketch/hgchmagic")
```

## Example

This is a basic example which shows you how this packages work:

Let´s load `makeup` package

``` r
library(hgchmagic)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
## basic example code
```

Create a bar char

``` r
# data <- ggplot2::diamonds
# 
# data <- data |> 
#   dplyr::group_by(cut) |> 
#   dplyr::summarise(price = sum(price))
# # data <- dsdataprep::aggregation_data(data = data,
# #                                      agg = "sum",
# #                                      group_var = "cut",
# #                                      to_agg = "price")
# 
# # Define custom properties for the bar chart
# ops <- list(title = "This is a title",
#             subtitle = "This is a subtitle",
#             caption = "A caption? Yes, this is a caption",
#             hor_title = "Categories",
#             ver_title = "Numbers",
#             bar_orientation = "hor")
# 
# hgch_bar(data, 
#          var_cat = "cut", 
#          var_num = "price", 
#          palette_colors = c("#ffa92a"),
#          order = c("Very Good"))
```
