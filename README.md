
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hgchmagic

<!-- badges: start -->
<!-- badges: end -->

hgchmagic is a wrapper for highcharter R package. With hgchmagic you can
provide more info in form of list and improve your visualizations.

The type of charts supported by hgchmagic are:


- `hgch_bar()`
- `hgch_line()`
- `hgch_scatter()`
- `hgch_treemap()`
- `hgch_donut()`
- `hgch_pie()`
- `hgch_sankey()`

Everyone of them assist user with different treatment regarding data
variable types. For example, for charts where categorical variables are
involved, you can use:

- `hgch_bar_Cat()`
- `hgch_line_CatCat()`
- `hgch_scatter_CatDatNum()`

## Installation

You can install the development version of hgchmagic like so:

``` r
# install.packages("devtools")
remotes::install_github("datasketch/hgchmagic")
```
