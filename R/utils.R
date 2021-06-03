
`%||%` <- function (x, y) {
  suppressWarnings({
    if (is.empty(x))
      return(y)
    else if (is.null(x) || is.na(x))
      return(y)
    else if (class(x) == "character" && all(nchar(x) == 0))
      return(y)
    else x
  })
}

is.empty <- function (x){
  if (length(x) == 0)
    return(TRUE)
  if (length(x) == 1 && nchar(x) == 0)
    return(TRUE)
  !as.logical(length(x))
}

#' @importFrom highcharter highchartOutput
#' @importFrom highcharter renderHighchart
#' @importFrom highcharter highchart
#' @importFrom highcharter hc_title
#' @importFrom highcharter hc_subtitle
#' @importFrom highcharter hc_chart
#' @importFrom highcharter hc_series
#' @importFrom highcharter hc_add_series
#' @importFrom highcharter hc_add_series_list
#' @importFrom highcharter hc_xAxis
#' @importFrom highcharter hc_yAxis
#' @importFrom highcharter hc_plotOptions
#' @importFrom highcharter hc_tooltip
#' @importFrom highcharter hc_credits
#' @importFrom highcharter hc_legend
#' @importFrom highcharter hc_add_theme
#' @importFrom highcharter hc_plotOptions
#' @importFrom highcharter hc_colorAxis
#' @importFrom highcharter hc_mapNavigation
#' @importFrom highcharter hc_add_dependency
#' @importFrom highcharter hcmap
#' @importFrom highcharter JS

#' @importFrom dplyr %>%

#' @import dsvizprep
#' @importFrom dsvizprep agg
NULL



