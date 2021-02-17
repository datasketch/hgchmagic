#' Scatter chart Dat Num
#'
#' @description
#' `hgch_scatter_DatNum()` Create a highcharter scatter plot based on a particular data type.
#' In this case, you can load data with only two columns,  where the firts is a
#' **date column**, and second is a **numeric column**, or be sure that two firts columns they meet this condition
#'
#' @export
#' @inheritParams hgch_scatter_NumNum
#' @family Dat-Num plots
#' @section Ftype:
#' Dat-Num
#' @examples
#' data <- sample_data("Dat-Num", n = 30)
#' hgch_scatter_DatNum(data)
#'
#' # data with more of one column
#' data <- sample_data("Dat-Num-Dat-Cat-Cat", n = 30)
#' hgch_scatter_DatNum(data)
#'
hgch_scatter_DatNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "scatter", ftype = "Dat-Num")

  d <- l$d
  ds <- NULL

  series <- lapply(unique(d$..group), function(s){
    ds <<- d %>% filter(..group == s)
    dss <- ds[,c(1, 2, 4)]
    dss <- ds %>%
      mutate(x = ds$a,
             y = ds[[2]],
             color = ds$..colors,
             label = labels)    list(
      name = s,
      color = ds$..colors[1],
      data = purrr::transpose(dss)
    )
  })



  h <- highchart() %>%
    hc_chart(type = "scatter"
    ) %>%
    hc_xAxis(
      type = 'datetime',
      title = list(text = l$title$x),
      tickInterval= l$date_intervals,
      labels = list(
        formatter= JS(l$formatter_date)
      )
    ) %>%
    hc_yAxis(title = list(text = l$title$y),
             labels = list(
               formatter = l$formats)
    ) %>%
    hc_add_series_list(series) %>%
    hc_tooltip(useHTML=TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme)))
  h
}
