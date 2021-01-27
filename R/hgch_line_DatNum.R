#' line Chart Dat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Dat-Num
#' @examples
#' hgch_line_DatNum(sampleData("Dat-Num", nrow = 10))
#' @export
hgch_line_DatNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "line", ftype = "Dat-Num")

  d <- l$d
  ds <- NULL
  series <- lapply(unique(d$..group), function(s){
    ds <<- d %>% filter(..group == s)
    dss <- ds[,c(1, 2, 4)]
    dss <- ds %>%
      mutate(x = ds$a,
             y = ds[[2]],
             color = ds$..colors,
             label = labels)
    list(
      name = s,
      color = ds$..colors[1],
      data = transpose(dss)
    )
  })


  h <- highchart() %>%
    hc_chart(type = "line",
             events = list(
               load = add_branding(l$theme)
             )
             ) %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
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
    hc_tooltip(useHTML = TRUE,
               headerFormat = NULL,
               formatter = JS(paste0("function () {return this.point.label;}"))
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme)))
  h
}


