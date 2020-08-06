#' line Chart Cat Dat Numeric
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_line_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export
hgch_line_CatDatNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts)

  d <- l$d
  ds <- NULL
  series <- lapply(unique(d$a), function(s){
    ds <<- d %>% filter(a == s)
    dss <- ds %>% select(a,b, ..b_label)
    dss <- dss %>%
      mutate(x = as.numeric(as.POSIXct(as.Date(ds$b, origin = "2000-01-01")))*1000,
             y = ds$c,
             label = ..b_label)
    list(
      name = s,#"First",
      color = unique(ds$..colors),
      data = transpose(dss)
    )
  })

  h <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = "line",
             events = list(
               load = add_branding(l$theme)
             )
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
               formatter = l$formatter_date_tooltip
    ) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_legend(enabled = l$theme$legend_show) %>%
    hc_add_theme(theme(opts =  c(l$theme)))

  h
}


#' line Chart Cat Dat
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Dat
#' @examples
#' hgch_line_CatDat(sampleData("Cat-Dat", nrow = 10))
#' @export
hgch_line_CatDat <- hgch_line_CatDatNum
