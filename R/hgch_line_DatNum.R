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
  l <- hgchmagic_prep(data, opts = opts)

  d <- l$d
  ds <- NULL
  series <- lapply(unique(d$group), function(s){
    ds <<- d %>% dplyr::filter(group == s)
    dss <- ds %>% dplyr::select(a, b, ..a_label)
    dss <- ds %>%
      dplyr::mutate(x = as.numeric(as.POSIXct(as.Date(ds$a, origin = l$min_date)))*1000,
                    y = ds$b,
                    color = ds$..colors,
                    label = ..a_label)
    list(
      name = s,
      color = ds$..colors[1],
      data = purrr::transpose(dss)
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
    hc_tooltip(useHTML=TRUE,
               formatter = l$formatter_date_tooltip
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme)))
  h
}


#' line Chart Dat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Dat
#' @examples
#' hgch_line_Dat(sampleData("Dat", nrow = 10))
#' @export
hgch_line_Dat <- hgch_line_DatNum
