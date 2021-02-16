#' scatter Chart Cat Dat Numeric
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_scatter_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export
hgch_scatter_CatDatNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts)

  d <- l$d
  ds <- NULL
  series <- lapply(unique(d$a), function(s){
    ds <<- d %>% dplyr::filter(a == s)
    dss <- ds %>% dplyr::select(a,b, ..b_label)
    dss <- dss %>%
      dplyr::mutate(x = as.numeric(as.POSIXct(as.Date(ds$b, origin = l$min_date)))*1000,
                    y = ds$c,
                    label = ..b_label)
    list(
      name = s,#"First",
      color = unique(ds$..colors),
      data = purrr::transpose(dss)
    )
  })


  h <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = "scatter",
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
    hc_add_theme(hgch_theme(opts =  c(l$theme)))

  h
}



#' scatter Chart Cat Dat
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Dat
#' @examples
#' hgch_scatter_CatDat(sampleData("Cat-Dat", nrow = 10))
#' @export
hgch_scatter_CatDat <- hgch_scatter_CatDatNum
