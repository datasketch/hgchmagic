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
    hc_chart(type = "line"
    ) %>%
    hc_xAxis(
      type = 'datetime',
      tickInterval= 7 * 24 * 3600 * 1000, #* 4,
      labels = list(
        formatter= JS(l$formatter_date)
      )
    ) %>%
    hc_add_series_list(series) %>%
    hc_tooltip(useHTML=TRUE,
               formatter = l$formatter_date_tooltip
    )
    # hc_plotOptions(
    #   series = list(
    #     connectNulls = TRUE
    #   )
    #)

  h
}
