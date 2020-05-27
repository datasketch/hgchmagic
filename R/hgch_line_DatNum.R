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
#print(d)
  series <- list(list(
    data = purrr::map(1:nrow(d), function(x) {
      d$b[x]
    })
  ))
#print(series)
  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = ifelse(l$spline, "spline","line"),
             events = list(
               load = add_branding(l$theme)
             )) %>%
    hc_add_series_list(series) %>%
    hc_xAxis(
      #categories = purrr::map(as.character(unique(d$a)), function(z) z),
      #type = 'category',
      crosshair = list(
        snap = TRUE
      )#,
      # labels = list(
      #   formatter = JS("function() {return Highcharts.dateFormat('%y-%b-%d', (this.value));}")
      # )
    )
  hc
}
