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
    ds <<- d %>% filter(group == s)
    dss <- ds %>% select(a, b, ..a_label)
    dss <- ds %>%
      mutate(x = as.numeric(as.POSIXct(as.Date(ds$a, origin = l$min_date)))*1000,
             y = ds$b,
             color = ds$..colors,
             label = ..a_label)
    list(
      name = s,
      color = ds$..colors[1],
      data = transpose(dss)
    )
  })


  h <- highchart() %>%
    hc_chart(type = "line"#,
             # events = list(
             #   load =   JS(
             #     paste0(
             #       "function() {this.renderer.image('",logo_path,"', this.chartWidth - 160, this.chartHeight - 60 , 150, 50).addClass('logo').add();}"
             #     )))
             ) %>%
    hc_xAxis(
      type = 'datetime',
      title = list(text = l$title$y),
      tickInterval= 7 * 24 * 3600, #* 1000, #* 4,
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
    hc_credits(enabled = TRUE, text = l$title$caption %||% "")
  h
}
