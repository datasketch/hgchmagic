#' Scatter Chart Cat Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num, Cat-Yea-Num
#' @examples
#' hgch_scatter_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export
hgch_scatter_NumNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)

  f <- homodatum::fringe(data)
  nms <- getFringeLabels(f)
  d <- getFringeDataFrame(f)

  labelsXY <- labelsXY(hor_title = opts$title$hor_title %||% nms[1],
                       ver_title = opts$title$ver_title %||% nms[2],
                       nms = nms, orientation = 'ver')

  hor_title <- as.character(labelsXY[1])
  ver_title <- as.character(labelsXY[2])
  opts$theme$text_show <- FALSE

  data_list <- map(1:nrow(d), function(z) {
    list(d$a[z], d$b[z])
  })

  format_num <- format_hgch(opts$style$format_num_sample, "")
  if (is.null(opts$tooltip)) {
    opts$tooltip <- paste0('<b>', nms[1], ':</b> ',
                           paste0(opts$scatter$prefix_x,'{point.x',format_num, '}', opts$scatter$suffix_x), '<br/>',
                           '<b>', nms[2], ':</b> ',
                           paste0(opts$scatter$prefix_y,'{point.y', format_num, '}', opts$scatter$suffix_y))
  }

  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = opts$title$title) %>%
    hc_subtitle(text = opts$title$subtitle) %>%
    hc_chart(
      type = 'scatter',
      zoomType = 'xy',
      events = list(
        load = add_branding(opts$theme)
      )
    ) %>%
    hc_add_dependency("plugins/highcharts-regression.js")  %>%
    hc_add_series(
      regression = opts$scatter$regression,
      regressionSettings = list(
        color = opts$scatter$regression_color,
        hideInLegend = ifelse(opts$scatter$regression_equation, FALSE, TRUE)
      ),
      data = data_list,
      showInLegend = F
    ) %>%
    hc_tooltip(useHTML=TRUE,
               pointFormat = opts$tooltip, headerFormat = NULL) %>%
    hc_xAxis(
      title = list(text = hor_title),
      labels = list(
        formatter = makeup::makeup_format_js(sample = opts$style$format_num_sample,
                                             locale = opts$style$locale,
                                             prefix = opts$scatter$prefix_x,
                                             suffix = opts$scatter$suffix_x))
    ) %>%
    hc_yAxis(
      title = list(text = ver_title),
      labels = list(
        formatter = makeup::makeup_format_js(sample = opts$style$format_num_sample,
                                             locale = opts$style$locale,
                                             prefix = opts$scatter$prefix_y,
                                             suffix = opts$scatter$suffix_y))
    ) %>%
    hc_add_theme(theme(opts = c(opts$theme)))
  hc
}
