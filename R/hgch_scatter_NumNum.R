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

  l <- hgchmagic_prep(data, opts = opts, plot = "scatter", ftype = "Num-Num")

  d <- l$d


  data_list <- map(1:nrow(d), function(z) {
    list(x = d$a[z],
         y = d$b[z],
         color = d$..colors[z],
         label = d$labels[z])
  })

  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(
      type = 'scatter',
      zoomType = 'xy',
      events = list(
        load = add_branding(opts$theme)
      )
    ) %>%
    hc_add_dependency("plugins/highcharts-regression.js")  %>%
    hc_add_series(
      regression = opts$extra$scatter_regression,
      regressionSettings = list(
        color = opts$extra$scatter_regression_color,
        hideInLegend = ifelse(opts$extra$scatter_regression_equation, FALSE, TRUE)
      ),
      data = data_list,
      showInLegend = F
    ) %>%
    hc_tooltip(useHTML=TRUE, formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_xAxis(
      title = list(text = l$titles$x),
      labels = list(
        formatter = makeup::makeup_format_js(sample = opts$extra$scatter_format_num_sample_x,
                                             locale = opts$style$locale,
                                             prefix = opts$extra$scatter_prefix_x,
                                             suffix = opts$extra$scatter_suffix_x))
    ) %>%
    hc_yAxis(
      title = list(text = l$titles$y),
      labels = list(
        formatter = makeup::makeup_format_js(sample = opts$extra$scatter_format_num_sample_y,
                                             locale = opts$style$locale,
                                             prefix = opts$extra$scatter_prefix_y,
                                             suffix = opts$extra$scatter_suffix_y))
    ) %>%
  hc_credits(enabled = TRUE, text = l$title$caption) %>%
     hc_add_theme(hgch_theme(opts = c(l$theme)))
  hc
}
