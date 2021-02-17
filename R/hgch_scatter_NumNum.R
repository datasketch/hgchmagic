#' Scatter chart Num Num
#'
#' @description
#' `hgch_scatter_NumNum()` Create a highcharter scatter plot based on a particular data type.
#' In this case, you can load data with only two columns,  where the firts and second columns are
#' **numeric class**, or be sure that two firts columns they meet this condition
#'
#' @export
#' @param data A data frame, data frame extension (e.g. a tibble), a
#'   lazy data frame (e.g. from dbplyr or dtplyr), or fringe data (e.g from homodatum).
#' @param ... Read \code{\link[dsvizopts]{chart_viz_options}} a general options summary to configure your hgchmagic plots
#'   and <[`scatter-options`][hgch_scatter_options]> which specifically contains the additional arguments
#'   that work only for this type of chart.
#' @family Num-Num plots
#' @section Ftype:
#' Num-Num
#' @examples
#' data <- sample_data("Num-Num", n = 30)
#' hgch_scatter_NumNum(data)
#'
#' example with iris data
#' data <- iris %>% select(Petal.Width, Petal.Length)
#' hgch_scatter_NumNum(data)
#'
#' # linear regression
#' hgch_scatter_NumNum(data, scatter_regression = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Num-Num-Dat-Cat-Cat", n = 30)
#' hgch_scatter_NumNum(data)
#'

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
