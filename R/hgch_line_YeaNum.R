#' Line chart Yea Num
#'
#' @description
#' `hgch_line_YeaNum()` Create a highcharter line plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column is a
#' **Year column** and the second must be  a **numeric class column**, or be sure that
#' two firts columns they meet this condition
#' @export
#' @param data A data frame, data frame extension (e.g. a tibble), a
#'   lazy data frame (e.g. from dbplyr or dtplyr), or fringe data (e.g from homodatum).
#' @param ... Read \code{\link[dsvizopts]{chart_viz_options}} a general options summary to configure your hgchmagic plots
#'   and <[`line-options`][hgch_line_options]> which specifically contains the additional arguments
#'   that work only for this type of chart.
#' @family Yea-Num plots
#' @section Ftype:
#' Yea-Num
#' @examples
#' data <- sample_data("Yea-Num", n = 30)
#' hgch_line_YeaNum(data)
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' hgch_line_YeaNum(data, agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Yea-Num-Dat-Yea-Yea", n = 30)
#' hgch_line_YeaNum(data)
#'
#' # calculate percentage
#' hgch_line_YeaNum(data, percentage = TRUE)
#'
#' # numeric format
#' hgch_line_YeaNum(data, percentage = TRUE, format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_line_YeaNum(agg = "mean",
#'              tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
hgch_line_YeaNum <- function(data, ...) {

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, ftype = "Yea-Num")

  d <- l$d

  series <- list(list(
    color = d$..colors[1],
    data = purrr::map(1:nrow(d), function(x) {
      list(y = d[[2]][x],
           name = d$a[x],
           label = d$labels[x]
           )
    })
  ))

  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = 'line',
             events = list(
               load = add_branding(opts$theme)
             )) %>%
    hc_add_series_list(series) %>%
     hc_xAxis(title = list(text = l$title$x),
              type = "category") %>%
    hc_yAxis(title = list(text = l$title$y),
             labels = list(
               formatter = l$formats)
    ) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_plotOptions(
      series = list(
        allowPointSelect= l$allow_point,
        cursor =  l$cursor,
        events = list(
          click = l$clickFunction
        )
      )) %>%
    hc_credits(enabled = TRUE, text = l$titles$caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hgch_theme(opts = l$theme))

  hc
}






