#' Line chart Yea Num
#'
#' @description
#' `hgch_line_Yea()` Create a highcharter line plot based on a particular data type.
#' In this case, you can load data with only one column, where this is a
#' **Year column**, or be sure of the first column of the dataframe its a year column,
#' since it will be done a counting the categories of this column
#' @export
#' @inheritParams hgch_line_YeaNum
#' @family Yea plots
#' @section Ftype:
#' Yea
#' @examples
#' data <- sample_data("Yea", n = 300, rep = T)
#' hgch_line_Yea(data)
#'
#' # data with more of one column
#' data <- sample_data("Yea-Dat-Yea-Yea", n = 30, rep = T)
#' hgch_line_Yea(data)
#'
#' # calculate percentage
#' hgch_line_Yea(data, percentage = TRUE)
#'
#' # numeric format
#' hgch_line_Yea(data, percentage = TRUE, format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_line_Yea(tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
#'
hgch_line_Yea <- function(data, ...) {

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, ftype = "Yea")

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
    hc_credits(enabled = TRUE, text = l$titles$caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hgch_theme(opts = l$theme))

  hc
}






