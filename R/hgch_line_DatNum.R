#' Line chart Dat Num
#'
#' @description
#' `hgch_line_DatNum()` Create a highcharter line plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column is a
#' **date column** and the second must be a **numeric class column**, or be sure that
#' two firts columns they meet this condition
#' @export
#' @inheritParams hgch_line_YeaNum
#' @family Dat-Num plots
#' @section Ftype:
#' Dat-Num
#' @examples
#' data <- sample_data("Dat-Num", n = 30)
#' hgch_line_DatNum(data)
#'
#' hgch_line_DatNum(data)
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' hgch_line_DatNum(data, agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Dat-Num-Cat-Cat-Cat-Num", n = 30)
#' hgch_line_DatNum(data)
#'
#' # calculate percentage
#' hgch_line_DatNum(data, percentage = TRUE)
#'
#' # numeric format
#' hgch_line_DatNum(data, percentage = TRUE, format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_line_DatNum(agg = "mean",
#'              tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
#'
hgch_line_DatNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "line", ftype = "Dat-Num")

  d <- l$d
  ds <- NULL

  series <- lapply(unique(d$..group), function(s){
    ds <<- d %>% filter(..group == s)
    dss <- ds[,c(1, 2, 4)]
    dss <- ds %>%
      mutate(x = ds$a,
             y = ds[[2]],
             color = ds$..colors,
             label = labels)

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
    hc_tooltip(useHTML = TRUE,
               headerFormat = NULL,
               formatter = JS(paste0("function () {return this.point.label;}"))
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme)))
  h
}


