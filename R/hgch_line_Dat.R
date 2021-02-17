#' Line chart Dat
#'
#' @description
#' `hgch_line_Dat()` Create a highcharter line plot based on a particular data type.
#' In this case, you can load data with only one **date column** or be sure of
#' the first column of the dataframe its categorical since it will be done a
#' counting the categories of this column
#' @export
#' @inheritParams hgch_line_YeaNum
#' @family Dat plots
#' @section Ftype:
#' Dat
#' @examples
#' data <- sample_data("Dat", n = 30)
#' hgch_line_Dat(data)
#'
#' # data with more of one column
#' data <- sample_data("Dat-Num-Dat-Dat-Dat", n = 30)
#' hgch_line_Dat(data)
#'
#' # calculate percentage
#' hgch_line_Dat(data, percentage = TRUE)
#'
#' # You can call the count and percentage in the tooltip plot
#' data %>%
#' hgch_line_Dat(tooltip = "Count: {Count} <br/> Percentage: {%}%")
hgch_line_Dat <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "line", ftype = "Dat")

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
      data = transpose(dss)
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


