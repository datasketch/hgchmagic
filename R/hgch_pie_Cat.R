#' Pie chart Cat
#'
#' @description
#' `hgch_pie_Cat()` Create a highcharter pie plot based on a particular data type.
#' In this case, you can load data with only one **categorical column** or be sure of
#' the first column of the dataframe its categorical since it will be done a
#' counting the categories of this column
#' @export
#' @param data A data frame, data frame extension (e.g. a tibble), a
#'   lazy data frame (e.g. from dbplyr or dtplyr), or fringe data (e.g from homodatum).
#' @param ... <[`chart-options`][hgch_viz_options]> Options to configure your hgchmagic plots
#' @family Cat plots
#' @section Ftype:
#' Cat
#' @examples
#' data <- sample_data("Cat", n = 30)
#' hgch_pie_Cat(data)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Num-Dat-Cat-Cat", n = 30)
#' hgch_pie_Cat(data)
#'
#' # calculate percentage
#' hgch_pie_Cat(data, percentage = TRUE)
#'
#' # You can call the count and percentage in the tooltip plot
#' data %>%
#' hgch_pie_Cat(tooltip = "Count: {Count} <br/> Percentage: {%}%")
#'
hgch_pie_Cat <- function(data, ...) {

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "pie", ftype = "Cat")

  d <- l$d

  data <- list()
  h <- purrr::map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "y" = d[[2]][z],
                            "label" = d$labels[z],
                            "color" = as.character(d$..colors[z]))
  })

  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = "pie",
             events = list(
               load = add_branding(l$theme)
             )) %>%
    hc_series(
      data
    ) %>%
    hc_plotOptions(
      series = list(
        allowPointSelect= l$allow_point,
        cursor =  l$cursor,
        events = list(
          click = l$clickFunction
        )
      )) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme,
                                      cats = "{point.name} <br/>")))

  hc
}


#' Pie chart Yea
#'
#' @description
#' `hgch_pie_Yea()` Create a highcharter pie plot based on a particular data type.
#' In this case, you can load data with only one **Year column** or be sure of
#' the first column of the dataframe its a year column since it will be done a
#' counting the years of this column
#' @export
#' @inheritParams hgch_pie_Cat
#' @family Yea plots
#' @section Ftype:
#' Yea
#' @examples
#' data <- sample_data("Yea", n = 30)
#' hgch_pie_Yea(data)
#'
#' # data with more of one column
#' data <- sample_data("Yea-Num-Dat-Yea-Yea", n = 30)
#' hgch_pie_Yea(data)
#'
#' # calculate percentage
#' hgch_pie_Yea(data, percentage = TRUE)
#'
#' # You can call the count and percentage in the tooltip plot
#' data %>%
#' hgch_pie_Yea(tooltip = "Count: {Count} <br/> Percentage: {%}%")
#'
hgch_pie_Yea <- hgch_pie_Cat
