#' Bar chart Cat Num
#'
#' @description
#' `hgch_bar_CatNum()` Create a highcharter bar plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column is a
#' **categorical column** and the second must be  a **numeric class column**, or be sure that
#' two firts columns they meet this condition
#' @export
#' @param data A data frame, data frame extension (e.g. a tibble), a
#'   lazy data frame (e.g. from dbplyr or dtplyr), or fringe data (e.g from homodatum).
#' @param ... Read \code{\link[dsvizopts]{chart_viz_options}} a general options summary to configure your hgchmagic plots
#'   and <[`bar-options`][hgch_bar_options]> which specifically contains the additional arguments
#'   that work only for this type of chart.
#' @family Cat-Num plots
#' @section Ftype:
#' Cat-Num
#' @examples
#' data <- sample_data("Cat-Num", n = 30)
#' hgch_bar_CatNum(data)
#'
#' example with iris data
#' data <- iris %>% select(Species, Petal.Width)
#' # this plot show the sum of petal width by species
#' hgch_bar_CatNum(data)
#' # if you want to calculate the average instead of the sum,
#' # you can use "agg" param inside a function
#' hgch_bar_CatNum(data, agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Cat-Num-Dat-Cat-Cat", n = 30)
#' hgch_bar_CatNum(data)
#'
#' # calculate percentage
#' hgch_bar_CatNum(data, percentage = TRUE)
#'
#' # numeric format
#' hgch_bar_CatNum(data, percentage = TRUE, format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_bar_CatNum(agg = "mean",
#'                 tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
hgch_bar_CatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, ftype = "Cat-Num")
  d <- l$d
  l$theme$legend_show <- FALSE

  h <- purrr::map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "y" = as.numeric(d[[2]][z]),
                            "label" = d$labels[z],
                            "color" = as.character(d$..colors[z]))
  })

  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = ifelse(l$orientation == "hor","bar","column"),
             events = list(
               load = add_branding(opts$theme)
             )
             ) %>%
    hc_series(
      data
    ) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_xAxis(title = list(text = l$titles$x),
             type = "category") %>%
    hc_yAxis(title = list(text = l$titles$y),
             labels = list(
               formatter = l$formats)
    ) %>%
    hc_plotOptions(
      series = list(
        borderWidth = 0,
        pointPadding = l$theme$bar_padding,
        groupPadding = l$theme$bar_groupWidth,
        pointWidth = l$theme$bar_pointWidth,
        states = list(
          hover = list(
            brightness= 0.1,
            color = l$color_hover
          ),
          select = list(
            color = l$color_click
          )
        ),
        allowPointSelect= l$allow_point,
        cursor =  l$cursor,
        events = list(
          click = l$clickFunction
        )
      )) %>%
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hgch_theme(opts = l$theme))

  hc
}





#' Bar chart Yea Num
#'
#' @description
#' `hgch_bar_YeaNum()` Create a highcharter bar plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column is a
#' **Year column** and the second must be  a **numeric class column**, or be sure that
#' two firts columns they meet this condition
#' @export
#' @inheritParams hgch_bar_CatNum
#' @family Yea-Num plots
#' @section Ftype:
#' Yea-Num
#' @examples
#' data <- sample_data("Yea-Num", n = 30)
#' hgch_bar_YeaNum(data)
#'
#' example with iris data
#' data <- iris %>% select(Species, Petal.Width)
#' # this plot show the sum of petal width by species
#' hgch_bar_YeaNum(data)
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' hgch_bar_YeaNum(data, agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Yea-Num-Dat-Yea-Yea", n = 30)
#' hgch_bar_YeaNum(data)
#'
#' # calculate percentage
#' hgch_bar_YeaNum(data, percentage = TRUE)
#'
#' # numeric format
#' hgch_bar_YeaNum(data, percentage = TRUE, format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_bar_YeaNum(agg = "mean",
#'              tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
hgch_bar_YeaNum <- hgch_bar_CatNum
