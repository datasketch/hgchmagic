#' Treemap chart Cat Num
#'
#' @description
#' `hgch_treemap_CatNum()` Create a highcharter treemap plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column is a
#' **categorical column** and the second must be  a **numeric class column**, or be sure that
#' two firts columns they meet this condition
#' @export
#' @param data A data frame, data frame extension (e.g. a tibble), a
#'   lazy data frame (e.g. from dbplyr or dtplyr), or fringe data (e.g from homodatum).
#' @param ... <[`chart-options`][hgch_viz_options]> Options to configure your hgchmagic plots
#' @family Cat-Num plots
#' @section Ftype:
#' Cat-Num
#' @examples
#' data <- sample_data("Cat-Num", n = 30)
#' hgch_treemap_CatNum(data)
#'
#' example with iris data
#' data <- iris %>% select(Species, Petal.Width)
#' # this plot show the sum of petal width by species
#' hgch_treemap_CatNum(data)
#' # if you want to calculate the average instead of the sum,
#' # you can use "agg" param inside a function
#' hgch_treemap_CatNum(data, agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Cat-Num-Dat-Cat-Cat", n = 30)
#' hgch_treemap_CatNum(data)
#' # Active data labels
#' hgch_treemap_CatNum(data,
#'                     dataLabels_show = TRUE)
#' # position and dimensions to points
#' hgch_treemap_CatNum(data, treemap_layout = "stripes")
#'
#' # calculate percentage and change numeric format
#' hgch_treemap_CatNum(data,
#'                     percentage = TRUE,
#'                     format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_treemap_CatNum(agg = "mean",
#'                     tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
hgch_treemap_CatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "treemap")
  l$theme$legend_show <- FALSE
  d <- l$d


  data <- purrr::map(1:nrow(d), function(z){
    list("name" = d$a[z],
         "value" = d[[2]][z],
         "label" = d$labels[z],
         "color" = as.character(d$..colors[z]))
  })

  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(
      events = list(
        load = add_branding(l$theme)
      )) %>%
    hc_plotOptions(
      series = list(
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
    hc_series(
      list(
        type = 'treemap',
        layoutAlgorithm = l$extra$treemap_layout,
        layoutStartingDirection = l$extra$treemap_direction,
        data = data)) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_credits(enabled = TRUE, text = opts$title$caption %||% "") %>%
    hc_legend(enabled = F) %>%
    hc_add_theme(hgch_theme(opts = c(l$theme,
                                     cats = "{point.name} <br/>")))

  hc
}


#' Treemap chart Yea Num
#'
#' @description
#' `hgch_treemap_YeaNum()` Create a highcharter treemap plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column is a
#' **Year column** and the second must be  a **numeric class column**, or be sure that
#' two firts columns they meet this condition
#' @export
#' @inheritParams hgch_treemap_CatNum
#' @family Yea-Num plots
#' @section Ftype:
#' Yea-Num
#' @examples
#' data <- sample_data("Yea-Num", n = 30)
#' hgch_treemap_YeaNum(data)
#'
#' example with iris data
#' data <- iris %>% select(Species, Petal.Width)
#' # this plot show the sum of petal width by species
#' hgch_treemap_YeaNum(data)
#' # if you want to calculate the average instead of the sum,
#' # you can use "agg" param inside a function
#' hgch_treemap_YeaNum(data, agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Yea-Num-Dat-Yea-Yea", n = 30)
#' hgch_treemap_YeaNum(data)
#' # Activate data labels
#' hgch_treemap_YeaNum(data,
#'                     dataLabels_show = TRUE)
#' # position and dimensions to points
#' hgch_treemap_YeaNum(data, treemap_layout = "stripes")
#'
#' # calculate percentage and change numeric format
#' hgch_treemap_YeaNum(data,
#'                     percentage = TRUE,
#'                     format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_treemap_YeaNum(agg = "mean",
#'                     tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
hgch_treemap_YeaNum <- hgch_treemap_CatNum
