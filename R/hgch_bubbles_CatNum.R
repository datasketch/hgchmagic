#' Bubbles chart Cat Num
#'
#' @description
#' `hgch_bubbles_CatNum()` Create a highcharter bubbles plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column is a
#' **categorical column** and the second must be  a **numeric class column**, or be sure that
#' two firts columns they meet this condition
#' @export
#' @param data A data frame, data frame extension (e.g. a tibble), a
#'   lazy data frame (e.g. from dbplyr or dtplyr), or fringe data (e.g from homodatum).
#' @param ... Read \code{\link[dsvizopts]{chart_viz_options}} a general options summary to configure your hgchmagic plots
#'   and <[`bubbles-options`][hgch_bubbles_options]> which specifically contains the additional arguments
#'   that work only for this type of chart.
#' @section Ftype:
#' Cat-Num
#' @examples
#' data <- sample_data("Cat-Num", n = 30)
#' hgch_bubbles_CatNum(data)
#'
#' example with iris data
#' data <- iris %>% select(Species, Petal.Width)
#' # this plot show the sum of petal width by species
#' hgch_bubbles_CatNum(data)
#' # if you want to calculate the average instead of the sum,
#' # you can use "agg" param inside a function
#' hgch_bubbles_CatNum(data, agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Cat-Num-Dat-Cat-Cat", n = 30)
#' hgch_bubbles_CatNum(data)
#' # Active data labels
#' hgch_bubbles_CatNum(data,
#'                     dataLabels_show = TRUE)
#'
#' # calculate percentage and change numeric format
#' hgch_bubbles_CatNum(data,
#'                     percentage = TRUE,
#'                     format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_bubbles_CatNum(agg = "mean",
#'                     tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
hgch_bubbles_CatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- hgchmagic_prep(data, opts = opts, plot = "bubbles")

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
    hc_chart(type = 'packedbubble',
             events = list(
               load = add_branding(l$theme)
             )) %>%
    hc_add_series(
      data = data
    ) %>%
    hc_plotOptions(
      packedbubble = list(
        marker= list(
          states = list(
            hover = list(
              brightness= 0.1,
              fillColor = l$color_hover
            ),
            select = list(
              fillColor = l$color_click
            ))
        ),
        allowPointSelect= l$allow_point,
        cursor =  l$cursor,
        events = list(
          click = l$clickFunction
        )
      )
    ) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme,
                                 cats = "{point.name} <br/>",
                                 bubble_opacity = l$extra$bubble_opacity,
                                 bubble_min = paste0(l$extra$bubble_min, "%"),
                                 bubble_max = paste0(l$extra$bubble_max, "%")))
    )


  hc
}

#' Bubbles chart Yea Num
#'
#' @description
#' `hgch_bubbles_YeaNum()` Create a highcharter bubbles plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column is a
#' **Year column** and the second must be  a **numeric class column**, or be sure that
#' two firts columns they meet this condition
#' @export
#' @inheritParams hgch_bubbles_CatNum
#' @family Yea-Num plots
#' @section Ftype:
#' Yea-Num
#' @examples
#' data <- sample_data("Yea-Num", n = 30)
#' hgch_bubbles_YeaNum(data)
#'
#' # if you want to calculate the average instead of the sum,
#' # you can use "agg" param inside a function
#' hgch_bubbles_YeaNum(data, agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Yea-Num-Dat-Yea-Yea", n = 30)
#' hgch_bubbles_YeaNum(data)
#' # Activate data labels
#' hgch_bubbles_YeaNum(data,
#'                     dataLabels_show = TRUE)
#'
#' # calculate percentage and change numeric format
#' hgch_bubbles_YeaNum(data,
#'                     percentage = TRUE,
#'                     format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_bubbles_YeaNum(agg = "mean",
#'                     tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
hgch_bubbles_YeaNum <- hgch_bubbles_CatNum
