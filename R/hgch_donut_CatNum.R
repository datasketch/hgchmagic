#' Donut chart Cat Num
#'
#' @description
#' `hgch_donut_CatNum()` Create a highcharter donut plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column is a
#' **categorical column** and the second must be  a **numeric class column**, or be sure that
#' two firts columns they meet this condition
#' @export
#' @inheritParams hgch_pie_CatNum
#' @family Cat-Num plots
#' @section Ftype:
#' Cat-Num
#' @examples
#' data <- sample_data("Cat-Num", n = 30)
#' hgch_donut_CatNum(data)
#'
#' example with iris data
#' data <- iris %>% select(Species, Petal.Width)
#' # this plot show the sum of petal width by species
#' hgch_donut_CatNum(data, color_by = "Species")
#' # if you want to calculate the average instead of the sum,
#' # you can use "agg" param inside a function
#' hgch_donut_CatNum(data, agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Cat-Num-Dat-Cat-Cat", n = 30)
#' hgch_donut_CatNum(data)
#'
#' # calculate percentage
#' hgch_donut_CatNum(data, percentage = TRUE)
#'
#' # numeric format
#' hgch_donut_CatNum(data, percentage = TRUE, format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_donut_CatNum(agg = "mean",
#'                 tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
hgch_donut_CatNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "donut")

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
      series = list(innerSize = "60%")) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme,
                                 cats = "{point.name} <br/>")))

  hc

}




#' Donut chart Yea Num
#'
#' @description
#' `hgch_donut_YeaNum()` Create a highcharter donut plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column is a
#' **Year column** and the second must be  a **numeric class column**, or be sure that
#' two firts columns they meet this condition
#' @export
#' @inheritParams hgch_pie_CatNum
#' @family Yea-Num plots
#' @section Ftype:
#' Yea-Num
#' @examples
#' data <- sample_data("Yea-Num", n = 30)
#' hgch_donut_YeaNum(data)
#'
#' example with iris data
#' data <- iris %>% select(Species, Petal.Width)
#' # this plot show the sum of petal width by species
#' hgch_donut_YeaNum(data)
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' hgch_donut_YeaNum(data, agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Yea-Num-Dat-Yea-Yea", n = 30)
#' hgch_donut_YeaNum(data)
#'
#' # calculate percentage
#' hgch_donut_YeaNum(data, percentage = TRUE)
#'
#' # numeric format
#' hgch_donut_YeaNum(data, percentage = TRUE, format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_donut_YeaNum(agg = "mean",
#'              tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
hgch_donut_YeaNum <- hgch_donut_CatNum
