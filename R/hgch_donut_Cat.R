#' Donut chart Cat
#'
#' @description
#' `hgch_donut_Cat()` Create a highcharter donut plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column is a
#' **categorical column** and the second must be  a **numeric class column**, or be sure that
#' two firts columns they meet this condition
#' @export
#' @inheritParams hgch_pie_CatNum
#' @family Cat plots
#' @section Ftype:
#' Cat
#' @examples
#' data <- sample_data("Cat", n = 30)
#' hgch_donut_Cat(data)
#'
#' example with iris data
#' data <- iris %>% select(Species, Petal.Width)
#' # this plot show the sum of petal width by species
#' hgch_donut_Cat(data, color_by = "Species")
#' # if you want to calculate the average instead of the sum,
#' # you can use "agg" param inside a function
#' hgch_donut_Cat(data, agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Cat-Dat-Cat-Cat", n = 30)
#' hgch_donut_Cat(data)
#'
#' # calculate percentage
#' hgch_donut_Cat(data, percentage = TRUE)
#'
#' # numeric format
#' hgch_donut_Cat(data, percentage = TRUE, format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_donut_Cat(agg = "mean",
#'                 tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
hgch_donut_Cat <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "donut", ftype = "Cat")

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
      series = list(innerSize = "60%",
                    allowPointSelect= l$allow_point,
                    cursor =  l$cursor,
                    events = list(
                      click = l$clickFunction
                    ))) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme,
                                      cats = "{point.name} <br/>")))

  hc

}


#' Donut chart Yea
#'
#' @description
#' `hgch_donut_Yea()` Create a highcharter donut plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column is a
#' **categorical column** and the second must be  a **numeric class column**, or be sure that
#' two firts columns they meet this condition
#' @export
#' @inheritParams hgch_pie_CatNum
#' @family Yea plots
#' @section Ftype:
#' Yea
#' @examples
#' data <- sample_data("Yea", n = 30)
#' hgch_donut_Yea(data)
#'
#' example with iris data
#' data <- iris %>% select(Species, Petal.Width)
#' # this plot show the sum of petal width by species
#' hgch_donut_Yea(data, color_by = "Species")
#' # if you want to calculate the average instead of the sum,
#' # you can use "agg" param inside a function
#' hgch_donut_Yea(data, agg = "mean")
#'
#' # data with more of one column
#' data <- sample_data("Yea-Dat-Cat-Cat", n = 30)
#' hgch_donut_Yea(data)
#'
#' # calculate percentage
#' hgch_donut_Yea(data, percentage = TRUE)
#'
#' # numeric format
#' hgch_donut_Yea(data, percentage = TRUE, format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_donut_Yea(agg = "mean",
#'                 tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
hgch_donut_Yea <- hgch_donut_Cat
