#' Treemap chart Cat
#'
#' @description
#' `hgch_treemap_Cat()` Create a highcharter treemap plot based on a particular data type.
#' In this case, you can load data with only one **categorical column** or be sure of
#' the first column of the dataframe its categorical, since it will be done a
#' counting the categories of this column.
#' @export
#' @inheritParams hgch_treemap_CatNum
#' @family Cat plots
#' @section Ftype:
#' Cat
#' @examples
#' data <- sample_data("Cat", n = 30)
#' hgch_treemap_Cat(data)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Num-Dat-Cat-Cat", n = 30)
#' hgch_treemap_Cat(data)
#'
#' # calculate percentage
#' hgch_treemap_Cat(data, percentage = TRUE)
#'
#' # You can call the count and percentage in the tooltip plot
#' data %>%
#' hgch_treemap_Cat(tooltip = "Count: {Count} <br/> Percentage: {%}%")
#'
hgch_treemap_Cat <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")
  data[[1]] <- homodatum::as_Cat(data[[1]])

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "treemap", ftype = "Cat")
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



#' treemap chart Yea
#'
#' @description
#' `hgch_treemap_Yea()` Create a highcharter treemap plot based on a particular data type.
#' In this case, you can load data with only one **Year column** or be sure of
#' the first column of the dataframe its a year column since it will be done a
#' counting the years of this column
#' @export
#' @inheritParams hgch_treemap_CatNum
#' @family Yea plots
#' @section Ftype:
#' Yea
#' @examples
#' data <- sample_data("Yea", n = 30)
#' hgch_treemap_Yea(data)
#'
#' # data with more of one column
#' data <- sample_data("Yea-Num-Dat-Yea-Yea", n = 30)
#' hgch_treemap_Yea(data)
#'
#' # calculate percentage
#' hgch_treemap_Yea(data, percentage = TRUE)
#'
#' # You can call the count and percentage in the tooltip plot
#' data %>%
#' hgch_treemap_Yea(tooltip = "Count: {Count} <br/> Percentage: {%}%")
hgch_treemap_Yea <- hgch_treemap_Cat
