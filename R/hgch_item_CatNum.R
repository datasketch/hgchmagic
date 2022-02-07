#' Item chart Cat Num
#'
#' @description
#' `hgch_item_CatNum()` Create a highcharter item plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts column is a
#' **categorical column** and the second must be  a **numeric class column**, or be sure that
#' two firts columns they meet this condition
#' @export
#' @param data A data frame, data frame extension (e.g. a tibble), a
#'   lazy data frame (e.g. from dbplyr or dtplyr), or fringe data (e.g from homodatum).
#' @param ... Read \code{\link[dsvizopts]{chart_viz_options}} a general options summary to configure your hgchmagic plots
#'   and <[`item-options`][hgch_item_options]> which specifically contains the additional arguments
#'   that work only for this type of chart.
#' @family Cat-Num plots
#' @section Ftype:
#' Cat-Num
#' @examples
#' data <- sample_data("Cat-Num", n = 30)
#' hgch_item_CatNum(data, color_by = names(data)[1])
#'
#' example with iris data
#' data <- iris %>% select(Species, Petal.Width)
#' # this plot show the sum of petal width by species
#' hgch_item_CatNum(data)
#' # if you want to calculate the average instead of the sum,
#' # you can use "agg" param inside a function
#' hgch_item_CatNum(data, agg = "mean", marker_radius = 7)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Num-Dat-Cat-Cat", n = 30)
#' hgch_item_CatNum(data)
#'
#' # calculate percentage
#' hgch_item_CatNum(data, percentage = TRUE)
#'
#' # numeric format
#' hgch_item_CatNum(data, percentage = TRUE, format_sample_num = "1.234,")
#'
#' # You can call the mean and percentage in the tooltip plot
#' num_name <- names(data)[2]
#' data %>%
#' hgch_item_CatNum(agg = "mean",
#'                 tooltip = paste0("Average: {", num_name ,"} <br/> Percentage: {%}%"))
hgch_item_CatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  data[[1]] <- homodatum::as_Cat(data[[1]])

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, ftype = "Cat-Num")
  d <- l$d
  d[[2]] <- scales::rescale(as.numeric(d[[2]]), c(1, 700))

  h <- purrr::map(unique(d[[1]]), function(z){
    d0 <- d %>% filter(a %in% z)
    list("name" = z,
         "y" = as.numeric(d0[[2]]),
         "label" = d0$labels %>% unlist(),
         marker = list(
           symbol= 'point'
         ),
         "color" = as.character(d0$..colors))
  })

  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type= 'item',
             events = list(
               load = add_branding(opts$theme)
             )
    ) %>%
    hc_add_series(
      layout= 'horizontal',
      data = h
    ) %>%
  hc_tooltip(useHTML = TRUE,
             formatter = JS(paste0("function () {return this.point.label;}"))) %>%
  hc_plotOptions(
    series = list(
      # borderWidth = 0,
      # pointPadding = l$theme$item_padding,
      # groupPadding = l$theme$item_groupWidth,
      # pointWidth = l$theme$item_pointWidth,
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
  hc_legend(enabled = l$theme$legend_show) %>%
  hc_add_theme(hgch_theme(opts = l$theme))

  hc
}


