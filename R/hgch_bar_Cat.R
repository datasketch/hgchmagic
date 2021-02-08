#' Bar chart Cat
#'
#' @description
#' `hgch_bar_Cat()` Create a highcharter bar plot based on a particular data type.
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
#' hgch_bar_Cat(data)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Num-Dat-Cat-Cat", n = 30)
#' hgch_bar_Cat(data)
#'
#' # calculate percentage
#' hgch_bar_Cat(data, percentage = TRUE)
#'
#' # You can call the count and percentage in the tooltip plot
#' data %>%
#' hgch_bar_Cat(tooltip = "Count: {Count} <br/> Percentage: {%}%")
hgch_bar_Cat <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, ftype = "Cat")
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


#' Bar chart Yea
#'
#' @description
#' `hgch_bar_Yea()` Create a highcharter bar plot based on a particular data type.
#' In this case, you can load data with only one **Year column** or be sure of
#' the first column of the dataframe its a year column since it will be done a
#' counting the years of this column
#' @export
#' @inheritParams hgch_bar_Cat
#' @family Yea plots
#' @section Ftype:
#' Yea
#' @examples
#' data <- sample_data("Yea", n = 30)
#' hgch_bar_Yea(data)
#'
#' # data with more of one column
#' data <- sample_data("Yea-Num-Dat-Yea-Yea", n = 30)
#' hgch_bar_Yea(data)
#'
#' # calculate percentage
#' hgch_bar_Yea(data, percentage = TRUE)
#'
#' # You can call the count and percentage in the tooltip plot
#' data %>%
#' hgch_bar_Yea(tooltip = "Count: {Count} <br/> Percentage: {%}%")
hgch_bar_Yea <- hgch_bar_Cat
