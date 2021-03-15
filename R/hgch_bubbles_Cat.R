#' Bubbles chart Cat
#'
#' @description
#' `hgch_bubbles_Cat()` Create a highcharter bubbles plot based on a particular data type.
#' In this case, you can load data with only one **categorical column** or be sure of
#' the first column of the dataframe its categorical since it will be done a
#' counting the categories of this column
#' @export
#' @inheritParams hgch_bubbles_CatNum
#' @family Cat plots
#' @section Ftype:
#' Cat
#' @examples
#' data <- sample_data("Cat", n = 30)
#' hgch_bubbles_Cat(data)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Num-Dat-Cat-Cat", n = 30)
#' hgch_bubbles_Cat(data)
#'
#' # calculate percentage
#' hgch_bubbles_Cat(data, percentage = TRUE)
#'
#' # You can call the count and percentage in the tooltip plot
#' data %>%
#' hgch_bubbles_Cat(tooltip = "Count: {Count} <br/> Percentage: {%}%")
#'
hgch_bubbles_Cat <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- hgchmagic_prep(data, opts = opts, plot = "bubbles", ftype = "Cat")

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



#' Bubbles chart Yea
#'
#' @description
#' `hgch_bubbles_Yea()` Create a highcharter bubbles plot based on a particular data type.
#' In this case, you can load data with only one **Year column** or be sure of
#' the first column of the dataframe its a year column since it will be done a
#' counting the years of this column
#' @export
#' @inheritParams hgch_bubbles_CatNum
#' @family Yea plots
#' @section Ftype:
#' Yea
#' @examples
#' data <- sample_data("Yea", n = 30)
#' hgch_bubbles_Yea(data)
#'
#' # data with more of one column
#' data <- sample_data("Yea-Num-Dat-Yea-Yea", n = 30)
#' hgch_bubbles_Yea(data)
#'
#' # calculate percentage
#' hgch_bubbles_Yea(data, percentage = TRUE)
#'
#' # You can call the count and percentage in the tooltip plot
#' data %>%
#' hgch_bubbles_Yea(tooltip = "Count: {Count} <br/> Percentage: {%}%")
#'
hgch_bubbles_Yea <- hgch_bubbles_Cat

