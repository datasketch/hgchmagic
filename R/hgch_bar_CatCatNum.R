#' Bar Chart Cat Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num, Cat-Yea-Num
#' @examples
#' hgch_bar_CatCatNum(sample_data("Cat-Cat-Num", nrow = 10))
#' @export
hgch_bar_CatCatNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts)

  d <- l$d

  series <- purrr::map(unique(d[[1]]), function(i) {
    d0 <- d %>%
      dplyr::filter(a %in% i)
    l0 <- list("name" = i,
               "color" = unique(d0$..colors),
               "data" = d0$c)
  })

  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type =  ifelse(l$orientation == "hor","bar","column"),
             events = list(
               load = add_branding(l$theme)
             )
             ) %>%
    hc_add_series_list(series) %>%
    hc_xAxis(title = list(text = l$title$x),
             categories = purrr::map(as.character(unique(d$b)), function(z) z),
             type = "category") %>%
    hc_yAxis(title = list(text = l$title$y),
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
            #//brightness: -0.5,
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
     hc_tooltip(useHTML=TRUE, pointFormat = l$tooltip, headerFormat = NULL) %>%
     hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_legend(enabled = l$theme$legend_show) %>%
    hc_add_theme(theme(opts = l$theme))

  if (l$graph_type == "stacked"){
    hc <- hc %>% hc_plotOptions(bar = list(stacking = "normal"), column = list(stacking = "normal"))
    if (l$percentage) {
      hc <- hc %>% hc_yAxis(maxRange = 100,
                            max = 100)
    }
  }

  hc
}



#' bar Chart Cat Cat Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bar_CatCat(sample_data("Cat-Cat", nrow = 10))
#' @export
hgch_bar_CatCat <- hgch_bar_CatCatNum


#' bar Chart Yea Cat
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bar_YeaCat(sample_data("Yea-Cat", nrow = 10))
#' @export
hgch_bar_YeaCat <- hgch_bar_CatCatNum


#' bar Chart Cat Yea
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bar_CatYea(sample_data("Cat-Yea", nrow = 10))
#' @export
hgch_bar_CatYea <- hgch_bar_CatCatNum


#' bar Chart Cat Yea Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bar_CatYeaNum(sample_data("Cat-Yea-Num", nrow = 10))
#' @export
hgch_bar_CatYeaNum <- hgch_bar_CatCatNum


#' bar Chart Yea Cat Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bar_YeaCatNum(sample_data("Yea-Cat-Num", nrow = 10))
#' @export
hgch_bar_YeaCatNum <- hgch_bar_CatCatNum

