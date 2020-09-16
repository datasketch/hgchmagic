#' Bubbles Chart Cat Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num, Cat-Yea-Num
#' @examples
#' hgch_bubbles_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export
hgch_bubbles_CatCatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "bubbles")

  d <- l$d

  series <- map(unique(d$a), function(x) {
    df <- d %>% filter(a %in% x)
    list(
      name = x,
      data =
        map(1:nrow(df), function (z) {
          list(
            name = df$b[z],
            value = df$c[z],
            color = df$..colors[z]
          )
        })
    )
  })


  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = 'packedbubble',
             events = list(
               load = add_branding(l$theme)
             )) %>%
    hc_add_series_list(series) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = l$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_legend(enabled = l$theme$legend_show) %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme,
                                 cats = "{point.name} <br/>",
                                 bubble_opacity = l$extra$bubble_opacity,
                                 bubble_min = paste0(l$extra$bubble_min, "%"),
                                 bubble_max = paste0(l$extra$bubble_max, "%"))))

  hc

}


#' bubbles Chart Cat Cat Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bubbles_CatCat(sample_data("Cat-Cat", nrow = 10))
#' @export
hgch_bubbles_CatCat <- hgch_bubbles_CatCatNum


#' bubbles Chart Yea Cat
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bubbles_YeaCat(sample_data("Yea-Cat", nrow = 10))
#' @export
hgch_bubbles_YeaCat <- hgch_bubbles_CatCatNum


#' bubbles Chart Cat Yea
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bubbles_CatYea(sample_data("Cat-Yea", nrow = 10))
#' @export
hgch_bubbles_CatYea <- hgch_bubbles_CatCatNum


#' bubbles Chart Cat Yea Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bubbles_CatYeaNum(sample_data("Cat-Yea-Num", nrow = 10))
#' @export
hgch_bubbles_CatYeaNum <- hgch_bubbles_CatCatNum


#' bubbles Chart Yea Cat Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bubbles_YeaCatNum(sample_data("Yea-Cat-Num", nrow = 10))
#' @export
hgch_bubbles_YeaCatNum <- hgch_bubbles_CatCatNum

