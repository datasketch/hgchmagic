#' line Chart Cat Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num, Cat-Yea-Num
#' @examples
#' hgch_line_CatYeaNum(sample_data("Cat-Yea-Num", nrow = 100))
#' @export
hgch_line_CatYeaNum <- function(data, ...){
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

  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = 'line',
             events = list(
               load = add_branding(l$theme)
             )) %>%
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
        allowPointSelect= l$allow_point,
        cursor =  l$cursor,
        events = list(
          click = l$clickFunction
        )
      )) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = l$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_legend(enabled =  l$theme$legend_show) %>%
    hc_add_theme(hgch_theme(opts = l$theme))


  hc
}





#' line Chart Cat Yea
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_line_CatYea(sample_data("Cat-Yea", nrow = 10))
#' @export
hgch_line_CatYea <- hgch_line_CatYeaNum

#' line Chart Yea Cat Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_line_YeaCatNum(sample_data("Yea-Cat-Num", nrow = 10))
#' @export
hgch_line_YeaCatNum <- hgch_line_CatYeaNum

