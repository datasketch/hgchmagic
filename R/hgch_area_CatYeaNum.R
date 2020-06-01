#' area Chart Cat Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num, Cat-Yea-Num
#' @examples
#' hgch_area_CatYeaNum(sample_data("Cat-Yea-Num", nrow = 100))
#' @export
hgch_area_CatYeaNum <- function(data, ...){
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

  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = 'area',
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
    hc_tooltip(useHTML=TRUE, pointFormat = l$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_legend(enabled = TRUE) %>%
    hc_add_theme(theme(opts = l$theme)) %>%
    hc_plotOptions(
      area = list(
        stacking= 'percent',
        lineColor= '#ffffff',
        lineWidth= 1,
        marker = list(
          lineWidth = 1,
          lineColor = '#ffffff'
        )
      )
)

  hc
}





#' area Chart Cat Yea
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_area_CatYea(sample_data("Cat-Yea", nrow = 10))
#' @export
hgch_area_CatYea <- hgch_area_CatYeaNum

#' area Chart Yea Cat Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_area_YeaCatNum(sample_data("Yea-Cat-Num", nrow = 10))
#' @export
hgch_area_YeaCatNum <- hgch_area_CatYeaNum

