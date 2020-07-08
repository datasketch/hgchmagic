#' area Chart Yea Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Yea-Num, Yea-Num
#' @examples
#' hgch_area_YeaNum(sample_data("Yea-Num", nrow = 10))
#' @export
hgch_area_YeaNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts)

  d <- l$d

  series <- list(list(
    data = purrr::map(1:nrow(d), function(x) {
      list(y = d$b[x],
           name = d$a[x])
    })
  ))

  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = 'area',
             events = list(
               load = add_branding(opts$theme)
             )) %>%
    hc_add_series_list(series) %>%
    hc_xAxis(title = list(text = l$title$x),
             type = "category") %>%
    hc_yAxis(title = list(text = l$title$y),
             labels = list(
               formatter = l$formats)
    ) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = l$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = l$titles$caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(theme(opts = l$theme))
  hc
}



#' area Chart Yea
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Yea
#' @examples
#' hgch_area_Yea(sample_data("Yea", nrow = 10))
#' @export
hgch_area_Yea <- hgch_area_YeaNum



