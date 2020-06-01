#' line Chart Yea Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Yea-Num, Yea-Num
#' @examples
#' hgch_line_YeaNum(sample_data("Yea-Num", nrow = 10))
#' @export
hgch_line_YeaNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts)

  d <- l$d

  h <- purrr::map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "y" = as.numeric(d$b[z]),
                            "color" = as.character(d$..colors[z]))
  })

  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = 'line',
             events = list(
               load = add_branding(opts$theme)
             )) %>%
    hc_series(
      data
    ) %>%
    hc_xAxis(title = list(text = l$title$x),
             type = "category") %>%
    hc_yAxis(title = list(text = l$title$y),
             labels = list(
               formatter = l$formats)
    ) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = l$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = l$titles$caption) %>%
    hc_legend(enabled = FALSE) #%>%
    hc_add_theme(theme(opts = l$theme))

  hc
}



#' line Chart Yea
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Yea
#' @examples
#' hgch_line_Yea(sample_data("Yea", nrow = 10))
#' @export
hgch_line_Yea <- hgch_line_YeaNum



