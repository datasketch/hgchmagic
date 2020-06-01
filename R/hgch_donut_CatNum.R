#' donut Chart Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Num, Yea-Num
#' @examples
#' hgch_donut_CatNum(sample_data("Cat-Num", nrow = 10))
#' @export
hgch_donut_CatNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "donut")

  d <- l$d

  data <- list()
  h <- purrr::map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "y" = d$b[z],
                            "color" = as.character(d$..colors[z]))
  })

  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = "pie",
             events = list(
               load = add_branding(l$theme)
             )) %>%
    hc_series(
      data
    ) %>%
    hc_plotOptions(
      series = list(innerSize = "60%")) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = l$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_add_theme(theme(opts =  c(l$theme,
                                 cats = "{point.name} <br/>")))

  hc

}



#' donut Chart Cat
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat, Yea
#' @examples
#' hgch_donut_Cat(sample_data("Cat", nrow = 10))
#' @export
hgch_donut_Cat <- hgch_donut_CatNum



#' donut Chart Yea
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Yea, Yea
#' @examples
#' hgch_donut_Yea(sample_data("Yea", nrow = 10))
#' @export
hgch_donut_Yea <- hgch_donut_CatNum

#' donut Chart Yea Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Yea, Yea
#' @examples
#' hgch_donut_YeaNum(sample_data("Yea-Num", nrow = 10))
#' @export
hgch_donut_YeaNum <- hgch_donut_CatNum
