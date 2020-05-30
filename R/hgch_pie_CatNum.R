#' Pie Chart Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Num, Yea-Num
#' @examples
#' hgch_pie_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export
hgch_pie_CatNum <- function(data, ...) {

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "pie")

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
    hc_tooltip(useHTML=TRUE, pointFormat = l$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_add_theme(theme(opts =  c(l$theme,
                                 cats = "{point.name} <br/>")))

  hc
}



#' Pie Chart Cat
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat, Yea
#' @examples
#' hgch_pie_Cat(sampleData("Cat", nrow = 10))
#' @export
hgch_pie_Cat <- hgch_pie_CatNum
