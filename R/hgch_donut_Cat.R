#' donut Chart Cat
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat, Yea-Num
#' @examples
#' hgch_donut_Cat(sample_data("Cat", nrow = 10))
#' @export
hgch_donut_Cat <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "donut", ftype = "Cat")

  d <- l$d

  data <- list()
  h <- purrr::map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "y" = d[[2]][z],
                            "label" = d$labels[z],
                            "color" = as.character(d$..colors[z]))
  })

  global_options(opts$style$format_sample_num)
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
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme,
                                      cats = "{point.name} <br/>")))

  hc

}


#' donut Chart Yea
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Yea, Yea
#' @examples
#' hgch_donut_Yea(sample_data("Yea", nrow = 10))
#' @export
hgch_donut_Yea <- hgch_donut_Cat
