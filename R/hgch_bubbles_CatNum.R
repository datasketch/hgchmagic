#' Bubbles Chart Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Num, Yea-Num
#' @examples
#' hgch_bubbles_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export
hgch_bubbles_CatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts)

  d <- l$d

  data <- purrr::map(1:nrow(d), function(z){
    list("name" = d$a[z],
         "value" = d$b[z],
         "color" = as.character(d$..colors[z]))
  })

  global_options(opts$style$format_num_sample)
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
    hc_tooltip(useHTML=TRUE, pointFormat = l$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(theme(opts =  c(l$theme,
                                 cats = "{point.name} <br/>",
                                 bubble_opacity = l$extra$bubble_opacity,
                                 bubble_min = paste0(l$extra$bubble_min, "%"),
                                 bubble_max = paste0(l$extra$bubble_max, "%")))
    )


  hc
}
