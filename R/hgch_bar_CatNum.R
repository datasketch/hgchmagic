#' Bar Chart Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Num, Yea-Num
#' @examples
#' hg_bar_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export
hgch_bar_CatNum <- function(data, ...){

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
    hc_chart(type = ifelse(l$orientation == "hor","bar","column"),
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
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(theme(opts = l$theme))

  hc
}
