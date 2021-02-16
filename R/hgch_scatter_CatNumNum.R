#' scatter Chart Cat Num Numeric
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Num-Num
#' @examples
#' hgch_scatter_CatNumNum(sample_data("Cat-Num-Num", nrow = 10))
#' @export
hgch_scatter_CatNumNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data = data, opts = opts, plot = "scatter")

  d <- l$d

  ds <- NULL
  series <- lapply(unique(d$a), function(s){
    ds <<- d %>% dplyr::filter(a == s)
    dss <- ds %>% dplyr::select(a,b)
    dss <- dss %>%
      dplyr::mutate(x = ds$b,
                    y = ds$c)
    list(
      name = s,#"First",
      color = unique(ds$..colors),
      data = purrr::transpose(dss)
    )
  })


  h <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = "scatter",
             events = list(
               load = add_branding(l$theme)
             )
    ) %>%
    hc_xAxis(
      title = list(text = l$titles$x),
      labels = list(
        formatter = makeup::makeup_format_js(sample = opts$style$format_num_sample,
                                             locale = opts$style$locale,
                                             prefix = opts$scatter$prefix_x,
                                             suffix = opts$scatter$suffix_x))
    ) %>%
    hc_yAxis(
      title = list(text = l$titles$y),
      labels = list(
        formatter = makeup::makeup_format_js(sample = opts$style$format_num_sample,
                                             locale = opts$style$locale,
                                             prefix = opts$scatter$prefix_y,
                                             suffix = opts$scatter$suffix_y))
    ) %>%
    hc_add_series_list(series) %>%
      hc_tooltip(useHTML=TRUE,
                 pointFormat = l$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme)))

  h
}
