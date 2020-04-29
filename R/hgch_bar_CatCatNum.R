#' Bar Chart Cat Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num, Cat-Yea-Num
#' @examples
#' hgch_bar_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export
hgch_bar_CatCatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)

  f <- homodatum::fringe(data)
  nms <- getFringeLabels(f)
  d <- getFringeDataFrame(f)

  labelsXY <- labelsXY(hor_title = opts$title$hor_title %||% nms[1],
                       ver_title = opts$title$ver_title %||% nms[2],
                       nms = nms, orientation = opts$chart$orientation)

  hor_title <- as.character(labelsXY[1])
  ver_title <- as.character(labelsXY[2])

  d <- preprocessData(d, opts$preprocess$drop_na)

  # Summarize
  d <- summarizeData(d, opts$summarize$agg, to_agg = c, a, b)
  d <- completevalues(d)
  if (opts$postprocess$percentage) {
    d <- d %>% group_by(b) %>%
      dplyr::mutate(c = (c / sum(c, na.rm = TRUE)) * 100)
  }
  # Postprocess
  d <- postprocess(d, "c", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)
  d <- order_category(d, col = "a", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)

  if (is.null(opts$style$color_by)) opts$style$color_by <- nms[1]

  color_by <- names(nms[match(opts$style$color_by, nms)])
  palette <- opts$theme$palette_colors
  d$..colors <- paletero::map_colors(d, color_by, palette, colors_df = NULL)

  series <- purrr::map(unique(d[[1]]), function(i) {
    d0 <- d %>%
      dplyr::filter(a %in% i)
    l0 <- list("name" = i,
               "color" = unique(d0$..colors),
               "data" = d0$c)
  })

  format_num <- format_hgch(opts$style$format_num_sample, "y")
  if (is.null(opts$tooltip)) {
    opts$tooltip <- paste0('<b>', nms[2], ': </b>{point.category}</br>',
                           '<b>', nms[1], ': </b>{series.name}</br>',
                           nms[3], ': ',
                           opts$style$prefix,'{point.', format_num,'}', opts$style$suffix)
  }


  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = opts$title$title) %>%
    hc_subtitle(text = opts$title$subtitle) %>%
    hc_chart(type = ifelse(opts$chart$orientation == "hor","bar","column"),
             events = list(
               load = add_branding(opts$theme)
             )) %>%
    hc_add_series_list(series) %>%
    hc_xAxis(title = list(text = hor_title),
             categories = purrr::map(as.character(unique(d$b)), function(z) z),
             type = "category") %>%
    hc_yAxis(title = list(text = ver_title),
             labels = list(
               formatter = makeup::makeup_format_js(sample = opts$style$format_num_sample,
                                                    locale = opts$style$locale,
                                                    prefix = opts$style$prefix,
                                                    suffix = opts$style$suffix))
    ) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = opts$title$caption %||% "") %>%
    hc_legend(enabled = TRUE) %>%
    hc_add_theme(theme(opts =  c(opts$theme,
                                 suffix = opts$style$suffix,
                                 prefix = opts$style$prefix,
                                 format_num = format_num)))

  if (opts$chart$graph_type == "stacked"){
    hc <- hc %>% hc_plotOptions(bar = list(stacking = "normal"), column = list(stacking = "normal"))
    if (opts$postprocess$percentage) {
      hc <- hc %>% hc_yAxis(maxRange = 100,
                            max = 100)
    }
  }

  hc
}
