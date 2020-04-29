#' line Chart Dat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Num, Yea-Num
#' @examples
#' hgch_line_CatNum(sampleData("Dat-Num", nrow = 10))
#' @export
hgch_line_DatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(theme,...)

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
  d <- summarizeData(d, opts$summarize$agg, to_agg = b, a)

  # Postprocess
  d <- postprocess(d, "b", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)
  d <- order_category(d, col = "a", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)

  #d$a <- makeup::makeup_dat(d$a,  locale = opts$style$locale)
  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- 'NA'
  print(d)

  series <- list(list(
    data = purrr::map(1:nrow(d), function(x) {
      d$b[x]
    })
  ))

  if (is.null(opts$tooltip)) opts$tooltip <- paste0('<b>{point.name}</b><br/>',
                                                    nms[2], ': ',
                                                    opts$prefix,'{point.y}', opts$suffix)

  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = opts$title$title) %>%
    hc_subtitle(text = opts$title$subtitle) %>%
    hc_chart(type = ifelse(opts$style$spline, "spline","line"),
             events = list(
               load = add_branding(opts$theme)
             )) %>%
    hc_add_series_list(series) %>%
    hc_xAxis(title = list(text = hor_title),
             categories = purrr::map(as.character(unique(d$a)), function(z) z)) %>%
    hc_yAxis(title = list(text = ver_title),
             labels = list(
               formatter = makeup::makeup_format_js(sample = opts$style$format_num_sample,
                                                    locale = opts$style$locale,
                                                    prefix = opts$style$prefix,
                                                    suffix = opts$style$suffix)),
             minRange = min(d$b, na.rm = TRUE),
             min = min(d$b, na.rm = TRUE)
    ) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = opts$title$caption %||% "") %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(theme(opts = opts$theme))

  hc
}
