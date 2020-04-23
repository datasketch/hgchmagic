#' Bar Chart Cat Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @inherit hgchmagic_default_opts
#' @inheritDotParams hgchmagic_default_opts
#' @section ctypes:
#' Cat-Num, Yea-Num
#' @examples
#' gg_bar_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export
hgch_bar_CatCatNum <- function(data, ...){

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
  # d$a[is.na(d$a)] <- 'NA'
  # d$b[is.na(d$b)] <- "NA"
  # Summarize
  d <- summarizeData(d, opts$summarize$agg, to_agg = c, a, b)
  d <- completevalues(d)

  # Postprocess
  d <- postprocess(d, "c", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)
  d <- order_category(d, col = "a", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)


  series <- purrr::map(unique(d[[1]]), function(i) {
    d0 <- d %>%
      dplyr::filter(a %in% i)
    l0 <- list("name" = i,
               "data" = d0$c)
  })

if (is.null(opts$tooltip)) opts$tooltip <- paste0('<b>', nms[2], ': </b>{point.category}</br>',
                                                 '<b>', nms[1], ': </b>{series.name}</br>',
                                                  nms[3], ': ',
                                                 opts$style$prefix,'{point.y}', opts$style$suffix)


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
    #hc_legend(enabled = FALSE) %>%
    hc_add_theme(theme(opts = opts$theme))

  hc
}
