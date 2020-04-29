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

  f <- homodatum::fringe(data)
  nms <- getFringeLabels(f)
  d <- getFringeDataFrame(f)

  labelsXY <- labelsXY(hor_title = opts$title$hor_title %||% nms[1],
                       ver_title = opts$title$ver_title %||% nms[2],
                       nms = nms, orientation = opts$chart$orientation)

  hor_title <- as.character(labelsXY[1])
  ver_title <- as.character(labelsXY[2])

  d <- preprocessData(d, opts$preprocess$drop_na)
  d$a[is.na(d$a)] <- 'NA'
  # Summarize
  d <- summarizeData(d, opts$summarize$agg, to_agg = b, a)

  # Postprocess
  d <- postprocess(d, "b", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)

  # Styles
  # Handle colors
  color_by <- names(nms[match(opts$style$color_by, nms)])
  palette <- opts$theme$palette_colors
  d$..colors <- paletero::map_colors(d, color_by, palette, colors_df = NULL)

  if (!is.null(opts$chart$highlight_value)) {
    w <- which(d$a %in% opts$chart$highlight_value)
    d$..colors[w] <- opts$chart$highlight_value_color
  }

  d <- order_category(d, col = "a", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)

  l <- purrr::map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "y" = as.numeric(d$b[z]),
                            "color" = as.character(d$..colors[z]))
  })

  format_num <- format_hgch(opts$style$format_num_sample, "y")

  if (is.null(opts$tooltip)) {
    opts$tooltip <- paste0('<b>{point.name}</b><br/>',
                           nms[2], ': ',
                           opts$style$prefix,'{point.',format_num ,'}', opts$style$suffix)
  }

  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = opts$title$title) %>%
    hc_subtitle(text = opts$title$subtitle) %>%
    hc_chart(type = ifelse(opts$chart$orientation == "hor","bar","column"),
             events = list(
               load = add_branding(opts$theme)
             )) %>%
    hc_series(
      data
    ) %>%
    hc_xAxis(title = list(text = hor_title),
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
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(theme(opts = c(opts$theme,
                                suffix = opts$style$suffix,
                                prefix = opts$style$prefix,
                                format_num = format_num)))

  hc
}
