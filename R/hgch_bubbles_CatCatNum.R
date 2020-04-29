#' Bubbles Chart Cat Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num, Cat-Yea-Num
#' @examples
#' hgch_bubbles_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export
hgch_bubbles_CatCatNum <- function(data, ...){

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
  # d$a[is.na(d$a)] <- "NA"
  # d$b[is.na(d$b)] <- "NA"
  # Summarize
  d <- summarizeData(d, opts$summarize$agg, to_agg = c, a, b)

  d <- completevalues(d)

  # Postprocess
  d <- postprocess(d, "c", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)
  d <- order_category(d, col = "a", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)

  if (is.null(opts$style$color_by)) opts$style$color_by <- nms[1]

  color_by <- names(nms[match(opts$style$color_by, nms)])
  palette <- opts$theme$palette_colors
  d$..colors <- paletero::map_colors(d, color_by, palette, colors_df = NULL)

  series <- map(unique(d$a), function(x) {
    df <- d %>% filter(a %in% x)
    list(
      name = x,
      data =
        map(1:nrow(df), function (z) {
          list(
            name = df$b[z],
            value = df$c[z],
            color = df$..colors[z]
          )
        })
    )
  })

  format_num <- format_hgch(opts$style$format_num_sample, "y")
  if (is.null(opts$tooltip)) {
    opts$tooltip <- paste0('<b>', nms[2], ': </b>{point.name}</br>',
                           '<b>', nms[1], ': </b>{series.name}</br>',
                           nms[3], ': ',
                           opts$style$prefix,'{point.', format_num,'}', opts$style$suffix)
  }


  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = opts$title$title) %>%
    hc_subtitle(text = opts$title$subtitle) %>%
    hc_chart(type = 'packedbubble',
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
                                 bubble_opacity = opts$chart$bubble_opacity,
                                 cats = "{point.name}<br/>",
                                 suffix = opts$style$suffix,
                                 prefix = opts$style$prefix,
                                 bubble_min = paste0(opts$chart$bubble_min, "%"),
                                 bubble_max = paste0(opts$chart$bubble_max, "%"),
                                 format_num = format_num)))

  hc
}
