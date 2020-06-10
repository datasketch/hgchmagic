#' @export
hgchmagic_prep <- function(data, opts = NULL, extra_pattern = ".", plot =  "bar"){

  # Handle homodatum
  f <- homodatum::fringe(data)
  nms <- fringe_labels(f)
  d <- fringe_d(f)

  frtype <- f$frtype


  frtype <- gsub("Yea", "Cat", frtype)
  d_frtype <- strsplit(frtype, split = "-") %>% unlist()
  var_cats <- grep("Cat", d_frtype)
  var_date <- grep("Dat", d_frtype)
  var_num <- grep("Num", d_frtype)

  if (!is.null(opts$title$hor_title)) {
  if (opts$title$hor_title == "") opts$title$hor_title <- NULL
  }
  if (!is.null(opts$title$ver_title)) {
  if (opts$title$ver_title == "") opts$title$ver_title <- NULL
  }

  if (opts$chart$tooltip == "") opts$chart$tooltip <- NULL

  if (identical(var_num, integer())) {
    if (length(d_frtype) == 1) {
      d$a <- as.character(d$a)
      d <- d %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(b = n())
      frtype <- "Cat-Num"
      nms[2] <- opts$summarize$agg_text %||% "Count"
      names(nms) <- c("a", "b")}
    if (length(d_frtype) == 2) {
      d$a <- as.character(d$a)
      d$b <- as.character(d$b)
      d <- d %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(c = n())
      frtype <- "Cat-Cat-Num"
      nms[3] <-  opts$summarize$agg_text %||% "Count"
      names(nms) <- c("a", "b", "c")}
  }

  labelsXY <- labelsXY(hor_title = opts$title$hor_title %||% nms[1],
                       ver_title = opts$title$ver_title %||% nms[2],
                       nms = nms, orientation = opts$chart$orientation)
  hor_title <- as.character(labelsXY[1])
  ver_title <- as.character(labelsXY[2])


  if (length(var_cats) > 1) {
    # print(nms)
    # print('hola')
    labelsXY <- opts$title$hor_title %||% nms[2]
    labelsXY[2] <- opts$title$ver_title %||% nms[3]
    if (opts$chart$orientation == "hor")  labelsXY <- rev(labelsXY)

    hor_title <- as.character(labelsXY[1])
    ver_title <- as.character(labelsXY[2])

    d$a <- as.character(d$a)
    d$b <- as.character(d$b)

    d <- preprocessData(d, drop_na = opts$preprocess$drop_na,
                        na_label = opts$preprocess$na_label, na_label_cols = "b")
    d <- preprocessData(d, drop_na = opts$preprocess$drop_na_legend,
                        na_label = opts$preprocess$na_label, na_label_cols = "a")

    d <- summarizeData(d, opts$summarize$agg, to_agg = c, a, b)

    d <- completevalues(d)

    if (opts$postprocess$percentage) {
      opts$style$suffix <- "%"
      d <- d %>% group_by(b) %>%
        dplyr::mutate(c = (c / sum(c, na.rm = TRUE)) * 100)
    }
    d <- postprocess(d, "c", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)

    if (is.null(opts$style$color_by)) opts$style$color_by <- nms[1]


  } else if (length(var_date) == 1) {
    d <- preprocessData(d, drop_na = TRUE, na_label_cols = "a")
    d <- summarizeData(d, opts$summarize$agg, to_agg = b, a)
    d <- postprocess(d, "b", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)
  } else {
    d$a <- as.character(d$a)
    d <- preprocessData(d, drop_na = opts$preprocess$drop_na,
                        na_label = opts$preprocess$na_label, na_label_cols = "a")
    d <- summarizeData(d, opts$summarize$agg, to_agg = b, a)
    # Postprocess
    d <- postprocess(d, "b", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)
  }

  # Styles
  # Handle colors
  color_by <- names(nms[match(opts$style$color_by, nms)])
 #print(color_by)
  palette <- opts$theme$palette_colors
  #print(d)
  d$..colors <- paletero::map_colors(d, color_by, palette, colors_df = NULL)

  if (length(var_cats) > 1) {
    d <- order_category(d, col = "a", order = opts$postprocess$order_legend, label_wrap = opts$style$label_wrap_legend)
    d <- order_category(d, col = "b", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)
  } else {
    if (!is.null(opts$chart$highlight_value)) {
      w <- which(d$a %in% opts$chart$highlight_value)
      d$..colors[w] <- opts$chart$highlight_value_color
    }
    d <- order_category(d, col = "a", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)
}

  # Handle number/strings/dates formats

  if (!identical(var_cats, integer())) {
    l_cats <- map(var_cats, function(f_cats){
      d[[f_cats]] <<- makeup_chr(d[[f_cats]], opts$style$format_cat_sample)
    })}
  if (!identical(var_date, integer())) {
    l_date <- map(var_date, function(f_date){
      #print( list(d[[f_date]], sample = "2000-12-31"))
      d[[f_date]] <<- makeup_dat(d[[f_date]], sample = opts$style$format_dat_sample,
                                 locale = opts$style$locale)

    })}
  #print(d)
  f_nums <- makeup::makeup_format_js(sample = opts$style$format_num_sample,
                                     locale = opts$style$locale,
                                     prefix = opts$style$prefix,
                                     suffix = opts$style$suffix)

 sample_labels <- opts$dataLabels$dataLabels_format_sample %||% opts$style$format_num_sample
  format_dataLabels <- format_hgch(plot = plot,
                            frtype = frtype,
                            sample = sample_labels,
                            prefix = opts$style$prefix,
                            suffix = opts$style$suffix)

  fmt_dataLabel <- opts$dataLabels$dataLabels_format_sample %||% opts$style$format_num_sample
  # f_nums_dataLabel <- makeup::makeup_format(sample = fmt_dataLabel)
  tooltip <- tooltip_hgch(plot, tooltip = opts$chart$tooltip,
                          nms = nms, frtype = frtype,
                          prefix = opts$style$prefix,
                          suffix = opts$style$suffix,
                          sample = opts$style$format_num_sample)  #print(tooltip)

  list(
    d = d,
    titles = list(
      title = opts$title$title,
      subtitle = opts$title$subtitle,
      caption = opts$title$caption %||% "",
      x = hor_title,
      y = ver_title
    ),
    orientation = opts$chart$orientation,
    percentage = opts$postprocess$percentage,
    formats = f_nums,
    tooltip = tooltip,
    spline = opts$style$spline,
    color_by = color_by,
    graph_type = opts$chart$graph_type,
    extra = get_extra_opts(opts, extra_pattern),
    theme = c(opts$theme,
              dataLabels_show = opts$dataLabels$dataLabels_show,
              dataLabels_color = opts$dataLabels$dataLabels_color %||% "constrast",
              dataLabels_size = opts$dataLabels$dataLabels_size %||% "11",
              dataLabels_text_outline = opts$dataLabels$dataLabels_text_outline,
              format_dataLabels = format_dataLabels,
              suffix = opts$style$suffix,
              prefix = opts$style$prefix),
    color_hover = opts$shiny$color_hover,
    color_click = opts$shiny$color_click,
    allow_point = opts$shiny$allow_point,
    cursor = opts$shiny$cursor,
    clickFunction = opts$shiny$clickFunction
  )



}
