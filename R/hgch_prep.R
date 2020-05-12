#' @export
hgchmagic_prep <- function(data, opts = NULL, extra_pattern = ".", value =  "y"){


  # Handle homodatum
  f <- homodatum::fringe(data)
  nms <- fringe_labels(f)
  d <- fringe_data(f)

  frtype_d <- f$frtype
  d_frtype <- strsplit(frtype_d, split = "-") %>% unlist()
  var_cats <- grep("Cat", d_frtype)
  var_date <- grep("Dat", d_frtype)
  var_num <- grep("Num", d_frtype)

  if (identical(var_num, integer())) {
    if (length(d_frtype) == 1) {
      d <- d %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(b = n())
      nms[2] <- opts$summarize$agg_text %||% "Count"}
    if (length(d_frtype) == 2) {
      d <- d %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(c = n())
      nms[3] <-  opts$summarize$agg_text %||% "Count"}
  }

  labelsXY <- labelsXY(hor_title = opts$title$hor_title %||% nms[1],
                       ver_title = opts$title$ver_title %||% nms[2],
                       nms = nms, orientation = opts$chart$orientation)
  hor_title <- as.character(labelsXY[1])
  ver_title <- as.character(labelsXY[2])

  format_num <- format_hgch(opts$style$format_num_sample, value)

  if (length(var_cats) > 1) {
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

    if (is.null(opts$tooltip)) {
      tooltip <- paste0('<b>', nms[2], ': </b>{point.category}</br>',
                             '<b>', nms[1], ': </b>{series.name}</br>',
                             nms[3], ': ',
                             opts$style$prefix,'{point.', format_num,'}', opts$style$suffix)
    }
  } else {
    d <- preprocessData(d, drop_na = opts$preprocess$drop_na,
                        na_label = opts$preprocess$na_label, na_label_cols = "a")
    d <- summarizeData(d, opts$summarize$agg, to_agg = b, a)
    # Postprocess
    d <- postprocess(d, "b", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)
    if (is.null(opts$tooltip)) {
      tooltip <- paste0('<b>{point.name}</b><br/>',
                        nms[2], ': ',
                        opts$style$prefix,'{point.',format_num ,'}', opts$style$suffix)
    }
  }

  # Styles
  # Handle colors
  color_by <- names(nms[match(opts$style$color_by, nms)])

  palette <- opts$theme$palette_colors
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
      d[[f_date]] <<- lubridate::as_date(d[[f_date]])#makeup_dat(d[[f_date]], sample = opts$style$format_dat_sample, locale = opts$style$locale)
    })}

  f_nums <- makeup::makeup_format_js(sample = opts$style$format_num_sample,
                                     locale = opts$style$locale,
                                     prefix = opts$style$prefix,
                                     suffix = opts$style$suffix)

  format_dataLabels <- format_hgch(opts$dataLabels$dataLabels_format_sample, value)


  fmt_dataLabel <- opts$dataLabels$dataLabels_format_sample %||% opts$style$format_num_sample
  # f_nums_dataLabel <- makeup::makeup_format(sample = fmt_dataLabel)
  #print(tooltip)

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
    color_by = color_by,
    graph_type = opts$chart$graph_type,
    extra = get_extra_opts(opts, extra_pattern),
    theme = c(opts$theme,
              dataLabels_show = opts$dataLabels$dataLabels_show,
              dataLabels_color = opts$dataLabels$dataLabels_color %||% "constrast",
              dataLabels_size = opts$dataLabels$dataLabels_size %||% "11",
              dataLabels_text_outline = opts$dataLabels$dataLabels_text_outline,
              format_dataLabels = format_dataLabels %||% format_num,
              suffix = opts$style$suffix,
              prefix = opts$style$prefix,
              format_num = format_num)
  )



}
