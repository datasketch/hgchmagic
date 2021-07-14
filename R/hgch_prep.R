#' @export
hgchmagic_prep <- function(data, opts = NULL, extra_pattern = ".", plot =  "bar", ftype = "Cat-Num"){

  if (is.null(data)) return()
  data <- sample_data("Cat-Dat")
  ftype <- "Cat-Dat"
  opts <- dsvizopts::dsviz_defaults()
  plot <- "area"

  # color -------------------------------------------------------------------

  palette_type <- opts$theme$palette_type %||% "categorical"
  if(is.null(opts$theme$palette_colors)){
    opts$theme$palette_colors <- opts$theme[[paste0("palette_colors_", palette_type)]]
  }
  palette_colors <- opts$theme$palette_colors

  # data preparation by type
  list_d <- dsvizprep::data_charts_prep(data = data,
                                        ftype = ftype,
                                        agg =  opts$summarize$agg,
                                        plot = plot,
                                        color_by = opts$style$color_by,
                                        ptage = opts$postprocess$percentage,
                                        ptage_col = opts$postprocess$percentage_col,
                                        drop_na = opts$preprocess$drop_na,
                                        na_label = opts$preprocess$na_label,
                                        drop_na_legend = opts$preprocess$drop_na_legend,
                                        sort_opts = opts$postprocess$sort,
                                        slice_n = opts$postprocess$slice_n,
                                        palette = palette_colors,
                                        highlight_value = opts$chart$highlight_value,
                                        highlight_value_color = opts$chart$highlight_value_color,
                                        order_legend = opts$postprocess$order_legend,
                                        order = opts$postprocess$order,
                                        label_wrap_legend = opts$style$label_wrap_legend,
                                        label_wrap = opts$style$label_wrap,
                                        group_extra_num = TRUE)
  # format setting of data being displayed
  data_format <- dsvizprep::format_prep(data = list_d$data,
                                        dic = list_d$dic,
                                        formats = list(sample_num = opts$style$format_sample_num,
                                                       sample_cat = opts$style$format_sample_cat,
                                                       prefix = opts$style$prefix,
                                                       suffix = opts$style$suffix))
  list_d$nms_tooltip <- "a"
  # add info tooltip in data
  data <- agg_tooltip(data = data_format, nms = list_d$nms, label_ftype = list_d$nms_tooltip, tooltip = opts$chart$tooltip)


}
