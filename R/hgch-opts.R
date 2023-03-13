#' Prepare data for visualization
#'
#' This function prepares data for visualization by performing operations such as aggregation and data formatting.
#' @param ... additional arguments to be passed to \code{\link[dsvizopts]{dsviz_default_opts}}
#'
#' @return list with options to change properties of plot
#' @import dsvizopts
#'
#' @keywords internal
plot_opts <- function(viz = NULL, ...) {
  if (is.null(viz)) return()
  opts <- dsvizopts::merge_dsviz_options(...)
  plot_type <- viz
  extra_opts <- list()

  if (viz == "bar") {
    bar_orientation <- opts$bar$bar_orientation
    if (bar_orientation == "hor") {
      plot_type <- "column"
    }
  }

  extra_opts$inner_size <- 0
  if (viz == "donut") {
    plot_type <- "pie"
    extra_opts$inner_size = "60%"
  }

  if (viz == "scatter") {
    extra_opts$marker_size = opts$theme$marker_radius
  }

  if (viz == "line") {
    extra_opts$axis_left_title <- opts$line$line_double_title_axis_left
    extra_opts$axis_right_title <- opts$line$line_double_title_axis_right
  }

  titles <- list(
    title = opts$titles$title,
    subtitle = opts$titles$subtitle,
    caption = opts$titles$caption,
    caption_show = !is.null(opts$titles$caption)
  )

  data_opts <- list(
    tooltip_template = opts$chart$tooltip_template,
    na_label = opts$prep$na_label,
    format_sample_num = opts$prep$format_sample_num,
    prefix_num = opts$prep$prefix_num,
    suffix_num = opts$prep$suffix_num,
    si_prefix = opts$prep$si_prefix %||% FALSE,
    format_sample_cat = opts$prep$format_sample_cat,
    format_sample_dat = opts$prep$format_sample_dat,
    color_by = opts$prep$color_by,
    palette_colors = opts$theme$palette_colors %||% opts$theme$palette_colors_categorical,
    palette_type = NULL,
    palette = NULL,
    order = opts$prep$order,
    order_legend = opts$prep$order_legend,
    label_wrap = opts$prep$label_wrap,
    new_line = "<br/>",
    sort = opts$prep$sort,
    slice_n = opts$prep$slice_n
  )

  general_opts <- list(
    hor_title = opts$titles$hor_title %||% " ",
    ver_title = opts$titles$ver_title %||% " ",
    plot_type = plot_type,
    legend_show = opts$theme$legend_show
  )

  general_opts <- modifyList(general_opts, extra_opts)

  list(titles = titles,
       data_opts = data_opts,
       general_opts = general_opts)

}


dataprep_opts <- function(...) {
  opts <- dsvizopts::merge_dsviz_options(...)
  list(
    agg = opts$prep$agg,
    agg_text = opts$prep$agg_text,
    percentage = opts$prep$percentage,
    percentage_name = opts$prep$percentage_col,
    extra_col = opts$prep$collapse_rows,
    agg_extra = opts$prep$agg_collapse_rows
  )
}

