#' Prepare data for visualization
#'
#' This function prepares data for visualization by performing operations such as aggregation and data formatting.
#' @param ... additional arguments to be passed to \code{\link[dsvizopts]{dsviz_default_opts}}
#'
#' @return list with options to change properties of plot
#' @import dsvizopts
#'
#' @keywords internal
plot_opts <- function(viz = NULL, frType = NULL, ...) {
  if (is.null(viz)) return()
  opts <- dsvizopts::merge_dsviz_options(...)
  plot_type <- viz
  extra_opts <- list()
  ndig <- makeup::which_num_format(opts$prep$format_sample_num)$separators$n_decimal
  if (opts$shiny$shiny_clickable) {
    input_name <- opts$shiny$shiny_input_name
    if (!is.null(frType)) {
      opts$theme$click_function <- click_functions(viz = viz,
                                                   frtype = frType,
                                                   id_click = input_name)
    }
    opts$theme$cursor <- opts$shiny$shiny_cursor
  }
  if (viz == "bar") {
    bar_orientation <- opts$bar$bar_orientation
    extra_opts$graph_type <- opts$bar$bar_graph_type
    extra_opts$percentage <- opts$prep$percentage
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

  if (viz == "sankey") {
    extra_opts$sankey_series = opts$sankey$sankey_serie_name
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
    label_wrap_legend = opts$prep$label_wrap_legend,
    new_line = "<br/>",
    sort = opts$prep$sort,
    sort_by_cat = opts$prep$sort_by_cat %||% FALSE,
    slice_n = opts$prep$slice_n,
    n_digits = ndig
  )

  general_opts <- list(
    hor_title = opts$titles$hor_title %||% " ",
    ver_title = opts$titles$ver_title %||% " ",
    plot_type = plot_type,
    format_sample_num = opts$prep$format_sample_num,
    legend_show = opts$theme$legend_show,
    legend_title = opts$titles$legend_title
  )

  general_opts <- modifyList(general_opts, extra_opts)
  opts$theme$palette_colors <- opts$theme$palette_colors %||% opts$theme$palette_colors_categorical
  opts$theme <- c(opts$theme, opts$data_labels)

  list(titles = titles,
       data_opts = data_opts,
       general_opts = general_opts,
       theme = opts$theme)

}


dataprep_opts <- function(...) {
  opts <- dsvizopts::merge_dsviz_options(...)
  list(
    agg = opts$prep$agg,
    agg_text = opts$prep$agg_text,
    na_rm = opts$prep$na_rm,
    percentage = opts$prep$percentage,
    percentage_name = opts$prep$percentage_name,
    percentage_col = opts$prep$percentage_col,
    extra_col = opts$prep$collapse_rows,
    agg_extra = opts$prep$agg_collapse_rows,
    agg_add = opts$prep$agg_data
  )
}

