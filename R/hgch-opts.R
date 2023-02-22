#' Prepare data for visualization
#'
#' This function prepares data for visualization by performing operations such as aggregation and data formatting.
#'
#' @param data a data.frame containing the data to be prepared
#' @param tooltip a character vector containing the tooltip labels for each column of data
#' @param new_labels a named character vector containing the labels to replace column names of data
#' @param engine the rendering engine for the tooltip labels, either "html" or "markdown"
#' @param as_df if TRUE, returns the data.frame with a new column for the tooltip, otherwise returns a character vector of the tooltip labels
#' @param na_row_default_column the name of the column in \code{data} containing default values for rows with NAs
#' @param na_row_default_value the default value for rows with NAs, if \code{na_row_default_column} is not provided
#' @param na_label the text label for missing values
#' @param format_num the sample format for numerical data
#' @param opts_format_num a list of formatting options for numerical data
#' @param format_cat the sample format for categorical data
#' @param format_date the sample format for date data
#'
#' @return list with options to change properties of plot
#'
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
    plot_type = plot_type
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

