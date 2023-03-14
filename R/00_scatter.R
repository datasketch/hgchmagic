#' Highcharter scatter Chart
#'
#' Creates a scatter chart using Highcharter library.
#'
#' @param data a data frame containing the variables to be plotted.
#' @param dic a named list, each element corresponding to a column name in \code{data} and defining a set of labels for that column.
#' @param var_cat the name of the categorical variable in the data frame
#' @param var_dat a character vector with the names of date variable.
#' @param var_num a character vector with the names of numerical variables.
#' @param ... additional arguments to be passed to \code{\link[dsvizopts]{dsviz_default_opts}}
#'
#' @return a Highcharter scatter chart.
#'
#' @seealso \code{\link{data_draw}}
#'
#' @export
hgch_scatter <- function (data, dic = NULL, var_cat = NULL, var_dat = NULL, var_num = NULL, ...) {

  opts <- plot_opts(viz = "scatter", ...)

  grouped_var <- c(var_cat, var_dat)
  if (!is.null(grouped_var)) {
  opts$data_opts$color_by <- grouped_var[1]
  }
  frType <- frtype_viz(var_date = var_dat, var_num = var_num, var_cat = var_cat)

  data_draw <- data_draw(data = data,
                         dic = dic,
                         var_cat = var_cat,
                         var_num = var_num,
                         var_date = var_dat,
                         frType = frType,
                         opts = opts$data_opts,
                         viz = "scatter")


  scatter <- highchart() |>
    hc_titles(opts = opts$titles)  |>
    hc_body_scatter(data = data_draw, frType = frType, opts = opts$general_opts) |>
    hc_add_theme(hgch_theme(opts = opts$theme))

  scatter

}


#' @export
hgch_scatter_CatDatNum <- function(data, ...) {
  var_cat <- names(data)[1]
  var_dat <- names(data)[2]
  var_num <- names(data)[3]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num

  data <- dsdataprep::aggregation_data(data = data,
                                       agg = opts_prep$agg,
                                       agg_name = var_num_name,
                                       group_var = c(var_cat, var_dat),
                                       to_agg = var_num,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  hgch_scatter(data = data, var_cat = var_cat, var_dat = var_dat, var_num = var_num_name, ...)
}

#' @export
hgch_scatter_NumNum <- function(data, ...) {

  var_num <- c(names(data)[1:2])
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num

  hgch_scatter(data = data, var_num = var_num, ...)
}

