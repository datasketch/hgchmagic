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
#' @examples
#'
#' data <- ggplot2::diamonds
#' data <- data |>
#'   dplyr::group_by(clarity) |>
#'   dplyr::summarise(x = sum(x, na.rm = T),
#'                    y = sum(carat, na.rm = T))
#'
#' hgch_scatter(data,
#'              var_num = c("x", "y"),
#'              var_cat = "clarity")
#'
#'  # Custom theme
#'   test_theme <- list(
#'     theme = list(
#'       palette_colors = c("#ffa92a", "lightgreen"),
#'       subtitle_align = "center",
#'       subtitle_family = "Roboto",
#'       subtitle_size = 15,
#'       subtitle_color = "#3b83b8",
#'       subtitle_weight = 700
#'     )
#'   )
#'
#'   hgch_scatter(data,
#'                var_num = c("x", "y"),
#'                var_cat = "clarity",
#'                opts = test_theme,
#'                title = "test title",
#'                subtitle = "test subtitle")
#'
#' @export
hgch_scatter <- function (data, dic = NULL, var_cat = NULL, var_dat = NULL, var_num = NULL, ...) {


  frType <- frtype_viz(var_date = var_dat, var_num = var_num, var_cat = var_cat)
  opts <- plot_opts(viz = "scatter", frType = frType, ...)

  grouped_var <- c(var_cat, var_dat)
  if (!is.null(grouped_var)) {
    opts$data_opts$color_by <- grouped_var[1]
  }
  data_draw <- data_draw(data = data,
                         dic = dic,
                         var_cat = var_cat,
                         var_num = var_num,
                         var_date = var_dat,
                         frType = frType,
                         opts = opts$data_opts,
                         viz = "scatter")

  global_options(opts$general_opts$format_sample_num)
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
  if (opts_prep$agg_add) {
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = opts_prep$agg,
                                       agg_name = var_num_name,
                                       group_var = c(var_cat, var_dat),
                                       to_agg = var_num,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  }
  hgch_scatter(data = data, var_cat = var_cat, var_dat = var_dat, var_num = var_num_name, ...)
}

#' @export
hgch_scatter_CatNumNum <- function(data, ...) {
  var_cat <- names(data)[1]
  var_num <- names(data)[2:3]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num
  if (opts_prep$agg_add) {
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = opts_prep$agg,
                                       agg_name = var_num_name,
                                       group_var = var_cat,
                                       to_agg = var_num,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  }
  hgch_scatter(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}

#' @export
hgch_scatter_NumNum <- function(data, ...) {

  var_num <- c(names(data)[1:2])
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num

  hgch_scatter(data = data, var_num = var_num, ...)
}

