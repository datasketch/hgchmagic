#' Highcharter treemap Chart
#'
#' Creates a treemap chart using Highcharter library.
#'
#' @param data a data frame containing the variables to be plotted.
#' @param dic a named list, each element corresponding to a column name in \code{data} and defining a set of labels for that column.
#' @param var_cat a character vector with the names of categorical variables.
#' @param var_num a character vector with the names of numerical variables.
#' @param ... additional arguments to be passed to \code{\link[dsvizopts]{dsviz_default_opts}}
#'
#' @return a Highcharter treemap chart.
#'
#' @seealso \code{\link{data_draw}}
#'
#' @examples
#' data(mtcars)
#' mtcars$cyl <- as.character(mtcars$cyl)
#' mtcars <- mtcars |>
#'   dplyr::group_by(cyl) |>
#'   dplyr::summarise(mpg = mean(mpg))
#'
#' hgch_treemap(mtcars,
#'              var_cat = "cyl",
#'              var_num = "mpg")
#'
#' ### Custom labels
#' data <- ggplot2::diamonds
#' data <- data |>
#'   dplyr::group_by(cut) |>
#'   dplyr::summarise(price_sum = sum(price)),
#'                                      agg = "sum",
#'                                      group_var = "cut",
#'                                      to_agg = "price")
#'
#' # Define custom layers
#' ops <- list(title = "title",
#'             subtitle = "subtitle",
#'             caption = "caption",
#'             hor_title = "Categorias",
#'             ver_title = "Numeros",
#'             bar_orientation = "hor")
#'
#' hgch_treemap(data,
#'              var_cat = "cut",
#'              var_num = "price_sum",
#'              opts =  ops)
#' @export
hgch_treemap <- function (data, dic = NULL, var_cat = NULL, var_num = NULL, ...) {

  if (is.null(var_cat)) stop("You must enter at least one categorical variable")

  frType <- frtype_viz(var_cat, var_num)
  opts <- plot_opts(viz = "treemap", frType = frType, ...)
  opts$data_opts$color_by <- var_cat[1]

  data_draw <- data_draw(data = data,
                         dic = dic,
                         var_cat = var_cat,
                         var_num = var_num,
                         frType = frType,
                         opts = opts$data_opts,
                         viz = "treemap")

  #global_options(opts$style$format_sample_num)
  treemap <- highchart() |>
    hc_titles(opts = opts$titles)  |>
    hc_body_treemap(data = data_draw, frType = frType, opts = opts$general_opts) |>
    hc_add_theme(hgch_theme(opts = opts$theme))

  treemap

}

#' treemap chart for a categorical variable
#'
#' This function creates a horizontal or vertical treemap chart for a categorical variable.
#'
#' @param data a data frame.
#' @param ... additional arguments to pass to the `hgch_treemap` function.
#'
#' @return A highchart object.
#' @export
#'
#' @examples
#' data(iris)
#' iris <- iris |> dplyr::select(Species)
#' hgch_treemap_Cat(iris, percentage = TRUE)
#' @export
hgch_treemap_Cat <- function(data, ...) {
  var_cat <- names(data)[1]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% "Count"
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "count",
                                       group_var = var_cat,
                                       agg_name = opts_prep$agg_text %||% var_num_name,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% "..percentage"
  }
  hgch_treemap(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}


#' @export
hgch_treemap_CatNum <- function(data, ...) {
  var_cat <- names(data)[1]
  var_num <- names(data)[2]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num

  data <- dsdataprep::aggregation_data(data = data,
                                       agg = opts_prep$agg,
                                       agg_name = var_num_name,
                                       group_var = var_cat,
                                       to_agg = var_num,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% paste0("..percentage", var_num)
  }
  hgch_treemap(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}

#' @export
hgch_treemap_CatCat <- function(data, ...) {
  var_cat <- names(data)[1:2]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% "Count"
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "count",
                                       group_var = var_cat,
                                       agg_name = opts_prep$agg_text %||% var_num_name,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% "..percentage"
  }
  hgch_treemap(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}

#' @export
hgch_treemap_CatCatNum <- function(data, ...) {
  var_cat <- names(data)[1:2]
  var_num <- names(data)[3]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num

  data <- dsdataprep::aggregation_data(data = data,
                                       agg = opts_prep$agg,
                                       agg_name = var_num_name,
                                       group_var = var_cat,
                                       to_agg = var_num,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% paste0("..percentage", var_num)
  }
  hgch_treemap(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}
