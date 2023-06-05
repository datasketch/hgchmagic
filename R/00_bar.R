  #' Highcharter Bar Chart
#'
#' Creates a bar chart using Highcharter library.
#'
#' @param data a data frame containing the variables to be plotted.
#' @param dic a named list, each element corresponding to a column name in \code{data} and defining a set of labels for that column.
#' @param var_cat a character vector with the names of categorical variables.
#' @param var_num a character vector with the names of numerical variables.
#' @param ... additional arguments to be passed to \code{\link[dsvizopts]{dsviz_default_opts}}
#'
#' @return a Highcharter bar chart.
#'
#' @seealso \code{\link{data_draw}}
#'
#'
#' @examples
#' data(mtcars)
#' mtcars$cyl <- as.character(mtcars$cyl)
#'
#' mtcars <- mtcars |>
#'   dplyr::group_by(cyl) |>
#'   dplyr::summarise(mpg = mean(mpg))
#'
#' hgch_bar(mtcars, var_cat = "cyl", var_num = "mpg")
#'
#'
#' ### Adding style to plots
#' ops <- list(titles = list(title = "title",
#'                           subtitle = "subtitle",
#'                           caption = "caption"))
#'
#'  data <- ggplot2::diamonds |>
#'    dplyr::select(cut, color, price, everything())
#'
#'  # plot 1
#'  hgch_bar_CatCat(data, opts = ops)
#'
#'  # plot 2
#'  hgch_bar_CatCatNum(data, opts = ops)
#'
#'
#'  # opts from theme (canvas)
#'  test_theme <- list(
#'    theme = list(
#'    background_color = "#2f2f2f",
#'    plot_margin_bottom = 30,
#'    plot_margin_left = 30,
#'    plot_margin_right = 30,
#'    plot_margin_top = 30,
#'    plot_background_color = "#f2f2f2",
#'    plot_border_color = "#ff2c2f",
#'    plot_border_size = 3,
#'    text_family = "ubuntu",
#'    text_size = 15
#'    )
#'  )
#'
#'  data <- ggplot2::diamonds |>
#'   dplyr::select(cut, everything())
#'
#' hgch_bar_Cat(data, opts = test_theme)
#'
#' @export
hgch_bar <- function (data, dic = NULL, var_cat = NULL, var_num = NULL, ...) {

  if (is.null(var_cat)) stop("You must enter at least one categorical variable")

  frType <- frtype_viz(var_cat, var_num)
  opts <- plot_opts(viz = "bar", frType = frType, ...)
  if (length(var_cat) > 1) {
  opts$data_opts$color_by <- var_cat[1]
  }
  count_var_num <- stringr::str_count(frType, "Num")

  if (count_var_num > 1) {
    opts$data_opts$color_by <- NULL
    if (!"..colors" %in% names(data)) {
      colors <- opts$data_opts$palette_colors[1:count_var_num]
      if (nrow(data) > 1) {
      data$..colors <- rep_len(colors, nrow(data))
      } else {
      data$..colors <- paste0(colors, collapse = "-")
      }
    }
  }

  data_draw <- data_draw(data = data,
                         dic = dic,
                         var_cat = var_cat,
                         var_num = var_num,
                         frType = frType,
                         opts = opts$data_opts,
                         viz = "bar")
 #print(data_draw)
  global_options(opts$general_opts$format_sample_num)
  bar <- highchart() |>
    hc_titles(opts = opts$titles)  |>
    hc_body(data = data_draw, frType = frType, opts = opts$general_opts) |>
    hc_add_theme(hgch_theme(opts = opts$theme))

  bar

}

#' Bar chart for a categorical variable
#'
#' This function creates a horizontal or vertical bar chart for a categorical variable.
#'
#' @param data a data frame.
#' @param ... additional arguments to pass to the `hgch_bar` function.
#'
#' @return A highchart object.
#' @export
#'
#' @examples
#' data(iris)
#' iris <- iris |> dplyr::select(Species)
#' hgch_bar_Cat(iris, percentage = TRUE)
#' @export
hgch_bar_Cat <- function(data, ...) {
  var_cat <- names(data)[1]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% "Count"
  data <- dsdataprep::aggregation_data(data = data,
                               agg = "count",
                               na_label = opts_prep$na_label,
                               group_var = var_cat,
                               agg_name = var_num_name,
                               percentage = opts_prep$percentage,
                               percentage_name = opts_prep$percentage_name,
                               extra_col = opts_prep$extra_col,
                               agg_extra = opts_prep$agg_extra)

  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% "..percentage Count"
  }
  hgch_bar(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}


#' @export
hgch_bar_CatNum <- function(data, ...) {
  var_cat <- names(data)[1]
  var_num <- names(data)[2]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num

  if (opts_prep$agg_add) {
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = opts_prep$agg,
                                       agg_name = var_num_name,
                                       group_var = var_cat,
                                       na_label = opts_prep$na_label,
                                       to_agg = var_num,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  }

  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% paste0("..percentage ", var_num)
  }
  hgch_bar(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}


#' @export
hgch_bar_CatCat <- function(data, ...) {
  var_cat <- c(names(data)[1], names(data)[2])
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% "Count"
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "count",
                                       group_var = var_cat,
                                       na_label = opts_prep$na_label,
                                       agg_name = var_num_name,
                                       percentage_col = opts_prep$percentage_col,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)


  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% "..percentage Count"
  }
  hgch_bar(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}


#' @export
hgch_bar_CatCatNum <- function(data, ...) {
  var_cat <- c(names(data)[1], names(data)[2])
  var_num <- names(data)[3]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num
  if (opts_prep$agg_add) {
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "mean",
                                       group_var = var_cat,
                                       to_agg = var_num,
                                       na_label = opts_prep$na_label,
                                       agg_name = var_num_name,
                                       percentage = opts_prep$percentage,
                                       percentage_col = opts_prep$percentage_col,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  }

  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% paste0("..percentage ", var_num)
  }
  hgch_bar(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}

#' @export
hgch_bar_CatNumNum <- function(data, ...) {
  var_cat <- names(data)[1]
  var_num <- names(data)[2:3]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num

  data <- dsdataprep::aggregation_data(data = data,
                                       agg = opts_prep$agg,
                                       agg_name = var_num_name,
                                       group_var = var_cat,
                                       na_label = opts_prep$na_label,
                                       to_agg = var_num,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% paste0("..percentage ", var_num)
  }
  hgch_bar(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}
