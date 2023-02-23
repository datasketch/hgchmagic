#' Highcharter line Chart
#'
#' Creates a line chart using Highcharter library.
#'
#' @param data a data frame containing the variables to be plotted.
#' @param dic a named list, each element corresponding to a column name in \code{data} and defining a set of labels for that column.
#' @param var_dat a character vector with the names of date variable.
#' @param var_num a character vector with the names of numerical variables.
#' @param ... additional arguments to be passed to \code{\link[dsvizopts]{dsviz_default_opts}}
#'
#' @return a Highcharter bar chart.
#'
#' @examples
#' data(mtcars)
#' mtcars$cyl <- as.character(mtcars$cyl)
#' mtcars <- mtcars |> group_by(cyl) |> summarize(mpg = mean(mpg))
#' hgch_line(mtcars, var_cat = "cyl", var_num = "mpg")
#'
#' @importFrom dplyr mutate group_by summarise
#'
#' @export
hgch_line <- function (data, dic = NULL, var_dat = NULL, var_num = NULL, ...) {

  if (is.null(var_dat)) stop("You must enter at least one categorical variable")
  opts <- plot_opts(viz = "line", ...)
  opts$data_opts$color_by <- var_dat[1]
  frType <- frtype_viz(var_date = var_dat, var_num = var_num)

  data_draw <- data_draw(data = data,
                         dic = dic,
                         var_num = var_num,
                         var_date = var_dat,
                         frType = frType,
                         opts = opts$data_opts,
                         viz = "line")

  #global_options(opts$style$format_sample_num)
  line <- highchart() |>
    hc_titles(opts = opts$titles)  |>
    hc_body_line(data = data_draw, frType = frType, opts = opts$general_opts)#|>
  # |>

  line

}
