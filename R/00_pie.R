#' Generate a Highcharts pie chart.
#'
#' This function generates a Highcharts pie chart based on the given data.
#'
#' @param data A data frame containing the data to be plotted.
#' @param dic A named list of character vectors that replaces column names in data. The names of the list should match column names in data, and each vector should contain the replacement names.
#' @param var_cat A character vector of categorical variable(s) to use in the chart.
#' @param var_num A character vector of numeric variable(s) to use in the chart.
#' @param ... Additional arguments to be passed to \code{\link{plot_opts}}.
#'
#' @return A Highcharts pie chart object.
#'
#' @seealso \code{\link{data_draw}}
#'
#'
#' @examples
#' data1 <- data.frame(name = c("A", "B", "C"), y = c(10, 20, 30))
#' hgch_pie(data1, var_cat = "name", var_num = "y")
#'
#' @export
hgch_pie <- function (data, dic = NULL, var_cat = NULL, var_num = NULL, ...) {

  if (is.null(var_cat)) stop("You must enter at least one categorical variable")
  opts <- plot_opts(viz = "pie", ...)
  opts$data_opts$color_by <- var_cat[1]
  frType <- frtype_viz(var_cat, var_num)

  data_draw <- data_draw(data = data,
                         dic = dic,
                         var_cat = var_cat,
                         var_num = var_num,
                         frType = frType,
                         opts = opts$data_opts,
                         viz = "pie")


  pie <- highchart() |>
    hc_titles(opts = opts$titles)  |>
    hc_body(data = data_draw, frType = frType, opts = opts$general_opts) |>
    hc_add_theme(hgch_theme(opts = opts$theme))

  pie

}

#' @export
hgch_pie_Cat <- function(data, ...) {
  var_cat <- names(data)[1]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% "Count"
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "count",
                                       group_var = var_cat,
                                       agg_name = var_num_name,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  hgch_pie(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}


#' @export
hgch_pie_CatNum <- function(data, ...) {
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

  hgch_pie(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}



