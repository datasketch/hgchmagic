#' @export
hgch_bar <- function (data, dic = NULL, var_cat = NULL, var_num = NULL, ...) {

  if (is.null(var_cat)) stop("You must enter at least one categorical variable")
  opts <- plot_opts(viz = "bar", ...)
  opts$data_opts$color_by <- var_cat[1]
  frType <- frtype_viz(var_cat, var_num)

  data_draw <- data_draw(data = data,
                         dic = dic,
                         var_cat = var_cat,
                         var_num = var_num,
                         frType = frType,
                         opts = opts$data_opts)

  #global_options(opts$style$format_sample_num)
  bar <- highchart() |>
    hc_titles(opts = opts$titles)  |>
    hc_body(data = data_draw, frType = frType, opts = opts$general_opts)#|>
  # |>

  bar

}


#' @export
hgch_bar_Cat <- function(data, ...) {
  var_cat <- names(data)[1]
  opts_prep <- dataprep_opts(...)
  data <- dsdataprep::aggregation_data(data = data,
                               agg = "count",
                               group_var = var_cat,
                               name = opts_prep$agg_text %||% "count",
                               percentage = opts_prep$percentage,
                               percentage_name = opts_prep$percentage_name,
                               extra_col = opts_prep$extra_col,
                               agg_extra = opts_prep$agg_extra)
  hgch_bar(data = data, var_cat = var_cat, var_num = "count", ...)
}


#' @export
hgch_bar_CatNum <- function(data, ...) {
  var_cat <- names(data)[1]
  var_num <- names(data)[2]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num
  print(var_num_name)
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = opts_prep$agg,
                                       name = var_num_name,
                                       group_var = var_cat,
                                       to_agg = var_num,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  hgch_bar(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}


#' @export
hgch_bar_CatCat <- function(data, ...) {
  var_cat <- c(names(data)[1], names(data)[2])
  opts_prep <- dataprep_opts(...)
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "count",
                                       group_var = var_cat,
                                       name = "Conteo",
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  hgch_bar(data = data, var_cat = var_cat, var_num = "Conteo", ...)
}


#' @export
hgch_bar_CatCatNum <- function(data, ...) {
  var_cat <- c(names(data)[1], names(data)[2])
  var_num <- names(data)[3]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "mean",
                                       group_var = var_cat,
                                       to_agg = var_num,
                                       name = var_num_name,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  hgch_bar(data = data, var_cat = var_cat, var_num = var_num_name, ...)
}
