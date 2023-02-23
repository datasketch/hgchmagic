#' @keywords internal
frtype_viz <- function(var_cat = NULL,
                       var_num = NULL,
                       var_date = NULL) {

  frtype <- NULL
  if (!is.null(var_cat)) frtype <- paste0(rep("Cat", length(var_cat)), collapse = "")
  if (!is.null(var_date)) frtype <- paste0(frtype, paste0(rep("Dat", length(var_date)), collapse = ""), collapse = "")
  if (!is.null(var_num)) frtype <- paste0(frtype, paste0(rep("Num", length(var_num)), collapse = ""), collapse = "")

  frtype
}

#' Create data for a Highcharts plot
#'
#' This function creates data for a Highcharts plot, given input data and options.
#'
#' @param data a data frame containing the data to be plotted
#' @param dic a named vector specifying the categories and colors for the data
#' @param var_cat the name of the categorical variable in the data frame
#' @param var_num the name of the numeric variable in the data frame
#' @param var_date the name of the date variable in the data frame
#' @param viz the type of visualization to be created ("treemap", "line", "bar", "pie", or "donut")
#' @param frType a string indicating the format of the data frame (e.g., "CatNum", "DatNum", "CatCatNum")
#' @param opts a list containing options for the plot (e.g., tooltip_template, na_label, format_sample_num)
#'
#' @return a list of data for use in creating a Highcharts plot
#'
#' @keywords internal
data_draw <- function(data,
                      dic = NULL,
                      var_cat = NULL,
                      var_num = NULL,
                      var_date = NULL,
                      viz = NULL,
                      frType = NULL,
                      opts = NULL) {


  var <- NULL
  index_names <- NULL

  if (!"..labels" %in% names(data)) {
    data$..labels <- dsdataprep::prep_tooltip(data = data,
                                              tooltip = opts$tooltip_template,
                                              new_labels = NULL,
                                              engine = "html",
                                              as_df = FALSE,
                                              na_row_default_column = NULL,
                                              na_row_default_value = NULL,
                                              na_label = opts$na_label,
                                              format_num = opts$format_sample_num,
                                              opts_format_num = list(prefix = opts$prefix_num,
                                                                     suffix = opts$suffix_num,
                                                                     si_prefix = opts$si_prefix),
                                              format_cat = opts$format_sample_cat,
                                              format_date = opts$format_sample_dat)
  }

  if (!"..colors" %in% names(data)) {
    data <- dsdataprep::add_data_colors(data = data,
                                        color_by = opts$color_by,
                                        palette_colors = opts$palette_colors,
                                        #palette_type = opts$palette_type,
                                        palette = opts$palette
                                        )
  }

  if (frType == "CatNum") {
    var <- c(var_cat, var_num, "..labels", "..colors")
  }
  if (frType == "DatNum") {
    var <- c(var_date, var_num, "..labels", "..colors")
  }

  if (frType == "CatCatNum") {
    index_names <- c("..index", "..legendIndex")
    var <- c(var_cat, var_num, "..labels", "..colors", "..index", "..legendIndex")
  }

  data <- dsdataprep::wrap_sort_data(data = data,
                                     col_cat = var_cat,
                                     col_num = var_num,
                                     order = opts$order,
                                     order_legend = opts$order_legend,
                                     label_wrap = opts$label_wrap,
                                     new_line = opts$new_line,
                                     sort = opts$sort,
                                     slice_n = opts$slice_n,
                                     #intra_cat = opts$intra_cat,
                                     index_names = index_names)

  data <- data |> select({{ var }}, everything())

  l <- NULL
  if (viz == "treemap") ld <- list_treemap(data, frType)
  if (viz == "line") ld <- list_line(data, frType)
  if (viz %in% c("bar", "pie", "donut")) ld <- list_bar(data, frType)
  ld

}


#' @keywords internal
global_options <- function(sample){
  params <- makeup::which_num_format(sample)
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- params$separators$thousands
  hcoptslang$decimalPoint <- params$separators$decimal
  options(highcharter.lang = hcoptslang)
}
