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

#' @keywords internal
data_draw <- function(data,
                      dic = NULL,
                      var_cat = NULL,
                      var_num = NULL,
                      var_date = NULL,
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
                                        palette_type = opts$palette_type,
                                        palette = opts$palette)
  }

  if (frType == "CatNum") {
    var <- c(var_cat, var_num, "..labels", "..colors")
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
  list_bar(data, frType)

}


#' @keywords internal
global_options <- function(sample){
  params <- makeup::which_num_format(sample)
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- params$separators$thousands
  hcoptslang$decimalPoint <- params$separators$decimal
  options(highcharter.lang = hcoptslang)
}
