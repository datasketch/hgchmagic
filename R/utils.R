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
completevalues <- function(d, var_num) {
  d <- d[,1:3]
  var_num <- rlang::sym(var_num)
  var_one <- names(d)[1]
  var_two <- names(d)[2]
  d[[1]] <- as.character(d[[1]])
  d[[2]] <- as.character(d[[2]])
  d[[1]][d[[2]] == "NA"] <- NA
  d[[2]][d[[2]] == "NA"] <- NA
  d <- d |>
    tidyr::spread({{var_two}}, !!var_num) %>%
    tidyr::gather({{var_two}}, !!var_num, -{{var_one}})
  d[[1]][d[[2]] == "NA"] <- NA
  d[[2]][d[[2]] == "NA"] <- NA
  d
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


  var <- c(var_cat, var_date, var_num, "..labels", "..colors")
  var_order <- c(var_cat, var_date, var_num)
  data <- data |> select({{ var_order }}, everything())
  index_names <- NULL


  if (!is.null(var_num)) {
    data[,var_num] <- round(data[,var_num], opts$n_digits)
  }

  if (viz == "sankey") {
    if (!any(grepl("Num", frType))) {
      data <- data |> select({{ var_cat }}) #|> tidyr::drop_na()
      data <- data_to_sankey(data)
      var_cat <- c("from", "to")
      opts$color_by <- "from"
      var <- c("from", "to", "weight", "..colors", "..labels")
    } else {
      if (frType %in% "CatCatNum") {
        var_sel <- c(var_cat, var_num)
        data <- data |> select({{ var_sel }}) #|> tidyr::drop_na()
        names(data) <- c("from", "to", "weight")
        var_cat <- c("from", "to")
        opts$color_by <- "from"
        var <- c("from", "to", "weight", "..colors", "..labels")
      }
    }
  }


  # if (viz %in% c("line")) {
  #   opts$na_label <- NA
    if (grepl("CatDat|CatCat", frType)) {
      data_all <- completevalues(data, var_num = var_num)
      data[[2]] <- as.character(data[[2]])
      data <- data_all |> left_join(data[,-3])
    }
  #}


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
                                        palette = opts$palette,
                                        na_color = opts$na_color,
                                        na_label = opts$na_label
                                        )
  }


  if (frType == "CatCatNum") {
    index_names <- c("..index", "..legendIndex")
    var_sel <- c(var_cat, var_num)
    if (viz == "sankey") var_sel <-  var <- c("from", "to", "weight")
    var <- c(var_sel, "..labels", "..colors", "..index", "..legendIndex")
  }

  data <- dsdataprep::wrap_sort_data(data = data,
                                     col_cat = var_cat,#opts$sort_by,
                                     col_num = var_num,
                                     order = opts$order,
                                     order_legend = opts$order_legend,
                                     label_wrap = opts$label_wrap,
                                     label_wrap_legend = opts$label_wrap_legend,
                                     new_line = opts$new_line,
                                     sort = opts$sort,
                                     #sort_by_cat = opts$sort_by_cat,
                                     slice_n = opts$slice_n,
                                     intra_cat = opts$intra_cat,
                                     index_names = index_names)




  data <- data |> select({{ var }}, everything())

  ld <- NULL
  if (viz == "treemap") ld <- list_treemap(data, frType)
  if (viz == "line") ld <- list_line(data, frType)
  if (viz == "scatter") ld <- list_scatter(data, frType)
  if (viz == "sankey") ld <- list_sankey(data, frType)
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


