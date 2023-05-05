#' Highcharter line Chart
#'
#' Creates a line chart using Highcharter library.
#'
#' @param data a data frame containing the variables to be plotted.
#' @param dic a named list, each element corresponding to a column name in \code{data} and defining a set of labels for that column.
#' @param var_cat the name of the categorical variable in the data frame
#' @param var_dat a character vector with the names of date variable.
#' @param var_num a character vector with the names of numerical variables.
#' @param ... additional arguments to be passed to \code{\link[dsvizopts]{dsviz_default_opts}}
#'
#' @return a Highcharter line chart.
#'
#' @seealso \code{\link{data_draw}}
#'
#' @examples
#' dates <- seq(as.POSIXct("2022-01-01"),
#'              as.POSIXct("2022-01-10"),
#'              by = "day")
#' values <- rnorm(length(dates))
#' df <- data.frame(date = dates, value = values)
#'
#' hgch_line(df, var_dat = "date", var_num = "value")
#'
#'### Custom colors
#' data <- lubridate::lakers
#' data$date <- lubridate::ymd(data$date)
#' data <- data |>
#'   dplyr::group_by(game_type, date) |>
#'   dplyr::summarise(x = sum(x, na.rm = T))
#'
#' hgch_line(data,
#'           var_cat = "game_type",
#'           var_dat = "date",
#'           var_num = "x",
#'           palette_colors = c("#ffa92a", "#f06142"),
#'           hor_title = "fecha",
#'           ver_title = "valor")
#'
#' # Custom theme
#'  test_theme <- list(
#'    theme = list(
#'      palette_colors = c("#ffa92a", "lightgreen"),
#'      subtitle_align = "center",
#'      subtitle_family = "Roboto",
#'      subtitle_size = 15,
#'      subtitle_color = "#3b83b8",
#'      subtitle_weight = 700
#'    )
#'  )
#'
#' # plot
#'  hgch_line(data,
#'            var_cat = "game_type",
#'            var_dat = "date",
#'            var_num = "x",
#'            hor_title = "fecha",
#'            ver_title = "valor",
#'            opts = test_theme)
#'
#' @export
hgch_line <- function (data, dic = NULL, var_cat = NULL, var_dat = NULL, var_num = NULL, ...) {

  if (is.null(var_dat)) stop("You must enter at least one categorical variable")

  frType <- frtype_viz(var_date = var_dat, var_num = var_num, var_cat = var_cat)
  opts <- plot_opts(viz = "line", frType = frType, ...)
  opts$data_opts$color_by <- var_cat
  count_var_num <- stringr::str_count(frType, "Num")

  if (count_var_num > 1) {
    opts$data_opts$color_by <- NULL
    if (!"..colors" %in% names(data)) {
      colors <- opts$data_opts$palette_colors[1:count_var_num]
      data$..colors <- rep_len(colors, nrow(data))
    }
  }


  data_draw <- data_draw(data = data,
                         dic = dic,
                         var_cat = var_cat,
                         var_num = var_num,
                         var_date = var_dat,
                         frType = frType,
                         opts = opts$data_opts,
                         viz = "line")

  line <- highchart() |>
    hc_titles(opts = opts$titles)  |>
    hc_body_line(data = data_draw, frType = frType, opts = opts$general_opts) |>
    hc_add_theme(hgch_theme(opts = opts$theme))

  line

}



#' @export
hgch_line_Dat <- function(data, ...) {
  var_dat <- names(data)[1]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% "Count"
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "count",
                                       group_var = var_dat,
                                       agg_name = opts_prep$agg_text %||% var_num_name,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% "..percentage"
  }
  hgch_line(data = data, var_dat = var_dat, var_num = var_num_name, ...)
}


#' @export
hgch_line_DatNum <- function(data, ...) {
  var_dat <- names(data)[1]
  var_num <- names(data)[2]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num

  if (opts_prep$agg_add) {
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = opts_prep$agg,
                                       agg_name = var_num_name,
                                       group_var = var_dat,
                                       to_agg = var_num,
                                       na_rm = opts_prep$na_rm,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  }

  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% paste0("..percentage ", var_num)
  }
  hgch_line(data = data, var_dat = var_dat, var_num = var_num_name, ...)
}


#' @export
hgch_line_CatDat <- function(data, ...) {
  var_cat <- names(data)[1]
  var_dat <- names(data)[2]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% "Count"
  data <- dsdataprep::aggregation_data(data = data,
                                       agg = "count",
                                       group_var = c(var_cat, var_dat),
                                       agg_name = opts_prep$agg_text %||% var_num_name,
                                       na_rm = opts_prep$na_rm,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       percentage_col = opts_prep$percentage_col,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% "..percentage"
  }
  hgch_line(data = data, var_cat = var_cat, var_dat = var_dat, var_num = var_num_name, ...)
}


#' @export
hgch_line_CatDatNum <- function(data, ...) {
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
                                       na_rm = opts_prep$na_rm,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       percentage_col = opts_prep$percentage_col,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  }
  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% paste0("..percentage ", var_num)
  }
  hgch_line(data = data, var_cat = var_cat, var_dat = var_dat, var_num = var_num_name, ...)
}


#' @export
hgch_line_DatNumNum <- function(data, ...) {
  var_dat <- names(data)[1]
  var_num <- names(data)[2:3]
  opts_prep <- dataprep_opts(...)
  var_num_name <- opts_prep$agg_text %||% var_num

  data <- dsdataprep::aggregation_data(data = data,
                                       agg = opts_prep$agg,
                                       agg_name = var_num_name,
                                       group_var = var_dat,
                                       to_agg = var_num,
                                       percentage = opts_prep$percentage,
                                       percentage_name = opts_prep$percentage_name,
                                       extra_col = opts_prep$extra_col,
                                       agg_extra = opts_prep$agg_extra)
  if (opts_prep$percentage) {
    var_num_name <- opts_prep$percentage_name %||% paste0("..percentage ", var_num)
  }
  hgch_line(data = data, var_dat = var_dat, var_num = var_num_name, ...)
}

