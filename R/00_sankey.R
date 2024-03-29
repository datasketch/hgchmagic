#' Highcharter sankey Chart
#'
#' Creates a sankey chart using Highcharter library.
#'
#' @param data a data frame containing the variables to be plotted.
#' @param dic a named list, each element corresponding to a column name in \code{data} and defining a set of labels for that column.
#' @param var_cat the names of the categorical variables in the data frame
#' @param var_num a character vector with the name of numerical variable.
#' @param ... additional arguments to be passed to \code{\link[dsvizopts]{dsviz_default_opts}}
#'
#' @return a Highcharter sankey chart.
#'
#' @seealso \code{\link{data_draw}}
#'
#' @examples
#' data <- ggplot2::diamonds
#' data <- data |>
#'   dplyr::group_by(cut, clarity) |>
#'   dplyr::summarise(total = sum(z, na.rm = T))
#'
#' hgch_sankey(data, var_cat = c("cut", "clarity"), var_num = "total")
#'
#' ### Adding style to plot
#' test_theme <- list(
#'   theme = list(
#'     palette_colors = "#ffa92a",
#'     caption_align = "right",
#'     caption_family = "Roboto",
#'     caption_size = 15,
#'     caption_color = "#3b83b8",
#'     caption_weight = 700
#'   )
#' )
#'
#' data <- dplyr::starwars
#' data <- data |>
#'   dplyr::select(hair_color, sex) |>
#'   tidyr::drop_na()
#'
#' hgch_sankey_CatCat(data,
#'                    opts = test_theme,
#'                    caption = "theme caption")
#'
#' @export
hgch_sankey <- function (data, dic = NULL, var_cat = NULL, var_num = NULL, ...) {


  frType <- frtype_viz(var_num = var_num, var_cat = var_cat)
  opts <- plot_opts(viz = "sankey", frType = frType, ...)
  opts$data_opts$color_by <- var_cat[1]

  data_draw <- data_draw(data = data,
                         dic = dic,
                         var_cat = var_cat,
                         var_num = var_num,
                         frType = frType,
                         opts = opts$data_opts,
                         viz = "sankey")

  global_options(opts$general_opts$format_sample_num)
  sankey <- highchart() |>
    hc_titles(opts = opts$titles)  |>
    hc_body_sankey(data = data_draw, frType = frType, opts = opts$general_opts) |>
    hc_add_theme(hgch_theme(opts = opts$theme))

  sankey

}



#' @export
hgch_sankey_CatCat <- function(data, ...) {
  var_cat <- c(names(data)[1:2])
  hgch_sankey(data = data, var_cat = var_cat,  ...)
}


#' @export
hgch_sankey_CatCatCat <- function(data, ...) {
  var_cat <- c(names(data)[1:3])
  hgch_sankey(data = data, var_cat = var_cat,  ...)
}


#' @export
hgch_sankey_CatCatCatCat <- function(data, ...) {
  var_cat <- c(names(data)[1:4])
  hgch_sankey(data = data, var_cat = var_cat,  ...)
}

#' @export
hgch_sankey_CatCatNum <- function(data, ...) {
  var_cat <- c(names(data)[1:2])
  var_num <- c(names(data)[3])
  hgch_sankey(data = data, var_cat = var_cat, var_num = var_num,  ...)
}

