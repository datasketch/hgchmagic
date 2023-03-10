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
#' @export
hgch_sankey <- function (data, dic = NULL, var_cat = NULL, var_num = NULL, ...) {

  opts <- plot_opts(viz = "sankey", ...)

  opts$data_opts$color_by <- var_cat[1]

  frType <- frtype_viz(var_num = var_num, var_cat = var_cat)
  data_draw <- data_draw(data = data,
                         dic = dic,
                         var_cat = var_cat,
                         var_num = var_num,
                         frType = frType,
                         opts = opts$data_opts,
                         viz = "sankey")


  sankey <- highchart() |>
    hc_titles(opts = opts$titles)  |>
    hc_body_sankey(data = data_draw, frType = frType, opts = opts$general_opts)#|>
  # |>

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

