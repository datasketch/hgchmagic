#' Extract Categorical and Numeric Data from a Data Frame
#'
#' This function takes a data frame and extracts categorical and numeric data from four specified columns.
#' It returns a list of data that can be used for visualizations.
#'
#' @param data A data frame.
#'
#' @return A list containing a data frame that has the following columns: "name", "y", "..label", and "..color".
#'
#' @examples
#' data(mtcars)
#' list_cat_num(mtcars)
#'
#' @import purrr
#'
#' @export
list_bar <- function(data, frtype) {
  d <- data
  if (frtype %in% c("Cat", "CatNum")) {
    data <- purrr::pmap(.l = list(d[[1]], d[[2]], d[[3]], d[[4]]),
                        .f = function(name, y, label, color) {
                          list("name" = as.character(name), "y" = as.numeric(y),
                               "label" = label, "color" = color)
                        })

    data <- list(data = data)
  }
  if (frtype %in% c("CatCat", "CatCatNum")) {
    d$..labels <- as.character(d$..labels)
    data_groups <- list(unique(d[[1]]),
                        split(d[complete.cases(
                          d[,c(setdiff(names(d), names(d)[1]))]),], d[[1]]))
    data <- list(
    categories = purrr::map(as.character(unique(d[[2]])), function(z) z),
    data = purrr::pmap(.l = data_groups, function(name, d0) {
      label_info <- pluck(d0, "..labels") %>% unlist()
      list("name" = as.character(name),
           "color" = unique(d0$..colors),
           "index" = unique(d0$..index),
           "legendIndex" = d0$..legendIndex,
           "data" = purrr::map2(label_info, as.numeric(d0[[3]]),
                                ~ list("label" = .x, "y" = .y)))
    })
    )


  }

  data
}


list_treemap <- function(data, frtype) {
  d <- data
  if (frtype %in% c("Cat", "CatNum")) {
    data <- purrr::pmap(
      list(d[[1]], d[[2]], d$..labels, d$..colors),
      function(name, value, label, color) {
        list(
          "name" = name,
          "value" = value,
          "label" = label,
          "color" = as.character(color)
        )
      }
    )
  }
}
