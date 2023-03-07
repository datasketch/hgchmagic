#' Extract Categorical and Numeric Data from a Data Frame
#'
#' This function takes a data frame and extracts categorical and numeric data from four specified columns.
#' It returns a list of data that can be used for visualizations.
#'
#' @param data A data frame.
#' @param frType a string indicating the format of the data frame (e.g., "CatNum", "DatNum", "CatCatNum")
#'
#' @return A list containing a data frame that has the following columns: "name", "y", "..label", and "..color".
#'
#' @importFrom purrr map pmap pluck
#'
#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
list_line <- function(data, frtype) {

  d <- data
  if (frtype %in% c("Dat", "DatNum")) {
    dl <- d %>%
      mutate(y = .[[2]], label = ..labels) |>
      select(y, label)
    data <- list(
      categories = unique(d[[1]]),
      data = list(data = purrr::transpose(dl), color = d$..colors[1])
    )
  }

  if (frtype %in% c("CatDat", "CatDatNum")) {
    data_groups <- list(unique(d[[1]]),
                        split(d[complete.cases(
                          d[,c(setdiff(names(d), names(d)[1]))]),], d[[1]]))

    series <- purrr::pmap(.l = data_groups, function(name, d0) {
      dl <- d0 %>% transmute(y = .[[3]], label = ..labels, color = ..colors)
      list(data = purrr::transpose(dl), name = name, color = unique(dl$color))
    })
    data <- list(
      categories = unique(d[[2]]),
      data = series
    )
  }

  data

}

#' @keywords internal
list_scatter <- function(data, frtype) {

  d <- data


  if (frtype %in% "NumNum") {
    d <- d %>%
      mutate(x = `[[`(., 1),
             y = `[[`(., 2),
             color = ..colors,
             label = ..labels) %>%
      select(x, y, color, label) %>%
      as.list()
    data <- purrr::pmap(d, function(x, y, color, label) {
      list(x = x,
           y = y,
           color = color,
           label = label)
    })
  }

  if (frtype %in% c("CatNumNum")) {
    data <- purrr::map(unique(d[[1]]), function(i) {
      var_cat <- names(d)[1]

      d0 <- d %>%
        dplyr::filter(!!sym(var_cat) %in% i)

      d0 <- d0 %>%
        mutate(
          x = as.numeric(d0[[2]]),
          y = as.numeric(d0[[3]]),
          color = ..colors,
          label = ..labels
        ) |>
        select(x, y, color, label)
      l <- list(name = i,
                color = unique(d0$color),
                marker = list(symbol = "circle", radius = 4),
                data = purrr::transpose(d0)
      )
    })
  }

  if (frtype %in% c("CatDat", "CatDatNum")) {
    data <- purrr::map(unique(d[[1]]), function(i) {
      var_cat <- names(d)[1]

      d0 <- d %>%
        dplyr::filter(!!sym(var_cat) %in% i)

      d0 <- d0 %>%
        mutate(
          x = as.numeric(as.POSIXct(d0[[2]], format="%Y-%m-%d"))*1000,
          date = as.character(d0[[2]]),
          y = as.numeric(d0[[3]]),
          color = ..colors,
          label = ..labels
        )
      l <- list(name = i,
                color = unique(d0$..colors),
                marker = list(symbol = "circle", radius = 4),
                data = purrr::transpose(d0)
      )
    })
  }

  data
}



#' @keywords internal
list_sankey <- function(data, frtype) {

  d <- data

  if (!any(grepl("Num", frtype))) {
    d <- d |> select(from, to, weight, ..colors, ..labels)
    data <- purrr::pmap(d, function(from, to, weight, ..colors, ..labels) {
      list(from = from,
           to = to,
           weight = weight,
           color = ..colors,
           label = ..labels)
    })
  }

  data

}
