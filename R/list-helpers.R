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
    axis_cat <- unique(d[[2]])
    if (all(grepl("^[0-9]+$", d[[2]]))) {
      axis_cat <- sort(unique(d[[2]]))
    }

    data_groups <- list(unique(d[[1]]),
                        split(d[complete.cases(
                          d[,c(setdiff(names(d), names(d)[1]))]),], d[[1]]))

    data <- list(
      categories = purrr::map(as.character(axis_cat), function(z) z),
      data = purrr::map(unique(d[[1]]), function(i) {
        d0 <- d |>
          dplyr::filter(!!sym(names(d)[1]) %in% i) |>
          dplyr::arrange(..index)
        label_info <- d0 %>% .$..labels %>% unlist()
        l0 <- list("name" = i,
                   "color" = unique(d0$..colors),
                   "legendIndex" = unique(d0$..legendIndex),
                   "data" = purrr::map(seq_along(d0[[3]]), function(i){
                     list("label" =  label_info[i],
                          "y" = d0[[3]][i]
                     )
                   })
        )
      })
    )


  }

  if (frtype %in% c("CatNumNum")) {
    color <- unique(d$..colors)
    if (length(color) != 2) {
      color <- strsplit(color, split = "-") |> unlist()
    }
    if (length(unique(d[[1]])) > 1) {
      series <- map(c(2,3), function(col) {
        list(
          name = names(d)[col],
          color = color[col-1],
          type = "bar",
          yAxis = col - 2,
          data = d[[col]]
        )
      })
    } else {
      series <- map(c(2,3), function(col) {
        list(
          name = names(d)[col],
          color = color[col-1],
          type = "bar",
          yAxis = col - 2,
          data = list(d[[col]])
        )
      })

    }
    data <- list(
      title_axis = names(d)[2:3],
      categories = purrr::map(unique(d[[1]]), ~as.character(.x)),
      data = series
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

  if (frtype %in% c("CatCat", "CatCatNum")) {
    var_cat <- names(d)[1]
    list_id <- purrr::map(unique(d[[1]]), function(i) {
      d0 <- d |> filter(!!sym(var_cat) %in% i)
      list(
        id = i,
        name = i,
        color = unique(d0$..colors)
      )
    })

    list_cats <- purrr::map(1:nrow(d), function(z) {
      nm <- ifelse(is.na(d[[2]][z]), "NA", d[[2]][z])
      list(
        name = nm,
        parent = d[[1]][z],
        value = d[[3]][z],
        label = d$..labels[z]#,
        #colorValue = d[[3]][z]
      )
    })
    data <- c(list_id, list_cats)
  }

  data

}

#' @keywords internal
list_line <- function(data, frtype) {

  d <- data
  if (frtype %in% c("Dat", "DatNum")) {
    dl <- d |> select(y = 2, label = ..labels)
    data <- list(
      categories = unique(d[[1]]),
      data = list(data = purrr::transpose(dl),
                  color = d$..colors |> unique())
    )
  }

  if (frtype %in% c("CatDat", "CatDatNum")) {
    var_cat <- names(data)[1]
    data_groups <- purrr::map(unique(d[[1]]), ~ d |> filter(!!sym(var_cat) %in% .x))
    names(data_groups) <- unique(d[[1]])
    data_groups <- list(unique(d[[1]]), data_groups)
    series <- purrr::pmap(.l = data_groups, function(name, d0) {
      names(d0)[3] <- "y"
      dl <- d0 |> transmute(y, label = ..labels, color = ..colors)
      list(data = purrr::transpose(dl), name = name, color = unique(dl$color))
    })
    data <- list(
      categories = unique(d[[2]]),
      data = series
    )
  }

  if (frtype %in% c("DatNumNum")) {
    color <- unique(d$..colors)
    series <- map(c(2,3), function(col) {
      list(
        name = names(d)[col],
        color = color[col-1],
        type = "line",
        yAxis = col - 2,
        data = d[[col]]
      )
    })

    data <- list(
      title_axis = names(d)[2:3],
      categories = unique(d[[1]]),
      data = series
    )
  }

  data

}

#' @keywords internal
list_scatter <- function(data, frtype) {

  d <- data


  if (frtype %in% "NumNum") {
    if ("x" %in% names(d)) d <- d |> rename(x1 = x)
    if ("y" %in% names(d)) d <- d |> rename(y1 = y)
    names(d)[1:2] <- c("x", "y")
    d <- d |>
      select(x,
             y,
             color = ..colors,
             label = ..labels) |>
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

      d0 <- d |>
        dplyr::filter(!!sym(var_cat) %in% i)

      d0 <- d0 |>
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

      d0 <- d |>
        dplyr::filter(!!sym(var_cat) %in% i)

      d0 <- d0 |>
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

  d <- d |> select(from, to, weight, ..colors, ..labels)
  data <- purrr::pmap(d, function(from, to, weight, ..colors, ..labels) {
    list(from = from,
         to = to,
         weight = weight,
         color = ..colors,
         label = ..labels)
  })
  nodes <- purrr::map(1:nrow(d), function(r){
    list(
      id = d$from[r],
      color = d$..colors[r]
    )
  })

  data <- list(
    data = data,
    nodes = nodes
  )

}
