#' @name count_pl
#' @export
count_pl <- function(x) {
  if (is.na(x)) {return(0)}
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

#' Bar (ordered categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca
#' @examples
#' hgch_bar_Oca(sampleData("Cat", nrow = 10))
#' @export hgch_bar_Oca
hgch_bar_Oca <- function(data,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         horLabel = NULL,
                         verLabel = NULL,
                         yLine = NULL,
                         yLineLabel = NULL,
                         dropNa = FALSE,
                         order = NULL,
                         orientation = "ver",
                         sort = "no",
                         sliceN = NULL,
                         theme = NULL,
                         export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[1]
  verLabel <- verLabel %||% paste("count", nms[1])
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  order <- union(order, unique(d$a)[!is.na(unique(d$a))])
  if (all(!is.na(order)) & any(is.na(d$a))) order <- c(union(order, unique(d$a[!is.na(d$a)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$a, order)), ]

  if (sort == "desc")
    d <- d %>%
    dplyr::arrange(desc(b))

  if (sort == "asc")
    d <- d %>%
    dplyr::arrange(b)

  if (!is.null(sliceN))
    d <- d %>%
    dplyr::slice(1:sliceN)

  if (orientation == "ver")
    hc <-  hchart(d, type = "column", hcaes(x = a, y = b))

  if (orientation == "hor")
    hc <- hchart(d, type = "bar", hcaes(x = a, y = b))

  hc <- hc %>%
    hc_tooltip(headerFormat = paste("<b>", paste0(horLabel, ": "), "</b>{point.key}<br/>"),
               pointFormat = paste0("<b>", verLabel, "</b>: {point.b}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = yLine,
                                                                  color = 'black',
                                                                  dashStyle = 'shortdash',
                                                                  width = 2,
                                                                  label = list(text = yLineLabel)))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Bar (ordered categories, numbers)
#'
#' Compare quantities among categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca-Num
#' @examples
#' hgch_bar_OcaNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_bar_OcaNum
hgch_bar_OcaNum <- function(data,
                            title = NULL,
                            subtitle = NULL,
                            caption = NULL,
                            horLabel = NULL,
                            verLabel = NULL,
                            yLine = NULL,
                            yLineLabel = NULL,
                            agg = "sum",
                            dropNa = FALSE,
                            order = NULL,
                            orientation = "ver",
                            percentage = FALSE,
                            sort = "no",
                            sliceN = NULL,
                            theme = NULL,
                            export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[1]
  verLabel <- verLabel %||% ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(agg, nms[2]))
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b))

  if (percentage) {
    d <- d %>%
      dplyr::mutate(b = b / sum(b))
    verLabel <- paste("%", verLabel)
  }

  order <- union(order, unique(d$a)[!is.na(unique(d$a))])
  if (all(!is.na(order)) & any(is.na(d$a))) order <- c(union(order, unique(d$a[!is.na(d$a)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$a, order)), ]

  if (sort == "desc")
    d <- d %>%
    dplyr::arrange(desc(b))

  if (sort == "asc")
    d <- d %>%
    dplyr::arrange(b)

  if (!is.null(sliceN))
    d <- d %>%
    dplyr::slice(1:sliceN)

  if (orientation == "ver")
    hc <-  hchart(d, type = "column", hcaes(x = a, y = b))

  if (orientation == "hor")
    hc <- hchart(d, type = "bar", hcaes(x = a, y = b))

  hc <- hc %>%
    hc_tooltip(headerFormat = paste("<b>", paste0(horLabel, ": "), "</b>{point.key}<br/>"),
               pointFormat = paste0("<b>", verLabel, "</b>: {point.b}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = yLine,
                                                                  color = 'black',
                                                                  dashStyle = 'shortdash',
                                                                  width = 2,
                                                                  label = list(text = yLineLabel))),
             labels = list(format = ifelse(percentage, "{value}%", "{value}"))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Bar (years, numbers)
#'
#' Compare quantities over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Yea-Num
#' @examples
#' hgch_bar_YeaNum(sampleData("Yea-Num", nrow = 10))
#' @export hgch_bar_YeaNum
hgch_bar_YeaNum <- hgch_bar_OcaNum


#' Bar (dates, numbers)
#'
#' Compare quantities over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Dat-Num
#' @examples
#' hgch_bar_DatNum(sampleData("Dat-Num", nrow = 10))
#' @export hgch_bar_DatNum
hgch_bar_DatNum <- hgch_bar_OcaNum


#' Grouped bar (categories, ordered categories)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca
#' @examples
#' hgch_bar_grouped_CatOca(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bar_grouped_CatOca
hgch_bar_grouped_CatOca <- function(data,
                                    title = NULL,
                                    subtitle = NULL,
                                    caption = NULL,
                                    horLabel = NULL,
                                    verLabel = NULL,
                                    yLine = NULL,
                                    yLineLabel = NULL,
                                    dropNa = FALSE,
                                    order = NULL,
                                    orientation = "ver",
                                    sort = "no",
                                    sliceN = NULL,
                                    theme = NULL,
                                    export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[2]
  verLabel <- verLabel %||% ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[1], paste("count", nms[1]))
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n())

  order <- union(order, unique(d$b)[!is.na(unique(d$b))])
  if (all(!is.na(order)) & any(is.na(d$b))) order <- c(union(order, unique(d$b[!is.na(d$b)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$b, order)), ]

  if (sort == "desc")
    d <- d %>%
    dplyr::arrange(desc(b))

  if (sort == "asc")
    d <- d %>%
    dplyr::arrange(b)

  if (!is.null(sliceN))
    d <- d %>%
    dplyr::slice(1:sliceN)

  if (orientation == "ver")
    hc <-  hchart(d, type = "column", hcaes(x = b, y = c, group = a))

  if (orientation == "hor")
    hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a))

  hc <- hc %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b>", paste0(nms[1], ": "), "</b>{point.a}<br/><b>",
                                    paste0(horLabel, ": "), "</b>{point.b}<br/><b>",
                                    verLabel, "</b>: {point.c}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = yLine,
                                                                  color = 'black',
                                                                  dashStyle = 'shortdash',
                                                                  width = 2,
                                                                  label = list(text = yLineLabel)))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Stacked bar (categories, ordered categories)
#'
#' Compare stacked categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca
#' @examples
#' hgch_bar_stacked_CatOca(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bar_stacked_CatOca
hgch_bar_stacked_CatOca <-  function(data,
                                     title = NULL,
                                     subtitle = NULL,
                                     caption = NULL,
                                     horLabel = NULL,
                                     verLabel = NULL,
                                     yLine = NULL,
                                     yLineLabel = NULL,
                                     dropNa = FALSE,
                                     order = NULL,
                                     orientation = "ver",
                                     sort = "no",
                                     sliceN = NULL,
                                     theme = NULL,
                                     export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[2]
  verLabel <- verLabel %||% ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[1], paste("count", nms[1]))
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n())

  order <- union(order, unique(d$b)[!is.na(unique(d$b))])
  if (all(!is.na(order)) & any(is.na(d$b))) order <- c(union(order, unique(d$b[!is.na(d$b)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$b, order)), ]

  if (sort == "desc")
    d <- d %>%
    dplyr::arrange(desc(b))

  if (sort == "asc")
    d <- d %>%
    dplyr::arrange(b)

  if (!is.null(sliceN))
    d <- d %>%
    dplyr::slice(1:sliceN)

  if (orientation == "ver")
    hc <-  hchart(d, type = "column", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(column = list(stacking = "normal"))

  if (orientation == "hor")
    hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(bar = list(stacking = "normal"))

  hc <- hc %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b>", paste0(nms[1], ": "), "</b>{point.a}<br/><b>",
                                    paste0(horLabel, ": "), "</b>{point.b}<br/><b>",
                                    verLabel, "</b>: {point.c}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = yLine,
                                                                  color = 'black',
                                                                  dashStyle = 'shortdash',
                                                                  width = 2,
                                                                  label = list(text = yLineLabel)))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' 100% stacked bar (categories, ordered categories)
#'
#' Compare 100% stacked categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca
#' @examples
#' hgch_bar_stacked_100_CatOca(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bar_stacked_100_CatOca
hgch_bar_stacked_100_CatOca <-  function(data,
                                         title = NULL,
                                         subtitle = NULL,
                                         caption = NULL,
                                         horLabel = NULL,
                                         verLabel = NULL,
                                         yLine = NULL,
                                         yLineLabel = NULL,
                                         dropNa = FALSE,
                                         order = NULL,
                                         orientation = "ver",
                                         sort = "no",
                                         sliceN = NULL,
                                         theme = NULL,
                                         export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[2]
  verLabel <- verLabel %||% ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[1], paste("count", nms[1]))
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n())

  order <- union(order, unique(d$b)[!is.na(unique(d$b))])
  if (all(!is.na(order)) & any(is.na(d$b))) order <- c(union(order, unique(d$b[!is.na(d$b)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$b, order)), ]

  if (sort == "desc")
    d <- d %>%
    dplyr::arrange(desc(b))

  if (sort == "asc")
    d <- d %>%
    dplyr::arrange(b)

  if (!is.null(sliceN))
    d <- d %>%
    dplyr::slice(1:sliceN)

  if (orientation == "ver")
    hc <-  hchart(d, type = "column", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(column = list(stacking = "percent"))

  if (orientation == "hor")
    hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(bar = list(stacking = "percent"))

  hc <- hc %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "</b>{point.a}<br/><b style = 'font-size:12px'>",
                                    paste0(horLabel, ": "), "</b>{point.b}<br/><b style = 'font-size:12px'>",
                                    verLabel, "</b>: {point.c} ({point.percentage:.3f}%)")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = yLine,
                                                                  color = 'black',
                                                                  dashStyle = 'shortdash',
                                                                  width = 2,
                                                                  label = list(text = yLineLabel))),
             labels = list(format = "{value}%")) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Grouped bar (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca-Num
#' @examples
#' hgch_bar_grouped_CatOcaNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_bar_grouped_CatOcaNum
hgch_bar_grouped_CatOcaNum <- function(data,
                                       title = NULL,
                                       subtitle = NULL,
                                       caption = NULL,
                                       horLabel = NULL,
                                       verLabel = NULL,
                                       yLine = NULL,
                                       yLineLabel = NULL,
                                       agg = "sum",
                                       dropNa = FALSE,
                                       order = NULL,
                                       orientation = "ver",
                                       percentage = FALSE,
                                       sort = "no",
                                       sliceN = NULL,
                                       theme = NULL,
                                       export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[2]
  verLabel <- verLabel %||% ifelse(nrow(d) == dplyr::n_distinct(d$b), nms[3], paste(agg, nms[3]))
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(agg, c))

  if (percentage) {
    d <- d %>%
      dplyr::mutate(c = c / sum(c))
    verLabel <- paste("%", verLabel)
  }

  order <- union(order, unique(d$b)[!is.na(unique(d$b))])
  if (all(!is.na(order)) & any(is.na(d$b))) order <- c(union(order, unique(d$b[!is.na(d$b)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$b, order)), ]

  ## QUITAAR ESTO O QUé???
  if (sort == "desc")
    d <- d %>%
    dplyr::arrange(desc(c))

  if (sort == "asc")
    d <- d %>%
    dplyr::arrange(c)

  if (!is.null(sliceN))
    d <- d %>%
    dplyr::slice(1:sliceN)

  if (orientation == "ver")
    hc <-  hchart(d, type = "column", hcaes(x = b, y = c, group = a))

  if (orientation == "hor")
    hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a))

  hc <- hc %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b>", paste0(nms[1], ": "), "</b>{point.a}<br/><b>",
                                    paste0(horLabel, ": "), "</b>{point.b}<br/><b>",
                                    verLabel, "</b>: {point.c", ifelse(percentage, ":.3f}", "}"))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = yLine,
                                                                  color = 'black',
                                                                  dashStyle = 'shortdash',
                                                                  width = 2,
                                                                  label = list(text = yLineLabel))),
             labels = list(format = ifelse(percentage, "{value}%", "{value}"))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Grouped bar (categories, year, numbers)
#'
#' Compare quantities among categories over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_bar_grouped_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_bar_grouped_CatYeaNum
hgch_bar_grouped_CatYeaNum <- hgch_bar_grouped_CatOcaNum


#' Grouped bar (categories, dates, numbers)
#'
#' Compare quantities among categories over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_bar_grouped_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_bar_grouped_CatDatNum
hgch_bar_grouped_CatDatNum <- hgch_bar_grouped_CatOcaNum


#' Stacked bar (categories, ordered categories, numbers)
#'
#' Compare quantities among stacked categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca-Num
#' @examples
#' hgch_bar_stacked_CatOcaNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_bar_stacked_CatOcaNum
hgch_bar_stacked_CatOcaNum <- function(data,
                                       title = NULL,
                                       subtitle = NULL,
                                       caption = NULL,
                                       horLabel = NULL,
                                       verLabel = NULL,
                                       yLine = NULL,
                                       yLineLabel = NULL,
                                       agg = "sum",
                                       dropNa = FALSE,
                                       order = NULL,
                                       orientation = "ver",
                                       percentage = FALSE,
                                       sort = "no",
                                       sliceN = NULL,
                                       theme = NULL,
                                       export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[2]
  verLabel <- verLabel %||% ifelse(nrow(d) == dplyr::n_distinct(d$b), nms[3], paste(agg, nms[3]))
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(agg, c))

  if (percentage) {
    d <- d %>%
      dplyr::mutate(c = c / sum(c))
    verLabel <- paste("%", verLabel)
  }

  order <- union(order, unique(d$b)[!is.na(unique(d$b))])
  if (all(!is.na(order)) & any(is.na(d$b))) order <- c(union(order, unique(d$b[!is.na(d$b)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$b, order)), ]

  ## QUITAAR ESTO O QUé???
  if (sort == "desc")
    d <- d %>%
    dplyr::arrange(desc(c))

  if (sort == "asc")
    d <- d %>%
    dplyr::arrange(c)

  if (!is.null(sliceN))
    d <- d %>%
    dplyr::slice(1:sliceN)

  if (orientation == "ver")
    hc <-  hchart(d, type = "column", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(column = list(stacking = "normal"))

  if (orientation == "hor")
    hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(bar = list(stacking = "normal"))

  hc <- hc %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b>", paste0(nms[1], ": "), "</b>{point.a}<br/><b>",
                                    paste0(horLabel, ": "), "</b>{point.b}<br/><b>",
                                    verLabel, "</b>: {point.c", ifelse(percentage, ":.3f}", "}"))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = yLine,
                                                                  color = 'black',
                                                                  dashStyle = 'shortdash',
                                                                  width = 2,
                                                                  label = list(text = yLineLabel))),
             labels = list(format = ifelse(percentage, "{value}%", "{value}"))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Stacked bar (categories, years, numbers)
#'
#' Compare quantities among stacked categories over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_bar_stacked_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_bar_stacked_CatYeaNum
hgch_bar_stacked_CatYeaNum <- hgch_bar_stacked_CatOcaNum


#' Stacked bar (categories, dates, numbers)
#'
#' Compare quantities among stacked categories over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_bar_stacked_CatYeaNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_bar_stacked_CatDatNum
hgch_bar_stacked_CatDatNum <- hgch_bar_stacked_CatOcaNum


#' 100% stacked bar (categories, ordered categories, numbers)
#'
#' Compare quantities among 100% stacked categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca-Num
#' @examples
#' hgch_bar_stacked_100_CatOcaNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_bar_stacked_100_CatOcaNum
hgch_bar_stacked_100_CatOcaNum <- function(data,
                                           title = NULL,
                                           subtitle = NULL,
                                           caption = NULL,
                                           horLabel = NULL,
                                           verLabel = NULL,
                                           yLine = NULL,
                                           yLineLabel = NULL,
                                           agg = "sum",
                                           dropNa = FALSE,
                                           order = NULL,
                                           orientation = "ver",
                                           sliceN = NULL,
                                           theme = NULL,
                                           export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[2]
  verLabel <- verLabel %||% ifelse(nrow(d) == dplyr::n_distinct(d$b), nms[3], paste(agg, nms[3]))
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(agg, c))

  order <- union(order, unique(d$b)[!is.na(unique(d$b))])
  if (all(!is.na(order)) & any(is.na(d$b))) order <- c(union(order, unique(d$b[!is.na(d$b)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$b, order)), ]

  ## QUITAAR ESTO O QUé???
  if (!is.null(sliceN))
    d <- d %>%
    dplyr::slice(1:sliceN)

  if (orientation == "ver")
    hc <-  hchart(d, type = "column", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(column = list(stacking = "percent"))

  if (orientation == "hor")
    hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(bar = list(stacking = "percent"))

  hc <- hc %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "</b>{point.a}<br/><b style = 'font-size:12px'>",
                                    paste0(horLabel, ": "), "</b>{point.b}<br/><b style = 'font-size:12px'>",
                                    verLabel, "</b>: {point.c} ({point.percentage:.3f}%)")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = yLine,
                                                                  color = 'black',
                                                                  dashStyle = 'shortdash',
                                                                  width = 2,
                                                                  label = list(text = yLineLabel))),
             labels = list(format = "{value}%")) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' 100% stacked bar (categories, years, numbers)
#'
#' Compare quantities among 100% stacked categories over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_bar_stacked_100_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_bar_stacked_100_CatYeaNum
hgch_bar_stacked_100_CatYeaNum <- hgch_bar_stacked_100_CatOcaNum


#' 100% stacked bar (categories, dates, numbers)
#'
#' Compare quantities among 100% stacked categories over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_bar_stacked_100_CatYeaNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_bar_stacked_100_CatDatNum
hgch_bar_stacked_100_CatDatNum <- hgch_bar_stacked_100_CatOcaNum


#' Bar (ordered category, n numbers)
#'
#' Compare n quantities among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca-NumP
#' @examples
#' hgch_bar_grouped_OcaNumP(sampleData("Oca-NumP", nrow = 10))
#' @export hgch_bar_grouped_OcaNumP
hgch_bar_grouped_OcaNumP <- function(data,
                                     title = NULL,
                                     subtitle = NULL,
                                     caption = NULL,
                                     horLabel = NULL,
                                     verLabel = NULL,
                                     yLine = NULL,
                                     yLineLabel = NULL,
                                     agg = "sum",
                                     dropNa = FALSE,
                                     order = NULL,
                                     orientation = "ver",
                                     percentage = FALSE,
                                     sort = "no",
                                     sliceN = NULL,
                                     theme = NULL,
                                     export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)

  d <- d  %>%
    tidyr::gather(variable, value, -a) %>%
    dplyr::group_by(a, variable) %>%
    dplyr::summarise(value = agg(agg, value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(2, 1, 3)
  d <- dplyr::mutate(d, variable = fct_recode_df(d, "variable", codes))
  names(d)[2] <- nms[1]

  hc <- hgch_bar_grouped_CatOcaNum(data = d,
                                   title = title ,
                                   subtitle = subtitle,
                                   caption = caption,
                                   horLabel = horLabel,
                                   verLabel = verLabel,
                                   yLine = yLine,
                                   yLineLabel = yLineLabel,
                                   agg = agg,
                                   dropNa = dropNa,
                                   order = order,
                                   orientation = orientation,
                                   percentage = percentage,
                                   sort = sort,
                                   sliceN = sliceN,
                                   theme = theme,
                                   export = export, ...)
  hc
}


#' Bar (years, n numbers)
#'
#' Compare n quantities over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Yea-NumP
#' @examples
#' hgch_bar_grouped_YeaNumP(sampleData("Yea-NumP", nrow = 10))
#' @export hgch_bar_grouped_YeaNumP
hgch_bar_grouped_YeaNumP <- hgch_bar_grouped_OcaNumP


#' Bar (dates, n numbers)
#'
#' Compare n quantities over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Dat-NumP
#' @examples
#' hgch_bar_grouped_DatNumP(sampleData("Dat-NumP", nrow = 10))
#' @export hgch_bar_grouped_DatNumP
hgch_bar_grouped_DatNumP <- hgch_bar_grouped_OcaNumP



#' Waterfall
#'
#' Waterfall
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#' hgch_waterfall_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_waterfall_CatNum
hgch_waterfall_CatNum <-function(data, title = NULL,  xAxisTitle = NULL, caption = NULL,
                                 aggregation = "sum", yAxisTitle = NULL, subtitle = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  data <- f$d
  data <- plyr::rename(data, c("a" = "name"))

  data_graph <- data %>%
    dplyr::group_by(name) %>%
    tidyr::drop_na(name) %>%
    dplyr::summarise(value = mean(b, na.rm = TRUE ))

  data_graph <- data_graph %>%
    dplyr::mutate(x = 0:(dim(data_graph)[1]-1),
                  y = value,
                  z = (x*y) - median(x*y),
                  color = getPalette()[1:(dim(data_graph)[1])])

  data_graph <- data_graph %>%
    select(y, z, value, name, color) %>%
    arrange(value)


  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_xAxis(text = list(text = xAxisTitle)) %>%
    hc_yAxis(text = list(text = yAxisTitle)) %>%
    hc_chart(type = "waterfall",
             polar = FALSE) %>%
    hc_xAxis(categories = data_graph$name) %>%
    hc_add_series(data_graph, showInLegend = FALSE) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.key}</b><br/>"),
               pointFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.value}</b><br/>")) %>%
    hc_credits(enabled = TRUE, text = caption)
  hc
}



#' Circular bar
#'
#' Circular bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @section OtherInfo:
#' moreInfo
#' @examples
#' hgch_circular_bar_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_circular_bar_CatNum
hgch_circular_bar_CatNum <- function(data,
                                     title = NULL,
                                     subtitle = NULL,
                                     caption = NULL,
                                     theme = NULL,
                                     export = FALSE,
                                     aggregation = "sum",
                                     ...) {


  f <- fringe(data)
  nms <- getClabels(f)


  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  df <- f$d
  df <- df %>% plyr::rename(c('a' = 'name', 'b' = 'y'))

  df <- df %>%
    group_by(name) %>%
    dplyr::summarise(y = agg(aggregation, y))

  if(length(is.na(df$name) > 0)) {
    df$name[is.na(df$name)] <- 'Na'
  }

  df <- df %>%
    drop_na(y) %>%
    arrange(-y) %>%
    dplyr::mutate(y = round((y/sum(y)) * 100, 2))
  #df$color <- map(0:(dim(df)[1] - 1), function(i) JS(paste0("Highcharts.getOptions().colors[",i,"]")))
  set.seed(23)
  df$color <- map(getPalette()[1:dim(df)[1]], function(i) i)


  rD <- function(n_rows){
    vc <- c()

    if(n_rows < 4){
      vi <- 112
    }else{
      vi <-  80 #187
    }

    if(n_rows < 4){
      dif <- 25
    }else{
      dif <- 10 #187
    }
    for(i in 1:n_rows){
      if(i==1){
        vc[i] <- vi - dif
      }else{
        nv1 <- vc[i-1]
        Nv <- nv1 - dif
        vc[i] <- Nv
      }
    }
    vc
  }


  df$radius <- rD(n_rows = dim(df)[1])
  df$innerRadius <- df$radius - ifelse(dim(df)[1] < 4, 24, 9)
  df$radius <- paste0(df$radius, '%')
  df$innerRadius <- paste0(df$innerRadius, '%')
  df$borderColor <- getPalette()[1:dim(df)[1]]


  a <- df %>%
    dplyr::group_by(name,borderColor) %>%
    do(data =  transpose(c(color = .$color, radius = .$radius, y = .$y, innerRadius = .$innerRadius)))
  #a$borderColor <- df$color


  xx <- list_parse(a)

  outerRadius <- df$radius
  innerRadius <- df$innerRadius
  #backgroundColor <-  map(0:(dim(df)[1] - 1),function(i) JS(paste0("Highcharts.Color(Highcharts.getOptions().colors[",i, "]).setOpacity(0.3).get()")))
  backgroundColor <-  map(getPalette()[1:dim(df)[1]],
                          function(i) JS(paste0("Highcharts.Color(",paste0("'",i,"'"),").setOpacity(0.3).get()")))

  #map(0:(dim(df)[1] - 1), function(i) JS(paste0("Highcharts.Color(Highcharts.getOptions().colors[",i,"]).setOpacity(0.3).get()")))
  borderWidth <- rep(0, dim(df)[1])

  res <- list(outerRadius = outerRadius,
              innerRadius = innerRadius,
              backgroundColor = backgroundColor,
              borderWidth = borderWidth) %>% transpose()


  #style = list(width = '1500px', height = '1500px', margin = '0 auto')

  hc <- highchart() %>%
    hc_chart(type = "solidgauge") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(borderWidth = 0,backgroundColor = 'none',shadow = FALSE,style = list(fontSize = '16px'),
               pointFormat = '{series.name}<br><span style="font-size:2em; color: {point.color}; font-weight: bold">{point.y}%</span>',
               positioner = JS("function (labelWidth, labelHeight) {return {x: 200 - labelWidth / 2,y: 180};}")) %>%
    hc_pane(startAngle = 0,endAngle = 360,
            background = res ) %>%
    hc_yAxis(min = 0,max = 100,lineWidth = 0,tickPositions = list()) %>%
    hc_plotOptions(solidgauge = list(borderWidth = '2px',dataLabels = list(enabled = FALSE),linecap = 'round',stickyTracking = FALSE)) %>%
    hc_add_series_list(xx)  %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_credits(enabled = TRUE, text = caption)

  hc

}

