#' Vertical area (ordered categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca
#' @examples
#' hgch_area_Oca(sampleData("Oca", nrow = 10))
#' @export hgch_area_Oca
hgch_area_Oca <- function(data,
                          title = NULL,
                          subtitle = NULL,
                          caption = NULL,
                          horLabel = NULL,
                          verLabel = NULL,
                          yLine = NULL,
                          yLineLabel = NULL,
                          dropNa = FALSE,
                          order = NULL,
                          percentage = FALSE,
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

  if (percentage) {
    d <- d %>%
      dplyr::mutate(b = b / sum(b))
    verLabel <- paste("%", verLabel)
  }

  order <- union(order, unique(d$a)[!is.na(unique(d$a))])
  if (all(!is.na(order)) & any(is.na(d$a))) order <- c(union(order, unique(d$a[!is.na(d$a)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$a, order)), ]

  hc <- hchart(d, type = "area", hcaes(x = a, y = b)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol = "circle"))) %>%
    hc_tooltip(headerFormat = paste("<b>", paste0(horLabel, ": "), "</b>{point.key}<br/>"),
               pointFormat = paste0("<b>", verLabel, "</b>: {point.b", ifelse(percentage, ":.3f}", "}"))) %>%
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


#' Vertical area (ordered categories, numbers)
#'
#' Compare quantities among categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca-Num
#' @examples
#' hgch_area_OcaNum(sampleData("Oca-Num", nrow = 10))
#' @export hgch_area_OcaNum
hgch_area_OcaNum <- function(data,
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
                             percentage = FALSE,
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

  # d <- d  %>%
  #   tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
  #                          b = NA)) %>%
  #   dplyr::group_by(a) %>%
  #   dplyr::summarise(b = agg(agg, b))
  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b)) %>%
    tidyr::spread(a, b) %>%
    tidyr::gather(a, b) %>%
    dplyr::mutate(format = b)
  d$b[is.na(d$b)] <- 0


  if (percentage) {
    d <- d %>%
      dplyr::mutate(b = b / sum(b))
    verLabel <- paste("%", verLabel)
  }

  order <- union(order, unique(d$a)[!is.na(unique(d$a))])
  if (all(!is.na(order)) & any(is.na(d$a))) order <- c(union(order, unique(d$a[!is.na(d$a)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$a, order)), ]

  hc <- hchart(d, type = "area", hcaes(x = a, y = b)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol = "circle"))) %>%
    hc_tooltip(headerFormat = paste("<b>", paste0(horLabel, ": "), "</b>{point.key}<br/>"),
               pointFormat = paste0("<b>", verLabel, "</b>: {point.b", ifelse(percentage, ":.3f}", "}"))) %>%
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


#' Vertical area (years, numbers)
#'
#' Compare quantities over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Yea-Num
#' @examples
#' hgch_area_YeaNum(sampleData("Yea-Num", nrow = 10))
#' @export hgch_area_YeaNum
hgch_area_YeaNum <- hgch_area_OcaNum


#' Vertical area (dates, numbers)
#'
#' Compare a quantities over time (Year-month-day)
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Dat-Num
#' @examples
#' hgch_area_DatNum(sampleData("Dat-Num", nrow = 10))
#' @export hgch_area_DatNum
hgch_area_DatNum <- hgch_area_OcaNum


#' Vertical stacked area (categories, ordered categories)
#'
#' Compare stacked categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca
#' @examples
#' hgch_area_stacked_CatOca(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_area_stacked_CatOca
hgch_area_stacked_CatOca <- function(data,
                                     title = NULL,
                                     subtitle = NULL,
                                     caption = NULL,
                                     horLabel = NULL,
                                     verLabel = NULL,
                                     yLine = NULL,
                                     yLineLabel = NULL,
                                     dropNa = FALSE,
                                     order = NULL,
                                     percentage = FALSE,
                                     theme = NULL,
                                     export = FALSE, ...) {



  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[1]
  verLabel <- verLabel %||% "count"
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
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::mutate(format = ifelse(is.na(c), "NA", c))
  d$c[is.na(d$c)] <- 0

  if (percentage) {
    d <- d %>%
      dplyr::mutate(c = c / sum(c),
                    format = ifelse(is.na(c), "NA", c))
    verLabel <- paste("%", verLabel)
  }

  order <- union(order, unique(d$b)[!is.na(unique(d$b))])
  if (all(!is.na(order)) & any(is.na(d$b))) order <- c(union(order, unique(d$b[!is.na(d$b)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$b, order)), ]

  hc <- hchart(d, type = "area", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(area = list(stacking = "normal"),
                   series = list(marker = list(enabled = TRUE, symbol = "circle"))) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b>", paste0(horLabel, ": "), "</b>{point.a}<br/><b>",
                                    paste0(nms[2], ": "), "</b>{point.b}<br/><b>",
                                    verLabel, "</b>: {point.format", ifelse(percentage, ":.3f}", "}"))) %>%
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


#' Vertical %100 stacked area (categories, ordered categories)
#'
#' Compare %100 stacked categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca
#' @examples
#' hgch_area_stacked_100_CatOca(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_area_stacked_100_CatOca
hgch_area_stacked_100_CatOca <- function(data,
                                         title = NULL,
                                         subtitle = NULL,
                                         caption = NULL,
                                         horLabel = NULL,
                                         verLabel = NULL,
                                         yLine = NULL,
                                         yLineLabel = NULL,
                                         dropNa = FALSE,
                                         order = NULL,
                                         percentage = FALSE,
                                         theme = NULL,
                                         export = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[1]
  verLabel <- verLabel %||% ifelse(percentage, "% count", "count")
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
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::mutate(format = ifelse(is.na(c), "NA", c))
  d$c[is.na(d$c)] <- 0

  if (percentage) {
    d <- d %>%
      dplyr::mutate(c = c / sum(c),
                    format = ifelse(is.na(c), "NA", c))
    verLabel <- paste("%", verLabel)
  }

  order <- union(order, unique(d$b)[!is.na(unique(d$b))])
  if (all(!is.na(order)) & any(is.na(d$b))) order <- c(union(order, unique(d$b[!is.na(d$b)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$b, order)), ]

  hc <- hchart(d, type = "area", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(area = list(stacking = "percent"),
                   series = list(marker = list(enabled = TRUE, symbol = "circle"))) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b>", paste0(horLabel, ": "), "</b>{point.a}<br/><b>",
                                    paste0(nms[2], ": "), "</b>{point.b}<br/><b>",
                                    verLabel, "</b>: {point.format", ifelse(percentage, ":.3f}", "}"))) %>%
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


#' Vertical area (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca-Num
#' @examples
#' hgch_area_CatOcaNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_area_CatOcaNum
hgch_area_CatOcaNum <- function(data,
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
                                percentage = FALSE,
                                theme = NULL,
                                export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[2]
  verLabel <- verLabel %||% ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[3], paste(agg, nms[3]))
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(agg, c)) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a) #%>%
   # dplyr::mutate(format = ifelse(is.na(c), "NA", c))
  d$c[is.na(d$c)] <- 0

  if (percentage) {
    d <- d %>%
      dplyr::mutate(c = c / sum(c))#,
                    #format = ifelse(is.na(c), "NA", c / sum(d$c, na.rm = TRUE)))
    verLabel <- paste("%", verLabel)
  }

  order <- union(order, unique(d$b)[!is.na(unique(d$b))])
  if (all(!is.na(order)) & any(is.na(d$b))) order <- c(union(order, unique(d$b[!is.na(d$b)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$b, order)), ]

  hc <- hchart(d, type = "area", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol = "circle"))) %>%
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


#' Vertical area (categories, years, numbers)
#'
#' Compare quantities among categories over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_area_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_area_CatYeaNum
hgch_area_CatYeaNum <- hgch_area_CatOcaNum


#' Vertical area (categories, dates, numbers)
#'
#' Compare quantities among categories over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_area_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_area_CatDatNum
hgch_area_CatDatNum <- hgch_area_CatOcaNum


#' Vertical stacked area (categories, ordered categories, numbers)
#'
#' Compare quantities among stacked categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca-Num
#' @examples
#' hgch_area_stacked_CatOcaNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_area_stacked_CatOcaNum
hgch_area_stacked_CatOcaNum <- function(data,
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
                                        percentage = FALSE,
                                        theme = NULL,
                                        export = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[2]
  verLabel <- verLabel %||% ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[3], paste(agg, nms[3]))
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(agg, c)) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a)
  d$c[is.na(d$c)] <- 0

  if (percentage) {
    d <- d %>%
      dplyr::mutate(c = c / sum(c))
    verLabel <- paste("%", verLabel)
  }

  order <- union(order, unique(d$b)[!is.na(unique(d$b))])
  if (all(!is.na(order)) & any(is.na(d$b))) order <- c(union(order, unique(d$b[!is.na(d$b)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$b, order)), ]

  hc <- hchart(d, type = "area", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(area = list(stacking = "normal"),
                   series = list(marker = list(enabled = TRUE, symbol = "circle"))) %>%
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


#' Vertical stacked area (categories, years, numbers)
#'
#' Compare quantities among stacked categories over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_area_stacked_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_area_stacked_CatYeaNum
hgch_area_stacked_CatYeaNum <- hgch_area_stacked_CatOcaNum


#' Vertical stacked area (categories, dates, numbers)
#'
#' Compare quantities among stacked categories over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_area_stacked_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_area_stacked_CatDatNum
hgch_area_stacked_CatDatNum <- hgch_area_stacked_CatOcaNum


#' Vertical 100% stacked area (categories, ordered categories, numbers)
#'
#' Compare quantities among 100% stacked categories over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca-Num
#' @examples
#' hgch_area_stacked_100_CatOcaNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_area_stacked_100_CatOcaNum
hgch_area_stacked_100_CatOcaNum <- function(data,
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
                                            theme = NULL,
                                            export = FALSE, ...) {



  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[2]
  verLabel <- verLabel %||% ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[3], paste(agg, nms[3]))
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
    dplyr::summarise(c = agg(agg, c)) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a)
  d$c[is.na(d$c)] <- 0

  order <- union(order, unique(d$b)[!is.na(unique(d$b))])
  if (all(!is.na(order)) & any(is.na(d$b))) order <- c(union(order, unique(d$b[!is.na(d$b)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$b, order)), ]

  hc <- hchart(d, type = "area", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(area = list(stacking = "percent")) %>%
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
  if (export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Vertical 100% stacked area (categories, years, numbers)
#'
#' Compare quantities among 100% stacked categories over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_area_stacked_100_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_area_stacked_100_CatYeaNum
hgch_area_stacked_100_CatYeaNum <- hgch_area_stacked_100_CatOcaNum


#' Vertical 100% stacked area (categories, dates, numbers)
#'
#' Compare quantities among 100% stacked categories over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_area_stacked_100_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_area_stacked_100_CatDatNum
hgch_area_stacked_100_CatDatNum <- hgch_area_stacked_100_CatOcaNum


#' Area (ordered category, n numbers)
#'
#' Compare n quantities among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca-NumP
#' @examples
#' hgch_area_OcaNumP(sampleData("Oca-NumP", nrow = 10))
#' @export hgch_area_OcaNumP
hgch_area_OcaNumP <- function(data,
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
                              percentage = FALSE,
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

  hc <- hgch_area_CatOcaNum(data = d,
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
                            percentage = percentage,
                            theme = theme,
                            export = export, ...)
  hc
}


#' Area (years, n numbers)
#'
#' Compare n quantities over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Yea-NumP
#' @examples
#' hgch_area_YeaNumP(sampleData("Yea-NumP", nrow = 10))
#' @export hgch_area_YeaNumP
hgch_area_YeaNumP <- hgch_area_OcaNumP


#' Area (dates, n numbers)
#'
#' Compare n quantities over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Dat-NumP
#' @examples
#' hgch_area_DatNumP(sampleData("Dat-NumP", nrow = 10))
#' @export hgch_area_DatNumP
hgch_area_DatNumP <- hgch_area_OcaNumP



# #' hgch_2yline_YeaNumNum
# #' 2 y lines
# #' @name hgch_multilines.
# #' @param x A data.frame
# #' @export
# #' @return highcharts viz
# #' @section ctypess: Yea-Num-Num
# #' @examples
# #' hgch_2yline_YeaNumNum(sampleDatta("Yea-Num-Num",nrow = 10))
# hgch_2yline_YeaNumNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL,
#                                yAxisTitle1 = NULL, yAxisTitle2 = NULL,
#                                symbol = NULL, theme = NULL, export = FALSE,...){
#
#   f <- fringe(data)
#   nms <- getClabels(f)
#
#   xAxisTitle <- xAxisTitle %||% nms[1]
#   yAxisTitle1 <- yAxisTitle1 %||% nms[2]
#   yAxisTitle2 <- yAxisTitle2 %||% nms[3]
#   title <-  title %||% ""
#
#   d <- f$d %>% dplyr::group_by(a) %>%
#     dplyr::summarise(b = mean(b), c = mean(c))
#
#   highchart() %>%
#     # hc_xAxis(categories = d$a) %>%
#     hc_yAxis_multiples(
#       list(title = list(text = yAxisTitle1),
#            showFirstLabel = FALSE,
#            showLastLabel = FALSE,
#            lineWidth = 0),
#       list(title = list(text = yAxisTitle2),
#            showFirstLabel = FALSE,
#            showLastLabel = FALSE,
#            opposite = TRUE)
#     ) %>%
#     hc_title(text = title) %>%
#    hc_subtitle(text = subtitle) %>%
#     hc_add_series(name = yAxisTitle1, type = "area",
#                   data = as.matrix(d[,c("a","b")])) %>%
#     hc_add_series(name = yAxisTitle2, type = "area", yAxis = 1,
#                   data = as.matrix(d[,c("a","c")]))
# }
#
#
#
# #' hgch_multilines_YeaNumP
# #' Multilines
# #' @name hgch_multilines.
# #' @param x A data.frame
# #' @export
# #' @return highcharts viz
# #' @section ctypess: Yea-NumP
# #' @examples
# #' hgch_multilines_YeaNumP(sampleDatta("Yea-Num-Num",nrow = 10))
# hgch_multilines_YeaNumP <- function(data,
#                                   title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
#                                   symbol = NULL,  startAtZero = FALSE,...){
#   f <- fringe(data)
#   nms <- getClabels(f)
#
#   xAxisTitle <- xAxisTitle %||% nms[1]
#   yAxisTitle <- yAxisTitle %||% ""
#   title <-  title %||% f$name
#   symbol <- symbol %||% "circle"
#   d <- f$d %>% tidyr::gather(variable,value, -a) %>%
#     dplyr::filter(!is.na(value)) %>% dplyr::group_by(a,variable) %>%
#     dplyr::summarise(value = mean(value)) %>% dplyr::ungroup()
#   codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
#   d <- d %>%
#     dplyr::mutate(variable = fct_recode_df(d,"variable",codes))
#   hc <- hchart(d, type = "area",hcaes( x = a, y = value, group = variable)) %>%
#     hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
#     hc_title(text = title) %>%
#    hc_subtitle(text = subtitle) %>%
#     hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
#     hc_yAxis(title = list(text=yAxisTitle))
#   if(startAtZero){
#     hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
#   }
#   hc
# }
