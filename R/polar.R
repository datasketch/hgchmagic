#' Radar (ordered categories)
#'
#' Comparing counts of categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca
#' @examples
#' hgch_radar_Oca(sampleData("Cat", nrow = 10))
#' @export hgch_radar_Oca
hgch_radar_Oca <- function(data,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           yLine = NULL,
                           dropNa = FALSE,
                           order = NULL,
                           sort = "no",
                           sliceN = NULL,
                           theme = NULL,
                           export = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

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

  hc <- hchart(d, type = "line", hcaes(x = a, y = b)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol = "circle"))) %>%
    hc_chart(polar = TRUE) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b>", paste0(nms[1], ": "), "</b>{point.a}<br/><b>",
                                    paste0("count ", nms[1], "</b>: {point.b}"))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = "", allowDecimals = FALSE) %>%
    hc_yAxis(title = "", plotLines = list(list(value = yLine,
                                               color = 'black',
                                               dashStyle = 'shortdash',
                                               width = 2))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}




#' Radar (ordered categories, numbers)
#'
#' Comparing quantities among categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca-Num
#' @examples
#' hgch_radar_OcaNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_radar_OcaNum
hgch_radar_OcaNum <- function(data,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              yLine = NULL,
                              dropNa = FALSE,
                              agg = "sum",
                              order = NULL,
                              percentage = FALSE,
                              sort = "no",
                              sliceN = NULL,
                              theme = NULL,
                              export = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

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

  hc <- hchart(d, type = "line", hcaes(x = a, y = b)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol = "circle"))) %>%
    hc_chart(polar = TRUE) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b>", paste0(nms[1], ": "), "</b>{point.a}<br/><b>",
                                    paste0(ifelse(percentage, "% ", "") ,
                                           ifelse(nrow(f$d) == dplyr::n_distinct(f$d$a), nms[2], paste(agg, nms[2])), ": "), "</b>: {point.b",
                                    ifelse(percentage, ":.3f}", "}"))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = "", allowDecimals = FALSE) %>%
    hc_yAxis(title = "", plotLines = list(list(value = yLine,
                                               color = 'black',
                                               dashStyle = 'shortdash',
                                               width = 2)),
             labels = list(format = ifelse(percentage, "{value}%", "{value}"))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Radar (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca-Num
#' @examples
#' hgch_radar_CatOcaNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_radar_CatOcaNum
hgch_radar_CatOcaNum <- function(data,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 yLine = NULL,
                                 agg = "sum",
                                 dropNa = FALSE,
                                 order = NULL,
                                 percentage = FALSE,
                                 theme = NULL,
                                 export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

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
  }

  order <- union(order, unique(d$b)[!is.na(unique(d$b))])
  if (all(!is.na(order)) & any(is.na(d$b))) order <- c(union(order, unique(d$b[!is.na(d$b)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$b, order)), ]

  hc <- hchart(d, type = "line", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol = "circle"))) %>%
    hc_chart(polar = TRUE) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b>", paste0(nms[1], ": "), "</b>{point.a}<br/><b>",
                                    paste0(nms[2], ": "), "</b>{point.b}<br/><b>",
                                    paste0(ifelse(percentage, "% ", "") ,
                                           ifelse(nrow(f$d) == dplyr::n_distinct(f$d$a), nms[3], paste(agg, nms[3]))),
                                    "</b>: {point.c", ifelse(percentage, ":.3f}", "}"))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = "", allowDecimals = FALSE) %>%
    hc_yAxis(title = "", plotLines = list(list(value = yLine,
                                               color = 'black',
                                               dashStyle = 'shortdash',
                                               width = 2)),
             labels = list(format = ifelse(percentage, "{value}%", "{value}"))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Radar (ordered categories, n numbers)
#'
#' Comparing n quantities among categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca-NumP
#' @examples
#' hgch_radar_OcaNumP(sampleData("Cat-NumP", nrow = 10))
#' @export hgch_radar_OcaNumP
hgch_radar_OcaNumP <- function(data,
                               title = NULL,
                               subtitle = NULL,
                               caption = NULL,
                               yLine = NULL,
                               dropNa = FALSE,
                               agg = "sum",
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

  hc <- hgch_radar_CatOcaNum(data = d,
                             title = title ,
                             subtitle = subtitle,
                             caption = caption,
                             yLine = yLine,
                             agg = agg,
                             dropNa = dropNa,
                             order = order,
                             percentage = percentage,
                             theme = theme,
                             export = export, ...)
  hc
}



#' Polar bar (ordered categories)
#'
#' Comparing counts of categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca
#' @examples
#' hgch_polarbar_Oca(sampleData("Cat", nrow = 10))
#' @export hgch_polarbar_Oca
hgch_polarbar_Oca <- function(data,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              yLine = NULL,
                              dropNa = FALSE,
                              order = NULL,
                              sort = "no",
                              sliceN = NULL,
                              theme = NULL,
                              export = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

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

  hc <- hchart(d, type = "column", hcaes(x = a, y = b)) %>%
    hc_chart(polar = TRUE) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b>", paste0(nms[1], ": "), "</b>{point.a}<br/><b>",
                                    paste0("count ", nms[1], "</b>: {point.b}"))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = "", allowDecimals = FALSE) %>%
    hc_yAxis(title = "", plotLines = list(list(value = yLine,
                                               color = 'black',
                                               dashStyle = 'shortdash',
                                               width = 2))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}




#' Polar bar (ordered categories, numbers)
#'
#' Comparing quantities among categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca-Num
#' @examples
#' hgch_polarbar_OcaNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_polarbar_OcaNum
hgch_polarbar_OcaNum <- function(data,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 yLine = NULL,
                                 dropNa = FALSE,
                                 agg = "sum",
                                 order = NULL,
                                 percentage = FALSE,
                                 sort = "no",
                                 sliceN = NULL,
                                 theme = NULL,
                                 export = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

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

  hc <- hchart(d, type = "column", hcaes(x = a, y = b)) %>%
    hc_chart(polar = TRUE) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b>", paste0(nms[1], ": "), "</b>{point.a}<br/><b>",
                                    paste0(ifelse(percentage, "% ", "") ,
                                           ifelse(nrow(f$d) == dplyr::n_distinct(f$d$a), nms[2], paste(agg, nms[2])), ": "), "</b>: {point.b",
                                    ifelse(percentage, ":.3f}", "}"))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = "", allowDecimals = FALSE) %>%
    hc_yAxis(title = "", plotLines = list(list(value = yLine,
                                               color = 'black',
                                               dashStyle = 'shortdash',
                                               width = 2)),
             labels = list(format = ifelse(percentage, "{value}%", "{value}"))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Polar bar (categories, ordered categories, numbers)
#'
#' Compare grouped quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca-Num
#' @examples
#' hgch_polarbar_grouped_CatOcaNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_polarbar_grouped_CatOcaNum
hgch_polarbar_grouped_CatOcaNum <- function(data,
                                            title = NULL,
                                            subtitle = NULL,
                                            caption = NULL,
                                            yLine = NULL,
                                            agg = "sum",
                                            dropNa = FALSE,
                                            order = NULL,
                                            percentage = FALSE,
                                            theme = NULL,
                                            export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

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
  }

  order <- union(order, unique(d$b)[!is.na(unique(d$b))])
  if (all(!is.na(order)) & any(is.na(d$b))) order <- c(union(order, unique(d$b[!is.na(d$b)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$b, order)), ]

  hc <- hchart(d, type = "column", hcaes(x = b, y = c, group = a)) %>%
    hc_chart(polar = TRUE) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b>", paste0(nms[1], ": "), "</b>{point.a}<br/><b>",
                                    paste0(nms[2], ": "), "</b>{point.b}<br/><b>",
                                    paste0(ifelse(percentage, "% ", "") ,
                                           ifelse(nrow(f$d) == dplyr::n_distinct(f$d$a), nms[3], paste(agg, nms[3]))),
                                    "</b>: {point.c", ifelse(percentage, ":.3f}", "}"))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = "", allowDecimals = FALSE) %>%
    hc_yAxis(title = "", plotLines = list(list(value = yLine,
                                               color = 'black',
                                               dashStyle = 'shortdash',
                                               width = 2)),
             labels = list(format = ifelse(percentage, "{value}%", "{value}"))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Polar bar (ordered categories, n numbers)
#'
#' Comparing n grouped quantities among categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca-NumP
#' @examples
#' hgch_polarbar_grouped_OcaNumP(sampleData("Cat-NumP", nrow = 10))
#' @export hgch_polarbar_grouped_OcaNumP
hgch_polarbar_grouped_OcaNumP <- function(data,
                                          title = NULL,
                                          subtitle = NULL,
                                          caption = NULL,
                                          yLine = NULL,
                                          dropNa = FALSE,
                                          agg = "sum",
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

  hc <- hgch_polarbar_grouped_CatOcaNum(data = d,
                                        title = title ,
                                        subtitle = subtitle,
                                        caption = caption,
                                        yLine = yLine,
                                        agg = agg,
                                        dropNa = dropNa,
                                        order = order,
                                        percentage = percentage,
                                        theme = theme,
                                        export = export, ...)
  hc
}
