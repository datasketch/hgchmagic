#' Line (ordered categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca
#' @examples
#' hgch_line_Oca(sampleData("Cat", nrow = 10))
#' @export hgch_line_Oca
hgch_line_Oca <- function(data,
                          title = NULL,
                          subtitle = NULL,
                          caption = NULL,
                          horLabel = NULL,
                          verLabel = NULL,
                          yLine = NULL,
                          yLineLabel = NULL,
                          dropNa = FALSE,
                          order = NULL,
                          startAtZero = TRUE,
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

  hc <- hchart(d, type = "line", hcaes(x = a, y = b)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol = "circle"))) %>%
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
             minRange = if (startAtZero) 0.1,
             min = if (startAtZero) 0,
             minPadding = if (startAtZero) 0) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Line (ordered categories, numbers)
#'
#' Compare quantities among categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca-Num
#' @examples
#' hgch_line_OcaNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_line_OcaNum
hgch_line_OcaNum <- function(data,
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
                             startAtZero = TRUE,
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

  hc <- hchart(d, type = "line", hcaes(x = a, y = b)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol = "circle"))) %>%
    hc_tooltip(headerFormat = paste("<b>", paste0(horLabel, ": "), "</b>{point.key}<br/>"),
               pointFormat = paste0("<b>", verLabel, "</b>: {point.b", ifelse(percentage, ":.3f}", "}"))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = yLine,
                                                                  icolor = 'black',
                                                                  dashStyle = 'shortdash',
                                                                  width = 2,
                                                                  label = list(text = yLineLabel))),
             labels = list(format = ifelse(percentage, "{value}%", "{value}")),
             minRange = if (startAtZero) 0.1,
             min = if (startAtZero) 0,
             minPadding = if (startAtZero) 0) %>%
  hc_add_theme(custom_theme(custom = theme)) %>%
  hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Line (years, numbers)
#'
#' Compare quantities over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Yea-Num
#' @examples
#' hgch_line_YeaNum(sampleData("Yea-Num", nrow = 10))
#' @export hgch_line_YeaNum
hgch_line_YeaNum <- hgch_line_OcaNum


#' Line (dates, numbers)
#'
#' Compare a quantities over time (Year-month-day)
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Dat-Num
#' @examples
#' hgch_line_DatNum(sampleData("Dat-Num", nrow = 10))
#' @export hgch_line_DatNum
hgch_line_DatNum <- hgch_line_OcaNum



#' Line (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Oca-Num
#' @examples
#' hgch_line_CatOcaNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_line_CatOcaNum
hgch_line_CatOcaNum <- function(data,
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
                                startAtZero = TRUE,
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

  hc <- hchart(d, type = "line", hcaes(x = b, y = c, group = a)) %>%
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
             labels = list(format = ifelse(percentage, "{value}%", "{value}")),
             minRange = if (startAtZero) 0.1,
             min = if (startAtZero) 0,
             minPadding = if (startAtZero) 0) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Line (categories, years, numbers)
#'
#' Compare quantities among categories over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_line_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_line_CatYeaNum
hgch_line_CatYeaNum <- hgch_line_CatOcaNum


#' Line (categories, dates, numbers)
#'
#' Compare quantities among categories over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_line_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_line_CatDatNum
hgch_line_CatDatNum <- hgch_line_CatOcaNum



#' Line (ordered category, n numbers)
#'
#' Compare n quantities among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca-NumP
#' @examples
#' hgch_line_OcaNumP(sampleData("Oca-NumP", nrow = 10))
#' @export hgch_line_OcaNumP
hgch_line_OcaNumP <- function(data,
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
                              startAtZero = TRUE,
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

  hc <- hgch_line_CatOcaNum(data = d,
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
                            startAtZero = startAtZero,
                            theme = theme,
                            export = export, ...)
  hc
}


#' Line (years, n numbers)
#'
#' Compare n quantities over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Yea-NumP
#' @examples
#' hgch_line_YeaNumP(sampleData("Yea-NumP", nrow = 10))
#' @export hgch_line_YeaNumP
hgch_line_YeaNumP <- hgch_line_OcaNumP


#' Line (dates, n numbers)
#'
#' Compare n quantities over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Dat-NumP
#' @examples
#' hgch_line_DatNumP(sampleData("Dat-NumP", nrow = 10))
#' @export hgch_line_DatNumP
hgch_line_DatNumP <- hgch_line_OcaNumP


#' 2 y line
#'
#' 2 y line
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Yea-Num-Num
#' @examples
#'
#' hgch_2yline_YeaNumNum(sampleData("Yea-Num-Num", nrow = 10))
#'
#' @export hgch_2yline_YeaNumNum
hgch_2yline_YeaNumNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL,
                                  yAxisTitle1 = NULL, yAxisTitle2 = NULL, aggregation = "sum",
                                  symbol = NULL, theme = NULL, export = FALSE,...){
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle1 <- yAxisTitle1 %||% nms[2]
  yAxisTitle2 <- yAxisTitle2 %||% nms[3]
  title <-  title %||% ""
  caption <- caption %||% ""

  d <- f$d %>%
    tidyr::drop_na(a) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(aggregation, b), c = agg(aggregation, c)) %>%
    arrange(a)


    hc <-  highchart() %>%
           hc_xAxis(categories = d$a) %>%
           hc_yAxis_multiples(
           list(title = list(text = yAxisTitle1),
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           lineWidth = 0),
      list(title = list(text = yAxisTitle2),
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           opposite = TRUE)
    ) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_add_series(name = yAxisTitle1, data = d$b) %>%
    hc_add_series(name = yAxisTitle2, data = d$c, yAxis = 1)

  hc <- hc %>% hc_add_theme(custom_theme(custom=theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  hc
}









# # Slope
# #
# # Slope
# #
# #
# # @param x A data.frame
# # @return highcharts viz
# # @section ctypes:
# # Cat-Yea-Num
# # @examples
# #
# # hgch_slope_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
# #
# # @export hgch_slope_CatYeaNum
# hgch_slope_CatYeaNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
#                                  symbol = NULL, aggregation = "sum",
#                                  startAtZero = FALSE, theme = NULL, export = FALSE,...){
#
#
#   if(nrow(data)==0) return()
#
#   f <- fringe(data)
#   nms <- getClabels(f)
#   #data <- f$d
#
#   xAxisTitle <- xAxisTitle %||% nms[2]
#   yAxisTitle <- yAxisTitle %||% nms[3]
#   title <-  title %||% ""
#   symbol <- symbol %||% "circle"
#
#
#   data  <- f$d %>%
#     tidyr::drop_na(a) %>%
#     dplyr::group_by(a,b) %>%
#     dplyr::summarise(c = agg(aggregation, c)) %>%
#     dplyr::arrange(b)
#
#   list_pre <- data %>%
#     group_by(name = a) %>%
#     do(data = .$c)
#
#   list_series <- list_parse(list_pre)
#
#
#   hc <- highchart() %>%
#     hc_xAxis(categories = unique(data$b)) %>%
#     hc_add_series_list(list_series)
#
#   hc <- hc %>%
#     hc_title(text = title) %>%
#     hc_subtitle(text = subtitle) %>%
#     hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
#     hc_yAxis(title = list(text=yAxisTitle), allowDecimals = FALSE)
#
#   if(!is.null(symbol)){
#     hc <- hc %>% hc_plotOptions(
#       series = list(marker = list(enabled = TRUE, symbol =  symbol))
#     )
#   }
#   if(startAtZero){
#     hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
#   }
#   hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
#   if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
#   hc
# }


#' Grouped line
#'
#' Grouped line
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypess: Yea-NumP
#' @examples
#'
#' hgch_multilines_YeaNumP(sampleData("Yea-NumP",nrow = 10))
#'
#' @export hgch_multilines_YeaNumP
hgch_multilines_YeaNumP <- function(data,
                                    title = NULL, subtitle = NULL, caption = NULL,
                                    xAxisTitle = NULL, yAxisTitle = NULL,
                                    symbol = NULL,  startAtZero = FALSE, theme = NULL, export = FALSE,...){
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  d <- f$d %>% tidyr::gather(variable,value, -a) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(a,variable) %>%
    dplyr::summarise(value = mean(value)) %>% dplyr::ungroup()# %>%
  #tidyr::drop_na() %>% arrange(a)

  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d,"variable",codes)) %>%
    tidyr::drop_na() %>%
    mutate(a = as.numeric(a))

  hc <- hchart(d, type = "line",hcaes( x = a, y = value, group = variable)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}
