#' Line (ordered categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca
#' @examples
#' hgch_line_Oca(sampleData("Oca", nrow = 10))
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
                          percentage = FALSE,
                          startAtZero = FALSE,
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

  hc <- hchart(d, type = "line", hcaes(x = a, y = b)) %>%
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


#' Line (ordered categories, numbers)
#'
#' Compare quantities among categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Oca-Num
#' @examples
#' hgch_line_OcaNum(sampleData("Oca-Num", nrow = 10))
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
                             startAtZero = FALSE,
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
#' hgch_line_CatOcaNum(sampleData("Cat-Oca-Num", nrow = 10))
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
                                startAtZero = FALSE,
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

hgch_line_CatYeaNum <- function(data,
                                title = NULL,
                                subtitle = NULL,
                                caption = NULL,
                                horLabel = NULL,
                                verLabel = NULL,
                                yLine = NULL,
                                yLineLabel = NULL,
                                agg = "sum",
                                dropNa = FALSE,
                                nDigits = NULL,
                                marks = c('', '.'),
                                format = c('', ''),
                                order = NULL,
                                percentage = FALSE,
                                startAtZero = FALSE,
                                tooltip = list(headerFormat = NULL, pointFormat = NULL),
                                export = FALSE,
                                thema = tma(),
                                plotBandsFromX = NULL,
                                plotBandsToX = NULL,
                                plotBandsColorX = 'rgba(68, 170, 213, .2)',
                                plotBandsFromY = NULL,
                                plotBandsToY = NULL,
                                plotBandsColorY = 'rgba(68, 170, 213, .2)',
                                plotLineValueY = NULL,
                                plotLineValueX = NULL,
                                plotLineDashStyleY = 'shortdash',
                                plotLineDashStyleX = 'shortdash', ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  horLabel <- horLabel %||% nms[2]
  verLabel <- verLabel %||% ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[3], paste(agg, nms[3]))
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""


  #uniqYear <- sort(unique(d$b))

  # k <- d %>% group_by(a, b) %>% dplyr::summarise(cont = n())
  # k <- k %>% filter(cont == 1)
  # k <- k %>% left_join(d)
  # k <- k[is.na(k$c),]
  # k$c[is.na(k$c)] <- 'NA'
  # k <- k %>% dplyr::select(a, b, valor = c)


  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  # d <- d %>%
  #   tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
  #                          b = ifelse(is.character(d$b), "NA", NA),
  #                          c = NA)) %>%
  #   dplyr::group_by(a, b) %>%
  #   dplyr::summarise(c = agg(agg, c))

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
  d$c[is.na(d$c)] <- NA


  if (is.null(nDigits)) {
    nDig <- 0
  } else {
    nDig <- nDigits
  }

  if (percentage) {
    d <- d %>% group_by(a) %>%
      dplyr::mutate(c = (c / sum(c)) * 100)
  }

  d$c <- round(d$c, nDig)


  # if (nrow(k) > 0) {
  #   d <- d %>% left_join(k)
  #   d$valor <- as.numeric(coalesce(d$valor, as.character(d$c)))
  #   d <- d %>% select(a, b, c = valor)
  # }



  series <- map(unique(d[[1]]), function(i) {
    d0 <- d %>%
      filter(a %in% i)
    l0 <- list("name" = i,
               "data" = d0$c)
    l0
  })


  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  aggFormAxis <- paste0("function() { return '", format[1] , "' + Highcharts.numberFormat(this.value, ", nDig, ", '", marks[2], "', '", marks[1], "') + '", format[2], "'}"
  )

  if (is.null(tooltip$pointFormat)) {
    tooltip$pointFormat <-paste0('<b>', nms[2], ': </b>{point.category}</br>',
                                 '<b>', nms[1], ': </b>{series.name}</br>',
                                 paste0(agg, ' ' ,nms[3], ': '), format[1],'{point.y}', format[2])
  }
  if (is.null(tooltip$headerFormat)) {
    tooltip$headerFormat <- " "
  }


  global_options(marks[1], marks[2])



  hc <- highchart() %>%
    hc_chart(type = 'line') %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(
      title = list(text = verLabel),
      categories = map(as.character(sort(unique(d$b))), function(z) z),
      type = 'category',
      plotBands = list(
        from = plotBandsFromX,
        to = plotBandsToX,
        color = plotBandsColorX
      ),
      plotLines = list(list(
        color = 'black',
        dashStyle = plotLineDashStyleX,
        width = 2,
        value = plotLineValueX
      ))
    ) %>%
    hc_yAxis(
      title = list(text = horLabel),
      labels = list(
        format = '{value}',#formatLabAxis,
        formatter = JS(aggFormAxis)
      ),
      minRange = if (startAtZero) 0.1,
      min = if (startAtZero) 0,
      minPadding = if (startAtZero) 0,
      plotBands = list(
        from = plotBandsFromY,
        to = plotBandsToY,
        color = plotBandsColorY#'rgba(68, 170, 213, .2)'
      ),
      plotLines = list(list(
        color = 'black',
        dashStyle = plotLineDashStyleY,
        width = 2,
        value = plotLineValueY
      ))
    ) %>%
    hc_add_series_list(series) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>%
    hc_add_theme(custom_theme(custom = thema)) %>%
    hc_credits(enabled = TRUE, text = caption) #%>%
  #hc_legend(enabled = FALSE)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)

  hc

}

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
                              startAtZero = FALSE,
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
