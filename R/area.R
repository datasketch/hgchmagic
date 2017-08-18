#' Vertical area
#'
#' Vertical area
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Dat-Num
#' @examples
#'
#' hgch_area_DatNum(sampleData("Dat-Num", nrow = 10))
#'
#' @export hgch_area_DatNum
hgch_area_DatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                             symbol = NULL, aggregation = "sum",
                             startAtZero = FALSE, theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  caption <- caption %||% ""

  d <- f$d %>% na.omit() %>% dplyr::group_by(a) %>% dplyr::summarise(b = agg(aggregation, b))
  if(nrow(d)==0) return()
  hc <- hchart(d, type = "area", hcaes(x = a, y = b)) %>%
  hc_plotOptions(
     series = list(marker = list(enabled = FALSE, symbol =  symbol))) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "</b><b style = 'font-size:12px'>{point.key}</b><br/>"),
               pointFormat = paste0("<b style = 'font-size:12px'>", yAxisTitle,"</b>: {point.b}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(!is.null(symbol)){
   hc <- hc %>% hc_plotOptions(
     series = list(marker = list(enabled = TRUE, symbol =  symbol))
   )
  }else{
   hc <- hc %>% hc_plotOptions(
     series = list(marker = list(enabled = FALSE))
   )
  }
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}



#' Vertical area
#'
#' Vertical area
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#'
#' hgch_area_CatNum(sampleData("Cat-Num", nrow = 10))
#'
#' @export hgch_area_CatNum
hgch_area_CatNum <-hgch_area_DatNum




#' Vertical area
#'
#' Vertical area
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Yea-Num
#' @examples
#'
#' hgch_area_YeaNum(sampleData("Yea-Num", nrow = 10))
#'
#' @export hgch_area_YeaNum
hgch_area_YeaNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                           symbol = NULL, aggregation = "sum",
                           startAtZero = FALSE, theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  caption <- caption %||% ""

  d <- f$d %>% na.omit() %>% dplyr::group_by(a) %>% dplyr::summarise(b = agg(aggregation, b))
  if(nrow(d)==0) return()
  hc <- hchart(d, type = "area", hcaes(x = sort(as.character(a), decreasing = TRUE), y = b)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    ) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "</b><b style = 'font-size:12px'>{point.key}</b><br/>"),
               pointFormat = paste0("<b style = 'font-size:12px'>", yAxisTitle,"</b>: {point.b}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(!is.null(symbol)){
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    )
  }else{
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = FALSE))
    )
  }
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Vertical area
#'
#' Vertical area
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#'
#' hgch_area_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#'
#' @export hgch_area_CatCatNum
hgch_area_CatCatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                             symbol = NULL, aggregation = "sum",
                             startAtZero = FALSE, theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  caption <- caption %||% ""

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = agg(aggregation, c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hc <- hchart(d, type = "area", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    ) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "</b>{point.a}<br/><b style = 'font-size:12px'>",
                                    paste0(xAxisTitle, ": "),"</b>{point.b}<br/><b style = 'font-size:12px'>",
                                    yAxisTitle,"</b>: {point.c}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(!is.null(symbol)){
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    )
  }else{
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = FALSE))
    )
  }
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Vertical area
#'
#' Vertical area
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#'
#' hgch_area_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#'
#' @export hgch_area_CatYeaNum
hgch_area_CatYeaNum <- hgch_area_CatCatNum



#' Vertical stacked area
#'
#' Vertical stacked area
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#'
#' hgch_area_stacked_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#'
#' @export hgch_area_stacked_CatCatNum
hgch_area_stacked_CatCatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                   symbol = NULL, aggregation = "sum",
                                   startAtZero = FALSE, theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  caption <- caption %||% ""

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = agg(aggregation, c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hc <- hchart(d, type = "area", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol)),
      area = list(stacking = "normal")
    ) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "</b>{point.a}<br/><b style = 'font-size:12px'>",
                                    paste0(xAxisTitle, ": "),"</b>{point.b}<br/><b style = 'font-size:12px'>",
                                    yAxisTitle,"</b>: {point.c}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(!is.null(symbol)){
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    )
  }else{
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = FALSE))
    )
  }
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Vertical stacked area
#'
#' Vertical stacked area
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#'
#' hgch_area_stacked_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#'
#' @export hgch_area_stacked_CatYeaNum
hgch_area_stacked_CatYeaNum <- hgch_area_stacked_CatCatNum




#' Vertical 100 stacked area
#'
#' Vertical 100 stacked area
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#'
#' hgch_area_stacked_100_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#'
#' @export hgch_area_stacked_100_CatCatNum
hgch_area_stacked_100_CatCatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                       symbol = NULL, aggregation = "sum",
                                       startAtZero = FALSE, theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  caption <- caption %||% ""

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = agg(aggregation, c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hc <- hchart(d, type = "area", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol)),
      area = list(stacking = "percent")
    ) %>%
    hc_tooltip(headerFormat = "",
               pointFormat = paste0("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "</b>{point.a}<br/><b style = 'font-size:12px'>",
                                    paste0(xAxisTitle, ": "),"</b>{point.b}<br/><b style = 'font-size:12px'>",
                                    yAxisTitle,"</b>: {point.c}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(!is.null(symbol)){
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    )
  }else{
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = FALSE))
    )
  }
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Vertical 100 stacked area
#'
#' Vertical 100 stacked area
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#'
#' hgch_area_stacked_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#'
#' @export hgch_area_stacked_100_CatYeaNum
hgch_area_stacked_100_CatYeaNum <- hgch_area_stacked_100_CatCatNum



#' Vertical area
#'
#' Vertical area
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#'
#' hgch_area_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#'
#' @export hgch_area_CatDatNum
hgch_area_CatDatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                             symbol = NULL, aggregation = "sum",
                             startAtZero = FALSE, theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  caption <- caption %||% ""

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = agg(aggregation, c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hc <- hchart(d, type = "area", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = FALSE, symbol =  symbol))
    ) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(nms[2], ": "), "</b><b style = 'font-size:12px'>{point.key}</b><br/>"),
              pointFormat = paste0("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "</b>{point.a}<br/>
                                    <b style = 'font-size:12px'>", yAxisTitle,"</b>: {point.c}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(!is.null(symbol)){
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    )
  }else{
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = FALSE))
    )
  }
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Vertical stacked area
#'
#' Vertical stacked area
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#'
#' hgch_area_stacked_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#'
#' @export hgch_area_stacked_CatDatNum
hgch_area_stacked_CatDatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                   symbol = NULL, aggregation = "sum",
                                   startAtZero = FALSE, theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  caption <- caption %||% ""

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = agg(aggregation, c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hc <- hchart(d, type = "area", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = FALSE, symbol =  symbol)),
      area = list(stacking = "normal")
    ) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(nms[2], ": "), "</b><b style = 'font-size:12px'>{point.key}</b><br/>"),
               pointFormat = paste0("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "</b>{point.a}<br/>
                                    <b style = 'font-size:12px'>", yAxisTitle,"</b>: {point.c}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(!is.null(symbol)){
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    )
  }else{
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = FALSE))
    )
  }
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
  }




#' Vertical 100 stacked area
#'
#' Vertical 100 stacked area
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#'
#' hgch_area_stacked_100_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#'
#' @export hgch_area_stacked_100_CatDatNum
hgch_area_stacked_100_CatDatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                       symbol = NULL, aggregation = "sum",
                                       startAtZero = FALSE, theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  caption <- caption %||% ""

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = agg(aggregation, c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hc <- hchart(d, type = "area", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = FALSE, symbol =  symbol)),
      area = list(stacking = "percent")
    ) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(nms[2], ": "), "</b><b style = 'font-size:12px'>{point.key}</b><br/>"),
               pointFormat = paste0("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "</b>{point.a}<br/>
                                    <b style = 'font-size:12px'>", yAxisTitle,"</b>: {point.c}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(!is.null(symbol)){
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    )
  }else{
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = FALSE))
    )
  }
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}



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
