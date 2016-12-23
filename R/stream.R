# http://jsfiddle.net/highcharts/V757d/

#
# #' hgch_stream_CaCaNu
# #' @name hgch_stream_CaCaNu
# #' @param x A data.frame
# #' @export
# #' @return highcharts viz
# #' @section ftype: Ca-Ca-Nu
# #' @examples
# #' hgch_stream_CaCaNu(sampleData("Ca-Ca-Nu",nrow = 10))
# hgch_stream_CaCaNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
#                              symbol = NULL, startAtZero = FALSE, export = FALSE,...){
#
#   f <- fringe(data)
#   nms <- getClabels(f)
#
#   xAxisTitle <- xAxisTitle %||% nms[2]
#   yAxisTitle <- yAxisTitle %||% nms[3]
#   title <-  title %||% ""
#   symbol <- symbol %||% "circle"
#
#   d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = mean(c))
#   if(nrow(d)==0) return()
#   #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
#   hc <- hchart(d, type = "areasplinerange", hcaes(x = b, y = c, group = a)) %>%
#     hc_plotOptions(
#       series = list(marker = list(enabled = TRUE, symbol =  symbol))
#     ) %>%
#     hc_title(text = title) %>%
#     hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE)
#   if(startAtZero){
#     hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
#   }
#   hc
# }
#
# #' hgch_stream_CaYeNu
# #' @name hgch_stream_CaYeNu
# #' @param x A data.frame
# #' @export
# #' @return highcharts viz
# #' @section ftype: Ca-Ye-Nu
# #' @examples
# #' hgch_stream_CaYeNu(sampleData("Ye-Nu-Nu",nrow = 10))
# hgch_stream_CaYeNu <- hgch_stream_CaCaNu
#
#
#
#
#
#
#
# #' hgch_stream_stack_CaCaNu
# #' @name hgch_stream_stack_CaCaNu
# #' @param x A data.frame
# #' @export
# #' @return highcharts viz
# #' @section ftype: Ca-Ca-Nu
# #' @examples
# #' hgch_stream_stack_CaCaNu(sampleData("Ca-Ca-Nu",nrow = 10))
# hgch_stream_stack_CaCaNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
#                                    symbol = NULL, startAtZero = FALSE, export = FALSE,...){
#
#   f <- fringe(data)
#   nms <- getClabels(f)
#
#   xAxisTitle <- xAxisTitle %||% nms[2]
#   yAxisTitle <- yAxisTitle %||% nms[3]
#   title <-  title %||% ""
#   symbol <- symbol %||% "circle"
#
#   d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = mean(c))
#   if(nrow(d)==0) return()
#   #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
#   hc <- hchart(d, type = "area", hcaes(x = b, y = c, group = a)) %>%
#     hc_plotOptions(
#       series = list(marker = list(enabled = TRUE, symbol =  symbol)),
#       area = list(stacking = "normal")
#     ) %>%
#     hc_title(text = title) %>%
#     hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE)
#   if(startAtZero){
#     hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
#   }
#   hc
# }
#
#
#
