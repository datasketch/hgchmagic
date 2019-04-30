#' #' Scatter (numbers, numbers)
#' #'
#' #' Two numeric variables set in a cartesian plane
#' #'
#' #' @param data A data.frame
#' #' @return Highcharts visualization
#' #' @section ctypes:
#' #' Num-Num
#' #' @examples
#' #' hgch_scatter_NumNum(sampleData("Num-Num", nrow = 10))
#' #' @export hgch_scatter_NumNum
#'
#' hgch_scatter_NumNum <- function() {
#'   library(hgchmagic)
#'   library(tidyverse)
#'
#'   title = NULL
#'   subtitle = NULL
#'   caption = NULL
#'   horLabel = NULL
#'   verLabel = NULL
#'   horLine = NULL
#'   horLineLabel = " "
#'   verLine = NULL
#'   verLineLabel = " "
#'   startAtZero = TRUE
#'   labelWrap = 12
#'   colors = NULL
#'   marks = c(".", ",")
#'   nDigits = NULL
#'   dropNa = FALSE
#'   prefix_X = NULL
#'   suffix_X = NULL
#'   prefix_Y = NULL
#'   suffix_Y = NULL
#'   order = NULL
#'   sort = "no"
#'   sliceN = NULL
#'   showText = TRUE
#'   tooltip = list(headerFormat = NULL, pointFormat = NULL)
#'   export = FALSE
#'   lang = 'es'
#'   theme = NULL
#'
#'
#'   data <- data.frame(x = runif(10), y = runif(10))
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   title <-  title %||% ""
#'   subtitle <- subtitle %||% ""
#'   caption <- caption %||% ""
#'
#'
#'   labelsXY <- orientationXY("ver",
#'                             x = nms[1],
#'                             y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2],  nms[2]),
#'                             hor = horLabel,
#'                             ver = verLabel)
#'   lineXY <- linesOrientation(orientation, horLine, verLine)
#'
#'   lineLabelsXY <- linesOrLabel("ver",
#'                                horLineLabel,
#'                                verLineLabel)
#'
#'   colorDefault <- c("#FECA84", "#3DB26F", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
#'
#'   if (!is.null(colors)) {
#'     colors <- unname(fillColors(d, "a", colors, colorScale = 'no'))
#'   } else {
#'     colors <- colorDefault
#'   }
#'
#'   if (dropNa)
#'     d <- d %>%
#'     tidyr::drop_na()
#'
#'
#'   formatLabAxis <- paste0('{value:', marks[1], marks[2], 'f}')
#'   if (!is.null(nDigits)) {
#'     formatLabAxis <- paste0('{value:', marks[1], marks[2], nDigits, 'f}')
#'   }
#'
#'
#'   if (is.null(prefix_X)) prefix_X <- ""
#'   if (is.null(suffix_X)) suffix_X <- ""
#'
#'   if (is.null(prefix_Y)) prefix_Y <- ""
#'   if (is.null(suffix_Y)) suffix_Y <- ""
#'
#'
#'
#'
#'   aggFormAxisX <- paste0("function() { return '", prefix_X , "' + Highcharts.numberFormat(this.value, ", nDig, ", '", marks[2], "', '", marks[1], "') + '", suffix_X, "'}"
#'   )
#'
#'
#'   aggFormAxisY <- paste0("function() { return '", prefix_Y , "' + Highcharts.numberFormat(this.value, ", nDig, ", '", marks[2], "', '", marks[1], "') + '", suffix_Y, "'}"
#'   )
#'
#'
#'
#'   dt1 <- map(1:nrow(d), function(z) {
#'     list(d$a[z], d$b[z])
#'   })
#'
#'
#'   highchart() %>%
#'     hc_chart(
#'       type = 'scatter',
#'       zoomType = 'xy'
#'     ) %>%
#'     hc_yAxis(
#'       title = list (
#'         text = labelsXY[2]),
#'       plotLines = list(
#'         list(value = lineXY[1],
#'              color = 'black',
#'              dashStyle = 'shortdash',
#'              width = 2,
#'              zIndex = 5,
#'              label = list(
#'                text = lineLabelsXY[2],
#'                style = list(
#'                  color = 'black'
#'                )
#'              ))),
#'       labels = list (
#'         format = formatLabAxis,
#'         formatter = JS(aggFormAxisY)
#'       )
#'     ) %>%
#'     hc_xAxis(
#'       title =  list(text = labelsXY[1]),
#'       startOnTick = TRUE,
#'       endOnTick = TRUE,
#'       showLastLabel = TRUE,
#'       plotLines = list(
#'         list(value = lineXY[2],
#'              color = 'black',
#'              dashStyle = 'shortdash',
#'              zIndex = 5,
#'              width = 2,
#'              label = list(
#'                text = lineLabelsXY[1],
#'                style = list(
#'                  color = 'black'
#'                )
#'              ))),
#'       labels = list (
#'         format = formatLabAxis,
#'         formatter = JS(aggFormAxisX)
#'       )
#'     ) %>%
#'     hc_legend(
#'       enabled = FALSE
#'     ) %>%
#'     hc_plotOptions(
#'       scatter = list(
#'         marker = list(
#'           radius = 5,
#'           states = list(
#'             hover = list(
#'               enabled = TRUE,
#'               lineColor = 'rgb(100,100,100)'
#'             )
#'           )
#'         )
#'       )
#'     ) %>%
#'     hc_tooltip(
#'       headerFormat = '',
#'       pointFormat = paste0(labelsXY[1], '{point.x} cm <br/>', labelsXY[2],'{point.y} kg')
#'     ) %>%
#'     hc_series(
#'       list(
#'         data = dt1
#'       )
#'     )
#'
#' }
#'
#'
#'
#' #' Scatter (numbers, numbers)
#' #'
#' #' Two numeric variables set in a cartesian plane
#' #'
#' #' @param data A data.frame
#' #' @return Highcharts visualization
#' #' @section ctypes:
#' #' Num-Num
#' #' @examples
#' #' hgch_scatter_NumNum(sampleData("Num-Num", nrow = 10))
#' #' @export hgch_scatter_NumNum
#' hgch_scatter_NumNum <- function(data,
#'                                 title = NULL,
#'                                 subtitle = NULL,
#'                                 caption = NULL,
#'                                 horLabel = NULL,
#'                                 verLabel = NULL,
#'                                 horLine = NULL,
#'                                 horLineLabel = NULL,
#'                                 verLine = NULL,
#'                                 verLineLabel = NULL,
#'                                 theme = NULL,
#'                                 export = FALSE, ...) {
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   horLabel <- horLabel %||% nms[1]
#'   verLabel <- verLabel %||% nms[2]
#'   horLineLabel <- horLineLabel %||% horLine
#'   verLineLabel <- verLineLabel %||% verLine
#'   title <-  title %||% ""
#'   subtitle <- subtitle %||% ""
#'   caption <- caption %||% ""
#'
#'   d <- d %>%
#'     tidyr::drop_na()
#'
#'   hc <- hchart(d, type = "scatter", hcaes(x = a, y = b)) %>%
#'     hc_tooltip(headerFormat = "",
#'                pointFormat = paste0("<b>", horLabel, "</b>: {point.a} <br/><b>",
#'                                     verLabel, "</b>: {point.b}")) %>%
#'     hc_title(text = title) %>%
#'     hc_subtitle(text = subtitle) %>%
#'     hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE, plotLines = list(list(value = verLine,
#'                                                                                          color = 'black',
#'                                                                                          dashStyle = 'shortdash',
#'                                                                                          width = 2,
#'                                                                                          label = list(text = verLineLabel)))) %>%
#'     hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = horLine,
#'                                                                   color = 'black',
#'                                                                   dashStyle = 'shortdash',
#'                                                                   width = 2,
#'                                                                   label = list(text = horLineLabel)))) %>%
#'     hc_add_theme(custom_theme(custom = theme)) %>%
#'     hc_credits(enabled = TRUE, text = caption)
#'   if (export) hc <- hc %>%
#'     hc_exporting(enabled = TRUE)
#'   hc
#' }
#'
#'
#' #' Scatter (numbers, numbers, numbers)
#' #'
#' #' Two numeric variables set in a cartesian plane with a size
#' #'
#' #' @param data A data.frame
#' #' @return Highcharts visualization
#' #' @section ctypes:
#' #' Num-Num-Num
#' #' @examples
#' #' hgch_scatter_NumNumNum(sampleData("Num-Num-Num", nrow = 10))
#' #' @export hgch_scatter_NumNumNum
#' hgch_scatter_NumNumNum <- function(data,
#'                                    title = NULL,
#'                                    subtitle = NULL,
#'                                    caption = NULL,
#'                                    horLabel = NULL,
#'                                    verLabel = NULL,
#'                                    horLine = NULL,
#'                                    horLineLabel = NULL,
#'                                    verLine = NULL,
#'                                    verLineLabel = NULL,
#'                                    theme = NULL,
#'                                    export = FALSE, ...) {
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   horLabel <- horLabel %||% nms[1]
#'   verLabel <- verLabel %||% nms[2]
#'   horLineLabel <- horLineLabel %||% horLine
#'   verLineLabel <- verLineLabel %||% verLine
#'   title <-  title %||% ""
#'   subtitle <- subtitle %||% ""
#'   caption <- caption %||% ""
#'
#'   d <- d %>%
#'     tidyr::drop_na()
#'
#'   hc <- hchart(d, type = "scatter", hcaes(x = a, y = b, size = c)) %>%
#'     hc_tooltip(headerFormat = "",
#'                pointFormat = paste0("<b>", horLabel, "</b>: {point.a}<br/><b>",
#'                                     verLabel, "</b>: {point.b}<br/><b>",
#'                                     nms[3], "</b>: {point.c}")) %>%
#'     hc_title(text = title) %>%
#'     hc_subtitle(text = subtitle) %>%
#'     hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE,  plotLines = list(list(value = verLine,
#'                                                                                           color = 'black',
#'                                                                                           dashStyle = 'shortdash',
#'                                                                                           width = 2,
#'                                                                                           label = list(text = verLineLabel)))) %>%
#'     hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = horLine,
#'                                                                   color = 'black',
#'                                                                   dashStyle = 'shortdash',
#'                                                                   width = 2,
#'                                                                   label = list(text = horLineLabel)))) %>%
#'     hc_add_theme(custom_theme(custom = theme)) %>%
#'     hc_credits(enabled = TRUE, text = caption)
#'   if (export) hc <- hc %>%
#'     hc_exporting(enabled = TRUE)
#'   hc
#' }
#'
#'
#'
#'
#' #' Scatter (categories, numbers, numbers)
#' #'
#' #' Two numeric variables set in a cartesian plane with a category
#' #'
#' #' @param data A data.frame
#' #' @return Highcharts visualization
#' #' @section ctypes:
#' #' Cat-Num-Num
#' #' @examples
#' #' hgch_scatter_CatNumNum(sampleData("Cat-Num-Num", nrow = 10))
#' #' @export hgch_scatter_CatNumNum
#' hgch_scatter_CatNumNum <- function(data,
#'                                    title = NULL,
#'                                    subtitle = NULL,
#'                                    caption = NULL,
#'                                    horLabel = NULL,
#'                                    verLabel = NULL,
#'                                    horLine = NULL,
#'                                    horLineLabel = NULL,
#'                                    verLine = NULL,
#'                                    verLineLabel = NULL,
#'                                    theme = NULL,
#'                                    export = FALSE, ...) {
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   horLabel <- horLabel %||% nms[2]
#'   verLabel <- verLabel %||% nms[3]
#'   horLineLabel <- horLineLabel %||% horLine
#'   verLineLabel <- verLineLabel %||% verLine
#'   title <-  title %||% ""
#'   subtitle <- subtitle %||% ""
#'   caption <- caption %||% ""
#'
#'   d <- d %>%
#'     tidyr::drop_na()
#'
#'   hc <- hchart(d, type = "bubble", hcaes(x = b, y = c)) %>%
#'     hc_plotOptions(series = list(dataLabels = list(enabled = TRUE,
#'                                                    format = "{point.a}"),
#'                                  marker = list(#fillColor = "rgba(47,11,113,0.6)",
#'                                    lineColor = NULL, lineWidth = 0))) %>%
#'     hc_tooltip(headerFormat = "",
#'                pointFormat = paste0("<b>", nms[1], "</b>: {point.a}<br/><b>",
#'                                     horLabel, "</b>: {point.b}<br/><b>",
#'                                     verLabel, "</b>: {point.c}")) %>%
#'     hc_title(text = title) %>%
#'     hc_subtitle(text = subtitle) %>%
#'     hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE, plotLines = list(list(value = verLine,
#'                                                                                          color = 'black',
#'                                                                                          dashStyle = 'shortdash',
#'                                                                                          width = 2,
#'                                                                                          label = list(text = verLineLabel)))) %>%
#'     hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = horLine,
#'                                                                   color = 'black',
#'                                                                   dashStyle = 'shortdash',
#'                                                                   width = 2,
#'                                                                   label = list(text = horLineLabel)))) %>%
#'     hc_add_theme(custom_theme(custom = theme)) %>%
#'     hc_credits(enabled = TRUE, text = caption)
#'   if (export) hc <- hc %>%
#'     hc_exporting(enabled = TRUE)
#'   hc
#' }
#'
#'
#' #' Coloured scatter (categories, numbers, numbers)
#' #'
#' #' Two numeric variables set in a cartesian plane coloured by a category
#' #'
#' #' @param data A data.frame
#' #' @return Highcharts visualization
#' #' @section ctypes:
#' #' Cat-Num-Num
#' #' @examples
#' #' hgch_scatter_coloured_CatNumNum(sampleData("Cat-Num-Num", nrow = 10))
#' #' @export hgch_scatter_coloured_CatNumNum
#' hgch_scatter_coloured_CatNumNum <- function(data,
#'                                             title = NULL,
#'                                             subtitle = NULL,
#'                                             caption = NULL,
#'                                             horLabel = NULL,
#'                                             verLabel = NULL,
#'                                             horLine = NULL,
#'                                             horLineLabel = NULL,
#'                                             verLine = NULL,
#'                                             verLineLabel = NULL,
#'                                             theme = NULL,
#'                                             export = FALSE, ...) {
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   horLabel <- horLabel %||% nms[2]
#'   verLabel <- verLabel %||% nms[3]
#'   horLineLabel <- horLineLabel %||% horLine
#'   verLineLabel <- verLineLabel %||% verLine
#'   title <-  title %||% ""
#'   subtitle <- subtitle %||% ""
#'   caption <- caption %||% ""
#'
#'   d <- d %>%
#'     tidyr::drop_na()
#'
#'   hc <- hchart(d, type = "scatter", hcaes(x = b, y = c, group = a)) %>%
#'     hc_tooltip(headerFormat = "",
#'                pointFormat = paste0("<b>", nms[1], "</b>: {point.a}<br/><b>",
#'                                     horLabel, "</b>: {point.b}<br/><b>",
#'                                     verLabel, "</b>: {point.c}")) %>%
#'     hc_title(text = title) %>%
#'     hc_subtitle(text = subtitle) %>%
#'     hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE, plotLines = list(list(value = verLine,
#'                                                                                          color = 'black',
#'                                                                                          dashStyle = 'shortdash',
#'                                                                                          width = 2,
#'                                                                                          label = list(text = verLineLabel)))) %>%
#'     hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = horLine,
#'                                                                   color = 'black',
#'                                                                   dashStyle = 'shortdash',
#'                                                                   width = 2,
#'                                                                   label = list(text = horLineLabel)))) %>%
#'     hc_add_theme(custom_theme(custom = theme)) %>%
#'     hc_credits(enabled = TRUE, text = caption)
#'   if (export) hc <- hc %>%
#'     hc_exporting(enabled = TRUE)
#'   hc
#' }
#'
#'
#' #' Scatter (categories, numbers, numbers, numbers)
#' #'
#' #' Two numeric variables set in a cartesian plane with a category and size
#' #'
#' #' @param data A data.frame
#' #' @return Highcharts visualization
#' #' @section ctypes:
#' #' Cat-Num-Num-Num
#' #' @examples
#' #' hgch_scatter_CatNumNumNum(sampleData("Cat-Num-Num-Num", nrow = 10))
#' #' @export hgch_scatter_CatNumNumNum
#' hgch_scatter_CatNumNumNum <- function(data,
#'                                       title = NULL,
#'                                       subtitle = NULL,
#'                                       caption = NULL,
#'                                       horLabel = NULL,
#'                                       verLabel = NULL,
#'                                       horLine = NULL,
#'                                       horLineLabel = NULL,
#'                                       verLine = NULL,
#'                                       verLineLabel = NULL,
#'                                       theme = NULL,
#'                                       export = FALSE, ...) {
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   horLabel <- horLabel %||% nms[2]
#'   verLabel <- verLabel %||% nms[3]
#'   horLineLabel <- horLineLabel %||% horLine
#'   verLineLabel <- verLineLabel %||% verLine
#'   title <-  title %||% ""
#'   subtitle <- subtitle %||% ""
#'   caption <- caption %||% ""
#'
#'   d <- d %>%
#'     tidyr::drop_na()
#'
#'   hc <- hchart(d, type = "bubble", hcaes(x = b, y = c, size = d)) %>%
#'     hc_plotOptions(series = list(dataLabels = list(enabled = TRUE,
#'                                                    format = "{point.a}"))) %>%
#'     hc_tooltip(headerFormat = "",
#'                pointFormat = paste0("<b>", nms[1], "</b>: {point.a}<br/><b>",
#'                                     horLabel, "</b>: {point.b}<br/><b>",
#'                                     verLabel, "</b>: {point.c}<br/><b>",
#'                                     nms[4], "</b>: {point.d}")) %>%
#'     hc_title(text = title) %>%
#'     hc_subtitle(text = subtitle) %>%
#'     hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE, plotLines = list(list(value = verLine,
#'                                                                                          color = 'black',
#'                                                                                          dashStyle = 'shortdash',
#'                                                                                          width = 2,
#'                                                                                          label = list(text = verLineLabel)))) %>%
#'     hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = horLine,
#'                                                                   color = 'black',
#'                                                                   dashStyle = 'shortdash',
#'                                                                   width = 2,
#'                                                                   label = list(text = horLineLabel)))) %>%
#'     hc_add_theme(custom_theme(custom = theme)) %>%
#'     hc_credits(enabled = TRUE, text = caption)
#'   if (export) hc <- hc %>%
#'     hc_exporting(enabled = TRUE)
#'   hc
#' }
#'
#'
#' #' Coloured scatter (categories, numbers, numbers, numbers)
#' #'
#' #' Two numeric variables set in a cartesian plane coloured by a category and size
#' #'
#' #' @param data A data.frame
#' #' @return Highcharts visualization
#' #' @section ctypes:
#' #' Cat-Num-Num
#' #' @examples
#' #' hgch_scatter_coloured_CatNumNumNum(sampleData("Cat-Num-Num-Num", nrow = 10))
#' #' @export hgch_scatter_coloured_CatNumNumNum
#' hgch_scatter_coloured_CatNumNumNum <- function(data,
#'                                                title = NULL,
#'                                                subtitle = NULL,
#'                                                caption = NULL,
#'                                                horLabel = NULL,
#'                                                verLabel = NULL,
#'                                                horLine = NULL,
#'                                                horLineLabel = NULL,
#'                                                verLine = NULL,
#'                                                verLineLabel = NULL,
#'                                                theme = NULL,
#'                                                export = FALSE, ...) {
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   horLabel <- horLabel %||% nms[2]
#'   verLabel <- verLabel %||% nms[3]
#'   horLineLabel <- horLineLabel %||% horLine
#'   verLineLabel <- verLineLabel %||% verLine
#'   title <-  title %||% ""
#'   subtitle <- subtitle %||% ""
#'   caption <- caption %||% ""
#'
#'   d <- d %>%
#'     tidyr::drop_na()
#'
#'   hc <- hchart(d, type = "bubble", hcaes(x = b, y = c, size = d, group = a)) %>%
#'     hc_tooltip(headerFormat = "",
#'                pointFormat = paste0("<b>", nms[1], "</b>: {point.a}<br/><b>",
#'                                     horLabel, "</b>: {point.b}<br/><b>",
#'                                     verLabel, "</b>: {point.c}<br/><b>",
#'                                     nms[4], "</b>: {point.d}")) %>%
#'     hc_title(text = title) %>%
#'     hc_subtitle(text = subtitle) %>%
#'     hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE, plotLines = list(list(value = verLine,
#'                                                                                          color = 'black',
#'                                                                                          dashStyle = 'shortdash',
#'                                                                                          width = 2,
#'                                                                                          label = list(text = verLineLabel)))) %>%
#'     hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = horLine,
#'                                                                   color = 'black',
#'                                                                   dashStyle = 'shortdash',
#'                                                                   width = 2,
#'                                                                   label = list(text = horLineLabel)))) %>%
#'     hc_add_theme(custom_theme(custom = theme)) %>%
#'     hc_credits(enabled = TRUE, text = caption)
#'   if (export) hc <- hc %>%
#'     hc_exporting(enabled = TRUE)
#'   hc
#' }
#'
#'
#' #' Scatter (categories, categories, numbers, numbers)
#' #'
#' #' Two numeric variables set in a cartesian plane with two categories
#' #'
#' #' @param data A data.frame
#' #' @return Highcharts visualization
#' #' @section ctypes:
#' #' Cat-Cat-Num-Num
#' #' @examples
#' #' hgch_scatter_CatCatNumNum(sampleData("Cat-Cat-Num-Num", nrow = 10))
#' #' @export hgch_scatter_CatCatNumNum
#' hgch_scatter_CatCatNumNum <- function(data,
#'                                       title = NULL,
#'                                       subtitle = NULL,
#'                                       caption = NULL,
#'                                       horLabel = NULL,
#'                                       verLabel = NULL,
#'                                       horLine = NULL,
#'                                       horLineLabel = NULL,
#'                                       verLine = NULL,
#'                                       verLineLabel = NULL,
#'                                       theme = NULL,
#'                                       export = FALSE, ...) {
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   horLabel <- horLabel %||% nms[3]
#'   verLabel <- verLabel %||% nms[4]
#'   horLineLabel <- horLineLabel %||% horLine
#'   verLineLabel <- verLineLabel %||% verLine
#'   title <-  title %||% ""
#'   subtitle <- subtitle %||% ""
#'   caption <- caption %||% ""
#'
#'   d <- d %>%
#'     tidyr::drop_na()
#'
#'   hc <- hchart(d, type = "bubble", hcaes(x = c, y = d, group = a)) %>%
#'     hc_plotOptions(series = list(dataLabels = list(enabled = TRUE,
#'                                                    format = "{point.b}"))) %>%
#'     hc_tooltip(headerFormat = "",
#'                pointFormat = paste0("<b>", nms[1], "</b>: {point.a}<br/><b>",
#'                                     nms[2], "</b>: {point.b}<br/><b>",
#'                                     horLabel, "</b>: {point.c}<br/><b>",
#'                                     verLabel, "</b>: {point.d}")) %>%
#'     hc_title(text = title) %>%
#'     hc_subtitle(text = subtitle) %>%
#'     hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE, plotLines = list(list(value = verLine,
#'                                                                                          color = 'black',
#'                                                                                          dashStyle = 'shortdash',
#'                                                                                          width = 2,
#'                                                                                          label = list(text = verLineLabel)))) %>%
#'     hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = horLine,
#'                                                                   color = 'black',
#'                                                                   dashStyle = 'shortdash',
#'                                                                   width = 2,
#'                                                                   label = list(text = horLineLabel)))) %>%
#'     hc_add_theme(custom_theme(custom = theme)) %>%
#'     hc_credits(enabled = TRUE, text = caption)
#'   if (export) hc <- hc %>%
#'     hc_exporting(enabled = TRUE)
#'   hc
#' }
#'
#'
#' #' Scatter (categories, categories, numbers, numbers, numbers)
#' #'
#' #' Two numeric variables set in a cartesian plane with two categories and size
#' #'
#' #' @param data A data.frame
#' #' @return Highcharts visualization
#' #' @section ctypes:
#' #' Cat-Cat-Num-Num-Num
#' #' @examples
#' #' hgch_scatter_CatCatNumNumNum(sampleData("Cat-Cat-Num-Num-Num", nrow = 10))
#' #' @export hgch_scatter_CatCatNumNumNum
#' hgch_scatter_CatCatNumNumNum <- function(data,
#'                                          title = NULL,
#'                                          subtitle = NULL,
#'                                          caption = NULL,
#'                                          horLabel = NULL,
#'                                          verLabel = NULL,
#'                                          horLine = NULL,
#'                                          horLineLabel = NULL,
#'                                          verLine = NULL,
#'                                          verLineLabel = NULL,
#'                                          theme = NULL,
#'                                          export = FALSE, ...) {
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'   d <- f$d
#'
#'   horLabel <- horLabel %||% nms[3]
#'   verLabel <- verLabel %||% nms[4]
#'   horLineLabel <- horLineLabel %||% horLine
#'   verLineLabel <- verLineLabel %||% verLine
#'   title <-  title %||% ""
#'   subtitle <- subtitle %||% ""
#'   caption <- caption %||% ""
#'
#'   d <- d %>%
#'     tidyr::drop_na()
#'
#'   hc <- hchart(d, type = "bubble", hcaes(x = c, y = d, size = e, group = a)) %>%
#'     hc_plotOptions(series = list(dataLabels = list(enabled = TRUE,
#'                                                    format = "{point.b}"))) %>%
#'     hc_tooltip(headerFormat = "",
#'                pointFormat = paste0("<b>", nms[1], "</b>: {point.a}<br/><b>",
#'                                     nms[2], "</b>: {point.b}<br/><b>",
#'                                     horLabel, "</b>: {point.c}<br/><b>",
#'                                     verLabel, "</b>: {point.d}<br/><b>",
#'                                     nms[5], "</b>: {point.e}")) %>%
#'     hc_title(text = title) %>%
#'     hc_subtitle(text = subtitle) %>%
#'     hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE, plotLines = list(list(value = verLine,
#'                                                                                          color = 'black',
#'                                                                                          dashStyle = 'shortdash',
#'                                                                                          width = 2,
#'                                                                                          label = list(text = verLineLabel)))) %>%
#'     hc_yAxis(title = list(text = verLabel), plotLines = list(list(value = horLine,
#'                                                                   color = 'black',
#'                                                                   dashStyle = 'shortdash',
#'                                                                   width = 2,
#'                                                                   label = list(text = horLineLabel)))) %>%
#'     hc_add_theme(custom_theme(custom = theme)) %>%
#'     hc_credits(enabled = TRUE, text = caption)
#'   if (export) hc <- hc %>%
#'     hc_exporting(enabled = TRUE)
#'   hc
#' }
