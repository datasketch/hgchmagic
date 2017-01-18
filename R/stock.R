#' hgch_stock_multilines_DaNu
#' @name hgch_stock_multilines_DaNu
#' @param d1 A dataframe
#' @param d2 A dataframe
#' @param d3 A dataframe
#' @export
#' @return highcharts viz
#' @section ftypes: Da-Nu
#' @examples
#' hgch_stock_multilines_DaNu(sampleData("Da-Nu", nrow = 10), sampleData("Da-Nu", nrow = 18))

hgch_stock_multilines_DaNu <- function(d1, d2, d3 = NULL, title = NULL, subtitle = NULL,
                                       caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                       symbol = NULL,  startAtZero = FALSE, ...) {

  f <- fringe(d1)
  nms <- getClabels(f)
  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  a0 <- d1[complete.cases(d1), ]
  b0 <- d2[complete.cases(d2), ]
  names(a0)[1] <- "a"
  names(b0)[1] <- "a"
  a0 <- xts::xts(a0[, -1], order.by = as.POSIXct(a0$a))
  b0 <- xts::xts(b0[, -1], order.by = as.POSIXct(b0$a))
  h0 <- highchart() %>%
    hc_add_series_xts(a0, color = "firebrick") %>%
    hc_add_series_xts(b0, color = "forestgreen") %>%
    hc_add_theme(hc_theme_gridlight()) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = yAxisTitle))

  if (!is.null(d3)) {
    c0 <- d3[complete.cases(d3), ]
    names(c0)[1] <- "a"
    c0 <- xts::xts(c0[, -1], order.by = as.POSIXct(c0$a))
    h0 <- h0 %>%
      hc_add_series_xts(c0, color = "gold")
  }
  h0

}

#' hgch_stock_lines_DaNuP
#' @name hgch_stock_lines_DaNuP
#' @param data A dataframe
#' @export
#' @return highcharts viz
#' @section ftypes: Da-NuP
#' @examples
#' hgch_stock_lines_DaNuP(sampleData("Da-NuP", nrow = 15))

hgch_stock_lines_DaNuP <- function(data, title = NULL, subtitle = NULL,
                                   caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                   symbol = NULL,  startAtZero = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  a0 <- data[complete.cases(data), ]
  names(a0)[1] <- "a"
  a1 <- xts::xts(a0[, -c(1, 3)], order.by = as.POSIXct(a0$a))
  a2 <- xts::xts(a0[, -c(1, 2)], order.by = as.POSIXct(a0$a))
  h0 <- highchart() %>%
    hc_add_series_xts(a1, color = "darkmagenta") %>%
    hc_add_series_xts(a2, color = "darkolivegreen") %>%
    hc_add_theme(hc_theme_google()) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = yAxisTitle))
  h0

}

#' hgch_stock_ohlc_DaNu4
#' @name hgch_stock_ohlc_DaNu4
#' @param data A dataframe
#' @param type Factor: "candlestick", "ohlc"
#' @export
#' @return highcharts viz
#' @section ftypes: Da-Nu-Nu-Nu-Nu
#' @examples
#' hgch_stock_ohlc_DaNu4(sampleData("Da-Nu-Nu-Nu-Nu", nrow = 15))

hgch_stock_ohlc_DaNu4 <- function(data, type = "ohlc", title = NULL, subtitle = NULL,
                                  caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                  symbol = NULL,  startAtZero = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  a0 <- data[complete.cases(data), ]
  names(a0)[1] <- "a"
  a0 <- xts::xts(a0[, -1], order.by = as.POSIXct(a0$a))
  h0 <- highchart() %>%
    hc_add_series_ohlc(a0, type = type) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = yAxisTitle))
  h0

}



