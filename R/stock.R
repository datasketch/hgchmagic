
#' hgch_stock_multilines_DaNu
#' @name hgch_stock_multilines_DaNu
#' @param list A list of dataframes
#' @export
#' @return highcharts viz
#' @section ftypes: Da-Nu   ----CORREGIRRRRRRRRRRRRRRRRR
#' @examples
#' hgch_stock_multilines_DaNu(sampleData("Da-Nu", nrow = 10), sampleData("Da-Nu", nrow = 18))

hgch_stock_multilines_DaNu <- function(list, title = NULL, subtitle = NULL,
                                       caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                       symbol = NULL,  startAtZero = FALSE, ...) {


  nms1 <- getClabels(fringe(list[[1]]))
  xAxisTitle <- xAxisTitle %||% nms1[1]
  #yAxisTitle <- yAxisTitle %||% nms[2]
  #title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  s00 <- map(list, function(z) {if (sum(grep("Close", names(z), ignore.case = TRUE)) > 0) 
                                     {dplyr::select(z, Date, dplyr::contains("Close"))}
                                else {z}})
  n0 <- as.character(map_chr(s00, ~ grep("Date", names(.x), ignore.case = TRUE,
                                         value = TRUE, invert = TRUE)))
  if (length(s00) == 1) {
    h0 <- hgch_stock_lines_DaNuP(s00[[1]], title = title, subtitle = subtitle, caption = caption, 
                                 xAxisTitle = xAxisTitle, yAxisTitle = yAxisTitle,
                                 symbol = symbol,  startAtZero = startAtZero)
  }
  
  else {
    h0 <- hgch_stock_lines_DaNuP(s00[[1]], title = title, subtitle = subtitle, caption = caption, 
                                 xAxisTitle = xAxisTitle, yAxisTitle = yAxisTitle,
                                 symbol = symbol,  startAtZero = startAtZero)
    
    s01 <- s00[names(s00)[-1]] %>%
      map(~ .x[complete.cases(.x), ]) %>%
      map(function(z) {names(z)[1] <- "a"
                       xts::xts(z[, -1], order.by = as.POSIXct(z$a))})
    
    map2(s01, n0[-1], function(z, l) {assign("h0", hc_add_series(h0, data = z, name = l),
                                             envir = parent.env(environment()))})
  }
  
  h0
}
  
#' hgch_stock_performance
#' @name hgch_stock_performance
#' @param list A list of dataframes
#' @export
#' @return highcharts viz
#' @section ftypes: Da-Nu-Nu-Nu-Nu (CORREEEEEGIRRRRRRRR TIPO OHLC)
#' @examples
#' hgch_stock_performance(sampleData("Da-Nu", nrow = 10), sampleData("Da-Nu", nrow = 18))

hgch_stock_performance <- function(list, since, title = NULL, subtitle = NULL,
                                   caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                   symbol = NULL,  startAtZero = FALSE, ...) {
  
  nms1 <- getClabels(fringe(list[[1]]))
  xAxisTitle <- xAxisTitle %||% nms1[1]
  #yAxisTitle <- yAxisTitle %||% nms[2]
  #title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  s00 <- map(list, function(z) {if (sum(grep("Close", names(z), ignore.case = TRUE)) > 0) 
                                   {dplyr::select(z, Date, dplyr::contains("Close"))}
                                else {z}}) %>%
    map(~ .x[complete.cases(.x), ])
  n0 <- as.character(map_chr(s00, ~ grep("Date", names(.x), ignore.case = TRUE,
                                         value = TRUE, invert = TRUE)))
  s01 <- map(s00, function(z) {names(z)[1] <- "a"
                          xts::xts(z[, -1], order.by = as.POSIXct(z$a))}) %>%
    map(function(z) {z[paste0(since, "::"), ]})
  
  s02 <-  map(s01, as.data.frame) %>%
    map(~ cbind(.x[, -1], "P0" = ((.x[, 1] * 100) / .x[1, 1]) - 100)) %>%
    map(~ round(.x, digits = 6)) %>%
    map(xts::as.xts)

  h0 <- highchart(type = "stock") %>%
    hc_plotOptions(series = list(marker = list(enabled = FALSE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = yAxisTitle), opposite = TRUE) %>%
    #hc_legend(enabled = TRUE, layout = "horizontal")
    #hc_add_theme(hc_theme_google())

    map2(s02, n0, function(z, l) {assign("h0", hc_add_series(h0, data = z, name = l),
                                                     envir = parent.env(environment()))})

  h0

}

#' hgch_stock_2yaxis
#' @name hgch_stock_2yaxis
#' @param d1 A dataframe
#' @param d2 A dataframe
#' @export
#' @return highcharts viz
#' @section ftypes: Da-Nu
#' @examples
#' hgch_stock_2yaxis(sampleData("Da-Nu", nrow = 10), sampleData("Da-Nu", nrow = 18))

hgch_stock_2yaxis <- function(d1, d2, title = NULL, subtitle = NULL, caption = NULL,
                                    xAxisTitle = NULL, yAxisTitle = c(NULL, NULL),
                                    symbol = NULL,  startAtZero = FALSE, ...) {

  nms1 <- getClabels(fringe(d1))
  nms2 <- getClabels(fringe(d2))
  xAxisTitle <- xAxisTitle %||% nms1[1]
  yAxisTitle1 <- yAxisTitle[1] %||% strsplit(nms1[2], split = "\\.")[[1]][1]
  yAxisTitle2 <- yAxisTitle[2] %||% strsplit(nms2[2], split = "\\.")[[1]][1]
  #title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  a0 <- d1[complete.cases(d1), ]
  b0 <- d2[complete.cases(d2), ]
  names(a0)[1] <- "a"
  names(b0)[1] <- "a"
  a0 <- xts::xts(a0[, -1], order.by = as.POSIXct(a0$a))
  b0 <- xts::xts(b0[, -1], order.by = as.POSIXct(b0$a))
  h0 <- highchart(type = "stock") %>%
    hc_yAxis_multiples(
      list(title = list(text = yAxisTitle1),
           lineWidth = 0,
           opposite = FALSE),
      list(title = list(text = yAxisTitle2),
           lineWidth = 0)) %>%
    hc_add_series(a0, name = strsplit(nms1[2], split = "\\.")[[1]][1], color = "forestgreen") %>%
    hc_add_series(b0, name = strsplit(nms2[2], split = "\\.")[[1]][1], yAxis = 1) %>%
    hc_add_theme(hc_theme_google()) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle), allowDecimals = FALSE)

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

  nms <- getClabels(fringe(data))
  xAxisTitle <- xAxisTitle %||% nms[1]
  #title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  d00 <- data[complete.cases(data), ]
  names(d00)[1] <- "a"
  d01 <- map(as.list(nms[-1]), function(x) {d00[, c("a", x)]}) %>%
    map(~ xts::xts(.x[, -1], order.by = as.POSIXct(.x$a)))

  h00 <- highchart(type = "stock") %>%
    hc_plotOptions(series = list(marker = list(enabled = FALSE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = yAxisTitle), opposite = FALSE)

  map2(d01, nms[-1], function(z, l) {assign("h00", hc_add_series(h00, data = z, name = l),
                                      envir = parent.env(environment()))})

  h00

}


#' hgch_stock_multiple_ohlc
#' @name hgch_stock_multiple_ohlc
#' @param list A list of data frames
#' @param type Factor: "candlestick", "ohlc"
#' @export
#' @return highcharts viz
#' @section ftypes: Da-Nu-Nu-Nu-Nu (COOORRREEEEGIR TIPOO OHLC)
#' @examples
#' hgch_stock_multiple_ohlc(sampleData("Da-Nu-Nu-Nu-Nu", nrow = 15), sampleData("Da-Nu-Nu-Nu-Nu"), nrow = 13)

hgch_stock_multiple_ohlc <- function(list, title = NULL, subtitle = NULL,
                                  caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                  symbol = NULL, startAtZero = FALSE, ...) {

  nms1 <- getClabels(fringe(list[[1]]))
  xAxisTitle <- xAxisTitle %||% nms1[1]
  #yAxisTitle <- yAxisTitle %||% nms[2]
  #title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  s00 <- map(list, ~ .x[complete.cases(.x), ]) %>%
    map(function(z) {names(z)[1] <- "a"
                     xts::xts(z[, -1], order.by = as.POSIXct(z$a))})
  
  h0 <- highchart(type = "stock") %>%
    hc_plotOptions(series = list(marker = list(enabled = FALSE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = yAxisTitle), showFirstLabel = FALSE) %>%
    hc_add_theme(hc_theme_gridlight())
    
  map(s00, function(z) {assign("h0", hc_add_series(h0, data = z), envir = parent.env(environment()))})

  h0

}

####---------------volumen
#' hgch_stock_ohlc_volume
#' @name hgch_stock_ohlc_volume
#' @param list A list of dataframes
#' @param volume A symbol (character) of whom the volume is going to be ploted.
#' Default NULL
#' @export
#' @return highcharts viz
#' @section ftypes: Da-Nu-Nu-Nu-Nu (COOOOOOORRRRRRRRREEEEEEEEEEEEGIR)
#' @examples
#' hgch_stock_ohlc_volume(sampleData("Da-Nu-Nu-Nu-Nu", nrow = 15), sampleData("Da-Nu-Nu-Nu-Nu"), nrow = 13)

hgch_stock_ohlc_volume <- function(list, volume = NULL, title = NULL, subtitle = NULL,
                                     caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                     symbol = NULL,  startAtZero = FALSE, ...) {
  
  nms1 <- getClabels(fringe(list[[1]]))
  xAxisTitle <- xAxisTitle %||% nms1[1]
  #yAxisTitle <- yAxisTitle %||% nms[2]
  #title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  s00 <- map(list, ~ .x[complete.cases(.x), ]) %>%
    map(function(z) {names(z)[1] <- "a"
    xts::xts(z[, -1], order.by = as.POSIXct(z$a))})
  
  h0 <- highchart(type = "stock") %>%
    hc_plotOptions(series = list(marker = list(enabled = FALSE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle), allowDecimals = FALSE) %>%
    hc_add_theme(hc_theme_gridlight())
  
  if (!is.null(volume)) {
    d00 <- as.data.frame(s00[[volume]]) %>%
      dplyr::select(dplyr::contains("Close")) %>%
      xts::as.xts()
    h0 <- hc_yAxis_multiples(h0, create_yaxis(naxis = 2,
                                        heights = c(2, 1),
                                        turnopposite =TRUE))
    h0 <- hc_add_series(h0, data = d00, type = "column", color = "forestgreen", yAxis = 1)
    map(s00, function(z) {assign("h0", hc_add_series(h0, data = z, yAxis = 0),
                                 envir = parent.env(environment()))})
  }
  else {
    h0 <- hc_yAxis(h0, title = list(text = yAxisTitle), showFirstLabel = FALSE) 
    map(s00, function(z) {assign("h0", hc_add_series(h0, data = z), 
                                 envir = parent.env(environment()))})
  }
  h0
  
}

