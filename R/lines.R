#' Line
#' 
#' Line
#' 
#' 
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Dat-Num
#' @examples
#' 
#' hgch_line_DatNum(sampleDatta("Dat-Num",nrow = 10))
#' 
#' @export hgch_line_DatNum
hgch_line_DatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                           symbol = NULL, startAtZero = FALSE, theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"

  d <- f$d %>% na.omit() %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b))
  if(nrow(d)==0) return()
  hc <- hchart(d, type = "line", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = FALSE, symbol =  symbol))
    ) %>%
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



#' Line
#' 
#' Line
#' 
#' 
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#' 
#' hgch_line_CatNum(sampleDatta("Cat-Num",nrow = 10))
#' 
#' @export hgch_line_CatNum
hgch_line_CatNum <-hgch_line_DatNum




#' Line
#' 
#' Line
#' 
#' 
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' 
#' hgch_line_CatYeaNum(sampleDatta("Cat-Yea-Num",nrow = 10))
#' 
#' @export hgch_line_CatYeaNum
hgch_line_CatYeaNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                             symbol = NULL, startAtZero = FALSE, theme = NULL, export = FALSE,...){


  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  f <- fringe(data)
  if(nrow(f$data)==0) return()
  nms <- getClabels(f)
  #data <- f$d

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"

  data  <- f$d %>%
    tidyr::drop_na() %>%
    dplyr::group_by(a,b) %>%
    dplyr::summarise(c = mean(c)) %>%
    dplyr::arrange(b)

  list_pre <- data %>%
    group_by(name = a) %>%
    do(data = .$c)

  list_series <- list_parse(list_pre)


  hc <- highchart() %>%
    hc_xAxis(categories = data$b) %>%
    hc_add_series_list(list_series)


  hc <- hc %>%
        hc_title(text = title) %>%
        hc_subtitle(text = subtitle) %>%
        hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
        hc_yAxis(title = list(text=yAxisTitle), allowDecimals = FALSE)

  if(!is.null(symbol)){
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    )
  }
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}



#' Line
#' 
#' Line
#' 
#' 
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Yea-Num
#' @examples
#' 
#' hgch_line_YeaNum(sampleDatta("Yea-Num",nrow = 10))
#' 
#' @export hgch_line_YeaNum
hgch_line_YeaNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                           symbol = NULL, startAtZero = FALSE, theme = NULL, export = FALSE,...){
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"

  d <- f$d %>% na.omit() %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b))
  if(nrow(d)==0) return()
  hc <- hchart(d, type = "line", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    ) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE)
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Line
#' 
#' Line
#' 
#' 
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' 
#' hgch_line_CatCatNum(sampleDatta("Cat-Cat-Num",nrow = 10))
#' 
#' @export hgch_line_CatCatNum
hgch_line_CatCatNum <- hgch_line_CatYeaNum




#' Line
#' 
#' Line
#' 
#' 
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' 
#' hgch_line_CatDatNum(sampleDatta("Cat-Dat-Num",nrow = 10))
#' 
#' @export hgch_line_CatDatNum
hgch_line_CatDatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                             symbol = NULL, startAtZero = FALSE, theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>%
    dplyr::summarise(c = mean(c)) %>%
    mutate(b = as.Date(b))
  if(nrow(d)==0) return()
  hc <- hchart(d, type = "line", hcaes(x = b, y = c, group = a)) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle), allowDecimals = FALSE)
  if(!is.null(symbol)){
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    )
  }
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}





#' 2 y line
#' 
#' 2 y line
#' 
#' 
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypess: Yea-Num-Num
#' @examples
#' 
#' hgch_2yline_YeaNumNum(sampleDatta("Yea-Num-Num",nrow = 10))
#' 
#' @export hgch_2yline_YeaNumNum
hgch_2yline_YeaNumNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL,
                               yAxisTitle1 = NULL, yAxisTitle2 = NULL,
                               symbol = NULL, theme = NULL, export = FALSE,...){
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle1 <- yAxisTitle1 %||% nms[2]
  yAxisTitle2 <- yAxisTitle2 %||% nms[3]
  title <-  title %||% ""

  d <- f$d %>%
    tidyr::drop_na(a) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = mean(b), c = mean(c)) %>%
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

  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  hc
}




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
#' hgch_multilines_YeaNumP(sampleDatta("Yea-NumP",nrow = 10))
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



#' Slope
#' 
#' Slope
#' 
#' 
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypess: Cat-Yea-Num
#' @examples
#' 
#' hgch_slope_CatYeaNum(sampleDatta('Cat-Yea-Num'))
#' 
#' @export hgch_slope_CatYeaNum
hgch_slope_CatYeaNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                              symbol = NULL, startAtZero = FALSE, theme = NULL, export = FALSE,...){


  if(nrow(data)==0) return()

  f <- fringe(data)
  nms <- getClabels(f)
  #data <- f$d

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"


  data  <- f$d %>%
    tidyr::drop_na(a) %>%
    dplyr::group_by(a,b) %>%
    dplyr::summarise(c = mean(c)) %>%
    dplyr::arrange(b)

  list_pre <- data %>%
    group_by(name = a) %>%
    do(data = .$c)

  list_series <- list_parse(list_pre)


  hc <- highchart() %>%
    hc_xAxis(categories = unique(data$b)) %>%
    hc_add_series_list(list_series)

  hc <- hc %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle), allowDecimals = FALSE)

  if(!is.null(symbol)){
    hc <- hc %>% hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    )
  }
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}
