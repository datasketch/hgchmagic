
#' Line
#' @name hgch_line_DaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Da-Nu
#' @examples
#' hgch_line_DaNu(sampleData("Da-Nu",nrow = 10))
hgch_line_DaNu <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
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
#' @name hgch_line_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_line_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_line_CaNu <-hgch_line_DaNu


#' Line
#' @name hgch_line_CaYeNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_line_CaYeNu(sampleData("Ca-Ye-Nu",nrow = 10))
hgch_line_CaYeNu <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
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
#' @name hgch_line_YeNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ye-Nu
#' @examples
#' hgch_line_YeNu(sampleData("Ye-Nu",nrow = 10))
hgch_line_YeNu <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
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
#' @name hgch_line_CaCaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ca-Nu
#' @examples
#' hgch_line_CaCaNu(sampleData("Ca-Ca-Nu",nrow = 10))
hgch_line_CaCaNu <- hgch_line_CaYeNu


#' Line
#' @name hgch_line_CaDaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Da-Nu
#' @examples
#' hgch_line_CaDaNu(sampleData("Ca-Da-Nu",nrow = 10))
hgch_line_CaDaNu <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
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
#' @name hgch_2yline_YeNuNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftypes: Ye-Nu-Nu
#' @examples
#' hgch_2yline_YeNuNu(sampleData("Ye-Nu-Nu",nrow = 10))
hgch_2yline_YeNuNu <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL,
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
#' @name hgch_multilines_YeNuP
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftypes: Ye-NuP
#' @examples
#' hgch_multilines_YeNuP(sampleData("Ye-NuP",nrow = 10))
hgch_multilines_YeNuP <- function(data,
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
#' @name hgch_slope_CaYeNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' hgch_slope_CaYeNu(sampleData('Ca-Ye-Nu'))
hgch_slope_CaYeNu <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
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
