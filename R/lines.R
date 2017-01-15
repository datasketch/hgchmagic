
#' hgch_line_DaNu
#' @name hgch_line_DaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Da-Nu
#' @examples
#' hgch_line_DaNu(sampleData("Ca-Da-Nu",nrow = 10))
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

#' hgch_line_CaNu
#' @name hgch_line_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_line_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_line_CaNu <-hgch_line_DaNu


#' hgch_line_CaYeNu
#' @name hgch_line_CaYeNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_line_CaYeNu(sampleData("Ye-Nu-Nu",nrow = 10))
hgch_line_CaYeNu <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                             symbol = NULL, startAtZero = FALSE, theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = mean(c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
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

#' hgch_line_YeNu
#' @name hgch_line_YeNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_line_YeNu(sampleData("Ye-Nu",nrow = 10))
hgch_line_YeNu <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                           symbol = NULL, startAtZero = FALSE, export = FALSE,...){
  data <- sampleData("Ye-Nu", nrow = 20)
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
#' hgch_line_CaCaNu
#' @name hgch_line_CaCaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ca-Nu
#' @examples
#' hgch_line_CaCaNu(sampleData("Ca-Ca-Nu",nrow = 10))
hgch_line_CaCaNu <- hgch_line_CaYeNu


#' hgch_line_CaDaNu
#' @name hgch_line_CaDaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
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



#' hgch_2yline_YeNuNu
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
 data <- sampleData("Ye-Nu-Nu",nrow = 10)
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle1 <- yAxisTitle1 %||% nms[2]
  yAxisTitle2 <- yAxisTitle2 %||% nms[3]
  title <-  title %||% ""

  d <- f$d %>%
       dplyr::group_by(a) %>%
       dplyr::summarise(b = mean(b), c = mean(c))

  hc <- highchart() %>%
    # hc_xAxis(categories = d$a) %>%
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
    hc_add_series(name = yAxisTitle1, type = "line",
                  data = as.matrix(d[,c("a","b")])) %>%
    hc_add_series(name = yAxisTitle2, type = "line", yAxis = 1,
                  data = as.matrix(d[,c("a","c")]))
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  hc
  }




#' hgch_multilines_YeNuP
#' Multilines
#' @name hgch_multilines.
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftypes: Ye-Nu*
#' @examples
#' hgch_multilines_YeNuP(sampleData("Ye-Nu-Nu",nrow = 10))
hgch_multilines_YeNuP <- function(data,
                                  title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                  symbol = NULL,  startAtZero = FALSE,...){
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  d <- f$d %>% tidyr::gather(variable,value, -a) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(a,variable) %>%
    dplyr::summarise(value = mean(value)) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d,"variable",codes))
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
