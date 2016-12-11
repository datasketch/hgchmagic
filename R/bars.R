

#' hgch_bar_CaYeNu
#' @name hgch_bar_CaYeNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_bar_CaYeNu(sampleData("Ye-Nu-Nu",nrow = 10))
hgch_bar_CaYeNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                            symbol = NULL,  startAtZero = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = mean(c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hc <- hchart(d, type = "column", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc
}

#' hgch_bar_CaCaNu
#' @name hgch_bar_CaCaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ca-Nu
#' @examples
#' hgch_bar_CaCaNu(sampleData("Ca-Ca-Nu",nrow = 10))
hgch_bar_CaCaNu <- hgch_bar_CaYeNu


#' hgch_bar_YeNu
#' @name hgch_bar_YeNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_bar_YeNu(sampleData("Ye-Nu",nrow = 10))
hgch_bar_YeNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                         startAtZero = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""

  d <- f$d %>% na.omit() %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b))
  if(nrow(d)==0) return()
  hc <- hchart(d, type = "column", hcaes(x = a, y = b)) %>%
    # hc_plotOptions(
    #   series = list(marker = list(enabled = TRUE, symbol =  symbol))
    # ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc
}




#' hgch_bar_hor_CaNu
#' @name hgch_bar_hor_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_bar_hor_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_bar_hor_CaNu <- function(data,
                              title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                              sort = "no", aggregate = "mean", ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% nms[2]
  d <- f$d
  d <- na.omit(d)
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b,na.rm = TRUE))
  if(sort == "top"){
    d <- d %>% dplyr::arrange(desc(b))
  }
  hchart(d, type = "bar", hcaes(x = a, y = b)) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_bar_hor_top_CaNu
#' @name hgch_bar_hor_top_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_bar_hor_top_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_bar_hor_top_CaNu <- function(data,
                                  title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                  reverse = TRUE, aggregate = "mean", ...){

  hgch_bar_hor_CaNu(data, title = title, xAxisTitle = xAxisTitle,
                    yAxisTitle = yAxisTitle, sort = "top", aggregate = aggregate)

}


#' hgch_bar_hor_Ca
#' @name hgch_bar_hor_Ca
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_bar_hor_Ca(sampleData("Ca",nrow = 10))
hgch_bar_hor_Ca <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                            sort = "no", aggregate = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = n())
  if(sort == "top"){
    d <- d %>% dplyr::arrange(desc(b))
  }
  hchart(d, type = "bar", hcaes(x = a, y = b)) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_bar_hor_top_Ca
#' @name hgch_bar_hor_top_Ca
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_bar_hor_top_Ca(sampleData("Ca-Nu",nrow = 10))
hgch_bar_hor_top_Ca <- function(data,
                                title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                reverse = TRUE, aggregate = "count", ...){

  hgch_bar_hor_Ca(data, title = title, xAxisTitle = xAxisTitle,
                  yAxisTitle = yAxisTitle, sort = "top", aggregate = aggregate)

}



