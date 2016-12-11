
#' hgch_pie_Ca
#' @name hgch_pie_Ca
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_pie_Ca(sampleData("Ca",nrow = 10))
hgch_pie_Ca <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                            sort = "no", aggregate = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = n())

  hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format=   '<b>{point.name}</b>: {point.percentage:.1f} %'))
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_pie_CaNu
#' @name hgch_pie_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_pie_CaNu(sampleData("Ca",nrow = 10))
hgch_pie_CaNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                        sort = "no", aggregate = "sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = sum(b))

  hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format=   '<b>{point.name}</b>: {point.percentage:.1f} %'))
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}


#' hgch_donut_Ca
#' @name hgch_donut_Ca
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_donut_Ca(sampleData("Ca",nrow = 10))
hgch_donut_Ca <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                        sort = "no", aggregate = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = n())

  hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(innerSize= '60%',dataLabels = list(enabled = TRUE,format=   '<b>{point.name}</b>: {point.percentage:.1f} %'))
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_donut_CaNu
#' @name hgch_donut_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_donut_CaNu(sampleData("Ca",nrow = 10))
hgch_donut_CaNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                          sort = "no", aggregate = "sum", ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = sum(b))

  hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(innerSize= '60%',dataLabels = list(enabled = TRUE,format=   '<b>{point.name}</b>: {point.percentage:.1f} %'))
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}





#' hgch_treemap_CaNu
#' @name hgch_treemap_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_treemap_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_treemap_CaNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                              minColor = "#E63917", maxColor= "#18941E", reverse = TRUE, ...){
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% nms[2]
  data <- f$d
  d <- data %>% na.omit() %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b))
  hchart(d, "treemap", hcaes(x = a, value = b, color = b)) %>%
    hc_title(text = title) %>%
    hc_colorAxis(maxColor = maxColor, minColor = minColor,reversed = reverse)
}



#' hgch_spider_CaNu
#' @name hgch_spider_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_spider_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_spider_CaNu <- function(data,
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
  highchart() %>%
    hc_chart(type = "line", polar = TRUE) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle),
             categories = d$a,tickmarkPlacement = 'on',lineWidth = 0
             ) %>%
    hc_series(
      list(
        name = nms[2],
        data = d$b,
        pointPlacement = 'on'
      ))
}







