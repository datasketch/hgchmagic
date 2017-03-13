
#' hgch_pie_Ca
#' @name hgch_pie_Ca
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_pie_Ca(sampleData("Ca",nrow = 10))
hgch_pie_Ca <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                            sort = "no", aggregate = "count", export = FALSE,...){

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
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_pie_CaNu
#' @name hgch_pie_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_pie_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_pie_CaNu <- function(data, title = NULL, subtitle = NULL, caption = NULL,
                          xAxisTitle = NULL, yAxisTitle = NULL, back_color = 'white',leg_op = TRUE,
                          color_title = 'black',sort = "no", aggregate = "sum", export = FALSE,...){


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
      series = list(dataLabels = list(enabled = leg_op ,format=   '<b>{point.name}</b>: {point.percentage:.1f} %'))
    ) %>%
    hc_chart(backgroundColor = back_color) %>%
    hc_title(text = title,style = list(color = color_title, useHTML = TRUE)) %>%
    hc_subtitle(text = subtitle) %>%
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
hgch_donut_Ca <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                        sort = "no", aggregate = "count", export = FALSE,...){

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
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_donut_CaNu
#' @name hgch_donut_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_donut_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_donut_CaNu <- function(data, title = NULL, subtitle = NULL, caption = NULL,
                            xAxisTitle = NULL, yAxisTitle = NULL, back_color = 'white',
                            color_title = 'black', leg_op = TRUE,
                            sort = "no", aggregate = "sum", export = FALSE,...){


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
      series = list(innerSize= '60%',dataLabels = list(enabled = leg_op,format=   '<b>{point.name}</b>: {point.percentage:.1f} %'))
    ) %>%
    hc_chart(backgroundColor = back_color) %>%
    hc_title(text = title,style = list(color = color_title, useHTML = TRUE))%>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_radar_Ca
#' @name hgch_radar_Ca
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_radar_Ca(sampleData("Ca", nrow = 10))
hgch_radar_Ca <- function(data,
                            title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                            sort = "no", aggregate = "mean", export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% nms[2]
  d <- f$d
  d <- na.omit(d)
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = n())
  highchart() %>%
    hc_chart(type = "line", polar = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
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


#' hgch_radar_CaNu
#' @name hgch_radar_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_radar_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_radar_CaNu <- function(data,
                            title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                            sort = "no", aggregate = "mean", export = FALSE,...){

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
    hc_subtitle(text = subtitle) %>%
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

