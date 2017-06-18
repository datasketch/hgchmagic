#' hgch_pie_Cat
#'
#' hgch_pie_Cat
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ftype:
#' Cat
#' @examples
#'
#' hgch_pie_Cat(sampleData("Cat",nrow = 10))
#'
#' @export hgch_pie_Cat
hgch_pie_Cat <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                            sort = "no", aggregate = "count", export = FALSE, theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = n())

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format=   '<b>{point.name}</b>: {point.percentage:.1f} %'))
    ) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}



#' hgch_pie_CatNum
#'
#' hgch_pie_CatNum
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ftype:
#' Cat-Num
#' @examples
#'
#' hgch_pie_CatNum(sampleData("Cat-Num",nrow = 10))
#'
#' @export hgch_pie_CatNum
hgch_pie_CatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                        sort = "no", aggregate = "sum", export = FALSE, theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = sum(b))

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format=   '<b>{point.name}</b>: {point.percentage:.1f} %'))
    ) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))

  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}




#' hgch_donut_Cat
#'
#' hgch_donut_Cat
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ftype:
#' Cat
#' @examples
#'
#' hgch_donut_Cat(sampleData("Cat",nrow = 10))
#'
#' @export hgch_donut_Cat
hgch_donut_Cat <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                        sort = "no", aggregate = "count", export = FALSE, theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = n())

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(innerSize= '60%',dataLabels = list(enabled = TRUE,format=   '<b>{point.name}</b>: {point.percentage:.1f} %'))
    ) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))

  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}



#' hgch_donut_CatNum
#'
#' hgch_donut_CatNum
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ftype:
#' Cat
#' @examples
#'
#' hgch_donut_CatNum(sampleData("Cat-Num",nrow = 10))
#'
#' @export hgch_donut_CatNum
hgch_donut_CatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                          sort = "no", aggregate = "sum", export = FALSE, theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = sum(b))

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(innerSize= '60%',dataLabels = list(enabled = TRUE,format=   '<b>{point.name}</b>: {point.percentage:.1f} %'))
    ) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))

  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}

#' hgch_radar_Cat
#'
#' hgch_radar_Cat
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ftype:
#' Cat
#' @examples
#'
#' hgch_radar_Cat(sampleData("Cat", nrow = 10))
#'
#' @export hgch_radar_Cat
hgch_radar_Cat <- function(data,
                           title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                           sort = "no", aggregate = "mean", export = FALSE, theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% nms[2]
  d <- f$d
  d <- na.omit(d)
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = n())
  hc <- highchart() %>%
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

  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}




#' hgch_radar_CatNum
#'
#' hgch_radar_CatNum
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ftype:
#' Cat-Num
#' @examples
#'
#' hgch_radar_CatNum(sampleData("Cat-Num",nrow = 10))
#'
#' @export hgch_radar_CatNum
hgch_radar_CatNum <- function(data,
                            title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                            sort = "no", aggregate = "mean", export = FALSE, theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% nms[2]
  d <- f$d
  d <- na.omit(d)
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b,na.rm = TRUE))
  hc <- highchart() %>%
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

  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}







