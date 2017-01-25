#' hgch_polarcolumn_CaNu
#' @name hgch_polarcolumn_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_polarcolumn_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_polarcolumn_CaNu <-function(data, title = ""){

  f <- fringe(data)
  nms <- getCnames(f)
  data <- f$d
  data <- plyr::rename(data, c("a" = "name"))

  data_graph <- data %>%
    dplyr::group_by(name) %>%
    tidyr::drop_na(name) %>%
    dplyr::summarise(value = mean(b, na.rm = TRUE ))

  data_graph <- data_graph %>%
    dplyr::mutate(y = value,
                  z = ((0:(dim(data_graph)[1]-1))*y) - median((0:(dim(data_graph)[1]-1))*y),
                  color = getPalette()[1:(dim(data_graph)[1])])

  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_chart(type = "column",
             polar = TRUE) %>%
    hc_xAxis(categories = data_graph$name) %>%
    hc_add_series(data_graph, showInLegend = FALSE)
  hc
}

#' hgch_spider_CaNu
#' @name hgch_spider_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_spider_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_spider_CaNu <-function(data, title = ""){

  f <- fringe(data)
  nms <- getCnames(f)
  data <- f$d
  data <- plyr::rename(data, c("a" = "name"))

  data_graph <- data %>%
    dplyr::group_by(name) %>%
    tidyr::drop_na(name) %>%
    dplyr::summarise(value = mean(b, na.rm = TRUE ))

  data_graph <- data_graph %>%
    dplyr::mutate(y = value,
                  z = ((0:(dim(data_graph)[1]-1))*y) - median((0:(dim(data_graph)[1]-1))*y),
                  color = getPalette()[1:(dim(data_graph)[1])])

  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_chart(type = "line",
             polar = TRUE) %>%
    hc_xAxis(categories = data_graph$name) %>%
    hc_add_series(data_graph, showInLegend = FALSE)
  hc
}

#' hgch_spider_CaNuNu
#' @name hgch_spider_CaNuNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu-Nu
#' @examples
#' hgch_spider_CaNuNu(sampleData("Ca-Nu-Nu",nrow = 10))
hgch_spider_CaNuNu <- function(data,
                               title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                               sort = "no", aggregate = "mean", theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% nms[2]
  d <- f$d
  d <- na.omit(d)
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>%
    dplyr::summarise(b = mean(b,na.rm = TRUE), c = mean(c, na.rm = TRUE))
  hc <- highchart() %>%
    hc_chart(type = "line", polar = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text=xAxisTitle),
             categories = d$a,tickmarkPlacement = 'on',
             lineWidth = 0) %>%
    hc_series(
      list(
        name = nms[2],
        data = d$b,
        pointPlacement = 'on'
      ),
      list(
        name = nms[3],
        data = d$c,
        pointPlacement = 'on'
      ))
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}



#' hgch_bubble_CaNu
#' @name hgch_bubble_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_bubble_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_bubble_CaNu <-function(data, title = ""){

  f <- fringe(data)
  nms <- getCnames(f)
  data <- f$d
  data <- plyr::rename(data, c("a" = "name"))

  data_graph <- data %>%
    dplyr::group_by(name) %>%
    tidyr::drop_na(name) %>%
    dplyr::summarise(value = mean(b, na.rm = TRUE ))

  data_graph <- data_graph %>%
    dplyr::mutate(y = value,
                  z = sqrt(y),
                  color = getPalette()[1:(dim(data_graph)[1])])

  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_chart(type = "bubble",
             polar = FALSE) %>%
    hc_xAxis(categories = data_graph$name) %>%
    hc_add_series(data_graph, showInLegend = FALSE)
  hc
}

#' hgch_scatter_CaNuNu
#' @name hgch_scatter_CaNuNu
#' @export
#' @section ftype: Ca-Nu-Nu
hgch_scatter_CaNuNu <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% getClabels(f)[2]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[3]
  title <-  title %||% ""

  d <- f$d %>% dplyr::filter(!is.na(a)) %>% dplyr::group_by(a) %>%
    dplyr::summarise(b = mean(b,na.rm = TRUE),c = mean(c, na.rm = TRUE))

  hc <- hchart(d, type = "bubble", hcaes(x = b, y = c)) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle)) %>%
    #     hc_tooltip(pointFormat = "<br><strong>{point.a}</strong><br>x:{point.x} <br>y: {point.y}") %>%
    hc_tooltip(formatter = JS("function() {
                              return '<strong>'+this.point.a+'</strong><br>x: '+ this.point.x +' y: '+ this.point.y +'</b>';
}")) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format= '{point.a}'
                                      # ,
                                      # style = list(textOutline="1px 1px #000000",
                                      #              fontSize = "11px",
                                      #              color = "#000")
      ),
      marker = list(fillColor = "rgba(47,11,113,0.6)",lineColor=NULL,lineWidth = 0))
    )
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}
#' hgch_scatter_CaNuNuNu
#' @name hgch_scatter_CaNuNuNu
#' @export
#' @section ftype: Ca-Nu-Nu-Nu
hgch_scatter_CaNuNuNu <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% getClabels(f)[2]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[3]
  title <-  title %||% ""

  d <- f$d %>% dplyr::filter(!is.na(a)) %>% dplyr::group_by(a) %>%
    dplyr::summarise(b = mean(b, na.rm = TRUE), c = mean(c, na.rm = TRUE),d = mean(d, na.rm = TRUE))

  hc <- hchart(d, type = "bubble", hcaes(x = b, y = c, size = d)) %>%
    hc_chart(zoomType = "xy") %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle)) %>%
    hc_tooltip(pointFormat = "<br>
               <strong>{point.a}</strong><br>
               x:{point.x} <br>
               y: {point.y} <br>
               Tamaño: {point.size}") %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format= '{point.a}'))
    )
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}
#' hgch_scatter_CaCaNuNu
#' @name hgch_scatter_CaCaNuNu
#' @export
#' @section ftype: Ca-Nu-Nu
hgch_scatter_CaCaNuNu <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% getClabels(f)[3]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[4]
  title <-  title %||% ""
  d <- f$d %>% dplyr::filter(!is.na(a),!is.na(b)) %>% dplyr::group_by(a,b) %>%
    dplyr::summarise(c = mean(c, na.rm = TRUE),d = mean(d,na.rm = TRUE))


  hc <- hchart(d, type = "scatter", hcaes(x = c, y = d, group = b)) %>%
    hc_chart(zoomType = "xy") %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle)) %>%
    hc_tooltip(pointFormat = "<br>
               <strong>{point.a}</strong><br>
               x:{point.x} <br>
               y: {point.y}") %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format= '{point.a}'))
    )
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}

#' hgch_scatter_CaCaNuNuNu
#' @name hgch_scatter_CaCaNuNuNu
#' @export
#' @section ftype: Ca-Nu-Nu
hgch_scatter_CaCaNuNuNu <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% getClabels(f)[3]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[4]
  title <-  title %||% ""
  d <- f$d %>% dplyr::filter(!is.na(a),!is.na(b)) %>% dplyr::group_by(a,b) %>%
    dplyr::summarise(c = mean(c,na.rm = TRUE), d = mean(d,na.rm = TRUE),e = mean(e,na.rm = TRUE))
  hc <- hchart(d, type = "bubble", hcaes(x = c, y = d, group = b, size = e)) %>%
    hc_chart(zoomType = "xy") %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle)) %>%
    hc_tooltip(pointFormat = "<br>
               <strong>{point.a}</strong><br>
               x:{point.x} <br>
               y: {point.y} <br>
               Tamaño: {point.size}") %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format= '{point.a}'))
    )
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}
