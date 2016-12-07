
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


#' hgch_line_DaNu
#' @name hgch_line_DaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Da-Nu
#' @examples
#' hgch_line_DaNu(sampleData("Ca-Da-Nu",nrow = 10))
hgch_line_DaNu <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                           symbol = NULL, startAtZero = FALSE, ...){

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
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
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
#' @name hgch_line_CaDaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_line_CaYeNu(sampleData("Ye-Nu-Nu",nrow = 10))
hgch_line_CaYeNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                             symbol = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = mean(c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hchart(d, type = "line", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
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
hgch_line_CaDaNu <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                             symbol = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = mean(c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hchart(d, type = "line", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = FALSE, symbol =  symbol))
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
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
                                title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
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
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc
}





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


#' hgch_spider_CaNuNu
#' @name hgch_spider_CaNuNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu-Nu
#' @examples
#' hgch_spider_CaNuNu(sampleData("Ca-Nu-Nu",nrow = 10))
hgch_spider_CaNuNu <- function(data,
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
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b,na.rm = TRUE), c = mean(c, na.rm = TRUE))
  highchart() %>%
    hc_chart(type = "line", polar = TRUE) %>%
    hc_title(text = title) %>%
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
}



#' hgch_scatter_CaNuNu
#' @name hgch_scatter_CaNuNu
#' @export
#' @section ftype: Ca-Nu-Nu
hgch_scatter_CaNuNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% getClabels(f)[2]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[3]
  title <-  title %||% ""

  d <- f$d %>% dplyr::filter(!is.na(b),!is.na(c)) %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b),c = mean(c))
  hchart(d, type = "bubble", hcaes(x = b, y = c)) %>%
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
}

#' hgch_scatter_CaNuNuNu
#' @name hgch_scatter_CaNuNuNu
#' @export
#' @section ftype: Ca-Nu-Nu-Nu
hgch_scatter_CaNuNuNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% getClabels(f)[2]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[3]
  title <-  title %||% ""

  d <- f$d %>% dplyr::filter(!is.na(b),!is.na(c)) %>% dplyr::group_by(a) %>%
    dplyr::summarise(b = mean(b), c = mean(c),d = mean(d))

  hchart(d, type = "bubble", hcaes(x = b, y = c, size = d)) %>%
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
}

#' hgch_scatter_CaCaNuNu
#' @name hgch_scatter_CaCaNuNu
#' @export
#' @section ftype: Ca-Nu-Nu
hgch_scatter_CaCaNuNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% getClabels(f)[3]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[4]
  title <-  title %||% ""
  d <- f$d %>% dplyr::filter(!is.na(c),!is.na(d)) %>% dplyr::group_by(a,b) %>%
    dplyr::summarise(c = mean(c),d = mean(d))
  hchart(d, type = "bubble", hcaes(x = c, y = d, group = b)) %>%
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
}

#' hgch_scatter_CaCaNuNuNu
#' @name hgch_scatter_CaCaNuNuNu
#' @export
#' @section ftype: Ca-Nu-Nu
hgch_scatter_CaCaNuNuNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% getClabels(f)[3]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[4]
  title <-  title %||% ""
  d <- f$d %>% dplyr::filter(!is.na(b),!is.na(c)) %>% dplyr::group_by(a,b) %>%
    dplyr::summarise(c = mean(c), d = mean(d),e = mean(e))
  hchart(d, type = "bubble", hcaes(x = c, y = d, group = b, size = e)) %>%
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
}





