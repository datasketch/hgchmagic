
#' hgch_multilines_ynp
#' Multilines
#' @name hgch_multilines.
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftypes: Ye-Nu*
#' @examples
#' hgch_multilines_ynp(sampleData("Ye-Nu-Nu",nrow = 10))
hgch_multilines_ynp <- function(data,
                                title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                symbol = NULL, ...){
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  d <- f$d %>% gather(variable,value, -a) %>%
    filter(!is.na(value)) %>% group_by(a,variable) %>%
    summarise(value = mean(value)) %>% ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    mutate(variable = fct_recode_df(d,"variable",codes))
  hchart(d, type = "line", x = a, y = value, group = variable) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
}





#' hgch_bar_cyn
#' @name hgch_bar_cyn
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_bar_cyn(sampleData("Ye-Nu-Nu",nrow = 10))
hgch_bar_cyn <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                         symbol = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"

  d <- f$d %>% na.omit() %>% group_by(a,b) %>% summarise(c = mean(c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hchart(d, type = "column", x = b, y = c, group = a) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
}

#' hgch_line_cyn
#' @name hgch_line_cyn
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_line_cyn(sampleData("Ye-Nu-Nu",nrow = 10))
hgch_line_cyn <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                          symbol = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"

  d <- f$d %>% na.omit() %>% group_by(a,b) %>% summarise(c = mean(c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hchart(d, type = "line", x = b, y = c, group = a) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
}

#' hgch_treemap_cn
#' @name hgch_treemap_cn
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_treemap_cn(sampleData("Ca-Nu",nrow = 10))
hgch_treemap_cn <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                            minColor = "#E63917", maxColor= "#18941E", reverse = TRUE, ...){
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% nms[2]
  d <- f$d %>% na.omit() %>% group_by(a) %>% summarise(b = mean(b))
  hchart(d, "treemap", x = a, value = b, color = b) %>%
    hc_title(text = title) %>%
    hc_colorAxis(maxColor = maxColor, minColor = minColor,reversed = reverse)
}

#' hgch_bar_hor_cn
#' @name hgch_bar_hor_cn
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_bar_hor_top_cn(sampleData("Ca-Nu",nrow = 10))
hgch_bar_hor_cn <- function(data,
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
  d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE))
  if(sort == "top"){
    d <- d %>% arrange(desc(b))
  }
  hchart(d, type = "bar", x = a, y = b) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_bar_hor_top_cn
#' @name hgch_bar_hor_top_cn
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_bar_hor_top_cn(sampleData("Ca-Nu",nrow = 10))
hgch_bar_hor_top_cn <- function(data,
                            title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                            reverse = TRUE, aggregate = "mean", ...){

  hgch_bar_hor_cn(data, title = title, xAxisTitle = xAxisTitle,
                  yAxisTitle = yAxisTitle, sort = "top", aggregate = aggregate)

}


#' hgch_bar_hor_c
#' @name hgch_bar_hor_c
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_bar_hor_c(sampleData("Ca",nrow = 10))
hgch_bar_hor_c <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                           sort = "no", aggregate = "count", ...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% group_by(a) %>% summarise(b = n())
  if(sort == "top"){
   d <- d %>% arrange(desc(b))
  }
  hchart(d, type = "bar", x = a, y = b) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
}

#' hgch_bar_hor_top_c
#' @name hgch_bar_hor_top_c
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_bar_hor_top_cn(sampleData("Ca-Nu",nrow = 10))
hgch_bar_hor_top_c <- function(data,
                                title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                reverse = TRUE, aggregate = "count", ...){

  hgch_bar_hor_c(data, title = title, xAxisTitle = xAxisTitle,
                  yAxisTitle = yAxisTitle, sort = "top", aggregate = aggregate)

}



#' hgch_spider_cn
#' @name hgch_spider_cn
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_spider_cn(sampleData("Ca-Nu",nrow = 10))
hgch_spider_cn <- function(data,
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
  d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE))
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
      ))
}


#' hgch_spider_cnn
#' @name hgch_spider_cnn
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu-Nu
#' @examples
#' hgch_spider_cnn(sampleData("Ca-Nu-Nu",nrow = 10))
hgch_spider_cnn <- function(data,
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
  d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE), c = mean(c, na.rm = TRUE))
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



#' hgch_scatter
#' @name hgch_scatter
#' @export
#' @section ftype: Ca-Nu-Nu
hgch_scatter <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% getClabels(f)[2]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[3]
  title <-  title %||% ""
  d <- f$d %>% filter(!is.na(b),!is.na(c)) %>% group_by(a) %>% summarise(b = mean(b),c = mean(c))
  hchart(d, type = "bubble", x = b, y = c) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle)) %>%
    #     hc_tooltip(pointFormat = "<br><strong>{point.a}</strong><br>x:{point.x} <br>y: {point.y}") %>%
    hc_tooltip(formatter = JS("function() {
                              return '<strong>'+this.point.a+'</strong><br>x: '+ this.point.x +' y: '+ this.point.y +'</b>';
}")) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format= '{point.a}',
                                      style = list(textOutline="1px 1px #000000",
                                                   fontSize = "11px",
                                                   color = "#000")),
                    marker = list(fillColor = "2f0b71rgba(47,11,113,0.6)",lineColor=NULL,lineWidth = 0))
    )
}

#' hgch_scatter
#' @name hgch_scatter
#' @export
#' @section ftype: Ca-Nu-Nu
hgch_scatter <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% getClabels(f)[2]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[3]
  title <-  title %||% ""
  d <- f$d %>% filter(!is.na(b),!is.na(c)) %>% group_by(a) %>% summarise(b = mean(b),c = mean(c))
  hchart(d, type = "bubble", x = b, y = c, color = a) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle)) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format= '{point.a}'))
    )
}

#' hgch_scatter
#' @name hgch_scatter
#' @export
#' @section ftype: Ca-Nu-Nu
hgch_scatter_CaCaNuNu <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% getClabels(f)[3]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[4]
  title <-  title %||% ""
  d <- f$d %>% filter(!is.na(c),!is.na(d)) %>% group_by(a,b) %>%
    summarise(c = mean(c),d = mean(d))
  hchart(d, type = "bubble", x = c, y = d, group = b) %>%
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

#' hgch_scatter
#' @name hgch_scatter
#' @export
#' @section ftype: Ca-Nu-Nu
hgch_scatter_CaCaNuNuNu <- function(f, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL){
  xAxisTitle <- xAxisTitle %||% getClabels(f)[3]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[4]
  title <-  title %||% ""
  d <- f$d %>% filter(!is.na(b),!is.na(c)) %>% group_by(a,b) %>%
    summarise(c = mean(c), d = mean(d),e = mean(e))
  hchart(d, type = "bubble", x = c, y = d, group = b, size = e) %>%
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





