#' Polar bar
#'
#' Polar bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat
#' @examples
#'
#' hgch_polarcolumn_Cat(sampleData("Cat",nrow = 10))
#'
#' @export hgch_polarcolumn_Cat
hgch_polarcolumn_Cat <-function(data, title = ""){

  f <- fringe(data)
  nms <- getCnames(f)
  data <- f$d
  data <- plyr::rename(data, c("a" = "name"))

  data_graph <- data %>%
    dplyr::group_by(name) %>%
    tidyr::drop_na(name) %>%
    dplyr::summarise(value = n())

  data_graph <- data_graph %>%
    dplyr::mutate(y = value,
                  z = ((0:(dim(data_graph)[1]-1))*y) - median((0:(dim(data_graph)[1]-1))*y),
                  color = getPalette()[1:(dim(data_graph)[1])])

  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_chart(type = "column",
             polar = TRUE) %>%
    hc_xAxis(Cattegories = data_graph$name) %>%
    hc_add_series(data_graph, showInLegend = FALSE)
  hc
}


#' Polar bar
#'
#' Polar bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#'
#' hgch_polarcolumn_CatNum(sampleData("Cat-Num",nrow = 10))
#'
#' @export hgch_polarcolumn_CatNum
hgch_polarcolumn_CatNum <-function(data, title = ""){

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
    hc_xAxis(Cattegories = data_graph$name) %>%
    hc_add_series(data_graph, showInLegend = FALSE)
  hc
}



#' Radar
#'
#' Radar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#'
#' hgch_spider_CatNum(sampleData("Cat-Num",nrow = 10))
#'
#' @export hgch_spider_CatNum
hgch_spider_CatNum <- function(data, title = ""){

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
    hc_xAxis(Cattegories = data_graph$name) %>%
    hc_add_series(data_graph, showInLegend = FALSE)
  hc
}



#' Radar
#'
#' Radar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num-Num
#' @examples
#'
#' hgch_spider_CatNumNum(sampleData("Cat-Num-Num",nrow = 10))
#'
#' @export hgch_spider_CatNumNum
hgch_spider_CatNumNum <- function(data,
                               title = NULL, subtitle = NULL, Catption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                               sort = "no", aggregate = "mean", theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% ""
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
             Cattegories = d$a,tickmarkPlacement = 'on',
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


# #' Bubble
# #' @name hgch_bubble_CatNum
# #' @param x A data.frame
# #' @export
# #' @return highcharts viz
# #' @section ctypes: Cat-Num
# #' @examples
# #' hgch_bubble_CatNum(sampleData("Cat-Num",nrow = 10))
# hgch_bubble_CatNum <-function(data, title = ""){
#
#   f <- fringe(data)
#   nms <- getCnames(f)
#   data <- f$d
#   data <- plyr::rename(data, c("a" = "name"))
#
#   data_graph <- data %>%
#     dplyr::group_by(name) %>%
#     tidyr::drop_na(name) %>%
#     dplyr::summarise(value = mean(b, na.rm = TRUE ))
#
#   data_graph <- data_graph %>%
#     dplyr::mutate(y = value,
#                   z = sqrt(y),
#                   color = getPalette()[1:(dim(data_graph)[1])])
#
#   hc <- highchart() %>%
#     hc_title(text = title) %>%
#     hc_chart(type = "bubble",
#              polar = FALSE) %>%
#     hc_xAxis(Cattegories = data_graph$name) %>%
#     hc_add_series(data_graph, showInLegend = FALSE)
#   hc
# }


#' Scatter
#'
#' Scatter
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num-Num
#' @examples
#'
#' hgch_scatter_CatNumNum(sampleData("Cat-Num-Num",nrow = 10))
#'
#' @export hgch_scatter_CatNumNum
hgch_scatter_CatNumNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL,
                                   yAxisTitle = NULL, theme = NULL, export = FALSE,...){

  if(class(data)[1] == "Fringe"){
    ni <- getClabels(data)
  }else{
    ni <- names(data)
  }

  x <- ni[2]
  y <- ni[3]

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% getClabels(f)[2]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[3]
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% drop_na()  %>% dplyr::group_by(a) %>%
    dplyr::summarise(b = mean(b,na.rm = TRUE), c = mean(c, na.rm = TRUE))

  d$text1 <- map_chr(d$b, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))
  d$text2 <- map_chr(d$c, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))

  hc <- hchart(d, type = "bubble", hcaes(x = b, y = c)) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle)) %>%
       hc_tooltip(
         headerFormat= '',
         pointFormat = paste0("<br><strong>{point.a}</strong><br>",
                                       x, ": {point.text1} <br>",
                                       y, ": {point.text2}")) %>%
      hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format= '{point.a}'
                                       #,
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



#' Scatter
#'
#' Scatter
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num-Num-Num
#' @examples
#'
#' hgch_scatter_CatNumNumNum(sampleData("Cat-Num-Num-Num",nrow = 10))
#'
#' @export hgch_scatter_CatNumNumNum
hgch_scatter_CatNumNumNum <- function(data, title = NULL, subtitle = NULL, caption = NULL,
                                      xAxisTitle = NULL, yAxisTitle = NULL,theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% getClabels(f)[2]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[3]
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% dplyr::filter(!is.na(a)) %>% dplyr::group_by(a) %>%
    dplyr::summarise(b = mean(b, na.rm = TRUE), c = mean(c, na.rm = TRUE),d = mean(d, na.rm = TRUE))

  hc <- hchart(d, type = "bubble", hcaes(x = b, y = c, size = d)) %>%
    hc_chart(zoomType = "xy") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
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



#' Scatter grouped
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat-Num-Num
#' @examples
#' hgch_scatter_CatCatNumNum(sampleData("Cat-Cat-Num-Num",nrow = 10))
#' @export hgch_scatter_CatCatNumNum
hgch_scatter_CatCatNumNum <- function(data, title = NULL, subtitle = NULL, caption = NULL,
                                      xAxisTitle = NULL, yAxisTitle = NULL,theme = NULL, export = FALSE,...){

  if(class(data)[1] == "Fringe"){
    ni <- getClabels(data)
  }else{
    ni <- names(data)
  }

  x <- ni[3]
  y <-  ni[4]

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% getClabels(f)[2]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[3]
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::drop_na()%>% dplyr::group_by(a,b) %>%
    dplyr::summarise(c = mean(c, na.rm = TRUE),d = mean(d,na.rm = TRUE))

  d$text1 <- map_chr(d$c, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))
  d$text2 <- map_chr(d$d, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))


  hc <- hchart(d, type = "scatter", hcaes(x = c, y = d, group = b)) %>%
    hc_chart(zoomType = "xy") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle)) %>%
    hc_tooltip(pointFormat = paste0("<br>
               <strong>{point.a}</strong><br>",
                                    x, ":{point.text1} <br>",
                                    y, ": {point.text2}")) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format= '{point.a}'))
    )
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}



#' Scatter
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat-Num-Num-Num
#' @examples
#' hgch_scatter_CatCatNumNumNum(sampleData("Cat-Cat-Num-Num-Num",nrow = 10))
#' @export hgch_scatter_CatCatNumNumNum
hgch_scatter_CatCatNumNumNum <- function(data, title = NULL, subtitle = NULL,
                                         caption = NULL,
                                         xAxisTitle = NULL, yAxisTitle = NULL,
                                         theme = NULL, export = FALSE,...){
  if(class(data)[1] == "Fringe"){
    ni <- getClabels(data)
  }else{
    ni <- names(data)
  }

  x <- ni[3]
  y <- ni[4]
  t <- ni[5]

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% getClabels(f)[3]
  yAxisTitle <- yAxisTitle %||% getClabels(f)[4]
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::drop_na() %>% dplyr::group_by(a,b) %>%
    dplyr::summarise(c = mean(c,na.rm = TRUE), d = mean(d,na.rm = TRUE),e = mean(e,na.rm = TRUE))

  d$text1 <- map_chr(d$c, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))
  d$text2 <- map_chr(d$d, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))
  d$text3 <- map_chr(d$e, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))

  hc <- hchart(d, type = "bubble", hcaes(x = c, y = d, group = b, size = e)) %>%
    hc_chart(zoomType = "xy") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle)) %>%
    hc_tooltip(
      pointFormat = paste0("<br>
               <strong>{point.a}</strong><br>",
                           x, ": {point.text1} <br>",
                           y, ": {point.text2} <br>",
                           t, ": {point.text3}")
    ) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE,format= '{point.a}'))
    )
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}
