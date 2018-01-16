#'@name count_pl
#' @export
count_pl <- function(x) {
  if(is.na(x)){return(0)}
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

#' Vertical bar (category)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat
#' @examples
#' hgch_bar_ver_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_bar_ver_Cat
hgch_bar_ver_Cat <- function(data,
                             title = NULL,
                             subtitle = NULL,
                             caption = NULL,
                             horLabel = NULL,
                             verLabel = NULL,
                             yLine = NULL,
                             yLineLabel = NULL,
                             agg = "sum",
                             sort = "no",
                             topn = NULL,
                             theme = NULL,
                             export = FALSE,...) {

  f <- fringe(data)
  nms <- getClabels(f)

  horLabel <- horLabel %||% nms[1]
  verLabel <- verLabel %||% nms[1]
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d %>%
    tidyr::replace_na(list(a = ifelse(is.character(f$d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  if (sort == "desc") {
    d <- d %>%
      dplyr::arrange(desc(b))
    if (!is.null(topn)) {
      d <- d %>%
        dplyr::slice(1:topn)
    }
  }
  if (sort == "asc") {
    d <- d %>%
      dplyr::arrange(b)
  }
  if (nrow(d) == 0) return()
  hc <- hchart(d, type = "column", hcaes(x = a, y = b)) %>%
    hc_tooltip(headerFormat = paste("<b>", paste0(horLabel, ": "), "</b>{point.key}<br/>"),
               pointFormat = paste0("<b>", verLabel, "</b>: {point.b}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = horLabel), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = paste(agg, verLabel)), plotLines = list(list(value = yLine,
                                                                              color = 'black',
                                                                              dashStyle = 'shortdash',
                                                                              width = 2,
                                                                              label = list(text = yLineLabel)))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}



#' Ordered vertical bar
#'
#' Ordered vertical bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat
#' @examples
#' hgch_bar_top_ver_Cat(sampleData("Cat", nrow = 10))
#'
#' @export hgch_bar_top_ver_Cat
hgch_bar_top_ver_Cat <- function(data,
                                 topn = NULL,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 xAxisTitle = NULL,
                                 yAxisTitle = NULL,
                                 reverse = TRUE,
                                 theme = NULL,
                                 export = FALSE,
                                 ...) {
  hgch_bar_ver_Cat(
    data,
    topn = topn,
    title = title,
    subtitle = subtitle,
    caption = caption,
    xAxisTitle = xAxisTitle,
    yAxisTitle = yAxisTitle,
    sort = "top",
    theme = theme,
    export = export
  )

}


#' Horizontal bar (category)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat
#' @examples
#' hgch_bar_hor_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_bar_hor_Cat
hgch_bar_hor_Cat <- function(data,
                             title = NULL,
                             subtitle = NULL,
                             caption = NULL,
                             horLabel = NULL,
                             verLabel = NULL,
                             yLine = NULL,
                             yLineLabel = NULL,
                             agg = "sum",
                             dropNa = FALSE,
                             order = NULL,
                             sort = "no",
                             sliceN = NULL,
                             theme = NULL,
                             export = FALSE,...) {

  f <- fringe(data)
  nms <- getClabels(f)

  horLabel <- horLabel %||% nms[1]
  verLabel <- verLabel %||% nms[1]
  yLineLabel <- yLineLabel %||% yLine
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d %>%
    tidyr::replace_na(list(a = ifelse(is.character(f$d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  if (sort == "desc") {
    d <- d %>%
      dplyr::arrange(desc(b))
    if (!is.null(topn)) {
      d <- d %>%
        dplyr::slice(1:topn)
    }
  }
  if (sort == "asc") {
    d <- d %>%
      dplyr::arrange(b)
  }
  if (nrow(d) == 0) return()
  hc <- hchart(d, type = "bar", hcaes(x = a, y = b)) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_tooltip(headerFormat = paste("<b>", paste0(horLabel, ": "), "</b>{point.key}<br/>"),
               pointFormat = paste0("<b>", verLabel, "</b>: {point.b}")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = verLabel), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = paste(agg, horLabel)), plotLines = list(list(value = yLine,
                                                                              color = 'black',
                                                                              dashStyle = 'shortdash',
                                                                              width = 2,
                                                                              label = list(text = yLineLabel)))) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)

  # hc_xAxis(title = list(text = yAxisTitle)) %>%
  # hc_yAxis(title = list(text = xAxisTitle))%>%

  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
    hc
}


#' Ordered horizontal bar
#'
#' Ordered horizontal bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat
#' @examples
#' hgch_bar_top_hor_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_bar_top_hor_Cat
hgch_bar_top_hor_Cat <- function(data,
                                topn = NULL,
                                title = NULL,
                                subtitle = NULL,
                                caption = NULL,
                                xAxisTitle = NULL,
                                yAxisTitle = NULL,
                                reverse = TRUE,
                                theme = NULL,
                                export = FALSE,
                                ...) {
  hgch_bar_hor_Cat(
    data,
    topn = topn,
    title = title,
    subtitle = subtitle,
    caption = caption,
    xAxisTitle = xAxisTitle,
    yAxisTitle = yAxisTitle,
    sort = "top",
    theme = theme,
    export = export
  )
}



#' Vertical bar
#'
#' Vertical bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#' hgch_bar_ver_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_bar_ver_CatNum
hgch_bar_ver_CatNum <-
  function(data,
           topn = NULL,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           sort = "no",
           aggregation = "sum",
           #back_color = 'white',
           color_title = 'black',
           startAtZero = FALSE,
           theme = NULL,
           export = FALSE,
           ...) {
    if(class(data)[1] == "Fringe"){
      ni <- getClabels(data)
    }else{
      ni <- names(data)
    }

    y <- ni[2]

    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% nms[1]
    yAxisTitle <- yAxisTitle %||% nms[2]
    title <-  title %||% ""
    caption <- caption %||% ""
    subtitle <- subtitle %||% ""

    d <- f$d %>%
      tidyr::drop_na() %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = agg(aggregation, b))

    #d$w <- map_chr(d$b, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))


    if (nrow(d) == 0)
      return()
    if (sort == "top") {
      d <- d %>% dplyr::arrange(desc(b))
      if (!is.null(topn)) {
        d <- dplyr::slice(d, 1:topn)
      } else {
        d <- d
      }
    }
    hc <- hchart(d, type = "column", hcaes(x = as.character(a), y = b)) %>%
      # hc_plotOptions(
      #   series = list(marker = list(enabled = TRUE, symbol =  symbol))
      # ) %>%
      #hc_chart(backgroundColor = back_color) %>%
      hc_title(text = title,style = list(color = color_title, useHTML = TRUE)) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = yAxisTitle)) %>%
      hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.key}</b><br/>"),
                 pointFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.b}</b><br/>"))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
      hc_credits(enabled = TRUE, text = caption)
    if (export)
      hc <- hc %>% hc_exporting(enabled = export)
    hc

  }


#' Ordered vertical bar
#'
#' Ordered vertical bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#' hgch_bar_top_ver_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_bar_top_ver_CatNum
hgch_bar_top_ver_CatNum <- function(data,
                                  topn = NULL,
                                  title = NULL,
                                  subtitle = NULL,
                                  caption = NULL,
                                  xAxisTitle = NULL,
                                  yAxisTitle = NULL,
                                  reverse = TRUE,
                                  aggregation = "sum",
                                  theme = NULL,
                                  export = FALSE,
                                  ...) {
  hgch_bar_ver_CatNum(
    data,
    topn = topn,
    title = title,
    subtitle = subtitle,
    caption = caption,
    xAxisTitle = xAxisTitle,
    yAxisTitle = yAxisTitle,
    sort = "top",
    aggregation = aggregation,
    theme = theme,
    export = export
  )

}



#' Vertical bar
#'
#' Vertical bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Yea-Num
#' @examples
#' hgch_bar_ver_YeaNum(sampleData("Yea-Num", nrow = 10))
#' @export hgch_bar_ver_YeaNum
hgch_bar_ver_YeaNum <- hgch_bar_ver_CatNum




#' Vertical bar
#'
#' Vertical bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Dat-Num
#' @examples
#' hgch_bar_ver_DatNum(sampleData("Dat-Num", nrow = 10))
#' @export hgch_bar_ver_DatNum
hgch_bar_ver_DatNum <- hgch_bar_ver_CatNum




#' Horizontal bar
#'
#' Horizontal bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#' hgch_bar_hor_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_bar_hor_CatNum
hgch_bar_hor_CatNum <- function(data,
                                topn = NULL,
                                title = NULL,
                                subtitle = NULL,
                                caption = NULL,
                                xAxisTitle = NULL,
                                yAxisTitle = NULL,
                                sort = "no",
                                aggregation = "sum",
                                #back_color = 'white',
                                color_title = 'black',
                                theme = NULL,
                                export = FALSE,
                                ...) {

  if(class(data)[1] == "Fringe"){
    ni <- getClabels(data)
  }else{
    ni <- names(data)
  }

  y <- ni[2]

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[1]
  title <-  title %||% " "
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d
  d <- na.omit(d)
  if (nrow(d) == 0)
    return()
  d <-  d %>%
        dplyr::group_by(a) %>%
        dplyr::summarise(b = agg(aggregation, b)) %>%
        drop_na()

  #d$w <- map_chr(d$b, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))

  if (sort == "top") {
    d <- d %>% dplyr::arrange(desc(b))
    if (!is.null(topn)) {
      d <- dplyr::slice(d, 1:topn)
    } else {
      d <- d
    }
  }
  hc <- hchart(d, type = "bar", hcaes(x = as.character(a), y = b)) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    #hc_chart(backgroundColor = back_color) %>%
    hc_title(text = title,style = list(color = color_title, useHTML = TRUE)) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = yAxisTitle)) %>%
    hc_yAxis(title = list(text = xAxisTitle))  %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.key}</b><br/>"),
               pointFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.b}</b><br/>"))
    # hc_tooltip(
    #   headerFormat= '',
    #   pointFormat=paste0('<b>{point.a}</b>:<br>',
    #                      y, ': {point.w}'))
    #hc_tooltip( pointFormat=paste0(
    #  y,': {point.b}'))
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Ordered horizontal bar
#'
#' Ordered horizontal bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#' hgch_bar_top_hor_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_bar_top_hor_CatNum
hgch_bar_top_hor_CatNum <- function(data,
                                    topn = NULL,
                                    title = NULL,
                                    subtitle = NULL,
                                    caption = NULL,
                                    xAxisTitle = NULL,
                                    yAxisTitle = NULL,
                                    reverse = TRUE,
                                    aggregation = "sum",
                                    theme = NULL,
                                    export = FALSE,
                                    ...) {
  hgch_bar_hor_CatNum(data,
                      topn = topn,
                      title = title,
                      subtitle = subtitle,
                      caption = caption,
                      xAxisTitle = xAxisTitle,
                      yAxisTitle = yAxisTitle,
                      sort = "top",
                      aggregation = aggregation,
                      theme = theme,
                      export = export
  )

}

#' Horizontal bar
#'
#' Horizontal bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Yea-Num
#' @examples
#' hgch_bar_hor_YeaNum(sampleData("Yea-Num", nrow = 10))
#' @export hgch_bar_hor_YeaNum
hgch_bar_hor_YeaNum <- hgch_bar_hor_CatNum




#' Horizontal bar
#'
#' Horizontal bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Dat-Num
#' @examples
#' hgch_bar_hor_DatNum(sampleData("Dat-Num", nrow = 10))
#' @export hgch_bar_hor_DatNum
hgch_bar_hor_DatNum <- hgch_bar_hor_CatNum


#' Vertical grouped bar
#'
#' Vertical grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bar_grouped_ver_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_bar_grouped_ver_CatCatNum
hgch_bar_grouped_ver_CatCatNum <-
  function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           symbol = NULL,
           aggregation = "sum",
           startAtZero = FALSE,
           theme = NULL,
           export = FALSE,
           ...) {
    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% nms[2]
    yAxisTitle <- yAxisTitle %||% nms[3]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"
    caption <- caption %||% ""
    subtitle <- subtitle %||% ""

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = agg(aggregation, c))
    # para que las fechas sean categóricas
    d$b <- as.character(d$b)
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <-
      hchart(d, type = "column", hcaes(x = as.character(b), y = c, group = a)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = yAxisTitle)) %>%
      hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.key}</b><br/>"),
                 pointFormat = paste("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "{point.a}</b><br/><b style = 'font-size:12px'>",
                                     paste0(yAxisTitle, ": "), "{point.c}</b><br/>"))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
      hc_credits(enabled = TRUE, text = caption)
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }


#' Vertical grouped bar
#'
#' Vertical grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_bar_grouped_ver_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_bar_grouped_ver_CatYeaNum
hgch_bar_grouped_ver_CatYeaNum <- hgch_bar_grouped_ver_CatCatNum



#' Vertical grouped bar
#'
#' Vertical grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_bar_grouped_ver_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_bar_grouped_ver_CatDatNum
hgch_bar_grouped_ver_CatDatNum <- hgch_bar_grouped_ver_CatCatNum




#' Horizontal grouped bar
#'
#' Horizontal grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bar_grouped_hor_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_bar_grouped_hor_CatCatNum
hgch_bar_grouped_hor_CatCatNum <-
  function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           symbol = NULL,
           aggregation = "sum",
           startAtZero = FALSE,
           theme = NULL,
           export = FALSE,
           ...) {
    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% nms[3]
    yAxisTitle <- yAxisTitle %||% nms[2]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"
    caption <- caption %||% ""
    subtitle <- subtitle %||% ""

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = agg(aggregation, c))
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <- hchart(d, type = "bar", hcaes(x = as.character(b), y = c, group = a)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = yAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = xAxisTitle)) %>%
      hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.key}</b><br/>"),
                 pointFormat = paste("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "{point.a}</b><br/><b style = 'font-size:12px'>",
                                     paste0(yAxisTitle, ": "), "{point.c}</b><br/>"))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
      hc_credits(enabled = TRUE, text = caption)
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }



#' Horizontal grouped bar
#'
#' Horizontal grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_bar_grouped_hor_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_bar_grouped_hor_CatYeaNum
hgch_bar_grouped_hor_CatYeaNum <- hgch_bar_grouped_hor_CatCatNum



#' Horizontal grouped bar
#'
#' Horizontal grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_bar_grouped_hor_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_bar_grouped_hor_CatDatNum
hgch_bar_grouped_hor_CatDatNum <- hgch_bar_grouped_hor_CatCatNum



#' Vertical stacked bar
#'
#' Vertical stacked bar
#'
#' @name hgch_bar_stacked_ver_CatCatNum
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bar_stacked_ver_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
hgch_bar_stacked_ver_CatCatNum <-
  function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           symbol = NULL,
           aggregation = "sum",
           startAtZero = FALSE,
           theme = NULL,
           export = FALSE,
           ...) {
    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% nms[2]
    yAxisTitle <- yAxisTitle %||% nms[3]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"
    caption <- caption %||% ""

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = agg(aggregation, c))
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <-
      hchart(d, type = "column", hcaes(x = as.character(b), y = c, group = a)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol)),
                     column = list(stacking = "normal")) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = yAxisTitle)) %>%
      hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.key}</b><br/>"),
                 pointFormat = paste("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "{point.a}</b><br/><b style = 'font-size:12px'>",
                                     paste0(yAxisTitle, ": "), "{point.c}</b><br/>"))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
      hc_credits(enabled = TRUE, text = caption)
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }


#' Vertical 100 stacked bar
#'
#' Vertical 100 stacked bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bar_stacked_100_ver_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_bar_stacked_100_ver_CatCatNum
hgch_bar_stacked_100_ver_CatCatNum <-
  function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           symbol = NULL,
           aggregation ="sum",
           startAtZero = FALSE,
           theme = NULL,
           export = FALSE,
           ...) {
    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% nms[2]
    yAxisTitle <- yAxisTitle %||% nms[3]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"
    caption <- caption %||% ""
    subtitle <- subtitle %||% ""

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = agg(aggregation, c))
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <-
      hchart(d, type = "column", hcaes(x = as.character(b), y = c, group = a)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol)),
                     column = list(stacking = "percent")) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = yAxisTitle)) %>%
      hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.key}</b><br/>"),
                 pointFormat = paste("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "{point.a}</b><br/><b style = 'font-size:12px'>",
                                     paste0(yAxisTitle, ": "), "{point.c}</b><br/>"))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
      hc_credits(enabled = TRUE, text = caption)
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }



#' Horizontal stacked bar
#'
#' Horizontal stacked bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bar_stacked_hor_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_bar_stacked_hor_CatCatNum
hgch_bar_stacked_hor_CatCatNum <-
  function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           symbol = NULL,
           aggregation = "sum",
           startAtZero = FALSE,
           theme = NULL,
           export = FALSE,
           ...) {
    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% nms[3]
    yAxisTitle <- yAxisTitle %||% nms[2]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"
    caption <- caption %||% ""
    subtitle <- subtitle %||% ""

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = agg(aggregation, c))
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <- hchart(d, type = "bar", hcaes(x = as.character(b), y = c, group = a)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol)),
                     bar = list(stacking = "normal")) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = yAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = xAxisTitle)) %>%
      hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.key}</b><br/>"),
                 pointFormat = paste("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "{point.a}</b><br/><b style = 'font-size:12px'>",
                                     paste0(xAxisTitle, ": "), "{point.c}</b><br/>"))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = xAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
      hc_credits(enabled = TRUE, text = caption)
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }


#' Horizontal 100 stacked bar
#'
#' Horizontal 100 stacked bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bar_stacked_100_hor_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_bar_stacked_100_hor_CatCatNum
hgch_bar_stacked_100_hor_CatCatNum <-
  function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           symbol = NULL,
           aggregation = "sum",
           startAtZero = FALSE,
           theme = NULL,
           export = FALSE,
           ...) {
    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% nms[3]
    yAxisTitle <- yAxisTitle %||% nms[2]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"
    caption <- caption %||% ""
    subtitle <- subtitle %||% ""

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = agg(aggregation, c))
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <- hchart(d, type = "bar", hcaes(x = as.character(b), y = c, group = a)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol)),
                     bar = list(stacking = "percent")) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = yAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = xAxisTitle)) %>%
      hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.key}</b><br/>"),
                 pointFormat = paste("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "{point.a}</b><br/><b style = 'font-size:12px'>",
                                     paste0(xAxisTitle, ": "), "{point.c}</b><br/>"))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = xAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
      hc_credits(enabled = TRUE, text = caption)
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }


#' Vertical stacked bar
#'
#' Vertical stacked bar
#'
#' @export hgch_bar_stacked_ver_CatYeaNum
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_bar_stacked_ver_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
hgch_bar_stacked_ver_CatYeaNum <- hgch_bar_stacked_ver_CatCatNum


#' Vertical stacked bar
#'
#' Vertical stacked bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_bar_stacked_ver_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_bar_stacked_ver_CatDatNum
hgch_bar_stacked_ver_CatDatNum <- hgch_bar_stacked_ver_CatCatNum


#' Horizontal stacked bar
#'
#' Horizontal stacked bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_bar_stacked_hor_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_bar_stacked_hor_CatYeaNum
hgch_bar_stacked_hor_CatYeaNum <- hgch_bar_stacked_hor_CatCatNum



#' Horizontal stacked bar
#'
#' Horizontal stacked bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_bar_stacked_hor_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_bar_stacked_hor_CatDatNum
hgch_bar_stacked_hor_CatDatNum <- hgch_bar_stacked_hor_CatCatNum


#' Vertical 100 stacked bar
#'
#' Vertical 100 stacked bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_bar_stacked_100_ver_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_bar_stacked_100_ver_CatDatNum
hgch_bar_stacked_100_ver_CatDatNum <- hgch_bar_stacked_100_ver_CatCatNum


#' Vertical 100 stacked bar
#'
#' Vertical 100 stacked bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_bar_stacked_100_ver_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_bar_stacked_100_ver_CatYeaNum
hgch_bar_stacked_100_ver_CatYeaNum <- hgch_bar_stacked_100_ver_CatCatNum


#' Horizontal 100 stacked bar
#'
#' Horizontal 100 stacked bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_bar_stacked_100_hor_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_bar_stacked_100_hor_CatDatNum
hgch_bar_stacked_100_hor_CatDatNum <- hgch_bar_stacked_100_hor_CatCatNum


#' Horizontal 100 stacked bar
#'
#' Horizontal 100 stacked bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_bar_stacked_100_hor_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_bar_stacked_100_hor_CatYeaNum
hgch_bar_stacked_100_hor_CatYeaNum <- hgch_bar_stacked_100_hor_CatCatNum



#' Vertical grouped bar
#'
#' Vertical grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#' hgch_bar_grouped_ver_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_bar_grouped_ver_CatNum
hgch_bar_grouped_ver_CatNum <- function(data,
                                        title = NULL,
                                        subtitle = NULL,
                                        caption = NULL,
                                        xAxisTitle = NULL,
                                        yAxisTitle = NULL,
                                        symbol = NULL,
                                        aggregation = "sum",
                                        startAtZero = FALSE,
                                        theme = NULL,
                                        export = FALSE,
                                        ...) {
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::gather(variable, value,-a) %>% dplyr::filter(!is.na(a)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(a, variable) %>%
    dplyr::summarise(value = agg(aggregation, value)) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)

  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  # d$text1 <- map_chr(d$value, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))

  hc <-
    hchart(d, type = "column", hcaes(x = a, y = value, group = variable)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle),
             allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = yAxisTitle)) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.key}</b><br/>"),
               pointFormat = paste("<b style = 'font-size:12px'>", "{point.variable}: {point.value}</b><br/>"))
    # hc_tooltip(
    #   headerFormat= '',
    #   pointFormat ="<b>{point.a}</b> <br>
    #                 {point.variable}: {point.text1}"
    # )
  if (startAtZero) {
    hc <-
      hc %>% hc_yAxis(
        title = list(text = yAxisTitle),
        minRange = 0.1,
        min = 0,
        minPadding = 0
      )
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Vertical grouped bar
#'
#' Vertical grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num-Num
#' @examples
#' hgch_bar_grouped_ver_CatNumNum(sampleData("Cat-Num-Num", nrow = 10))
#' @export hgch_bar_grouped_ver_CatNumNum
hgch_bar_grouped_ver_CatNumNum <- function(data,
                                         title = NULL,
                                         subtitle = NULL,
                                         caption = NULL,
                                         xAxisTitle = NULL,
                                         yAxisTitle = NULL,
                                         symbol = NULL,
                                         aggregation = "sum",
                                         startAtZero = FALSE,
                                         theme = NULL,
                                         export = FALSE,
                                         ...) {
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% ""
  symbol <- symbol %||% "circle"
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::gather(variable, value,-a) %>% dplyr::filter(!is.na(a)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(a, variable) %>%
    dplyr::summarise(value = agg(aggregation, value)) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)

  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  # d$text1 <- map_chr(d$value, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))

  hc <-
    hchart(d, type = "column", hcaes(x = a, y = value, group = variable)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle),
             allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = yAxisTitle)) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.key}</b><br/>"),
               pointFormat = paste("<b style = 'font-size:12px'>", "{point.variable}: {point.value}</b><br/>"))
  # hc_tooltip(
  #   headerFormat= '',
  #   pointFormat ="<b>{point.a}</b> <br>
  #                 {point.variable}: {point.text1}"
  # )
  if (startAtZero) {
    hc <-
      hc %>% hc_yAxis(
        title = list(text = yAxisTitle),
        minRange = 0.1,
        min = 0,
        minPadding = 0
      )
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Vertical grouped bar
#'
#' Vertical grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num-Num-Num
#' @examples
#' hgch_bar_grouped_ver_CatNumNumNum(sampleData("Cat-Num-Num-Num", nrow = 10))
#' @export hgch_bar_grouped_ver_CatNumNumNum
hgch_bar_grouped_ver_CatNumNumNum <- function(data,
                                           title = NULL,
                                           subtitle = NULL,
                                           caption = NULL,
                                           xAxisTitle = NULL,
                                           yAxisTitle = NULL,
                                           symbol = NULL,
                                           aggregation = "sum",
                                           startAtZero = FALSE,
                                           theme = NULL,
                                           export = FALSE,
                                           ...) {
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% ""
  symbol <- symbol %||% "circle"
  caption <- caption %||% ""

  d <- f$d %>% tidyr::gather(variable, value,-a) %>% dplyr::filter(!is.na(a)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(a, variable) %>%
    dplyr::summarise(value = agg(aggregation, value)) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)

  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  # d$text1 <- map_chr(d$value, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))

  hc <-
    hchart(d, type = "column", hcaes(x = a, y = value, group = variable)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle),
             allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = yAxisTitle)) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.key}</b><br/>"),
               pointFormat = paste("<b style = 'font-size:12px'>", "{point.variable}: {point.value}</b><br/>"))
  # hc_tooltip(
  #   headerFormat= '',
  #   pointFormat ="<b>{point.a}</b> <br>
  #                 {point.variable}: {point.text1}"
  # )
  if (startAtZero) {
    hc <-
      hc %>% hc_yAxis(
        title = list(text = yAxisTitle),
        minRange = 0.1,
        min = 0,
        minPadding = 0
      )
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}

#' Vertical grouped bar
#'
#' Vertical grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num-Num-Num-Num
#' @examples
#' hgch_bar_grouped_ver_CatNumNumNumNum(sampleData("Cat-Num-Num-Num-Num", nrow = 10))
#' @export hgch_bar_grouped_ver_CatNumNumNumNum
hgch_bar_grouped_ver_CatNumNumNumNum <- function(data,
                                                 title = NULL,
                                                 subtitle = NULL,
                                                 caption = NULL,
                                                 xAxisTitle = NULL,
                                                 yAxisTitle = NULL,
                                                 symbol = NULL,
                                                 aggregation = "sum",
                                                 startAtZero = FALSE,
                                                 theme = NULL,
                                                 export = FALSE,
                                                 ...) {
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% ""
  symbol <- symbol %||% "circle"
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::gather(variable, value,-a) %>% dplyr::filter(!is.na(a)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(a, variable) %>%
    dplyr::summarise(value = agg(aggregation, value)) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)

  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  # d$text1 <- map_chr(d$value, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))

  hc <-
    hchart(d, type = "column", hcaes(x = a, y = value, group = variable)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle),
             allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = yAxisTitle)) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.key}</b><br/>"),
               pointFormat = paste("<b style = 'font-size:12px'>", "{point.variable}: {point.value}</b><br/>"))
  # hc_tooltip(
  #   headerFormat= '',
  #   pointFormat ="<b>{point.a}</b> <br>
  #                 {point.variable}: {point.text1}"
  # )
  if (startAtZero) {
    hc <-
      hc %>% hc_yAxis(
        title = list(text = yAxisTitle),
        minRange = 0.1,
        min = 0,
        minPadding = 0
      )
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Horizontal grouped bar
#'
#' Horizontal grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#' hgch_bar_grouped_hor_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_bar_grouped_hor_CatNum
hgch_bar_grouped_hor_CatNum <- function(data,
                                         title = NULL,
                                         subtitle = NULL,
                                         caption = NULL,
                                         xAxisTitle = NULL,
                                         yAxisTitle = NULL,
                                         symbol = NULL,
                                         aggregation = "sum",
                                         startAtZero = FALSE,
                                         theme = NULL,
                                         export = FALSE,
                                         ...) {
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% nms[1]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::gather(variable, value, -a) %>% dplyr::filter(!is.na(a)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(a, variable) %>%
    dplyr::summarise(value = agg(aggregation, value)) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  #d$text1 <- map_chr(d$value, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))

    hc <-
    hchart(d, type = "bar", hcaes(x = a, y = value, group = variable)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = yAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = xAxisTitle)) %>%
      hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.key}</b><br/>"),
                 pointFormat = paste("<b style = 'font-size:12px'>", "{point.variable}: {point.value}</b><br/>"))
  if (startAtZero) {
    hc <-
      hc %>% hc_yAxis(
        title = list(text = xAxisTitle),
        minRange = 0.1,
        min = 0,
        minPadding = 0
      )
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Horizontal grouped bar
#'
#' Horizontal grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num-Num
#' @examples
#' hgch_bar_grouped_hor_CatNumNum(sampleData("Cat-Num-Num", nrow = 10))
#' @export hgch_bar_grouped_hor_CatNumNum
hgch_bar_grouped_hor_CatNumNum <- function(data,
                                         title = NULL,
                                         subtitle = NULL,
                                         caption = NULL,
                                         xAxisTitle = NULL,
                                         yAxisTitle = NULL,
                                         symbol = NULL,
                                         aggregation = "sum",
                                         startAtZero = FALSE,
                                         theme = NULL,
                                         export = FALSE,
                                         ...) {
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% nms[1]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::gather(variable, value, -a) %>% dplyr::filter(!is.na(a)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(a, variable) %>%
    dplyr::summarise(value = agg(aggregation, value)) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  #d$text1 <- map_chr(d$value, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))

  hc <-
    hchart(d, type = "bar", hcaes(x = a, y = value, group = variable)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = yAxisTitle),
             allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = xAxisTitle)) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.key}</b><br/>"),
               pointFormat = paste("<b style = 'font-size:12px'>", "{point.variable}: {point.value}</b><br/>"))
  if (startAtZero) {
    hc <-
      hc %>% hc_yAxis(
        title = list(text = xAxisTitle),
        minRange = 0.1,
        min = 0,
        minPadding = 0
      )
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Horizontal grouped bar
#'
#' Horizontal grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num-Num-Num
#' @examples
#' hgch_bar_grouped_hor_CatNumNumNum(sampleData("Cat-Num-Num-Num", nrow = 10))
#' @export hgch_bar_grouped_hor_CatNumNumNum
hgch_bar_grouped_hor_CatNumNumNum <- function(data,
                                              title = NULL,
                                         subtitle = NULL,
                                         caption = NULL,
                                         xAxisTitle = NULL,
                                         yAxisTitle = NULL,
                                         symbol = NULL,
                                         aggregation = "sum",
                                         startAtZero = FALSE,
                                         theme = NULL,
                                         export = FALSE,
                                         ...) {
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% nms[1]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::gather(variable, value, -a) %>% dplyr::filter(!is.na(a)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(a, variable) %>%
    dplyr::summarise(value = agg(aggregation, value)) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  #d$text1 <- map_chr(d$value, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))

  hc <-
    hchart(d, type = "bar", hcaes(x = a, y = value, group = variable)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = yAxisTitle),
             allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = xAxisTitle)) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.key}</b><br/>"),
               pointFormat = paste("<b style = 'font-size:12px'>", "{point.variable}: {point.value}</b><br/>"))
  if (startAtZero) {
    hc <-
      hc %>% hc_yAxis(
        title = list(text = xAxisTitle),
        minRange = 0.1,
        min = 0,
        minPadding = 0
      )
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Horizontal grouped bar
#'
#' Horizontal grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num-Num-Num-Num
#' @examples
#' hgch_bar_grouped_hor_CatNumNumNumNum(sampleData("Cat-Num-Num-Num-Num", nrow = 10))
#' @export hgch_bar_grouped_hor_CatNumNumNumNum
hgch_bar_grouped_hor_CatNumNumNumNum <- function(data,
                                         title = NULL,
                                         subtitle = NULL,
                                         caption = NULL,
                                         xAxisTitle = NULL,
                                         yAxisTitle = NULL,
                                         symbol = NULL,
                                         aggregation = "sum",
                                         startAtZero = FALSE,
                                         theme = NULL,
                                         export = FALSE,
                                         ...) {
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% nms[1]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::gather(variable, value, -a) %>% dplyr::filter(!is.na(a)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(a, variable) %>%
    dplyr::summarise(value = agg(aggregation, value)) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  #d$text1 <- map_chr(d$value, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))

  hc <-
    hchart(d, type = "bar", hcaes(x = a, y = value, group = variable)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = yAxisTitle),
             allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = xAxisTitle)) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.key}</b><br/>"),
               pointFormat = paste("<b style = 'font-size:12px'>", "{point.variable}: {point.value}</b><br/>"))
  if (startAtZero) {
    hc <-
      hc %>% hc_yAxis(
        title = list(text = xAxisTitle),
        minRange = 0.1,
        min = 0,
        minPadding = 0
      )
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Vertical bar
#'
#' Vertical bar
#'
#' @name hgch_bar_ver_Num
#' @param x A data.frame
#' @export hgch_bar_ver_Num
#' @return highcharts viz
#' @section ctypes:
#' Num
#' @examples
#' hgch_bar_ver_Num(sampleData("Num", nrow = 10))
hgch_bar_ver_Num <- function(data,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         xAxisTitle = NULL,
                         yAxisTitle = NULL,
                         symbol = NULL,
                         startAtZero = FALSE,
                         theme = NULL,
                         export = FALSE,
                         ...) {

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% nms[1]
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

 d <- f$d %>% tidyr::gather(variable, value, 1:length(nms)) %>%
   dplyr::filter(!is.na(value)) %>% dplyr::group_by(variable) %>% dplyr::ungroup()
 codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
 d <- d %>%
   dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
 hgch_bar_ver_CatNum(d, title = title, subtitle = subtitle, caption = caption, xAxisTitle = xAxisTitle, yAxisTitle = yAxisTitle,
                     symbol = symbol, aggregation = aggregation, startAtZero = startAtZero, theme = theme, export = export)

}

#' Vertical bar
#'
#' Vertical bar
#'
#' @name hgch_bar_ver_NumNum
#' @param x A data.frame
#' @export hgch_bar_ver_NumNum
#' @return highcharts viz
#' @section ctypes:
#' Num-Num
#' @examples
#' hgch_bar_ver_NumNum(sampleData("Num-Num", nrow = 10))
hgch_bar_ver_NumNum <- function(data,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              xAxisTitle = NULL,
                              yAxisTitle = NULL,
                              symbol = NULL,
                              aggregation = "sum",
                              startAtZero = FALSE,
                              theme = NULL,
                              export = FALSE,
                              ...) {

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% nms[1]
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::gather(variable, value, 1:length(nms)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(variable) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  hgch_bar_ver_CatNum(d, title = title, subtitle = subtitle, caption = caption, xAxisTitle = xAxisTitle, yAxisTitle = yAxisTitle,
                      symbol = symbol, aggregation = aggregation, startAtZero = startAtZero, theme = theme, export = export)
}

#' Vertical bar
#'
#' Vertical bar
#'
#' @name hgch_bar_ver_NumNumNum
#' @param x A data.frame
#' @export hgch_bar_ver_NumNumNum
#' @return highcharts viz
#' @section ctypes:
#' Num-Num-Num
#' @examples
#' hgch_bar_ver_NumNumNum(sampleData("Num-Num-Num", nrow = 10))
hgch_bar_ver_NumNumNum <- function(data,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              xAxisTitle = NULL,
                              yAxisTitle = NULL,
                              symbol = NULL,
                              aggregation = "sum",
                              startAtZero = FALSE,
                              theme = NULL,
                              export = FALSE,
                              ...) {

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% nms[1]
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::gather(variable, value, 1:length(nms)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(variable) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  hgch_bar_ver_CatNum(d, title = title, subtitle = subtitle, caption = caption, xAxisTitle = xAxisTitle, yAxisTitle = yAxisTitle,
                      symbol = symbol, aggregation = aggregation, startAtZero = startAtZero, theme = theme, export = export)
}


#' Vertical bar
#'
#' Vertical bar
#'
#' @name hgch_bar_ver_NumNumNumNum
#' @param x A data.frame
#' @export hgch_bar_ver_NumNumNumNum
#' @return highcharts viz
#' @section ctypes:
#' Num-Num-Num-Num
#' @examples
#' hgch_bar_ver_NumNumNumNum(sampleData("Num-Num-Num-Num", nrow = 10))
hgch_bar_ver_NumNumNumNum <- function(data,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              xAxisTitle = NULL,
                              yAxisTitle = NULL,
                              symbol = NULL,
                              aggregation = "sum",
                              startAtZero = FALSE,
                              theme = NULL,
                              export = FALSE,
                              ...) {

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% nms[1]
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""


  d <- f$d %>% tidyr::gather(variable, value, 1:length(nms)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(variable) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  hgch_bar_ver_CatNum(d, title = title, subtitle = subtitle, caption = caption, xAxisTitle = xAxisTitle, yAxisTitle = yAxisTitle,
                      symbol = symbol, aggregation = aggregation, startAtZero = startAtZero, theme = theme, export = export)
}


#' Horizontal bar
#'
#' Horizontal bar
#'
#' @name hgch_bar_hor_Num
#' @param x A data.frame
#' @export hgch_bar_hor_Num
#' @return highcharts viz
#' @section ctypes:
#' Num
#' @examples
#' hgch_bar_hor_Num(sampleData("Num", nrow = 10))
hgch_bar_hor_Num <- function(data,
                             title = NULL,
                             subtitle = NULL,
                             caption = NULL,
                             xAxisTitle = NULL,
                             yAxisTitle = NULL,
                             symbol = NULL,
                             aggregation = "sum",
                             startAtZero = FALSE,
                             theme = NULL,
                             export = FALSE,
                             ...) {

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::gather(variable, value, 1:length(nms)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(variable) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  hgch_bar_hor_CatNum(d, title = title, subtitle = subtitle, caption = caption, xAxisTitle = xAxisTitle, yAxisTitle = yAxisTitle,
                      symbol = symbol, aggregation = aggregation, startAtZero = startAtZero, theme = theme, export = export)
}

#' Horizontal bar
#'
#' Horizontal bar
#'
#' @name hgch_bar_hor_NumNum
#' @param x A data.frame
#' @export hgch_bar_hor_NumNum
#' @return highcharts viz
#' @section ctypes:
#' Num-Num
#' @examples
#' hgch_bar_hor_NumNum(sampleData("Num-Num", nrow = 10))
hgch_bar_hor_NumNum <- function(data,
                                title = NULL,
                                subtitle = NULL,
                                caption = NULL,
                                xAxisTitle = NULL,
                                yAxisTitle = NULL,
                                symbol = NULL,
                                aggregation = "sum",
                                startAtZero = FALSE,
                                theme = NULL,
                                export = FALSE,
                                ...) {

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::gather(variable, value, 1:length(nms)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(variable) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  hgch_bar_hor_CatNum(d, title = title, subtitle = subtitle, caption = caption, xAxisTitle = xAxisTitle, yAxisTitle = yAxisTitle,
                      symbol = symbol, aggregation = aggregation, startAtZero = startAtZero, theme = theme, export = export)
}

#' Horizontal bar
#'
#' Horizontal bar
#'
#' @name hgch_bar_hor_NumNumNum
#' @param x A data.frame
#' @export hgch_bar_hor_NumNumNum
#' @return highcharts viz
#' @section ctypes:
#' Num-Num-Num
#' @examples
#' hgch_bar_hor_NumNumNum(sampleData("Num-Num-Num", nrow = 10))
hgch_bar_hor_NumNumNum <- function(data,
                                   title = NULL,
                                   subtitle = NULL,
                                   caption = NULL,
                                   xAxisTitle = NULL,
                                   yAxisTitle = NULL,
                                   symbol = NULL,
                                   aggregation = "sum",
                                   startAtZero = FALSE,
                                   theme = NULL,
                                   export = FALSE,
                                   ...) {

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  d <- f$d %>% tidyr::gather(variable, value, 1:length(nms)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(variable) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  hgch_bar_hor_CatNum(d, title = title, subtitle = subtitle, caption = caption, xAxisTitle = xAxisTitle, yAxisTitle = yAxisTitle,
                      symbol = symbol, aggregation = aggregation, startAtZero = startAtZero, theme = theme, export = export)
}


#' Horizontal bar
#'
#' Horizontal bar
#'
#' @name hgch_bar_hor_NumNumNumNum
#' @param x A data.frame
#' @export hgch_bar_hor_NumNumNumNum
#' @return highcharts viz
#' @section ctypes:
#' Num-Num-Num-Num
#' @examples
#' hgch_bar_hor_NumNumNumNum(sampleData("Num-Num-Num-Num", nrow = 10))
hgch_bar_hor_NumNumNumNum <- function(data,
                                      title = NULL,
                                      subtitle = NULL,
                                      caption = NULL,
                                      xAxisTitle = NULL,
                                      yAxisTitle = NULL,
                                      symbol = NULL,
                                      aggregation = "sum",
                                      startAtZero = FALSE,
                                      theme = NULL,
                                      export = FALSE,
                                      ...) {

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""


  d <- f$d %>% tidyr::gather(variable, value, 1:length(nms)) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(variable) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  hgch_bar_hor_CatNum(d, title = title, subtitle = subtitle, caption = caption, xAxisTitle = xAxisTitle, yAxisTitle = yAxisTitle,
                      symbol = symbol, aggregation = aggregation, startAtZero = startAtZero, theme = theme, export = export)
}



#' Waterfall
#'
#' Waterfall
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#' hgch_waterfall_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_waterfall_CatNum
hgch_waterfall_CatNum <-function(data, title = NULL,  xAxisTitle = NULL, caption = NULL,
                                 aggregation = "sum", yAxisTitle = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  caption <- caption %||% ""
  subtitle <- subtitle %||% ""

  data <- f$d
  data <- plyr::rename(data, c("a" = "name"))

  data_graph <- data %>%
    dplyr::group_by(name) %>%
    tidyr::drop_na(name) %>%
    dplyr::summarise(value = mean(b, na.rm = TRUE ))

  data_graph <- data_graph %>%
    dplyr::mutate(x = 0:(dim(data_graph)[1]-1),
                  y = value,
                  z = (x*y) - median(x*y),
                  color = getPalette()[1:(dim(data_graph)[1])])

  data_graph <- data_graph %>%
    select(y, z, value, name, color) %>%
    arrange(value)


  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_xAxis(text = list(text = xAxisTitle)) %>%
    hc_yAxis(text = list(text = yAxisTitle)) %>%
    hc_chart(type = "waterfall",
             polar = FALSE) %>%
    hc_xAxis(categories = data_graph$name) %>%
    hc_add_series(data_graph, showInLegend = FALSE) %>%
    hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.key}</b><br/>"),
               pointFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.value}</b><br/>")) %>%
    hc_credits(enabled = TRUE, text = caption)
  hc
}




#' Vertical grouped bar
#'
#' Vertical grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat
#' @examples
#' hgch_bar_grouped_ver_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bar_grouped_ver_CatCat
hgch_bar_grouped_ver_CatCat <-
  function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           symbol = NULL,
           startAtZero = FALSE,
           theme = NULL,
           export = FALSE,
           ...) {

    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% nms[2]
    yAxisTitle <- yAxisTitle %||% "Cantidad"
    title <-  title %||% ""
    symbol <- symbol %||% "circle"
    caption <- caption %||% ""
    subtitle <- subtitle %||% ""

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = n())
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <-
      hchart(d, type = "column", hcaes(x = b, y = c, group = a)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = yAxisTitle)) %>%
      hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.key}</b><br/>"),
                 pointFormat = paste("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "{point.a}</b><br/>",
                                     "<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.c}</b><br/>"))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
      hc_credits(enabled = TRUE, text = caption)
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }




#' Horizontal grouped bar
#'
#' Horizontal grouped bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat
#' @examples
#' hgch_bar_grouped_hor_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bar_grouped_hor_CatCat
hgch_bar_grouped_hor_CatCat <-
  function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           symbol = NULL,
           startAtZero = FALSE,
           theme = NULL,
           export = FALSE,
           ...) {
    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% "Cantidad"
    yAxisTitle <- yAxisTitle %||% nms[2]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"
    caption <- caption %||% ""
    subtitle <- subtitle %||% ""

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = n())
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = yAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = xAxisTitle)) %>%
      hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.key}</b><br/>"),
                 pointFormat = paste("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "{point.a}</b><br/>",
                                     "<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.c}</b><br/>"))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = xAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
      hc_credits(enabled = TRUE, text = caption)
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }


#' Vertical stacked bar
#'
#' Vertical stacked bar
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat
#' @examples
#' hgch_bar_stacked_ver_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bar_stacked_ver_CatCat
hgch_bar_stacked_ver_CatCat <-
  function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           symbol = NULL,
           startAtZero = FALSE,
           theme = NULL,
           export = FALSE,
           ...) {
    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% nms[2]
    yAxisTitle <- yAxisTitle %||% "Cantidad"
    title <-  title %||% ""
    symbol <- symbol %||% "circle"
    caption <- caption %||% ""
    subtitle <- subtitle %||% ""

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = n())
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <-
      hchart(d, type = "column", hcaes(x = b, y = c, group = a)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol)),
                     column = list(stacking = "normal")) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = yAxisTitle)) %>%
      hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.key}</b><br/>"),
                 pointFormat = paste("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "{point.a}</b><br/>",
                                     "<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.c}</b><br/>"))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
      hc_credits(enabled = TRUE, text = caption)
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }



#' Horizontal stacked bar
#'
#' Horizontal stacked bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat
#' @examples
#' hgch_bar_stacked_hor_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bar_stacked_hor_CatCat
hgch_bar_stacked_hor_CatCat <-
        function(data,
                 title = NULL,
                 subtitle = NULL,
                 caption = NULL,
                 xAxisTitle = NULL,
                 yAxisTitle = NULL,
                 symbol = NULL,
                 startAtZero = FALSE,
                 theme = NULL,
                 export = FALSE,
                 ...) {
          f <- fringe(data)
          nms <- getClabels(f)

          xAxisTitle <- xAxisTitle %||% "Cantidad"
          yAxisTitle <- yAxisTitle %||% nms[2]
          title <-  title %||% ""
          symbol <- symbol %||% "circle"
          caption <- caption %||% ""
          subtitle <- subtitle %||% ""

          d <-
            f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = n())
          if (nrow(d) == 0)
            return()
          #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
          hc <-
            hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
            hc_plotOptions(series = list(stacking = "normal", marker = list(enabled = TRUE, symbol =  symbol)))%>%
            hc_title(text = title) %>%
            hc_subtitle(text = subtitle) %>%
            hc_xAxis(title = list(text = yAxisTitle),
                     allowDecimals = FALSE) %>%
            hc_yAxis(title = list(text = xAxisTitle)) %>%
            hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.key}</b><br/>"),
                       pointFormat = paste("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "{point.a}</b><br/>",
                                           "<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.c}</b><br/>"))
          if (startAtZero) {
            hc <-
              hc %>% hc_yAxis(
                title = list(text = xAxisTitle),
                minRange = 0.1,
                min = 0,
                minPadding = 0
              )
          }
          hc <- hc %>% hc_add_theme(custom_theme(custom = theme))%>%
            hc_credits(enabled = TRUE, text = caption)
          if (export)
            hc <- hc %>% hc_exporting(enabled = TRUE)
          hc
        }



#' Vertical 100 stacked bar
#'
#' Vertical 100 stacked bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat
#' @examples
#' hgch_bar_stacked_100_ver_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bar_stacked_100_ver_CatCat
hgch_bar_stacked_100_ver_CatCat <-
        function(data,
                 title = NULL,
                 subtitle = NULL,
                 caption = NULL,
                 xAxisTitle = NULL,
                 yAxisTitle = NULL,
                 symbol = NULL,
                 startAtZero = FALSE,
                 theme = NULL,
                 export = FALSE,
                 ...) {
        f <- fringe(data)
        nms <- getClabels(f)

        xAxisTitle <- xAxisTitle %||% nms[2]
        yAxisTitle <- yAxisTitle %||% "Cantidad"
        title <-  title %||% ""
        symbol <- symbol %||% "circle"
        caption <- caption %||% ""
        subtitle <- subtitle %||% ""

        d <- f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = n())
        if (nrow(d) == 0)
        return()
        #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
        hc <-
        hchart(d, type = "column", hcaes(x = b, y = c, group = a)) %>%
        hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol)),
                                     column = list(stacking = "percent")) %>%
        hc_title(text = title) %>%
        hc_subtitle(text = subtitle) %>%
        hc_xAxis(title = list(text = xAxisTitle),
                 allowDecimals = FALSE) %>%
        hc_yAxis(title = list(text = yAxisTitle)) %>%
          hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.key}</b><br/>"),
                     pointFormat = paste("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "{point.a}</b><br/>",
                                         "<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.c}</b><br/>"))
        if (startAtZero) {
          hc <-hc %>% hc_yAxis(
            title = list(text = yAxisTitle),
            minRange = 0.1,
            min = 0,
            minPadding = 0
            )
            }
            hc <- hc %>% hc_add_theme(custom_theme(custom = theme))%>%
              hc_credits(enabled = TRUE, text = caption)

            if (export)
            hc <- hc %>% hc_exporting(enabled = TRUE)
            hc
        }


#' Horizontal 100 stacked bar
#'
#' Horizontal 100 stacked bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Cat
#' @examples
#' hgch_bar_stacked_100_hor_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bar_stacked_100_hor_CatCat
hgch_bar_stacked_100_hor_CatCat <-
  function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           symbol = NULL,
           startAtZero = FALSE,
           theme = NULL,
           export = FALSE,
           ...) {
    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% "Cantidad"
    yAxisTitle <- yAxisTitle %||% nms[2]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"
    caption <- caption %||% ""
    subtitle <- subtitle %||% ""

    d <- f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = n())
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <-
      hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
      hc_plotOptions(series = list(stacking = "percent", marker = list(enabled = TRUE, symbol =  symbol))) %>%
                    # column = list(stacking = "percent")) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = yAxisTitle)) %>%
      hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", paste0(yAxisTitle, ": "), "{point.key}</b><br/>"),
                 pointFormat = paste("<b style = 'font-size:12px'>", paste0(nms[1], ": "), "</b><b style = 'color:{point.color}'>{point.a}</b><br/>",
                                     "<b style = 'font-size:12px'>", paste0(xAxisTitle, ": "), "{point.c}</b><br/>"))
    if (startAtZero) {
      hc <-hc %>% hc_yAxis(
        title = list(text = yAxisTitle),
        minRange = 0.1,
        min = 0,
        minPadding = 0
      )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
      hc_credits(enabled = TRUE, text = caption)
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }



#' Circular bar
#'
#' Circular bar
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @section OtherInfo:
#' moreInfo
#' @examples
#' hgch_circular_bar_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_circular_bar_CatNum
hgch_circular_bar_CatNum <- function(data,
                                     title = NULL,
                                     subtitle = NULL,
                                     caption = NULL,
                                     theme = NULL,
                                     export = FALSE,
                                     aggregation = "sum",
                                     ...) {


  f <- fringe(data)
  nms <- getClabels(f)


  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  df <- f$d
  df <- df %>% plyr::rename(c('a' = 'name', 'b' = 'y'))

  df <- df %>%
    group_by(name) %>%
    dplyr::summarise(y = agg(aggregation, y))

  if(length(is.na(df$name) > 0)) {
    df$name[is.na(df$name)] <- 'Na'
  }

  df <- df %>%
    drop_na(y) %>%
    arrange(-y) %>%
    dplyr::mutate(y = round((y/sum(y)) * 100, 2))
  #df$color <- map(0:(dim(df)[1] - 1), function(i) JS(paste0("Highcharts.getOptions().colors[",i,"]")))
  set.seed(23)
  df$color <- map(getPalette()[1:dim(df)[1]], function(i) i)


  rD <- function(n_rows){
    vc <- c()

    if(n_rows < 4){
      vi <- 112
    }else{
      vi <-  80 #187
    }

    if(n_rows < 4){
      dif <- 25
    }else{
      dif <- 10 #187
    }
    for(i in 1:n_rows){
      if(i==1){
        vc[i] <- vi - dif
      }else{
        nv1 <- vc[i-1]
        Nv <- nv1 - dif
        vc[i] <- Nv
      }
    }
    vc
  }


  df$radius <- rD(n_rows = dim(df)[1])
  df$innerRadius <- df$radius - ifelse(dim(df)[1] < 4, 24, 9)
  df$radius <- paste0(df$radius, '%')
  df$innerRadius <- paste0(df$innerRadius, '%')
  df$borderColor <- getPalette()[1:dim(df)[1]]


  a <- df %>%
    dplyr::group_by(name,borderColor) %>%
    do(data =  transpose(c(color = .$color, radius = .$radius, y = .$y, innerRadius = .$innerRadius)))
  #a$borderColor <- df$color


  xx <- list_parse(a)

  outerRadius <- df$radius
  innerRadius <- df$innerRadius
  #backgroundColor <-  map(0:(dim(df)[1] - 1),function(i) JS(paste0("Highcharts.Color(Highcharts.getOptions().colors[",i, "]).setOpacity(0.3).get()")))
  backgroundColor <-  map(getPalette()[1:dim(df)[1]],
                          function(i) JS(paste0("Highcharts.Color(",paste0("'",i,"'"),").setOpacity(0.3).get()")))

  #map(0:(dim(df)[1] - 1), function(i) JS(paste0("Highcharts.Color(Highcharts.getOptions().colors[",i,"]).setOpacity(0.3).get()")))
  borderWidth <- rep(0, dim(df)[1])

  res <- list(outerRadius = outerRadius,
              innerRadius = innerRadius,
              backgroundColor = backgroundColor,
              borderWidth = borderWidth) %>% transpose()


  #style = list(width = '1500px', height = '1500px', margin = '0 auto')

  hc <- highchart() %>%
    hc_chart(type = "solidgauge") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(borderWidth = 0,backgroundColor = 'none',shadow = FALSE,style = list(fontSize = '16px'),
               pointFormat = '{series.name}<br><span style="font-size:2em; color: {point.color}; font-weight: bold">{point.y}%</span>',
               positioner = JS("function (labelWidth, labelHeight) {return {x: 200 - labelWidth / 2,y: 180};}")) %>%
    hc_pane(startAngle = 0,endAngle = 360,
            background = res ) %>%
    hc_yAxis(min = 0,max = 100,lineWidth = 0,tickPositions = list()) %>%
    hc_plotOptions(solidgauge = list(borderWidth = '2px',dataLabels = list(enabled = FALSE),linecap = 'round',stickyTracking = FALSE)) %>%
    hc_add_series_list(xx)  %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_credits(enabled = TRUE, text = caption)

  hc

}

