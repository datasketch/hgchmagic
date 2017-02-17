#' hgch_bar_ver_Ca
#' @name hgch_bar_ver_Ca
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_bar_ver_Ca(sampleData("Ca",nrow = 10))
hgch_bar_ver_Ca <-
  function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           sort = "no",
           aggregate = "count",
           theme = NULL,
           export = FALSE,
           ...) {

    if(class(data)[1] == "Fringe"){
      ni <- getClabels(data)
    }else{
      ni <- names(data)
    }

    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% nms[1]
    yAxisTitle <- yAxisTitle %||% nms[2]
    title <-  title %||% ""
    d <- f$d
    if (nrow(d) == 0)
      return()
    d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = n()) %>% drop_na()
    if (sort == "top") {
      d <- d %>% dplyr::arrange(desc(b))
    }
    d$ni <- ni

    hc <- hchart(d, type = "column", hcaes(x = a, y = b)) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle)) %>%
      hc_yAxis(title = list(text = yAxisTitle)) %>%
      hc_tooltip(
        formatter = JS("function(){
                return this.point.ni + ': <b>' + Highcharts.numberFormat(this.point.b,1,'.',',')+'</b><br/>';
            }")
      )
    #%>%
     # hc_tooltip( pointFormat=paste0(
    #    ni,': {point.b}'))
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }

#' hgch_bar_ver_top_Ca
#' @name hgch_bar_ver_top_Ca
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_bar_ver_top_Ca(sampleData("Ca-Nu",nrow = 10))
hgch_bar_ver_top_Ca <- function(data,
                                title = NULL,
                                subtitle = NULL,
                                caption = NULL,
                                xAxisTitle = NULL,
                                yAxisTitle = NULL,
                                reverse = TRUE,
                                aggregate = "count",
                                theme = NULL,
                                export = FALSE,
                                ...) {
  hgch_bar_ver_Ca(
    data,
    title = title,
    xAxisTitle = xAxisTitle,
    yAxisTitle = yAxisTitle,
    sort = "top",
    aggregate = aggregate,
    theme = theme
  )

}


#' hgch_bar_hor_Ca
#' @name hgch_bar_hor_Ca
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_bar_hor_Ca(sampleData("Ca",nrow = 10))
hgch_bar_hor_Ca <-
  function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           sort = "no",
           aggregate = "count",
           theme = NULL,
           export = FALSE,
           ...) {

    if(class(data)[1] == "Fringe"){
      ni <- getClabels(data)
    }else{
      ni <- names(data)
    }

    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% nms[1]
    yAxisTitle <- yAxisTitle %||% nms[2]
    title <-  title %||% ""
    d <- f$d
    if (nrow(d) == 0)
      return()
    d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = n())
    if (sort == "top") {
      d <- d %>% dplyr::arrange(desc(b))
    }
    hc <- hchart(d, type = "bar", hcaes(x = a, y = b)) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle)) %>%
      hc_yAxis(title = list(text = yAxisTitle))%>%
      hc_tooltip( pointFormat=paste0(
        ni,': {point.b}'))
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
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
                                title = NULL,
                                subtitle = NULL,
                                caption = NULL,
                                xAxisTitle = NULL,
                                yAxisTitle = NULL,
                                reverse = TRUE,
                                aggregate = "count",
                                theme = NULL,
                                export = FALSE,
                                ...) {
  hgch_bar_hor_Ca(
    data,
    title = title,
    subtitle = subtitle,
    xAxisTitle = xAxisTitle,
    yAxisTitle = yAxisTitle,
    sort = "top",
    aggregate = aggregate,
    theme = theme
  )

}




#' hgch_bar_ver_CaNu
#' @name hgch_bar_ver_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_bar_ver_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_bar_ver_CaNu <-
  function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           sort = "no",
           aggregate = NULL,
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

    d <- f$d %>%
      tidyr::drop_na() %>%
      dplyr::group_by(a) %>%
      dplyr::summarise(b = mean(b))

    if (nrow(d) == 0)
      return()
    if (sort == "top") {
      d <- d %>% dplyr::arrange(desc(b))
    }
    hc <- hchart(d, type = "column", hcaes(x = a, y = b)) %>%
      # hc_plotOptions(
      #   series = list(marker = list(enabled = TRUE, symbol =  symbol))
      # ) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = yAxisTitle)) %>%
      hc_tooltip(
        formatter = JS(paste0("function(){
                return '<b>' + this.point.a + '</b><br/>' +
                 '",y,"'+ ': <b>' +Highcharts.numberFormat(this.point.b,1,'.',',')+'</b><br/>';
            }"))
      )
      #hc_tooltip( pointFormat=paste0(
       # y,': {point.b}'))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc

  }
#' hgch_bar_ver_top_CaNu
#' @name hgch_bar_ver_top_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_bar_ver_top_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_bar_ver_top_CaNu <- function(data,
                                  title = NULL,
                                  subtitle = NULL,
                                  caption = NULL,
                                  xAxisTitle = NULL,
                                  yAxisTitle = NULL,
                                  reverse = TRUE,
                                  aggregate = "mean",
                                  theme = NULL,
                                  export = FALSE,
                                  ...) {
  hgch_bar_ver_CaNu(
    data,
    title = title,
    subtitle = subtitle,
    xAxisTitle = xAxisTitle,
    yAxisTitle = yAxisTitle,
    sort = "top",
    aggregate = aggregate,
    theme = theme
  )

}

#' hgch_bar_ver_YeNu
#' @name hgch_bar_ver_YeNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_bar_ver_YeNu(sampleData("Ye-Nu",nrow = 10))
hgch_bar_ver_YeNu <- hgch_bar_ver_CaNu


#' hgch_bar_ver_DaNu
#' @name hgch_bar_ver_DaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_bar_ver_DaNu(sampleData("Da-Nu",nrow = 10))
hgch_bar_ver_DaNu <- hgch_bar_ver_CaNu


#' hgch_bar_hor_CaNu
#' @name hgch_bar_hor_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_bar_hor_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_bar_hor_CaNu <- function(data,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              xAxisTitle = NULL,
                              yAxisTitle = NULL,
                              sort = "no",
                              aggregate = "mean",
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

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% " "
  d <- f$d
  d <- na.omit(d)
  if (nrow(d) == 0)
    return()
  d <-
    d %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b, na.rm = TRUE)) %>% drop_na()
  if (sort == "top") {
    d <- d %>% dplyr::arrange(desc(b))
  }
  hc <- hchart(d, type = "bar", hcaes(x = a, y = b)) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle)) %>%
    hc_yAxis(title = list(text = yAxisTitle))  %>%
    hc_tooltip(
      formatter = JS(paste0("function(){
                return '<b>' + this.point.a + '</b><br/>' +
                '",y,"'+ ': <b>' +Highcharts.numberFormat(this.point.b,1,'.',',')+'</b><br/>';
            }"))
    )
    #hc_tooltip( pointFormat=paste0(
    #  y,': {point.b}'))
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
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
                                  title = NULL,
                                  subtitle = NULL,
                                  caption = NULL,
                                  xAxisTitle = NULL,
                                  yAxisTitle = NULL,
                                  reverse = TRUE,
                                  aggregate = "mean",
                                  theme = NULL,
                                  export = FALSE,
                                  ...) {
  hgch_bar_hor_CaNu(
    data,
    title = title,
    subtitle = subtitle,
    xAxisTitle = xAxisTitle,
    yAxisTitle = yAxisTitle,
    sort = "top",
    aggregate = aggregate,
    theme = theme,
    export = export
  )

}




#' hgch_bar_grouped_ver_CaCaNu
#' @name hgch_bar_grouped_ver_CaCaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ca-Nu
#' @examples
#' hgch_bar_grouped_ver_CaCaNu(sampleData("Ca-Ca-Nu",nrow = 10))
hgch_bar_grouped_ver_CaCaNu <-
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
    yAxisTitle <- yAxisTitle %||% nms[3]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = mean(c))
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
      hc_yAxis(title = list(text = yAxisTitle))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }
#' hgch_bar_grouped_ver_CaYeNu
#' @name hgch_bar_grouped_ver_CaYeNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_bar_grouped_ver_CaYeNu(sampleData("Ye-Nu-Nu",nrow = 10))
hgch_bar_grouped_ver_CaYeNu <- hgch_bar_grouped_ver_CaCaNu


#' hgch_bar_grouped_hor_CaCaNu
#' @name hgch_bar_grouped_hor_CaCaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ca-Nu
#' @examples
#' hgch_bar_grouped_hor_CaCaNu(sampleData("Ca-Ca-Nu",nrow = 10))
hgch_bar_grouped_hor_CaCaNu <-
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
    yAxisTitle <- yAxisTitle %||% nms[3]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = mean(c))
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = yAxisTitle))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }


#' hgch_bar_stacked_ver_CaCaNu
#' @name hgch_bar_stacked_CaCaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_bar_stacked_ver_CaCaNu(sampleData("Ye-Nu-Nu",nrow = 10))
hgch_bar_stacked_ver_CaCaNu <-
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
    yAxisTitle <- yAxisTitle %||% nms[3]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = mean(c))
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
      hc_yAxis(title = list(text = yAxisTitle))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }
#' hgch_bar_stacked_100_ver_CaCaNu
#' @name hgch_bar_stacked_100_ver_CaCaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_bar_stacked_100_ver_CaCaNu(sampleData("Ye-Nu-Nu",nrow = 10))
hgch_bar_stacked_100_ver_CaCaNu <-
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
    yAxisTitle <- yAxisTitle %||% nms[3]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = mean(c))
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
      hc_yAxis(title = list(text = yAxisTitle))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }

#' hgch_bar_stacked_hor_CaCaNu
#' @name hgch_bar_stacked_hor_CaCaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_bar_stacked_hor_CaCaNu(sampleData("Ye-Nu-Nu",nrow = 10))
hgch_bar_stacked_hor_CaCaNu <-
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
    yAxisTitle <- yAxisTitle %||% nms[3]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = mean(c))
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol)),
                     bar = list(stacking = "normal")) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = yAxisTitle))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }
#' hgch_bar_stacked_100_hor_CaCaNu
#' @name hgch_bar_stacked_100_hor_CaCaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_bar_stacked_100_hor_CaCaNu(sampleData("Ye-Nu-Nu",nrow = 10))
hgch_bar_stacked_100_hor_CaCaNu <-
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
    yAxisTitle <- yAxisTitle %||% nms[3]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = mean(c))
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol)),
                     bar = list(stacking = "percent")) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = yAxisTitle))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }

#' hgch_bar_stacked_ver_CaDaNu
#' @name hgch_bar_stacked_ver_CaDaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ca-Nu
#' @examples
#' hgch_bar_stacked_ver_CaDaNu(sampleData("Ca-Ca-Nu",nrow = 10))
hgch_bar_stacked_ver_CaDaNu <- hgch_bar_stacked_ver_CaCaNu

#' hgch_bar_stacked_100_ver_CaDaNu
#' @name hgch_bar_stacked_100_ver_CaDaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ca-Nu
#' @examples
#' hgch_bar_stacked_100_ver_CaDaNu(sampleData("Ca-Ca-Nu",nrow = 10))
hgch_bar_stacked_100_ver_CaDaNu <- hgch_bar_stacked_100_ver_CaCaNu






#' hgch_bar_grouped_ver_CaNuP
#' Multilines
#' @name hgch_bar_grouped_ver_CaNuP
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftypes: Ye-Nu*
#' @examples
#' hgch_bar_grouped_ver_CaNuP(sampleData("Ca-NuP",nrow = 10))
hgch_bar_grouped_ver_CaNuP <- function(data,
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

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  d <- f$d %>% tidyr::gather(variable, value,-a) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(a, variable) %>%
    dplyr::summarise(value = mean(value)) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  hc <-
    hchart(d, type = "column", hcaes(x = a, y = value, group = variable)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle),
             allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = yAxisTitle)) %>%
    hc_tooltip(
      formatter = JS(paste0("function(){
                return '<b>' + this.point.a + '</b><br/>' +
                this.point.variable + ': <b>' +Highcharts.numberFormat(this.point.value,1,'.',',')+'</b><br/>';
            }"))
    )
  if (startAtZero) {
    hc <-
      hc %>% hc_yAxis(
        title = list(text = yAxisTitle),
        minRange = 0.1,
        min = 0,
        minPadding = 0
      )
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}
#' hgch_bar_grouped_hor_CaNuP
#' Multilines
#' @name hgch_bar_grouped_hor_CaNuP
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftypes: Ye-Nu*
#' @examples
#' hgch_bar_grouped_hor_CaNuP(sampleData("Ca-NuP",nrow = 10))
hgch_bar_grouped_hor_CaNuP <- function(data,
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

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% f$name
  symbol <- symbol %||% "circle"
  d <- f$d %>% tidyr::gather(variable, value,-a) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(a, variable) %>%
    dplyr::summarise(value = mean(value)) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  hc <-
    hchart(d, type = "bar", hcaes(x = a, y = value, group = variable)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle),
             allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = yAxisTitle)) %>%
    hc_tooltip(
      formatter = JS(paste0("function(){
                return '<b>' + this.point.a + '</b><br/>' +
                this.point.variable + ': <b>' +Highcharts.numberFormat(this.point.value,1,'.',',')+'</b><br/>';
            }"))
    )
  if (startAtZero) {
    hc <-
      hc %>% hc_yAxis(
        title = list(text = yAxisTitle),
        minRange = 0.1,
        min = 0,
        minPadding = 0
      )
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}



#' hgch_bar_NuP
#' Multilines
#' @name hgch_bar_NuP
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftypes: Ye-Nu*
#' @examples
#' hgch_bar_NuP(sampleData("NuP",nrow = 10))
hgch_bar_ver_NuP <- function(data,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         xAxisTitle = NULL,
                         yAxisTitle = NULL,
                         ...) {
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% ""
  yAxisTitle <- yAxisTitle %||% nms[1]
  title <-  title %||% f$name

  d <- f$d %>% tidyr::gather(variable, value,-a) %>%
    dplyr::filter(!is.na(value)) %>% dplyr::group_by(a, variable) %>%
    dplyr::summarise(value = mean(value)) %>% dplyr::ungroup()
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)
  d <- d %>%
    dplyr::mutate(variable = fct_recode_df(d, "variable", codes))
  hc <-
    hchart(d, type = "line", hcaes(x = a, y = value, group = variable)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xAxisTitle),
             allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = yAxisTitle))
  if (startAtZero) {
    hc <-
      hc %>% hc_yAxis(
        title = list(text = yAxisTitle),
        minRange = 0.1,
        min = 0,
        minPadding = 0
      )
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' hgch_waterfall_CaNu
#' @name hgch_waterfall_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_waterfall_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_waterfall_CaNu <-function(data, title = NULL,  xAxisTitle = NULL,
                               yAxisTitle = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
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
    hc_xAxis(text = list(text = yAxisTitle)) %>%
    hc_chart(type = "waterfall",
             polar = FALSE) %>%
    hc_xAxis(categories = data_graph$name) %>%
    hc_add_series(data_graph, showInLegend = FALSE)
  hc
}


#' hgch_bar_grouped_ver_CaCa
#' @name hgch_bar_grouped_ver_CaCa
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_bar_grouped_ver_CaCa(sampleData("Ca-Ca",nrow = 10))
hgch_bar_grouped_ver_CaCa <-
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
    yAxisTitle <- yAxisTitle %||% nms[3]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"

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
      hc_yAxis(title = list(text = yAxisTitle))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }


#' hgch_bar_grouped_hor_CaCa
#' @name hgch_bar_grouped_hor_CaCa
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ca
#' @examples
#' hgch_bar_grouped_hor_CaCa(sampleData("Ca-Ca",nrow = 10))
hgch_bar_grouped_hor_CaCa <-
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
    yAxisTitle <- yAxisTitle %||% nms[3]
    title <-  title %||% ""
    symbol <- symbol %||% "circle"

    d <-
      f$d %>% na.omit() %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = n())
    if (nrow(d) == 0)
      return()
    #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
    hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
      hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_xAxis(title = list(text = xAxisTitle),
               allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = yAxisTitle))
    if (startAtZero) {
      hc <-
        hc %>% hc_yAxis(
          title = list(text = yAxisTitle),
          minRange = 0.1,
          min = 0,
          minPadding = 0
        )
    }
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }
