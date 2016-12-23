#' hgch_bar_ver_Ca
#' @name hgch_bar_ver_Ca
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_bar_ver_Ca(sampleData("Ca",nrow = 10))
hgch_bar_ver_Ca <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                            sort = "no", aggregate = "count",theme = NULL, export = FALSE,...){

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
  hc <- hchart(d, type = "column", hcaes(x = a, y = b)) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
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
                                title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                reverse = TRUE, aggregate = "count", theme = NULL, export = FALSE,...){

  hgch_bar_ver_Ca(data, title = title, xAxisTitle = xAxisTitle,
                  yAxisTitle = yAxisTitle, sort = "top", aggregate = aggregate, theme = theme)

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
                            sort = "no", aggregate = "count", theme = NULL, export = FALSE,...){

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
  hc <- hchart(d, type = "bar", hcaes(x = a, y = b)) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
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
                                title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                reverse = TRUE, aggregate = "count", theme = NULL, export = FALSE,...){

  hgch_bar_hor_Ca(data, title = title, xAxisTitle = xAxisTitle,
                  yAxisTitle = yAxisTitle, sort = "top", aggregate = aggregate, theme = theme)

}




#' hgch_bar_ver_CaNu
#' @name hgch_bar_ver_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ye-Nu
#' @examples
#' hgch_bar_ver_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_bar_ver_CaNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                              sort = "no", aggregate = NULL,
                          startAtZero = FALSE,theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""

  d <- f$d %>% na.omit() %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b))
  if(nrow(d)==0) return()
  if(sort == "top"){
    d <- d %>% dplyr::arrange(desc(b))
  }
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
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
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
                                  title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                  reverse = TRUE, aggregate = "mean", theme = NULL, export = FALSE,...){

  hgch_bar_ver_CaNu(data, title = title, xAxisTitle = xAxisTitle,
                    yAxisTitle = yAxisTitle, sort = "top", aggregate = aggregate, theme = theme)

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
                              title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                              sort = "no", aggregate = "mean", theme = NULL, export = FALSE,...){

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
  hc <- hchart(d, type = "bar", hcaes(x = a, y = b)) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle)) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
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
                                  title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                  reverse = TRUE, aggregate = "mean", theme = NULL, export = FALSE,...){

  hgch_bar_hor_CaNu(data, title = title, xAxisTitle = xAxisTitle,
                    yAxisTitle = yAxisTitle, sort = "top", aggregate = aggregate, theme = theme)

}




#' hgch_bar_grouped_ver_CaCaNu
#' @name hgch_bar_grouped_ver_CaCaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Ca-Nu
#' @examples
#' hgch_bar_grouped_ver_CaCaNu(sampleData("Ca-Ca-Nu",nrow = 10))
hgch_bar_grouped_ver_CaCaNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                        symbol = NULL,  startAtZero = FALSE,theme = NULL, export = FALSE,...){

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
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
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
hgch_bar_grouped_hor_CaCaNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                        symbol = NULL,  startAtZero = FALSE,theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = mean(c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol))
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
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
hgch_bar_stacked_ver_CaCaNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                            symbol = NULL,  startAtZero = FALSE,theme = NULL, export = FALSE,...){

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
      series = list(marker = list(enabled = TRUE, symbol =  symbol)),
      column = list(stacking = "normal")
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
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
hgch_bar_stacked_100_ver_CaCaNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                        symbol = NULL,  startAtZero = FALSE,theme = NULL, export = FALSE,...){

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
      series = list(marker = list(enabled = TRUE, symbol =  symbol)),
      column = list(stacking = "percent")
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
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
hgch_bar_stacked_hor_CaCaNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                        symbol = NULL,  startAtZero = FALSE,theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = mean(c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol)),
      bar = list(stacking = "normal")
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
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
hgch_bar_stacked_100_hor_CaCaNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                            symbol = NULL,  startAtZero = FALSE,theme = NULL, export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[2]
  yAxisTitle <- yAxisTitle %||% nms[3]
  title <-  title %||% ""
  symbol <- symbol %||% "circle"

  d <- f$d %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = mean(c))
  if(nrow(d)==0) return()
  #d <- d %>% group_by(a) %>% summarise(b = mean(b,na.rm = TRUE)) %>% arrange(desc(b))
  hc <- hchart(d, type = "bar", hcaes(x = b, y = c, group = a)) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, symbol =  symbol)),
      bar = list(stacking = "percent")
    ) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
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
                                  title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                  symbol = NULL,  startAtZero = FALSE,theme = NULL, export = FALSE,...){
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
  hc <- hchart(d, type = "column",hcaes( x = a, y = value, group = variable)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
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
                                       title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                                       symbol = NULL,  startAtZero = FALSE,theme = NULL, export = FALSE,...){
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
  hc <- hchart(d, type = "bar",hcaes( x = a, y = value, group = variable)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, symbol =  symbol))) %>%
    hc_title(text = title) %>%
    hc_xAxis(title = list(text=xAxisTitle), allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text=yAxisTitle))
  if(startAtZero){
    hc <- hc %>% hc_yAxis(title = list(text=yAxisTitle), minRange = 0.1, min = 0, minPadding = 0)
  }
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}
