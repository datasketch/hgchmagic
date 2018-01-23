#' Pie (categories)
#'
#' Comparing counts of categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat
#' @examples
#' hgch_pie_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_pie_Cat
hgch_pie_Cat <- function(data,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         agg = "count",
                         theme = NULL,
                         export = FALSE,...) {

  f <- fringe(data)
  nms <- getClabels(f)

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d %>%
    tidyr::replace_na(list(a = ifelse(is.character(f$d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  if (nrow(d) == 0) return()

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, format = '<b>{point.a}</b>: {point.b} ({point.percentage:.1f}%)')),
                   pie = list(cursor = 'pointer', dataLabels = list(style = list(connectorWidth = 0,
                                                                                 width = '100px',
                                                                                 #color = "#393939",
                                                                                 #fontFamily = "roboto_slab_bold",
                                                                                 strokeWidth = 1,
                                                                                 fill = 'none')))) %>%
    hc_tooltip(headerFormat = "", pointFormat = "<b>{point.a}</b>: {point.b} ({point.percentage:.3f}%)", followPointer = TRUE, shared = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_add_theme(custom_theme(custom = theme))
  if (export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Pie (quatities)
#'
#' Comparing quantities among categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Num
#' @examples
#' hgch_pie_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_pie_CatNum
hgch_pie_CatNum <- function(data,
                            title = NULL,
                            subtitle = NULL,
                            caption = NULL,
                            agg = "sum",
                            theme = NULL,
                            export = FALSE,...) {

  f <- fringe(data)
  nms <- getClabels(f)

  title <- title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d %>%
    tidyr::replace_na(list(a = ifelse(is.character(f$d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b))

  if (nrow(d) == 0) return()

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, format = '<b>{point.a}</b>: {point.b} ({point.percentage:.1f}%)')),
                   pie = list(cursor = 'pointer', dataLabels = list(style = list(connectorWidth = 0,
                                                                                 width = '100px',
                                                                                 #color = "#393939",
                                                                                 #fontFamily = "roboto_slab_bold",
                                                                                 strokeWidth = 1,
                                                                                 fill = 'none')))) %>%
    hc_tooltip(headerFormat = "", pointFormat = "<b>{point.a}</b>: {point.b} ({point.percentage:.3f}%)", followPointer = TRUE, shared = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_add_theme(custom_theme(custom = theme))

  if (export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' Donut (categories)
#'
#' Comparing counts of categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat
#' @examples
#' hgch_donut_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_donut_Cat
hgch_donut_Cat <- function(data,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           agg = "count",
                           theme = NULL,
                           export = FALSE,...) {

  f <- fringe(data)
  nms <- getClabels(f)

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d %>%
    tidyr::replace_na(list(a = ifelse(is.character(f$d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  if (nrow(d) == 0) return()

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(series = list(innerSize = "60%", dataLabels = list(enabled = TRUE, format = '<b>{point.a}</b>: {point.b} ({point.percentage:.1f}%)')),
                   pie = list(cursor = 'pointer', dataLabels = list(style = list(connectorWidth = 0,
                                                                                 width = '100px',
                                                                                 #color = "#393939",
                                                                                 #fontFamily = "roboto_slab_bold",
                                                                                 strokeWidth = 1,
                                                                                 fill = 'none')))) %>%
    hc_tooltip(headerFormat = "", pointFormat = "<b>{point.a}</b>: {point.b} ({point.percentage:.3f}%)", followPointer = TRUE, shared = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_add_theme(custom_theme(custom = theme))

  if (export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}



#' Donut (quantities)
#'
#' Comparing quantities among categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Num
#' @examples
#' hgch_donut_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_donut_CatNum
hgch_donut_CatNum <- function(data,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              agg = "sum",
                              theme = NULL,
                              export = FALSE,...) {

  f <- fringe(data)
  nms <- getClabels(f)

  title <- title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d %>%
    tidyr::replace_na(list(a = ifelse(is.character(f$d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b))

  if (nrow(d) == 0) return()

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(series = list(innerSize = "60%", dataLabels = list(enabled = TRUE, format = '<b>{point.a}</b>: {point.b} ({point.percentage:.1f}%)')),
                   pie = list(cursor = 'pointer', dataLabels = list(style = list(connectorWidth = 0,
                                                                                 width = '100px',
                                                                                 #color = "#393939",
                                                                                 #fontFamily = "roboto_slab_bold",
                                                                                 strokeWidth = 1,
                                                                                 fill = 'none')))) %>%
    hc_tooltip(headerFormat = "", pointFormat = "<b>{point.a}</b>: {point.b} ({point.percentage:.3f}%)", followPointer = TRUE, shared = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_add_theme(custom_theme(custom = theme))

  if (export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}



#' Pyramid
#'
#' Pyramid
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#'
#' hgch_pyramid_CatNum(sampleData("Cat-Num",nrow = 10))
#'
#' @export hgch_pyramid_CatNum
hgch_pyramid_CatNum <-function(data, title = ""){

  f <- fringe(data)
  nms <- getCnames(f)
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
    dplyr::select(x, y, z, value, name, color) %>%
    dplyr::arrange(-value)

  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_chart(type = "pyramid",
             polar = FALSE) %>%
    hc_xAxis(categories = data_graph$name) %>%
    hc_add_series(data_graph, showInLegend = FALSE)
  hc
}





#' Funnel
#'
#' Funnel
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#'
#' hgch_funnel_CatNum(sampleData("Cat-Num",nrow = 10))
#'
#' @export hgch_funnel_CatNum
hgch_funnel_CatNum <-function(data, title = ""){


  f <- fringe(data)
  nms <- getCnames(f)
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
    select(x, y, z, value, name, color) %>%
    arrange(-value)

  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_chart(type = "funnel",
             polar = FALSE) %>%
    hc_xAxis(categories = data_graph$name) %>%
    hc_add_series(data_graph, showInLegend = FALSE)
  hc

}



