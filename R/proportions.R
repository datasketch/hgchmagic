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
                         dropNa = FALSE,
                         order = NULL,
                         sort = "no",
                         sliceN = NULL,
                         theme = NULL,
                         export = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  order <- union(order, unique(d$a)[!is.na(unique(d$a))])
  if (all(!is.na(order)) & any(is.na(d$a))) order <- c(union(order, unique(d$a[!is.na(d$a)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$a, order)), ]

  if (sort == "desc")
    d <- d %>%
    dplyr::arrange(desc(b))

  if (sort == "asc")
    d <- d %>%
    dplyr::arrange(b)

  if (!is.null(sliceN))
    d <- d %>%
    dplyr::slice(1:sliceN)


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


#' Pie (categories, numbers)
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
                            yLine = NULL,
                            dropNa = FALSE,
                            agg = "sum",
                            order = NULL,
                            sort = "no",
                            sliceN = NULL,
                            theme = NULL,
                            export = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b))

  order <- union(order, unique(d$a)[!is.na(unique(d$a))])
  if (all(!is.na(order)) & any(is.na(d$a))) order <- c(union(order, unique(d$a[!is.na(d$a)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$a, order)), ]

  if (sort == "desc")
    d <- d %>%
    dplyr::arrange(desc(b))

  if (sort == "asc")
    d <- d %>%
    dplyr::arrange(b)

  if (!is.null(sliceN))
    d <- d %>%
    dplyr::slice(1:sliceN)

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, format = "<b>{point.a}</b>: {point.b} ({point.percentage:.1f}%)")),
                   pie = list(cursor = "pointer", dataLabels = list(style = list(connectorWidth = 0,
                                                                                 width = "100px",
                                                                                 #color = "#393939",
                                                                                 #fontFamily = "roboto_slab_bold",
                                                                                 strokeWidth = 1,
                                                                                 fill = "none")))) %>%
    hc_tooltip(headerFormat = "", pointFormat = paste(ifelse(nrow(f$d) == dplyr::n_distinct(f$d$a), "", agg),
                                                      "<b>{point.a}</b>: {point.b} ({point.percentage:.3f}%)"), followPointer = TRUE, shared = TRUE) %>%
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
                           dropNa = FALSE,
                           order = NULL,
                           sort = "no",
                           sliceN = NULL,
                           theme = NULL,
                           export = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  order <- union(order, unique(d$a)[!is.na(unique(d$a))])
  if (all(!is.na(order)) & any(is.na(d$a))) order <- c(union(order, unique(d$a[!is.na(d$a)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$a, order)), ]

  if (sort == "desc")
    d <- d %>%
    dplyr::arrange(desc(b))

  if (sort == "asc")
    d <- d %>%
    dplyr::arrange(b)

  if (!is.null(sliceN))
    d <- d %>%
    dplyr::slice(1:sliceN)


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


#' Donut (categories, numbers)
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
                              yLine = NULL,
                              dropNa = FALSE,
                              agg = "sum",
                              order = NULL,
                              sort = "no",
                              sliceN = NULL,
                              theme = NULL,
                              export = FALSE, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b))

  order <- union(order, unique(d$a)[!is.na(unique(d$a))])
  if (all(!is.na(order)) & any(is.na(d$a))) order <- c(union(order, unique(d$a[!is.na(d$a)])), NA)
  order[is.na(order)] <- "NA"
  d <- d[order(match(d$a, order)), ]

  if (sort == "desc")
    d <- d %>%
    dplyr::arrange(desc(b))

  if (sort == "asc")
    d <- d %>%
    dplyr::arrange(b)

  if (!is.null(sliceN))
    d <- d %>%
    dplyr::slice(1:sliceN)

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(series = list(innerSize = "60%", dataLabels = list(enabled = TRUE, format = "<b>{point.a}</b>: {point.b} ({point.percentage:.1f}%)")),
                   pie = list(cursor = "pointer", dataLabels = list(style = list(connectorWidth = 0,
                                                                                 width = "100px",
                                                                                 #color = "#393939",
                                                                                 #fontFamily = "roboto_slab_bold",
                                                                                 strokeWidth = 1,
                                                                                 fill = "none")))) %>%
    hc_tooltip(headerFormat = "", pointFormat = paste(ifelse(nrow(f$d) == dplyr::n_distinct(f$d$a), "", agg),
                                                      "<b>{point.a}</b>: {point.b} ({point.percentage:.3f}%)"), followPointer = TRUE, shared = TRUE) %>%
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



