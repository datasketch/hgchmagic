#' hgch_pie_Cat
#'
#' hgch_pie_Cat
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat
#' @examples
#'
#' hgch_pie_Cat(sampleData("Cat",nrow = 10))
#'
#' @export hgch_pie_Cat
hgch_pie_Cat <- function(data, title = NULL, subtitle = NULL, caption = NULL, font_size = '13px',
                         sort = "no", aggregate = "count", export = FALSE, theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  title <-  title %||% ""
  subtitle <-  subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d
  if(nrow(d) == 0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = n())
  # para que el label  de NA sea NA y no vacío
  d <- tidyr::replace_na(d, list(a = "NA", b = NA))

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE, format = '<b>{point.a}</b>: {point.b}')),
     # hc_plotOptions(
        pie = list(
          #allowPointSelect =  TRUE,
          cursor = 'pointer',
          dataLabels = list(
            #enabled = FALSE,
            style = list(
              connectorWidth = 0,
              fontSize = font_size,
              width = '100px',
              #color = "#393939",
              #fontFamily = "roboto_slab_bold",
              strokeWidth=1,
              fill = 'none')
          )
        )

    ) %>%
    hc_tooltip(headerFormat = "", pointFormat = "<b>{point.a}</b>: {point.b}", followPointer = TRUE, shared = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption)
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
#' @section ctypes:
#' Cat-Num
#' @examples
#'
#' hgch_pie_CatNum(sampleData("Cat-Num", nrow = 10))
#'
#' @export hgch_pie_CatNum
hgch_pie_CatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL, sort = "no",
                            aggregate = "sum", export = FALSE, font_size = '13px', theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  title <- title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = sum(b, na.rm = TRUE))
  # para que el label  de NA sea NA y no vacío
  d <- tidyr::replace_na(d, list(a = "NA", b = NA))

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(dataLabels = list(enabled = TRUE, format = '<b>{point.a}</b>: {point.b}')),
                    pie = list(
                      #allowPointSelect =  TRUE,
                      cursor = 'pointer',
                      dataLabels = list(
                        #enabled = FALSE,
                        style = list(
                          connectorWidth = 0,
                          width = '100px',
                          fontSize = font_size,
                          #color = "#393939",
                          #fontFamily = "roboto_slab_bold",
                          strokeWidth = 1,
                          fill = 'none')
                      )
                    )
    ) %>%
    hc_tooltip(headerFormat = "", pointFormat = "<b>{point.a}</b>: {point.b}", followPointer=TRUE, shared = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption)

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
#' @section ctypes:
#' Cat
#' @examples
#'
#' hgch_donut_Cat(sampleData("Cat", nrow = 10))
#'
#' @export hgch_donut_Cat
hgch_donut_Cat <- function(data, title = NULL, subtitle = NULL, caption = NULL,
                           sort = "no", aggregate = "count", export = FALSE, theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d
  if (nrow(d) == 0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = n())
  # para que el label  de NA sea NA y no vacío
  d <- tidyr::replace_na(d, list(a = "NA", b = NA))

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(innerSize= '60%',dataLabels = list(enabled = TRUE,format=   '<b>{point.a}</b>: {point.b}'))
    ) %>%
    hc_tooltip(headerFormat = "", pointFormat = "<b>{point.a}</b>: {point.b}", followPointer=TRUE, shared = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption)

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
#' @section ctypes:
#' Cat-Num
#' @examples
#'
#' hgch_donut_CatNum(sampleData("Cat-Num",nrow = 10))
#'
#' @export hgch_donut_CatNum
hgch_donut_CatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL,
                              sort = "no", aggregate = "sum", export = FALSE, theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  title <- title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d
  if(nrow(d)==0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = sum(b, na.rm = TRUE))
  # para que el label  de NA sea NA y no vacío
  d <- tidyr::replace_na(d, list(a = "NA", b = NA))

  hc <- hchart(d, type = "pie", hcaes(x = a, y = b)) %>%
    hc_plotOptions(
      series = list(innerSize= '60%',dataLabels = list(enabled = TRUE,format=   '<b>{point.a}</b>: {point.b}'))
    ) %>%
    hc_tooltip(headerFormat = "", pointFormat = "<b>{point.a}</b>: {point.b}", followPointer=TRUE, shared = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption)

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
#' @section ctypes:
#' Cat
#' @examples
#'
#' hgch_radar_Cat(sampleData("Cat", nrow = 10))
#'
#' @export hgch_radar_Cat
hgch_radar_Cat <- function(data, title = NULL, subtitle = NULL, caption = NULL,
                           sort = "no", aggregate = "mean", export = FALSE, theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d
  d <- na.omit(d)
  if(nrow(d) == 0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = n())
  hc <- highchart() %>%
    hc_chart(type = "line", polar = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = "",
             categories = d$a, tickmarkPlacement = 'on',lineWidth = 0) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_series(
      list(
        name = nms[1],
        data = d$b,
        pointPlacement = 'on'
      ))
  hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
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
#' @section ctypes:
#' Cat-Num
#' @examples
#'
#' hgch_radar_CatNum(sampleData("Cat-Num", nrow = 10))
#'
#' @export hgch_radar_CatNum
hgch_radar_CatNum <- function(data, title = NULL, subtitle = NULL, caption = NULL,
                              sort = "no", aggregate = "mean", export = FALSE, theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d
  d <- na.omit(d)
  if(nrow(d) == 0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b,na.rm = TRUE))
  hc <- highchart() %>%
    hc_chart(type = "line", polar = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = "",
             categories = d$a, tickmarkPlacement = 'on',lineWidth = 0) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
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

#' hgch_radar_CatNumNum
#'
#' hgch_radar_CatNumNum
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num-Num
#' @examples
#'
#' hgch_radar_CatNumNum(sampleData("Cat-Num-Num", nrow = 10))
#'
#' @export hgch_radar_CatNumNum
hgch_radar_CatNumNum <- function(data, title = NULL, subtitle = NULL, caption = NULL,
                                 sort = "no", aggregate = "mean", export = FALSE, theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d
  d <- na.omit(d)
  if(nrow(d) == 0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b, na.rm = TRUE), c = mean(c, na.rm = TRUE))
  hc <- highchart() %>%
    hc_chart(type = "line", polar = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = "",
             categories = d$a, tickmarkPlacement = 'on',lineWidth = 0) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
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


#' hgch_radar_CatNumNumNum
#'
#' hgch_radar_CatNumNumNum
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num-Num-Num
#' @examples
#'
#' hgch_radar_CatNumNumNum(sampleData("Cat-Num-Num-Num", nrow = 10))
#'
#' @export hgch_radar_CatNumNumNum
hgch_radar_CatNumNumNum <- function(data, title = NULL, subtitle = NULL, caption = NULL,
                                    sort = "no", aggregate = "mean", export = FALSE, theme = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  d <- f$d
  d <- na.omit(d)
  if(nrow(d) == 0) return()
  d <- d %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b, na.rm = TRUE),
                                                     c = mean(c, na.rm = TRUE),
                                                     d = mean(d, na.rm = TRUE))
  hc <- highchart() %>%
    hc_chart(type = "line", polar = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = "",
             categories = d$a, tickmarkPlacement = 'on', lineWidth = 0) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
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
      ),
      list(
        name = nms[4],
        data = d$d,
        pointPlacement = 'on'
      ))

  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}




