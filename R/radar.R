#' Radar (categories)
#'
#' Comparing counts of categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat
#' @examples
#' hgch_radar_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_radar_Cat
hgch_radar_Cat <- function(data,
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

  #se podría hacer un gather y un map para agregar las series necesarias

  if (nrow(d) == 0) return()
  hc <- highchart() %>%
    hc_chart(type = "line", polar = TRUE) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = "", categories = d$a, tickmarkPlacement = "on", lineWidth = 0) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_series(list(name = nms[1], data = d$b)) %>%
    hc_add_theme(custom_theme(custom = theme))
  if (export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}




#' Radar (quantities)
#'
#' Comparing quantities among categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Num
#' @examples
#'
#' hgch_radar_CatNum(sampleData("Cat-Num", nrow = 10))
#'
#' @export hgch_radar_CatNum
hgch_radar_CatNum <- function(data,
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

#' hgch_radar_CatNumNumNumNum
#'
#' hgch_radar_CatNumNumNumNum
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num-Num-Num-Num
#' @examples
#'
#' hgch_radar_CatNumNumNumNum(sampleData("Cat-Num-Num-Num-Num", nrow = 10))
#'
#' @export hgch_radar_CatNumNumNumNum
hgch_radar_CatNumNumNumNum <- function(data, title = NULL, subtitle = NULL, caption = NULL,
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
                                                     d = mean(d, na.rm = TRUE),
                                                     e = mean(e, na.rm = TRUE))
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
      ),
      list(
        name = nms[5],
        data = d$e,
        pointPlacement = 'on'
      ))

  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}
