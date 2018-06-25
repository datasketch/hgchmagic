#' Bar (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' hgch_bar_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_bar_Cat
hgch_bar_Cat <- function(data,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         horLabel = NULL,
                         verLabel = NULL,
                         horLine = NULL,
                         horLineLabel = NULL,
                         verLine = NULL,
                         verLineLabel = NULL,
                         marks = c(" ", "."),
                         nDigits = 2,
                         colors = c("#009EE3", "#F9B233"),
                         dropNa = FALSE,
                         format = "{value}",
                         highlightValue = NULL,
                         order = NULL,
                         orientation = "ver",
                         percentage = FALSE,
                         sort = "no",
                         sliceN = NULL,
                         theme = NULL,
                         tooltip = list("headerFormat" = NULL,
                                        "pointFormat" = NULL,
                                        "shared" = NULL),
                         export = FALSE, ...) {



  options(highcharter.lang = sepThous(marks))

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = paste("count", nms[1]),
                            hor = horLabel,
                            ver = verLabel)
  lineXY <- orientationXY(orientation,
                          NULL,
                          NULL,
                          hor = horLine,
                          ver = verLine)
  lineLabelsXY <- orientationXY(orientation,
                                x = horLine,
                                y = verLine,
                                hor = horLineLabel,
                                ver = verLineLabel,
                                line = TRUE)
  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA))) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())
  d$b <- round(d$b, digits = nDigits)
  d <- percentColumn(d, "b", percentage, nDt = nDigits)
  d <- orderCategory(d, "a", order)
  d <- sortSlice(d, "b", sort, sliceN)
  d <- highlightValueData(d, "a", highlightValue, colors[1], colors[2])



  if (percentage) {
    d <- plyr::rename(d, c('b' = 'conteo'))
    d$b <- d$percent
    tooltip <- tooltipHc(d, nms, tooltip, paste("count", nms[1]), "conteo", percentage,  nDt = nDigits)
  } else {
    tooltip <- tooltipHc(d, nms, tooltip, paste("count", nms[1]), "b", percentage,  nDt = nDigits)
  }

  hc <- hchart(d, type = ifelse(orientation == "hor", "bar", "column"), hcaes(x = a, y = b, color = color)) %>%
    hc_tooltip(headerFormat = tooltip$headerFormat,
               pointFormat = tooltip$pointFormat,
               shared = tooltip$shared) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = labelsXY[1]),
             allowDecimals = FALSE,
             plotLines = list(list(value = lineXY[2],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[1])))) %>%
    hc_yAxis(title = list(text = labelsXY[2]),
             plotLines = list(list(value = lineXY[1],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[2]))))
  if (percentage) {
    hc <- hc %>% hc_yAxis(
      labels = list(
        formatter = JS('function() {
                      return this.value+"%";}')
      )
    )
  }
  hc <- hc %>%  hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Bar (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Num, Yea-Num, Dat-Num,
#' @examples
#' hgch_bar_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_bar_CatNum
hgch_bar_CatNum <- function(data,
                            title = NULL,
                            subtitle = NULL,
                            caption = NULL,
                            horLabel = NULL,
                            verLabel = NULL,
                            horLine = NULL,
                            horLineLabel = NULL,
                            verLine = NULL,
                            verLineLabel = NULL,
                            agg = "sum",
                            marks = c("", "."),
                            nDigits = 0,
                            colors = c("#009EE3", "#F9B233"),
                            dropNa = FALSE,
                            format = "{value}",
                            highlightValue = NULL,
                            order = NULL,
                            orientation = "ver",
                            percentage = FALSE,
                            sort = "no",
                            sliceN = NULL,
                            theme = NULL,
                            tooltip = list("headerFormat" = NULL,
                                           "pointFormat" = NULL,
                                           "shared" = NULL),
                            export = FALSE, ...) {

  #options(highcharter.lang = sepThous(marks))

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(agg, nms[2])),
                            hor = horLabel,
                            ver = verLabel)
  lineXY <- orientationXY(orientation,
                          NULL,
                          NULL,
                          hor = horLine,
                          ver = verLine)
  lineLabelsXY <- orientationXY(orientation,
                                x = horLine,
                                y = verLine,
                                hor = horLineLabel,
                                ver = verLineLabel,
                                line = TRUE)
  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b))

  d <- percentColumn(d, "b", percentage, nDt = nDigits)
  d <- orderCategory(d, "a", order)
  d <- sortSlice(d, "b", sort, sliceN)
  d <- highlightValueData(d, "a", highlightValue, colors[1], colors[2])
  tooltip <- tooltipHc(d, nms, tooltip, paste(agg, nms[2]), "b", percentage, nDt = nDigits)

  options(highcharter.lang = sepThous(marks))

  hc <- hchart(d, type = ifelse(orientation == "hor", "bar", "column"), hcaes(x = a, y = b, color = color)) %>%
    hc_tooltip(headerFormat = tooltip$headerFormat,
               pointFormat = tooltip$pointFormat,
               shared = tooltip$shared) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = labelsXY[1]),
             #allowDecimals = FALSE,
             plotLines = list(list(value = lineXY[2],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[1])))) %>%
    hc_yAxis(title = list(text = labelsXY[2]),
             plotLines = list(list(value = lineXY[1],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[2]))),
             labels = list(
               format= '{value}',
               formatter= JS(paste0("function () {
                 return Highcharts.numberFormat(this.value,",nDigits,",'", marks[2],"','", marks[1],"');
               }")
             ))) %>%
    #hc_legend(layout = legendLayout) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Bar (years, numbers)
#'
#' Compare quantities over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Yea-Num
#' @examples
#' hgch_bar_YeaNum(sampleData("Yea-Num", nrow = 10))
#' @export hgch_bar_YeaNum
hgch_bar_YeaNum <- hgch_bar_CatNum


#' Bar (dates, numbers)
#'
#' Compare quantities over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Dat-Num
#' @examples
#' hgch_bar_DatNum(sampleData("Dat-Num", nrow = 10))
#' @export hgch_bar_DatNum
hgch_bar_DatNum <- hgch_bar_CatNum


#' Grouped bar by first category (categories, categories)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat, Cat-Dat, Cat-Yea, Yea-Cat, Yea-Dat, Yea-Yea, Dat-Cat, Dat-Yea, Dat-Dat
#' @examples
#' hgch_bar_grouped_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bar_grouped_CatCat
hgch_bar_grouped_CatCat <- function(data,
                                    title = NULL,
                                    subtitle = NULL,
                                    caption = NULL,
                                    horLabel = NULL,
                                    verLabel = NULL,
                                    horLine = NULL,
                                    horLineLabel = NULL,
                                    verLine = NULL,
                                    verLineLabel = NULL,
                                    #colors = c("#009EE3", "#F9B233"),
                                    dropNa = FALSE,
                                    format = "{value}",
                                    legendLayout = "horizontal",
                                    #highlightValue = NULL,
                                    order = NULL,
                                    orientation = "ver",
                                    percentage = FALSE,
                                    sort = "no",
                                    sliceN = NULL,
                                    theme = NULL,
                                    tooltip = list("headerFormat" = NULL,
                                                   "pointFormat" = NULL,
                                                   "shared" = NULL),
                                    export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = paste("count", nms[2]),
                            hor = horLabel,
                            ver = verLabel)
  lineXY <- orientationXY(orientation,
                          NULL,
                          NULL,
                          hor = horLine,
                          ver = verLine)
  lineLabelsXY <- orientationXY(orientation,
                                x = horLine,
                                y = verLine,
                                hor = horLineLabel,
                                ver = verLineLabel,
                                line = TRUE)
  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a) #%>%
    #dplyr::mutate(formato = ifelse(is.na(c), "NA", c))
  d$c[is.na(d$c)] <- 0

  d <- percentColumn(d, "c", percentage)
  d <- orderCategory(d, "a", order)
  d <- sortSlice(d, "c", sort, sliceN)
  tooltip <- tooltipHc(d, nms, tooltip, paste("count", nms[2]), "c", percentage)

  hc <- hchart(d, type = ifelse(orientation == "hor", "bar", "column"), hcaes(x = a, y = c, group = b)) %>%
    hc_tooltip(headerFormat = tooltip$headerFormat,
               pointFormat = tooltip$pointFormat,
               shared = tooltip$shared) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = labelsXY[1]),
             allowDecimals = FALSE,
             plotLines = list(list(value = lineXY[2],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[1])))) %>%
    hc_yAxis(title = list(text = labelsXY[2]),
             plotLines = list(list(value = lineXY[1],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[2]))),
             labels = list(format = format)) %>%
    hc_legend(layout = legendLayout) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Stacked bar by first cartegory (categories, categories)
#'
#' Compare stacked categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat, Cat-Dat, Cat-Yea, Yea-Cat, Yea-Dat, Yea-Yea, Dat-Cat, Dat-Yea, Dat-Dat
#' @examples
#' hgch_bar_stacked_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bar_stacked_CatCat
hgch_bar_stacked_CatCat <-  function(data,
                                     title = NULL,
                                     subtitle = NULL,
                                     caption = NULL,
                                     horLabel = NULL,
                                     verLabel = NULL,
                                     horLine = NULL,
                                     horLineLabel = NULL,
                                     verLine = NULL,
                                     verLineLabel = NULL,
                                     dropNa = FALSE,
                                     format = "{value}",
                                     legendLayout = "horizontal",
                                     order = NULL,
                                     orientation = "ver",
                                     percentage = FALSE,
                                     sort = "no",
                                     sliceN = NULL,
                                     theme = NULL,
                                     tooltip = list("headerFormat" = NULL,
                                                    "pointFormat" = NULL,
                                                    "shared" = NULL),
                                     export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = paste("count", nms[2]),
                            hor = horLabel,
                            ver = verLabel)
  lineXY <- orientationXY(orientation,
                          NULL,
                          NULL,
                          hor = horLine,
                          ver = verLine)
  lineLabelsXY <- orientationXY(orientation,
                                x = horLine,
                                y = verLine,
                                hor = horLineLabel,
                                ver = verLineLabel,
                                line = TRUE)

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a) #%>%
  #dplyr::mutate(formato = ifelse(is.na(c), "NA", c))
  d$c[is.na(d$c)] <- 0

  d <- percentColumn(d, "c", percentage)
  d <- orderCategory(d, "a", order)
  d <- sortSlice(d, "c", sort, sliceN) # FALTA
  tooltip <- tooltipHc(d, nms, tooltip, paste("count", nms[2]), "c", percentage)

  hc <- hchart(d, type = ifelse(orientation == "hor", "bar", "column"), hcaes(x = a, y = c, group = b)) %>%
    hc_plotOptions(bar = list(stacking = "normal"), column = list(stacking = "normal")) %>%
    hc_tooltip(headerFormat = tooltip$headerFormat,
               pointFormat = tooltip$pointFormat,
               shared = tooltip$shared) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = labelsXY[1]),
             allowDecimals = FALSE,
             plotLines = list(list(value = lineXY[2],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[1])))) %>%
    hc_yAxis(title = list(text = labelsXY[2]),
             plotLines = list(list(value = lineXY[1],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[2]))),
             labels = list(format = format)) %>%
    hc_legend(layout = legendLayout) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' 100% stacked bar by first category (categories, ordered categories)
#'
#' Compare 100% stacked categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat, Cat-Dat, Cat-Yea, Yea-Cat, Yea-Dat, Yea-Yea, Dat-Cat, Dat-Yea, Dat-Dat
#' @examples
#' hgch_bar_stacked_100_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bar_stacked_100_CatCat
hgch_bar_stacked_100_CatCat <- function(data,
                                        title = NULL,
                                        subtitle = NULL,
                                        caption = NULL,
                                        horLabel = NULL,
                                        verLabel = NULL,
                                        horLine = NULL,
                                        horLineLabel = NULL,
                                        verLine = NULL,
                                        verLineLabel = NULL,
                                        dropNa = FALSE,
                                        format = "{value}",
                                        legendLayout = "horizontal",
                                        order = NULL,
                                        orientation = "ver",
                                        sliceN = NULL,
                                        theme = NULL,
                                        tooltip = list("headerFormat" = NULL,
                                                       "pointFormat" = NULL,
                                                       "shared" = NULL),
                                        export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = paste("count", nms[2]),
                            hor = horLabel,
                            ver = verLabel)
  lineXY <- orientationXY(orientation,
                          NULL,
                          NULL,
                          hor = horLine,
                          ver = verLine)
  lineLabelsXY <- orientationXY(orientation,
                                x = horLine,
                                y = verLine,
                                hor = horLineLabel,
                                ver = verLineLabel,
                                line = TRUE)

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = n()) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a) #%>%
  #dplyr::mutate(formato = ifelse(is.na(c), "NA", c))
  d$c[is.na(d$c)] <- 0

  d <- orderCategory(d, "a", order)
  d <- sortSlice(d, "c", "no", sliceN) # FALTA
  tooltip <- tooltipHc(d, nms, tooltip, paste("count", nms[2]), "c", FALSE, stacked100 = TRUE)

  hc <- hchart(d, type = ifelse(orientation == "hor", "bar", "column"), hcaes(x = a, y = c, group = b)) %>%
    hc_plotOptions(bar = list(stacking = "percent"), column = list(stacking = "percent")) %>%
    hc_tooltip(headerFormat = tooltip$headerFormat,
               pointFormat = tooltip$pointFormat,
               shared = tooltip$shared) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = labelsXY[1]),
             allowDecimals = FALSE,
             plotLines = list(list(value = lineXY[2],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[1])))) %>%
    hc_yAxis(title = list(text = labelsXY[2]),
             plotLines = list(list(value = lineXY[1],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[2]))),
             labels = list(format = paste0(format, "%"))) %>%
    hc_legend(layout = legendLayout) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Grouped bar by first category (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Dat-Num, Cat-Yea-Num, Yea-Cat-Num, Yea-Dat-Num, Yea-Yea-Num, Dat-Cat-Num, Dat-Yea-Num, Dat-Dat-Num
#' @examples
#' hgch_bar_grouped_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_bar_grouped_CatCatNum
hgch_bar_grouped_CatCatNum <- function(data,
                                       title = NULL,
                                       subtitle = NULL,
                                       caption = NULL,
                                       horLabel = NULL,
                                       verLabel = NULL,
                                       horLine = NULL,
                                       horLineLabel = NULL,
                                       verLine = NULL,
                                       verLineLabel = NULL,
                                       agg = "sum",
                                       #colors = c("#009EE3", "#F9B233"),
                                       dropNa = FALSE,
                                       format = "{value}",
                                       legendLayout = "horizontal",
                                       #highlightValue = NULL,
                                       order = NULL,
                                       orientation = "ver",
                                       percentage = FALSE,
                                       sort = "no",
                                       sliceN = NULL,
                                       theme = NULL,
                                       tooltip = list("headerFormat" = NULL,
                                                      "pointFormat" = NULL,
                                                      "shared" = NULL),
                                       export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$b), nms[3], paste(agg, nms[3])),
                            hor = horLabel,
                            ver = verLabel)
  lineXY <- orientationXY(orientation,
                          NULL,
                          NULL,
                          hor = horLine,
                          ver = verLine)
  lineLabelsXY <- orientationXY(orientation,
                                x = horLine,
                                y = verLine,
                                hor = horLineLabel,
                                ver = verLineLabel,
                                line = TRUE)

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(agg, c)) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a) #%>%
  #dplyr::mutate(formato = ifelse(is.na(c), "NA", c))
  d$c[is.na(d$c)] <- 0

  d <- percentColumn(d, "c", percentage)
  d <- orderCategory(d, "a", order)
  d <- sortSlice(d, "c", sort, sliceN)
  tooltip <- tooltipHc(d, nms, tooltip, paste("count", nms[2]), "c", percentage)

  hc <- hchart(d, type = ifelse(orientation == "hor", "bar", "column"), hcaes(x = a, y = c, group = b)) %>%
    hc_tooltip(headerFormat = tooltip$headerFormat,
               pointFormat = tooltip$pointFormat,
               shared = tooltip$shared) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = labelsXY[1]),
             allowDecimals = FALSE,
             plotLines = list(list(value = lineXY[2],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[1])))) %>%
    hc_yAxis(title = list(text = labelsXY[2]),
             plotLines = list(list(value = lineXY[1],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[2]))),
             labels = list(format = format)) %>%
    hc_legend(layout = legendLayout) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Grouped bar (categories, year, numbers)
#'
#' Compare quantities among categories over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_bar_grouped_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_bar_grouped_CatYeaNum
hgch_bar_grouped_CatYeaNum <- hgch_bar_grouped_CatCatNum


#' Grouped bar (categories, dates, numbers)
#'
#' Compare quantities among categories over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_bar_grouped_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_bar_grouped_CatDatNum
hgch_bar_grouped_CatDatNum <- hgch_bar_grouped_CatCatNum


#' Stacked bar by first category (categories, ordered categories, numbers)
#'
#' Compare quantities among stacked categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bar_stacked_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_bar_stacked_CatCatNum
hgch_bar_stacked_CatCatNum <- function(data,
                                       title = NULL,
                                       subtitle = NULL,
                                       caption = NULL,
                                       horLabel = NULL,
                                       verLabel = NULL,
                                       horLine = NULL,
                                       horLineLabel = NULL,
                                       verLine = NULL,
                                       verLineLabel = NULL,
                                       agg = "sum",
                                       dropNa = FALSE,
                                       format = "{value}",
                                       legendLayout = "horizontal",
                                       order = NULL,
                                       orientation = "ver",
                                       percentage = FALSE,
                                       sort = "no",
                                       sliceN = NULL,
                                       theme = NULL,
                                       tooltip = list("headerFormat" = NULL,
                                                      "pointFormat" = NULL,
                                                      "shared" = NULL),
                                       export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$b), nms[3], paste(agg, nms[3])),
                            hor = horLabel,
                            ver = verLabel)
  lineXY <- orientationXY(orientation,
                          NULL,
                          NULL,
                          hor = horLine,
                          ver = verLine)
  lineLabelsXY <- orientationXY(orientation,
                                x = horLine,
                                y = verLine,
                                hor = horLineLabel,
                                ver = verLineLabel,
                                line = TRUE)

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(agg, c)) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a) #%>%
  #dplyr::mutate(formato = ifelse(is.na(c), "NA", c))
  d$c[is.na(d$c)] <- 0

  d <- percentColumn(d, "c", percentage)
  d <- orderCategory(d, "a", order)
  d <- sortSlice(d, "c", sort, sliceN)
  tooltip <- tooltipHc(d, nms, tooltip, paste("count", nms[2]), "c", percentage)

  hc <- hchart(d, type = ifelse(orientation == "hor", "bar", "column"), hcaes(x = a, y = c, group = b)) %>%
    hc_plotOptions(bar = list(stacking = "normal"), column = list(stacking = "normal")) %>%
    hc_tooltip(headerFormat = tooltip$headerFormat,
               pointFormat = tooltip$pointFormat,
               shared = tooltip$shared) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = labelsXY[1]),
             allowDecimals = FALSE,
             plotLines = list(list(value = lineXY[2],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[1])))) %>%
    hc_yAxis(title = list(text = labelsXY[2]),
             plotLines = list(list(value = lineXY[1],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[2]))),
             labels = list(format = format)) %>%
    hc_legend(layout = legendLayout) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' Stacked bar (categories, years, numbers)
#'
#' Compare quantities among stacked categories over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_bar_stacked_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_bar_stacked_CatYeaNum
hgch_bar_stacked_CatYeaNum <- hgch_bar_stacked_CatCatNum


#' Stacked bar (categories, dates, numbers)
#'
#' Compare quantities among stacked categories over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_bar_stacked_CatYeaNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_bar_stacked_CatDatNum
hgch_bar_stacked_CatDatNum <- hgch_bar_stacked_CatCatNum


#' 100% stacked bar (categories, ordered categories, numbers)
#'
#' Compare quantities among 100% stacked categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_bar_stacked_100_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_bar_stacked_100_CatCatNum
hgch_bar_stacked_100_CatCatNum <- function(data,
                                           title = NULL,
                                           subtitle = NULL,
                                           caption = NULL,
                                           horLabel = NULL,
                                           verLabel = NULL,
                                           horLine = NULL,
                                           horLineLabel = NULL,
                                           verLine = NULL,
                                           verLineLabel = NULL,
                                           agg = "sum",
                                           nDigits = 2,
                                           dropNa = FALSE,
                                           format = "{value}",
                                           legendLayout = "horizontal",
                                           order = NULL,
                                           orientation = "ver",
                                           sliceN = NULL,
                                           theme = NULL,
                                           tooltip = list("headerFormat" = NULL,
                                                          "pointFormat" = NULL,
                                                          "shared" = NULL),
                                           export = FALSE, ...) {
  data <- sampleData('Cat-Cat-Num')
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$b), nms[3], paste(agg, nms[3])),
                            hor = horLabel,
                            ver = verLabel)
  lineXY <- orientationXY(orientation,
                          NULL,
                          NULL,
                          hor = horLine,
                          ver = verLine)
  lineLabelsXY <- orientationXY(orientation,
                                x = horLine,
                                y = verLine,
                                hor = horLineLabel,
                                ver = verLineLabel,
                                line = TRUE)

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(agg, c)) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a) #%>%
  #dplyr::mutate(formato = ifelse(is.na(c), "NA", c))
  d$c[is.na(d$c)] <- 0

  d <- orderCategory(d, "a", order)
  d <- sortSlice(d, "c", "no", sliceN) # FALTA
  tooltip <- tooltipHc(d, nms, tooltip, paste("count", nms[2]), "c", FALSE, stacked100 = TRUE, nDt = nDigits)

  hc <- hchart(d, type = ifelse(orientation == "hor", "bar", "column"), hcaes(x = a, y = c, group = b)) %>%
    hc_plotOptions(bar = list(stacking = "percent"), column = list(stacking = "percent")) %>%
    hc_tooltip(headerFormat = tooltip$headerFormat,
               pointFormat = tooltip$pointFormat,
               shared = tooltip$shared) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = labelsXY[1]),
             allowDecimals = FALSE,
             plotLines = list(list(value = lineXY[2],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[1])))) %>%
    hc_yAxis(title = list(text = labelsXY[2]),
             plotLines = list(list(value = lineXY[1],
                                   color = 'black',
                                   dashStyle = 'shortdash',
                                   width = 2,
                                   label = list(text = lineLabelsXY[2]))),
             labels = list(format = paste0(format, "%"))) %>%
    hc_legend(layout = legendLayout) %>%
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc
}


#' 100% stacked bar (categories, years, numbers)
#'
#' Compare quantities among 100% stacked categories over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Yea-Num
#' @examples
#' hgch_bar_stacked_100_CatYeaNum(sampleData("Cat-Yea-Num", nrow = 10))
#' @export hgch_bar_stacked_100_CatYeaNum
hgch_bar_stacked_100_CatYeaNum <- hgch_bar_stacked_100_CatCatNum


#' 100% stacked bar (categories, dates, numbers)
#'
#' Compare quantities among 100% stacked categories over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_bar_stacked_100_CatYeaNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export hgch_bar_stacked_100_CatDatNum
hgch_bar_stacked_100_CatDatNum <- hgch_bar_stacked_100_CatCatNum


#' Bar (ordered category, n numbers)
#'
#' Compare n quantities among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-NumP
#' @examples
#' hgch_bar_grouped_CatNumP(sampleData("Cat-NumP", nrow = 10))
#' @export hgch_bar_grouped_CatNumP
hgch_bar_grouped_CatNumP <- function(data,
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
                                     orientation = "ver",
                                     percentage = FALSE,
                                     sort = "no",
                                     sliceN = NULL,
                                     theme = NULL,
                                     export = FALSE, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  codes <- data_frame(variable = letters[1:ncol(f$d)], to = nms)

  d <- d  %>%
    tidyr::gather(variable, value, -a) %>%
    dplyr::group_by(a, variable) %>%
    dplyr::summarise(value = agg(agg, value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(2, 1, 3)
  d <- dplyr::mutate(d, variable = fct_recode_df(d, "variable", codes))
  names(d)[2] <- nms[1]

  hc <- hgch_bar_grouped_CatCatNum(data = d,
                                   title = title ,
                                   subtitle = subtitle,
                                   caption = caption,
                                   horLabel = horLabel,
                                   verLabel = verLabel,
                                   yLine = yLine,
                                   yLineLabel = yLineLabel,
                                   agg = agg,
                                   dropNa = dropNa,
                                   order = order,
                                   orientation = orientation,
                                   percentage = percentage,
                                   sort = sort,
                                   sliceN = sliceN,
                                   theme = theme,
                                   export = export, ...)
  hc
}


#' Bar (years, n numbers)
#'
#' Compare n quantities over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Yea-NumP
#' @examples
#' hgch_bar_grouped_YeaNumP(sampleData("Yea-NumP", nrow = 10))
#' @export hgch_bar_grouped_YeaNumP
hgch_bar_grouped_YeaNumP <- hgch_bar_grouped_CatNumP


#' Bar (dates, n numbers)
#'
#' Compare n quantities over dates
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Dat-NumP
#' @examples
#' hgch_bar_grouped_DatNumP(sampleData("Dat-NumP", nrow = 10))
#' @export hgch_bar_grouped_DatNumP
hgch_bar_grouped_DatNumP <- hgch_bar_grouped_CatNumP



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
                                 aggregation = "sum", yAxisTitle = NULL, subtitle = NULL){

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

