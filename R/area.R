#' Area (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Num, Yea-Num, Dat-Num,
#' @examples
#' hgch_area_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_area_CatNum
hgch_area_CatNum <-  function(data,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              horLabel = NULL,
                              verLabel = NULL,
                              horLine = NULL,
                              horLineLabel = " ",
                              verLine = NULL,
                              verLineLabel = " ",
                              orientation = "ver",
                              startAtZero = TRUE,
                              labelWrap = 12,
                              colors = NULL,
                              colorOpacity = 0.5,
                              agg = "sum",
                              spline = FALSE,
                              marks = c(".", ","),
                              nDigits = NULL,
                              dropNa = FALSE,
                              percentage = FALSE,
                              format = c('', ''),
                              order = NULL,
                              sort = "no",
                              sliceN = NULL,
                              showText = TRUE,
                              tooltip = list(headerFormat = NULL, pointFormat = NULL),
                              export = FALSE,
                              lang = 'es',
                              theme = NULL, ...) {


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
  lineXY <- linesOrientation(orientation, horLine, verLine)

  lineLabelsXY <- linesOrLabel(orientation,
                               horLineLabel,
                               verLineLabel)

  colorDefault <- c("#74D1F7", "#2E0F35", "#B70F7F", "#C2C4C4", "#8097A4", "#A6CEDE", "#801549", "#FECA84", "#ACD9C2")



    if (!is.null(colors)) {
      colors <- unname(fillColors(d, "a", colors, colorScale = 'no'))
    } else {
      colors <- colorDefault
    }

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b))

  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- 'NA'

  if (is.null(nDigits)) {
    nDig <- 0
  } else {
    nDig <- nDigits
  }

  if (percentage) {
    d$b <- (d[['b']] * 100) / sum(d[['b']], na.rm = TRUE)
  }

  d$b <- round(d$b, nDig)
  d <- sortSlice(d, "b", sort, sliceN)
  d <- orderCategory(d, "a", order, labelWrap)


  d <- d %>% plyr::rename(c('b' = 'y'))
  d$color <- NA


  series <- list(list(
    data = map(1:nrow(d), function(x) {
      d$y[x]
    })
  ))


  formatLabAxis <- paste0('{value:', marks[1], marks[2], 'f}')
  if (!is.null(nDigits)) {
    formatLabAxis <- paste0('{value:', marks[1], marks[2], nDigits, 'f}')
  }


  if (is.null(format)) {
    format[1] = ""
    format[2] = ""
  }

  aggFormAxis <- 'function() {return this.value+"";}'


  if (percentage) {
    aggFormAxis <- 'function() {return this.value+"%";}'
    format[2] <- "%"
  }


  aggFormAxis <- paste0("function() { return '", format[1] , "' + Highcharts.numberFormat(this.value, ", nDig, ", '", marks[2], "', '", marks[1], "') + '", format[2], "'}"
  )


  if (is.null(tooltip$pointFormat)) {
    tooltip$pointFormat <- paste0('<b>{point.category}</b><br/>', paste0(agg, ' ' ,nms[2], ': '), format[1],'{point.y}', format[2])
  }
  if (is.null(tooltip$headerFormat)) {
    tooltip$headerFormat <- ""
  }

  global_options(marks[1], marks[2])
  exportLang(language = lang)

  hc <- highchart() %>%
    hc_chart(type = ifelse(spline, "areaspline", "area"),
             inverted = ifelse(orientation == 'ver', FALSE, TRUE)) %>%
    hc_plotOptions(
      series = list(
        fillOpacity = colorOpacity
      )
    ) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>%
    hc_xAxis(
      title =  list(text = labelsXY[1]),
      categories = map(as.character(unique(d$a)), function(z) z),

      plotLines = list(
        list(value = lineXY[2],
             color = 'black',
             dashStyle = 'shortdash',
             zIndex = 5,
             width = 2,
             label = list(
               text = lineLabelsXY[1],
               style = list(
                 color = 'black'
               )
             )))
      #type= 'category'
    ) %>%
    hc_yAxis(
      minRange = if (startAtZero) 0.1,
      min = if (startAtZero) 0,
      minPadding = if (startAtZero) 0,
      title = list (
        text = labelsXY[2]),
      plotLines = list(
        list(value = lineXY[1],
             color = 'black',
             dashStyle = 'shortdash',
             width = 2,
             zIndex = 5,
             label = list(
               text = lineLabelsXY[2],
               style = list(
                 color = 'black'
               )
             ))),
      labels = list (
        format = formatLabAxis,
        formatter = JS(aggFormAxis)
      )
    ) %>%
    hc_add_series_list(series) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(enabled = FALSE)
  if (export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}
  if (is.null(theme)) {
    hc <- hc %>% hc_add_theme(custom_theme(custom = tma(showText = showText, colores = colors)))
  } else {
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
  }
  hc

}

#' Area (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' hgch_area_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_area_Cat
hgch_area_Cat <-  function(data,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           horLabel = NULL,
                           verLabel = NULL,
                           horLine = NULL,
                           horLineLabel = " ",
                           verLine = NULL,
                           verLineLabel = " ",
                           orientation = "ver",
                           startAtZero = TRUE,
                           labelWrap = 12,
                           colors = NULL,
                           colorOpacity = 0.5,
                           agg = "sum",
                           spline = FALSE,
                           marks = c(".", ","),
                           nDigits = NULL,
                           dropNa = FALSE,
                           percentage = FALSE,
                           format = c('', ''),
                           order = NULL,
                           sort = "no",
                           sliceN = NULL,
                           showText = TRUE,
                           tooltip = list(headerFormat = NULL, pointFormat = NULL),
                           export = FALSE,
                           lang = 'es',
                           theme = NULL, ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  names(d) <- c(f$dic_$d$label, paste0("count ", f$dic_$d$label))

  h <- hgch_area_CatNum(data = d, title = title, subtitle = subtitle, caption = caption, horLabel = horLabel,verLabel = verLabel,horLine = horLine,horLineLabel = horLineLabel,verLine = verLine, verLineLabel = verLineLabel,orientation = orientation,startAtZero = startAtZero,labelWrap = labelWrap,colors = colors,colorOpacity=colorOpacity ,agg = agg,spline = spline,marks = marks,nDigits = nDigits,dropNa = dropNa,percentage = percentage,format = format,order = order,sort = sort,sliceN = sliceN,showText = showText, tooltip = tooltip,export = export,lang = lang,theme = theme, ...)
  h
}



#' Area (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Dat-Num, Cat-Yea-Num, Yea-Cat-Num, Yea-Dat-Num, Yea-Yea-Num, Dat-Cat-Num, Dat-Yea-Num, Dat-Dat-Num
#' @examples
#' hgch_area_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_area_CatCatNum
hgch_area_CatCatNum <- function(data,
                                title = NULL,
                                subtitle = NULL,
                                caption = NULL,
                                horLabel = NULL,
                                verLabel = NULL,
                                horLine = NULL,
                                horLineLabel = " ",
                                verLine = NULL,
                                verLineLabel = " ",
                                orientation = "ver",
                                graphType = "grouped",
                                startAtZero = TRUE,
                                agg = "sum",
                                spline = FALSE,
                                colors = NULL,
                                colorOpacity = 0.5,
                                dropNaV = c(FALSE, FALSE),
                                format = c("", ""),
                                labelWrapV = c(12, 12),
                                legendPosition = "right",
                                marks = c(".", ","),
                                nDigits = NULL,
                                order1 = NULL,
                                order2 = NULL,
                                percentage = FALSE,
                                showText = TRUE,
                                theme = NULL,
                                tooltip = list("headerFormat" = NULL,
                                               "pointFormat" = NULL,
                                               "shared" = NULL),
                                export = FALSE, lang = 'es',...) {


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

  lineXY <- linesOrientation(orientation, horLine, verLine)

  lineLabelsXY <- linesOrLabel(orientation,
                               horLineLabel,
                               verLineLabel)

  colorDefault <- c("#74D1F7", "#2E0F35", "#B70F7F", "#C2C4C4", "#8097A4", "#A6CEDE", "#801549", "#FECA84", "#ACD9C2")


    if (!is.null(colors)) {
      colors <- unname(fillColors(d, "a", colors, 'discrete'))
    } else {
      colors <- colorDefault
    }

  if (dropNaV[1])
    d <- d %>%
    tidyr::drop_na(a)

  if(dropNaV[2])
    d <- d %>%
    tidyr::drop_na(b)


  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(agg, c)) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a)
  d$c[is.na(d$c)] <- NA
  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- NA
  d$b <- as.character(d$b)
  d$b[is.na(d$b)] <- NA

  if (is.null(nDigits)) {
    nDig <- 0
  } else {
    nDig <- nDigits
  }

  if (percentage) {
    d <- d %>% group_by(b) %>%
      dplyr::mutate(c = (c / sum(c, na.rm = TRUE)) * 100)
  }


  d <- orderCategory(d, "a", order = order1, labelWrap = labelWrapV[1])
  d <- orderCategory(d, "b", order = order2, labelWrap = labelWrapV[2])
  d$c <- round(d$c, nDig)


  series <- map(unique(d[[1]]), function(i) {
    d0 <- d %>%
      filter(a %in% i)
    l0 <- list("name" = i,
               "data" = d0$c)
  })


  if (percentage & nchar(format[2]) == 0) {
    aggFormAxis <- 'function() {return this.value+"%";}'
    format[2] <- "%"
  }

  formatLabAxis <- paste0('{value:', marks[1], marks[2], 'f}')
  if (!is.null(nDigits)) {
    formatLabAxis <- paste0('{value:', marks[1], marks[2], nDigits, 'f}')
  }


  if (is.null(format)) {
    format[1] = ""
    format[2] = ""
  }

  aggFormAxis <- 'function() {return this.value+"";}'


  aggFormAxis <- paste0("function() { return '", format[1] , "' + Highcharts.numberFormat(this.value, ", nDig, ", '", marks[2], "', '", marks[1], "') + '", format[2], "'}"
  )


  if (is.null(tooltip$pointFormat)) {
    tooltip$pointFormat <-paste0('<b>', nms[2], ': </b>{point.category}</br>',
                                 '<b>', nms[1], ': </b>{series.name}</br>',
                                 paste0(agg, ' ' ,nms[3], ': '), format[1],'{point.y}', format[2])
  }
  if (is.null(tooltip$headerFormat)) {
    tooltip$headerFormat <- " "
  }


  global_options(marks[1], marks[2])
  exportLang(language = lang)


  hc <- highchart() %>%
    hc_chart(type = ifelse(spline, "areaspline", "area"),
             inverted = ifelse(orientation == 'ver', FALSE, TRUE)) %>%
    hc_plotOptions(
      series = list(
        fillOpacity = colorOpacity
      )
    )
  if (graphType == "stacked"){
    hc <- hc %>% hc_plotOptions(area = list(stacking = 'normal', fillOpacity = colorOpacity), areaspline = list(stacking = 'normal', fillOpacity = colorOpacity))
    if (percentage) {
      hc <- hc %>% hc_yAxis(maxRange = 100,
                            max = 100)
    }
  }
  hc <- hc %>% hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(
      categories = map(as.character(unique(d$b)), function(z) z),
      title = list(text = labelsXY[1]),
      plotLines = list(
        list(value = lineXY[2],
             color = 'black',
             dashStyle = 'shortdash',
             zIndex = 5,
             width = 2,
             label = list(
               text = lineLabelsXY[1],
               style = list(
                 color = 'black'
               )
             ))),
      type= 'category'
    ) %>%
    hc_yAxis(
      minRange = if (startAtZero) 0.1,
      min = if (startAtZero) 0,
      minPadding = if (startAtZero) 0,
      title = list (
        text = labelsXY[2]),
      plotLines = list(
        list(value = lineXY[1],
             color = 'black',
             dashStyle = 'shortdash',
             width = 2,
             zIndex = 5,
             label = list(
               text = lineLabelsXY[2],
               style = list(
                 color = 'black'
               )
             ))),

      labels = list (
        format = formatLabAxis,
        formatter = JS(aggFormAxis)
      )) %>%
    hc_add_series_list(series) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(enabled = TRUE, align = legendPosition)
  if (export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}
  if (is.null(theme)) {
    hc <- hc %>% hc_add_theme(custom_theme(custom = tma(showText = showText, colores = colors)))
  } else {
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
  }
  hc
}


#' Area (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat
#' @examples
#' hgch_area_CatCat(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_area_CatCat
hgch_area_CatCat <- function(data,
                             title = NULL,
                             subtitle = NULL,
                             caption = NULL,
                             horLabel = NULL,
                             verLabel = NULL,
                             horLine = NULL,
                             horLineLabel = " ",
                             verLine = NULL,
                             verLineLabel = " ",
                             orientation = "ver",
                             graphType = "grouped",
                             startAtZero = TRUE,
                             agg = "sum",
                             spline = FALSE,
                             colors = NULL,
                             colorOpacity = 0.5,
                             dropNaV = c(FALSE, FALSE),
                             format = c("", ""),
                             labelWrapV = c(12, 12),
                             legendPosition = "right",
                             marks = c(".", ","),
                             nDigits = NULL,
                             order1 = NULL,
                             order2 = NULL,
                             percentage = FALSE,
                             showText = TRUE,
                             theme = NULL,
                             tooltip = list("headerFormat" = NULL,
                                            "pointFormat" = NULL,
                                            "shared" = NULL),
                             export = FALSE, lang = 'es',...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(c = n())

  names(d) <- c(f$dic_$d$label, paste0("count", f$dic_$d$label[1]))

  h <- hgch_area_CatCatNum(data = d, title = title,subtitle = subtitle,caption = caption,horLabel = horLabel,verLabel = verLabel,horLine = horLine,horLineLabel = horLineLabel,verLine = verLine,verLineLabel = verLineLabel,orientation = orientation,graphType = graphType,startAtZero = startAtZero,agg = agg,spline = spline,colors = colors,colorOpacity=colorOpacity,dropNaV = dropNaV,format = format,labelWrapV = labelWrapV,legendPosition = legendPosition,marks = marks,nDigits = nDigits,order1 = order1,order2 = order2,percentage = percentage,showText = showText,theme = theme,tooltip = tooltip,export = export,lang = lang, ...)
  h
}



#' Area (ordered category, n numbers)
#'
#' Compare n quantities among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-NumP
#' @examples
#' hgch_area_CatNumP(sampleData("Cat-NumP", nrow = 10))
#' @export hgch_area_CatNumP

hgch_area_CatNumP <- function(data,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              horLabel = NULL,
                              verLabel = NULL,
                              horLine = NULL,
                              horLineLabel = " ",
                              verLine = NULL,
                              verLineLabel = " ",
                              orientation = "ver",
                              graphType = "grouped",
                              startAtZero = TRUE,
                              agg = "sum",
                              spline = FALSE,
                              colors = NULL,
                              colorOpacity = 0.5,
                              dropNaV = c(FALSE, FALSE),
                              format = c("", ""),
                              labelWrapV = c(12, 12),
                              legendPosition = "right",
                              marks = c(".", ","),
                              nDigits = NULL,
                              order1 = NULL,
                              order2 = NULL,
                              percentage = FALSE,
                              showText = TRUE,
                              theme = NULL,
                              tooltip = list("headerFormat" = NULL,
                                             "pointFormat" = NULL,
                                             "shared" = NULL),
                              export = FALSE, lang = 'es', ...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  names(d) <- f$dic_$d$label

  data <- d %>%
    gather("categories", "count", names(d)[-1])

  h <- hgch_area_CatCatNum(data = data, title = title,subtitle = subtitle,caption = caption,horLabel = horLabel,verLabel = verLabel,horLine = horLine,horLineLabel = horLineLabel,verLine = verLine,verLineLabel = verLineLabel,orientation = orientation,graphType = graphType, startAtZero = startAtZero,agg = agg,spline = spline,colors = colors,colorOpacity=colorOpacity,dropNaV = dropNaV,format = format,labelWrapV = labelWrapV,legendPosition = legendPosition,marks = marks,nDigits = nDigits,order1 = order1,order2 = order2,percentage = percentage, showText = showText,theme = theme,tooltip = tooltip,export = export, lang = lang,...)
  h
}
