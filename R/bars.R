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
hgch_bar_CatNum <-  function(data,
                             title = NULL,
                             subtitle = NULL,
                             caption = NULL,
                             horLabel = NULL,
                             verLabel = NULL,
                             horLine = NULL,
                             horLineLabel = " ",
                             verLine = NULL,
                             verLineLabel = " ",
                             labelWrap = 12,
                             colors = NULL,
                             colorScale = 'no',
                             agg = "sum",
                             orientation = "ver",
                             marks = c(".", ","),
                             nDigits = NULL,
                             dropNa = FALSE,
                             highlightValueColor = '#F9B233',
                             percentage = FALSE,
                             format = c('', ''),
                             highlightValue = NULL,
                             order = NULL,
                             sort = "no",
                             sliceN = NULL,
                             showText = TRUE,
                             legendPosition = c("right", "bottom"),
                             tooltip = list(headerFormat = NULL, pointFormat = NULL),
                             export = FALSE,
                             theme = NULL,
                             lang = 'es',
                             ...) {


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

  if (colorScale == 'discrete') {
    colorDefault <- c("#74D1F7", "#2E0F35", "#B70F7F", "#C2C4C4", "#8097A4", "#A6CEDE", "#801549", "#FECA84", "#ACD9C2")
  } else {
    colorDefault <- leaflet::colorNumeric(c("#2E0F35", "#A6CEDE"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }


    if (!is.null(colors)) {
      colors <- unname(fillColors(d, "a", colors, colorScale))
    } else {
      if (colorScale == 'no') {
      colors <- c("#74D1F7", "#74D1F7")
      } else {
      colors <- colorDefault
      }
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
  d <- orderCategory(d, "a", order, labelWrap)
  d <- sortSlice(d, "b", sort, sliceN)


  d <- d %>% plyr::rename(c('b' = 'y'))
  d$color <- NA

  if (!is.null(highlightValue)) {
    w <- which(d$a %in% highlightValue)
    d$color[w] <- highlightValueColor
  }

  data <- list()
  bla <- map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "y" = d$y[z],
                            "color" = as.character(d$color[z]))
  })

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
    tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(agg, ' ' ,nms[2], ': '), format[1],'{point.y}', format[2])
  }
  if (is.null(tooltip$headerFormat)) {
    tooltip$headerFormat <- ""
  }

  global_options(marks[1], marks[2])
  exportLang(language = lang)
  hc <- highchart() %>%
    hc_chart(type = ifelse(orientation == "hor", "bar", "column")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>%
    hc_xAxis(
      title =  list(text = labelsXY[1]),
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
    hc_series(
      data
    ) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(enabled = FALSE,
              align= legendPosition[1],
              verticalAlign= legendPosition[2])
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
hgch_bar_Cat <-  function(data,
                          title = NULL,
                          subtitle = NULL,
                          caption = NULL,
                          horLabel = NULL,
                          verLabel = NULL,
                          horLine = NULL,
                          horLineLabel = " ",
                          verLine = NULL,
                          verLineLabel = " ",
                          labelWrap = 12,
                          colors = NULL,
                          colorScale = 'no',
                          agg = "sum",
                          orientation = "ver",
                          marks = c(".", ","),
                          nDigits = NULL,
                          dropNa = FALSE,
                          highlightValueColor = '#F9B233',
                          percentage = FALSE,
                          format = c('', ''),
                          highlightValue = NULL,
                          order = NULL,
                          sort = "no",
                          sliceN = NULL,
                          showText = TRUE,
                          legendPosition = c("right", "bottom"),
                          tooltip = list(headerFormat = NULL, pointFormat = NULL),
                          export = FALSE,
                          theme = NULL,
                          lang = 'es', ...) {

  nameD <- paste0('Count ', names(data))
  data <- data  %>%
    dplyr::group_by_(names(data)) %>%
    dplyr::summarise(Conteo = n())

  data <- plyr::rename(data, c('Conteo' = nameD))

  h <- hgch_bar_CatNum(data, title = title, subtitle = subtitle, caption = caption, horLabel = horLabel, verLabel = verLabel, horLine = horLine, horLineLabel = horLineLabel, verLine = verLine, verLineLabel = verLineLabel, labelWrap = labelWrap, orientation = orientation, marks = marks, nDigits = nDigits, dropNa = dropNa, highlightValueColor = highlightValueColor, percentage = percentage, colors = colors, colorScale = colorScale, agg = agg, format = format, highlightValue = highlightValue, order = order, sort = sort, sliceN = sliceN,showText=showText,legendPosition = legendPosition, tooltip = tooltip, export = export, theme = theme, lang = lang, ...)
  h
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
#' hgch_bar_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_bar_CatCatNum
hgch_bar_CatCatNum <- function(data,
                               title = NULL,
                               subtitle = NULL,
                               caption = NULL,
                               horLabel = NULL,
                               verLabel = NULL,
                               horLine = NULL,
                               horLineLabel = " ",
                               verLine = NULL,
                               verLineLabel = " ",
                               graphType = "grouped",
                               agg = "sum",
                               colors = c("#009EE3", "#F9B233"),
                               colorScale = 'discrete',
                               dropNaV = c(FALSE, FALSE),
                               format = c("", ""),
                               labelWrapV = c(12, 12),
                               marks = c(".", ","),
                               nDigits = NULL,
                               order1 = NULL,
                               order2 = NULL,
                               orientation = "ver",
                               percentage = FALSE,
                               showText = TRUE,
                               legendPosition = c("right", "bottom"),
                               theme = NULL,
                               tooltip = list("headerFormat" = NULL,
                                              "pointFormat" = NULL,
                                              "shared" = NULL),
                               export = FALSE,
                               lang = 'es', ...) {


  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

  labelsXY <- orientationXY(orientation,
                            x = nms[2],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a) & nrow(d) == dplyr::n_distinct(d$b),
                                       nms[3],
                                       paste(agg, nms[3])),
                            hor = horLabel,
                            ver = verLabel)

  lineXY <- linesOrientation(orientation, horLine, verLine)

  lineLabelsXY <- linesOrLabel(orientation,
                               horLineLabel,
                               verLineLabel)


  if (colorScale == 'discrete') colorDefault <- unname(fillColors(d, "a", c("#74D1F7", "#2E0F35", "#B70F7F", "#C2C4C4", "#8097A4", "#A6CEDE", "#801549", "#FECA84", "#ACD9C2"), colorScale))

    if (!is.null(colors)) {
        colors <- unname(fillColors(d, "a", colors, colorScale))
    } else {
      if (colorScale == 'no') {
        colors <- c("#74D1F7", "#74D1F7")
      } else {
        colors <- colorDefault
      }
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
  d$a[is.na(d$a)] <- "NA"
  d$b[is.na(d$b)] <- "NA"


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

  hc <- highchart() %>%
    hc_chart(type = ifelse(orientation == "hor", "bar", "column")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(
      categories = map(as.character(unique(d$b)), function(z) z),
      title = list(text = labelsXY[1]),
      allowDecimals = FALSE,
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
    hc_legend(enabled = TRUE,
              align = legendPosition[1],
              verticalAlign = legendPosition[2])

  if (graphType == "stacked"){
    hc <- hc %>% hc_plotOptions(bar = list(stacking = "normal"), column = list(stacking = "normal"))
    if (percentage) {
      hc <- hc %>% hc_yAxis(maxRange = 100,
                            max = 100)
    }
  }
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  if (is.null(theme)) {
    hc <- hc %>% hc_add_theme(custom_theme(custom = tma(showText = showText, colores = colors, diffColorsBar = FALSE)))
  } else {
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme))
  }
  hc
}


#' Grouped bar by first category (categories, categories)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat, Cat-Dat, Cat-Yea, Yea-Cat, Yea-Dat, Yea-Yea, Dat-Cat, Dat-Yea, Dat-Dat
#' @examples
#' hgch_bar_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bar_CatCat
hgch_bar_CatCat <-function(data,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           horLabel = NULL,
                           verLabel = NULL,
                           horLine = NULL,
                           horLineLabel = " ",
                           verLine = NULL,
                           verLineLabel = " ",
                           graphType = "grouped",
                           agg = "sum",
                           colors = c("#009EE3", "#F9B233"),
                           colorScale = 'discrete',
                           dropNaV = c(FALSE, FALSE),
                           format = c("", ""),
                           labelWrapV = c(12, 12),
                           marks = c(".", ","),
                           nDigits = NULL,
                           order1 = NULL,
                           order2 = NULL,
                           orientation = "ver",
                           percentage = FALSE,
                           showText = TRUE,
                           legendPosition = c("right", "bottom"),
                           theme = NULL,
                           tooltip = list("headerFormat" = NULL,
                                          "pointFormat" = NULL,
                                          "shared" = NULL),
                           export = FALSE, ...) {

  datN <- names(data)
  data <- data %>%
    dplyr::group_by_(datN[1], datN[2]) %>%
    dplyr::summarise(Conteo = n())

  hgch_bar_CatCatNum(data,title,subtitle,caption,horLabel,verLabel,horLine,horLineLabel,verLine,verLineLabel,graphType,agg,colors,colorScale,dropNaV,format,labelWrapV, marks, nDigits,order1,order2,orientation,percentage,showText,legendPosition,theme,tooltip,export, ...)
}


#' Bar (ordered category, n numbers)
#'
#' Compare n quantities among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-NumP
#' @examples
#' hgch_bar_CatNumP(sampleData("Cat-NumP", nrow = 10))
#' @export hgch_bar_CatNumP

hgch_bar_CatNumP <- function(data,
                             title = NULL,
                             subtitle = NULL,
                             caption = NULL,
                             horLabel = NULL,
                             verLabel = NULL,
                             horLine = NULL,
                             horLineLabel = " ",
                             verLine = NULL,
                             verLineLabel = " ",
                             graphType = "grouped",
                             agg = "sum",
                             colors = c("#009EE3", "#F9B233"),
                             colorScale = 'discrete',
                             dropNaV = c(FALSE, FALSE),
                             format = c("", ""),
                             labelWrapV = c(12, 12),
                             marks = c(".", ","),
                             nDigits = NULL,
                             order1 = NULL,
                             order2 = NULL,
                             orientation = "ver",
                             percentage = FALSE,
                             showText = TRUE,
                             legendPosition = c("right", "bottom"),
                             theme = tma(diffColorsBar = FALSE),
                             tooltip = list("headerFormat" = NULL,
                                            "pointFormat" = NULL,
                                            "shared" = NULL),
                             export = FALSE, ...) {

  data <- data %>% gather("Categories", "Conteo", names(data)[-1])
  hgch_bar_CatCatNum(data,title,subtitle,caption,horLabel,verLabel,horLine,horLineLabel,verLine,verLineLabel,graphType,agg,colors,colorScale,dropNaV,format,labelWrapV, marks, nDigits,order1,order2,orientation,percentage, showText,legendPosition,theme,tooltip,export, ...)
}
