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
hgch_pie_CatNum <-  function(data,
                             title = NULL,
                             subtitle = NULL,
                             caption = NULL,
                             labelWrap = 12,
                             colors = NULL,
                             colorScale = 'discrete',
                             agg = "sum",
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
                             lang = 'es',
                             theme = NULL, ...) {


  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

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

  legFormat <- "<b>{point.name}</b>: {point.y} ({point.percentage:.1f}%)"

    if (is.null(format)) {
    format[1] = ""
    format[2] = ""
  }

  if (percentage) {
    format[2] <- "%"
    legFormat <- paste0("<b>{point.name}</b>: {point.y:",marks[2], nDig,"f}%")
  }


  if (is.null(tooltip$pointFormat)) {
    tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(agg, ' ' ,nms[2], ': '), format[1],'{point.y}', format[2])
  }
  if (is.null(tooltip$headerFormat)) {
    tooltip$headerFormat <- ""
  }

  global_options(marks[1], marks[2])
  exportLang(language = lang)

  hc <- highchart() %>%
    hc_chart(type = "pie",
             plotBackgroundColor = NULL,
             plotBorderWidth = NULL,
             plotShadow = FALSE) %>%
    hc_plotOptions(series = list(dataLabels = list( format = legFormat))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>%
    hc_series(
      data
    ) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(align= legendPosition[1],
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

#' pie (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' hgch_pie_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_pie_Cat
hgch_pie_Cat <-  function(data,
                          title = NULL,
                          subtitle = NULL,
                          caption = NULL,
                          labelWrap = 12,
                          colors = NULL,
                          colorScale = 'discrete',
                          agg = "sum",
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
                          lang = 'es',
                          theme = NULL, ...) {

  nameD <- paste0('Count ', names(data))
  data <- data  %>%
    dplyr::group_by_(names(data)) %>%
    dplyr::summarise(Conteo = n())

  data <- plyr::rename(data, c('Conteo' = nameD))

  h <- hgch_pie_CatNum(data, title = title, subtitle = subtitle, caption = caption,labelWrap = labelWrap, marks = marks, nDigits = nDigits, dropNa = dropNa, highlightValueColor = highlightValueColor, percentage = percentage, colors = colors, colorScale = colorScale, agg = agg, format = format, highlightValue = highlightValue, order = order, sort = sort, sliceN = sliceN, showText = showText, legendPosition = legendPosition, tooltip = tooltip, export = export, lang = lang, theme = theme, ...)
  h
}


#' donut (categories, numbers)
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
hgch_donut_CatNum <-  function(data,
                               title = NULL,
                               subtitle = NULL,
                               caption = NULL,
                               labelWrap = 12,
                               colors = NULL,
                               colorScale = 'discrete',
                               agg = "sum",
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
                               lang = 'es',
                               theme = NULL, ...) {


  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""

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

  legFormat <- "<b>{point.name}</b>: {point.y} ({point.percentage:.1f}%)"

  if (is.null(format)) {
    format[1] = ""
    format[2] = ""
  }

  if (percentage) {
    format[2] <- "%"
    legFormat <- paste0("<b>{point.name}</b>: {point.y:",marks[2], nDig,"f}%")
  }


  if (is.null(tooltip$pointFormat)) {
    tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(agg, ' ' ,nms[2], ': '), format[1],'{point.y}', format[2])
  }
  if (is.null(tooltip$headerFormat)) {
    tooltip$headerFormat <- ""
  }

  global_options(marks[1], marks[2])
  exportLang(language = lang)

  hc <- highchart() %>%
    hc_chart(type = "pie",
             plotBackgroundColor = NULL,
             plotBorderWidth = NULL,
             plotShadow = FALSE) %>%
    hc_plotOptions(series = list(innerSize = "60%", dataLabels = list( format = legFormat))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>%
    hc_series(
      data
    ) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(align= legendPosition[1],
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


#' donut (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' hgch_donut_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_donut_Cat
hgch_donut_Cat <-  function(data,
                            title = NULL,
                            subtitle = NULL,
                            caption = NULL,
                            labelWrap = 12,
                            colors = NULL,
                            colorScale = 'discrete',
                            agg = "sum",
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
                            lang = 'es',
                            theme = NULL, ...) {

  nameD <- paste0('Count ', names(data))
  data <- data  %>%
    dplyr::group_by_(names(data)) %>%
    dplyr::summarise(Conteo = n())

  data <- plyr::rename(data, c('Conteo' = nameD))

  h <- hgch_donut_CatNum(data, title = title, subtitle = subtitle, caption = caption,labelWrap = labelWrap, marks = marks, nDigits = nDigits, dropNa = dropNa, highlightValueColor = highlightValueColor, percentage = percentage, colors = colors, colorScale = colorScale, agg = agg, format = format, highlightValue = highlightValue, order = order, sort = sort, sliceN = sliceN,showText = showText, legendPosition = legendPosition, tooltip = tooltip, export = export, lang = lang, theme = theme, ...)
  h
}
