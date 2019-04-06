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
                             prefix = NULL,
                             suffix = NULL,
                             highlightValue = NULL,
                             order = NULL,
                             sort = "no",
                             sliceN = NULL,
                             showText = FALSE,
                             legendPosition = "center",
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
    colorDefault <- c("#FECA84", "#3DB26F", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
  } else {
    colorDefault <- leaflet::colorNumeric(c("#53255E", "#ff4097"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }


    if (!is.null(colors)) {
      colors <- unname(fillColors(d, "a", colors, colorScale))
    } else {
      if (colorScale == 'no') {
      colors <- c("#FECA84", "#FECA84")
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
    prefix = ""
    suffix = ""
  }

  if (percentage) {
    suffix <- "%"
    legFormat <- paste0("<b>{point.name}</b>: {point.y:",marks[2], nDig,"f}%")
  }


  if (is.null(tooltip$pointFormat)) {
    tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(agg, ' ' ,nms[2], ': '), prefix,'{point.y}', suffix)
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
    #hc_plotOptions(series = list(dataLabels = list( format = legFormat))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>%
    hc_series(
      data
    ) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(align= legendPosition)#,
              #verticalAlign= legendPosition[2])
  if (export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}
  if (is.null(theme)) {
    hc <- hc %>% hc_add_theme(tma(custom = list(showText = showText, colores = colors)))
  } else {
    hc <- hc %>% hc_add_theme(theme)
  }


  if (showText) {
    hc <- hc %>%
      hc_plotOptions(
        pie = list(
          dataLabels = list(
            distance = -30,
            format = paste0(prefix, "{y}", suffix)
          ))
      )
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
                          prefix = NULL,
                          suffix = NULL,
                          highlightValue = NULL,
                          order = NULL,
                          sort = "no",
                          sliceN = NULL,
                          showText = FALSE,
                          legendPosition = "center",
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

  h <- hgch_pie_CatNum(data = d, title = title, subtitle = subtitle, caption = caption,labelWrap = labelWrap, marks = marks, nDigits = nDigits, dropNa = dropNa, highlightValueColor = highlightValueColor, percentage = percentage, colors = colors, colorScale = colorScale, agg = agg, prefix = prefix, suffix = suffix, highlightValue = highlightValue, order = order, sort = sort, sliceN = sliceN, showText = showText, legendPosition = legendPosition, tooltip = tooltip, export = export, lang = lang, theme = theme, ...)
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
                               prefix = NULL,
                               suffix = NULL,
                               highlightValue = NULL,
                               order = NULL,
                               sort = "no",
                               sliceN = NULL,
                               showText = FALSE,
                               legendPosition = "center",
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
    colorDefault <- c("#FECA84", "#3DB26F", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
  } else {
    colorDefault <- leaflet::colorNumeric(c("#53255E", "#ff4097"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }


    if (!is.null(colors)) {
      colors <- unname(fillColors(d, "a", colors, colorScale))
    } else {
      if (colorScale == 'no') {
       colors <- c("#FECA84", "#FECA84")
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
    prefix = ""
    suffix = ""
  }

  if (percentage) {
    suffix <- "%"
    legFormat <- paste0("<b>{point.name}</b>: {point.y:",marks[2], nDig,"f}%")
  }


  if (is.null(tooltip$pointFormat)) {
    tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(agg, ' ' ,nms[2], ': '), prefix,'{point.y}', suffix)
  }
  if (is.null(tooltip$headerFormat)) {
    tooltip$headerFormat <- ""
  }

  global_options(marks[1], marks[2])
  exportLang(language = lang)
#, dataLabels = list( format = legFormat)
  hc <- highchart() %>%
    hc_chart(type = "pie",
             plotBackgroundColor = NULL,
             plotBorderWidth = NULL,
             plotShadow = FALSE) %>%
    hc_plotOptions(series = list(innerSize = "60%")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>%
    hc_series(
      data
    ) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(align= legendPosition)#[1],
              #verticalAlign= legendPosition[2])
  if (export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}
  if (is.null(theme)) {
    hc <- hc %>% hc_add_theme(tma(custom = list(showText = showText, colores = colors)))
  } else {
    hc <- hc %>% hc_add_theme(theme)
  }

  if (showText) {
    hc <- hc %>%
      hc_plotOptions(
        pie = list(
          dataLabels = list(
            distance = -30,
            format = paste0(prefix, "{y}", suffix)
          ))
      )
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
                            prefix = NULL,
                            suffix = NULL,
                            highlightValue = NULL,
                            order = NULL,
                            sort = "no",
                            sliceN = NULL,
                            showText = FALSE,
                            legendPosition = "center",
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

  h <- hgch_donut_CatNum(data = d, title = title, subtitle = subtitle, caption = caption,labelWrap = labelWrap, marks = marks, nDigits = nDigits, dropNa = dropNa, highlightValueColor = highlightValueColor, percentage = percentage, colors = colors, colorScale = colorScale, agg = agg, prefix = prefix, suffix = suffix, highlightValue = highlightValue, order = order, sort = sort, sliceN = sliceN,showText = showText, legendPosition = legendPosition, tooltip = tooltip, export = export, lang = lang, theme = theme, ...)
  h
}
