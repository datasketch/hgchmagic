#' Treemap (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Num, Yea-Num, Dat-Num,
#' @examples
#' hgch_treemap_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_treemap_CatNum

hgch_treemap_CatNum<-  function(data,
                                title = NULL,
                                subtitle = NULL,
                                caption = NULL,
                                labelWrap = 12,
                                colors = NULL,
                                colorScale = 'continuous',
                                agg = "sum",
                                marks = c(".", ","),
                                nDigits = NULL,
                                dropNa = FALSE,
                                highlightValueColor = '#F9B233',
                                percentage = FALSE,
                                format = c('', ''),
                                highlightValue = NULL,
                                sliceN = NULL,
                                showText = TRUE,
                                showLegend = TRUE,
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


  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  if (colorScale == 'discrete') {
    colorDefault <- c("#74D1F7", "#2E0F35", "#B70F7F", "#C2C4C4", "#8097A4", "#A6CEDE", "#801549", "#FECA84", "#ACD9C2")
    colorDefault <- discreteColorSelect(colorDefault, d)
  } else {
    colorDefault <- leaflet::colorNumeric(c("#2E0F35", "#A6CEDE"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }


  if (!is.null(colors)) {
    colors <- unname(fillColors(d, "a", colors, colorScale))
  } else {
    colors <- colorDefault
  }


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
  d <- orderCategory(d, "a", unique(d$a), labelWrap)
  d <- sortSlice(d, "b", "asc", sliceN)


  d <- d %>% plyr::rename(c('b' = 'value'))
  d$color <- colors

  if (!is.null(highlightValue)) {
    w <- which(d$a %in% highlightValue)
    d$color[w] <- highlightValueColor
  }


  data <- map(1:nrow(d), function(z){
    list("name" = d$a[z],
         "value" = d$value[z],
         "color" = as.character(d$color[z]),
         "colorValue" = d$value[z])
  })


  if (is.null(format)) {
    format[1] = ""
    format[2] = ""
  }

  if (percentage && format[2] == "") {
    format[2] <- "%"
  }


  formatText <- JS(paste0("function () {
                return this.point.name + '<br/>' + '",format[1],"' + Highcharts.numberFormat(this.point.value, ", nDig,", '", marks[2], "','", marks[1], "'", ") + '", format[2],"';}"))

  if (is.null(tooltip$pointFormat)) {
    tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(agg, ' ' ,nms[2], ': '), format[1],'{point.value}', format[2])
  }
  if (is.null(tooltip$headerFormat)) {
    tooltip$headerFormat <- ""
  }

  global_options(marks[1], marks[2])
  exportLang(language = lang)
  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>%
    hc_series(
      list(
        type = 'treemap',
        layoutAlgorithm = 'squarified',
        data = data))

  if (colorScale == 'continuous') {
    hc <- hc %>%
      hc_colorAxis(
        minColor = colors[1],
        maxColor = colors[length(d$a)]
      )
  }

  if (showText) {
    hc <- hc %>%
      hc_plotOptions(
        treemap = list(
          dataLabels = list(
            formatter = formatText
          ))
      )
  }
  hc <- hc %>% hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(enabled = showLegend,
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
    hc <- hc %>% hc_add_theme(tma(colores = colors))
  } else {
    hc <- hc %>% hc_add_theme(theme)
  }
  hc
}


#' Treemap (categories)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' hgch_treemap_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_treemap_Cat

hgch_treemap_Cat <-  function(data,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              labelWrap = 12,
                              colors = NULL,
                              colorScale = 'continuous',
                              agg = "sum",
                              marks = c(".", ","),
                              nDigits = NULL,
                              dropNa = FALSE,
                              highlightValueColor = '#F9B233',
                              percentage = FALSE,
                              format = c('', ''),
                              highlightValue = NULL,
                              sliceN = NULL,
                              showText = TRUE,
                              showLegend = TRUE,
                              legendPosition = c("right", "bottom"),
                              tooltip = list(headerFormat = NULL, pointFormat = NULL),
                              export = FALSE,
                              theme = NULL,
                              lang = 'es',...) {

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(c = n())

  names(d) <- c(f$dic_$d$label, paste0("count", f$dic_$d$label[1]))

  h <- hgch_treemap_CatNum(data = d, title = title,subtitle = subtitle, caption = caption, labelWrap = labelWrap,colors = colors,colorScale = colorScale, agg = agg,marks = marks, nDigits = nDigits,dropNa = dropNa, highlightValueColor = highlightValueColor, percentage = percentage, format = format, highlightValue = highlightValue,sliceN = sliceN,showText = showText,showLegend = showLegend, legendPosition = legendPosition,tooltip = tooltip,export = export,theme = theme, lang = lang,...)
  h
}


#' Treemap (categories, categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Yea-Num, Cat-Dat-Num,
#' @examples
#' hgch_treemap_CatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_treemap_CatCatNum

hgch_treemap_CatCatNum <- function(data,
                                    title = NULL,
                                    subtitle = NULL,
                                    caption = NULL,
                                    agg = "sum",
                                    colors = NULL,
                                    colorScale = 'discrete',
                                    dropNaV = c(FALSE, FALSE),
                                    format = c("", ""),
                                    labelWrapV = c(12, 12),
                                    marks = c(".", ","),
                                    nDigits = NULL,
                                    percentage = FALSE,
                                    showText = TRUE,
                                    showLegend = TRUE,
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
    dplyr::summarise(c = agg(agg, c))

  d <- d %>% drop_na(c)

  if (colorScale == 'discrete') {
    colorDefault <- c("#74D1F7", "#2E0F35", "#B70F7F", "#C2C4C4", "#8097A4", "#A6CEDE", "#801549", "#FECA84", "#ACD9C2")
    colorDefault <- discreteColorSelect(colorDefault, d)
  } else {
    colorDefault <- leaflet::colorNumeric(c("#2E0F35", "#A6CEDE"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }


  if (!is.null(colors)) {
    colors <- unname(fillColors(d, "a", colors, colorScale))
  } else {
    colors <- colorDefault
  }


  if (is.null(nDigits)) {
    nDig <- 0
  } else {
    nDig <- nDigits
  }

  if (percentage) {
    d <- d %>% group_by(b) %>%
      dplyr::mutate(c = (c / sum(c, na.rm = TRUE)) * 100)
  }

  d <- orderCategory(d, "a", order = unique(d$a), labelWrap = labelWrapV[1])
  d <- orderCategory(d, "b", order = unique(d$b), labelWrap = labelWrapV[2])
  d$c <- round(d$c, nDig)

  paleta <- data.frame(a = unique(d$a), colors)

  listaId <- map(1:length(paleta$a), function(i) {
    list(
      id = as.character(paleta$a[i]),
      name = as.character(paleta$a[i]),
      color = as.character(paleta$color[i])
    )
  })


  listaMg <- map(1:nrow(d), function(z) {
    list(
      name = d$b[z],
      parent = d$a[z],
      value = d$c[z],
      colorValue = d$c[z]
    )

  })

  data <- c(listaId, listaMg)

  if (is.null(format)) {
    format[1] = ""
    format[2] = ""
  }

  if (percentage && format[2] == "") {
    format[2] <- "%"
  }


  formatText <- JS(paste0("function () {
                return this.point.name + '<br/>' + '",format[1],"' + Highcharts.numberFormat(this.point.value, ", nDig,", '", marks[2], "','", marks[1], "'", ") + '", format[2],"';}"))

  if (is.null(tooltip$pointFormat)) {
    tooltip$pointFormat <-paste0('<b>', nms[2], ': </b>{point.name}</br>',
                                 # '<b>', nms[1], ': </b>{point.node.name}</br>',
                                 paste0(agg, ' ' ,nms[3], ': '), format[1],'{point.value}', format[2])
  }
  if (is.null(tooltip$headerFormat)) {
    tooltip$headerFormat <- " "
  }


  global_options(marks[1], marks[2])
  exportLang(language = lang)
  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>%
    hc_series(
      list(
        type = "treemap",
        layoutAlgorithm = 'squarified',
        alternateStartingDirection = TRUE,
        levels = list(list(
          level = 1,
          layoutAlgorithm = 'sliceAndDice',
          dataLabels = list(
            enabled = TRUE,
            align = 'left',
            verticalAlign = 'top',
            style = list(
              fontSize = '15px',
              fontWeight = 'bold'
            )
          )
        )),
        data = data
      ))

  if (colorScale == 'continuous') {
    hc <- hc %>%
      hc_colorAxis(
        maxColor = as.character(colors$colorDefault[1]),
        minColor = as.character(colors$colorDefault[length(colors$a)])
      )
  }

  if (showText) {
    hc <- hc %>%
      hc_plotOptions(
        treemap = list(
          dataLabels = list(
            formatter = formatText
          ))
      )
  }
  hc <- hc %>% hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(enabled = showLegend,
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
    hc <- hc %>% hc_add_theme(tma(colores = colors))
  } else {
    hc <- hc %>% hc_add_theme(theme)
  }
  hc
}

#' Treemap (categories, categories)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat, Cat-Yea, Cat-Dat,
#' @examples
#' hgch_treemap_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_treemap_CatCat

hgch_treemap_CatCat <- function(data,
                                 title = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 agg = "sum",
                                 colors = NULL,
                                 colorScale = 'discrete',
                                 dropNaV = c(FALSE, FALSE),
                                 format = c("", ""),
                                 labelWrapV = c(12, 12),
                                 marks = c(".", ","),
                                 nDigits = NULL,
                                 percentage = FALSE,
                                 showText = TRUE,
                                 showLegend = TRUE,
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

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(c = n())

  names(d) <- c(f$dic_$d$label, paste0("count", f$dic_$d$label[1]))

  h <- hgch_treemap_CatCatNum(data = d, title = title, subtitle = subtitle,caption = caption,agg = agg,colors = colors,colorScale = colorScale,dropNaV = dropNaV,format = format,labelWrapV = labelWrapV,marks = marks,nDigits = nDigits,percentage = percentage,showText = showText,showLegend = showLegend,legendPosition = legendPosition,theme = theme, tooltip = tooltip, export = export,lang = lang, ...)
  h
}


#' Treemap (categories, categories)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-NumP, Yea-NumP, Dat-NumP
#' @examples
#' hgch_treemap_CatNumP(sampleData("Cat-NumP", nrow = 10))
#' @export hgch_treemap_CatNumP

hgch_treemap_CatNumP <- function(data,
                                  title = NULL,
                                  subtitle = NULL,
                                  caption = NULL,
                                  agg = "sum",
                                  colors = NULL,
                                  colorScale = 'discrete',
                                  dropNaV = c(FALSE, FALSE),
                                  format = c("", ""),
                                  labelWrapV = c(12, 12),
                                  marks = c(".", ","),
                                  nDigits = NULL,
                                  percentage = FALSE,
                                  showText = TRUE,
                                  showLegend = TRUE,
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
  names(d) <- f$dic_$d$label

  data <- d %>%
    gather("categories", "count", names(d)[-1])
  h <-  hgch_treemap_CatCatNum(data = data,title = title, subtitle = subtitle,caption = caption,agg = agg,colors = colors,colorScale = colorScale,dropNaV = dropNaV,format = format,labelWrapV = labelWrapV,marks = marks,nDigits = nDigits,percentage = percentage,showText = showText,showLegend = showLegend,legendPosition = legendPosition,theme = theme, tooltip = tooltip, export = export,lang = lang, ...)
  h
}
