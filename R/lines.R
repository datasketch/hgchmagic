#' Lines (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Num, Yea-Num, Dat-Num,
#' @examples
#' hgch_line_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_line_CatNum
hgch_line_CatNum <-  function(data,
                              title = NULL,
                              subtitle = NULL,
                              caption = NULL,
                              horLabel = NULL,
                              verLabel = NULL,
                              horLine = NULL,
                              horLineLabel = " ",
                              verLine = NULL,
                              verLineLabel = " ",
                              startAtZero = TRUE,
                              labelWrap = 12,
                              colors = NULL,
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
                              plotBandsFromX = NULL,
                              plotBandsToX = NULL,
                              plotBandsColorX = 'rgba(68, 170, 213, .2)',
                              plotBandsFromY = NULL,
                              plotBandsToY = NULL,
                              plotBandsColorY = 'rgba(68, 170, 213, .2)',
                              tooltip = list(headerFormat = NULL, pointFormat = NULL),
                              export = FALSE,
                              theme = tma(), ...) {


  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  horLabel <- horLabel %||% nms[1]
  verLabel <- verLabel %||% ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(agg, nms[2]))

    colorDefault <- c("#74D1F7", "#2E0F35", "#B70F7F", "#C2C4C4", "#8097A4", "#A6CEDE", "#801549", "#FECA84", "#ACD9C2")


  if (is.null(theme$colors)) {
    if (!is.null(colors)) {
      theme$colors <- unname(fillColors(d, "a", colors, colorScale))
    } else {
        theme$colors <- colorDefault
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

  hc <- highchart() %>%
    hc_chart(type = ifelse(spline, "spline", "line")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>%
    hc_xAxis(
      title =  list(text = horLabel),
      categories = map(as.character(unique(d$a)), function(z) z),
      plotBands = list(
        from = plotBandsFromX,
        to = plotBandsToX,
        color = plotBandsColorX
      ),
      plotLines = list(
        list(value = verLine,
             color = 'black',
             dashStyle = 'shortdash',
             zIndex = 5,
             width = 2,
             label = list(
               text = verLineLabel,
               style = list(
                 color = 'black'
               )
             )))
      #type= 'category'
    ) %>%
    hc_yAxis(
      title = list (
        text = verLabel),
      minRange = if (startAtZero) 0.1,
      min = if (startAtZero) 0,
      minPadding = if (startAtZero) 0,
      plotBands = list(
        from = plotBandsFromY,
        to = plotBandsToY,
        color = plotBandsColorY
      ),
      plotLines = list(
        list(value = horLine,
             color = 'black',
             dashStyle = 'shortdash',
             width = 2,
             zIndex = 5,
             label = list(
               text = horLineLabel,
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
    hc_add_theme(custom_theme(custom = theme)) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(enabled = FALSE)
  if (export) hc <- hc %>%
    hc_exporting(enabled = TRUE)
  hc

}


#' Lines (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' hgch_line_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_line_Cat
hgch_line_Cat <-  function(data,
                          title = NULL,
                          subtitle = NULL,
                          caption = NULL,
                          horLabel = NULL,
                          verLabel = NULL,
                          horLine = NULL,
                          horLineLabel = " ",
                          verLine = NULL,
                          verLineLabel = " ",
                          startAtZero = TRUE,
                          labelWrap = 12,
                          colors = NULL,
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
                          plotBandsFromX = NULL,
                          plotBandsToX = NULL,
                          plotBandsColorX = 'rgba(68, 170, 213, .2)',
                          plotBandsFromY = NULL,
                          plotBandsToY = NULL,
                          plotBandsColorY = 'rgba(68, 170, 213, .2)',
                          tooltip = list(headerFormat = NULL, pointFormat = NULL),
                          export = FALSE,
                          theme = tma(), ...) {


  data <- data  %>%
    dplyr::group_by_(names(data)) %>%
    dplyr::summarise(Conteo = n())

  h <- hgch_line_CatNum(data = data, title = title, subtitle = subtitle, caption = caption, horLabel = horLabel,verLabel = verLabel,horLine = horLine,horLineLabel = horLineLabel,verLine = verLine ,verLineLabel = verLineLabel,startAtZero = startAtZero,labelWrap = labelWrap,colors ,agg = agg,spline = spline,marks = marks,nDigits = nDigits,dropNa = dropNa,percentage = percentage,format = format,order = order,sort = sort,sliceN = sliceN,plotBandsFromX = plotBandsFromX,plotBandsToX = plotBandsToX,plotBandsColorX = plotBandsColorX,plotBandsFromY = plotBandsFromY ,plotBandsToY = plotBandsToY,plotBandsColorY = plotBandsColorY,tooltip = tooltip,export = export,theme = theme, ...)
  h
}


#' Line (years, numbers)
#'
#' Compare quantities over years
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Yea-Num
#' @examples
#' hgch_line_YeaNum(sampleData("Yea-Num", nrow = 10))
#' @export hgch_line_YeaNum
hgch_line_YeaNum <- hgch_line_CatNum
