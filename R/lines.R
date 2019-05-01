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
                              opts = NULL, ...
                              ) {

  if (is.null(data) | nrow(data) == 0) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)
  labelsXY <- orientationXY(opts$orientation,
                            x = nms[1],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(prefix_agg, nms[2])),
                            hor = opts$horLabel,
                            ver = opts$verLabel)
  lineXY <- linesOrientation(opts$orientation, opts$horLine, opts$verLine)

  lineLabelsXY <- linesOrLabel(opts$orientation,
                               opts$horLine_label,
                               opts$verLine_label)

  colorDefault <- "#3DB26F"

    if (!is.null(opts$colors)) {
      opts$colors <- rep(opts$colors[1], length(unique(d$a)))
    } else {
      opts$colors <- rep(colorDefault, length(unique(d$a)))
    }

  if (opts$dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(opts$agg, b))

  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- 'NA'

  if (is.null(opts$nDigits)) {
    nDig <- 0
  } else {
    nDig <- opts$nDigits
  }

  if (opts$percentage) {
    d$b <- (d[['b']] * 100) / sum(d[['b']], na.rm = TRUE)
  }

  d$b <- round(d$b, nDig)
  d <- sortSlice(d, "b", opts$sort, opts$sliceN)
  d <- orderCategory(d, "a", opts$order, opts$labelWrap)


  d <- d %>% plyr::rename(c('b' = 'y'))
  d$color <- NA


  series <- list(list(
    data = map(1:nrow(d), function(x) {
      d$y[x]
    })
  ))


  formatLabAxis <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
  if (!is.null(opts$nDigits)) {
    formatLabAxis <- paste0('{value:', opts$marks[1], opts$marks[2], opts$nDigits, 'f}')
  }


  if (is.null(opts$prefix)) opts$prefix <- ""
  if (is.null(opts$suffix)) opts$suffix <- ""


  aggFormAxis <- 'function() {return this.value+"";}'


  if (opts$percentage & opts$suffix == "") {
    aggFormAxis <- 'function() {return this.value+"%";}'
    opts$suffix <- "%"
  }


  aggFormAxis <- paste0("function() { return '", opts$prefix , "' + Highcharts.numberFormat(this.value, ", nDig, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffix, "'}"
  )


  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <- paste0('<b>{point.category}</b><br/>', paste0(prefix_agg, ' ' ,nms[2], ': '), opts$prefix,'{point.y}', opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- ""
  }

  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)

  hc <- highchart() %>%
    hc_chart(type = ifelse(opts$spline, "spline", "line"),
             inverted = ifelse(opts$orientation == 'ver', FALSE, TRUE)) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
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
      minRange = if (opts$startAtZero) 0.1,
      min = if (opts$startAtZero) 0,
      minPadding = if (opts$startAtZero) 0,
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
    hc_plotOptions(
      series = list(
        allowPointSelect= opts$allow_point,
        cursor =  opts$cursor,
        events = list(
          click = opts$clickFunction
        )
      )) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(enabled = FALSE)

  if (opts$export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}

  if (is.null(opts$theme)) {
    hc <- hc %>% hc_add_theme(tma(custom = list(showText = opts$showText, colors = opts$colors)))
  } else {
    hc <- hc %>% hc_add_theme(opts$theme)
  }

  if (opts$showText) {
    hc <- hc %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(
            format = paste0(opts$prefix, "{y}", opts$suffix)
          ))
      )
  }

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
                           opts = NULL, ...) {

  if (is.null(data) | nrow(data) == 0) {
    stop("Load an available dataset")
  }

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)

  names(d) <- c(f$dic_$d$label, paste0(prefix_agg, f$dic_$d$label))

  h <- hgch_line_CatNum(data = d, opts = opts, ...)
  h
}



#' Line (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Dat-Num, Cat-Yea-Num, Yea-Cat-Num, Yea-Dat-Num, Yea-Yea-Num, Dat-Cat-Num, Dat-Yea-Num, Dat-Dat-Num
#' @examples
#' hgch_line_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_line_CatCatNum
hgch_line_CatCatNum <- function(data,
                                opts = NULL, ...) {


  if (is.null(data) | nrow(data) == 0) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <- opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)
  labelsXY <- orientationXY(opts$orientation,
                            x = nms[1],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$b), nms[3], paste(prefix_agg, nms[3])),
                            hor = opts$horLabel,
                            ver = opts$verLabel)

  lineXY <- linesOrientation(opts$orientation, opts$horLine, opts$verLine)

  lineLabelsXY <- linesOrLabel(opts$orientation,
                               opts$horLine_label,
                               opts$verLine_label)

  if (opts$color_scale == 'discrete') {
    colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
    colorDefault <- discreteColorSelect(colorDefault, d)
  } else if (opts$color_scale == "no"){
    colorDefault <- rep("#3DB26F", length(unique(d$a)))
  } else {
    colorDefault <- leaflet::colorNumeric(c("#53255E", "#ff4097"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }


  if (!is.null(opts$colors)) {
    opts$colors <- unname(fillColors(d, "a", opts$colors, opts$color_scale))
  } else {
    opts$colors <- colorDefault
  }


  if (opts$dropNaV[1])
    d <- d %>%
    tidyr::drop_na(a)

  if(opts$dropNaV[2])
    d <- d %>%
    tidyr::drop_na(b)


  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(opts$agg, c)) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a)
  d$c[is.na(d$c)] <- NA
  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- NA
  d$b <- as.character(d$b)
  d$b[is.na(d$b)] <- NA

  if (is.null(opts$nDigits)) {
    nDig <- 0
  } else {
    nDig <- opts$nDigits
  }

  if (opts$percentage) {
    d <- d %>% group_by(b) %>%
      dplyr::mutate(c = (c / sum(c, na.rm = TRUE)) * 100)
  }


  d <- orderCategory(d, "a", order = opts$order1, labelWrap = opts$labelWrapV[1])
  d <- orderCategory(d, "b", order = opts$order2, labelWrap = opts$labelWrapV[2])
  d$c <- round(d$c, nDig)


  series <- map(unique(d[[1]]), function(i) {
    d0 <- d %>%
      filter(a %in% i)
    l0 <- list("name" = i,
               "data" = d0$c)
  })


  if (opts$percentage & is.null(opts$suffix)) {
    aggFormAxis <- 'function() {return this.value+"%";}'
    opts$suffix <- "%"
  }

  formatLabAxis <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
  if (!is.null(opts$nDigits)) {
    formatLabAxis <- paste0('{value:', opts$marks[1], opts$marks[2], opts$nDigits, 'f}')
  }

  if (is.null(opts$prefix)) opts$prefix <- ""
  if (is.null(opts$suffix)) opts$suffix <- ""


  aggFormAxis <- 'function() {return this.value+"";}'


  aggFormAxis <- paste0("function() { return '", opts$prefix , "' + Highcharts.numberFormat(this.value, ", nDig, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffix, "'}"
  )


  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <-paste0('<b>', nms[2], ': </b>{point.category}</br>',
                                 '<b>', nms[1], ': </b>{series.name}</br>',
                                 paste0(prefix_agg, ' ' ,nms[3], ': '), opts$prefix,'{point.y}', opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- " "
  }


  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)


  hc <- highchart() %>%
    hc_chart(type = ifelse(opts$spline, "spline", "line"),
             inverted = ifelse(opts$orientation == 'ver', FALSE, TRUE)) %>%
    hc_title(text = title) %>%
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
      minRange = if (opts$startAtZero) 0.1,
      min = if (opts$startAtZero) 0,
      minPadding = if (opts$startAtZero) 0,
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
    hc_plotOptions(
      series = list(
        allowPointSelect= opts$allow_point,
        cursor =  opts$cursor,
        events = list(
          click = opts$clickFunction
        )
      )) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(enabled = TRUE, align = opts$legend_position)
  if (opts$export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}
  if (is.null(opts$theme)) {
    hc <- hc %>% hc_add_theme(tma(custom = list(showText = opts$showText, colors = opts$colors)))
  } else {
    hc <- hc %>% hc_add_theme(opts$theme)
  }

  if (opts$showText) {
    hc <- hc %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(
            format = paste0(opts$prefix, "{y}", opts$suffix)
          ))
      )
  }

  hc
}

#' Line (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat
#' @examples
#' hgch_line_CatCat(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_line_CatCat
hgch_line_CatCat <- function(data,
                             opts = NULL, ...) {

  if (is.null(data) | nrow(data) == 0) {
    stop("Load an available dataset")
  }

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(c = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste0(prefix_agg, f$dic_$d$label[1]))

  h <- hgch_line_CatCatNum(data = d, opts = opts, ...)
  h
}




#' Line (ordered category, n numbers)
#'
#' Compare n quantities among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-NumP
#' @examples
#' hgch_line_CatNumP(sampleData("Cat-NumP", nrow = 10))
#' @export hgch_line_CatNumP

hgch_line_CatNumP <- function(data,
                              opts = NULL, ...) {

  if (is.null(data) | nrow(data) == 0) {
    stop("Load an available dataset")
  }

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  names(d) <- f$dic_$d$label

  data <- d %>%
    gather("categories", "count", names(d)[-1])
  h <- hgch_line_CatCatNum(data = data, opts = opts, ...)
  h
}
