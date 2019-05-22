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
hgch_bar_CatNum <-  function(data = NULL,
                             opts = NULL, ...) {

  if (is.null(data)) {
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
  d <- orderCategory(d, "a", opts$order, opts$labelWrap)
  d <- sortSlice(d, "b", opts$sort, opts$sliceN)


  d <- d %>% plyr::rename(c('b' = 'y'))
  d$color <- NA

  if (!is.null(opts$highlight_value)) {
    w <- which(d$a %in% opts$highlight_value)
    d$color[w] <- opts$highlight_valueColor
  }

  data <- list()
  bla <- purrr::map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "y" = d$y[z],
                            "color" = as.character(d$color[z]))
  })

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
    opts$tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(prefix_agg, ' ' ,nms[2], ': '), opts$prefix,'{point.y}', opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- ""
  }

  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)
  hc <- highchart() %>%
    hc_chart(type = ifelse(opts$orientation == "hor", "bar", "column")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
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
    hc_plotOptions(
      series = list(
        states = list(
          hover = list(
            #//brightness: -0.5,
            color = opts$color_hover
          ),
          select = list(
            color = opts$color_click
          )
        ),
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
        bar = list(
          dataLabels = list(
            format = paste0(opts$prefix, "{y}", opts$suffix)
          )),
        column = list(
          dataLabels = list(
            format = paste0(opts$prefix, "{y}", opts$suffix)
          )
        )
      )
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
                          opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)

  names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label))

  h <- hgch_bar_CatNum(data = d, opts = opts, ...)
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
                               opts = NULL, ...) {

  if (is.null(data)) {
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
                            x = nms[2],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a) & nrow(d) == dplyr::n_distinct(d$b),
                                       nms[3],
                                       paste(prefix_agg, nms[3])),
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
  d$a[is.na(d$a)] <- "NA"
  d$b[is.na(d$b)] <- "NA"


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


  series <- purrr::map(unique(d[[1]]), function(i) {
    d0 <- d %>%
      dplyr::filter(a %in% i)
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
    hc_chart(type = ifelse(opts$orientation == "hor", "bar", "column")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(
      categories = purrr::map(as.character(unique(d$b)), function(z) z),
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
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_plotOptions(
      bar = list(
        colorByPoint = FALSE),
      column = list(
        colorByPoint = FALSE),
      series = list(
        states = list(
          hover = list(
            #//brightness: -0.5,
            color = opts$color_hover
          ),
          select = list(
            color = opts$color_click
          )
        ),
        allowPointSelect= opts$allow_point,
        cursor =  opts$cursor,
        events = list(
          click = opts$clickFunction
        )
      )
    ) %>%
    hc_legend(enabled = TRUE,
              align = opts$legend_position)

  if (opts$graphType == "stacked"){
    hc <- hc %>% hc_plotOptions(bar = list(stacking = "normal"), column = list(stacking = "normal"))
    if (opts$percentage) {
      hc <- hc %>% hc_yAxis(maxRange = 100,
                            max = 100)
    }
  }
  if (opts$export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}
  if (is.null(opts$theme)) {
    hc <- hc %>% hc_add_theme(tma(custom = list(showText = opts$showText, colors = opts$colors, colors_diff= FALSE)))
  } else {
    hc <- hc %>% hc_add_theme(opts$theme)
  }


  if (opts$showText) {
    hc <- hc %>%
      hc_plotOptions(
        bar = list(
          dataLabels = list(
            format = paste0(opts$prefix, "{y}", opts$suffix)
          )),
        column = list(
          dataLabels = list(
            format = paste0(opts$prefix, "{y}", opts$suffix)
          )
        )
      )
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
                           opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(c = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "Count", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label[1]))

  h <- hgch_bar_CatCatNum(data = d, opts = opts, ...)
  h
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
                             opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  names(d) <- f$dic_$d$label

  data <- d %>%
    gather("categories", "count", names(d)[-1])
  h <- hgch_bar_CatCatNum(data,opts = opts, ...)
  h
}
