#' Scatter Num Num
#'
#' @param data
#'
#' @export
hgch_scatter_NumNum <- function(data = NULL,
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

labelsXY <- orientationXY('ver',
                          x = nms[1],
                          y = nms[2],
                          hor = opts$horLabel,
                          ver = opts$verLabel)
lineXY <- linesOrientation('ver', opts$horLine, opts$verLine)

lineLabelsXY <- linesOrLabel('ver',
                             opts$horLine_label,
                             opts$verLine_label)


if (is.null(opts$colors)) opts$colors <- '#3DB26F'

if (opts$dropNa)
  d <- d %>%
  tidyr::drop_na()

d <- d %>% drop_na()

if (is.null(opts$nDigitsY)) {
  nDigY <- 0
} else {
  nDigY <- opts$nDigitsY
}

if (is.null(opts$nDigitsX)) {
  nDigX <- 0
} else {
  nDigX <- opts$nDigitsX
}


d$a <- round(d$a, nDigX)
d$b <- round(d$b, nDigY)



formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
if (!is.null(opts$nDigitsX)) {
  formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], opts$nDigitsX, 'f}')
}


formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
if (!is.null(opts$nDigitsY)) {
  formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], opts$nDigitsY, 'f}')
}

if (is.null(opts$prefixX)) opts$prefixX <- ""
if (is.null(opts$prefixY)) opts$prefixY <- ""
if (is.null(opts$suffixX)) opts$suffixX <- ""
if (is.null(opts$suffixY)) opts$suffixY <- ""


aggFormAxisX <- paste0("function() { return '", opts$prefixX , "' + Highcharts.numberFormat(this.value, ", nDigX, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffixX, "'}"
)
aggFormAxisY <- paste0("function() { return '", opts$prefixY , "' + Highcharts.numberFormat(this.value, ", nDigY, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffixY, "'}"
)


if (is.null(opts$tooltip$pointFormat)) {
  opts$tooltip$pointFormat <- paste0('<b>', nms[1], ':</b> ', paste0(opts$prefixX,'{point.x}', opts$suffixX), '<br/>','<b>', nms[2], ':</b> ', paste0(opts$prefixY,'{point.y}', opts$suffixY))
}
if (is.null(opts$tooltip$headerFormat)) {
  opts$tooltip$headerFormat <- ""
}

global_options(opts$marks[1], opts$marks[2])
exportLang(language = opts$lang)

data_list <- map(1:nrow(data), function(z) {
  list(d$a[z], d$b[z])
})

hc <- highchart() %>%
  hc_chart(
    type = 'scatter',
    zoomType = 'xy'
  ) %>%
  hc_add_dependency("plugins/highcharts-regression.js") %>%
  hc_title(text = title) %>%
  hc_subtitle(text = subtitle) %>%
  hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
  hc_xAxis(
    title =  list(text = labelsXY[1]),
    labels = list (
      format = formatLabAxisX,
      formatter = JS(aggFormAxisX)
    ),
    plotLines = list(
      list(value = lineXY[1],
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
  ) %>%
  hc_yAxis(
    title = list (
      text = labelsXY[2]),
    plotLines = list(
      list(value = lineXY[2],
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
      format = formatLabAxisY,
      formatter = JS(aggFormAxisY)
    )
  ) %>%
  hc_add_series(
    regression = opts$regression,
    regressionSettings = list(
      color = opts$regression_color,
      hideInLegend = ifelse(opts$regression_equation, FALSE, TRUE)
    ),
    data = data_list,
    showInLegend = F
  ) %>%
  hc_plotOptions(
    series = list(
      marker= list(
        fillOpacity= opts$fill_opacity,
        states = list(
          hover = list(
            fillColor = opts$color_hover
          ),
          select = list(
            fillColor = opts$color_click
          ))
      ),
      allowPointSelect= opts$allow_point,
      cursor =  opts$cursor,
      events = list(
        click = opts$clickFunction
      )
    )) %>%
  hc_credits(enabled = TRUE, text = caption) #%>%
  #hc_legend(enabled = FALSE)

if (opts$export){
  hc <- hc %>%
    hc_exporting(enabled = TRUE, buttons= list(
      contextButton= list(
        menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
      )
    ))}

if (is.null(opts$theme)) {
  hc <- hc %>% hc_add_theme(tma(custom = list(line_width = 0, showText = FALSE, colors = opts$colors)))
} else {
  hc <- hc %>% hc_add_theme(opts$theme)
}

  hc
# if (opts$regression) {
#   mod <- lm(d$b ~ d$a)
#   result <-  predict(mod, d['a'])
#   result <- data.frame(a = d$a, b = as.numeric(result))
# hc %>%
#   hc_add_series(
#     type = 'line',
#     color = 'red',
#     lineWidth = 2,
#     data = map(1:nrow(result), function(z) {
#       list(result$a[z], result$b[z])
#     }),
#     marker = list(
#       enabled = FALSE
#     ),
#     states = list(
#       hover = list(
#         lineWidth = 0
#       )
#     )
#   )
# }
}



#' Scatter Cat Num Num
#'
#' @param data
#'
#' @export
hgch_scatter_CatNumNum <- function(data = NULL,
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

  labelsXY <- orientationXY('ver',
                            x = nms[1],
                            y = nms[2],
                            hor = opts$horLabel,
                            ver = opts$verLabel)
  lineXY <- linesOrientation('ver', opts$horLine, opts$verLine)

  lineLabelsXY <- linesOrLabel('ver',
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

  d <- d %>% drop_na()

  if (is.null(opts$nDigitsY)) {
    nDigY <- 0
  } else {
    nDigY <- opts$nDigitsY
  }

  if (is.null(opts$nDigitsX)) {
    nDigX <- 0
  } else {
    nDigX <- opts$nDigitsX
  }


  d$b <- round(d$b, nDigX)
  d$c <- round(d$c, nDigY)



  formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
  if (!is.null(opts$nDigitsX)) {
    formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], opts$nDigitsX, 'f}')
  }


  formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
  if (!is.null(opts$nDigitsY)) {
    formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], opts$nDigitsY, 'f}')
  }

  if (is.null(opts$prefixX)) opts$prefixX <- ""
  if (is.null(opts$prefixY)) opts$prefixY <- ""
  if (is.null(opts$suffixX)) opts$suffixX <- ""
  if (is.null(opts$suffixY)) opts$suffixY <- ""


  aggFormAxisX <- paste0("function() { return '", opts$prefixX , "' + Highcharts.numberFormat(this.value, ", nDigX, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffixX, "'}"
  )
  aggFormAxisY <- paste0("function() { return '", opts$prefixY , "' + Highcharts.numberFormat(this.value, ", nDigY, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffixY, "'}"
  )


  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <- paste0('<b>{series.name}</b><br/>','<b>', nms[1], ':</b> ', paste0(opts$prefixX,'{point.x}', opts$suffixX), '<br/>','<b>', nms[2], ':</b> ', paste0(opts$prefixY,'{point.y}', opts$suffixY))
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- ""
  }

  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)

  if (!is.null(opts$regression_color)) {
    d_color <- data.frame(a = unique(d$a), color = discreteColorSelect(opts$regression_color, d))
    d <- left_join(d,d_color)
  }


  data_list <- map(as.character(unique(d$a)), function(cat) {
    df <- d %>% filter(a %in% cat)
    list(
      regression = opts$regression,
      regressionSettings = list(
        color = as.character(unique(df$color)),
        hideInLegend = ifelse(opts$regression_equation, FALSE, TRUE)
      ),
      name = cat,
      data = map(1:nrow(df), function(z) {
        list(df$b[z], df$c[z])
      })
    )
  })

  hc <- highchart() %>%
    hc_chart(
      type = 'scatter',
      zoomType = 'xy'
    ) %>%
    hc_add_dependency("plugins/highcharts-regression.js") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
    hc_xAxis(
      title =  list(text = labelsXY[1]),
      labels = list (
        format = formatLabAxisX,
        formatter = JS(aggFormAxisX)
      ),
      plotLines = list(
        list(value = lineXY[1],
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
    ) %>%
    hc_yAxis(
      title = list (
        text = labelsXY[2]),
      plotLines = list(
        list(value = lineXY[2],
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
        format = formatLabAxisY,
        formatter = JS(aggFormAxisY)
      )
    ) %>%
    hc_add_series_list(
      data_list
    ) %>%
    hc_plotOptions(
      series = list(
        marker= list(
          fillOpacity= opts$fill_opacity,
          states = list(
            hover = list(
              fillColor = opts$color_hover
            ),
            select = list(
              fillColor = opts$color_click
            ))
        ),
        allowPointSelect= opts$allow_point,
        cursor =  opts$cursor,
        events = list(
          click = opts$clickFunction
        )
      )) %>%
    hc_credits(enabled = TRUE, text = caption) #%>%
  #hc_legend(enabled = FALSE)

  if (opts$export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}

  if (is.null(opts$theme)) {
    hc <- hc %>% hc_add_theme(tma(custom = list(line_width = 0, showText = FALSE, colors = opts$colors)))
  } else {
    hc <- hc %>% hc_add_theme(opts$theme)
  }

  hc

}


#' Scatter Cat Num Num Num
#'
#' @param data
#'
#' @export
hgch_scatter_CatNumNumNum <- function(data = NULL,
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

  labelsXY <- orientationXY('ver',
                            x = nms[1],
                            y = nms[2],
                            hor = opts$horLabel,
                            ver = opts$verLabel)
  lineXY <- linesOrientation('ver', opts$horLine, opts$verLine)

  lineLabelsXY <- linesOrLabel('ver',
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

  d <- d %>% drop_na()

  if (is.null(opts$nDigitsY)) {
    nDigY <- 0
  } else {
    nDigY <- opts$nDigitsY
  }

  if (is.null(opts$nDigitsX)) {
    nDigX <- 0
  } else {
    nDigX <- opts$nDigitsX
  }

  if (is.null(opts$nDigitsSize)) {
    nDigS <- 0
  } else {
    nDigS <- opts$nDigitsSize
  }

  d$b <- round(d$b, nDigX)
  d$c <- round(d$c, nDigY)
  d$d <- round(d$d, nDigS)


  formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
  if (!is.null(opts$nDigitsX)) {
    formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], opts$nDigitsX, 'f}')
  }


  formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
  if (!is.null(opts$nDigitsY)) {
    formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], opts$nDigitsY, 'f}')
  }

  if (is.null(opts$prefixX)) opts$prefixX <- ""
  if (is.null(opts$prefixY)) opts$prefixY <- ""
  if (is.null(opts$prefixSize)) opts$prefixSize <- ""
  if (is.null(opts$suffixX)) opts$suffixX <- ""
  if (is.null(opts$suffixY)) opts$suffixY <- ""
  if (is.null(opts$suffixSize)) opts$suffixSize <- ""


  aggFormAxisX <- paste0("function() { return '", opts$prefixX , "' + Highcharts.numberFormat(this.value, ", nDigX, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffixX, "'}"
  )
  aggFormAxisY <- paste0("function() { return '", opts$prefixY , "' + Highcharts.numberFormat(this.value, ", nDigY, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffixY, "'}"
  )


  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <- paste0('<b>{series.name}</b><br/><b>', nms[2], ':</b> ', opts$prefixX,'{point.x}', opts$suffixX, '<br/>','<b>', nms[3], ':</b> ', opts$prefixY,'{point.y}', opts$suffixY, '<br/><b>', nms[4], ':</b> ', opts$prefixSize,'{point.z}', opts$suffixSize, '(size)')
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- ""
  }

  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)

  if (!is.null(opts$regression_color)) {
    d_color <- data.frame(a = unique(d$a), color = discreteColorSelect(opts$regression_color, d))
    d <- left_join(d, d_color)
  }


  data_list <- map(as.character(unique(d$a)), function(cat) {
    df <- d %>% filter(a %in% cat)
    list(
      regression = opts$regression,
      regressionSettings = list(
        color = as.character(unique(df$color)),
        hideInLegend = ifelse(opts$regression_equation, FALSE, TRUE)
      ),
      name = cat,
      data = map(1:nrow(df), function(z) {
        list(x = df$b[z], y = df$c[z], z = df$d[z])
      })
    )
  })

  hc <- highchart() %>%
    hc_chart(
      type = 'bubble',
      zoomType = 'xy'
    ) %>%
    hc_add_dependency("plugins/highcharts-regression.js") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
    hc_xAxis(
      title =  list(text = labelsXY[1]),
      labels = list (
        format = formatLabAxisX,
        formatter = JS(aggFormAxisX)
      ),
      plotLines = list(
        list(value = lineXY[1],
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
    ) %>%
    hc_yAxis(
      title = list (
        text = labelsXY[2]),
      plotLines = list(
        list(value = lineXY[2],
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
        format = formatLabAxisY,
        formatter = JS(aggFormAxisY)
      )
    ) %>%
    hc_add_series_list(
      data_list
    ) %>%
    hc_plotOptions(
      series = list(
        marker= list(
          fillOpacity= opts$fill_opacity,
          states = list(
            hover = list(
              fillColor = opts$color_hover
            ),
            select = list(
              fillColor = opts$color_click
            ))
        ),
        allowPointSelect= opts$allow_point,
        cursor =  opts$cursor,
        events = list(
          click = opts$clickFunction
        )
      )) %>%
    hc_credits(enabled = TRUE, text = caption) #%>%
  #hc_legend(enabled = FALSE)

  if (opts$export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}

  if (is.null(opts$theme)) {
    hc <- hc %>% hc_add_theme(tma(custom = list(line_width = 0, showText = FALSE, colors = opts$colors)))
  } else {
    hc <- hc %>% hc_add_theme(opts$theme)
  }

  hc

}
