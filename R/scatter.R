#' Scatter Num Num
#'
#' @param data
#'
#' @export
hgch_scatter_NumNum <- function(data = NULL,
                                agg_text = NULL,
                                allow_point = FALSE,
                                background = "#ffffff",
                                border_color = "#CCCCCC",
                                border_width = 1,
                                bubble_min = '3%',
                                bubble_max = '12%',
                                caption = NULL,
                                clickFunction = NULL,
                                colors = NULL,
                                color_click = NULL,
                                color_hover = NULL,
                                cursor =  NULL,
                                drop_na = FALSE,
                                export = FALSE,
                                fill_opacity = 0.5,
                                hor_label = NULL,
                                hor_line = NULL,
                                hor_line_label = " ",
                                lang = 'es',
                                marks = c(".", ","),
                                n_digits_y = NULL,
                                n_digits_x = NULL,
                                percentage = FALSE,
                                prefix_x = NULL,
                                prefix_y = NULL,
                                regression = FALSE,
                                regression_color = '#d35400',
                                regression_equation = TRUE,
                                text_show = TRUE,
                                subtitle = NULL,
                                suffix = NULL,
                                suffix_x = NULL,
                                suffix_y = NULL,
                                title = NULL,
                                theme = NULL,
                                tooltip = NULL,
                                ver_label = NULL,
                                ver_line = NULL,
                                ver_line_label = " ",
                                opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  defaultOptions <- list(
    agg_text = agg_text,
    allow_point = allow_point,
    background = background,
    caption = caption,
    clickFunction = clickFunction,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    cursor =  cursor,
    drop_na = drop_na,
    export = export,
    fill_opacity = fill_opacity,
    hor_label = hor_label,
    hor_line = hor_line,
    hor_line_label = hor_line_label,
    lang = lang,
    marks = marks,
    n_digits_y = n_digits_y,
    n_digits_x = n_digits_x,
    percentage = percentage,
    prefix_x = prefix_x,
    prefix_y = prefix_y,
    regression = regression,
    regression_color = regression_color,
    regression_equation = regression_equation,
    text_show = text_show,
    spline = spline,
    subtitle = subtitle,
    suffix_x = suffix_x,
    suffix_y = suffix_y,
    title = title,
    theme = theme,
    tooltip = tooltip,
    ver_label = ver_label,
    ver_line = ver_line,
    ver_line_label = ver_line_label
  )

  opts <- modifyList(defaultOptions, opts %||% list())

f <- fringe(data)
nms <- getClabels(f)
d <- f$d

title <-  opts$title %||% ""
subtitle <- opts$subtitle %||% ""
caption <- opts$caption %||% ""

labelsXY <- orientationXY('ver',
                          x = nms[1],
                          y = nms[2],
                          hor = opts$hor_label,
                          ver = opts$ver_label)
lineXY <- linesOrientation('ver', opts$hor_line, opts$ver_line)

lineLabelsXY <- linesOrLabel('ver',
                             opts$hor_line_label,
                             opts$ver_line_label)


if (is.null(opts$colors)) opts$colors <- '#3DB26F'

d <- d %>% drop_na()

if (is.null(opts$n_digits_y)) {
  nDigY <- 0
} else {
  nDigY <- opts$n_digits_y
}

if (is.null(opts$n_digits_x)) {
  nDigX <- 0
} else {
  nDigX <- opts$n_digits_x
}


d$a <- round(d$a, nDigX)
d$b <- round(d$b, nDigY)



formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
if (!is.null(opts$n_digits_x)) {
  formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], opts$n_digits_x, 'f}')
}


formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
if (!is.null(opts$n_digits_y)) {
  formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], opts$n_digits_y, 'f}')
}

if (is.null(opts$prefix_x)) opts$prefix_x <- ""
if (is.null(opts$prefix_y)) opts$prefix_y <- ""
if (is.null(opts$suffix_x)) opts$suffix_x <- ""
if (is.null(opts$suffix_y)) opts$suffix_y <- ""


aggFormAxisX <- paste0("function() { return '", opts$prefix_x , "' + Highcharts.numberFormat(this.value, ", nDigX, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffix_x, "'}"
)
aggFormAxisY <- paste0("function() { return '", opts$prefix_y , "' + Highcharts.numberFormat(this.value, ", nDigY, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffix_y, "'}"
)




global_options(opts$marks[1], opts$marks[2])
exportLang(language = opts$lang)
if (is.null(opts$tooltip)) {
  opts$tooltip <- paste0('<b>', nms[1], ':</b> ', paste0(opts$prefix_x,'{point.x}', opts$suffix_x), '<br/>','<b>', nms[2], ':</b> ', paste0(opts$prefix_y,'{point.y}', opts$suffix_y))
}

data_list <- map(1:nrow(d), function(z) {
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
  hc_tooltip(useHTML=TRUE,
             pointFormat = opts$tooltip, headerFormat = NULL) %>%
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

#' Scatter Num Num Num
#'
#' @param data
#'
#' @export
hgch_scatter_NumNumNum <- function(data = NULL,
                                   agg = "sum",
                                   agg_text = NULL,
                                   allow_point = FALSE,
                                   background = "#ffffff",
                                   caption = NULL,
                                   clickFunction = NULL,
                                   colors = NULL,
                                   color_click = NULL,
                                   color_hover = NULL,
                                   color_opacity = 0.7,
                                   color_scale = 'discrete',
                                   cursor =  NULL,
                                   drop_na = FALSE,
                                   export = FALSE,
                                   fill_opacity = 0.5,
                                   highlight_value = NULL,
                                   highlight_value_color = '#F9B233',
                                   hor_label = NULL,
                                   hor_line = NULL,
                                   hor_line_label = " ",
                                   label_wrap = 12,
                                   lang = 'es',
                                   legend_position  = "center",
                                   legend_show = TRUE,
                                   marks = c(".", ","),
                                   n_digits = NULL,
                                   n_digits_size = NULL,
                                   n_digits_y = NULL,
                                   n_digits_x = NULL,
                                   orientation = "ver",
                                   percentage = FALSE,
                                   prefix = NULL,
                                   prefix_size = NULL,
                                   prefix_x = NULL,
                                   prefix_y = NULL,
                                   regression = FALSE,
                                   regression_color = '#d35400',
                                   regression_equation = TRUE,
                                   text_show = TRUE,
                                   slice_n = NULL,
                                   sort = "no",
                                   spline = FALSE,
                                   subtitle = NULL,
                                   suffix = NULL,
                                   suffix_x = NULL,
                                   suffix_y = NULL,
                                   suffix_size = NULL,
                                   title = NULL,
                                   theme = NULL,
                                   tooltip = NULL,
                                   ver_label = NULL,
                                   ver_line = NULL,
                                   ver_line_label = " ",
                                   opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  defaultOptions <- list(
    agg_text = agg_text,
    allow_point = allow_point,
    background = background,
    caption = caption,
    clickFunction = clickFunction,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    color_opacity = color_opacity,
    color_scale = color_scale,
    cursor =  cursor,
    drop_na = drop_na,
    export = export,
    fill_opacity = fill_opacity,
    highlight_value = highlight_value,
    highlight_value_color = highlight_value_color,
    hor_label = hor_label,
    hor_line = hor_line,
    hor_line_label = hor_line_label,
    label_wrap = label_wrap,
    lang = lang,
    legend_position  = legend_position,
    legend_show = legend_show,
    marks = marks,
    n_digits = n_digits,
    n_digits_size = n_digits_size,
    n_digits_y = n_digits_y,
    n_digits_x = n_digits_x,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    prefix_size = prefix_size,
    prefix_x = prefix_x,
    prefix_y = prefix_y,
    regression = regression,
    regression_color = regression_color,
    regression_equation = regression_equation,
    text_show = text_show,
    spline = spline,
    subtitle = subtitle,
    suffix = suffix,
    suffix_x = suffix_x,
    suffix_y = suffix_y,
    suffix_size = suffix_size,
    title = title,
    theme = theme,
    tooltip = tooltip,
    ver_label = ver_label,
    ver_line = ver_line,
    ver_line_label = ver_line_label
  )

  opts <- modifyList(defaultOptions, opts %||% list())

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""

  labelsXY <- orientationXY('ver',
                            x = nms[1],
                            y = nms[2],
                            hor = opts$hor_label,
                            ver = opts$ver_label)
  lineXY <- linesOrientation('ver', opts$hor_line, opts$ver_line)

  lineLabelsXY <- linesOrLabel('ver',
                               opts$hor_line_label,
                               opts$ver_line_label)


  if (opts$color_scale == 'discrete') {
    colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
    colorDefault <- discreteColorSelect(colorDefault, d)
  } else if (opts$color_scale == "no"){
    colorDefault <- rep("#3DB26F", length(unique(d$a)))
  } else {
    colorDefault <- leaflet::colorNumeric(c("#53255E", "#ff4097"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }


  if (!is.null(opts$colors)) {
    opts$colors <- opts$colors
  } else {
    opts$colors <- colorDefault
  }


  d <- d %>% drop_na()

  if (is.null(opts$n_digits_y)) {
    nDigY <- 0
  } else {
    nDigY <- opts$n_digits_y
  }

  if (is.null(opts$n_digits_x)) {
    nDigX <- 0
  } else {
    nDigX <- opts$n_digits_x
  }

  if (is.null(opts$n_digits_size)) {
    nDigS <- 0
  } else {
    nDigS <- opts$n_digits_size
  }

  d$a <- round(d$a, nDigX)
  d$b <- round(d$b, nDigY)
  d$c <- round(d$c, nDigS)


  formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
  if (!is.null(opts$n_digits_x)) {
    formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], opts$n_digits_x, 'f}')
  }


  formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
  if (!is.null(opts$n_digits_y)) {
    formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], opts$n_digits_y, 'f}')
  }

  if (is.null(opts$prefix_x)) opts$prefix_x <- ""
  if (is.null(opts$prefix_y)) opts$prefix_y <- ""
  if (is.null(opts$prefix_size)) opts$prefix_size <- ""
  if (is.null(opts$suffix_x)) opts$suffix_x <- ""
  if (is.null(opts$suffix_y)) opts$suffix_y <- ""
  if (is.null(opts$suffix_size)) opts$suffix_size <- ""


  aggFormAxisX <- paste0("function() { return '", opts$prefix_x , "' + Highcharts.numberFormat(this.value, ", nDigX, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffix_x, "'}"
  )
  aggFormAxisY <- paste0("function() { return '", opts$prefix_y , "' + Highcharts.numberFormat(this.value, ", nDigY, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffix_y, "'}"
  )


  if (is.null(opts$tooltip)) {
    opts$tooltip <- paste0('<b>',nms[1], ': </b>', opts$prefix_x,'{point.x}', opts$suffix_x, '<br/>','<b>', nms[2], ':</b> ', opts$prefix_y,'{point.y}', opts$suffix_y, '<br/><b>', nms[3], ':</b> ', opts$prefix_size,'{point.z}', opts$suffix_size, '(size)')
  }

  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)

  if (!is.null(opts$regression_color)) {
    d$color <- opts$regression_color
  }


  data_list <- map(1:nrow(d), function(z) {
    list(x = d$a[z], y = d$b[z], z = d$c[z])
  })

  hc <- highchart() %>%
    hc_chart(
      type = 'bubble',
      zoomType = 'xy'
    ) %>%
    hc_add_dependency("plugins/highcharts-regression.js") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip, headerFormat = NULL) %>%
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

}

#' Scatter Cat Num Num
#'
#' @param data
#'
#' @export
hgch_scatter_CatNumNum <- function(data = NULL,    agg = "sum",
                                   agg_text = NULL,
                                   allow_point = FALSE,
                                   background = "#ffffff",
                                   caption = NULL,
                                   clickFunction = NULL,
                                   colors = NULL,
                                   color_click = NULL,
                                   color_hover = NULL,
                                   color_opacity = 0.7,
                                   color_scale = 'discrete',
                                   cursor =  NULL,
                                   drop_na = FALSE,
                                   export = FALSE,
                                   fill_opacity = 0.5,
                                   highlight_value = NULL,
                                   highlight_value_color = '#F9B233',
                                   hor_label = NULL,
                                   hor_line = NULL,
                                   hor_line_label = " ",
                                   label_wrap = 12,
                                   lang = 'es',
                                   legend_position  = "center",
                                   legend_show = TRUE,
                                   marks = c(".", ","),
                                   n_digits = NULL,
                                   n_digits_size = NULL,
                                   n_digits_y = NULL,
                                   n_digits_x = NULL,
                                   orientation = "ver",
                                   percentage = FALSE,
                                   prefix = NULL,
                                   prefix_size = NULL,
                                   prefix_x = NULL,
                                   prefix_y = NULL,
                                   regression = FALSE,
                                   regression_color = '#d35400',
                                   regression_equation = TRUE,
                                   text_show = TRUE,
                                   slice_n = NULL,
                                   sort = "no",
                                   spline = FALSE,
                                   subtitle = NULL,
                                   suffix = NULL,
                                   suffix_x = NULL,
                                   suffix_y = NULL,
                                   suffix_size = NULL,
                                   title = NULL,
                                   theme = NULL,
                                   tooltip = NULL,
                                   ver_label = NULL,
                                   ver_line = NULL,
                                   ver_line_label = " ",
                                   opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  defaultOptions <- list(
    agg_text = agg_text,
    allow_point = allow_point,
    background = background,
    caption = caption,
    clickFunction = clickFunction,#JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    color_opacity = color_opacity,
    color_scale = color_scale,
    cursor =  cursor,
    drop_na = drop_na,
    export = export,
    fill_opacity = fill_opacity,
    highlight_value = highlight_value,
    highlight_value_color = highlight_value_color,
    hor_label = hor_label,
    hor_line = hor_line,
    hor_line_label = hor_line_label,
    label_wrap = label_wrap,
    lang = lang,
    legend_position  = legend_position,
    legend_show = legend_show,
    marks = marks,
    n_digits = n_digits,
    n_digits_size = n_digits_size,
    n_digits_y = n_digits_y,
    n_digits_x = n_digits_x,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    prefix_size = prefix_size,
    prefix_x = prefix_x,
    prefix_y = prefix_y,
    regression = regression,
    regression_color = regression_color,
    regression_equation = regression_equation,
    text_show = text_show,
    spline = spline,
    subtitle = subtitle,
    suffix = suffix,
    suffix_x = suffix_x,
    suffix_y = suffix_y,
    suffix_size = suffix_size,
    title = title,
    theme = theme,
    tooltip = tooltip,
    ver_label = ver_label,
    ver_line = ver_line,
    ver_line_label = ver_line_label
  )

  opts <- modifyList(defaultOptions, opts %||% list())

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""

  labelsXY <- orientationXY('ver',
                            x = nms[1],
                            y = nms[2],
                            hor = opts$hor_label,
                            ver = opts$ver_label)
  lineXY <- linesOrientation('ver', opts$hor_line, opts$ver_line)

  lineLabelsXY <- linesOrLabel('ver',
                               opts$hor_line_label,
                               opts$ver_line_label)


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

  if (is.null(opts$n_digits_y)) {
    nDigY <- 0
  } else {
    nDigY <- opts$n_digits_y
  }

  if (is.null(opts$n_digits_x)) {
    nDigX <- 0
  } else {
    nDigX <- opts$n_digits_x
  }


  d$b <- round(d$b, nDigX)
  d$c <- round(d$c, nDigY)



  formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
  if (!is.null(opts$n_digits_x)) {
    formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], opts$n_digits_x, 'f}')
  }


  formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
  if (!is.null(opts$n_digits_y)) {
    formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], opts$n_digits_y, 'f}')
  }

  if (is.null(opts$prefix_x)) opts$prefix_x <- ""
  if (is.null(opts$prefix_y)) opts$prefix_y <- ""
  if (is.null(opts$suffix_x)) opts$suffix_x <- ""
  if (is.null(opts$suffix_y)) opts$suffix_y <- ""


  aggFormAxisX <- paste0("function() { return '", opts$prefix_x , "' + Highcharts.numberFormat(this.value, ", nDigX, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffix_x, "'}"
  )
  aggFormAxisY <- paste0("function() { return '", opts$prefix_y , "' + Highcharts.numberFormat(this.value, ", nDigY, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffix_y, "'}"
  )


  if (is.null(opts$tooltip)) {
    opts$tooltip <- paste0('<b>{series.name}</b><br/>','<b>', nms[1], ':</b> ', paste0(opts$prefix_x,'{point.x}', opts$suffix_x), '<br/>','<b>', nms[2], ':</b> ', paste0(opts$prefix_y,'{point.y}', opts$suffix_y))
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
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip, headerFormat = NULL) %>%
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
                            hor = opts$hor_label,
                            ver = opts$ver_label)
  lineXY <- linesOrientation('ver', opts$hor_line, opts$ver_line)

  lineLabelsXY <- linesOrLabel('ver',
                               opts$hor_line_label,
                               opts$ver_line_label)


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

  if (is.null(opts$n_digits_y)) {
    nDigY <- 0
  } else {
    nDigY <- opts$n_digits_y
  }

  if (is.null(opts$n_digits_x)) {
    nDigX <- 0
  } else {
    nDigX <- opts$n_digits_x
  }

  if (is.null(opts$n_digits_size)) {
    nDigS <- 0
  } else {
    nDigS <- opts$n_digits_size
  }

  d$b <- round(d$b, nDigX)
  d$c <- round(d$c, nDigY)
  d$d <- round(d$d, nDigS)


  formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
  if (!is.null(opts$n_digits_x)) {
    formatLabAxisX <- paste0('{value:', opts$marks[1], opts$marks[2], opts$n_digits_x, 'f}')
  }


  formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
  if (!is.null(opts$n_digits_y)) {
    formatLabAxisY <- paste0('{value:', opts$marks[1], opts$marks[2], opts$n_digits_y, 'f}')
  }

  if (is.null(opts$prefix_x)) opts$prefix_x <- ""
  if (is.null(opts$prefix_y)) opts$prefix_y <- ""
  if (is.null(opts$prefix_size)) opts$prefix_size <- ""
  if (is.null(opts$suffix_x)) opts$suffix_x <- ""
  if (is.null(opts$suffix_y)) opts$suffix_y <- ""
  if (is.null(opts$suffix_size)) opts$suffix_size <- ""


  aggFormAxisX <- paste0("function() { return '", opts$prefix_x , "' + Highcharts.numberFormat(this.value, ", nDigX, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffix_x, "'}"
  )
  aggFormAxisY <- paste0("function() { return '", opts$prefix_y , "' + Highcharts.numberFormat(this.value, ", nDigY, ", '", opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffix_y, "'}"
  )


  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <- paste0('<b>{series.name}</b><br/><b>', nms[2], ':</b> ', opts$prefix_x,'{point.x}', opts$suffix_x, '<br/>','<b>', nms[3], ':</b> ', opts$prefix_y,'{point.y}', opts$suffix_y, '<br/><b>', nms[4], ':</b> ', opts$prefix_size,'{point.z}', opts$suffix_size, '(size)')
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
