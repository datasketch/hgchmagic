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
                              agg = "sum",
                              agg_text = NULL,
                              allow_point = FALSE,
                              background = "#ffffff",
                              caption = NULL,
                              click_function = NULL,
                              colors = NULL,
                              color_click = NULL,
                              color_hover = NULL,
                              cursor =  NULL,
                              drop_na = FALSE,
                              export = FALSE,
                              hor_label = NULL,
                              hor_line = NULL,
                              hor_line_label = " ",
                              label_wrap = 12,
                              lang = 'es',
                              marks = c(".", ","),
                              n_digits = NULL,
                              order = NULL,
                              orientation = "ver",
                              percentage = FALSE,
                              prefix = NULL,
                              text_show = TRUE,
                              slice_n = NULL,
                              sort = "no",
                              spline = FALSE,
                              start_zero = TRUE,
                              subtitle = NULL,
                              suffix = NULL,
                              title = NULL,
                              theme = NULL,
                              tooltip = list(headerFormat = NULL, pointFormat = NULL),
                              ver_label = NULL,
                              ver_line = NULL,
                              ver_line_label = " ",
                              opts = NULL, ...
                              ) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  defaultOptions <- list(
    agg = agg,
    agg_text = agg_text,
    allow_point = allow_point,
    background = background,
    caption = caption,
    click_function = click_function,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    cursor =  cursor,
    drop_na = drop_na,
    export = export,
    hor_label = hor_label,
    hor_line = hor_line,
    hor_line_label = hor_line_label,
    label_wrap = label_wrap,
    lang = lang,
    marks = marks,
    n_digits = n_digits,
    order = order,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    text_show = text_show,
    slice_n = slice_n,
    sort = sort,
    spline = spline,
    start_zero = start_zero,
    subtitle = subtitle,
    suffix = suffix,
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

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)
  labelsXY <- orientationXY(opts$orientation,
                            x = nms[1],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(prefix_agg, nms[2])),
                            hor = opts$hor_label,
                            ver = opts$ver_label)


  line_h <- ifelse(as.character(opts$hor_line), NULL, opts$hor_line)
  line_v <- ifelse(as.character(opts$ver_line), NULL, opts$ver_line)
  lineXY <- linesOrientation(opts$orientation, line_h, line_v)

  lineLabelsXY <- linesOrLabel(opts$orientation,
                               opts$hor_line_label,
                               opts$ver_line_label)

  colorDefault <- "#3DB26F"

    if (!is.null(opts$colors)) {
      opts$colors <- rep(opts$colors[1], length(unique(d$a)))
    } else {
      opts$colors <- rep(colorDefault, length(unique(d$a)))
    }

  if (opts$drop_na)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(opts$agg, b))

  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- 'NA'

  if (is.null(opts$n_digits)) {
    nDig <- 0
  } else {
    nDig <- opts$n_digits
  }

  if (opts$percentage) {
    d$b <- (d[['b']] * 100) / sum(d[['b']], na.rm = TRUE)
  }

  d$b <- round(d$b, nDig)
  d <- sortSlice(d, "b", opts$sort, opts$slice_n)
  d <- orderCategory(d, "a", opts$order, opts$label_wrap)


  d <- d %>% plyr::rename(c('b' = 'y'))
  d$color <- NA


  series <- list(list(
    data = purrr::map(1:nrow(d), function(x) {
      d$y[x]
    })
  ))


  formatLabAxis <- paste0('{value:', opts$marks[1], opts$marks[2], 'f}')
  if (!is.null(opts$n_digits)) {
    formatLabAxis <- paste0('{value:', opts$marks[1], opts$marks[2], opts$n_digits, 'f}')
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
      categories = purrr::map(as.character(unique(d$a)), function(z) z),

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
      minRange = if (opts$start_zero) 0.1,
      min = if (opts$start_zero) 0,
      minPadding = if (opts$start_zero) 0,
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
        marker= list(
        states = list(
          hover = list(
            fillColor = opts$color_hover
          ),
          select = list(
            fillColor = opts$color_click
          )
        )),
        allowPointSelect= opts$allow_point,
        cursor =  opts$cursor,
        events = list(
          click = opts$click_function
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


  theme_user <- opts$theme
  optsTheme <- list(showText = opts$text_show, colors = opts$colors, background = opts$background)
  themeCustom <- modifyList(optsTheme, theme_user %||% list())
  hc <- hc %>% hc_add_theme(tma(custom = themeCustom))

  if (opts$text_show) {
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
                           agg_text = NULL,
                           allow_point = FALSE,
                           background = "#ffffff",
                           caption = NULL,
                           click_function = NULL,
                           colors = NULL,
                           color_click = NULL,
                           color_hover = NULL,
                           cursor =  NULL,
                           drop_na = FALSE,
                           export = FALSE,
                           hor_label = NULL,
                           hor_line = NULL,
                           hor_line_label = " ",
                           label_wrap = 12,
                           lang = 'es',
                           marks = c(".", ","),
                           n_digits = NULL,
                           order = NULL,
                           orientation = "ver",
                           percentage = FALSE,
                           prefix = NULL,
                           text_show = TRUE,
                           slice_n = NULL,
                           sort = "no",
                           spline = FALSE,
                           start_zero = TRUE,
                           subtitle = NULL,
                           suffix = NULL,
                           title = NULL,
                           theme = NULL,
                           tooltip = list(headerFormat = NULL, pointFormat = NULL),
                           ver_label = NULL,
                           ver_line = NULL,
                           ver_line_label = " ",
                           opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = n())

  defaultOptions <- list(
    agg_text = agg_text,
    allow_point = allow_point,
    background = background,
    caption = caption,
    click_function = click_function,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    cursor =  cursor,
    drop_na = drop_na,
    export = export,
    hor_label = hor_label,
    hor_line = hor_line,
    hor_line_label = hor_line_label,
    label_wrap = label_wrap,
    lang = lang,
    marks = marks,
    n_digits = n_digits,
    order = order,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    text_show = text_show,
    slice_n = slice_n,
    sort = sort,
    spline = spline,
    start_zero = start_zero,
    subtitle = subtitle,
    suffix = suffix,
    title = title,
    theme = theme,
    tooltip = tooltip,
    ver_label = ver_label,
    ver_line = ver_line,
    ver_line_label = ver_line_label
  )

  opts <- modifyList(defaultOptions, opts %||% list())

  prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)

  names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label))
  opts$agg_text <- ''

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
                                agg = "sum",
                                agg_text = NULL,
                                allow_point = FALSE,
                                background = "#ffffff",
                                border_color = "#CCCCCC",
                                border_width = 1,
                                caption = NULL,
                                click_function = NULL,#JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")
                                colors = NULL,
                                color_click = NULL,
                                color_hover = NULL,
                                color_opacity = 0.7,
                                color_scale = 'discrete',
                                cursor =  NULL,
                                drop_na = FALSE,
                                drop_na_legend = FALSE,
                                export = FALSE,
                                highlight_value = NULL,
                                highlight_value_color = '#F9B233',
                                hor_label = NULL,
                                hor_line = NULL,
                                hor_line_label = " ",
                                label_wrap = 12,
                                label_wrap_legend = 12,
                                lang = 'es',
                                legend_position  = "center",
                                legend_show = TRUE,
                                marks = c(".", ","),
                                n_digits = NULL,
                                null_color = "#f7f7f7",
                                order1 = NULL,
                                order2 = NULL,
                                orientation = "ver",
                                percentage = FALSE,
                                prefix = NULL,
                                text_show = TRUE,
                                slice_n = NULL,
                                sort = "no",
                                spline = FALSE,
                                start_zero = TRUE,
                                subtitle = NULL,
                                suffix = NULL,
                                title = NULL,
                                theme = NULL,
                                tooltip = list(headerFormat = NULL, pointFormat = NULL),
                                ver_label = NULL,
                                ver_line = NULL,
                                ver_line_label = " ",
                                opts = NULL, ...) {


  if (is.null(data)) {
    stop("Load an available dataset")
  }

  defaultOptions <- list(
    agg = agg,
    agg_text = agg_text,
    allow_point = allow_point,
    background = background,
    border_color = border_color,
    border_width = border_width,
    caption = caption,
    click_function = click_function,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    color_opacity = color_opacity,
    color_scale = color_scale,
    cursor =  cursor,
    drop_na = drop_na,
    drop_na_legend = drop_na_legend,
    export = export,
    highlight_value = highlight_value,
    highlight_value_color = highlight_value_color,
    hor_label = hor_label,
    hor_line = hor_line,
    hor_line_label = hor_line_label,
    label_wrap = label_wrap,
    label_wrap_legend = label_wrap_legend,
    lang = lang,
    legend_position  = legend_position,
    legend_show = legend_show,
    marks = marks,
    n_digits = n_digits,
    null_color = null_color,
    order1 = order1,
    order2 = order2,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    text_show = text_show,
    slice_n = slice_n,
    sort = sort,
    spline = spline,
    start_zero = start_zero,
    subtitle = subtitle,
    suffix = suffix,
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

  title <- opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)
  labelsXY <- orientationXY(opts$orientation,
                            x = nms[2],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$b), nms[3], paste(prefix_agg, nms[3])),
                            hor = opts$hor_label,
                            ver = opts$ver_label)


  line_h <- ifelse(as.character(opts$hor_line), NULL, opts$hor_line)
  line_v <- ifelse(as.character(opts$ver_line), NULL, opts$ver_line)
  lineXY <- linesOrientation(opts$orientation, line_h, line_v)

  lineLabelsXY <- linesOrLabel(opts$orientation,
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


  if (opts$drop_na)
    d <- d %>%
    tidyr::drop_na(b)

  if(opts$drop_na_legend)
    d <- d %>%
    tidyr::drop_na(a)


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

  if (is.null(opts$n_digits)) {
    nDig <- 0
  } else {
    nDig <- opts$n_digits
  }

  if (opts$percentage) {
    d <- d %>% group_by(b) %>%
      dplyr::mutate(c = (c / sum(c, na.rm = TRUE)) * 100)
  }


  d <- orderCategory(d, "a", order = opts$order1, label_wrap = opts$label_wrap_legend)
  d <- orderCategory(d, "b", order = opts$order2, label_wrap = opts$label_wrap)
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
  if (!is.null(opts$n_digits)) {
    formatLabAxis <- paste0('{value:', opts$marks[1], opts$marks[2], opts$n_digits, 'f}')
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
      categories = purrr::map(as.character(unique(d$b)), function(z) z),
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
      minRange = if (opts$start_zero) 0.1,
      min = if (opts$start_zero) 0,
      minPadding = if (opts$start_zero) 0,
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
        marker = list(
        states = list(
          hover = list(
            fillColor = opts$color_hover
          ),
          select = list(
            fillColor = opts$color_click
          )
        )),
        allowPointSelect= opts$allow_point,
        cursor =  opts$cursor,
        events = list(
          click = opts$click_function
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

  theme_user <- opts$theme
  optsTheme <- list(showText = opts$text_show, colors = opts$colors, background = opts$background)
  themeCustom <- modifyList(optsTheme, theme_user %||% list())
  hc <- hc %>% hc_add_theme(tma(custom = themeCustom))


  if (opts$text_show) {
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
                             agg_text = NULL,
                             allow_point = FALSE,
                             background = "#ffffff",
                             border_color = "#CCCCCC",
                             border_width = 1,
                             caption = NULL,
                             click_function = NULL,
                             colors = NULL,
                             color_click = NULL,
                             color_hover = NULL,
                             color_opacity = 0.7,
                             color_scale = 'discrete',
                             cursor =  NULL,
                             drop_na = FALSE,
                             drop_na_legend = FALSE,
                             export = FALSE,
                             fill_opacity = 0.5,
                             highlight_value = NULL,
                             highlight_value_color = '#F9B233',
                             hor_label = NULL,
                             hor_line = NULL,
                             hor_line_label = " ",
                             label_wrap = 12,
                             label_wrap_legend = 12,
                             lang = 'es',
                             legend_position  = "center",
                             legend_show = TRUE,
                             marks = c(".", ","),
                             n_digits = NULL,
                             null_color = "#f7f7f7",
                             order1 = NULL,
                             order2 = NULL,
                             orientation = "ver",
                             percentage = FALSE,
                             prefix = NULL,
                             text_show = TRUE,
                             slice_n = NULL,
                             sort = "no",
                             spline = FALSE,
                             start_zero = TRUE,
                             subtitle = NULL,
                             suffix = NULL,
                             title = NULL,
                             theme = NULL,
                             tooltip = list(headerFormat = NULL, pointFormat = NULL),
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
    border_color = border_color,
    border_width = border_width,
    caption = caption,
    click_function = click_function,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    color_opacity = color_opacity,
    color_scale = color_scale,
    cursor =  cursor,
    drop_na = drop_na,
    drop_na_legend = drop_na_legend,
    export = export,
    highlight_value = highlight_value,
    highlight_value_color = highlight_value_color,
    hor_label = hor_label,
    hor_line = hor_line,
    hor_line_label = hor_line_label,
    label_wrap = label_wrap,
    label_wrap_legend = label_wrap_legend,
    lang = lang,
    legend_position  = legend_position,
    legend_show = legend_show,
    marks = marks,
    n_digits = n_digits,
    null_color = null_color,
    order1 = order1,
    order2 = order2,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    text_show = text_show,
    slice_n = slice_n,
    sort = sort,
    spline = spline,
    start_zero = start_zero,
    subtitle = subtitle,
    suffix = suffix,
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

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(c = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste0(prefix_agg, f$dic_$d$label[1]))

  h <- hgch_line_CatCatNum(data = d, opts = opts, ...)
  h
}


