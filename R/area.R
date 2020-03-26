#' Area (categories, numbers)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Num, Yea-Num, Dat-Num,
#' @examples
#' hgch_area_CatNum(sampleData("Cat-Num", nrow = 10))
#' @export hgch_area_CatNum

hgch_area_CatNum <-  function(data = NULL,
                              agg = "sum",
                              agg_text = NULL,
                              allow_point = FALSE,
                              background = "#ffffff",
                              caption = NULL,
                              click_function = NULL,
                              colors = NULL,
                              color_click = NULL,
                              color_hover = NULL,
                              color_scale = "discrete",
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
                              opts = NULL, ...){


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
  d <- orderCategory(d, "a", opts$order, opts$label_wrap)
  d <- sortSlice(d, "b", opts$sort, opts$slice_n)


  d <- d %>% plyr::rename(c('b' = 'y'))
  d$color <- NA

  if (!is.null(opts$highlight_value)) {
    w <- which(d$a %in% opts$highlight_value)
    d$color[w] <- opts$highlight_value_color
  }

  data <- list()
  bla <- purrr::map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "y" = d$y[z],
                            "color" = as.character(d$color[z]))
  })

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
    opts$tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(prefix_agg, ' ' ,nms[2], ': '), opts$prefix,'{point.y}', opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- ""
  }

  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)
  hc <- highchart() %>%
    hc_chart(type = ifelse(opts$spline, "areaspline", "area"),
             inverted = ifelse(opts$orientation == 'ver', FALSE, TRUE)) %>%
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
        fillOpacity = opts$fill_opacity,
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
  optsTheme <- list(showText = opts$text_show, colors = opts$colors,  background = opts$background)
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



#' Area (categories)
#'
#' Compare category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' hgch_area_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_area_Cat

hgch_area_Cat <-  function(data = NULL,
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

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)

  names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label))

  h <- hgch_area_CatNum(data = d, opts = opts, ...)
  h
}

#' Area (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat-Num, Cat-Dat-Num, Cat-Yea-Num, Yea-Cat-Num, Yea-Dat-Num, Yea-Yea-Num, Dat-Cat-Num, Dat-Yea-Num, Dat-Dat-Num
#' @examples
#' hgch_area_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_area_CatCatNum

hgch_area_CatCatNum <- function(data = NULL,
                                agg = "sum",
                                agg_text = NULL,
                                allow_point = FALSE,
                                background = "#ffffff",
                                caption = NULL,
                                click_function = NULL,#JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")
                                colors = NULL,
                                color_click = NULL,
                                color_hover = NULL,
                                color_scale = 'discrete',
                                cursor =  NULL,
                                drop_na =  FALSE,
                                drop_na_legend = FALSE,
                                export = FALSE,
                                fill_opacity = 0.5,
                                graph_type = "grouped",
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
    caption = caption,
    click_function = click_function,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    color_scale = color_scale,
    cursor =  cursor,
    drop_na = drop_na,
    drop_na_legend = drop_na_legend,
    export = export,
    fill_opacity = fill_opacity,
    graph_type = graph_type,
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

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""
  #print(opts$ver_line)

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)
  labelsXY <- orientationXY(opts$orientation,
                            x = nms[2],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a) & nrow(d) == dplyr::n_distinct(d$b),
                                       nms[3],
                                       paste(prefix_agg, nms[3])),
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
    hc_chart(type = ifelse(opts$spline, "areaspline", "area"),
             inverted = ifelse(opts$orientation == 'ver', FALSE, TRUE))
  if (opts$graph_type == "stacked"){
    hc <- hc %>% hc_plotOptions(area = list(stacking = 'normal', fillOpacity = opts$fill_opacity), areaspline = list(stacking = 'normal', fillOpacity = opts$color_opacity))
    if (opts$percentage) {
      hc <- hc %>% hc_yAxis(maxRange = 100,
                            max = 100)
    }
  }
  hc <- hc %>% hc_title(text = title) %>%
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
      series = list(
        fillOpacity = opts$fill_opacity,
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
  optsTheme <- list(showText = opts$text_show, colors = opts$colors,  background = opts$background)
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

#' Area (categories, ordered categories, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat
#' @examples
#' hgch_area_CatCat(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_area_CatCat
hgch_area_CatCat <- function(data = NULL,
                             agg_text = NULL,
                             allow_point = FALSE,
                             background = "#ffffff",
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
                             graph_type = "grouped",
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
    fill_opacity = fill_opacity,
    graph_type = graph_type,
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

  h <- hgch_area_CatCatNum(data = d, opts = opts, ...)
  h
}

#' @export
hgch_area_CatDatNum <- function(data = NULL,
                                agg = "sum",
                                agg_text = NULL,
                                allow_point = FALSE,
                                background = "#ffffff",
                                caption = NULL,
                                click_function = NULL,
                                colors = NULL,
                                color_click = NULL,
                                color_hover = NULL,
                                color_scale = 'discrete',
                                cursor =  NULL,
                                drop_na =  FALSE,
                                drop_na_legend = FALSE,
                                export = FALSE,
                                fill_opacity = 0.5,
                                graph_type = "grouped",
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
    caption = caption,
    click_function = click_function,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    color_scale = color_scale,
    cursor =  cursor,
    drop_na = drop_na,
    drop_na_legend = drop_na_legend,
    export = export,
    fill_opacity = fill_opacity,
    graph_type = graph_type,
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

  d <- d %>% drop_na(b)

  dates <- full_seq(d$b, 1)

  d <- d %>% group_by(a, b) %>%
    summarise(cases = sum(c)) %>%
    ungroup()

  d <- d %>%
    group_by(a) %>%
    mutate(cumcases = cumsum(cases)) %>% ungroup() %>%
    select(-cases)


  d <- d %>%
    complete(b = dates, a, fill = list(cumcases = NA)) %>%
    group_by(a) %>%
    fill(cumcases) %>%
    ungroup() %>%
    mutate(cumcases = ifelse(is.na(cumcases), 0, cumcases)) %>%
    select(2,1,Casos = cumcases)


  names(d) <- names(data)
  hgch_area_CatCatNum(d, opts = opts, ...)

}




#' Area (categories, date, numbers)
#'
#' Compare quantities among two categories
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Dat
#' @examples
#' hgch_area_CatDat(sampleData("Cat-Cat-Num", nrow = 10))
#' @export hgch_area_CatDat
hgch_area_CatDat <- function(data = NULL,
                             agg_text = NULL,
                             allow_point = FALSE,
                             background = "#ffffff",
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
                             graph_type = "grouped",
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
    fill_opacity = fill_opacity,
    graph_type = graph_type,
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

  h <- hgch_area_CatDatNum(data = d, opts = opts, ...)
  h
}
