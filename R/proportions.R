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
                             agg = "sum",
                             agg_text = NULL,
                             allow_point = FALSE,
                             background = "#ffffff",
                             border_color = "#CCCCCC",
                             border_width = 1,
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
                             highlight_value = NULL,
                             highlight_valueColor = '#F9B233',
                             labelWrap = 12,
                             lang = 'es',
                             legend_position  = "center",
                             legend_show = TRUE,
                             marks = c(".", ","),
                             nDigits = NULL,
                             null_color = "#f7f7f7",
                             order = NULL,
                             orientation = "ver",
                             percentage = FALSE,
                             prefix = NULL,
                             text_show = TRUE,
                             sliceN = NULL,
                             sort = "no",
                             subtitle = NULL,
                             suffix = NULL,
                             title = NULL,
                             theme = NULL,
                             tooltip = list(headerFormat = NULL, pointFormat = NULL),
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
    clickFunction = clickFunction,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    color_opacity = color_opacity,
    color_scale = color_scale,
    cursor =  cursor,
    drop_na = drop_na,
    export = export,
    highlight_value = highlight_value,
    highlight_valueColor = highlight_valueColor,
    labelWrap = labelWrap,
    lang = lang,
    legend_position  = legend_position,
    legend_show = legend_show,
    map_navigation = map_navigation,
    marks = marks,
    nDigits = nDigits,
    null_color = null_color,
    order = order,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    text_show = text_show,
    sliceN = sliceN,
    sort = sort,
    subtitle = subtitle,
    suffix = suffix,
    title = title,
    theme = theme,
    tooltip = tooltip
  )

  opts <- modifyList(defaultOptions, opts %||% list())

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)

  if (opts$color_scale == 'discrete') {
    colorDefault <- c("#FECA84", "#3DB26F", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
  } else {
    colorDefault <- leaflet::colorNumeric(c("#53255E", "#ff4097"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }


    if (!is.null(opts$colors)) {
      opts$colors <- unname(fillColors(d, "a", opts$colors, opts$color_scale))
    } else {
      if (opts$color_scale == 'no') {
        opts$colors <- c("#FECA84", "#FECA84")
      } else {
        opts$colors <- colorDefault
      }
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
    w <- which(d$a %in% opts$highlight_vValue)
    d$color[w] <- opts$highlight_valueColor
  }

  data <- list()
  bla <- purrr::map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "y" = d$y[z],
                            "color" = as.character(d$color[z]))
  })

  legFormat <- "<b>{point.name}</b>: {point.y} ({point.percentage:.1f}%)"

  if (is.null(opts$prefix)) opts$prefix <- ""
  if (is.null(opts$suffix)) opts$suffix <- ""


  if (opts$percentage & opts$suffix == "") {
    opts$suffix <- "%"
    legFormat <- paste0("<b>{point.name}</b>: {point.y:",opts$marks[2], opts$nDig,"f}%")
  }


  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(prefix_agg, ' ' ,nms[2], ': '), opts$prefix,'{point.y}', opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- ""
  }

  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)

  hc <- highchart() %>%
    hc_chart(type = "pie",
             plotBackgroundColor = NULL,
             plotBorderWidth = NULL,
             plotShadow = FALSE) %>%
    #hc_plotOptions(series = list(dataLabels = list( format = legFormat))) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
    hc_series(
      data
    ) %>%
    hc_plotOptions(
      pie = list(
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
    hc_legend(align= opts$legend_position)#,
              #verticalAlign= legendPosition[2])
  if (opts$export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}
  if (is.null(opts$theme)) {
    hc <- hc %>% hc_add_theme(tma(custom = list(showText = opts$text_show, colors = opts$colors, background = opts$background)))
  } else {
    hc <- hc %>% hc_add_theme(opts$theme)
  }


  if (opts$text_show) {
    hc <- hc %>%
      hc_plotOptions(
        pie = list(
          dataLabels = list(
            distance = -30,
            format = paste0(opts$prefix, "{y}", opts$suffix)
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
                          agg_text = NULL,
                          allow_point = FALSE,
                          background = "#ffffff",
                          border_color = "#CCCCCC",
                          border_width = 1,
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
                          highlight_value = NULL,
                          highlight_valueColor = '#F9B233',
                          labelWrap = 12,
                          lang = 'es',
                          legend_position  = "center",
                          legend_show = TRUE,
                          marks = c(".", ","),
                          nDigits = NULL,
                          null_color = "#f7f7f7",
                          order = NULL,
                          orientation = "ver",
                          percentage = FALSE,
                          prefix = NULL,
                          text_show = TRUE,
                          sliceN = NULL,
                          sort = "no",
                          subtitle = NULL,
                          suffix = NULL,
                          title = NULL,
                          theme = NULL,
                          tooltip = list(headerFormat = NULL, pointFormat = NULL),
                          opts = NULL, ...) {


  defaultOptions <- list(
    agg_text = agg_text,
    allow_point = allow_point,
    background = background,
    border_color = border_color,
    border_width = border_width,
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
    highlight_value = highlight_value,
    highlight_valueColor = highlight_valueColor,
    labelWrap = labelWrap,
    lang = lang,
    legend_position  = legend_position,
    legend_show = legend_show,
    map_navigation = map_navigation,
    marks = marks,
    nDigits = nDigits,
    null_color = null_color,
    order = order,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    text_show = text_show,
    sliceN = sliceN,
    sort = sort,
    subtitle = subtitle,
    suffix = suffix,
    title = title,
    theme = theme,
    tooltip = tooltip
  )

  opts <- modifyList(defaultOptions, opts %||% list())

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste0(prefix_agg, f$dic_$d$label))

  h <- hgch_pie_CatNum(data = d, opts = opts, ...)
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
                               agg = "sum",
                               agg_text = NULL,
                               allow_point = FALSE,
                               background = "#ffffff",
                               border_color = "#CCCCCC",
                               border_width = 1,
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
                               highlight_value = NULL,
                               highlight_valueColor = '#F9B233',
                               labelWrap = 12,
                               lang = 'es',
                               legend_position  = "center",
                               legend_show = TRUE,
                               marks = c(".", ","),
                               nDigits = NULL,
                               null_color = "#f7f7f7",
                               order = NULL,
                               orientation = "ver",
                               percentage = FALSE,
                               prefix = NULL,
                               text_show = TRUE,
                               sliceN = NULL,
                               sort = "no",
                               subtitle = NULL,
                               suffix = NULL,
                               title = NULL,
                               theme = NULL,
                               tooltip = list(headerFormat = NULL, pointFormat = NULL),
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
    clickFunction = clickFunction,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    color_opacity = color_opacity,
    color_scale = color_scale,
    cursor =  cursor,
    drop_na = drop_na,
    export = export,
    highlight_value = highlight_value,
    highlight_valueColor = highlight_valueColor,
    labelWrap = labelWrap,
    lang = lang,
    legend_position  = legend_position,
    legend_show = legend_show,
    map_navigation = map_navigation,
    marks = marks,
    nDigits = nDigits,
    null_color = null_color,
    order = order,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    text_show = text_show,
    sliceN = sliceN,
    sort = sort,
    subtitle = subtitle,
    suffix = suffix,
    title = title,
    theme = theme,
    tooltip = tooltip
  )

  opts <- modifyList(defaultOptions, opts %||% list())

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <- opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)

  if (opts$color_scale == 'discrete') {
    colorDefault <- c("#FECA84", "#3DB26F", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
  } else {
    colorDefault <- leaflet::colorNumeric(c("#53255E", "#ff4097"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }


    if (!is.null(opts$colors)) {
      opts$colors <- unname(fillColors(d, "a", opts$colors, opts$color_scale))
    } else {
      if (opts$color_scale == 'no') {
        opts$colors <- c("#FECA84", "#FECA84")
      } else {
        opts$colors <- colorDefault
      }
    }

  if (opts$dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d %>%
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

  legFormat <- "<b>{point.name}</b>: {point.y} ({point.percentage:.1f}%)"

  if (is.null(opts$prefix)) opts$prefix <- ""
  if (is.null(opts$suffix)) opts$suffix <- ""

  if (opts$percentage & opts$suffix == "") {
    opts$suffix <- "%"
    legFormat <- paste0("<b>{point.name}</b>: {point.y:", opts$marks[2], nDig,"f}%")
  }


  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(prefix_agg, ' ' ,nms[2], ': '), opts$prefix,'{point.y}', opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- ""
  }

  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)
#, dataLabels = list( format = legFormat)
  hc <- highchart() %>%
    hc_chart(type = "pie",
             plotBackgroundColor = NULL,
             plotBorderWidth = NULL,
             plotShadow = FALSE) %>%
    hc_plotOptions(
      series = list(innerSize = "60%"),
      pie = list(
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
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
    hc_series(
      data
    ) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(align= opts$legend_position)#[1],
              #verticalAlign= legendPosition[2])
  if (opts$export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}
  if (is.null(opts$theme)) {
    hc <- hc %>% hc_add_theme(tma(custom = list(showText = opts$text_show, colores = opts$colors, background = opts$background)))
  } else {
    hc <- hc %>% hc_add_theme(opts$theme)
  }

  if (opts$text_show) {
    hc <- hc %>%
      hc_plotOptions(
        pie = list(
          dataLabels = list(
            distance = -30,
            format = paste0(opts$prefix, "{y}", opts$suffix)
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
                            agg_text = NULL,
                            allow_point = FALSE,
                            background = "#ffffff",
                            border_color = "#CCCCCC",
                            border_width = 1,
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
                            highlight_value = NULL,
                            highlight_valueColor = '#F9B233',
                            labelWrap = 12,
                            lang = 'es',
                            legend_position  = "center",
                            legend_show = TRUE,
                            marks = c(".", ","),
                            nDigits = NULL,
                            null_color = "#f7f7f7",
                            order = NULL,
                            orientation = "ver",
                            percentage = FALSE,
                            prefix = NULL,
                            text_show = TRUE,
                            sliceN = NULL,
                            sort = "no",
                            subtitle = NULL,
                            suffix = NULL,
                            title = NULL,
                            theme = NULL,
                            tooltip = list(headerFormat = NULL, pointFormat = NULL),
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
    clickFunction = clickFunction,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    color_opacity = color_opacity,
    color_scale = color_scale,
    cursor =  cursor,
    drop_na = drop_na,
    export = export,
    highlight_value = highlight_value,
    highlight_valueColor = highlight_valueColor,
    labelWrap = labelWrap,
    lang = lang,
    legend_position  = legend_position,
    legend_show = legend_show,
    map_navigation = map_navigation,
    marks = marks,
    nDigits = nDigits,
    null_color = null_color,
    order = order,
    orientation = orientation,
    percentage = percentage,
    prefix = prefix,
    text_show = text_show,
    sliceN = sliceN,
    sort = sort,
    subtitle = subtitle,
    suffix = suffix,
    title = title,
    theme = theme,
    tooltip = tooltip
  )

  opts <- modifyList(defaultOptions, opts %||% list())
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste0(prefix_agg, f$dic_$d$label))

  h <- hgch_donut_CatNum(data = d, opts = opts, ...)
}
