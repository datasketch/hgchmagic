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

hgch_treemap_CatNum <-  function(data,
                                 agg = "sum",
                                 agg_text = NULL,
                                 background = "#ffffff",
                                 border_color = "#CCCCCC",
                                 border_width = 1,
                                 caption = NULL,
                                 clickFunction = NULL,#JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")
                                 colors = NULL,
                                 color_click = NULL,
                                 color_hover = NULL,
                                 color_scale = 'discrete',
                                 cursor =  NULL,
                                 drop_na = FALSE,
                                 export = FALSE,
                                 highlight_value = NULL,
                                 highlight_value_color = '#F9B233',
                                 label_wrap = 12,
                                 lang = 'es',
                                 marks = c(".", ","),
                                 n_digits = NULL,
                                 order = NULL,
                                 percentage = FALSE,
                                 prefix = NULL,
                                 slice_n = NULL,
                                 sort = "no",
                                 subtitle = NULL,
                                 suffix = NULL,
                                 text_show = TRUE,
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
    background = background,
    border_color = border_color,
    border_width = border_width,
    caption = caption,
    clickFunction = clickFunction,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    color_scale = color_scale,
    cursor =  cursor,
    drop_na = drop_na,
    export = export,
    highlight_value = highlight_value,
    highlight_value_color = highlight_value_color,
    label_wrap = label_wrap,
    lang = lang,
    marks = marks,
    n_digits = n_digits,
    order = order,
    percentage = percentage,
    prefix = prefix,
    text_show = text_show,
    slice_n = slice_n,
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

  if (opts$drop_na)
    d <- d %>%
    tidyr::drop_na()

  if (opts$color_scale == 'discrete') {
    colorDefault <- c("#74D1F7", "#2E0F35", "#B70F7F", "#C2C4C4", "#8097A4", "#A6CEDE", "#801549", "#FECA84", "#ACD9C2")
    colorDefault <- discreteColorSelect(colorDefault, d)
  } else {
    colorDefault <- leaflet::colorNumeric(c("#2E0F35", "#A6CEDE"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }

  colores_treemap <- opts$colors
  if (!is.null(opts$theme$colors)) colores_treemap <- opts$theme$colors

  if (!is.null(colores_treemap)) {
    opts$colors <- unname(fillColors(d, "a",colores_treemap, opts$color_scale))
  } else {
    opts$colors <- colorDefault
  }
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
  d <- orderCategory(d, "a", unique(d$a), label_wrap = opts$label_wrap)
  d <- sortSlice(d, "b", "asc", opts$slice_n)


  d <- d %>% plyr::rename(c('b' = 'value'))
  d$color <- opts$colors

  if (!is.null(opts$highlight_value)) {
    w <- which(d$a %in% opts$highlight_value)
    d$color[w] <- opts$highlight_value_color
  }


  data <- purrr::map(1:nrow(d), function(z){
    list("name" = d$a[z],
         "value" = d$value[z],
         "color" = as.character(d$color[z]),
         "colorValue" = d$value[z])
  })


  if (is.null(opts$prefix)) opts$prefix <- ""
  if (is.null(opts$suffix)) opts$suffix <- ""


  if (opts$percentage && opts$suffix == "") {
    opts$suffix <- "%"
  }


  formatText <- JS(paste0("function () {
                return this.point.name + '<br/>' + '", opts$prefix,"' + Highcharts.numberFormat(this.point.value, ", nDig,", '", opts$marks[2], "','", opts$marks[1], "'", ") + '", opts$suffix,"';}"))

  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(prefix_agg, ' ' ,nms[2], ': '), opts$prefix,'{point.value}', opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- ""
  }

  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)
  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
    hc_series(
      list(
        type = 'treemap',
        layoutAlgorithm = 'squarified',
        data = data)) %>%
    hc_plotOptions(
      series = list(
        states = list(
          hover = list(
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
        ))
    )

  if (opts$color_scale == 'continuous') {
    hc <- hc %>%
      hc_colorAxis(
        minColor = colors[1],
        maxColor = colors[length(d$a)]
      )
  }

  if (opts$text_show) {
    hc <- hc %>%
      hc_plotOptions(
        treemap = list(
          dataLabels = list(
            formatter = formatText
          ))
      )
  }
  hc <- hc %>% hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(enabled = opts$legend_show,
              align= opts$legend_position)
  if (opts$export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}


  theme_user <- opts$theme
  optsTheme <- list(background = opts$background)
  themeCustom <- modifyList(optsTheme, theme_user %||% list())
  hc <- hc %>% hc_add_theme(tma(custom = themeCustom ))


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
                              agg_text = NULL,
                              background = "#ffffff",
                              caption = NULL,
                              clickFunction = NULL,#JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")
                              colors = NULL,
                              color_click = NULL,
                              color_hover = NULL,
                              color_scale = 'discrete',
                              cursor =  NULL,
                              drop_na = FALSE,
                              export = FALSE,
                              highlight_value = NULL,
                              highlight_value_color = '#F9B233',
                              label_wrap = 12,
                              lang = 'es',
                              marks = c(".", ","),
                              n_digits = NULL,
                              order = NULL,
                              percentage = FALSE,
                              prefix = NULL,
                              slice_n = NULL,
                              sort = "no",
                              subtitle = NULL,
                              suffix = NULL,
                              title = NULL,
                              text_show = TRUE,
                              theme = NULL,
                              tooltip = list(headerFormat = NULL, pointFormat = NULL),
                              opts = NULL,...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }
  defaultOptions <- list(
    agg_text = agg_text,
    background = background,
    caption = caption,
    clickFunction = clickFunction,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    color_scale = color_scale,
    cursor =  cursor,
    drop_na = drop_na,
    export = export,
    highlight_value = highlight_value,
    highlight_value_color = highlight_value_color,
    label_wrap = label_wrap,
    lang = lang,
    marks = marks,
    n_digits = n_digits,
    order = order,
    percentage = percentage,
    prefix = prefix,
    text_show = text_show,
    slice_n = slice_n,
    sort = sort,
    subtitle = subtitle,
    suffix = suffix,
    text_show = text_show,
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
    dplyr::summarise(c = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste0(prefix_agg, f$dic_$d$label[1]))

  h <- hgch_treemap_CatNum(data = d, opts = opts, ...)
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
                                   agg = "sum",
                                   agg_text = NULL,
                                   background = "#ffffff",
                                   caption = NULL,
                                   clickFunction = NULL,#JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")
                                   colors = NULL,
                                   color_click = NULL,
                                   color_hover = NULL,
                                   color_scale = 'discrete',
                                   cursor =  NULL,
                                   drop_na = FALSE,
                                   drop_na_legend = FALSE,
                                   export = FALSE,
                                   highlight_value = NULL,
                                   highlight_value_color = '#F9B233',
                                   label_wrap = 12,
                                   label_wrap_legend = 12,
                                   lang = 'es',
                                   legend_position  = "center",
                                   legend_show = TRUE,
                                   marks = c(".", ","),
                                   n_digits = NULL,
                                   order1 = NULL,
                                   order2 = NULL,
                                   percentage = FALSE,
                                   prefix = NULL,
                                   text_show = TRUE,
                                   slice_n = NULL,
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
    background = background,
    caption = caption,
    clickFunction = clickFunction,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    color_scale = color_scale,
    cursor =  cursor,
    drop_na = drop_na,
    drop_na_legend = drop_na_legend,
    export = export,
    highlight_value = highlight_value,
    highlight_value_color = highlight_value_color,
    label_wrap = label_wrap,
    label_wrap_legend = label_wrap_legend,
    lang = lang,
    legend_position  = legend_position,
    legend_show = legend_show,
    marks = marks,
    n_digits = n_digits,
    order1 = order1,
    order2 = order2,
    percentage = percentage,
    prefix = prefix,
    text_show = text_show,
    slice_n = slice_n,
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
    dplyr::summarise(c = agg(opts$agg, c))

  d <- d %>% drop_na(c)
  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)

  if (opts$color_scale == 'discrete') {
    colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
    colorDefault <- discreteColorSelect(colorDefault, d)
  } else {
    colorDefault <- leaflet::colorNumeric(c("#2E0F35", "#A6CEDE"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }

  colores_treemap <- opts$colors
  if (!is.null(opts$theme$colors)) colores_treemap <- opts$theme$colors

  if (!is.null(colores_treemap)) {
    opts$colors <- unname(fillColors(d, "a",colores_treemap, opts$color_scale))
  } else {
    opts$colors <- colorDefault
  }


  if (is.null(opts$n_digits)) {
    nDig <- 0
  } else {
    nDig <- opts$n_digits
  }

  if (opts$percentage) {
    d <- d %>% group_by(b) %>%
      dplyr::mutate(c = (c / sum(c, na.rm = TRUE)) * 100)
  }

  d <- orderCategory(d, "a", order = unique(d$a), label_wrap = opts$label_wrap_legend)
  d <- orderCategory(d, "b", order = unique(d$b), label_wrap = opts$label_wrap)
  d$c <- round(d$c, nDig)

  paleta <- data.frame(a = unique(d$a), color = opts$colors)

  listaId <- map(1:length(paleta$a), function(i) {
    list(
      id = as.character(paleta$a[i]),
      name = as.character(paleta$a[i]),
      color = as.character(paleta$color[i])
    )
  })


  listaMg <- map(1:nrow(d), function(z) {
    nm <- ifelse(is.na(d$b[z]), "NA", d$b[z])
    list(
      name = nm,
      parent = d$a[z],
      value = d$c[z],
      colorValue = d$c[z]
    )

  })

  data <- c(listaId, listaMg)

  if (is.null(opts$prefix)) opts$prefix <- ""
  if (is.null(opts$suffix)) opts$suffix <- ""


  if (opts$percentage && opts$suffix == "") {
    opts$suffix <- "%"
  }


  formatText <- JS(paste0("function () {
                return this.point.name + '<br/>' + '", opts$prefix,"' + Highcharts.numberFormat(this.point.value, ", nDig,", '", opts$marks[2], "','", opts$marks[1], "'", ") + '", opts$suffix,"';}"))

  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <-paste0('<b>', nms[2], ': </b>{point.name}</br>',
                                 # '<b>', nms[1], ': </b>{point.node.name}</br>',
                                 paste0(prefix_agg, ' ' ,nms[3], ': '), opts$prefix,'{point.value}', opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- " "
  }


  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)
  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
    hc_series(
      list(
        type = "treemap",
        #layoutAlgorithm = 'squarified',
        #alternateStartingDirection = TRUE,
        levels = list(list(
          level = 1,
          #layoutAlgorithm = 'sliceAndDice',
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

  if (opts$color_scale == 'continuous') {
    hc <- hc %>%
      hc_colorAxis(
        maxColor = as.character(colorDefault[1]),
        minColor = as.character(colorDefault[length(colors$a)])
      )
  }

  if (opts$text_show) {
    hc <- hc %>%
      hc_plotOptions(
        treemap = list(
          dataLabels = list(
            formatter = formatText
          ))
      )
  }
  hc <- hc %>% hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(enabled = opts$legend_show,
              align= opts$legend_position)
  if (opts$export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}

  theme_user <- opts$theme
  optsTheme <- list( background = opts$background)
  themeCustom <- modifyList(optsTheme, theme_user %||% list())
  hc <- hc %>% hc_add_theme(tma(custom = themeCustom ))

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
                                agg_text = NULL,
                                allow_point = FALSE,
                                background = "#ffffff",
                                caption = NULL,
                                clickFunction = NULL,#JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")
                                colors = NULL,
                                color_click = NULL,
                                color_hover = NULL,
                                color_scale = 'discrete',
                                cursor =  NULL,
                                drop_na = FALSE,
                                drop_na_legend = FALSE,
                                export = FALSE,
                                highlight_value = NULL,
                                highlight_value_color = '#F9B233',
                                label_wrap = 12,
                                label_wrap_legend = 12,
                                lang = 'es',
                                legend_position  = "center",
                                legend_show = TRUE,
                                marks = c(".", ","),
                                n_digits = NULL,
                                order1 = NULL,
                                order2 = NULL,
                                percentage = FALSE,
                                prefix = NULL,
                                text_show = TRUE,
                                slice_n = NULL,
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
    caption = caption,
    clickFunction = clickFunction,
    colors = colors,
    color_click = color_click,
    color_hover = color_hover,
    color_scale = color_scale,
    cursor =  cursor,
    drop_na = drop_na,
    drop_na_legend = drop_na_legend,
    export = export,
    highlight_value = highlight_value,
    highlight_value_color = highlight_value_color,
    label_wrap = label_wrap,
    label_wrap_legend = label_wrap_legend,
    lang = lang,
    legend_position  = legend_position,
    legend_show = legend_show,
    marks = marks,
    n_digits = n_digits,
    order1 = order1,
    order2 = order2,
    percentage = percentage,
    prefix = prefix,
    text_show = text_show,
    slice_n = slice_n,
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
    dplyr::summarise(c = n())

  names(d) <- c(f$dic_$d$label, paste0("count", f$dic_$d$label[1]))

  h <- hgch_treemap_CatCatNum(data = d, opts = opts)
  h
}


