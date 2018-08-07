# #
# #
# # colores <- if (length(colores == 1)) {
# #   colores <- c(colores, "#ffffff")
# #   ylOrBn <- colorRampPalette(colores)
# #   ylOrBn(nColores)
# # } else {
# #   ylOrBn <- colorRampPalette(colores)
# #   c(colores, ylOrBn(nColores)[-1])
# # }
#
# library(highcharter)
# library(datafringe)
# library(dichromat)
#
# custom_theme  <- function(custom = NULL,...){
#   if (!is.null(custom)) {
#     theme <- custom
#   }
#   theme
# }
#
# global_options <- function(marksMil){
#
#   hcoptslang <- getOption("highcharter.lang")
#   hcoptslang$thousandsSep <- marksMil
#
#   options(highcharter.lang = hcoptslang)
# }
#
#
# tma <- function(background = 'transparent',
#                 bordercolor = 'transparent',
#                 colores = c("#2E0F35", "#74D1F7", "#B70F7F", "#C2C4C4", "#8097A4", "#A6CEDE", "#801549", "#FECA84", "#ACD9C2"),
#                 width = NULL,
#                 height = NULL,
#                 fontFamily = 'Ubuntu',
#                 fontSize = '11px',
#                 marginBottom = NULL,
#                 marginLeft = NULL,
#                 marginRight = NULL,
#                 marginTop = NULL,
#                 diffColorsBar = FALSE,
#                 borderRadius = 0,
#                 borderWidth = 0,
#                 plotBackgroundColor = "transparent",
#                 plotBorderColor = "#cccccc",
#                 plotBorderWidth = 1,
#                 showLabel = FALSE,
#                 labsData = list(colLabel = NULL, familyLabel = 'Ubuntu',sizeLabel = NULL,
#                                 textDecoration = 'none', textShadow = 'none', textOutline = 'none'),
#                 stylesY = list(gridLineWidth = 1, lineColor = '#ccd6eb', tickColor = '#ccd6eb', gridLineColor = '#e6e6e6',
#                                tickLength = 10, lineWidth = 1),
#                 stylesLabelY = list(color = '#666666', fontSize = '11px', fontWeight = NULL, enabled =TRUE),
#                 stylesTitleY = list(color = '#666666', fontSize = '13px'),
#                 stylesX = list(gridLineWidth = 0, lineColor = '#ccd6eb', tickColor = '#ccd6eb', gridLineColor = '#e6e6e6',
#                                tickLength = 10, lineWidth = 1),
#                 stylesLabelX = list(color = '#666666', fontSize = '11px', fontWeight = NULL, enabled =TRUE),
#                 stylesTitleX = list(color = '#666666', fontSize = '13px')) {
#
# if (length(colores) == 1) {
#   colores <- c(colores, colores)
# }
#
#
#   hc_theme(
#     colors = colores,
#     chart = list(
#       backgroundColor = background,
#       borderColor = bordercolor,
#       borderRadius = borderRadius,
#       borderWidth = borderWidth,
#       width = width,
#       height = height,
#       marginBottom = marginBottom,
#       marginLeft = marginLeft,
#       marginRight = marginRight,
#       marginTop = marginRight,
#       plotBackgroundColor = plotBackgroundColor,
#       plotBorderColor = plotBorderColor,
#       plotBorderWidth = plotBorderWidth,
#       style = list (
#         fontFamily = fontFamily,
#         fontSize = fontSize
#       )
#     ),
#     plotOptions = list (
#       bar = list(
#         colorByPoint = diffColorsBar,
#         dataLabels = list (
#           enabled = showLabel,
#           style = list (
#             color = labsData$colLabel,
#             fontFamily = labsData$familyLabel,
#             fontSize = labsData$sizeLabel,
#             textDecoration= labsData$textDecoration,
#             textShadow = labsData$textShadow,
#             textOutline = labsData$textOutline
#           )
#         )
#       ),
#       column = list(
#         colorByPoint = diffColorsBar,
#         dataLabels = list (
#           enabled = showLabel,
#           style = list (
#             color = labsData$colLabel,
#             fontFamily = labsData$familyLabel,
#             fontSize = labsData$sizeLabel,
#             textDecoration= labsData$textDecoration,
#             textShadow = labsData$textShadow,
#             textOutline = labsData$textOutline
#           )
#         )
#       )
#     ),
#     xAxis = list (
#       gridLineWidth = stylesX$gridLineWidth,
#       lineColor = stylesX$lineColor, #color del eje x
#       tickColor = stylesX$tickColor, #color de las divisiones del eje x
#       gridLineColor = stylesX$gridLineColor,
#       tickLength = stylesX$tickLength,
#       lineWidth= stylesX$lineWidth,
#       labels = list(
#         style = list(
#           color = stylesLabelX$color, #color nombre de las etiquetas
#           fontSize = stylesLabelX$fontSize,
#           fontWeight = stylesLabelX$fontWeight
#         ),
#         enabled = stylesLabelX$enabled
#       ),
#       title = list(
#         style = list(
#           color = stylesTitleX$color,# color del titulo del eje
#           fontSize = stylesTitleX$fontSize
#           )
#       )
#     ),
#     yAxis = list(
#
#       gridLineWidth = stylesY$gridLineWidth,
#       lineColor = stylesY$lineColor,
#       tickColor = stylesY$tickColor,
#       gridLineColor = stylesY$gridLineColor,
#       tickLength = stylesY$tickLength,
#       lineWidth= stylesY$lineWidth,
#       labels = list(
#         style = list(
#           color = stylesLabelY$color,
#           fontSize = stylesLabelY$fontSize,
#           fontWeight = stylesLabelY$fontWeight
#         ),
#         enabled = stylesLabelY$enabled
#       ),
#       title = list(
#         style = list(
#           color = stylesTitleY$color,
#           fontSize = stylesTitleY$fontSize
#         )
#       )
#     )
#   )
# }
#
#
# a <- function(data,
#               title = NULL,
#               subtitle = NULL,
#               caption = NULL,
#               horLabel = NULL,
#               verLabel = NULL,
#               horLine = NULL,
#               horLineLabel = NULL,
#               verLine = NULL,
#               verLineLabel = NULL,
#               agg = "sum",
#               orientation = "ver",
#               marks = c("", "."),
#               nDigits = NULL,
#               dropNa = FALSE,
#               colorHighlightValue = '#F9B233',
#               percentage = FALSE,
#               format = c('', ''),
#               highlightValue = NULL,
#               order = NULL,
#               sort = "no",
#               sliceN = NULL,
#               tooltip = list(headerFormat = NULL, pointFormat = NULL),
#               export = FALSE,
#               thema = tma()) {
#
#
#   f <- fringe(data)
#   nms <- getClabels(f)
#   d <- f$d
#
#   title <-  title %||% ""
#   subtitle <- subtitle %||% ""
#   caption <- caption %||% ""
#
#   labelsXY <- orientationXY(orientation,
#                             x = nms[1],
#                             y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(agg, nms[2])),
#                             hor = horLabel,
#                             ver = verLabel)
#   lineXY <- orientationXY(orientation,
#                           NULL,
#                           NULL,
#                           hor = horLine,
#                           ver = verLine)
#   lineLabelsXY <- orientationXY(orientation,
#                                 x = horLine,
#                                 y = verLine,
#                                 hor = horLineLabel,
#                                 ver = verLineLabel,
#                                 line = TRUE)
#
#   if (dropNa)
#     d <- d %>%
#     tidyr::drop_na()
#
#   d <- d  %>%
#     tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
#                            b = NA)) %>%
#     dplyr::group_by(a) %>%
#     dplyr::summarise(b = agg(agg, b))
#
#   if (is.null(nDigits)) {
#     nDig <- 0
#   } else {
#     nDig <- nDigits
#   }
#
#   if (percentage) {
#     d$b <- (d[['b']] * 100) / sum(d[['b']], na.rm = TRUE)
#   }
#
#   d$b <- round(d$b, nDig)
#   d <- orderCategory(d, "a", order)
#   d <- sortSlice(d, "b", sort, sliceN)
#
#
#   d <- d %>% plyr::rename(c('b' = 'y'))
#   d$color <- NA
#
#   if (!is.null(highlightValue)) {
#     w <- which(d$a %in% highlightValue)
#     d$color[w] <- colorHighlightValue
#   }
#
#   data <- list()
#   bla <- map(1:nrow(d), function(z){
#     data$data[[z]] <<- list("name" = d$a[z],
#                             "y" = d$y[z],
#                             "color" = as.character(d$color[z]))
#   })
#
#   formatLabAxis <- paste0('{value:', marks[1], marks[2], 'f}')
#   if (!is.null(nDigits)) {
#     formatLabAxis <- paste0('{value:', marks[1], marks[2], nDigits, 'f}')
#   }
#
#
#   if (is.null(format)) {
#     format[1] = ""
#     format[2] = ""
#   }
#
#   aggFormAxis <- 'function() {return this.value+"";}'
#
#
#   if (percentage) {
#     aggFormAxis <- 'function() {return this.value+"%";}'
#     format[2] <- "%"
#   }
#
#
#
#
#     aggFormAxis <- paste0('function() {return "', format[1],'" + this.value+"', format[2],'";}')
#
#   if (is.null(tooltip$pointFormat)) {
#     tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(agg, ' ' ,nms[2], ': '), format[1],'{point.y}', format[2])
#   }
#   if (is.null(tooltip$headerFormat)) {
#       tooltip$headerFormat <- ""
#   }
#
#   global_options(marks[1])
#
#   hc <- highchart() %>%
#     hc_chart(type = ifelse(orientation == "hor", "bar", "column")) %>%
#     hc_title(text = title) %>%
#     hc_subtitle(text = subtitle) %>%
#     hc_tooltip(useHTML=TRUE, pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>%
#     hc_xAxis(
#       title =  list(text = labelsXY[1]),
#       plotLines = list(
#         list(value = lineXY[2],
#              color = 'black',
#              dashStyle = 'shortdash',
#              width = 2,
#              label = list(text = lineLabelsXY[1]))),
#       type= 'category'
#     ) %>%
#     hc_yAxis(
#       title = list (
#         text = labelsXY[2]),
#       plotLines = list(
#         list(value = lineXY[1],
#              color = 'black',
#              dashStyle = 'shortdash',
#              width = 2,
#              zIndex = 5,
#              label = list(text = lineLabelsXY[2]))),
#       labels = list (
#         format = formatLabAxis,
#         formatter = JS(aggFormAxis)
#       )
#     ) %>%
#     hc_series(
#       data
#     ) %>%
#     hc_add_theme(custom_theme(custom = thema)) %>%
#     hc_credits(enabled = TRUE, text = caption) %>%
#     hc_legend(enabled = FALSE)
#   if (export) hc <- hc %>%
#     hc_exporting(enabled = TRUE)
#   hc
#
# }
#
# pb <- sampleData("Cat-Num")#data.frame(a = c("perro", "gato"), b = c(30, 70))
# #pb <- data.frame(a = c("perro", "gato"), b = c(0.30, 0.35))
#
# # tooltip = mirar headerFormat
# # arreglar horline y verline horLine = 1000, horLineLabel = 'sada'
# # format = c('antes', 'despues'), mirar marks en el eje, mirar order
#
# a(data = pb, orientation = 'ver', horLabel = 'ansjdaa',
#   dropNa = TRUE, percentage = TRUE, nDigits = 2,thema = tma(showLabel = TRUE)
#   )
#
#
#
#
#   library(colormap)
#
# colorQuantile("Blues", domain = NULL)
