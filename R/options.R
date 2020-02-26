# default_options <- list(
#   agg = "sum",
#   agg_text = NULL,
#   allow_point = FALSE,
#   background = "#ffffff",
#   border_color = "#CCCCCC",
#   border_width = 1,
#   bubble_min = '3%',
#   bubble_max = '12%',
#   caption = NULL,
#   clickFunction = NULL,#JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")
#   colors = NULL,
#   color_click = NULL,
#   color_hover = NULL,
#   color_opacity = 0.7,
#   color_scale = 'discrete',
#   cursor =  NULL,
#   drop_na = FALSE,
#   drop_naV = c(FALSE, FALSE),
#   dropNaV = c(FALSE, FALSE),
#   export = FALSE,
#   fill_opacity = 0.5,
#   graph_type = "grouped",
#   highlight_value = NULL,
#   highlight_valueColor = '#F9B233',
#   horLabel = NULL,
#   horLine = NULL,
#   horLine_label = " ",
#   labelWrap = 12,
#   labelWrapV = c(12, 12),
#   lang = 'es',
#   legend_position  = "center",
#   legend_show = TRUE,
#   map_navigation = TRUE,
#   marks = c(".", ","),
#   nDigits = NULL,
#   nDigitsSize = NULL,
#   nDigitsY = NULL,
#   nDigitsX = NULL,
#   null_color = "#f7f7f7",
#   order = NULL,
#   order1 = NULL,
#   order2 = NULL,
#   orientation = "ver",
#   percentage = FALSE,
#   prefix = NULL,
#   prefixSize = NULL,
#   prefixX = NULL,
#   prefixY = NULL,
#   regression = FALSE,
#   regression_color = '#d35400',
#   regression_equation = TRUE,
#   text_show = TRUE,
#   sliceN = NULL,
#   sort = "no",
#   spline = FALSE,
#   startAtZero = TRUE,
#   subtitle = NULL,
#   suffix = NULL,
#   suffixX = NULL,
#   suffixY = NULL,
#   suffixSize = NULL,
#   title = NULL,
#   theme = NULL,
#   tooltip = list(headerFormat = NULL, pointFormat = NULL),
#   verLabel = NULL,
#   verLine = NULL,
#   verLine_label = " "
# )
#
#
# getOptions <- function(opts = NULL){
#
#   if(is.null(opts)){
#     opts <- default_options
#   }else{
#     opts <- modifyList(default_options, opts)
#   }
#
#   opts
# }
