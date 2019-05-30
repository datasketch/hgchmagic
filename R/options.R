default_options <- list(
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  horLabel = NULL,
  verLabel = NULL,
  horLine = NULL,
  horLine_label = " ",
  verLine = NULL,
  verLine_label = " ",
  labelWrap = 12,
  colors = NULL,
  color_scale = 'discrete',
  color_hover = NULL,
  color_click = NULL,
  agg = "sum",
  agg_text = NULL,
  orientation = "ver",
  marks = c(".", ","),
  nDigits = NULL,
  dropNa = FALSE,
  highlight_valueColor = '#F9B233',
  percentage = FALSE,
  prefix = NULL,
  suffix = NULL,
  highlight_value = NULL,
  order = NULL,
  sort = "no",
  sliceN = NULL,
  showText = TRUE,
  tooltip = list(headerFormat = NULL, pointFormat = NULL),
  export = FALSE,
  theme = NULL,
  lang = 'es',
  allow_point = FALSE,
  cursor =  NULL,
  clickFunction = NULL,#JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")

  # opciones que faltan en barras CatCatNum
  graphType = "grouped",
  dropNaV = c(FALSE, FALSE),
  labelWrapV = c(12, 12),
  order1 = NULL,
  order2 = NULL,
  legend_position  = "center",
  legend_show = TRUE,
  # opciones que faltan en lineas
  startAtZero = TRUE,
  spline = FALSE,
  fill_opacity = 0.5,

  bubble_min = '3%',
  bubble_max = '12%',

  border_color = "#CCCCCC",
  border_width = 1,
  null_color = "#f7f7f7",
  map_navigation = TRUE
)


getOptions <- function(opts = NULL){

  if(is.null(opts)){
    opts <- default_options
  }else{
    opts <- modifyList(default_options, opts)
  }

  opts
}
