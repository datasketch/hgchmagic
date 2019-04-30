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
  cursor =  NULL,
  clickFunction = NULL#JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")

)


getOptions <- function(opts = NULL){

  if(is.null(opts)){
    opts <- default_options
  }else{
    opts <- modifyList(default_options, opts)
  }

  opts
}
