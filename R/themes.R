#' @export
global_options <- function(marksMil, marksDec){

  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- marksMil
  hcoptslang$decimalPoint <- marksDec
  options(highcharter.lang = hcoptslang)
}


getDefaultTheme <- list(
  background = "#ffffff",

  border_color = 'transparent',
  border_radius = 0,
  border_width = 0,
  border_widthBar = 0,

  colors = NULL,
  colors_diff= TRUE,

  font_family = "Ubuntu",
  font_size = '11px',
  font_color = '#5A6B72',

  height = NULL,

  margin_bottom = NULL,
  margin_left = NULL,
  margin_right = NULL,
  margin_top = NULL,

  width = NULL,


  plot_backgroundColor = "transparent",
  plot_borderColor = "#cccccc",
  plot_borderWidth = 0,

  showText = TRUE,
  symbLine = TRUE,

  line_width = 2,

  negativeColor = FALSE,

  linePointStart = 0,
  labsData_colLabel = 'contrast',
  labsData_familyLabel = 'Ubuntu',
  labsData_size = NULL,
  labsData_textDecoration = 'none',
  labsData_textShadow = 'none',
  labsData_textOutline = 'none',


  legend_show = TRUE,
  legend_background = "transparent",
  legend_backgroundBorderColor = "#5A6B72",
  legend_backgroundWidth = 0,

  stylesY_gridLineWidth = 1,
  stylesY_lineColor = '#5A6B72',
  stylesY_tickColor = 'transparent',
  stylesY_gridLineColor = '#5A6B72',
  stylesY_tickLength = 10,
  stylesY_lineWidth = 0,
  stylesY_gridLineDashStyle = 'dot',
  stylesLabelY_fontSize = '11px',
  stylesLabelY_fontWeight = NULL,
  stylesLabelY_enabled =TRUE,
  stylesTitleY_fontSize = '13px',
  stylesX_gridLineWidth = 0,
  stylesX_lineColor = '#5A6B72',
  stylesX_tickColor = '#5A6B72',
  stylesX_gridLineColor = '#5A6B72',
  stylesX_tickLength = 0,
  stylesX_lineWidth = 1,
  stylesLabelX_fontSize = '11px',
  stylesLabelX_fontWeight = NULL,
  stylesLabelX_enabled =TRUE,
  stylesTitleX_fontSize = '13px'
)

getTheme <- function(theme = NULL){

  userTheme <- theme
  defaultTheme <- getDefaultTheme

  if(!is.null(theme)){
    theme <- modifyList(defaultTheme, userTheme)
  }else{
    theme <- defaultTheme
  }

  theme
}


#' @export
tma <- function(custom = NULL, ...) {

  custom <- getTheme(theme = custom)

  if (is.null(custom$colors)) custom$colors <-  c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
  if (length(custom$colors)) custom$colors <- c(custom$colors, custom$colors)

  hc_theme(
    colors = custom$colors,
    chart = list(
      reflow = TRUE,
      renderTo = 'container',
      backgroundColor = custom$background,
      borderColor = custom$border_color,
      borderRadius = custom$border_radius,
      borderWidth = custom$border_width,
      width = custom$width,
      height = custom$height,
      marginBottom = custom$margin_bottom,
      marginLeft = custom$margin_left,
      marginRight = custom$margin_right,
      marginTop = custom$margin_top,
      plotBackgroundColor = custom$plot_backgroundColor,
      plotBorderColor = custom$plot_borderColor,
      plotBorderWidth = custom$plot_borderWidth,
      style = list (
        fontFamily = custom$font_family,
        fontSize = custom$font_size
      )),
    plotOptions = list (
      bar = list(
        colorByPoint = custom$colors_diff,
        dataLabels = list (
          enabled = custom$showText,
          style = list (
            color = custom$labsData_colLabel,
            fontFamily = custom$labsData_familyLabel,
            fontSize = custom$labsData_sizeLabel,
            textDecoration= custom$labsData_textDecoration,
            textShadow = custom$labsData_textShadow,
            textOutline = custom$labsData_textOutline
          )
        )
      ),
      column = list(
        colorByPoint = custom$colors_diff,
        dataLabels = list (
          enabled = custom$showText,
          style = list (
            color = custom$labsData_colLabel,
            fontFamily = custom$labsData_familyLabel,
            fontSize = custom$labsData_sizeLabel,
            textDecoration= custom$labsData_textDecoration,
            textShadow = custom$labsData_textShadow,
            textOutline = custom$labsData_textOutline
          )
        )
      ),
      pie = list(
        showInLegend = custom$legend_show,
        dataLabels = list (
          enabled = custom$showText,
          style = list (
            color = custom$labsData_colLabel,
            fontFamily = custom$labsData_familyLabel,
            fontSize = custom$labsData_sizeLabel,
            textDecoration= custom$labsData_textDecoration,
            textShadow = custom$labsData_textShadow,
            textOutline = custom$labsData_textOutline
          )
        )
      ),
      series = list(
        borderWidth = custom$border_widthBar,
        dataLabels = list (
          enabled = custom$showText,
          style = list (
            color = custom$labsData_colLabel,
            fontFamily = custom$labsData_familyLabel,
            fontSize = custom$labsData_sizeLabel,
            textDecoration= custom$labsData_textDecoration,
            textShadow = custom$labsData_textShadow,
            textOutline = custom$labsData_textOutline
          )
        ),
        lineWidth = custom$line_width,
        negativeColor = custom$negativeColor,
        pointStart = custom$line_pointStart,
        marker = list(
          enabled = custom$symbLine
        )
      )
      ),
    xAxis = list (
      gridLineWidth = custom$stylesX_gridLineWidth,
      lineColor = custom$stylesX_lineColor, #color del eje x
      tickColor = custom$stylesX_tickColor, #color de las divisiones del eje x
      gridLineColor = custom$stylesX_gridLineColor,
      tickLength = custom$stylesX_tickLength,
      lineWidth= custom$stylesX_lineWidth,
      labels = list(
        style = list(
          color = custom$font_color, #color nombre de las etiquetas
          fontSize = custom$stylesLabelX_fontSize,
          fontWeight = custom$stylesLabelX_fontWeight
        ),
        enabled = custom$stylesLabelX_enabled
      ),
      title = list(
        style = list(
          color = custom$font_color,# color del titulo del eje
          fontSize = custom$stylesTitleX_fontSize
        )
      )
    ),
    yAxis = list(
      gridLineDashStyle = custom$stylesY_gridLineDashStyle,
      gridLineWidth = custom$stylesY_gridLineWidth,
      lineColor = custom$stylesY_lineColor,
      tickColor = custom$stylesY_tickColor,
      gridLineColor = custom$stylesY_gridLineColor,
      tickLength = custom$stylesY_tickLength,
      lineWidth= custom$stylesY_lineWidth,
      labels = list(
        style = list(
          color = custom$font_color,
          fontSize = custom$stylesLabelY_fontSize,
          fontWeight = custom$stylesLabelY_fontWeight
        ),
        enabled = custom$stylesLabelY_enabled
      ),
      title = list(
        style = list(
          color = custom$font_color,
          fontSize = custom$stylesTitleY_fontSize
        )
      )
    ),
    legend = list(
    backgroundColor = custom$legend_background,
    borderColor = custom$legend_backgroundBorderColor,
    #borderWidth = custom$legend$backgroundWidth,
    itemStyle = list(
      color = custom$font_color
  )
  )
  )
}
