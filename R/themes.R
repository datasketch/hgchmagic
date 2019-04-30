#' @export
global_options <- function(marksMil, marksDec){

  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- marksMil
  hcoptslang$decimalPoint <- marksDec
  options(highcharter.lang = hcoptslang)
}


getDefaultTheme <- list(
  background = "#ffffff",
  fontFamily = "Ubuntu",
  colors = NULL,
  color = '#5A6B72',
  width = NULL,
  height = NULL,
  fontSize = '11px',
  margin_bottom = NULL,
  margin_left = NULL,
  margin_right = NULL,
  margin_top = NULL,
  diffColorsBar = TRUE,
  border_color = 'transparent',
  border_radius = 0,
  border_width = 0,
  border_widthBar = 0,
  plot_backgroundColor = "transparent",
  plot_borderColor = "#cccccc",
  plot_borderWidth = 0,
  showText = TRUE,
  symbLine = TRUE,
  lineWidth = 2,
  negativeColor = FALSE,
  linePointStart = 0,
  labsData_colLabel = 'contrast',
  labsData_familyLabel = 'Ubuntu',
  labsData_size = NULL,
  labsData_textDecoration = 'none',
  labsData_textShadow = 'none',
  labsData_textOutline = 'none',
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
  stylesTitleX_fontSize = '13px',
  legend_show = TRUE,
  legend_background = "transparent",
  legend_backgroundBorderColor = "#5A6B72",
  legend_backgroundWidth = 0
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
      borderColor = custom$bordercolor,
      border_radius = custom$border_radius,
      borderWidth = custom$borderWidth,
      width = custom$width,
      height = custom$height,
      marginBottom = custom$marginBottom,
      marginLeft = custom$marginLeft,
      marginRight = custom$marginRight,
      marginTop = custom$marginRight,
      plotBackgroundColor = custom$plotBackgroundColor,
      plotBorderColor = custom$plotBorderColor,
      plotBorderWidth = custom$plotBorderWidth,
      style = list (
        fontFamily = custom$fontFamily,
        fontSize = custom$fontSize
      )),
    plotOptions = list (
      bar = list(
        colorByPoint = custom$diffColorsBar,
        dataLabels = list (
          enabled = custom$showText,
          style = list (
            color = custom$labsData$colLabel,
            fontFamily = custom$labsData$familyLabel,
            fontSize = custom$labsData$sizeLabel,
            textDecoration= custom$labsData$textDecoration,
            textShadow = custom$labsData$textShadow,
            textOutline = custom$labsData$textOutline
          )
        )
      ),
      column = list(
        colorByPoint = custom$diffColorsBar,
        dataLabels = list (
          enabled = custom$showText,
          style = list (
            color = custom$labsData$colLabel,
            fontFamily = custom$labsData$familyLabel,
            fontSize = custom$labsData$sizeLabel,
            textDecoration= custom$labsData$textDecoration,
            textShadow = custom$labsData$textShadow,
            textOutline = custom$labsData$textOutline
          )
        )
      ),
      pie = list(
        showInLegend = custom$showLegend,
        dataLabels = list (
          enabled = custom$showText,
          style = list (
            color = custom$labsData$colLabel,
            fontFamily = custom$labsData$familyLabel,
            fontSize = custom$labsData$sizeLabel,
            textDecoration= custom$labsData$textDecoration,
            textShadow = custom$labsData$textShadow,
            textOutline = custom$labsData$textOutline
          )
        )
      ),
      series = list(
        borderWidth = custom$borderWidthBar,
        dataLabels = list (
          enabled = custom$showText,
          style = list (
            color = custom$labsData$colLabel,
            fontFamily = custom$labsData$familyLabel,
            fontSize = custom$labsData$sizeLabel,
            textDecoration= custom$labsData$textDecoration,
            textShadow = custom$labsData$textShadow,
            textOutline = custom$labsData$textOutline
          )
        ),
        lineWidth = custom$lineWidth,
        negativeColor = custom$negativeColor,
        pointStart = custom$linePointStart,
        marker = list(
          enabled = custom$symbLine
        )
      )
      ),
    xAxis = list (
      gridLineWidth = custom$stylesX$gridLineWidth,
      lineColor = custom$stylesX$lineColor, #color del eje x
      tickColor = custom$stylesX$tickColor, #color de las divisiones del eje x
      gridLineColor = custom$custom$stylesX$gridLineColor,
      tickLength = custom$stylesX$tickLength,
      lineWidth= custom$stylesX$lineWidth,
      labels = list(
        style = list(
          color = custom$color, #color nombre de las etiquetas
          fontSize = custom$stylesLabelX$fontSize,
          fontWeight = custom$stylesLabelX$fontWeight
        ),
        enabled = custom$stylesLabelX$enabled
      ),
      title = list(
        style = list(
          color = custom$color,# color del titulo del eje
          fontSize = custom$stylesTitleX$fontSize
        )
      )
    ),
    yAxis = list(
      gridLineDashStyle = custom$stylesY$gridLineDashStyle,
      gridLineWidth = custom$stylesY$gridLineWidth,
      lineColor = custom$stylesY$lineColor,
      tickColor = custom$stylesY$tickColor,
      gridLineColor = custom$stylesY$gridLineColor,
      tickLength = custom$stylesY$tickLength,
      lineWidth= custom$stylesY$lineWidth,
      labels = list(
        style = list(
          color = custom$color,
          fontSize = custom$stylesLabelY$fontSize,
          fontWeight = custom$stylesLabelY$fontWeight
        ),
        enabled = custom$stylesLabelY$enabled
      ),
      title = list(
        style = list(
          color = custom$color,
          fontSize = custom$stylesTitleY$fontSize
        )
      )
    ),
    legend = list(
    backgroundColor = custom$legend$background,
    borderColor = custom$legend$backgroundBorderColor,
    #borderWidth = custom$legend$backgroundWidth,
    itemStyle = list(
      color = custom$color
  )
  )
  )
}
