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
  bordercolor = 'transparent',
  colors = NULL,
  color = '#5A6B72',
  width = NULL,
  height = NULL,
  fontSize = '11px',
  marginBottom = NULL,
  marginLeft = NULL,
  marginRight = NULL,
  marginTop = NULL,
  diffColorsBar = TRUE,
  borderRadius = 0,
  borderWidth = 0,
  borderWidthBar = 0,
  plotBackgroundColor = "transparent",
  plotBorderColor = "#cccccc",
  plotBorderWidth = 0,
  showText = TRUE,
  symbLine = TRUE,
  showLegend = TRUE,
  lineWidth = 2,
  negativeColor = FALSE,
  linePointStart = 0,
  labsData = list(colLabel = 'contrast', familyLabel = 'Ubuntu',sizeLabel = NULL,
                  textDecoration = 'none', textShadow = 'none', textOutline = 'none'),
  stylesY = list(gridLineWidth = 1, lineColor = '#5A6B72', tickColor = 'transparent', gridLineColor = '#5A6B72',
                 tickLength = 10, lineWidth = 0, gridLineDashStyle = 'dot'),
  stylesLabelY = list(fontSize = '11px', fontWeight = NULL, enabled =TRUE),
  stylesTitleY = list(fontSize = '13px'),
  stylesX = list(gridLineWidth = 0, lineColor = '#5A6B72', tickColor = '#5A6B72', gridLineColor = '#5A6B72',
                 tickLength = 0, lineWidth = 1),
  stylesLabelX = list(fontSize = '11px', fontWeight = NULL, enabled =TRUE),
  stylesTitleX = list(fontSize = '13px'),
  legend = list(background = "transparent", backgroundBorderColor = "#5A6B72", backgroundWidth = 1)
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

  if (is.null(custom$colors)) custom$colors <-  c("#FECA84", "#3DB26F", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
  if (length(custom$colors)) custom$colors <- c(custom$colors, custom$colors)

  hc_theme(
    colors = custom$colors,
    chart = list(
      reflow = TRUE,
      renderTo = 'container',
      backgroundColor = custom$background,
      borderColor = custom$bordercolor,
      borderRadius = custom$borderRadius,
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
