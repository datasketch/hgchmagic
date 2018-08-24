
#' @export
getPalette <- function(type = "qualitative", rev = FALSE){
  dsGreen <- "#95C11E"
  dsYellow <- "#FFED00"
  dsMagenta <- "#E5007D"
  dsBlue <- "#009EE3"
  dsOrange <- "#F9B233"
  dsPink <- "#EF8998"
  dsLightBlue <- "#16C5E0"
  dsPurple <- "#A839B0"
  dsRed <- "#C92F2F"
  dsGray <- "#A9A9A9"
  dsLila <- "#9B71AF"
  dsPalette <- c(dsBlue,dsMagenta,dsGreen,dsOrange,dsYellow,dsPink,
                 dsLightBlue,dsPurple,dsRed,dsGray, dsLila)
  p <- c(dsPalette,dsPalette,dsPalette)
  if(type == "sequential") {
    p <-  c(dsMagenta,dsBlue)
  }
  if(rev) p <- rev(p)
  p
}

#' @export
custom_theme  <- function(custom = NULL,...){
  ds_theme <-
    hc_theme(
      colors = getPalette(11),
      chart = list(
        style = list(
          fontFamily = "Roboto"
        )
      ),
      title = list(
        align = "left",
        style = list(
          fontFamily = "Roboto Condensed",
          fontWeight = "bold"
        )
      ),
      subtitle = list(
        align = "left",
        style = list(
          fontFamily = "Roboto Condensed"
        )
      ),
      legend = list(
        align = "right",
        verticalAlign = "bottom"
      ),
      xAxis = list(
        gridLineWidth = 1,
        gridLineColor = "#F3F3F3",
        lineColor = "#F3F3F3",
        minorGridLineColor = "#F3F3F3",
        tickColor = "#F3F3F3",
        tickWidth = 1
      ),
      yAxis = list(
        gridLineColor = "#F3F3F3",
        lineColor = "#F3F3F3",
        minorGridLineColor = "#F3F3F3",
        tickColor = "#F3F3F3",
        tickWidth = 1
      ),
      plotOptions = list(
        line = list(
          marker = list(enabled = FALSE),
          states = list(hover = list(lineWidthPlus = 1))
        ),
        spline = list(
          marker = list(enabled = FALSE),
          states = list(hover = list(lineWidthPlus = 1))
        ),
        area = list(
          marker = list(enabled = FALSE),
          states = list(hover = list(lineWidthPlus = 1))
        ),
        areaspline = list(
          marker = list(enabled = FALSE),
          states = list(hover = list(lineWidthPlus = 1))
        )
      )
    )

  theme <- structure(ds_theme, class = "hc_theme")

  if (!is.null(custom)) {
    # str(custom)
    # custom <- structure(custom, class = "hc_theme")
    # theme <- hc_theme_merge(
    #   theme,
    #   hc_theme(custom)
    # )
    theme <- custom
  }
  theme
}


#' @export
global_options <- function(marksMil, marksDec){

  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- marksMil
  hcoptslang$decimalPoint <- marksDec
  options(highcharter.lang = hcoptslang)
}

#' @export
tma <- function(background = 'transparent',
                bordercolor = 'transparent',
                colores = c("#2E0F35", "#74D1F7", "#B70F7F", "#C2C4C4", "#8097A4", "#A6CEDE", "#801549", "#FECA84", "#ACD9C2"),
                width = NULL,
                height = NULL,
                fontFamily = 'Ubuntu',
                fontSize = '11px',
                marginBottom = NULL,
                marginLeft = NULL,
                marginRight = NULL,
                marginTop = NULL,
                diffColorsBar = FALSE,
                borderRadius = 0,
                borderWidth = 0,
                plotBackgroundColor = "transparent",
                plotBorderColor = "#cccccc",
                plotBorderWidth = 1,
                showLabel = FALSE,
                symbLine = TRUE,
                lineWidth = 2,
                negativeColor = FALSE,
                linePointStart = 0,
                labsData = list(colLabel = NULL, familyLabel = 'Ubuntu',sizeLabel = NULL,
                                textDecoration = 'none', textShadow = 'none', textOutline = 'none'),
                stylesY = list(gridLineWidth = 1, lineColor = '#ccd6eb', tickColor = '#ccd6eb', gridLineColor = '#e6e6e6',
                               tickLength = 10, lineWidth = 1),
                stylesLabelY = list(color = '#666666', fontSize = '11px', fontWeight = NULL, enabled =TRUE),
                stylesTitleY = list(color = '#666666', fontSize = '13px'),
                stylesX = list(gridLineWidth = 0, lineColor = '#ccd6eb', tickColor = '#ccd6eb', gridLineColor = '#e6e6e6',
                               tickLength = 10, lineWidth = 1),
                stylesLabelX = list(color = '#666666', fontSize = '11px', fontWeight = NULL, enabled =TRUE),
                stylesTitleX = list(color = '#666666', fontSize = '13px')) {

  if (length(colores) == 1) {
    colores <- c(colores, colores)
  }


  hc_theme(
    colors = colores,
    chart = list(
      backgroundColor = background,
      borderColor = bordercolor,
      borderRadius = borderRadius,
      borderWidth = borderWidth,
      width = width,
      height = height,
      marginBottom = marginBottom,
      marginLeft = marginLeft,
      marginRight = marginRight,
      marginTop = marginRight,
      plotBackgroundColor = plotBackgroundColor,
      plotBorderColor = plotBorderColor,
      plotBorderWidth = plotBorderWidth,
      style = list (
        fontFamily = fontFamily,
        fontSize = fontSize
      )
    ),
    plotOptions = list (
      bar = list(
        colorByPoint = diffColorsBar,
        dataLabels = list (
          enabled = showLabel,
          style = list (
            color = labsData$colLabel,
            fontFamily = labsData$familyLabel,
            fontSize = labsData$sizeLabel,
            textDecoration= labsData$textDecoration,
            textShadow = labsData$textShadow,
            textOutline = labsData$textOutline
          )
        )
      ),
      column = list(
        colorByPoint = diffColorsBar,
        dataLabels = list (
          enabled = showLabel,
          style = list (
            color = labsData$colLabel,
            fontFamily = labsData$familyLabel,
            fontSize = labsData$sizeLabel,
            textDecoration= labsData$textDecoration,
            textShadow = labsData$textShadow,
            textOutline = labsData$textOutline
          )
        )
      ),
      line = list(
        colorByPoint = diffColorsBar,
        dataLabels = list (
          enabled = showLabel,
          style = list (
            color = labsData$colLabel,
            fontFamily = labsData$familyLabel,
            fontSize = labsData$sizeLabel,
            textDecoration= labsData$textDecoration,
            textShadow = labsData$textShadow,
            textOutline = labsData$textOutline
          )
        )
      ),
      series = list(
        lineWidth = lineWidth,
        negativeColor = negativeColor,
        pointStart = linePointStart,
        marker = list(
          enabled = symbLine
        )
      )
    ),
    xAxis = list (
      gridLineWidth = stylesX$gridLineWidth,
      lineColor = stylesX$lineColor, #color del eje x
      tickColor = stylesX$tickColor, #color de las divisiones del eje x
      gridLineColor = stylesX$gridLineColor,
      tickLength = stylesX$tickLength,
      lineWidth= stylesX$lineWidth,
      labels = list(
        style = list(
          color = stylesLabelX$color, #color nombre de las etiquetas
          fontSize = stylesLabelX$fontSize,
          fontWeight = stylesLabelX$fontWeight
        ),
        enabled = stylesLabelX$enabled
      ),
      title = list(
        style = list(
          color = stylesTitleX$color,# color del titulo del eje
          fontSize = stylesTitleX$fontSize
        )
      )
    ),
    yAxis = list(

      gridLineWidth = stylesY$gridLineWidth,
      lineColor = stylesY$lineColor,
      tickColor = stylesY$tickColor,
      gridLineColor = stylesY$gridLineColor,
      tickLength = stylesY$tickLength,
      lineWidth= stylesY$lineWidth,
      labels = list(
        style = list(
          color = stylesLabelY$color,
          fontSize = stylesLabelY$fontSize,
          fontWeight = stylesLabelY$fontWeight
        ),
        enabled = stylesLabelY$enabled
      ),
      title = list(
        style = list(
          color = stylesTitleY$color,
          fontSize = stylesTitleY$fontSize
        )
      )
    )
  )
}
