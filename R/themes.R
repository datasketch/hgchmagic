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
