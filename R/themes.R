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


ds_theme <- hc_theme(
  colors = getPalette(11),
  chart = list(
    backgroundColor = "#ffffff",
    divBackgroundImage = ""
  ),
  title = list(
    style = list(
      color = '#333333',
      fontFamily = "Lato"
    )
  ),
  subtitle = list(
    style = list(
      color = '#666666',
      fontFamily = ""
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = '',
      color = 'black'
    ),
    itemHoverStyle = list(
      color = 'gray'
    )
  )
)
