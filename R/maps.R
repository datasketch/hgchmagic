#' hgch_map_choro_world_GeNu
#' @name hgch_map_choro_world_GeNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_map_choro_world_GeNu(sampleData("Ca",nrow = 10))
hgch_map_choro_world_GeNu <- function(data, title = NULL,
                                      xAxisTitle = NULL,
                                      yAxisTitle = NULL,
                                      minColor = "#E63917",
                                      maxColor= "#18941E",
                                      aggregate = "count",theme = NULL,
                                      export = FALSE,...){

  f <- fringe(data)
  nms <- getClabels(f)

  data(worldgeojson)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% nms[2]
  title <-  title %||% ""
  d <- f$d %>% na.omit()
  d$iso3 <- d$a

  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    #hc_chart(zoomType = "xy") %>%
    hc_add_series_map(worldgeojson, d,value = "b", joinBy = "iso3") %>%
    hc_colorAxis(maxColor = maxColor, minColor = minColor) %>%
    hc_mapNavigation(enabled = TRUE)
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}




#
# data(GNI2014, package = "treemap")
#
# dshmstops <- data.frame(q = c(0, exp(1:5)/exp(5)), c = substring(viridis(5 + 1), 0, 7)) %>%
#   list.parse2()
#
#
#
#

#
#
#
#
# highchart() %>%
#   hc_title(text = "Charting GNI data") %>%
#   hc_add_series_map(worldgeojson, GNI2014,
#                     value = "GNI", joinBy = "iso3") %>%
#   hc_colorAxis(stops = dshmstops)
