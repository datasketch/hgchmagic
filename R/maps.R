#' hgch_map_choro_world_GeNu
#' @name hgch_map_choro_world_GeNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca
#' @examples
#' hgch_map_choro_world_GeNu(sampleData("Ca",nrow = 10))
hgch_map_choro_world_GeNu <- function(data, title = NULL,
                                      subtitle = NULL,
                                      xAxisTitle = NULL,
                                      yAxisTitle = NULL,
                                      minColor = "#E63917",
                                      maxColor= "#18941E",
                                      aggregate = "count", theme = NULL,
                                      export = FALSE, ...){

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


### Latin American


#' hgch_map_bubbles_latinAmerican_GeNu
#' @name hgch_map_bubbles_latinAmerican_GeNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca, Ca0Nu
#' @examples
#' hgch_map_bubbles_latinAmerican_GeNu(sampleData("Ca",nrow = 10))
hgch_map_bubbles_latinAmerican_GeNu <- function(data,
                                                title = NULL,
                                                subtitle = NULL,
                                                geoinfoPath = NULL,
                                                geoCodeVar = NULL,
                                                geoNameVar = NULL,
                                                theme = NULL,
                                                export = FALSE,
                                                leg_pos = 'bottom',
                                                leg_col =  "#505053",
                                                leg_alg = 'left',...){

  if(class(data)[1] == "Fringe"){
    ni <- getClabels(f)[-1]
  }else{
    ni <- names(data)[-1]
  }
  
  f <- fringe(data)
  nms <- getClabels(f)

  geoinfoPath <- geoinfoPath %||% system.file("aux/world-geo.csv",package = "hgchmagic")
  geoCodeVar <- geoCodeVar %||% "code"
  geo <- read_csv(geoinfoPath)
  geo$a <- geo[[geoCodeVar]]
  if(is.null(geoNameVar))
    geoNameVar <- geoCodeVar
  geo$name <- geo[[geoNameVar]]
  varLabel <- nms[2]
  dgeo <- f$d %>% na.omit() %>% group_by(a) %>% dplyr::summarise(b = mean(b))
  d <- dgeo %>% left_join(geo[c("a","name","lat","lon")],"a")

  data <- d %>% filter(!is.na(b))
  data <- plyr::rename(data, c('b' = 'z'))
  data$nou <- ni

mapLam <- jsonlite::fromJSON(system.file("aux/latin-america.json",package = "hgchmagic"), simplifyVector = FALSE)
mapLam <- geojsonio::as.json(mapLam)

hc <- highchart(type = "map") %>%
       hc_title(text = title) %>%
       hc_subtitle(text = subtitle) %>%
       hc_chart(backgroundColor = "#CDD2D4") %>%
       hc_add_series(mapData = mapLam, showInLegend = FALSE, dataLabels = list(
         enabled = TRUE, format='{point.name}'
         )
         ) %>%
       hc_mapNavigation(enabled = TRUE,
                        buttonOptions = list(align = leg_alg,
                                             verticalAlign = leg_pos,
                                             theme = list(
                                               fill = leg_col))
       )%>%
       hc_add_series(data = data, type = "mapbubble",
                     maxSize = 30,  showInLegend = FALSE,tooltip= list(
                     headerFormat= '',
                     pointFormat='<b>{point.name}</b>:<br>
                     {point.nou}: {point.z}'))


       hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
       if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
       hc

}


#' hgch_map_bubbles_latinAmerican_GeNuNu
#' @name hgch_map_bubbles_latinAmerican_GeNuNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca, Ca0Nu
#' @examples
#' hgch_map_bubbles_latinAmerican_GeNuNu(sampleData("Ca",nrow = 10))

hgch_map_bubbles_latinAmerican_GeNuNu <- function(data,
                                                  title = NULL,
                                                  subtitle = NULL,
                                                  geoinfoPath = NULL,
                                                  geoCodeVar = NULL,
                                                  geoNameVar = NULL,
                                                  theme = NULL,
                                                  export = FALSE,
                                                  leg_pos = 'bottom',
                                                  leg_col =  "#505053",
                                                  leg_alg = 'left',...){


  if(class(data)[1] == "Fringe"){
    ni <- getClabels(f)[-1]
  }else{
  ni <- names(data)[-1]
  }
  f <- fringe(data)
  nms <- getClabels(f)

  geoinfoPath <- geoinfoPath %||% system.file("aux/world-geo.csv",package = "hgchmagic")
  geoCodeVar <- geoCodeVar %||% "code"
  geo <- read_csv(geoinfoPath)
  geo$a <- geo[[geoCodeVar]]
  if(is.null(geoNameVar))
    geoNameVar <- geoCodeVar
  geo$name <- geo[[geoNameVar]]
  varLabel <- nms[2]
  dgeo <- f$d %>% na.omit() %>% group_by(a) %>% dplyr::summarise(b = mean(b), c = mean(c))
  d <- f$d %>% left_join(geo[c("a","name","lat","lon")],"a")
  d$var1 <- ni[1]
  d$var2 <- ni[2]
  d$z <- d$c
  serie1 <- select(d, -c, -z)
  serie1 <- plyr::rename(serie1, c('b' = 'z'))

  mapLam <- jsonlite::fromJSON(system.file("aux/latin-america.json",package = "hgchmagic"), simplifyVector = FALSE)
  mapLam <- geojsonio::as.json(mapLam)

  hc <- highchart(type = "map") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_chart(backgroundColor = "#CDD2D4") %>%
    hc_add_series(mapData = mapLam, showInLegend = FALSE, dataLabels = list(
      enabled = TRUE, format='{point.name}'
    )
    ) %>%
    hc_mapNavigation(enabled = TRUE,
                     buttonOptions = list(align = leg_alg,
                                          verticalAlign = leg_pos,
                                          theme = list(
                                            fill = leg_col))
    )%>%
    hc_add_series(data = serie1, type = "mapbubble",
                  maxSize = 30, showInLegend = FALSE, tooltip = list(
                    useHTML = TRUE,
                    headerFormat = '<table>',
                    pointFormat ="<b>{point.name}</b> <br>
                                   {point.var1}: {point.z}",
                    footerFormat= '</table>'
                  )) %>%
    hc_add_series(data = d, type = "mapbubble", minSize = 2,
                  maxSize = 40,showInLegend = FALSE,tooltip= list(
                    useHTML = TRUE,
                    headerFormat = '<table>',
                    pointFormat ="<b>{point.name}</b> <br>
                                   {point.var1}: {point.b} <br>
                                   {point.var2}: {point.c}",
                    footerFormat= '</table>'))

  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc

}



