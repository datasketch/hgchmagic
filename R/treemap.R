
#' hgch_treemap_CaNu
#' @name hgch_treemap_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_treemap_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_treemap_CaNu <- function(data, title = NULL, subtitle = NULL, caption = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                              minColor = "#E63917", maxColor= "#18941E",
                              reverse = TRUE, export = FALSE,...){
  # data <- sampleData("Ca-Nu")
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% nms[2]
  data <- f$d
  d <- data %>% na.omit() %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b))
  hc <- hchart(d, "treemap", hcaes(x = a, value = b, color = b)) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_colorAxis(maxColor = maxColor, minColor = minColor,reversed = reverse) %>%
    hc_tooltip(
      formatter = JS(paste0("function(){
                return this.point.a + ': <b>' +Highcharts.numberFormat(this.point.b,1,'.',',')+'</b><br/>';
            }"))
    )
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}

#' hgch_treemap_discrete_color_CaNu
#' @name hgch_treemap_discrete_color_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_treemap_discrete_color_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_treemap_discrete_color_CaNu <-function(data, title = NULL, subtitle = NULL, export = FALSE){

  f <- fringe(data)
  nms <- getCnames(f)
  data <- f$d


  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% nms[2]
  data <- plyr::rename(data, c("a" = "name"))

  data_graph <- data %>%
    dplyr::group_by(name) %>%
    tidyr::drop_na(name) %>%
    dplyr::summarise(value = mean(b, na.rm = TRUE ))

  data_graph <- data_graph %>%
    dplyr::mutate(x = 0:(dim(data_graph)[1]-1),
                  y = 10 + x + 10 * sin(x),
                  y = round(y, 1),
                  z = (x*y) - median(x*y),
                  color = getPalette()[1:(dim(data_graph)[1])])

  hc <- highchart() %>%
        hc_title(text = title) %>%
        hc_subtitle(text = subtitle) %>%
        hc_chart(type = "treemap",
                 polar = FALSE) %>%
        hc_xAxis(categories = data_graph$name) %>%
        hc_add_series(data_graph, showInLegend = FALSE)
  if (export)
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


#' hgch_treemap_CaCaNu
#' @name hgch_treemap_CaCaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_treemap_CaCaNu(sampleData("Ca-Ca-Nu",nrow = 10))
hgch_treemap_CaCaNu <- function(data, title = NULL,subtitle = NULL,
                                minColor = "#E63917", maxColor= "#18941E",
                                reverse = TRUE, export = FALSE,...){
  f <- fringe(data)
  nms <- getClabels(f)

  title <-  title %||% f$name
  data <- f$d
  data <- data %>% drop_na(a,b) %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = mean(c, na.rm = TRUE))

  tm <- treemap::treemap(data,
                         index=c("a","b"),
                         vSize="c",
                         vColor="c",
                         type="value", palette = viridis::viridis(6),draw = FALSE)
  hc <- hctreemap(tm, allowDrillToNode = TRUE, layoutAlgorithm = "squarified") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_plotOptions(
      series = list(
        levels = list(
          level = 1,
          dataLabels = list(enabled = TRUE),
          borderWidth = 3
        )
      )
    )
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}
