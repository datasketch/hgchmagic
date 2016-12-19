
#' hgch_treemap_CaNu
#' @name hgch_treemap_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_treemap_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_treemap_CaNu <- function(data, title = NULL, xAxisTitle = NULL, yAxisTitle = NULL,
                              minColor = "#E63917", maxColor= "#18941E", reverse = TRUE, ...){
  # data <- sampleData("Ca-Nu")
  f <- fringe(data)
  nms <- getClabels(f)

  xAxisTitle <- xAxisTitle %||% nms[1]
  yAxisTitle <- yAxisTitle %||% ""
  title <-  title %||% nms[2]
  data <- f$d
  d <- data %>% na.omit() %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b))
  hchart(d, "treemap", hcaes(x = a, value = b, color = b)) %>%
    hc_title(text = title) %>%
    hc_colorAxis(maxColor = maxColor, minColor = minColor,reversed = reverse)
}


#' hgch_treemap_CaCaNu
#' @name hgch_treemap_CaCaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_treemap_CaCaNu(sampleData("Ca-Nu",nrow = 10))
hgch_treemap_CaCaNu <- function(data, title = NULL,
                                minColor = "#E63917", maxColor= "#18941E", reverse = TRUE, ...){
  f <- fringe(data)
  nms <- getClabels(f)

  title <-  title %||% f$name
  data <- f$d
  data <- data %>% na.omit() %>% dplyr::group_by(a,b) %>% dplyr::summarise(c = mean(c))

  tm <- treemap::treemap(data,
                         index=c("a","b"),
                         vSize="c",
                         vColor="c",
                         type="value", palette = viridis::viridis(6),draw = FALSE)
  hctreemap(tm, allowDrillToNode = TRUE, layoutAlgorithm = "squarified") %>%
    hc_title(text = title) %>%
    hc_plotOptions(
      series = list(
        levels = list(
          level = 1,
          dataLabels = list(enabled = TRUE),
          borderWidth = 3
        )
      )
    )

}
