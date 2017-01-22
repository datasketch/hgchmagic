#' hgch_pyramid_CaNu
#' @name hgch_pyramid_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_pyramid_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_pyramid_CaNu <-function(data, title = ""){

  f <- fringe(data)
  nms <- getCnames(f)
  data <- f$d
  data <- plyr::rename(data, c("a" = "name"))

  data_graph <- data %>%
                dplyr::group_by(name) %>%
                tidyr::drop_na(name) %>%
                dplyr::summarise(value = mean(b, na.rm = TRUE ))

  data_graph <- data_graph %>%
                dplyr::mutate(x = 0:(dim(data_graph)[1]-1),
                              y = value,
                              z = (x*y) - median(x*y),
                              color = getPalette()[1:(dim(data_graph)[1])])

  data_graph <- data_graph %>%
                dplyr::select(x, y, z, value, name, color) %>%
                dplyr::arrange(-value)

  hc <- highchart() %>%
        hc_title(text = title) %>%
        hc_chart(type = "pyramid",
                 polar = FALSE) %>%
        hc_xAxis(categories = data_graph$name) %>%
        hc_add_series(data_graph, showInLegend = FALSE)
  hc
}



#' hgch_funnel_CaNu
#' @name hgch_funnel_CaNu
#' @param x A data.frame
#' @export
#' @return highcharts viz
#' @section ftype: Ca-Nu
#' @examples
#' hgch_funnel_CaNu(sampleData("Ca-Nu",nrow = 10))
hgch_funnel_CaNu <-function(data, title = ""){


  f <- fringe(data)
  nms <- getCnames(f)
  data <- f$d
  data <- plyr::rename(data, c("a" = "name"))

  data_graph <- data %>%
                dplyr::group_by(name) %>%
                tidyr::drop_na(name) %>%
                dplyr::summarise(value = mean(b, na.rm = TRUE ))

  data_graph <- data_graph %>%
                dplyr::mutate(x = 0:(dim(data_graph)[1]-1),
                              y = value,
                              z = (x*y) - median(x*y),
                              color = getPalette()[1:(dim(data_graph)[1])])

  data_graph <- data_graph %>%
                select(x, y, z, value, name, color) %>%
                arrange(-value)

  hc <- highchart() %>%
        hc_title(text = title) %>%
        hc_chart(type = "funnel",
                 polar = FALSE) %>%
        hc_xAxis(categories = data_graph$name) %>%
        hc_add_series(data_graph, showInLegend = FALSE)
  hc

}


