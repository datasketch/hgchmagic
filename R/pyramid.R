#' Pyramid
#' 
#' Pyramid
#' 
#' 
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#' 
#' hgch_pyramid_CatNum(sampleData("Cat-Num",nrow = 10))
#' 
#' @export hgch_pyramid_CatNum
hgch_pyramid_CatNum <-function(data, title = ""){

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





#' Funnel
#' 
#' Funnel
#' 
#' 
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' Cat-Num
#' @examples
#' 
#' hgch_funnel_CatNum(sampleData("Cat-Num",nrow = 10))
#' 
#' @export hgch_funnel_CatNum
hgch_funnel_CatNum <-function(data, title = ""){


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


