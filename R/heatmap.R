#' Heatmap matrix
#'
#'  Heatmap matrix
#'
#'
#' @param x A data.frame
#' @return highcharts viz
#' @section ctypes:
#' CatCatNum
#' @examples
#' hgch_heatmap_matrix_CatCatNum(sampleData("CatCatNum", nrow = 20))
#' @export hgch_heatmap_matrix_CatCatNum
hgch_heatmap_matrix_CatCatNum <- function(data,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           xAxisTitle = NULL,
           yAxisTitle = NULL,
           sort = "no",
           theme = NULL,
           export = FALSE,
           ...) {

    if(class(data)[1] == "Fringe"){
      ni <- getClabels(data)
    }else{
      ni <- names(data)
    }

    f <- fringe(data)
    nms <- getClabels(f)

    xAxisTitle <- xAxisTitle %||% nms[1]
    yAxisTitle <- yAxisTitle %||% nms[2]
    title <-  title %||% ""
    caption <- caption %||% ""
    subtitle <- subtitle %||% ""

    d <- f$d
    if (nrow(d) == 0)
      return()
    d <- d %>% dplyr::group_by(a, b) %>% dplyr::summarise(c = sum(c)) %>% drop_na()

    hc <- highchart() %>%
      hc_add_series(data = d, type = "heatmap", hcaes(x = b, y = a, value = c)) %>%
      hc_title(text = title) %>%
      hc_subtitle(text = subtitle) %>%
      hc_yAxis(categories = unique(d[[1]]),title = list(text = yAxisTitle)) %>%
      hc_xAxis(categories = unique(d[[2]]),title = list(text = xAxisTitle)) %>%
      hc_colorAxis() %>%
      hc_tooltip(headerFormat = paste("<b style = 'font-size:12px'>", xAxisTitle, "</b><br/>"),
                 pointFormat = "<b style = 'font-size:12px'>{point.a}: {point.b}</b>")
    hc <- hc %>% hc_add_theme(custom_theme(custom = theme)) %>%
      hc_credits(enabled = TRUE, text = caption)
    if (export)
      hc <- hc %>% hc_exporting(enabled = TRUE)
    hc
  }

