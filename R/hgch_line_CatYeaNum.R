#' line Chart Cat Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num, Cat-Yea-Num
#' @examples
#' hgch_line_CatYeaNum(sample_data("Cat-Yea-Num", nrow = 100))
#' @export
hgch_line_CatYeaNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, ftype = "Cat-Yea-Num")

  d <- l$d


  series <- purrr::map(unique(d[[1]]), function(i) {
    d0 <- d %>%
      dplyr::filter(a %in% i) #%>% drop_na()
    label_info <- d0 %>% .$labels %>% unlist()
    l0 <- list("name" = i,
               "color" = unique(d0$..colors),
               "data" = map(seq_along(d0[[3]]), function(i){
                 list("label" =  label_info[i],
                      "y" = d0[[3]][i]
                 )
               })
    )
  })

  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = 'line',
             events = list(
               load = add_branding(l$theme)
             )) %>%
    hc_add_series_list(series) %>%
    hc_xAxis(title = list(text = l$title$x),
             categories = purrr::map(as.character(unique(d$b)), function(z) z),
             type = "category") %>%
    hc_yAxis(title = list(text = l$title$y),
             labels = list(
               formatter = l$formats)
    ) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_legend(enabled =  l$theme$legend_show) %>%
    hc_add_theme(hgch_theme(opts = l$theme))


  hc
}





#' line Chart Cat Yea
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_line_CatYea(sample_data("Cat-Yea", nrow = 10))
#' @export
hgch_line_CatYea <- hgch_line_CatYeaNum

#' line Chart Yea Cat Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_line_YeaCatNum(sample_data("Yea-Cat-Num", nrow = 10))
#' @export
hgch_line_YeaCatNum <- hgch_line_CatYeaNum

