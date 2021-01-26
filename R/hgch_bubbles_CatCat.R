#' Bubbles Chart Cat Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat, Cat-Yea-Num
#' @examples
#' hgch_bubbles_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export
hgch_bubbles_CatCat <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "bubbles", ftype = "Cat-Cat")

  d <- l$d


  series <- purrr::map(unique(d[[1]]), function(i) {
    d0 <- d %>%
      dplyr::filter(a %in% i) #%>% drop_na()
    label_info <- d0 %>% .$labels %>% unlist()
    l0 <- list("name" = i,
               "color" = unique(d0$..colors),
               "data" = map(seq_along(d0[[3]]), function(i){
                 list("label" =  label_info[i],
                      "value" = d0[[3]][i]
                 )
               })
    )
  })

  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = 'packedbubble',
             events = list(
               load = add_branding(l$theme)
             )) %>%
    hc_add_series_list(series) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_legend(enabled = l$theme$legend_show) %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme,
                                      cats = "{point.name} <br/>",
                                      bubble_opacity = l$extra$bubble_opacity,
                                      bubble_min = paste0(l$extra$bubble_min, "%"),
                                      bubble_max = paste0(l$extra$bubble_max, "%"))))

  hc

}


#' bubbles Chart Yea Cat
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Yea-Cat
#' @examples
#' hgch_bubbles_YeaCat(sample_data("Yea-Cat", nrow = 10))
#' @export
hgch_bubbles_YeaCat <- hgch_bubbles_CatCat


#' bubbles Chart Cat Yea
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Yea
#' @examples
#' hgch_bubbles_CatYea(sample_data("Cat-Yea", nrow = 10))
#' @export
hgch_bubbles_CatYea <- hgch_bubbles_CatCat

