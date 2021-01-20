#' Treemap Chart Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Num, Yea-Num
#' @examples
#' hgch_treemap_CatNum(sample_data("Cat-Num", nrow = 10))
#' @export
hgch_treemap_CatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "treemap")
  l$theme$legend_show <- FALSE
  d <- l$d


  data <- purrr::map(1:nrow(d), function(z){
    list("name" = d$a[z],
         "value" = d[[2]][z],
         "label" = d$labels[z],
         "color" = as.character(d$..colors[z]))
  })

  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(
      events = list(
        load = add_branding(l$theme)
      )) %>%
    hc_plotOptions(
      series = list(
        states = list(
          hover = list(
            brightness= 0.1,
            color = l$color_hover
          ),
          select = list(
            color = l$color_click
          )
        ),
        allowPointSelect= l$allow_point,
        cursor =  l$cursor,
        events = list(
          click = l$clickFunction
        )
      )) %>%
    hc_series(
      list(
        type = 'treemap',
        layoutAlgorithm = l$extra$treemap_layout,
        layoutStartingDirection = l$extra$treemap_direction,
        data = data)) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_credits(enabled = TRUE, text = opts$title$caption %||% "") %>%
    hc_legend(enabled = F) %>%
    hc_add_theme(hgch_theme(opts = c(l$theme,
                                     cats = "{point.name} <br/>")))

  hc
}


#' Treemap Chart Cat
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat, Yea
#' @examples
#' hgch_treemap_Cat(sample_data("Cat", nrow = 10))
#' @export
hgch_treemap_Cat <-  hgch_treemap_CatNum


#' treemap Chart Yea
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Yea, Yea
#' @examples
#' hgch_treemap_Yea(sample_data("Yea", nrow = 10))
#' @export
hgch_treemap_Yea <- hgch_treemap_CatNum

#' treemap Chart Yea Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Yea, Yea
#' @examples
#' hgch_treemap_YeaNum(sample_data("Yea-Num", nrow = 10))
#' @export
hgch_treemap_YeaNum <- hgch_treemap_CatNum
