#' Bubbles Chart Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat
#' @examples
#' hgch_bubbles_Cat(sample_data("Cat", nrow = 10))
#' @export
hgch_bubbles_Cat <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- hgchmagic_prep(data, opts = opts, plot = "bubbles", ftype = "Cat")

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
    hc_chart(type = 'packedbubble',
             events = list(
               load = add_branding(l$theme)
             )) %>%
    hc_add_series(
      data = data
    ) %>%
    hc_plotOptions(
      packedbubble = list(
        marker= list(
          states = list(
            hover = list(
              brightness= 0.1,
              fillColor = l$color_hover
            ),
            select = list(
              fillColor = l$color_click
            ))
        ),
        allowPointSelect= l$allow_point,
        cursor =  l$cursor,
        events = list(
          click = l$clickFunction
        )
      )
    ) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme,
                                      cats = "{point.name} <br/>",
                                      bubble_opacity = l$extra$bubble_opacity,
                                      bubble_min = paste0(l$extra$bubble_min, "%"),
                                      bubble_max = paste0(l$extra$bubble_max, "%")))
    )


  hc
}



#' bubbles Chart Yea
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Yea
#' @examples
#' hgch_bubbles_Yea(sample_data("Yea", nrow = 10))
#' @export
hgch_bubbles_Yea <- hgch_bubbles_Cat

