#' Bar Chart Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Num, Yea-Num
#' @examples
#' hgch_bar_CatNum(sample_data("Cat-Num", nrow = 10))
#' @export
hgch_bar_CatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts)

  d <- l$d

  h <- purrr::map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "y" = as.numeric(d$b[z]),
                            "color" = as.character(d$..colors[z]))
  })

  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = ifelse(l$orientation == "hor","bar","column"),
             events = list(
               load = add_branding(opts$theme)
             )) %>%
    hc_series(
      data
    ) %>%
    hc_xAxis(title = list(text = l$title$x),
             type = "category") %>%
    hc_yAxis(title = list(text = l$title$y),
             labels = list(
               formatter = l$formats)
    ) %>%
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
    hc_tooltip(useHTML=TRUE, pointFormat = l$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(theme(opts = l$theme))

  hc
}



#' Bar Chart Cat
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat
#' @examples
#' hgch_bar_Cat(sample_data("Cat", nrow = 10))
#' @export
hgch_bar_Cat <- hgch_bar_CatNum



#' Bar Chart Yea
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Yea
#' @examples
#' hgch_bar_Yea(sample_data("Yea", nrow = 10))
#' @export
hgch_bar_Yea <- hgch_bar_CatNum


#' Bar Chart Yea Num
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Yea
#' @examples
#' hgch_bar_YeaNum(sample_data("Yea-Num", nrow = 10))
#' @export
hgch_bar_YeaNum <- hgch_bar_CatNum
