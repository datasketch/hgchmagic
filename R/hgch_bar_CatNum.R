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
  l <- hgchmagic_prep(data, opts = opts, ftype = "Cat-Num")
  d <- l$d
  l$theme$legend_show <- FALSE

  h <- purrr::map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "y" = as.numeric(d[[2]][z]),
                            "label" = d$labels[z],
                            "color" = as.character(d$..colors[z]))
  })

  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = ifelse(l$orientation == "hor","bar","column"),
             events = list(
               load = add_branding(opts$theme)
             )
             ) %>%
    hc_series(
      data
    ) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_xAxis(title = list(text = l$titles$x),
             type = "category") %>%
    hc_yAxis(title = list(text = l$titles$y),
             labels = list(
               formatter = l$formats)
    ) %>%
    hc_plotOptions(
      series = list(
        borderWidth = 0,
        pointPadding = l$theme$bar_padding,
        groupPadding = l$theme$bar_groupWidth,
        pointWidth = l$theme$bar_pointWidth,
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
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hgch_theme(opts = l$theme))

  hc
}





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
