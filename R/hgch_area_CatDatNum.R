#' area Chart Cat Dat Numeric
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Dat-Num
#' @examples
#' hgch_area_CatDatNum(sampleData("Cat-Dat-Num", nrow = 10))
#' @export
hgch_area_CatDatNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, ftype = "Cat-Dat-Num", plot = "area")

  d <- l$d

  ds <- NULL
  series <- lapply(unique(d$a), function(s){
    ds <<- d %>% filter(a == s)
    dss <- ds %>% select(a,b, labels)
    dss <- dss %>%
      mutate(x = as.numeric(ds$b),
             y = ds[[3]],
             label = labels)
    list(
      name = s,
      color = unique(ds$..colors),
      data = transpose(dss)
    )
  })

  h <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = "area",
             renderTo = 'container',
             events = list(
               load = add_branding(l$theme)
             )
    ) %>%
    hc_add_series_list(series) %>%
    hc_xAxis(
      type = 'datetime',
      title = list(text = l$title$x),
      tickInterval= l$date_intervals,
      labels = list(
        formatter= JS(l$formatter_date)
      )
    ) %>%
    hc_yAxis(title = list(text = l$title$y),
             labels = list(
               formatter = l$formats)
    ) %>%
    hc_tooltip(useHTML=TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))
    ) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_legend(enabled = l$theme$legend_show) %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme,
                                      cats = "{point.y} <br/>")))

  h
}


#' area Chart Cat Dat
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Dat
#' @examples
#' hgch_area_CatDat(sampleData("Cat-Dat", nrow = 10))
#' @export
hgch_area_CatDat <- hgch_area_CatDatNum
