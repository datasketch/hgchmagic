#' Scatter chart Cat Dat Num
#'
#' @description
#' `hgch_scatter_CatDatNum()` Create a highcharter scatter plot based on a particular data type.
#' In this case, you can load data with only three columns,  where the firts is a **categorical column**,
#' second is a **date column**, and the three is a **numeric column**, or be sure that three firts columns
#' they meet this condition.
#'
#' @export
#' @inheritParams hgch_scatter_NumNum
#' @family Cat-Dat-Num plots
#' @section Ftype:
#' Dat-Num
#' @examples
#' data <- sample_data("Cat-Dat-Num", n = 30)
#' hgch_scatter_CatDatNum(data)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Dat-Num-Dat-Cat-Cat", n = 30)
#' hgch_scatter_CatDatNum(data)
#'
hgch_scatter_CatDatNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "scatter", ftype = "Cat-Dat-Num")

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
    hc_chart(type = "scatter",
             events = list(
               load = add_branding(l$theme)
             )
    ) %>%
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
    hc_add_series_list(series) %>%
    hc_tooltip(useHTML=TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))
    ) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme)))

  h
}


