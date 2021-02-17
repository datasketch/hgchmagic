#' Area chart Cat Dat Num
#'
#' @description
#' `hgch_area_CatDatNum()` Create a highcharter area plot based on a particular data type.
#' In this case, you can load data with only three columns, where the firts column is a
#' **categorical column**, second is a **date column** and the third must be a **numeric class column**,
#'  or be sure that three firts columns they meet this condition.
#' @export
#' @inheritParams hgch_area_YeaNum
#' @family Cat-Dat-Num plots
#' @section Ftype:
#' Cat-Dat-Num
#' @examples
#' data <- sample_data("Cat-Dat-Num", n = 30)
#' hgch_area_CatDatNum(data)
#'
#' # Activate data labels
#' hgch_area_CatDatNum(data,
#'                     dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Dat-Num-Num-Num-Cat", n = 30)
#' hgch_area_CatDatNum(data)
#'
#' # Change variable to color and pallete type
#' hgch_area_CatDatNum(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_area_CatDatNum(tooltip = info_tool)
#'
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

