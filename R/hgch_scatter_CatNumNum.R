#' Scatter chart Num Num
#'
#' @description
#' `hgch_scatter_CatNumNum()` Create a highcharter scatter plot based on a particular data type.
#' In this case, you can load data with only three columns,  where the first is
#' a **categorical column**, and second and third columns are
#' **numeric class**, or be sure that two firts columns they meet this condition
#'
#' @export
#' @inheritParams hgch_scatter_NumNum
#' @family Cat-Num-Num plots
#' @section Ftype:
#' Cat-Num-Num
#' @examples
#' data <- sample_data("Cat-Num-Num", n = 30)
#' hgch_scatter_CatNumNum(data)
#'
#' example with iris data
#' data <- iris %>% select(Species, Petal.Width, Petal.Length)
#' hgch_scatter_CatNumNum(data)
#'

#' # data with more of one column
#' data <- sample_data("Cat-Num-Num-Dat-Cat-Cat", n = 30)
#' hgch_scatter_CatNumNum(data)
#'
hgch_scatter_CatNumNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data = data, opts = opts, plot = "scatter", ftype = "Cat-Num-Num")

  d <- l$d
  ds <- NULL
  series <- lapply(unique(d$a), function(s){

    ds <<- d %>% dplyr::filter(a == s)
    dss <- ds %>% dplyr::select(a,b, labels)
    dss <- dss %>%
      dplyr::mutate(x = as.numeric(ds$b),
                    y = ds[[3]],
                    label = labels)
    list(
      name = s,
      color = unique(ds$..colors),
      data = purrr::transpose(dss)
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
      title = list(text = l$titles$x),
      labels = list(
        formatter = makeup::makeup_format_js(sample = opts$style$format_sample_num,
                                             locale = opts$style$locale,
                                             prefix = opts$scatter$prefix_x,
                                             suffix = opts$scatter$suffix_x))
    ) %>%
    hc_yAxis(
      title = list(text = l$titles$y),
      labels = list(
        formatter = makeup::makeup_format_js(sample = opts$style$format_sample_num,
                                             locale = opts$style$locale,
                                             prefix = opts$scatter$prefix_y,
                                             suffix = opts$scatter$suffix_y))
    ) %>%
    hc_add_series_list(series) %>%
    hc_tooltip(useHTML=TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_add_theme(hgch_theme(opts =  c(l$theme)))

  h
}
