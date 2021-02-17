#' Area chart Cat Yea
#'
#' @description
#' `hgch_area_CatYea()` Create a highcharter area plot based on a particular data type.
#' In this case, you can load data with only three columns, where the firts column is a
#' **categorical column**, and second is a **year column**,
#'  or be sure that three firts columns they meet this condition
#' @export
#' @inheritParams hgch_area_YeaNum
#' @family Cat-Yea plots
#' @section Ftype:
#' Cat-Yea
#' @examples
#' data <- sample_data("Cat-Yea", n = 30)
#' hgch_area_CatYea(data)
#'
#' # Activate data labels
#' hgch_area_CatYea(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Yea-Dat-Yea-Cat", n = 30)
#' hgch_area_CatYea(data)
#'
#' # Change variable to color and pallete type
#' hgch_area_CatYea(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_area_CatYea(tooltip = info_tool)
#'
hgch_area_CatYea <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, ftype = "Cat-Yea")

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
    hc_chart(type = 'area',
             events = list(
               load = add_branding(l$theme)
             )) %>%
    hc_add_series_list(series) %>%
    hc_xAxis(title = list(text = l$title$x),
             categories = purrr::map(as.character(unique(d$b)), function(z) z),
             type = "category") %>%
    # hc_yAxis(title = list(text = l$title$y),
    #          labels = list(
    #            formatter = l$formats)
    # ) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_legend(enabled = l$theme$legend_show) %>%
    hc_add_theme(hgch_theme(opts = l$theme)) %>%
    hc_plotOptions(
      area = list(
        stacking= 'percent',
        areaColor= '#ffffff',
        areaWidth= 1,
        marker = list(
          areaWidth = 1,
          areaColor = '#ffffff'
        )
      )
    )

  hc
}
