#' bubbles chart Cat Cat
#'
#' @description
#' `hgch_bubbles_CatCat()` Create a highcharter bubbles plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts and second columns are
#' **categoricals columns**, or be sure that firts two columns they meet this condition, since it
#' will be done a counting the categories of this columns.
#' @export
#' @inheritParams hgch_bubbles_CatNum
#' @family Cat-Cat plots
#' @section Ftype:
#' Cat-Cat
#' @examples
#' data <- sample_data("Cat-Cat", n = 30)
#' hgch_bubbles_CatCat(data)
#'
#' # Activate data labels
#' hgch_bubbles_CatCat(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Cat-Num-Yea-Cat", n = 30)
#' hgch_bubbles_CatCat(data)
#'
#' # Change variable to color and pallete type
#' hgch_bubbles_CatCat(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_bubbles_CatCat(tooltip = info_tool)
#'
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
               "data" = purrr::map(seq_along(d0[[3]]), function(i){
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


#' Bubbles chart Yea Cat
#'
#' @description
#' `hgch_bubbles_YeaCat()` Create a highcharter bubbles plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts is a
#' **year column** and the second must be a **categorical column**, or be sure that firts two columns
#' they meet this condition, since it will be done a counting the categories of this columns.
#' @export
#' @inheritParams hgch_bubbles_CatNum
#' @family Yea-Cat plots
#' @section Ftype:
#' Yea-Cat
#' @examples
#' data <- sample_data("Yea-Cat", n = 30)
#' hgch_bubbles_CatCat(data)
#'
#' # Activate data labels
#' hgch_bubbles_YeaCat(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Yea-Cat-Dat-Yea-Cat", n = 30)
#' hgch_bubbles_YeaCat(data)
#'
#' # Change variable to color and pallete type
#' hgch_bubbles_YeaCat(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_bubbles_YeaCat(tooltip = info_tool)
#'
hgch_bubbles_YeaCat <- hgch_bubbles_CatCat


#' Bubbles chart Cat Yea
#'
#' @description
#' `hgch_bubbles_CatYea()` Create a highcharter bubbles plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts is a
#' **categorical column** and the second must be a **year column**, or be sure that firts two columns
#' they meet this condition, since it will be done a counting the categories of this columns.
#' @export
#' @inheritParams hgch_bubbles_CatNum
#' @family Cat-Yea plots
#' @section Ftype:
#' Cat-Yea
#' @examples
#' data <- sample_data("Cat-Yea", n = 30)
#' hgch_bubbles_CatYea(data)
#'
#' # Activate data labels
#' hgch_bubbles_CatYea(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Yea-Cat-Dat-Num-Cat", n = 30)
#' hgch_bubbles_CatYea(data)
#'
#' # Change variable to color and pallete type
#' hgch_bubbles_CatYea(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[3],":</b> {", names_data[3],"}<br/>")
#' data %>%
#'  hgch_bubbles_CatYea(tooltip = info_tool)
#'
hgch_bubbles_CatYea <- hgch_bubbles_CatCat

