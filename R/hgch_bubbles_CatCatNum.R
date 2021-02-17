#' Bubbles chart Cat Cat Num
#'
#' @description
#' `hgch_bubbles_CatCatNum()` Create a highcharter bubbles plot based on a particular data type.
#' In this case, you can load data with only three columns, where the firts and second columns are
#' **categoricals columns** and the third must be  a **numeric class column**, or be sure that
#' three firts columns they meet this condition
#' @export
#' @inheritParams hgch_bubbles_CatNum
#' @family Cat-Cat-Num plots
#' @section Ftype:
#' Cat-Cat-Num
#' @examples
#' data <- sample_data("Cat-Cat-Num", n = 30)
#' hgch_bubbles_CatCatNum(data)
#'
#' # Activate data labels
#' hgch_bubbles_CatCatNum(data,
#'                        dataLabels_show = TRUE)
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' hgch_bubbles_CatCatNum(data,
#'                        agg = "mean",
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Cat-Num-Dat-Yea-Cat", n = 30)
#' hgch_bubbles_CatCatNum(data)
#'
#' # Change variable to color and pallete type
#' hgch_bubbles_CatCatNum(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_bubbles_CatCatNum(tooltip = info_tool)
#'
hgch_bubbles_CatCatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, plot = "bubbles", ftype = "Cat-Cat-Num")

  d <- l$d

<<<<<<< HEAD

  series <- purrr::map(unique(d[[1]]), function(i) {
    d0 <- d %>%
      dplyr::filter(a %in% i) #%>% drop_na()
    label_info <- d0 %>% .$labels %>% unlist()
    l0 <- list("name" = i,
               "color" = unique(d0$..colors),
               "data" = map(seq_along(d0[[3]]), function(i){
                 list("label" =  label_info[i],
                      "value" = d0[[3]][i]
                 )
               })
=======
  series <- purrr::map(unique(d$a), function(x) {
    df <- d %>% dplyr::filter(a %in% x)
    list(
      name = x,
      data =
        purrr::map(1:nrow(df), function (z) {
          list(
            name = df$b[z],
            value = df$c[z],
            color = df$..colors[z]
          )
        })
>>>>>>> origin/master
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



#' Bubbles chart Cat Yea Num
#'
#' @description
#' `hgch_bubbles_CatYeaNum()` Create a highcharter bubbles plot based on a particular data type.
#' In this case, you can load data with only three columns, where the firts column is a
#' **categorical column**, second is a **year column** and the third must be  a **numeric class column**,
#'  or be sure that three firts columns they meet this condition
#' @export
#' @inheritParams hgch_bubbles_CatNum
#' @section Ftype:
#' Cat-Yea-Num
#' @examples
#' data <- sample_data("Cat-Yea-Num", n = 30)
#' hgch_bubbles_CatYeaNum(data)
#'
#' # Activate data labels
#' hgch_bubbles_CatYeaNum(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Yea-Num-Dat-Yea-Cat", n = 30)
#' hgch_bubbles_CatYeaNum(data)
#'
#' # Change variable to color and pallete type
#' hgch_bubbles_CatYeaNum(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_bubbles_CatYeaNum(tooltip = info_tool)
#'
hgch_bubbles_CatYeaNum <- hgch_bubbles_CatCatNum


#' Bubbles chart Yea Cat Num
#'
#' @description
#' `hgch_bubbles_YeaCatNum()` Create a highcharter bubbles plot based on a particular data type.
#' In this case, you can load data with only three columns, where the firts column is a
#' **year column**, second is a **categorical column** and the third must be  a **numeric class column**,
#'  or be sure that three firts columns they meet this condition
#' @export
#' @inheritParams hgch_bubbles_CatNum
#' @family Yea-Cat-Num plots
#' @section Ftype:
#' Yea-Cat-Num
#' @examples
#' data <- sample_data("Yea-Cat-Num", n = 30)
#' hgch_bubbles_YeaCatNum(data)
#'
#' # Activate data labels
#' hgch_bubbles_YeaCatNum(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Yea-Cat-Num-Dat-Yea-Cat", n = 30)
#' hgch_bubbles_YeaCatNum(data)
#'
#' # Change variable to color and pallete type
#' hgch_bubbles_YeaCatNum(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_bubbles_YeaCatNum(tooltip = info_tool)
#'
hgch_bubbles_YeaCatNum <- hgch_bubbles_CatCatNum

