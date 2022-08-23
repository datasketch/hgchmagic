#' Bar chart Cat Cat Num
#'
#' @description
#' `hgch_bar_CatCatNum()` Create a highcharter bar plot based on a particular data type.
#' In this case, you can load data with only three columns, where the firts and second columns are
#' **categoricals columns** and the third must be  a **numeric class column**, or be sure that
#' three firts columns they meet this condition
#' @export
#' @inheritParams hgch_bar_CatNum
#' @family Cat-Cat-Num plots
#' @section Ftype:
#' Cat-Cat-Num
#' @examples
#' data <- sample_data("Cat-Cat-Num", n = 30)
#' hgch_bar_CatCatNum(data)
#'
#' # Activate data labels
#' hgch_bar_CatCatNum(data,
#'                        dataLabels_show = TRUE)
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' hgch_bar_CatCatNum(data,
#'                        agg = "mean",
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Cat-Num-Dat-Yea-Cat", n = 30)
#' hgch_bar_CatCatNum(data)
#'
#' # Change variable to color and pallete type
#' hgch_bar_CatCatNum(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_bar_CatCatNum(tooltip = info_tool)
#'
hgch_bar_CatCatNum <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")
  data[[1]] <- homodatum::as_Cat(data[[1]])
  data[[2]] <- homodatum::as_Cat(data[[2]])
  #data[[3]] <- homodatum::as_Num(data[[3]])

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, ftype = "Cat-Cat-Num", plot = "bar")
  d <- l$d

  series <- purrr::map(unique(d[[1]]), function(i) {
    d0 <- d %>%
      dplyr::filter(a %in% i) #%>% drop_na()
    label_info <- d0 %>% .$labels %>% unlist()
    l0 <- list("name" = i,
               "color" = unique(d0$..colors),
               "index" = d0$..index,
               "legendIndex" = d0$..legendIndex,
               "data" = purrr::map(seq_along(d0[[3]]), function(i){
                 list("label" =  label_info[i],
                      "y" = d0[[3]][i]
                 )
               })
    )
  })
 #print(series)
  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type =  ifelse(l$orientation == "hor","bar","column"),
             events = list(
               load = add_branding(l$theme)
             )
    ) %>%
    hc_add_series_list(series) %>%
    hc_xAxis(title = list(text = l$title$x),
             categories = purrr::map(as.character(unique(d$b)), function(z) z),
             type = "category",
             labels = list(
               formatter = l$formatter_x_js#,
               #step = l$extra$labelsStepX,
             )) %>%
    hc_yAxis(title = list(text = l$title$y),
             reversed = l$extra$reversedYaxis,
             labels = list(
               align= l$y_axis_align,
               formatter = l$formatter_js %||% l$formats)
    ) %>%
    hc_tooltip(useHTML = TRUE,
               headerFormat = NULL,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_plotOptions(
      series = list(
        borderWidth = 0,
        pointPadding = l$theme$bar_padding,
        groupPadding = l$theme$bar_groupWidth,
        pointWidth = l$theme$bar_pointWidth,
        states = list(
          hover = list(
            #//brightness: -0.5,
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
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
    hc_legend(enabled = l$theme$legend_show)

  if (l$graph_type == "stacked"){
    hc <- hc %>% hc_plotOptions(bar = list(stacking = "normal"), column = list(stacking = "normal"))
    if (l$percentage) {
      hc <- hc %>%
        hc_plotOptions(
          bar = list(
            stacking = 'percent'
          ),
          column = list(
            stacking = 'percent'
          )
        ) %>%
        hc_yAxis(maxRange = 100,
                 max = 100)
    }
  }

  hc <- hc %>%  hc_add_theme(hgch_theme(opts = l$theme))


  hc
}





#' Bar chart Cat Yea Num
#'
#' @description
#' `hgch_bar_CatYeaNum()` Create a highcharter bar plot based on a particular data type.
#' In this case, you can load data with only three columns, where the firts column is a
#' **categorical column**, second is a **year column** and the third must be  a **numeric class column**,
#'  or be sure that three firts columns they meet this condition
#' @export
#' @inheritParams hgch_bar_CatNum
#' @section Ftype:
#' Cat-Yea-Num
#' @examples
#' data <- sample_data("Cat-Yea-Num", n = 30)
#' hgch_bar_CatYeaNum(data)
#'
#' # Activate data labels
#' hgch_bar_CatYeaNum(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Yea-Num-Dat-Yea-Cat", n = 30)
#' hgch_bar_CatYeaNum(data)
#'
#' # Change variable to color and pallete type
#' hgch_bar_CatYeaNum(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_bar_CatYeaNum(tooltip = info_tool)
#'
hgch_bar_CatYeaNum <- hgch_bar_CatCatNum


#' Bar chart Yea Cat Num
#'
#' @description
#' `hgch_bar_YeaCatNum()` Create a highcharter bar plot based on a particular data type.
#' In this case, you can load data with only three columns, where the firts column is a
#' **year column**, second is a **categorical column** and the third must be  a **numeric class column**,
#'  or be sure that three firts columns they meet this condition
#' @export
#' @inheritParams hgch_bar_CatNum
#' @family Yea-Cat-Num plots
#' @section Ftype:
#' Yea-Cat-Num
#' @examples
#' data <- sample_data("Yea-Cat-Num", n = 30)
#' hgch_bar_YeaCatNum(data)
#'
#' # Activate data labels
#' hgch_bar_YeaCatNum(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Yea-Cat-Num-Dat-Yea-Cat", n = 30)
#' hgch_bar_YeaCatNum(data)
#'
#' # Change variable to color and pallete type
#' hgch_bar_YeaCatNum(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_bar_YeaCatNum(tooltip = info_tool)
#'
hgch_bar_YeaCatNum <- hgch_bar_CatCatNum



