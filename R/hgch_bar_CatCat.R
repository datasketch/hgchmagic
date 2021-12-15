#' Bar chart Cat Cat
#'
#' @description
#' `hgch_bar_CatCat()` Create a highcharter bar plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts and second columns are
#' **categoricals columns**, or be sure that firts two columns they meet this condition, since it
#' will be done a counting the categories of this columns.
#' @export
#' @inheritParams hgch_bar_CatNum
#' @family Cat-Cat plots
#' @section Ftype:
#' Cat-Cat
#' @examples
#' data <- sample_data("Cat-Cat", n = 30)
#' hgch_bar_CatCat(data)
#'
#' # Activate data labels
#' hgch_bar_CatCat(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Cat-Num-Yea-Cat", n = 30)
#' hgch_bar_CatCat(data)
#'
#' # Change variable to color and pallete type
#' hgch_bar_CatCat(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_bar_CatCat(tooltip = info_tool)
#'
hgch_bar_CatCat <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts, ftype = "Cat-Cat", plot = "bar")
  d <- l$d


  series <- purrr::map(unique(d[[1]]), function(i) {
    d0 <- d %>%
      dplyr::filter(a %in% i) #%>% drop_na()
    label_info <- d0 %>% .$labels %>% unlist()
    l0 <- list("name" = i,
               "color" = unique(d0$..colors),
               "data" = purrr::map(seq_along(d0[[3]]), function(i){
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

  hc <- hc %>%  hc_add_theme(hgch_theme(opts = c(l$theme)))


  hc
}

#' Bar chart Yea Cat
#'
#' @description
#' `hgch_bar_YeaCat()` Create a highcharter bar plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts is a
#' **year column** and the second must be a **categorical column**, or be sure that firts two columns
#' they meet this condition, since it will be done a counting the categories of this columns.
#' @export
#' @inheritParams hgch_bar_CatNum
#' @family Yea-Cat plots
#' @section Ftype:
#' Yea-Cat
#' @examples
#' data <- sample_data("Yea-Cat", n = 30)
#' hgch_bar_CatCat(data)
#'
#' # Activate data labels
#' hgch_bar_YeaCat(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Yea-Cat-Dat-Yea-Cat", n = 30)
#' hgch_bar_YeaCat(data)
#'
#' # Change variable to color and pallete type
#' hgch_bar_YeaCat(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_bar_YeaCat(tooltip = info_tool)
#'
hgch_bar_YeaCat <- hgch_bar_CatCat

#' Bar chart Cat Yea
#'
#' @description
#' `hgch_bar_CatYea()` Create a highcharter bar plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts is a
#' **categorical column** and the second must be a **year column**, or be sure that firts two columns
#' they meet this condition, since it will be done a counting the categories of this columns.
#' @export
#' @inheritParams hgch_bar_CatNum
#' @family Cat-Yea plots
#' @section Ftype:
#' Cat-Yea
#' @examples
#' data <- sample_data("Cat-Yea", n = 30)
#' hgch_bar_CatYea(data)
#'
#' # Activate data labels
#' hgch_bar_CatYea(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Yea-Cat-Dat-Num-Cat", n = 30)
#' hgch_bar_CatYea(data)
#'
#' # Change variable to color and pallete type
#' hgch_bar_CatYea(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[3],":</b> {", names_data[3],"}<br/>")
#' data %>%
#'  hgch_bar_CatYea(tooltip = info_tool)
#'
hgch_bar_CatYea <- hgch_bar_CatCat
