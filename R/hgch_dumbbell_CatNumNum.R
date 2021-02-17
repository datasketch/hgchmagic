#' dumbbell chart Num Num
#'
#' @description
#' `hgch_dumbbell_CatNumNum()` Create a highcharter dumbbell plot based on a particular data type.
#' In this case, you can load data with only three columns,  where the first is
#' a **categorical column**, and second and third columns are
#' **numeric class**, or be sure that two firts columns they meet this condition
#'
#' @export
#' @param data A data frame, data frame extension (e.g. a tibble), a
#'   lazy data frame (e.g. from dbplyr or dtplyr), or fringe data (e.g from homodatum).
#' @param ... Read \code{\link[dsvizopts]{chart_viz_options}} a general options summary to configure your hgchmagic plots.
#' @family Cat-Num-Num plots
#' @section Ftype:
#' Cat-Num-Num
#' @examples
#' data <- sample_data("Cat-Num-Num", n = 30)
#' hgch_dumbbell_CatNumNum(data)
#'
#' example with iris data
#' data <- iris %>% select(Species, Petal.Width, Petal.Length)
#' hgch_dumbbell_CatNumNum(data)
#'

#' # data with more of one column
#' data <- sample_data("Cat-Num-Num-Dat-Cat-Cat", n = 30)
#' hgch_dumbbell_CatNumNum(data)
#'
hgch_dumbbell_CatNumNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts,  ftype = "Cat-Num-Num")
  d <- l$d
  l$theme$legend_show <- FALSE
  title_x = l$titles$x
  title_y = l$titles$y
  if(!is.null(opts$title$hor_title)){
    if(opts$title$hor_title == ""){
      title_x <- opts$title$hor_title}}
  if(!is.null(opts$title$ver_title)){
    if(opts$title$ver_title == ""){
      title_y <- opts$title$ver_title}}


  palette <- paletero::paletero(c("high", "low"), opts$theme$palette_colors_categorical)


  dat <- data
  names(dat) <- c("category", "low", "high")

  dat <- dat %>%
    dplyr::ungroup() %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::mutate(color_low = palette[2],
                  color_high = palette[1],
                  label_low = names(data)[2],
                  label_high = names(data)[3])

  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(
      type = "dumbbell",
      polar = FALSE,
      inverted = TRUE,
      events = list(
        load = add_branding(opts$theme)
      )
    ) %>%
    hc_add_series(
      dat,
      showInLegend = FALSE,
      marker = list(fillColor = palette[1]),
      lowColor= palette[2],
      connectorColor = "#8a8a8a"
    ) %>%
    hc_xAxis(title = list(text = title_x),
             categories = dat$category) %>%
    hc_yAxis(title = list(text = title_y),
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
    hc_tooltip(shared= TRUE,
               useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}")),
               style = list(width = "300px", whiteSpace = "normal"),
               pointFormat= '<b style="color:{point.color_low};">{point.label_low}: </b>{point.low}</br>
                                 <b style="color:{point.color_high};">{point.label_high}: </b>{point.high}',
               valueDecimals= 0) %>%
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hgch_theme(opts = l$theme))

  hc
}


#' Dumbbell chart Cat Cat
#'
#' @description
#' `hgch_dumbbell_CatCat()` Create a highcharter dumbbell plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts and second columns are
#' **categoricals columns**, or be sure that firts two columns they meet this condition, since it
#' will be done a counting the categories of this columns.
#' @export
#' @inheritParams hgch_dumbbell_CatNumNum
#' @family Cat-Cat plots
#' @section Ftype:
#' Cat-Cat
#' @examples
#' data <- sample_data("Cat-Cat", n = 30)
#' hgch_dumbbell_CatCat(data)
#'
#' # Activate data labels
#' hgch_dumbbell_CatCat(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Cat-Num-Yea-Cat", n = 30)
#' hgch_dumbbell_CatCat(data)
#'
#' # Change variable to color and pallete type
#' hgch_dumbbell_CatCat(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_dumbbell_CatCat(tooltip = info_tool)
#'
hgch_dumbbell_CatCat <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")
  if (length(data[,2] %>% dplyr::distinct() %>% dplyr::pull()) != 2) stop("The second column should contain two unique categories.")

  opts <- dsvizopts::merge_dsviz_options(...)
  data <- data[c(1,2)] %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(n = dplyr::n()) %>%
    tidyr::spread(key = 2, value = 3)

  hgch_dumbbell_CatNumNum(data, opts = opts)

}


#' Dumbbell chart Cat Cat Num
#'
#' @description
#' `hgch_dumbbell_CatCatNum()` Create a highcharter dumbbell plot based on a particular data type.
#' In this case, you can load data with only three columns, where the firts and second columns are
#' **categoricals columns** and the third must be  a **numeric class column**, or be sure that
#' three firts columns they meet this condition
#' @export
#' @inheritParams hgch_dumbbell_CatNumNum
#' @family Cat-Cat-Num plots
#' @section Ftype:
#' Cat-Cat-Num
#' @examples
#' data <- sample_data("Cat-Cat-Num", n = 30)
#' hgch_dumbbell_CatCatNum(data)
#'
#' # Activate data labels
#' hgch_dumbbell_CatCatNum(data,
#'                        dataLabels_show = TRUE)
#'
#' # if you want to calculate the average instead of the sum, you can use agg inside a function
#' hgch_dumbbell_CatCatNum(data,
#'                        agg = "mean",
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Cat-Num-Dat-Yea-Cat", n = 30)
#' hgch_dumbbell_CatCatNum(data)
#'
#' # Change variable to color and pallete type
#' hgch_dumbbell_CatCatNum(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_dumbbell_CatCatNum(tooltip = info_tool)
#'
hgch_dumbbell_CatCatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")
  if (length(data[,2] %>% dplyr::distinct() %>% dplyr::pull()) != 2) stop("The second column should contain two unique categories.")

  opts <- dsvizopts::merge_dsviz_options(...)

  data <- data[c(1:3)]
  names(data)[3] <- "c"

  agg <- opts$summarize$agg
  data <- data %>%
    dplyr::group_by_at(c(1,2)) %>%
    dplyr::summarise(c := agg(agg, c)) %>%
    dplyr::mutate(c=as.numeric(c)) %>%
    tidyr::spread(key = 2, value = 3)

  hgch_dumbbell_CatNumNum(data, opts = opts)

}
