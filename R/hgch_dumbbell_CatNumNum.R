#' Dumbbell Cat Num Num
#'
#'
#' @param data A data.frame
#' @section
#'
#' @examples
#' hgch_dumbbell_CatNumNum(sample_data("Cat-Num-Num", nrow = 1000) %>% group_by_at(1) %>% summarise_all(mean))
#' @export
hgch_dumbbell_CatNumNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts)
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

  palette <- paletero::paletero(c("high", "low"), opts$theme$palette_colors)

  dat <- data
  names(dat) <- c("category", "low", "high")

  dat <- dat %>%
    ungroup() %>%
    filter(complete.cases(.)) %>%
    mutate(color_low = palette[2],
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
               useHTML= TRUE,
               pointFormat= '<b style="color:{point.color_low};">{point.label_low}: </b>{point.low}</br>
                                 <b style="color:{point.color_high};">{point.label_high}: </b>{point.high}',
               valueDecimals= 0) %>%
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hgch_theme(opts = l$theme))

  hc
}
