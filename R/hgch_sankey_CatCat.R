#' Dumbbell Cat Num Num
#'
#'
#' @param data A data.frame
#' @section
#'
#' @examples
#' hgch_dumbbell_CatNumNum(sample_data("Cat-Num-Num", nrow = 1000) %>% group_by_at(1) %>% summarise_all(mean))
#' @export
hgch_sankey_CatCat <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts)
  d <- l$d
  l$theme$legend_show <- FALSE
  l$theme$dataLabels_show <- TRUE
  l$theme$format_dataLabels <- ""
  l$theme$border_color <- NULL
  title_x = l$titles$x
  title_y = l$titles$y
  if(!is.null(opts$title$hor_title)){
    if(opts$title$hor_title == ""){
      title_x <- opts$title$hor_title}}
  if(!is.null(opts$title$ver_title)){
    if(opts$title$ver_title == ""){
      title_y <- opts$title$ver_title}}

  color_by <- "from"
  if(!is.null(opts$style$color_by)){
    if(!opts$style$color_by %in% names(data)){
      stop("Group by parameter is not a valid column name of the data input.")
    }
    col_idx_group_by <- c(1,2)[names(data) == "color"]
    if(col_idx_group_by == 2){
      color_by <- "to"
    }
  }

  dat <- data_to_sankey(data)

  nodes <- dat %>%
    mutate(name = if(color_by == "to") to else from,
           color = paletero::paletero(name, opts$theme$palette_colors),
           from = ifelse(from == to, paste0(from, "_from"), from),
           to = ifelse(from == to, paste0(to, "_to"), to)) %>%
    distinct(from, name, color)

  names(nodes) <- c("id", "name", "color")

  nodes <- nodes %>% purrr::transpose()

  dat <- dat %>%
    mutate(name = if(color_by == "to") to else from,
           color = paletero::paletero(name, opts$theme$palette_colors),
           from = ifelse(from %in% unique(dat$to), paste0(from, "_from"), from))

  global_options(opts$style$format_sample_num)

  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(
      type = "sankey",
      polar = FALSE,
      inverted = FALSE,
      events = list(
        load = add_branding(opts$theme)
      )
    ) %>%
    hc_add_series(
      dat,
      nodes = nodes,
      dataLabels= list(
        nodeFormat = '{point.name}'
      ),
      colorByPoint = TRUE,
      showInLegend = FALSE
    ) %>%
    hc_xAxis(title = list(text = title_x)) %>%
    hc_yAxis(title = list(text = title_y),
             visible = TRUE,
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
      hc_tooltip(outside = TRUE) %>%
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hgch_theme(opts = l$theme))

  hc
}
