#' Sankey Cat Cat
#'
#'
#' @param data A data.frame
#' @section
#'
#' @examples
#' hgch_sankey_CatCat(sample_data("Cat-Cat"))
#' @export
hgch_sankey_CatCat <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)
  l <- hgchmagic_prep(data, opts = opts)
  d <- l$d
  l$theme$legend_show <- FALSE
  l$theme$dataLabels_show <- TRUE
  l$theme$format_dataLabels <- ""

  color_by <- "from"
  if(!is.null(opts$style$color_by)){
    if(!opts$style$color_by %in% names(data)){
      stop("Group by parameter is not a valid column name of the data input.")
    }
    col_idx_group_by <- c(1,2)[names(data) == opts$style$color_by]
    if(col_idx_group_by == 2){
      color_by <- "to"
    }
  }

  data_sankey_format <- data_to_sankey(data) %>%
    mutate(name = if(color_by == "to") to else from)

  nodes_unique <- unique(c(unique(data_sankey_format$from), unique(data_sankey_format$to)))

  colors <- data.frame(name = nodes_unique) %>%
    mutate(color = paletero::paletero(name, opts$theme$palette_colors))

  if(!is.null(names(opts$theme$palette_colors))){
    colors <- data.frame(name = nodes_unique) %>%
      left_join(
        data.frame(name = names(opts$theme$palette_colors),
                   color = opts$theme$palette_colors, row.names = NULL),
        by = "name") %>%
      mutate(color = ifelse(is.na(color), "#cbcdcf", color))
  }

  dat <- data_sankey_format %>%
    left_join(colors,
              by.x = color_by, by.y = "name") %>%
    mutate(from_label = from,
           to_label = to,
           from = paste0(from, "_from"),
           to = paste0(to, "_to"))

  nodes_from <- dat %>% distinct(from, from_label) %>% rename(id = from, name = from_label)
  nodes_to <- dat %>% distinct(to, to_label) %>% rename(id = to, name = to_label)
  nodes <- bind_rows(nodes_from, nodes_to) %>%
    left_join(colors, by = "name") %>% purrr::transpose()

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
      name = "",
      nodes = nodes,
      dataLabels= list(
        nodeFormat = '{point.name}'
      ),
      colorByPoint = TRUE,
      showInLegend = FALSE
    ) %>%
    hc_xAxis(title = list(text = l$titles$x)) %>%
    hc_yAxis(title = list(text = l$titles$y),
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
