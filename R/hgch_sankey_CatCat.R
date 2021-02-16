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

  palette <- opts$theme$palette_colors
  if(is.null(palette)){
    palette <- opts$theme$palette_colors_categorical
  }

  data_dummy <- data[,1:2] %>% dplyr::mutate_all(~paste0(., "_dummy"))
  l <- hgchmagic_prep(data_dummy, opts = opts)
  d <- l$d
  l$theme$legend_show <- FALSE
  l$theme$dataLabels_show <- TRUE
  l$theme$format_dataLabels <- ""

  color_by <- "from"
  if(!is.null(opts$style$color_by)) {
    color_by <- opts$style$color_by
    if(!color_by %in% c("from", "to")){
      stop("Group by parameter must be 'from' or 'to'.")
    }
  }

  for(i in seq(length(names(data)))){
    data[,i] <- data %>% dplyr::select_at(i) %>% dplyr::mutate_all(~paste0(., i))
  }
  data_sankey_format <- highcharter::data_to_sankey(data) %>%
    dplyr::mutate(from_label = substr(from,1,nchar(from)-1),
                  to_label = substr(to,1,nchar(to)-1),
                  name = if(color_by == "to") to_label else from_label)

  nodes_unique <- unique(c(unique(data_sankey_format$from_label), unique(data_sankey_format$to_label)))

  colors <- data.frame(name = nodes_unique) %>%
    dplyr::mutate(color = paletero::paletero(name, as.character(palette)))

  if(!is.null(names(palette))){
    colors <- data.frame(name = nodes_unique) %>%
      dplyr::left_join(
        data.frame(name = names(palette),
                   color = palette, row.names = NULL),
        by = "name") %>%
      dplyr::mutate(color = ifelse(is.na(color), "#cbcdcf", as.character(color)))
  }

  dat <- data_sankey_format %>%
    dplyr::left_join(colors,
              by.x = color_by, by.y = "name") %>%
    dplyr::group_by_at(color_by) %>%
    dplyr::mutate(pct = paste0(round(100*weight / sum(weight), 0), "%")) %>%
    dplyr::group_by(from) %>%
    dplyr::mutate(total_from = sum(weight)) %>%
    dplyr::group_by(to) %>%
    dplyr::mutate(total_to = sum(weight)) %>%
    dplyr::ungroup()

  nodes_from <- dat %>%
    dplyr::distinct(from, from_label, total_from) %>%
    dplyr::rename(id = from, name = from_label, total = total_from) %>%
    dplyr::mutate(pct = paste0(round(100 * total / sum(total), 0), "%"))

  nodes_to <- dat %>%
    dplyr::distinct(to, to_label, total_to) %>%
    dplyr::rename(id = to, name = to_label, total = total_to) %>%
    dplyr::mutate(pct = paste0(round(100 * total / sum(total), 0), "%"))

  nodes <- dplyr::bind_rows(nodes_from, nodes_to) %>%
    dplyr::distinct(id, name, total, pct) %>%
    dplyr::left_join(colors, by = "name") %>%
    purrr::transpose()

  global_options(opts$style$format_sample_num)

  dataLabel <- '{point.name}'
  if(is.null(opts$dataLabels$dataLabels_type)){
    dataLabel <- '{point.name}'
  } else if(opts$dataLabels$dataLabels_type == "percentage"){
    dataLabel <- '{point.name}: {point.pct}'
  } else if(opts$dataLabels$dataLabels_type == "total"){
    dataLabel <- '{point.name}: {point.total}'
  }

  if(!is.null(opts$dataLabels$dataLabels_type)){
    if(!opts$dataLabels$dataLabels_type %in% c("percentage", "total"))
      warning("Datalabel type must be 'total' or 'percentage'.")
  }

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
        nodeFormat = dataLabel
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
    hc_tooltip(pointFormatter = JS("
    function() {
      var result = this.from + ' \u2192 ' + this.to +
                   '<br>Total: <b>' + this.weight + '</b>' +
                   '<br>Percentage: <b>' + this.pct + '</b>';
      return result;
    }")
               ) %>%
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hgch_theme(opts = l$theme))

  hc
}

#' Sankey Cat Cat Cat
#'
#'
#' @param data A data.frame
#' @section
#'
#' @examples
#' hgch_sankey_CatCatCat(sample_data("Cat-Cat-Cat"))
#' @export
hgch_sankey_CatCatCat <- hgch_sankey_CatCat
