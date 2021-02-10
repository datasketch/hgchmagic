#' Sankey chart Cat Cat
#'
#' @description
#' `hgch_sankey_CatCat()` Create a highcharter sankey plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts and second columns are
#' **categoricals columns**, or be sure that firts two columns they meet this condition, since it
#' will be done a counting the categories of this columns.
#' @export
#' @param data A data frame, data frame extension (e.g. a tibble), a
#'   lazy data frame (e.g. from dbplyr or dtplyr), or fringe data (e.g from homodatum).
#' @param ... Read \code{\link[dsvizopts]{chart_viz_options}} a general options summary to configure your hgchmagic plots
#'   and <[`sankey-options`][hgch_sankey_options]> which specifically contains the additional arguments
#'   that work only for this type of chart.
#' @family Cat-Cat plots
#' @section Ftype:
#' Cat-Cat
#' @examples
#' data <- sample_data("Cat-Cat", n = 30)
#' hgch_sankey_CatCat(data)
#'
#' # Activate data labels
#' hgch_sankey_CatCat(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Cat-Num-Yea-Cat", n = 30)
#' hgch_sankey_CatCat(data)
#'

hgch_sankey_CatCat <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")
  opts <- dsvizopts::merge_dsviz_options(...)

  palette <- opts$theme$palette_colors
  if(is.null(palette)){
    palette <- opts$theme$palette_colors_categorical
  }

  data_dummy <- data[,1:2] %>% mutate_all(~paste0(., "_dummy"))
  l <- hgchmagic_prep(data_dummy, opts = opts, plot = "sankey", ftype = "Cat-Cat")
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
    data[,i] <- data %>% select_at(i) %>% mutate_all(~paste0(., i))
  }
  data_sankey_format <- data_to_sankey(data) %>%
    mutate(from_label = substr(from,1,nchar(from)-1),
           to_label = substr(to,1,nchar(to)-1),
           name = if(color_by == "to") to_label else from_label)

  nodes_unique <- unique(c(unique(data_sankey_format$from_label), unique(data_sankey_format$to_label)))

  colors <- data.frame(name = nodes_unique, color = paletero::paletero(nodes_unique, as.character(palette))) %>%
    mutate_all(as.character)

  if(!is.null(names(palette))){
    colors <- data.frame(name = nodes_unique) %>%
      left_join(
        data.frame(name = names(palette),
                   color = palette, row.names = NULL),
        by = "name") %>%
      mutate(color = ifelse(is.na(color), "#cbcdcf", as.character(color)))
  }

  dat <- data_sankey_format %>%
    left_join(colors,
              by.x = color_by, by.y = "name") %>%
    group_by_at(color_by) %>%
    mutate(pct = paste0(round(100*weight / sum(weight), 0), "%")) %>%
    group_by(from) %>% mutate(total_from = sum(weight)) %>%
    group_by(to) %>% mutate(total_to = sum(weight)) %>%
    ungroup()

  nodes_from <- dat %>% distinct(from, from_label, total_from) %>%
    rename(id = from, name = from_label, total = total_from) %>%
    mutate(pct = paste0(round(100 * total / sum(total), 0), "%"))

  nodes_to <- dat %>% distinct(to, to_label, total_to) %>%
    rename(id = to, name = to_label, total = total_to) %>%
    mutate(pct = paste0(round(100 * total / sum(total), 0), "%"))

  nodes <- bind_rows(nodes_from, nodes_to) %>% distinct(id, name, total, pct) %>%
    left_join(colors, by = "name") %>% purrr::transpose()

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
      var result = this.from_label + ' \u2192 ' + this.to_label +
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
