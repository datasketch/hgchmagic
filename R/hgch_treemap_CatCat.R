#' Treemap chart Cat Cat
#'
#' @description
#' `hgch_treemap_CatCat()` Create a highcharter treemap plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts and second columns are
#' **categoricals columns**, or be sure that firts two columns they meet this condition, since it
#' will be done a counting the categories of this columns.
#' @export
#' @param data A data frame, data frame extension (e.g. a tibble), a
#'   lazy data frame (e.g. from dbplyr or dtplyr), or fringe data (e.g from homodatum).
#' @param ... Read <[`chart-options`][hgch_viz_options]> a general options summary to configure your hgchmagic plots
#'   and <[`treemap-options`][hgch_treemap_options]> which specifically contains the additional arguments
#'   that work only for this type of chart
#' @family Cat-Cat plots
#' @section Ftype:
#' Cat-Cat
#' @examples
#' data <- sample_data("Cat-Cat", n = 30)
#' hgch_treemap_CatCat(data)
#'
#' # Activate data labels
#' hgch_treemap_CatCat(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Cat-Num-Yea-Cat", n = 30)
#' hgch_treemap_CatCat(data)
#'
#' # Change variable to color and pallete type
#' hgch_treemap_CatCat(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_treemap_CatCat(tooltip = info_tool)
#'
hgch_treemap_CatCat <- function(data, ...){
  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- hgchmagic_prep(data, opts = opts, plot = "treemap", ftype = "Cat-Cat")

  d <- l$d

  color_by <- "a"#l$color_by
  paleta <- d[,c(color_by, "..colors")]
  paleta <- paleta %>% distinct(..colors, .keep_all = TRUE)

  listaId <- map(1:length(paleta[[color_by]]), function(i) {
    list(
      id = as.character(paleta[[color_by]][i]),
      name = as.character(paleta[[color_by]][i]),
      color = as.character(paleta$..colors[i])
    )
  })

  listaMg <- map(1:nrow(d), function(z) {
    nm <- ifelse(is.na(d$b[z]), "NA", d$b[z])
    list(
      name = nm,
      parent = d$a[z],
      value = d[[3]][z],
      label = d$labels[z],
      colorValue = d[[3]][z]
    )

  })

  data <- c(listaId, listaMg)

  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(events = list(
               load = add_branding(l$theme)
             )) %>%
    hc_series(
      list(
        type = "treemap",
        layoutAlgorithm = l$extra$treemap_layout,
        levels = list(list(
          level = 1
        )),
        data = data
      )) %>%
     hc_tooltip(useHTML = TRUE,
                formatter = JS(paste0("function () {return this.point.label;}")),
                style = list(width = "300px", whiteSpace = "normal")) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
  hc_add_theme(hgch_theme(opts =  c(l$theme,
                               cats = "{point.name} <br/>")))

  hc

}




#' Treemap chart Yea Cat
#'
#' @description
#' `hgch_treemap_YeaCat()` Create a highcharter treemap plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts is a
#' **year column** and the second must be a **categorical column**, or be sure that firts two columns
#' they meet this condition, since it will be done a counting the categories of this columns.
#' @export
#' @inheritParams hgch_treemap_CatCat
#' @family Yea-Cat plots
#' @section Ftype:
#' Yea-Cat
#' @examples
#' data <- sample_data("Yea-Cat", n = 30)
#' hgch_treemap_CatCat(data)
#'
#' # Activate data labels
#' hgch_treemap_YeaCat(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Yea-Cat-Dat-Yea-Cat", n = 30)
#' hgch_treemap_YeaCat(data)
#'
#' # Change variable to color and pallete type
#' hgch_treemap_YeaCat(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[4],":</b> {", names_data[4],"}<br/>")
#' data %>%
#'  hgch_treemap_YeaCat(tooltip = info_tool)
#'
hgch_treemap_YeaCat <- hgch_treemap_CatCat


#' Treemap chart Cat Yea
#'
#' @description
#' `hgch_treemap_CatYea()` Create a highcharter treemap plot based on a particular data type.
#' In this case, you can load data with only two columns, where the firts is a
#' **categorical column** and the second must be a **year column**, or be sure that firts two columns
#' they meet this condition, since it will be done a counting the categories of this columns.
#' @export
#' @inheritParams hgch_treemap_CatCat
#' @family Cat-Yea plots
#' @section Ftype:
#' Cat-Yea
#' @examples
#' data <- sample_data("Cat-Yea", n = 30)
#' hgch_treemap_CatYea(data)
#'
#' # Activate data labels
#' hgch_treemap_CatYea(data,
#'                        dataLabels_show = TRUE)
#'
#' # data with more of one column
#' data <- sample_data("Cat-Yea-Cat-Dat-Num-Cat", n = 30)
#' hgch_treemap_CatYea(data)
#'
#' # Change variable to color and pallete type
#' hgch_treemap_CatYea(data,
#'                        color_by = names(data)[2],
#'                        palette_type = "sequential")
#'
#' # Change tooltip info and add additional information contained in your data
#' names_data <- names(data)
#' info_tool <- paste0("<b>",names_data[1],":</b> {", names_data[1],"}<br/><b>", names_data[3],":</b> {", names_data[3],"}<br/>")
#' data %>%
#'  hgch_treemap_CatYea(tooltip = info_tool)
#'
hgch_treemap_CatYea <- hgch_treemap_CatCat

