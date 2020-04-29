#' Treemap Chart Cat Cat Numeric
#'
#' This chart does not allow for chaning orientation
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num, Cat-Yea-Num
#' @examples
#' hgch_treemap_CatCatNum(sampleData("Cat-Cat-Num", nrow = 10))
#' @export
hgch_treemap_CatCatNum <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(theme,...)

  f <- homodatum::fringe(data)
  nms <- getFringeLabels(f)
  d <- getFringeDataFrame(f)

  labelsXY <- labelsXY(hor_title = opts$title$hor_title %||% nms[1],
                       ver_title = opts$title$ver_title %||% nms[2],
                       nms = nms, orientation = opts$chart$orientation)

  hor_title <- as.character(labelsXY[1])
  ver_title <- as.character(labelsXY[2])

  d <- preprocessData(d, opts$preprocess$drop_na)
  # d$a[is.na(d$a)] <- 'NA'
  # d$b[is.na(d$b)] <- "NA"
  # Summarize
  d <- summarizeData(d, opts$summarize$agg, to_agg = c, a, b)
  d <- completevalues(d)

  # Postprocess
  d <- postprocess(d, "c", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)
  d <- order_category(d, col = "a", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)

  color_by <- names(nms[match(opts$style$color_by, nms)])
  if (identical(color_by, character())) color_by <- "a"
  palette <- opts$theme$palette_colors
  d$..colors <- paletero::map_colors(d, color_by, palette, colors_df = NULL)
  paleta <- d[,c(color_by, "..colors")] #
  paleta <- paleta %>% distinct(..colors, .keep_all = TRUE)
  print(paleta)
  # paleta <- d %>% distinct(..colors)
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
      value = d$c[z],
      colorValue = d$c[z]
    )

  })

  data <- c(listaId, listaMg)

  if (is.null(opts$tooltip)) opts$tooltip <-paste0('<b>', nms[2], ': </b>{point.name}</br>',
                                                   # '<b>', nms[1], ': </b>{point.node.name}</br>',
                                                   nms[3], ': ', opts$style$prefix,'{point.value}', opts$style$suffix)


  global_options(opts$style$format_num_sample)
  hc <- highchart() %>%
    hc_title(text = opts$title$title) %>%
    hc_subtitle(text = opts$title$subtitle) %>%
    hc_chart(type = ifelse(opts$chart$orientation == "hor","treemap","column"),
             events = list(
               load = add_branding(opts$theme)
             )) %>%
    hc_series(
      list(
        type = "treemap",
        levels = list(list(
          level = 1#,
          # dataLabels = list(
          #   enabled = TRUE,
          #   align = 'left',
          #   verticalAlign = 'top',
          #   style = list(
          #     fontSize = '15px',
          #     fontWeight = 'bold'
          #   )
          # )
        )),
        data = data
      )) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = opts$title$caption %||% "") #%>%
    #hc_legend(enabled = FALSE) %>%
    #hc_add_theme(theme(opts = opts$theme))

  hc
}
