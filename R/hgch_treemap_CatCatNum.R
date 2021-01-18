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

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- hgchmagic_prep(data, opts = opts, plot = "treemap", ftype = "Cat-Cat-Num")

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
      value = d$c[z],
      colorValue = d$c[z]
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
     hc_tooltip(useHTML=TRUE, pointFormat = l$tooltip, headerFormat = NULL) %>%
    hc_credits(enabled = TRUE, text = l$title$caption %||% "") %>%
  hc_add_theme(hgch_theme(opts =  c(l$theme,
                               cats = "{point.name} <br/>")))

  hc

}


#' treemap Chart Cat Cat Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_treemap_CatCat(sample_data("Cat-Cat", nrow = 10))
#' @export
hgch_treemap_CatCat <- hgch_treemap_CatCatNum


#' treemap Chart Yea Cat
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_treemap_YeaCat(sample_data("Yea-Cat", nrow = 10))
#' @export
hgch_treemap_YeaCat <- hgch_treemap_CatCatNum


#' treemap Chart Cat Yea
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_treemap_CatYea(sample_data("Cat-Yea", nrow = 10))
#' @export
hgch_treemap_CatYea <- hgch_treemap_CatCatNum


#' treemap Chart Cat Yea Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_treemap_CatYeaNum(sample_data("Cat-Yea-Num", nrow = 10))
#' @export
hgch_treemap_CatYeaNum <- hgch_treemap_CatCatNum


#' treemap Chart Yea Cat Num
#'
#'
#' @param data A data.frame
#' @section ctypes:
#' Cat-Cat-Num
#' @examples
#' hgch_treemap_YeaCatNum(sample_data("Yea-Cat-Num", nrow = 10))
#' @export
hgch_treemap_YeaCatNum <- hgch_treemap_CatCatNum

