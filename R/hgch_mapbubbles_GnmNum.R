#' #' map (gnm, numbers)
#' #' @param data A data.frame
#' #' @export hgch_mapbubbles_GltGlnNum
#' hgch_mapbubbles_GltGlnNum <-  function(data = NULL, ...) {
#'
#'
#'   if (is.null(data)) {
#'     h <-  hcmap(mapName,
#'                 showInLegend = F)
#'   }
#'
#'   opts <- getOptions(opts = opts)
#'
#'   f <- fringe(data)
#'   nms <- getClabels(f)
#'
#'   d <- f$d
#'
#'   title <-  opts$title %||% ""
#'   subtitle <- opts$subtitle %||% ""
#'   caption <- opts$caption %||% ""
#'
#'   prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)
#'   d <- d %>% drop_na(b,c,d)
#'
#'   if (opts$dropNa)
#'     d <- d %>%
#'     tidyr::drop_na(a)
#'
#'   colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
#'
#'   if (!is.null(opts$colors)) {
#'     opts$colors <- unname(fillColors(d, "a", opts$colors, opts$color_scale))
#'   } else {
#'     opts$colors <- colorDefault
#'   }
#'
#'   d$a[is.na(d$a)] <- "NA"
#'
#'   if (is.null(opts$nDigits)) {
#'     nDig <- 0
#'   } else {
#'     nDig <- opts$nDigits
#'   }
#'
#'   if (opts$percentage) {
#'     d$d <- (d[['d']] * 100) / sum(d[['d']], na.rm = TRUE)
#'   }
#'
#'   d$d <- round(d$d, nDig)
#'
#'   if (is.null(opts$prefix)) opts$prefix <- ""
#'   if (is.null(opts$suffix)) opts$suffix <- ""
#'
#'
#'   if (opts$percentage & opts$suffix == "") {
#'     opts$suffix <- "%"
#'   }
#'
#'
#'
#'   df <- d %>%
#'     select(name = a, z = d, lat = b , lon = c, everything())
#'
#'
#'   dt_color <- data.frame(name = unique(df$name), color = discreteColorSelect(opts$colors, d))
#'   df <- df %>% left_join(dt_color, by = "name")
#'
#'   if (is.null(opts$tooltip$pointFormat)) {
#'     opts$tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(prefix_agg, ' ' ,nms[4], ': '), opts$prefix,'{point.z}', opts$suffix)
#'   }
#'   if (is.null(opts$tooltip$headerFormat)) {
#'     opts$tooltip$headerFormat <- ""
#'   }
#'
#'   h <- hcmap(mapName,
#'              showInLegend = F,
#'              borderColor = opts$border_color) %>%
#'     hc_title(text = title) %>%
#'     hc_subtitle(text = subtitle) %>%
#'     hc_credits(enabled = TRUE, text = caption) %>%
#'     hc_mapNavigation(
#'       enabled = opts$map_navigation
#'     )
#'
#'   global_options(opts$marks[1], opts$marks[2])
#'   exportLang(language = opts$lang)
#'   # h <- map(unique(df$name), function(w) {
#'   #   df_c <- df %>% dplyr::filter(name %in% w)
#'   h <- h %>% hc_add_series(
#'     type = "mapbubble",
#'     showInLegend = F,
#'     #name = unique(df_c$name),
#'     #color = unique(df_c$color),
#'     data = df,#df_c,
#'     marker= list(
#'       fillOpacity = opts$fill_opacity,
#'       states = list(
#'         hover = list(
#'           fillColor = opts$color_hover
#'         ),
#'         select = list(
#'           fillColor = opts$color_click
#'         ))
#'     ),
#'     borderWidth = opts$border_width,
#'     minSize = opts$bubble_min,
#'     maxSize = opts$bubble_max,
#'     allowPointSelect= opts$allow_point,
#'     cursor =  opts$cursor,
#'     events = list(
#'       click = opts$clickFunction
#'     )
#'   ) %>%
#'     hc_exporting(enabled = opts$export, buttons= list(
#'       contextButton= list(
#'         menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
#'       ))) %>%
#'     hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat)
#'
#'   if (is.null(opts$theme)) {
#'     h <- h %>% hc_add_theme(tma(custom = list(line_width = 0, showText = opts$showText)))
#'   } else {
#'     h <- h %>% hc_add_theme(opts$theme)
#'   }
#'
#'   #})
#'   h
#'
#' }
