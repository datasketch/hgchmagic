#' map (gnm, numbers)
#' @param data A data.frame
#' @export hgch_map_choropleth_GnmNum
hgch_map_choropleth_GnmNum <-  function(data = NULL,
                             mapName = "countries/co/co-all",
                             opts = NULL, ...) {

  if (is.null(data)) {
   h <-  hcmap(mapName,
               showInLegend = F)
  }

  opts <- getOptions(opts = opts)


  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)


  if (opts$dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(opts$agg, b))
  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- 'NA'

  if (is.null(opts$nDigits)) {
    nDig <- 0
  } else {
    nDig <- opts$nDigits
  }

  if (opts$percentage) {
    d$b <- (d[['b']] * 100) / sum(d[['b']], na.rm = TRUE)
  }

  d$b <- round(d$b, nDig)

  if (is.null(opts$prefix)) opts$prefix <- ""
  if (is.null(opts$suffix)) opts$suffix <- ""


  if (opts$percentage & opts$suffix == "") {
    opts$suffix <- "%"
  }


  d$name_alt <- iconv(tolower(d$a), to = "ASCII//TRANSLIT")
  mapV <- get_data_from_map(download_map_data(mapName))
  mapV$name_alt <- iconv(tolower(mapV$name), to = "ASCII//TRANSLIT")


  d <- left_join(d, mapV, by = 'name_alt')


  colorDefault <- c("#F75E64", "#53255E")

  if (!is.null(opts$colors)) {
    opts$colors <- opts$colors
  } else {
    opts$colors <- colorDefault
  }


  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(prefix_agg, ' ' ,nms[2], ': '), opts$prefix,'{point.value}', opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- ""
  }

  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)
  hc <- hcmap(mapName,
              data = d,
              value = "b",
              joinBy = 'hc-a2',
              borderColor = opts$border_color,
              nullColor = opts$null_color,
              showInLegend = FALSE
  ) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
    hc_colorAxis(minColor = opts$colors[1], maxColor = opts$colors[2]) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(enabled = opts$legend_show,
              layout= 'vertical',
              align= 'left',
              verticalAlign = 'bottom') %>%
    hc_plotOptions(
      series = list(
        states = list(
          hover = list(
            color = opts$color_hover
          ),
          select = list(
            color = opts$color_click
          )
        ),
        allowPointSelect= opts$allow_point,
        cursor =  opts$cursor,
        events = list(
          click = opts$clickFunction
        )
      )) %>%
    hc_mapNavigation(
      enabled = opts$map_navigation
    ) %>%
    hc_credits(enabled = TRUE, text = caption)

  if (opts$export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}

  if (is.null(opts$theme)) {
    hc <- hc %>% hc_add_theme(tma(custom = list(showText = opts$showText, colors = opts$colors)))
  } else {
    hc <- hc %>% hc_add_theme(opts$theme)
  }

  hc

}

#' map (gnm)
#' @param data A data.frame
#' @export hgch_map_choropleth_Gnm
hgch_map_choropleth_Gnm <-  function(data = NULL,
                                     mapName = "countries/co/co-all",
                          opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(b = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "count ", opts$agg_text)

  names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label))

  opts$agg_text <- " "
  h <- hgch_map_choropleth_GnmNum(data = d, mapName = mapName, opts = opts, ...)
  h
}


#' map (gnm, numbers)
#' @param data A data.frame
#' @export hgch_map_bubbles_CatGltGlnNum
hgch_map_bubbles_CatGltGlnNum <-  function(data = NULL,
                                           mapName = "countries/co/co-all",
                                           opts = NULL, ...) {

  if (is.null(data)) {
    h <-  hcmap(mapName,
                showInLegend = F)
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)

  d <- f$d

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)
  d <- d %>% drop_na(b,c,d)

  if (opts$dropNa)
    d <- d %>%
    tidyr::drop_na(a)

  colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")

  if (!is.null(opts$colors)) {
    opts$colors <- unname(fillColors(d, "a", opts$colors, opts$color_scale))
  } else {
    opts$colors <- colorDefault
  }

  d$a[is.na(d$a)] <- "NA"

  if (is.null(opts$nDigits)) {
    nDig <- 0
  } else {
    nDig <- opts$nDigits
  }

  if (opts$percentage) {
    d$b <- (d[['d']] * 100) / sum(d[['d']], na.rm = TRUE)
  }

  d$b <- round(d$b, nDig)

  if (is.null(opts$prefix)) opts$prefix <- ""
  if (is.null(opts$suffix)) opts$suffix <- ""


  if (opts$percentage & opts$suffix == "") {
    opts$suffix <- "%"
  }



  df <- d %>%
    select(name = a, z = d, lat = b , lon = c, everything())


  dt_color <- data.frame(name = unique(df$name), color = discreteColorSelect(opts$colors, d))
  df <- df %>% left_join(dt_color, by = "name")

  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(prefix_agg, ' ' ,nms[4], ': '), opts$prefix,'{point.z}', opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- ""
  }

  h <- hcmap(mapName,
             showInLegend = F,
             borderColor = opts$border_color) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_mapNavigation(
      enabled = opts$map_navigation
    )

  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)
  # h <- map(unique(df$name), function(w) {
  #   df_c <- df %>% dplyr::filter(name %in% w)
    h <- h %>% hc_add_series(
      type = "mapbubble",
      showInLegend = F,
      #name = unique(df_c$name),
      #color = unique(df_c$color),
      data = df,#df_c,
      marker= list(
        fillOpacity = opts$fill_opacity,
        states = list(
          hover = list(
            fillColor = opts$color_hover
          ),
          select = list(
            fillColor = opts$color_click
          ))
      ),
      borderWidth = opts$border_width,
      minSize = opts$bubble_min,
      maxSize = opts$bubble_max,
      allowPointSelect= opts$allow_point,
      cursor =  opts$cursor,
      events = list(
        click = opts$clickFunction
      )
    ) %>%
      hc_exporting(enabled = opts$export, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        ))) %>%
      hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat)

    if (is.null(opts$theme)) {
      h <- h %>% hc_add_theme(tma(custom = list(line_width = 0, showText = opts$showText)))
    } else {
      h <- h %>% hc_add_theme(opts$theme)
    }

  #})
  h

}


