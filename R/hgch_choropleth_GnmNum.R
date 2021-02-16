#' map (gnm, numbers)
#' @param data A data.frame
#' @export hgch_choropleth_GnmNum
hgch_choropleth_GnmNum <-  function(data = NULL, ...) {

  opts <- dsvizopts::merge_dsviz_options(...)
  map_name <- "countries/co/co-all" #opts$extra$map_name_hgch
  if (is.null(data)) {
    hc <- hcmap(map_name,
                showInLegend = F)
  }

  l <- hgchmagic_prep(data, opts = opts, plot = "map")
  d <- l$d


  d$name_alt <- iconv(tolower(gsub("<br/>", " ",d$a)), to = "ASCII//TRANSLIT")
  mapV <- highcharter::get_data_from_map(highcharter::download_map_data(map_name)) %>% tidyr::drop_na(name)

  mapV$name_alt <- iconv(tolower(mapV$name), to = "ASCII//TRANSLIT")

  d <- dplyr::left_join(d, mapV, by = 'name_alt')

  colors <- opts$theme$palette_colors

  global_options(opts$style$format_num_sample)
  hc <- hcmap(map_name,
              data = d,
              value = "b",
              joinBy = 'hc-a2',
              borderColor = opts$theme$border_color,
              nullColor = opts$theme$na_color,
              showInLegend = FALSE
  ) %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = l$tooltip, headerFormat = NULL) %>%
    hc_colorAxis(minColor = colors[1], maxColor = colors[2]) %>%
    hc_credits(enabled = F, text = l$title$caption) %>%
    hc_legend(enabled = opts$theme$legend_show,
              layout= opts$theme$legend_layout,
              align= 'left',
              verticalAlign = opts$theme$legend_verticalAlign) %>%
  hc_add_theme(hgch_theme(opts = l$theme)) %>%
    hc_plotOptions(
      series = list(
        states = list(
          hover = list(
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
    hc_mapNavigation(
      enabled = opts$theme$map_zoom
     )

  hc

}
