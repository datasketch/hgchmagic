#' @import dplyr
#' @import highcharter

hc_titles <- function (hc, opts) {
  hc |>
    hc_title(text = opts$title) |>
    hc_subtitle(text = opts$subtitle)  |>
    hc_credits(enabled = opts$caption_show, text = opts$caption)
}

hc_body <- function(hc, data, frType, opts = NULL) {

  hc <-   hc |>
    hc_chart(type = opts$plot_type#,
             # events = list(
             #   load = add_branding(opts$theme)
             # )
    )


  if (frType == "CatNum") {
    hc <- hc |>
      hc_series(
        data
      ) |>
      hc_xAxis(title = list(text = opts$hor_title),
               type = "category") |>
      hc_yAxis(title = list(text = opts$ver_title)#,
               #reversed = opts$reversed_yaxis
               # labels = list(
               #   formatter = l$formats)
      ) |>
      hc_legend(enabled = FALSE)
  }

  if (frType == "CatCatNum") {
    hc <- hc |>
      hc_add_series_list(
        data$data
      ) |>
      hc_xAxis(title = list(text = opts$hor_title),
               categories = data$categories,
               type = "category"#,
               # labels = list(
               #   formatter = l$formatter_x_js#,
               #   #step = l$extra$labelsStepX,
               # )
      ) |>
      hc_yAxis(title = list(text = opts$ver_title)#,
               #reversed = opts$reversed_yaxis#,
               # labels = list(
               #   align= l$y_axis_align,
               #   formatter = l$formatter_js %||% l$formats)
      )

    if (opts$graph_type == "stacked"){
      hc <- hc |>
        hc_plotOptions(bar = list(stacking = "normal"), column = list(stacking = "normal"))
      if (opts$percentage) {
        hc <- hc |>
          hc_plotOptions(
            bar = list(
              stacking = 'percent'
            ),
            column = list(
              stacking = 'percent'
            )
          ) |>
          hc_yAxis(maxRange = 100,
                   max = 100)
      }
    }
  }

  #
  if (frType == "CatNumNum") {
    hc <- hc |>
      hc_chart(
        zoomType = 'xy'
      ) |>
      hc_xAxis(
        type = "category",
        categories = data$categories,
        title = list(text = opts$hor_title)
      ) |>
      hc_yAxis_multiples(
        list(title = list(text = opts$axis_left_title %||% data$title_axis[1])),
        list(title = list(text = opts$axis_rigth_title %||% data$title_axis[2]),
             opposite = TRUE)
      ) |>
      hc_tooltip(useHTML = TRUE,
                 shared = TRUE) |>
      hc_add_series_list(
        data$data
      )
  }
  if (frType == "CatNumNum") {
    hc <- hc
  } else {
    hc <- hc |>
      hc_tooltip(useHTML = TRUE,
                 formatter = JS(paste0("function () {return this.point.label;}"))) |>
      hc_plotOptions(
        series = list(innerSize = opts$inner_size)
      )
  }
  hc

}



hc_body_treemap <- function(hc, data, frType, opts = NULL) {

  #global_options(opts$style$format_sample_num)

  hc <- hc

  if (frType == "CatNum") {
    hc <- hc |>
      hc_series(
        list(
          type = 'treemap',
          # showInLegend = l$theme$legend_show,
          # legendType = 'point',
          #layoutAlgorithm = "squarified",
          # layoutStartingDirection = l$extra$treemap_direction,
          data = data)
      )
  }

  if (frType == "CatCatNum") {
    hc <- hc |>
      hc_series(
        list(
          type = "treemap",
          layoutAlgorithm = opts$treemap_layout,
          alternateStartingDirection = TRUE,
          levels = list(
            list(
              level = 1,
              borderWidth = opts$treemap_borderWidth_levelOne,
              borderColor = opts$treemap_borderColor_levelOne,
              dataLabels = list(
                enabled = TRUE,
                align = 'left',
                verticalAlign = 'top',
                style = list(
                  fontSize = '15px',
                  fontWeight = 'bold'
                )
              )
            ),
            list(
              level = 2,
              dataLabels = list(
                enabled = TRUE
              )
            )
          ),
          data = data
        ))
  }

  hc |>
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}")))

}


hc_body_line <- function(hc, data, frType, opts = NULL) {

  #global_options(opts$style$format_sample_num)

  hc <- hc

  if (frType == "DatNum") {
    hc <- hc |>
      hc_chart(type = opts$plot_type
      ) |>
      hc_xAxis(
        type = 'datetime',
        categories = data$categories,
        title = list(text = opts$hor_title)
      ) |>
      hc_yAxis(title = list(text = opts$ver_title)) |>
      hc_series(
        data$data
      ) |>
      hc_legend(enabled = FALSE)
  }

  if (frType == "CatDatNum") {
    hc <- hc |>
      hc_chart(type = opts$plot_type
      ) |>
      hc_xAxis(
        type = 'datetime',
        categories = data$categories,
        title = list(text = opts$hor_title)
      ) |>
      hc_yAxis(title = list(text = opts$ver_title)) |>
      hc_add_series_list(
        data$data
      ) |>
      hc_legend(enabled = opts$legend_show)
  }

  if (frType == "DatNumNum") {

    hc <- hc |>
      hc_chart(
        zoomType = 'xy'
      ) |>
      hc_xAxis(
        type = 'datetime',
        categories = data$categories,
        title = list(text = opts$hor_title)
      ) |>
      hc_yAxis_multiples(
        list(title = list(text = opts$axis_left_title %||% data$title_axis[1])),
        list(title = list(text = opts$axis_rigth_title %||% data$title_axis[2]),
             opposite = TRUE)
      ) |>
      hc_add_series_list(
        data$data
      )
  }

  if (frType == "DatNumNum") {
    hc <- hc
  } else {
    hc <- hc |>
      hc_tooltip(useHTML = TRUE,
                 formatter = JS(paste0("function () {return this.point.label;}")))
  }

  hc

}


hc_body_scatter <- function(hc, data, frType, opts = NULL) {

  #global_options(opts$style$format_sample_num)

  hc <- hc

  if (frType == "NumNum") {
    hc <- hc |>
      hc_chart(
        #type = "scatter",
        zoomType = 'xy'
      ) |>
      hc_boost(
        enabled = TRUE,
        useGPUTranslations = TRUE,
        usePreAllocated = TRUE
      ) |>
      hc_xAxis(
        title = list(text = opts$hor_title)
      ) |>
      hc_yAxis(
        title = list(text = opts$ver_title)
      ) |>
      hc_add_series(
        type = 'scatter',
        data = data,
        marker = list(
          radius = opts$marker_size
        )
      ) |>
      hc_legend(enabled = FALSE)
  }
  if (frType == "CatNumNum") {
    hc <- hc |>
      hc_chart(
        type = "scatter",
        zoomType = 'xy'
      ) |>
      hc_boost(
        enabled = TRUE,
        useGPUTranslations = TRUE,
        usePreAllocated = TRUE
      ) |>
      hc_xAxis(
        title = list(text = opts$hor_title)
      ) |>
      hc_yAxis(
        title = list(text = opts$ver_title)
      ) |>
      hc_add_series_list(
        data
      ) |>
      hc_legend(enabled = opts$legend_show)
  }
  if (frType == "CatDatNum") {
    hc <- hc |>
      hc_chart(
        type = "scatter"
      ) |>
      hc_xAxis(
        title = list(text = opts$hor_title),
        type = 'datetime'#,
        # labels = list(
        #   formatter = JS("function() {return Highcharts.dateFormat('%d-%b-%y', (this.value));}")
        # )
      ) |>
      hc_yAxis(
        title = list(text = opts$ver_title)
      ) |>
      hc_add_series_list(
        data
      ) |>
      hc_legend(enabled = opts$legend_show)
  }

  hc |>
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}")))

}


hc_body_sankey <- function(hc, data, frType, opts = NULL) {

  hc <- hc |>
    hc_chart(type = 'sankey',
             polar = FALSE) |>
    hc_add_series(
      keys = list('from', 'to', 'weight', 'color', 'label'),
      data = data$data,
      nodes = data$nodes,
      linkOpacity = 0.7,
      opacity = 1,
      minLinkWidth = 5,
      nodeWidth = 15,
      name = opts$legend_title,
      clip = F,
      dataLabels = list(
        align= 'left',
        verticalAlign= 'middle',
        crop=FALSE,
        overflow=T
      )
    ) |>
    hc_tooltip(
      headerFormat = opts$sankey_series %||% ""
    )

  hc #|>
  #hc_tooltip(useHTML = TRUE,
  #           formatter = JS(paste0("function () {return this.point.label;}")))

}
