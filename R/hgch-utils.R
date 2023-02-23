#' @import dplyr
#' @import highcharter

hc_titles <- function (hc, opts) {
  hc |>
    hc_title(text = opts$title) |>
    hc_subtitle(text = opts$subtitle)  |>
    hc_credits(enabled = opts$caption_show, text = opts$caption)
}

hc_body <- function(hc, data, frType, opts = NULL) {

  #global_options(opts$style$format_sample_num)

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
    #print(data)
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
  }

  hc |>
   hc_tooltip(useHTML = TRUE,
              formatter = JS(paste0("function () {return this.point.label;}"))) |>
    hc_plotOptions(
      series = list(innerSize = opts$inner_size)
    )

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
          # layoutAlgorithm = l$extra$treemap_layout,
          # layoutStartingDirection = l$extra$treemap_direction,
          data = data)
      )
  }

  if (frType == "CatCatNum") {
    hc <- hc
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
        ) %>%
        hc_xAxis(
          type = 'datetime',
          categories = data$categories
        ) |>
        hc_series(
          data$data
        ) |>
      hc_legend(enabled = FALSE)
  }

  hc |>
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}")))

}


