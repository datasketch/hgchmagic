#' @keywords internal
hgch_theme <- function(opts = NULL) {
  message("in theme_datasketch")

  highcharter::hc_theme(
    useHTML = TRUE,
    colors = opts$palette_colors,
    styledMode = TRUE,
    chart = list(
      #reflow = TRUE,
      #renderTo = 'container',
      backgroundColor = opts$background_color,

      marginBottom = opts$plot_margin_bottom,
      marginLeft = opts$plot_margin_left,
      marginRight = opts$plot_margin_right,
      marginTop = opts$plot_margin_top,

      plotBackgroundColor = opts$plot_background_color,
      borderColor = opts$plot_border_color,
      borderWidth = opts$plot_border_size,
      style = list (
        fontFamily = opts$text_family,
        fontSize = paste0(opts$text_size, 'px')
      )),
    title = list(
      useHTML = TRUE,
      align = opts$title_align,
      style = list(
        fontFamily = opts$title_family,
        fontSize = paste0(opts$title_size, 'px'),
        color = opts$title_color,
        fontWeight = opts$title_weight
      )
    ),
    subtitle = list(
      useHTML = TRUE,
      align = opts$subtitle_align,
      style = list(
        fontFamily = opts$title_family,
        fontSize = paste0(opts$subtitle_size, 'px'),
        color = opts$subtitle_color,
        fontWeight = opts$subtitle_weight
      )
    ),
    credits = list(
      useHTML = TRUE,
      href = opts$caption_link,
      margin = opts$caption_margin,
      position = list(
        align = opts$caption_align,
        x = ifelse(opts$caption_align == "right",-20, 20),
        y = opts$y_credits
      ),
      style = list(
        fontFamily = opts$title_family,
        fontSize = paste0(opts$caption_size, 'px'),
        color = opts$caption_color
      )
    ),

    xAxis = list(
      visible = opts$grid_x_enabled,
      gridLineWidth = opts$grid_x_width,
      lineColor = opts$axis_line_x_color %||% opts$axis_line_color, #color del eje x
      tickColor = opts$axis_ticks_color,#color de las divisiones del eje x
      gridLineColor = opts$grid_x_color %||% opts$grid_color,
      gridLineDashStyle = opts$grid_x_line_type %||% opts$grid_line_type,
      tickLength = opts$axis_tick_length,
      lineWidth = opts$axis_line_x_size %||% opts$axis_line_size,
      labels = list(
        rotation = opts$labelsRotationX,
        style = list(
          color = opts$axis_title_color %||% opts$text_color, #opts$font_color, #color nombre de las etiquetas
          fontFamily = opts$text_family,
          fontSize = paste0(opts$text_size, 'px')
        )),
      title = list(
        margin = opts$axis_margin_x,
        style = list(
          color = opts$axis_title_color %||% opts$text_color,# color del titulo del eje
          fontSize = paste0(opts$axis_title_size, 'px')
        )
      ),
      plotLines = list(
        list(value = opts$plotLine_value_x,
             color = 'black',
             dashStyle = 'shortdash'#,
             #width = 2,
             #zIndex = 5,
             # label = list(
             #   text = lineLabelsXY[2],
             #   style = list(
             #     color = 'black'
             #   )
             # )
        ))
    ),
    yAxis = list(
      visible = opts$grid_y_enabled,
      gridLineWidth = opts$grid_y_width,
      lineWidth = opts$axis_line_y_size %||% opts$axis_line_size,
      lineColor = opts$axis_line_y_color %||% opts$axis_line_color,
      tickColor = opts$axis_ticks_color,
      gridLineColor = opts$grid_y_color %||% opts$grid_color,
      gridLineDashStyle = opts$grid_y_line_type %||% opts$grid_line_type,
      tickLength = opts$axis_tick_length,
      max = opts$y_max,
      min = opts$y_min,
      labels = list(
        rotation = opts$labelsRotationY,
        style = list(
          color = opts$axis_title_color %||% opts$text_color, #opts$font_color, #color nombre de las etiquetas
          fontFamily = opts$text_family,
          fontSize = paste0(opts$text_size, 'px')
        )),
      title = list(
        margin = opts$axis_margin_y,
        style = list(
          color = opts$axis_title_color %||% opts$text_color,# color del titulo del eje
          fontSize = paste0(opts$axis_title_size, 'px')
        )
      ),
      plotLines = list(
        list(value = opts$plotLine_value_y,
             color = 'black',
             dashStyle = 'shortdash'#,
             #width = 2,
             #zIndex = 5,
             # label = list(
             #   text = lineLabelsXY[2],
             #   style = list(
             #     color = 'black'
             #   )
             # )
        ))
    ),
    plotOptions = list (
      packedbubble = list(
        minSize = opts$bubble_min,
        maxSize = opts$bubble_max,
        animation = list(
          duration = opts$animation_duration
        ),
        # zMin = 0,
        # zMax = 1000,
        layoutAlgorithm = list(
          splitSeries = FALSE,
          gravitationalConstant = 0.02
        ),
        marker= list(
          fillOpacity = opts$bubble_opacity)
      ),

      series = list(
        connectNulls = opts$connect_lines_nulls,
        colorByPoint = opts$color_by_point,
        animation = list(
          duration = opts$animation_duration
        ),
        # dataLabels = list (
        #   enabled = opts$dataLabels_show,
        #   style = labels_style,
        #   inside = opts$dataLabels_inside,
        #   format = opts$templatedataLabels %||% paste0(opts$cats, opts$format_dataLabels),
        #   verticalAlign = opts$dataLabels_align#'middle'
        # ),
        marker = list(
          enabled = opts$marker_enabled,
          symbol = "circle",
          radius = opts$marker_radius
        )
      ),
      pie = list(
        animation = list(
          duration = opts$animation_duration
        ),
        dataLabels = list(distance = ifelse(opts$inner_dataLabels,-100, 30)),
        showInLegend = opts$legend_show
      )
    ),
    legend = list(
      backgroundColor = opts$legend_background,
      borderColor = opts$legend_backgroundBorderColor,
      borderWidth = opts$legend_backgroundWidth,
      maxHeight = opts$legend_maxHeight,
      title = list(
        text = opts$legend_title),
      layout = opts$legend_layout,
      align = opts$legend_align,
      #y = y_legend,
      verticalAlign = opts$legend_verticalAlign,
      itemMarginTop = opts$legend_itemMarginTop,
      itemMarginBottom = opts$legend_itemMarginBottom,
      reversed = opts$legend_reversed,
      itemStyle = list(
        fontFamily = opts$legend_family,
        fontSize = paste0(opts$legend_size %||% opts$text_size, 'px'),
        color = opts$legend_color %||% opts$text_color
      )
    ),
    tooltip = list(
      useHTML = TRUE,
      style = list(
        width = '350px',
        whiteSpace = 'normal',
        fontFamily = opts$tooltip_family %||% opts$text_family,
        fontSize = paste0(opts$text_size, 'px')
      )
    )
  )
}
