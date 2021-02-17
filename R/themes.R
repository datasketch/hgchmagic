#' @export
global_options <- function(sample){
  params <- makeup::which_num_format(sample)
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- params$separators$thousands
  hcoptslang$decimalPoint <- params$separators$decimal
  options(highcharter.lang = hcoptslang)
}

url_logo <- function(logo, background_color) {
  isUrl <- grepl("http", logo)
  if (isUrl) logo_url <- logo
  if (grepl("/", logo) & !isUrl) {
    logo_path <- logo
  } else {
    logo_path <- dsvizopts::local_logo_path(logo, background_color)
  }
  logo_url <- knitr::image_uri(f = logo_path)
  logo_url
}

#' @export
add_branding <- function(opts) {
  if (!opts$branding_include) return()
  logo_path <- url_logo(logo = opts$logo,background_color = opts$background_color)
  logo_width <- opts$logo_width #- 30
  logo_height <- opts$logo_height #- 20
  chartWidth <-  logo_width + 10
  if (opts$logo_position == "left") chartWidth <- 550 #logo_width + opts$logo_x_position
  chartHeight <- logo_height + 10
  highcharter::JS(
    paste0(
      "function() {this.renderer.image('",logo_path,"', this.chartWidth - ", chartWidth, ", this.chartHeight - ", chartHeight, ",", logo_width, ", ", logo_height,").addClass('logo').add()}"
    ))
}

#' @export
hgch_theme <- function(opts = NULL){
  message("in theme_datasketch")

  labels_style <- list (
    color = opts$dataLabels_color,
    fontFamily = opts$text_family,
    fontSize = paste0(opts$dataLabels_size, "px"),
    textDecoration= "none",
    textShadow = "none",
    textOutline = ifelse(opts$dataLabels_text_outline, "1px contrast", "none")
  )
  y_legend <- opts$legend_y_position

  if (is.null(opts$plot_margin_bottom)) {
    if(opts$branding_include || opts$credits) opts$plot_margin_bottom <- 130
  }

  if (opts$text_size == "") opts$text_size <- 13
  highcharter::hc_theme(
    colors = opts$palette_colors,
    chart = list(
      reflow = TRUE,
      renderTo = 'container',
      backgroundColor = opts$background_color,

      marginBottom = opts$plot_margin_bottom,
      marginLeft = opts$plot_margin_left,
      marginRight = opts$plot_margin_right,
      marginTop = opts$plot_margin_top,

      plotBackgroundColor = opts$plot_background_color,
      plotBorderColor = opts$plot_border_color,
      plotBorderWidth = opts$plot_border_width,
      style = list (
        fontFamily = opts$text_family,
        fontSize = paste0(opts$text_size, 'px')
      )),
    title = list(
      align = opts$title_align,
      style = list(
        fontFamily = opts$title_family,
        fontSize = paste0(opts$title_size, 'px'),
        color = opts$title_color,
        fontWeight = opts$title_weight
      )
    ),
    subtitle = list(
      align = opts$subtitle_align,
      style = list(
        fontFamily = opts$title_family,
        fontSize = paste0(opts$subtitle_size, 'px'),
        color = opts$subtitle_color,
        fontWeight = opts$subtitle_weight
      )
    ),
    credits = list(
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
        style = list(
          color = opts$axis_title_color %||% opts$text_color, #opts$font_color, #color nombre de las etiquetas
          fontFamily = opts$text_family,
          fontSize = paste0(opts$text_size, 'px')
        )),
      title = list(
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
      labels = list(
        style = list(
          color = opts$axis_title_color %||% opts$text_color, #opts$font_color, #color nombre de las etiquetas
          fontFamily = opts$text_family,
          fontSize = paste0(opts$text_size, 'px')
        )),
      title = list(
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
        colorByPoint = FALSE,
        animation = list(
          duration = opts$animation_duration
        ),
        dataLabels = list (
          enabled = opts$dataLabels_show,
          style = labels_style,
          format = paste0(opts$cats, opts$format_dataLabels)
        ),
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
      #backgroundColor = custom$legend_background,
      #borderColor = custom$legend_backgroundBorderColor,
      #borderWidth = custom$legend$backgroundWidth,
      layout = opts$legend_layout,
      align = opts$legend_align,
      y = y_legend,
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
      style = list(
        width = '350px',
        whiteSpace = 'normal',
        fontFamily = opts$tooltip_family %||% opts$text_family,
        fontSize = paste0(opts$text_size, 'px')
      )
    )
  )
}
