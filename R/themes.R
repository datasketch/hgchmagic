#' @export
global_options <- function(sample){
  params <- makeup::which_num_format(sample)
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- params$separators$thousands
  hcoptslang$decimalPoint <- params$separators$decimal
  options(highcharter.lang = hcoptslang)
}

url_logo <- function(logo, background_color) {
  if (grepl("http", logo)) logo_url <- logo
  logo_path <- local_logo_path(logo, background_color)
  logo_url <- knitr::image_uri(f = logo_path)
  logo_url
}

#' @export
add_branding <- function(opts) {

  getDefaultTheme <- dsvizopts:::theme_datasketch
  opts <- modifyList(getDefaultTheme, opts)

  if (!opts$branding_include) return()

  logo_path <- url_logo(logo = opts$logo,background_color = opts$background_color)

  JS(
    paste0(
    "function() {this.renderer.image('",logo_path,"', this.chartWidth - 135, this.chartHeight - 40 , 130, 35).addClass('logo').add();}"
    ))
}


theme <- function(opts = NULL){
  message("in theme_datasketch")


  getDefaultTheme <- dsvizopts:::theme_datasketch
  opts <- modifyList(getDefaultTheme, opts)
  #print(opts)

  opts$plot_margin_bottom <- NULL
  if(opts$branding_include) opts$plot_margin_bottom <- 100

  hc_theme(
    colors = opts$palette_colors,
    chart = list(
      reflow = TRUE,
      renderTo = 'container',
      backgroundColor = opts$background_color,
      # borderColor = opts$border_color,
      # borderRadius = opts$border_radius,
      # borderWidth = opts$border_width,
      # width = opts$width,
      # height = opts$height,
      marginBottom = opts$plot_margin_bottom,
      # marginLeft = opts$margin_left,
      # marginRight = opts$margin_right,
      # marginTop = opts$margin_top,
      # plotBackgroundColor = opts$plot_backgroundColor,
      # plotBorderColor = opts$plot_borderColor,
      # plotBorderWidth = opts$plot_borderWidth,
      style = list (
        fontFamily = opts$text_family,
        fontSize = opts$text_size
      ))
  )

}





