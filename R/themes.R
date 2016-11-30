#'
#' #' @export
#' getPalette <- function(type = "qualitative", rev = FALSE){
#'   dsGreen <- "#95C11E"
#'   dsYellow <- "#FFED00"
#'   dsMagenta <- "#E5007D"
#'   dsBlue <- "#009EE3"
#'   dsOrange <- "#F9B233"
#'   dsPink <- "#EF8998"
#'   dsLightBlue <- "#16C5E0"
#'   dsPurple <- "#A839B0"
#'   dsRed <- "#C92F2F"
#'   dsGray <- "#A9A9A9"
#'   dsLila <- "#9B71AF"
#'   dsPalette <- c(dsBlue,dsMagenta,dsGreen,dsOrange,dsYellow,dsPink,
#'                  dsLightBlue,dsPurple,dsRed,dsGray, dsLila)
#'   p <- c(dsPalette,dsPalette,dsPalette)
#'   if(type == "sequential") {
#'     p <-  c(dsMagenta,dsBlue)
#'   }
#'   if(rev) p <- rev(p)
#'   p
#' }
#'
#' #' @export
#' theme_ds_clean <- function(){
#'   theme_ds() + theme(
#'     axis.line=element_blank(),
#'     axis.title.x=element_blank(),
#'     axis.text.x=element_blank(),
#'     axis.text.y=element_blank(),
#'     axis.ticks=element_blank(),
#'     axis.ticks.x=element_blank(),
#'     axis.ticks.y=element_blank(),
#'     axis.title.y = element_blank(),
#'     panel.grid.major=element_blank())
#' }
#'
#'
#' #' @export
#' theme_ds <- function(){
#'   type <- 'outer'
#'   inner <- type == 'inner'
#'   palette <- list(
#'     background = "#ffffff", # #ffffff #F0EDFF #F8EDFA #FDF8FD
#'     text = list(inner = "#555555", outer = "#111111"),
#'     line = list(inner = "#826A50", outer = "#362C21"),
#'     gridline = "#c9c7d3",
#'     swatch = c("#111111","#65ADC2","#233B43","#E84646","#C29365","#362C21","#316675","#168E7F","#109B37"),
#'     gradient = list(low = "#65ADC2", high = "#362C21")
#'   )
#'   spacing <- 1.6
#'   line_colour <- "#1d1d1d"
#'   text_colour <- "#555555"
#'   text_size <- 20
#'   line_weight <- 0.5
#'   x_title_spacing <- function(spacing)
#'     max(-1.2, -(spacing / 1.25) + 0.5)
#'   y_title_spacing <- function(spacing)
#'     max(0.8, min(2.4, spacing))
#'
#'   theme(
#'     legend.title=element_blank(),
#'     line = element_line(
#'       colour = line_colour,
#'       size = line_weight,
#'       linetype = 1,
#'       lineend = "butt"),
#'     rect = element_rect(
#'       fill = "white",
#'       colour = text_colour,
#'       size = 0.5,
#'       linetype = 1),
#'     text = element_text(
#'       debug=FALSE,
#'       margin=margin(),
#'       family = '',
#'       face = "plain",
#'       colour = text_colour,
#'       size = text_size,
#'       hjust = 0.5,
#'       vjust = 0.5,
#'       angle = 0,
#'       lineheight = 0.9),
#'     axis.text = element_text(
#'       debug=FALSE,
#'       margin=margin(),
#'       size = rel(0.8),
#'       colour = text_colour),
#'     strip.text = element_text(
#'       debug=FALSE,
#'       margin=margin(),
#'       size = rel(0.8)),
#'     axis.line = element_line(
#'       colour = line_colour),
#'     axis.line.x = element_line(colour = line_colour),
#'     axis.line.y = element_line(colour = line_colour),
#'     axis.text.x = element_text(
#'       debug=FALSE,
#'       margin=margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = 'cm'),
#'       vjust = 1,
#'       colour = text_colour,
#'       face='bold'),
#'     axis.text.y = element_text(
#'       debug=FALSE,
#'       margin=margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = 'cm'),
#'       hjust = 1,
#'       colour = text_colour,
#'       face='bold'),
#'     axis.ticks = element_line(colour = line_colour),
#'     axis.title = element_text(face='bold',colour = text_colour),
#'     axis.title.x = element_text(
#'       debug=FALSE,
#'       margin=margin(),
#'       vjust=x_title_spacing(spacing)),
#'     axis.title.y = element_text(
#'       debug=FALSE,
#'       margin=margin(),
#'       angle = 90,
#'       vjust=y_title_spacing(spacing)),
#'     axis.ticks.length = grid::unit(0.15, "cm"),
#'     legend.background = element_rect(
#'       colour = ifelse(inner, 'white', palette$background),
#'       fill = ifelse(inner, 'white', palette$background)),
#'     legend.margin = grid::unit(0.2 * spacing, "cm"),
#'     legend.key = element_rect(
#'       colour = ifelse(inner, 'white', palette$background),
#'       fill = palette$background),
#'     legend.key.size = grid::unit(
#'       1.2, "lines"),
#'     legend.key.height = NULL,
#'     legend.key.width = NULL,
#'     legend.text = element_text(
#'       debug=FALSE,
#'       margin=margin(),
#'       size = rel(0.8)),
#'     legend.position = "right",
#'     legend.direction = NULL,
#'     legend.justification = "center",
#'     legend.box = NULL,
#'     panel.background = element_rect(fill = palette$background,colour = NA),
#'     panel.border = element_blank(),
#'     panel.grid.major = element_line(linetype='dashed',colour = palette$gridline),
#'     panel.grid.minor = element_blank(),
#'     panel.margin = grid::unit(0.5 * spacing, 'cm'),
#'     panel.margin.x = NULL,
#'     panel.margin.y = NULL,
#'     panel.ontop = FALSE,
#'     strip.background = element_rect(
#'       fill = ifelse(inner, 'white', palette$background),
#'       colour = NA),
#'     strip.text.x = element_text(
#'       debug=FALSE,
#'       margin=margin(),
#'       size = rel(1.1),
#'       face = 'bold'),
#'     strip.text.y = element_text(
#'       debug=FALSE,
#'       margin=margin(),
#'       angle = -90,
#'       face = 'bold',
#'       size = rel(1.1)),
#'     strip.switch.pad.grid = grid::unit(0, 'cm'),
#'     strip.switch.pad.wrap = grid::unit(0, 'cm'),
#'     plot.background = element_rect(
#'       colour = ifelse(inner, 'white', palette$background),
#'       fill = ifelse(inner, 'white', palette$background)),
#'
#'     plot.title = element_text(
#'       debug=FALSE,
#'       margin=margin(0, 0, 6.6, 0),
#'       size = rel(1.2),
#'       vjust = spacing,
#'       face='bold'),
#'     plot.margin = grid::unit(c(0.625, 0.625, 0.625, 0.625) * spacing, 'cm'),
#'     complete = TRUE
#'   )
#' }
#'
#'
#'
