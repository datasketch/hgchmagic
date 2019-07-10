#' Bubbles Cat Num
#'
#' @param data
#'
#' @export
hgch_bubbles_CatNum <- function(data = NULL,
                                opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)

  if (opts$color_scale == 'discrete') {
    colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
    colorDefault <- discreteColorSelect(colorDefault, d)
  } else if (opts$color_scale == "no"){
    colorDefault <- rep("#3DB26F", length(unique(d$a)))
  } else {
    colorDefault <- leaflet::colorNumeric(c("#53255E", "#ff4097"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }


  if (!is.null(opts$colors)) {
    opts$colors <- unname(fillColors(d, "a", opts$colors, opts$color_scale))
  } else {
    opts$colors <- colorDefault
  }

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
  d <- orderCategory(d, "a", opts$order, opts$label_wrap)
  d <- sortSlice(d, "b", opts$sort, opts$sliceN)


  d <- d %>% plyr::rename(c('b' = 'y'))
  d$color <- NA#colorDefault[1:nrow(d)]

  if (!is.null(opts$highlight_value)) {
    w <- which(d$a %in% opts$highlight_value)
    d$color[w] <- opts$highlight_valueColor
  }


  data <- purrr::map(1:nrow(d), function(z){
    list("name" = d$a[z],
         "value" = d$y[z],
         "color" = as.character(d$color[z]))
  })



  if (is.null(opts$prefix)) opts$prefix <- ""
  if (is.null(opts$suffix)) opts$suffix <- ""


  if (opts$percentage & opts$suffix == "") {
    opts$suffix <- "%"
  }



  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(prefix_agg, ' ' ,nms[2], ': '), opts$prefix,'{point.y}', opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- ""
  }

  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)
  hc <- highchart() %>%
    hc_chart(
      type = 'packedbubble',
      height = '100%'
    ) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
    hc_plotOptions(
      packedbubble = list(
        minSize = '30%',
        maxSize = '120%',
        zMin = 0,
        zMax = 1000,
        layoutAlgorithm = list(
          splitSeries = FALSE,
          gravitationalConstant = 0.02
        ),
        dataLabels = list(
          format = paste0(opts$prefix, "{point.name} <br/> {point.value}", opts$suffix)
        ),
        marker= list(
          fillOpacity= opts$fill_opacity,
          states = list(
          hover = list(
           fillColor = opts$color_hover
          ),
          select = list(
            fillColor = opts$color_click
          ))
        ),
        allowPointSelect= opts$allow_point,
        cursor =  opts$cursor,
        events = list(
          click = opts$clickFunction
        )
        )
    ) %>%
    hc_add_series(
      data = data
    ) %>%
    hc_legend(enabled = F)

  if (opts$export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}

  if (is.null(opts$theme)) {
    hc <- hc %>% hc_add_theme(tma(list(line_width = 0, showText = opts$showText, colors = opts$colors)))
  } else {
    hc <- hc %>% hc_add_theme(opts$theme)
  }
  hc

}




#' bubbles (categories)
#'
#' Compare aggregations among category's levels
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat, Yea, Dat
#' @examples
#' hgch_bubbles_Cat(sampleData("Cat", nrow = 10))
#' @export hgch_bubbles_Cat
hgch_bubbles_Cat <-  function(data = NULL,
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

  h <- hgch_bubbles_CatNum(data = d, opts = opts, ...)
  h
}


#' Bubbles Cat Cat Num
#'
#' @param data
#'
#' @export
hgch_bubbles_CatCatNum <- function(data = NULL,
                                   opts = NULL, ...) {
  if (is.null(data)) {
    stop("Load an available dataset")
  }

  opts <- getOptions(opts = opts)

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""

  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)

  if (opts$color_scale == 'discrete') {
    colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
    colorDefault <- discreteColorSelect(colorDefault, d)
  } else if (opts$color_scale == "no"){
    colorDefault <- rep("#3DB26F", length(unique(d$a)))
  } else {
    colorDefault <- leaflet::colorNumeric(c("#53255E", "#ff4097"), 1:length(unique(d$a)))(1:length(unique(d$a)))
  }


  if (!is.null(opts$colors)) {
    opts$colors <- unname(fillColors(d, "a", opts$colors, opts$color_scale))
  } else {
    opts$colors <- colorDefault
  }
  if (opts$dropNaV[1])
    d <- d %>%
    tidyr::drop_na(a)

  if(opts$dropNaV[2])
    d <- d %>%
    tidyr::drop_na(b)

  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(opts$agg, c))

  d <- d %>% drop_na(c)

  if (opts$percentage) {
    d <- d %>% group_by(b) %>%
      dplyr::mutate(c = (c / sum(c, na.rm = TRUE)) * 100)
  }


  if (is.null(opts$prefix)) opts$prefix <- ""
  if (is.null(opts$suffix)) opts$suffix <- ""

  if (opts$percentage & is.null(opts$suffix)) {
    opts$suffix <- "%"
  }

  d <- map(unique(d$a), function(x) {
    df <- d %>% filter(a %in% x)
    list(
      name = x,
      data =
        map(1:nrow(df), function (z) {
          list(
            name = df$b[z],
            value = df$c[z]
          )
        })
    )
  })

  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <-paste0('<b>', nms[2], ': </b>{point.name}</br>',
                                      '<b>', nms[1], ': </b>{series.name}</br>',
                                      paste0(prefix_agg, ' ' ,nms[3], ': '), opts$prefix,'{point.y}', opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- " "
  }

  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)
  hc <- highchart() %>%
    hc_chart(
      type = 'packedbubble',
      height = '100%'
    ) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_tooltip(useHTML=TRUE, pointFormat = opts$tooltip$pointFormat, headerFormat = opts$tooltip$headerFormat) %>%
    hc_plotOptions(
      packedbubble = list(
        minSize = '30%',
        maxSize = '120%',
        zMin = 0,
        zMax = 1000,
        layoutAlgorithm = list(
          splitSeries = FALSE,
          gravitationalConstant = 0.02
        ),
        dataLabels = list(
          format = paste0(opts$prefix, "{point.name} <br/> {point.value}", opts$suffix)
        ),
        marker= list(
          fillOpacity= opts$fill_opacity,
          states = list(
            hover = list(
              fillColor = opts$color_hover
            ),
            select = list(
              fillColor = opts$color_click
            ))
        ),
        allowPointSelect= opts$allow_point,
        cursor =  opts$cursor,
        events = list(
          click = opts$clickFunction
        )
      )
    ) %>%
    hc_add_series_list(
      x = d
    ) %>%
    hc_legend(
      enabled = TRUE
    )


  if (opts$export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}

  if (is.null(opts$theme)) {
    hc <- hc %>% hc_add_theme(tma(list(line_width = 0, showText = opts$showText, colors = opts$colors)))
  } else {
    hc <- hc %>% hc_add_theme(opts$theme)
  }
  hc
}

#' Grouped bubbles by first category (categories, categories)
#'
#' @param data A data.frame
#' @return Highcharts visualization
#' @section ctypes:
#' Cat-Cat, Cat-Dat, Cat-Yea, Yea-Cat, Yea-Dat, Yea-Yea, Dat-Cat, Dat-Yea, Dat-Dat
#' @examples
#' hgch_bubbles_CatCat(sampleData("Cat-Cat", nrow = 10))
#' @export hgch_bubbles_CatCat
hgch_bubbles_CatCat <-function(data,
                               opts = NULL, ...) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  d <- d %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(c = n())

  prefix_agg <- ifelse(is.null(opts$agg_text), "Count", opts$agg_text)
  names(d) <- c(f$dic_$d$label, paste(prefix_agg, f$dic_$d$label[1]))

  h <- hgch_bubbles_CatCatNum(data = d, opts = opts, ...)
  h
}
