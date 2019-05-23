data <- sampleData("Cat-Num")
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
d <- orderCategory(d, "a", opts$order, opts$labelWrap)
d <- sortSlice(d, "b", opts$sort, opts$sliceN)


d <- d %>% plyr::rename(c('b' = 'y'))
d$color <- NA

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
        enabled = TRUE,
        format = '{point.name}')
    )
  ) %>%
  hc_add_series(
    data = data
  )

hc
if (is.null(opts$theme)) {
  hc <- hc %>% hc_add_theme(tma(custom = list(showText = opts$showText, colors = opts$colors)))
} else {
  hc <- hc %>% hc_add_theme(opts$theme)
}
hc
