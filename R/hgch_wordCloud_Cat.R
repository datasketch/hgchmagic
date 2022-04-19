hgch_wordCloud_Cat <- function(data, ...){

  if (is.null(data)) stop(" dataset to visualize")

  opts <- dsvizopts::merge_dsviz_options(...)

  l <- hgchmagic_prep(data, opts = opts, ftype = "Cat", plot = "wordCloud")
  d <- l$d
  l$theme$legend_show <- FALSE
  data <- NULL
  h <- purrr::map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "weight" = as.numeric(d[[2]][z]),
                            "label" = d$labels[z]
                            )
  })

  global_options(opts$style$format_sample_num)
  hc <- highchart() %>%
    hc_title(text = l$title$title) %>%
    hc_subtitle(text = l$title$subtitle) %>%
    hc_chart(type = 'wordcloud'#,
             #spiral = 'archimedean'
    ) %>%
    hc_series(
      data
    ) %>%
    hc_tooltip(useHTML = TRUE,
               formatter = JS(paste0("function () {return this.point.label;}"))) %>%
    hc_plotOptions(
      series = list(
        allowPointSelect= l$allow_point,
        spiral= l$theme$wordSpiral, #rectangular, archimedean 'square'
        placementStrategy = l$theme$placementStrategy, #'center', #center, random,
        minFontSize = l$theme$wordMinFont,
        maxFontSize = l$theme$wordMaxFont,
        cursor =  l$cursor,
        events = list(
          click = l$clickFunction
        ),
        style = list(
          fontFamily= l$theme$text_family,
          fontWeight = l$theme$text_weight
        ),
        rotation = list(
          from = l$theme$wordFromRotation,
          to = l$theme$wordToRotation,
          orientations = l$theme$wordOrientations
        )
      )) %>%
    hc_credits(enabled = TRUE, text = l$title$caption) %>%
    hc_legend(enabled = FALSE) %>%
    hc_add_theme(hgch_theme(opts = l$theme))

  hc
}
