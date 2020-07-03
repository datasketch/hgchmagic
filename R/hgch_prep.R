#' @export
hgchmagic_prep <- function(data, opts = NULL, extra_pattern = ".", plot =  "bar"){
  #data <- sample_data("Cat-Dat-Num")
  # Handle homodatum
  f <- homodatum::fringe(data)
  nms <- fringe_labels(f)
  d <- fringe_d(f)
  frtype <- f$frtype
  dic <- f$dic
  dic$id <- names(d)
  var_date <- grep("Dat", dic$hdType)
  var_cats <- grep("Cat", dic$hdType)
  min_date <- NULL
  formatter_tooltip <- NULL
  formatter <- NULL



  if (grepl("Yea", frtype)) {
    has_year <- dic$id[dic$hdType == "Yea"]
    #if (any(is.na(d[has_year]))) {
    d[[has_year]] <- as.character(d[[has_year]])
  }
  #}

  # Una sola variable de agregacion
  one_var_group <- length(grep("Dat|Cat|Yea", dic$hdType)) == 1

  if (one_var_group) {
    if (length(dic$hdType) == 1) {
      d <- d %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(b = n())
      frtype <- paste0(frtype, "-Num")
      nms[2] <- opts$summarize$agg_text %||% "Count"
      names(nms) <- c("a", "b") }

    if (grepl("Dat", frtype)) {
      d <- d %>% drop_na()
    } else {
      d <- preprocessData(d, drop_na = opts$preprocess$drop_na,
                          na_label = opts$preprocess$na_label, na_label_cols = "a")
    }
    d <- summarizeData(d, opts$summarize$agg, to_agg = b, a)
    d <- postprocess(d, "b", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)

    labelsXY <- labelsXY(hor_title = opts$title$hor_title %||% nms[1],
                         ver_title = opts$title$ver_title %||% nms[2],
                         nms = nms, orientation = opts$chart$orientation)
    hor_title <- as.character(labelsXY[1])
    ver_title <- as.character(labelsXY[2])

    if (grepl("Dat", frtype)) {
      d$..colors <- opts$theme$palette_colors[1]
      min_date <- min(d$a)
      d$group <- nms[[2]]
    } else {
      color_by <- names(nms[match(opts$style$color_by, nms)])
      palette <- opts$theme$palette_colors
      d$..colors <- paletero::map_colors(d, color_by, palette, colors_df = NULL)
    }

    if (!is.null(opts$chart$highlight_value)) {
      w <- grep(paste0(opts$chart$highlight_value, collapse = '|'), d$a)
      print(w)
      d$..colors[w] <- opts$chart$highlight_value_color
    }

    if (!grepl("Dat", frtype)) {
      d <- order_category(d, col = "a", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)
    }

    if (opts$postprocess$percentage) {
      d$b <- (d$b/sum(d$b))*100
      opts$style$suffix <- "%"
    }

  }

  # dos variables de agregacion
  two_var_group <- length(grep("Dat|Cat|Yea", dic$hdType)) == 2
  if (two_var_group) {
    if (length(dic$hdType) == 2) {
      d$a <- as.character(d$a)
      d <- d %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(c = n())
      frtype <- paste(frtype, "-Num")
      nms[3] <-  opts$summarize$agg_text %||% "Count"
      names(nms) <- c("a", "b", "c") }
    if (grepl("Dat", frtype)) {
      d <- d %>% drop_na(b)
    } else {
      d <- preprocessData(d, drop_na = opts$preprocess$drop_na,
                          na_label = opts$preprocess$na_label, na_label_cols = "b")
    }

    d <- preprocessData(d, drop_na = opts$preprocess$drop_na_legend,
                        na_label = opts$preprocess$na_label, na_label_cols = "a")
    d <- summarizeData(d, opts$summarize$agg, to_agg = c, a, b)

    labelsXY <- opts$title$hor_title %||% nms[2]
    labelsXY[2] <- opts$title$ver_title %||% nms[3]
    if (opts$chart$orientation == "hor")  labelsXY <- rev(labelsXY)
    hor_title <- as.character(labelsXY[1])
    ver_title <- as.character(labelsXY[2])

    if (!grepl("Dat", frtype)) {
      d <- completevalues(d)
    }

    palette <- opts$theme$palette_colors
    d$..colors <- paletero::map_colors(d, 'a', palette, colors_df = NULL)

    if (!is.null(opts$chart$highlight_value)) {
      d$..colors <- palette[1]
      w <- grep(paste0(opts$chart$highlight_value, collapse = '|'), d$a)
      d$..colors[w] <- opts$chart$highlight_value_color
    }
    d <- order_category(d, col = "a", order = opts$postprocess$order_legend, label_wrap = opts$style$label_wrap_legend)

    if (!grepl("Dat", frtype)) {
      d <- order_category(d, col = "b", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)
    }

    if (opts$postprocess$percentage) {
      by_col <- opts$postprocess$percentage_col
      if (is.null(by_col)) {
      } else {
        by_col <- names(nms[match(by_col, nms)])
      }
      print(by_col)
      d <- d %>% group_by_(by_col) %>%
        dplyr::mutate(c = (c / sum(c, na.rm = TRUE)) * 100)
      opts$style$suffix <- "%"
    }
    d <- postprocess(d, "c", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)

  }



  if (!identical(var_date, integer())) {
    labs <- NULL
    l_date <- map(var_date, function(f_date){
      d_var <- dic$id[[f_date]]
      d[[paste0("..", d_var, "_label")]] <<- makeup_dat(d[[f_date]], sample = "Junio 4 2011")
      #opts$style$format_dat_sample,locale = opts$style$locale)
      d[[paste0("..", d_var, "_label")]] <<- makeup_chr(d[[paste0("..", d_var, "_label")]],  opts$style$format_cat_sample)
      labs <<- as.list(d[[paste0("..", d_var, "_label")]])
      options(scipen = 9999)
      names(labs) <<- as.numeric(as.POSIXct(as.Date(d[[d_var]], origin =  min_date)))*1000
      labs <<- jsonlite::toJSON(labs, auto_unbox = TRUE)
    })
    formatter <- "
     function() {
     var labels = <<labs>>;
     return labels[this.value]}"
    formatter <- glue::glue(formatter, .open = "<<", .close = ">>")

    formatter_tooltip <-
      JS(
        paste0("function () {return '<i>' + this.point.label +'</i><br/><b>' + this.series.name + ':</b> ' + this.point.y;}"
        ))
  }

  if (!identical(var_cats, integer())) {
    l_cats <- map(var_cats, function(f_cats){
      d[[f_cats]] <<- makeup_chr(d[[f_cats]], opts$style$format_cat_sample)
    })}



  # Mas varaibles numericas sin categorias de agregacion

  more_var_num <- all(grepl("Num", dic$hdType))

  if (more_var_num) {
    if (length(grepl("Num", dic$hdType)) == 1) {
      d$index <- 1:nrow(d)
      frtype <- paste0(frtype, "-Num")
      nms[2] <- opts$summarize$agg_text %||% "Index"
      names(nms) <- c("a", "b")
    }
    d <- d %>% drop_na()
    hor_title <- opts$title$hor_title %||% nms[[1]]
    ver_title <- opts$title$ver_title %||% nms[[2]]
  }

  f_nums <- makeup::makeup_format_js(sample = opts$style$format_num_sample,
                                     locale = opts$style$locale,
                                     prefix = opts$style$prefix,
                                     suffix = opts$style$suffix)

  tooltip <- tooltip_hgch(plot, tooltip = opts$chart$tooltip,
                          nms = nms, frtype = frtype,
                          prefix = opts$style$prefix,
                          suffix = opts$style$suffix,
                          sample = opts$style$format_num_sample)

  if (plot != "scatter") {
    sample_labels <- opts$dataLabels$dataLabels_format_sample %||% opts$style$format_num_sample
    format_dataLabels <- format_hgch(plot = plot,
                                     frtype = frtype,
                                     sample = sample_labels,
                                     prefix = opts$style$prefix,
                                     suffix = opts$style$suffix)
  } else {
    format_dataLabels <- NULL
  }

  show_caption  <- opts$title$caption %||% ""
  y_caption <- -10
  if (show_caption == "") {
    show_caption <- FALSE
  } else {
    show_caption <- TRUE
    lines <- length(strsplit(opts$title$caption, split = "<br/>")%>% unlist())
    y_caption <- ifelse(lines == 0, y_caption, (lines+2) * -10)
  }



  list(
    d = d,
    titles = list(
      title = opts$title$title,
      subtitle = opts$title$subtitle,
      caption = opts$title$caption %||% "",
      x = hor_title,
      y = ver_title
    ),
    min_date = min_date,
    formatter_date = formatter,
    formatter_date_tooltip = formatter_tooltip,
    tooltip = tooltip,
    formats = f_nums,
    date_intervals = date_intervals(opts$extra$date_intervals),
    orientation = opts$chart$orientation,
    theme = c(opts$theme,
              credits = show_caption,
              y_credits = y_caption,
              dataLabels_show = opts$dataLabels$dataLabels_show,
              dataLabels_color = opts$dataLabels$dataLabels_color %||% "constrast",
              dataLabels_size = opts$dataLabels$dataLabels_size %||% "11",
              dataLabels_text_outline = opts$dataLabels$dataLabels_text_outline,
              format_dataLabels = format_dataLabels,
              suffix = opts$style$suffix,
              prefix = opts$style$prefix),
    color_hover = opts$shiny$color_hover,
    color_click = opts$shiny$color_click,
    allow_point = opts$shiny$allow_point,
    cursor = opts$shiny$cursor,
    clickFunction = opts$shiny$clickFunction,
    graph_type = opts$chart$graph_type,
    extra = get_extra_opts(opts, extra_pattern)
  )
}
