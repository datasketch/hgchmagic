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
    if (any(is.na(d[has_year]))) {
      d[[has_year]] <- as.character(d[[has_year]])
    }}

  # Una sola variable de agregacion
  one_var_group <- length(grep("Dat|Cat|Yea", dic$hdType)) == 1

  if (one_var_group) {
    if (length(dic$hdType) == 1) {
      d <- d %>%
        dplyr::group_by_all() %>%
        dplyr::summarise(b = n())
      frtype <- paste(frtype, "-Num")
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
    d <- postprocess(d, "c", sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)

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



  list(
    d = d,
    min_date = min_date,
    formatter_date = formatter,
    formatter_date_tooltip = formatter_tooltip
  )
}
