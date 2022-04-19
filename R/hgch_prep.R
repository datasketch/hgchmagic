#' @export
hgchmagic_prep <- function(data, opts = NULL, extra_pattern = ".", plot =  "bar", ftype = "Cat-Num"){

  if (is.null(data)) return()

  # Clean text
  if (plot == "wordCloud") {
    data <- dsvizprep::data_word_prep(data, ftype = ftype, lang = opts$shiny$lang, stopwords = opts$theme$stopwords)
    data[[1]] <- makeup::makeup_chr(data[[1]], opts$style$format_sample_cat)
  }

  # data prep ---------------------------------------------------------------

  f <- homodatum::fringe(data)
  nms <- homodatum::fringe_labels(f)
  nms[length(nms)+1] <- c("%")
  names(nms) <- c(names(nms)[-length(nms)], "..percentage")
  nms[length(nms)+1] <- c("Count")
  names(nms) <- c(names(nms)[-length(nms)], "..count")
  d <- homodatum::fringe_d(f)
  frtype <- f$frtype
  dic <- f$dic
  dic$id <- names(d)



  # dictionary and data preparation when variable is yea or pct -------------

  if (grepl("Pct", frtype)) {
    dic$hdType[dic$hdType == "Pct"] <- "Num"
    frtype <- gsub("Pct", "Num", frtype)
  }

  if (grepl("Yea", frtype)) {
    has_year <- dic$id[dic$hdType == "Yea"]
    #if (any(is.na(d[has_year]))) {
    d[[has_year]] <- as.character(d[[has_year]])
  }
  # only data plot ----------------------------------------------------------

  ftype_vec <- stringr::str_split(ftype,pattern = "-") %>% unlist()
  ftype_length <- length(ftype_vec)
  dd <- d[,1:ftype_length]
  dic_p <- dic %>% dplyr::filter(id %in% names(dd))




  min_date <- NULL
  var_g <- NULL ## categorical ..groups
  dic_agg <- NULL
  #formatter_tooltip <- NULL
  formatter <- NULL


  if (any(c("Cat", "Dat", "Yea") %in% dic_p$hdType)) dic_agg <- dic_p %>% dplyr::filter(hdType %in% c("Cat", "Dat", "Yea"))
  if (any(c("Cat", "Dat", "Yea") %in% dic_p$hdType)) var_g <- unique(dic_agg$id)



  # agregation all numeric variables and collapse categorica variabl --------

  var_nums <- grep("Num", dic$hdType)
  agg_num <- NULL
  if (!identical(var_nums, integer())) agg_num <- dic$id[var_nums]
  func_paste <- function(x) paste(unique(x), collapse = '. ')
  var_cats <- grep("Cat|Yea|Dat", dic$hdType)
  agg_cats <- NULL
  if (!identical(var_cats, integer())) agg_cats <- dic$id[var_cats]
  dd <- NULL

  agg_var <- "..count"
  has_num_var <- "Num" %in% dic_p$hdType

  if (has_num_var &  sum(grepl("Num",  ftype_vec)) > 1) {
    agg_var <- opts$postprocess$percentage_col %||% "b"
  }


  if (!is.null(var_g)) {
    if (length(grep("Dat|Cat|Yea", ftype_vec)) == 1) {
      if (has_num_var & sum(grepl("Num",  ftype_vec)) == 1)  {
        agg_var <- "b"
        }
    } else {
      if (has_num_var) {
        agg_var <- "c"
      }
    }
  }


  dic_alt <- dic

  if (agg_var == "..count") {
    dic_p <- dic_p %>% dplyr::bind_rows(dplyr::bind_rows(data.frame(id = "..count", label = "Count", hdType = "Num") %>%
                                                           dplyr::mutate_all(as.character)))
  } else {
    dic_p <- dic_p
  }

  if (opts$postprocess$percentage) {
    dic_p <- dic_p %>% dplyr::filter(id != agg_var)
    dic_p <- dic_p %>% dplyr::bind_rows(dplyr::bind_rows(data.frame(id = "..percentage", label = "%", hdType = "Num") %>%
                                                           dplyr::mutate_all(as.character)))
  }


  if (!is.null(var_g)) {
    dn <- d
    if (!is.null(agg_num))  dn <- d[,-var_nums]


    if (length(var_g) == 1) {
      dd <- function_agg(df = d, agg = opts$summarize$agg, to_agg = agg_num, a)
      if (grepl("Dat", ftype)) {
        dd <- dd %>% tidyr::drop_na()
        dn <- dn %>% tidyr::drop_na(a)
        dn$..group <- nms[[2]]
      }

      if (!grepl("Dat", ftype)) {
        dd <- dsvizprep::preprocessData(dd, drop_na = opts$preprocess$drop_na,
                                        na_label = opts$preprocess$na_label, na_label_cols = "a")
      } else {
          min_date <- min(dd$a)
          dd$a <- as.numeric(as.POSIXct(as.Date(dd$a, origin = min_date)))*1000
          dn$..date <- dn$a
          dn$a <- as.numeric(as.POSIXct(as.Date(dn$a, origin = min_date)))*1000
      }
      dd <- dsvizprep::postprocess(dd, agg_var, sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)

      dd$..percentage <- (dd[[agg_var]]/sum(dd[[agg_var]], na.rm = TRUE)) * 100

      dn <- dn %>%
        dplyr::group_by(a) %>%
        dplyr::summarise_all(.funs = func_paste)

      if (grepl("Dat", ftype)) {
        dic_p <- dic_p %>%
          dplyr::bind_rows(data.frame(id = "..group", label = "..group", hdType = "Cat") %>%
                             dplyr::mutate_all(as.character))
        nms[length(nms)+1] <- c("..group")
        names(nms) <- c(names(nms)[-length(nms)], "..group")
      }
    } else {

      dd <- function_agg(df = d, agg = opts$summarize$agg, to_agg = agg_num, a, b)

      if (grepl("Dat", ftype)) {
        dd <- dd %>% tidyr::drop_na(b)
        dn <- dn %>% tidyr::drop_na(b)}

      if (grepl("Dat", ftype))  {
        dn$..date <- dn$b
        dn$b <- as.numeric(as.POSIXct(as.Date(dn$b, origin = min_date)))*1000
        min_date <- min(as.Date(dd$b))
        dd$b <- as.numeric(as.POSIXct(as.Date(dd$b, origin = min_date)))*1000
      }

      by_col <- opts$postprocess$percentage_col
      if (is.null(by_col)) {
        by_col <- "a"
      } else {
        by_col <- names(nms[match(by_col, nms)])
      }
      agg_var_t <- rlang::sym(agg_var)
      dd <- dd %>%
        dplyr::group_by_(by_col) %>%
        dplyr::mutate(..percentage = (!!agg_var_t/sum(!!agg_var_t, na.rm = TRUE))*100)


      if (!grepl("Dat", ftype)) {
        dd <- dsvizprep::preprocessData(dd, drop_na = opts$preprocess$drop_na,
                                        na_label = opts$preprocess$na_label, na_label_cols = "b")
      }

      d_c <- dd[,dic_p$id]
      if (opts$postprocess$percentage) {
        agg_var <- "..percentage"
      } else {
        if (grepl("Num", ftype)) dd <- dd[,-3]
        dd <- dd
      }

      d_c <- completevalues(d_c, agg_var)
      d_c$b <- as.character(d_c$b)
      dd$b <- as.character(dd$b)
      dd <- d_c %>% dplyr::left_join(dd)
      dd <- dsvizprep::preprocessData(dd, drop_na = opts$preprocess$drop_na_legend,
                                      na_label = opts$preprocess$na_label, na_label_cols = "a")
      dd <- dsvizprep::postprocess(dd, agg_var, sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)

      dn$a[is.na(dn$a)] <- opts$preprocess$na_label
      if (!grepl("Dat", ftype)){
        dn$b[is.na(dn$b)] <- opts$preprocess$na_label
      }

      dn <- dn %>%
        dplyr::group_by(a, b) %>%
        dplyr::summarise_each(dplyr::funs(func_paste))
      if (!grepl("Dat", ftype)){
      dd$b <- as.character(dd$b)
      dn$b <- as.character(dn$b)
      } else {
        dd$b <- as.numeric(dd$b)
        dn$b <- as.numeric(dn$b)
      }

    }

    l_cats <- purrr::map(names(dn), function(f_cats){
      dn[[paste0(f_cats, "_label")]] <<- makeup::makeup_chr(dn[[f_cats]], opts$style$format_sample_cat)
      dn[[f_cats]]
    })
    dic_alt <- dic_alt %>%
      dplyr::bind_rows(data.frame(id = c("..count", "..percentage"), label = c("Count", "%"), hdType = c("Num", "Num")) %>%
                         dplyr::mutate_all(as.character))

    d <- dd %>% dplyr::left_join(dn, by = var_g)
  }


  # format in original data -------------------------------------------------
  # general format to numerical data to prepare information from tooltip

  var_nums <- grep("Num", dic_alt$hdType)
  #print(d)
  if (!identical(var_nums, integer())) {
    var_nums <- dic_alt$id[var_nums]

    l_nums <- purrr::map(var_nums, function(f_nums){
      d[[paste0(f_nums, "_label")]] <<- makeup::makeup_num(as.numeric(d[[f_nums]]), sample = opts$style$format_sample_num)
    })}


  if (!identical(grep("Dat", ftype_vec), integer())) {
    labs <- NULL
    d$..date_label <- makeup::makeup_dat(d[["..date"]],
                                         sample = opts$style$format_sample_dat,
                                         #locale = opts$style$locale,
                                         format = opts$style$format_dat
    )

    if (grep("Dat", ftype_vec) == 1) d$a_label <- d$..date_label
    if (grep("Dat", ftype_vec) == 2) d$b_label <- d$..date_label

    labs <- as.list(d$..date_label)

    options(scipen = 9999)
    if(sum(grepl("Dat|Cat|Yea", ftype_vec)) == 1) {
      names(labs) <- d$a
    } else {
      names(labs) <-  d$b
    }

    labs <- jsonlite::toJSON(labs, auto_unbox = TRUE)

    formatter <- "
       function() {
       var labels = <<labs>>;
       return labels[this.value]}"
    formatter <- glue::glue(formatter, .open = "<<", .close = ">>")


    # formatter_tooltip <-
    #   JS(
    #     paste0("function () {return '<i>' + this.point.label +'</i><br/><b>' + this.series.name + ':</b> ' + this.point.y;}"
    #     ))
  }

 # add label from tooltip info ---------------------------------------------

  default_tooltip <- dic_p$label
  default_tooltip <- setdiff(default_tooltip, "..group")

  if (opts$chart$tooltip == "") opts$chart$tooltip <- NULL
  d <- d %>%
    dplyr::mutate(labels = glue::glue(hgch_tooltip(nms = nms, label_ftype = default_tooltip, tooltip = opts$chart$tooltip)) %>%
                    lapply(htmltools::HTML))

  d <- d[, c(dic_p$id,  "labels")]

  # axis labels -------------------------------------------------------------


  if (!identical(grep("Dat", ftype_vec), integer())) dic_p <- dic_p %>% dplyr::filter(id != "..group")

  nms_dic <- stats::setNames(dic_p$label, dic_p$id)

  labelsXY <- dsvizprep::labelsXY(hor_title = opts$title$hor_title %||% nms_dic[[(1 + (length(nms_dic) - 2))]], #
                                  ver_title = opts$title$ver_title %||% nms_dic[[length(nms_dic)]],
                                  nms = nms_dic, orientation = opts$chart$orientation)

  hor_title <- as.character(labelsXY[1])
  ver_title <- as.character(labelsXY[2])



  # color -------------------------------------------------------------------

  palette_type <- opts$theme$palette_type %||% "categorical"

  color_by <- NULL

  if (!is.null(opts$style$color_by)) color_by <- names(nms[match(opts$style$color_by, nms)])

  if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 2)  color_by <- "a"



  if(is.null(opts$theme$palette_colors)){
    opts$theme$palette_colors <- opts$theme[[paste0("palette_colors_", palette_type)]]
  }
  palette <- opts$theme$palette_colors

  if ("color" %in% dic$hdType) {
    d$..colors <- d[[dic$id[dic$hdType == "color"][1]]]
  } else {
    if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 1 && sum(grepl("Dat", ftype_vec)) == 1) {
      d$..colors <- palette[1]
    } else {
      d$..colors <- paletero::map_colors(d, color_by, palette, colors_df = NULL)

    }
  }

  d$..colors[d$a == "(NA)"] <- opts$theme$na_color

  if (!is.null(opts$chart$highlight_value)) {
    if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 2) d$..colors <- palette[1]
    w <- grep(paste0(opts$chart$highlight_value, collapse = '|'), d[[color_by %||% "a"]])
    d$..colors[w] <- opts$chart$highlight_value_color
  }
 #print(d)

  # order -------------------------------------------------------------------


  if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 1) {
    if (!grepl("Dat", ftype)) {
      d <- dsvizprep::order_category(d, col = "a", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)
    }
  }

  if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 2) {
    d <- dsvizprep::order_category(d, col = "a", order = opts$postprocess$order_legend, label_wrap = opts$style$label_wrap_legend)
    legend_index <- data.frame(a = unique(d$a), ..legendIndex = 0:(length(unique(d$a))-1))
    d <- d %>% left_join(legend_index)
    orderStacked <- c(opts$postprocess$order_stacked,setdiff(unique(d$a),opts$postprocess$order_stacked))
    cat_index <- data.frame(a = orderStacked, ..index = 0:(length(unique(d$a))-1))
    d <- d %>% left_join(cat_index) %>% arrange(..index)

    if (!grepl("Dat", frtype)) {
      d <- dsvizprep::order_category(d, col = "b", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)
    }

  }

  suffix_enter <- opts$style$suffix
  if (opts$postprocess$percentage) suffix_enter <- suffix_enter %||% "%"


  f_nums <- makeup::makeup_format_js(sample = opts$style$format_sample_num,
                                     locale = opts$style$locale,
                                     prefix = opts$style$prefix,
                                     suffix = suffix_enter)


  if (plot != "scatter") {

    sample_labels <- opts$dataLabels$dataLabels_format_sample %||% opts$style$format_sample_num
    format_dataLabels <- format_hgch(plot = plot,
                                     frtype = ftype,
                                     sample = sample_labels,
                                     prefix = opts$style$prefix,
                                     suffix = suffix_enter)
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
  color_by_point <- FALSE
  if (plot == "wordCloud") color_by_point <- TRUE


  # end options -------------------------------------------------------------


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
    #formatter_date_tooltip = formatter_tooltip,
    formats = f_nums,
    date_intervals = date_intervals(opts$extra$date_intervals),
    orientation = opts$chart$orientation,
    percentage = opts$postprocess$percentage,
    formatter_js = opts$extra$formatter_js,
    formatter_x_js = opts$extra$formatter_x_js,
    y_axis_align = opts$theme$y_axis_align,
    theme = c(opts$theme,
              color_by_point =  color_by_point,
              isNullCaption = is.null(opts$title$caption),
              bar_pointWidth = opts$theme$bar_pointWidth,
              credits = show_caption,
              y_credits = y_caption,
              animation_duration = opts$extra$animation_duration,
              templatedataLabels = opts$dataLabels$dataLabels_template,
              dataLabels_show = opts$dataLabels$dataLabels_show,
              dataLabels_type = opts$dataLabels$dataLabels_type,
              dataLabels_color = opts$dataLabels$dataLabels_color %||% "constrast",
              dataLabels_size = opts$dataLabels$dataLabels_size %||% "11",
              dataLabels_text_outline = opts$dataLabels$dataLabels_text_outline,
              format_dataLabels = format_dataLabels,
              suffix = suffix_enter,
              prefix = opts$style$prefix),
    color_hover = opts$shiny$color_hover,
    color_click = opts$shiny$color_click,
    allow_point = opts$shiny$allow_point,
    cursor = opts$shiny$cursor,
    spline = opts$style$spline,
    clickFunction = opts$shiny$clickFunction,
    graph_type = opts$chart$graph_type,
    extra = dsvizopts::get_extra_opts(opts, extra_pattern)
  )

}
