#' @export
hgchmagic_prep <- function(data, opts = NULL, extra_pattern = ".", plot =  "bar", ftype = "Cat-Num"){

  if (is.null(data)) return()

# data prep ---------------------------------------------------------------

  f <- homodatum::fringe(data)
  nms <- homodatum::fringe_labels(f)
  d <- fringe_d(f)
  frtype <- f$frtype
  dic <- f$dic
  dic$id <- names(d)


# general functions -------------------------------------------------------

  func_paste <- function(x) paste(unique(x), collapse = '. ')

# dictionary and data preparation when variable is yea or pct -------------

  if (grepl("Pct", frtype)) {
    dic$hdType[dic$hdType == "Pct"] <- "Num"
    frtype <- gsub("Pct", "Num", frtype)
  }

  if (grepl("Yea", frtype)) {
    has_year <- dic$id[dic$hdType == "Yea"]
    d[[has_year]] <- as.character(d[[has_year]])
  }

# only data plot ----------------------------------------------------------

  ftype_vec <- str_split(ftype,pattern = "-") %>% unlist()
  ftype_length <- length(ftype_vec)
  d_p <- d[,1:ftype_length]
  dic_p <- dic %>% filter(id %in% names(d_p))


# detect grouping variables -----------------------------------------------
# by default the first categorical variables of the dataframe

  min_date <- NULL
  var_g <- NULL
  dic_agg <- NULL
  if (any(c("Cat", "Dat", "Yea") %in% dic_p$hdType)) dic_agg <- dic_p %>% filter(hdType %in% c("Cat", "Dat", "Yea"))
  if (any(c("Cat", "Dat", "Yea") %in% dic_p$hdType)) var_g <- unique(dic_agg$id)


# detect if the data has numeric variables

  has_num_var <- "Num" %in% dic_p$hdType



# data preparation keeping all the information entered --------------------

  if (!has_num_var) { # if there is no numerical variable a count is necessary
    d_p <- d_p %>%
      dplyr::group_by_all() %>%
      dplyr::summarise(..count = n())

    ftype <- paste0(ftype, "-Num")
    ftype_length <- ftype_length + 1
    n_nms <- names(nms)
    nms[length(dic$id)+1] <- opts$summarize$agg_text %||% "Count"
    names(nms) <- c(n_nms, "..count")
    dic_count <- data.frame(id = "..count", label = "Count", hdType = "Num")
    dic <- bind_rows(dic, dic_count)
    dic_p <- bind_rows(dic_p, dic_count)
  } else {
    if (length(grep("Num", ftype_vec)) > 1) { # only numerical variables or more than one numerical variable and a single category
      d_p <- d
    } else if (length(grep("Dat|Cat|Yea", ftype_vec)) == 1) {
      if (grepl("Dat", ftype)) {
        d_p <- d_p %>% drop_na()
        d <- d %>% drop_na(a)
        min_date <- min(d_p$a)
      }
      d_p <- dsvizopts:::summarizeData(d_p, opts$summarize$agg, to_agg = b, a)
    } else {
      if (grepl("Dat", ftype)) {
        d_p <- d_p %>% drop_na(b)
        d <- d %>% drop_na(b)
        min_date <- min(d_p$b)
      }
      d_p <- dsvizopts:::summarizeData(d_p, opts$summarize$agg, to_agg = c, a, b)
    }
  }





# processing of data ------------------------------------------------------


  if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 1) {  #just an aggregation variable

    if (grepl("Dat", ftype)) {
      d$group <- nms[[2]]
    } else {
      d_p <- preprocessData(d_p, drop_na = opts$preprocess$drop_na,
                          na_label = opts$preprocess$na_label, na_label_cols = "a")
    }
    d_p <- postprocess(d_p, agg_var, sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)
    d_p$..percentage <- (d_p$b/sum(d_p$b,na.rm = TRUE)) * 100
    if (grepl("Dat", ftype))  d_p$a <- as.numeric(as.POSIXct(as.Date(d_p$a, origin = min_date)))*1000
  } else if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 2) {
    if (!grepl("Dat", ftype)) {
      d_p <- preprocessData(d_p, drop_na = opts$preprocess$drop_na,
                          na_label = opts$preprocess$na_label, na_label_cols = "b")
    }
    d_p <- preprocessData(d_p, drop_na = opts$preprocess$drop_na_legend,
                        na_label = opts$preprocess$na_label, na_label_cols = "a")
    d_p <- postprocess(d_p, agg_var, sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)
    d_p <- completevalues(d_p)
    by_col <- opts$postprocess$percentage_col
    if (is.null(by_col)) {
      by_col <- "a"
    } else {
      by_col <- names(nms[match(by_col, nms)])
    }
    d_p <- d_p %>%
            group_by_(by_col) %>%
             dplyr::mutate(..percentage = (c / sum(c, na.rm = TRUE)) * 100)
    if (grepl("Dat", ftype))  d_p$b <- as.numeric(as.POSIXct(as.Date(d_p$b, origin = min_date)))*1000
  } else {
    d_p <- d_p
  }



   agg_var <- "..count"

   if (!is.null(var_g)) {
     if (length(grep("Dat|Cat|Yea", ftype_vec)) == 1) {
       if (has_num_var)  {
         d <- d[,-2]
         agg_var <- "b"
       }
       d <- d %>%
         group_by(a) %>%
         summarise_each(funs(func_paste))
       if (!grepl("Dat", ftype)){
         d$a[is.na(d$a)] <- opts$preprocess$na_label
       } else {
         d$..date <- d$a
         d$a <- as.numeric(as.POSIXct(as.Date(d$a, origin = min_date)))*1000
         }
     } else {
       if (has_num_var) {
         d <- d[,-3]
         agg_var <- "c"
       }
       d <- d %>%
         group_by(a, b) %>%
         summarise_each(funs(func_paste))
       d$a[is.na(d$a)] <- opts$preprocess$na_label
       if (!grepl("Dat", ftype)) {
         d$b[is.na(d$b)] <- opts$preprocess$na_label
       } else {
         d$..date <- d$b
         d$b <- as.numeric(as.POSIXct(as.Date(d$b, origin = min_date)))*1000
       }
     }
     d <- d_p %>% left_join(d, by = var_g)
   }

# axis labels -------------------------------------------------------------

  nms_dic <- setNames(dic_p$label, dic_p$id)

  labelsXY <- labelsXY(hor_title = opts$title$hor_title %||% nms_dic[[(1 + (length(nms_dic) - 2))]], #
                       ver_title = opts$title$ver_title %||% nms_dic[[length(nms_dic)]],
                       nms = nms_dic, orientation = opts$chart$orientation)

  hor_title <- as.character(labelsXY[1])
  ver_title <- as.character(labelsXY[2])



# color -------------------------------------------------------------------

  palette <- opts$theme$palette_colors
  palette_type <- opts$theme$palette_type %||% "categorical"

  color_by <- NULL
  if (!is.null(opts$style$color_by)) color_by <- names(nms[match(opts$style$color_by, nms)])

  if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 2) color_by <- "a"

  if(is.null(palette)){
    palette <- opts$theme[[paste0("palette_colors_", palette_type)]]
  }

  if ("color" %in% dic$hdType) {
    d$..colors <- d[[dic$id[dic$hdType == "color"][1]]]
  } else {
    if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 1 && sum(grepl("Dat", ftype_vec)) == 1) {
      d$..colors <- palette[1]
    } else {
      d$..colors <- paletero::map_colors(d, color_by, palette, colors_df = NULL)
    }
  }

  if (!is.null(opts$chart$highlight_value)) {
    if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 2) d$..colors <- palette[1]
    w <- grep(paste0(opts$chart$highlight_value, collapse = '|'), d[[color_by %||% "a"]])
    d$..colors[w] <- opts$chart$highlight_value_color
  }


# order -------------------------------------------------------------------


  if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 1) {
    if (!grepl("Dat", ftype)) {
      d <- order_category(d, col = "a", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)
    }
  }

  if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 2) {
  d <- order_category(d, col = "a", order = opts$postprocess$order_legend, label_wrap = opts$style$label_wrap_legend)

  if (!grepl("Dat", frtype)) {
    d <- order_category(d, col = "b", order = opts$postprocess$order, label_wrap = opts$style$label_wrap)
  }
}

# end options -------------------------------------------------------------

  list(
   d = d,
   titles = list(
     title = opts$title$title,
     subtitle = opts$title$subtitle,
     caption = opts$title$caption %||% "",
     x = hor_title,
     y = ver_title
   )
  )

}
