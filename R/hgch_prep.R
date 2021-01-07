#' @export
hgchmagic_prep <- function(data, opts = NULL, extra_pattern = ".", plot =  "bar", ftype = "Cat-Num"){

  if (is.null(data)) return()

  #ftype <- "Cat-Num"
  #data <- sample_data("Cat-Num-Dat-Num-Cat-Num-Num")


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
    if (length(grep("Dat|Cat|Yea", ftype_vec)) == 1) {
      d_p <- dsvizopts:::summarizeData(d_p, opts$summarize$agg, to_agg = b, a)
    } else {
      d_p <- dsvizopts:::summarizeData(d_p, opts$summarize$agg, to_agg = c, a, b)
    }
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
    } else {
      if (has_num_var) {
        d <- d[,-3]
        agg_var <- "c"
      }
      d <- d %>%
        group_by(a, b) %>%
        summarise_each(funs(func_paste))
    }
    d <- d_p %>% left_join(d, by = var_g)
  }



# processing of data ------------------------------------------------------


  if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 1) {  #just an aggregation variable

    if (grepl("Dat", ftype)) {
      d <- d %>% drop_na()
    } else {
      d <- preprocessData(d, drop_na = opts$preprocess$drop_na,
                          na_label = opts$preprocess$na_label, na_label_cols = "a")
    }
    d <- postprocess(d, agg_var, sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)
  } else if (sum(grepl("Dat|Cat|Yea", ftype_vec)) == 2) {
    if (grepl("Dat", ftype)) {
      d <- d %>% drop_na(b)
    } else {
      d <- preprocessData(d, drop_na = opts$preprocess$drop_na,
                          na_label = opts$preprocess$na_label, na_label_cols = "b")
    }

    d <- preprocessData(d, drop_na = opts$preprocess$drop_na_legend,
                        na_label = opts$preprocess$na_label, na_label_cols = "a")
    d <- postprocess(d, agg_var, sort = opts$postprocess$sort, slice_n = opts$postprocess$slice_n)
  } else {
    d <- d
  }

  nms_dic <- setNames(dic_p$label, dic_p$id)
  labelsXY <- labelsXY(hor_title = opts$title$hor_title %||% nms_dic[[1]],
                       ver_title = opts$title$ver_title %||% nms_dic[[length(nms_dic)]],
                       nms = nms_dic, orientation = opts$chart$orientation)

  hor_title <- as.character(labelsXY[1])
  ver_title <- as.character(labelsXY[2])




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
