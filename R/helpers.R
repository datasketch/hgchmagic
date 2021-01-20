#'
#' @export
function_agg <- function (df, agg, to_agg, ...) {
  group_var <- rlang::enquos(...)

  if (is.null(to_agg)) {
    dd <- df %>%
      dplyr::group_by(!!!group_var) %>%
      summarise(..count = n())
  } else {
    dd <- df %>%
      dplyr::group_by(!!!group_var) %>%
      summarise(dplyr::across(to_agg, ~ agg(agg, .x)), ..count = n())
  }
  dd

}


#' @export
hgch_tooltip <- function(df, nms, label_ftype = NULL, tooltip) {
  if (is.null(nms)) stop("Enter names")
  nms <- nms
  nms <- gsub("[][!()*`|]", "",nms)
  label_ftype_clean <- gsub("[][!()*`|{}]", "", label_ftype)
  nms_names <- names(nms)

  if (is.null(tooltip)) {
    tooltip  <- paste0(map(seq_along(label_ftype), function(i) {
      paste0(label_ftype[i], ": {", label_ftype_clean[i], "}")
    }) %>% unlist(), collapse = "<br/>")
  } else {
    tooltip <- gsub("[][()*`|]", "", tooltip)#gsub("[][!#$()*,.:;<=>@^`|~.", "", tooltip)
  }
print(tooltip)
    points <- gsub("\\{|\\}", "",
                   stringr::str_extract_all(tooltip, "\\{.*?\\}")[[1]])
    if (identical(points, character())) {
      tooltip <- tooltip
    } else {
      l <- purrr::map(seq_along(points), function(i){
        print(points[i])
        true_points <-  paste0("{",names(nms[match(points[i], nms)]),"_label}")
        tooltip <<- gsub(paste0("\\{",points[i], "\\}"), true_points, tooltip)
      })[[length(points)]]}

  tooltip
}


#' Complete values in groups without numeric information
#' @export
completevalues <- function(d, var_num) {
  var_num <- sym(var_num)
  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA))) %>%
    tidyr::spread(b, !!var_num) %>%
    tidyr::gather(b, !!var_num, -a)
  d$a[d$a == "NA"] <- NA
  d$b[d$b == "NA"] <- NA
  # d$a[is.na(d$a)] <- "NA"
  # d$b[is.na(d$b)] <- "NA"
  d
}



tooltip_codes <- function(sample, prefix, suffix) {

  params <- makeup::which_num_format(sample)$separators
  thousandsSep <- params$thousands
  decimalPoint <- params$decimal
  n_decimal <- params$n_decimal

  l <- list(
    line = list(
      `Cat-Num` = list(
        a = "{point.category}",
        b = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Cat-Cat-Num` = list(
        a = "{series.name}",
        b = "{point.category}",
        c = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Yea-Num` = list(
        a = "{point.category}",
        b = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Cat-Yea-Num` = list(
        a = "{series.name}",
        b = "{point.category}",
        c = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      )
    ),
    bar = list(
      `Cat-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Cat-Cat-Num` = list(
        a = "{series.name}",
        b = "{point.category}",
        c = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Yea-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Cat-Yea-Num` = list(
        a = "{series.name}",
        b = "{point.category}",
        c = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      )
    ),
    pie = list(
      `Cat-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Yea-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      )
    ),
    donut = list(
      `Cat-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Yea-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      )
    ),
    bubbles = list(
      `Cat-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.value",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Cat-Cat-Num` = list(
        a = "{series.name}",
        b = "{point.name}",
        c = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Yea-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.value",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Cat-Yea-Num` = list(
        a = "{series.name}",
        b = "{point.name}",
        c = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      )
    ),
    treemap = list(
      `Cat-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.value",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Cat-Cat-Num` = list(
        a = "{point.parent}",
        b = "{point.node.name}",
        c = paste0(prefix, "{point.value",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Yea-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.value",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Cat-Yea-Num` = list(
        a = "{point.parent}",
        b = "{point.node.name}",
        c = paste0(prefix, "{point.value",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      )
    ),
    scatter = list(
      `Num-Num` = list(
        a = "{point.x}",
        b = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Cat-Num-Num` = list(
        a = "{series.name}",
        b = paste0( "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}"),
        c = paste0( "{point.x",':', thousandsSep, decimalPoint, n_decimal, "f}")
      )
    ),
    map = list(
      `Cat-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.value",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      )
    )
  )
  l
}


# esta toca completarla para lo casos en los que el formato numerico es para mas de una
# varible, como en scatter
#' @export
format_hgch <- function(plot, frtype, sample, suffix, prefix) {
  d_frtype <- strsplit(frtype, split = "-") %>% unlist()
  if (sum(grepl("Num", d_frtype)) == 0) return()

  num_var <- grep("Num", d_frtype)

  params <- makeup::which_num_format(sample)
  thousandsSep <- params$separators$thousands
  decimalPoint <- params$separators$decimal
  n_decimal <- params$separators$n_decimal

  l_tool <- tooltip_codes(sample = sample, suffix = suffix, prefix = prefix)
  l_tool[[plot]][[frtype]][[num_var]]

}





# date intervals

date_intervals <- function(date_intervals) {
  if (is.null(date_intervals)) return()

  if (date_intervals == "day") {
    t_d <- 60 * 60 * 24 * 30
  } else if (date_intervals == "month") {
    t_d <- 30 * 24 * 3600 * 1000
  } else if (date_intervals == "year") {
    t_d <- 1000 * 60 * 60 * 24 * 365
  }
  else {
    t_d <-  7 * 24 * 3600 * 1000
  }
  t_d
}

