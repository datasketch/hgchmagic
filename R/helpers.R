#' Complete values in groups without numeric information
#' @export
completevalues <- function(d) {
  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = ifelse(is.character(d$b), "NA", NA),
                           c = NA)) %>%
    tidyr::spread(b, c) %>%
    tidyr::gather(b, c, -a)
  d$a[is.na(d$a)] <- "NA"
  d$b[is.na(d$b)] <- "NA"
  d
}



tooltip_codes <- function(sample, prefix, suffix) {

  params <- makeup::which_num_format(sample)$separators
  thousandsSep <- params$thousands
  decimalPoint <- params$decimal
  n_decimal <- params$n_decimal

  l <- list(
    bar = list(
      `Cat-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Cat-Cat-Num` = list(
        a = "{series.name}",
        b = "{point.category}",
        c = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      )
    ),
    pie = list(
      `Cat-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      )
    ),
    donut = list(
      `Cat-Num` = list(
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
      )
    ),
    treemap = list(
      `Cat-Num` = list(
        a = "{point.name}",
        b = paste0(prefix, "{point.value",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      ),
      `Cat-Cat-Num` = list(
        a = "{point.node.name}",
        b = "{point.name}",
        c = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
      )
    ),
    scatter = list(
      `Num-Num` = list(
        a = "{point.x}",
        b = paste0(prefix, "{point.y",':', thousandsSep, decimalPoint, n_decimal, "f}", suffix)
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

#' @export
tooltip_hgch <- function(plot, tooltip, nms, frtype, prefix,  suffix, sample) {

  l_tool <- tooltip_codes(sample, prefix, suffix)
  d_frtype <- strsplit(frtype, split = "-") %>% unlist()
  nms_names <- names(nms)

  if (is.null(tooltip)) {
    points <- l_tool[[plot]][[frtype]]
    l <- map(seq_along(nms), function(i) {
      paste0('<b>', nms[i], ': </b>', points[names(points) == nms_names[i]][[nms_names[i]]])
    }) %>% unlist()
    tooltip <- paste0(l, collapse = "</br>")
  } else {
    points <- gsub("\\{|\\}", "",
                   stringr::str_extract_all(tooltip, "\\{.*?\\}")[[1]])
    if (identical(points, character())) {
      tooltip <- tooltip
    } else {
      l <- purrr::map(1:length(points), function(i){
        true_points <-  names(nms[match(points[i], nms)])
        replace <- l_tool[[plot]][[frtype]][[true_points]]
        tooltip <<- gsub(paste0("\\{",points[i], "\\}"), replace, tooltip)
      })[[length(points)]]
    }
  }

  tooltip

}
