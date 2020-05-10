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

#' @export
format_hgch <- function(sample, value = "value") {
  if (is.null(sample)) return()
  params <- makeup::which_num_format(sample)
  thousandsSep <- params$separators$thousands
  decimalPoint <- params$separators$decimal
  n_decimal <- params$separators$n_decimal
  f <- paste0(value,':', thousandsSep, decimalPoint, n_decimal, "f")
  f
}
