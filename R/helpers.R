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
