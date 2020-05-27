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

#' #' @export
#' tooltip_hgch <- function(nms, plot, frtype, tooltip, suffix, prefix, format_num) {
#'
#'   data <- data.frame(cosas = c("Piedra", "Papel", "Tijera"), total = c(23, 45, 111))
#'   data <- sample_data("Num-Num")
#'   f <- homodatum::fringe(data)
#'   nms <- fringe_labels(f)
#'   d <- fringe_d(f)
#'
#'   frtype_d <- f$frtype
#'   d_frtype <- strsplit(frtype_d, split = "-") %>% unlist()
#'
#'   tooltip <- "este es el tooltip de {cosas} con un total de {total}"
#'
#'   plot <- "scatter"
#'   frtype <- frtype_d
#'   d_frtype <- strsplit(frtype, split = "-") %>% unlist()
#'   format_num <- format_hgch("1 243,2")
#'   num_p <- grep("Num",d_frtype)
#'   if (length(num_p) > 1) {
#'   num_format <- paste0('<b>',nms[[1]], ':</b>',  prefix,'{point.', format_num,'}', suffix)
#'   }
#'
#'   if (is.null(tooltip)) {
#'   points <- hgchmagic:::tooltip_codes[[plot]][[frtype]]
#'
#'   paste0('<b>', nms[1], ': </b>', points[[1]], '</br>',
#'          '<b>', nms[2], ': </b>', points[[2]],'</br>',
#'          nms[3], ': ',
#'          prefix,'{point.', format_num,'}', suffix)
#'
#'   } else {
#'     points <- gsub("\\{|\\}", "",
#'                    stringr::str_extract_all(tooltip, "\\{.*?\\}")[[1]])
#'     if (identical(points, character())) {
#'       tooltip <- tooltip
#'     } else {
#'       tooltip <- purrr::map(1:length(points), function(i){
#'         true_points <-  names(nms[match(points[i], nms)])
#'         replace <- hgchmagic:::tooltip_codes[[plot]][[frtype]][[true_points]]
#'         tooltip <<- gsub(paste0("\\{",points[i], "\\}"), replace, tooltip)
#'     })[[length(points)]]
#'     }
#'   }
#'
#'
#' }
