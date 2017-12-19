
agg <- function(aggregation,...){
  f <- NULL
  if(aggregation == "sum")
    f <- sum(..., na.rm = TRUE)
  if(aggregation == "mean")
    f <- mean(...,na.rm = TRUE)
  if(aggregation == "median")
    f <- median(...,na.rm = TRUE)
  f
}

parseNA <- function(d, filter = TRUE) {
  if (filter) {
    d %>%
      drop_na()
  } else {
    d %>%
      mutate(a = ifelse(is.na(a), 'NA', a))
  }
}

getAxisLabes <- function(labels, nms, alignment, cTypes) {
  outputLabels <- 1:2 %>%
    purrr::map(function(i_labels) {
      if (is.null(labels[[i_labels]])) {
        if (length(cTypes) == 1) {

        } else {

        }
      } else {
        labels[[i_labels]]
      }
    })
}
