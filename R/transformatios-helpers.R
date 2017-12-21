
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
