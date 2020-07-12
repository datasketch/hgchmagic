
`%||%` <- function (x, y) {
  suppressWarnings({
    if (is.empty(x))
      return(y)
    else if (is.null(x) || is.na(x))
      return(y)
    else if (class(x) == "character" && all(nchar(x) == 0))
      return(y)
    else x
  })
}

is.empty <- function (x){
  if (length(x) == 0)
    return(TRUE)
  if (length(x) == 1 && nchar(x) == 0)
    return(TRUE)
  !as.logical(length(x))
}
