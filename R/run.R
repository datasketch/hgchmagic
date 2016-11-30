
#' @export
run_gg <- function(d,ggname){
  #ggname <- "gg_waffle."

  if(validateD(d,ggname))
    do.call(ggname,list(d))
  else
    stop("D did not validate")
}

validateD <- function(d,ggname){
  guessFtype(d) %in% ggFtype(ggname)
}
