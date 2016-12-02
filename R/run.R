
#' @export
run_hgch <- function(d,hgchname){
  #hgchname <- "hgch_waffle."

  if(validateD(d,hgchname))
    do.call(hgchname,list(d))
  else
    stop("D did not validate")
}

validateD <- function(d,hgchname){
  guessFtype(d) %in% hgchFtype(hgchname)
}
