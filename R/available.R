
#' @export
hgchMeta <- function(){
  read_csv(system.file("meta.csv",package = "hgchmagic"))
}

#' @export
hgchWhich <- function(d){
  meta <- hgchMeta()
  guessedftype <- guessFtype(d) # TODO possibleFtypes
  meta %>% filter(ftype == guessedftype)
}

#' @export
hgchFtype <- function(ftypeIn){
  meta <- hgchMeta()
  meta %>% filter(ftype == ftypeIn)
}

#' @export
hgchNames <- function(){
  hgchMeta()$name
}




