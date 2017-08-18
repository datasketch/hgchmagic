
#' @export
hgchMeta <- function(){
  read_csv(system.file("meta.csv",package = "hgchmagic"))
}


#' @export
hgchWhich <- function(d){
  meta <- hgchMeta()
  guessedctypes <- guessCtypes(d, as_string = TRUE) # TODO possibleFtypes
  meta %>% filter(ctypes == guessedctypes)
}

#' @export
hgchCtypes <- function(ctypesIn){
  meta <- hgchMeta()
  meta %>% filter(ctypes == ctypesIn)
}

#' @export
hgchNames <- function(){
  hgchMeta()$name
}




