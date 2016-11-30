
#' gg_test_docs
#' test
#' @name gg_test_docs
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section noftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
gg_test_docs <- NULL

#' gg_test_docs2
#' test
#' @name gg_test_docs2
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Ca-Ca
#' @examples
#' add(1, 1)
gg_test_docs2 <- NULL

#' @export
ggWhich <- function(d){
  pf <- ggFtype()
  ftype <- guessFtype(d) # TODO possibleFtypes
  names(keep(pf, ~ ftype %in% .))
}


#' @export
vizList <- function(){
  db <- Rd_db("hgchmagic")
  meta <- unname(map_chr(db, tools:::.Rd_get_name))
  keep(meta, ~ grepl("^hgch_*$",.))
}


#' @export
hgchFtype <- function(hgch = NULL){
  db <- Rd_db("ciudatos")
  db <- db[grepl("^hgch_.*$",names(db))]
  meta <- lapply(db, tools:::.Rd_get_section, "section")
  cleanFtypeDoc <- function(ftype){
    ftype <- as.character(ftype[[2]][[2]])
    strsplit(gsub(" |\n","",ftype),",")[[1]]
  }
  meta <- lapply(meta,cleanFtypeDoc)
  names(meta) <- gsub(".Rd","",names(meta))
  if(!is.null(hgch)) return(meta[[hgch]])
  meta
}



#' #' @export
#' ggList <- function(type = NULL,wrongNames = FALSE){
#'   #http://stackoverflow.com/questions/7495685/how-to-access-the-help-documentation-rd-source-files-in-r
#'   db <- Rd_db("hgchmagic")
#'   meta <- unname(map_chr(db, tools:::.Rd_get_name))
#'   meta <- meta[!grepl("gg_test_docs",meta)]
#'   if(wrongNames) return(keep(meta, ~ !grepl("^gg_.*\\.$",.)))
#'   ggs <- keep(meta, ~ grepl("^gg_.*\\.$",.))
#'   if(!is.null(type))
#'     return(ggs[grepl(type,ggs)])
#'   ggs
#' }
#'
#' #' @export
#' ggFtype <- function(gg = NULL){
#'   db <- Rd_db("hgchmagic")
#'   meta <- map(db, tools:::.Rd_get_section, "section")
#'   meta <- meta[!grepl("gg_test_docs",names(meta))]
#'   #ftype <- meta$gg_test_docs.Rd
#'   safe_cleanFtypeDoc <- safely(hgchmagic:::cleanFtypeDoc)
#'   parsedMeta <- map(meta,safe_cleanFtypeDoc)
#'   results <- parsedMeta %>% map(~.$result)
#'   errors <- parsedMeta %>% map(~.$error) %>% purrr::discard(is.null)
#'   names(results) <- gsub(".Rd","",names(results))
#'   names(errors) <- gsub(".Rd","",names(errors))
#'   if(!is_empty(errors))
#'     stop("Something wrong with ftypes for:\n",paste(names(errors),collapse = "\n  "))
#'   if(!is.null(gg)) return(results[[gg]])
#'   results
#' }
#'
#'
#' cleanFtypeDoc <- function(ftype){
#'   sectionName <- as.character(ftype[[1]][[1]])
#'   if(sectionName != "ftypes") stop("No section name ftype")
#'   ftype <- as.character(ftype[[2]][[2]])
#'   strsplit(gsub(" |\n","",ftype),",")[[1]]
#' }
