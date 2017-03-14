
#' #' hgch_test_docs
#' #' test
#' #' @name hgch_test_docs
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section noftypes: Ca,Ca-Nu
#' #' @examples
#' #' add(1, 1)
#' hgch_test_docs <- NULL
#'
#' #' hgch_test_docs2
#' #' test
#' #' @name hgch_test_docs2
#' #' @export
#' #' @return The sum of \code{x} and \code{y}.
#' #' @section ftypes: Ca,Ca-Ca-Ca
#' #' @examples
#' #' add(1, 1)
#' hgch_test_docs2 <- NULL

#' @export
hgchWhich <- function(d){
  pf <- hgchFtype()
  ftype <- guessFtype(d) # TODO possibleFtypes
  names(keep(pf, ~ ftype %in% .))
}


#' @export
vizList <- function(){
  db <- tools::Rd_db("hgchmagic")
  meta <- unname(purrr::map_chr(db, tools:::.Rd_get_name))
  meta <- meta[meta != "hgchWhich"]
}

#' @export
hgchFtype <- function(hgch = NULL){
  db <- tools::Rd_db("hgchmagic")
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

#' @export
hgchNames <- function(hgch = NULL){
  db <- tools::Rd_db("hgchmagic")
  db <- db[grepl("^hgch_.*$",names(db))]
  meta <- lapply(db, tools:::.Rd_get_section, "title")
  cleanFtypeDoc <- function(ftype){
    ftype <- as.character(ftype[[1]][[1]])
    strsplit(gsub("\n","",ftype),",")[[1]]
  }
  meta <- lapply(meta,cleanFtypeDoc)
  names(meta) <- gsub(".Rd","",names(meta))
  if(!is.null(hgch)) return(meta[[hgch]])
  meta
}


#' #' @export
#' hgchList <- function(type = NULL,wrongNames = FALSE){
#'   #http://stackoverflow.com/questions/7495685/how-to-access-the-help-documentation-rd-source-files-in-r
#'   db <- Rd_db("hgchmagic")
#'   meta <- unname(map_chr(db, tools:::.Rd_get_name))
#'   meta <- meta[!grepl("hgch_test_docs",meta)]
#'   if(wrongNames) return(keep(meta, ~ !grepl("^hgch_.*\\.$",.)))
#'   hgchs <- keep(meta, ~ grepl("^hgch_.*\\.$",.))
#'   if(!is.null(type))
#'     return(hgchs[grepl(type,hgchs)])
#'   hgchs
#' }
#'
#' #' @export
#' hgchFtype <- function(hgch = NULL){
#'   db <- Rd_db("hgchmagic")
#'   meta <- map(db, tools:::.Rd_get_section, "section")
#'   meta <- meta[!grepl("hgch_test_docs",names(meta))]
#'   #ftype <- meta$hgch_test_docs.Rd
#'   safe_cleanFtypeDoc <- safely(hgchmagic:::cleanFtypeDoc)
#'   parsedMeta <- map(meta,safe_cleanFtypeDoc)
#'   results <- parsedMeta %>% map(~.$result)
#'   errors <- parsedMeta %>% map(~.$error) %>% purrr::discard(is.null)
#'   names(results) <- gsub(".Rd","",names(results))
#'   names(errors) <- gsub(".Rd","",names(errors))
#'   if(!is_empty(errors))
#'     stop("Something wrong with ftypes for:\n",paste(names(errors),collapse = "\n  "))
#'   if(!is.null(hgch)) return(results[[hgch]])
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
