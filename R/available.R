
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
  # db <- db[grepl("^hgch_.*$",names(db))]
  # meta <- lapply(db, tools:::.Rd_get_section, "section")
  # i <<- 1
  # cleanFtypeDoc <- function(ftype){
  #   message(i)
  #   i <<- i + 1
  #   if(identical(ftype[[1]], list())) return()
  #   ftype <- as.character(ftype[[1]])
  #   if(identical(ftype, character(0))) return(0)
  #   ftype <- ftype[length(ftype)-1]
  #   gsub(" |\n","",ftype)
  # }
  # meta <- lapply(meta,cleanFtypeDoc)
  # names(meta) <- gsub(".Rd","",names(meta))
  # if(!is.null(hgch)) return(meta[[hgch]])
  # meta

  db <- db[grepl("^hgch_.*$",names(db))]
  names(db) <- gsub(".Rd","",names(db))
  f <- function(dbi) {
    # dbi <- db[[i-1]]
    x <- as.character(dbi)
    xx <- str_extract(x, " (.*)\n")
    xxx <- xx[grepl("-|Ca|Ye|Nu|Da|NuP", xx)]
    xxx <- xxx[!duplicated(xxx)]
    ftype <- as.character(paste(xxx, collapse = ", "))
    strsplit(gsub(" |\n","",ftype),",")[[1]]
  }
  results <- purrr::map(db,f)
  names(results) <- gsub(".Rd","",names(results))
  return(results)
}

#' @export
hgchNames <- function(hgch = NULL){
  db <- tools::Rd_db("hgchmagic")
  db <- db[grepl("^hgch_.*$",names(db))]
  cleanFtypeDoc <- function(ftype){
    ftype <- as.character(ftype[[1]])
    if(length(ftype) > 1){
      options(warn=-1)
      ftype <- ftype[1:which(ftype == "\n")]
      ftype <- ftype[ftype != "\n"]
    }
    return(paste(gsub("\n|\\}","",ftype), collapse = ""))
  }
  meta <- lapply(db,cleanFtypeDoc)
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


ftype <- db[grepl("\\%", db)]
ftype <- ftype[[1]]
ftype <- db[[1]]
