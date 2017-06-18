#
# .onLoad <- function(libname, pkgname){
#   if(!"hgchmagic" %in% installed.packages()){
#     warning("Install hgchmagic first")
#     return()
#   }
#   # If loaded with devtools
#   if (pkgname %in% devtools::dev_packages()) {
#     dir <- "inst"
#   }
#   # If not loaded with devtools
#   if (!(pkgname %in% devtools::dev_packages())) {
#     dir <- find.package(pkgname, lib.loc = NULL,quiet = TRUE)
#     if(file.exists(file.path(dir,"meta.csv"))) return()
#   }
#   db <- tools::Rd_db("hgchmagic")
#   if(length(db)== 0){
#     stop("db returned empty, restart session")
#     return()
#   }
#   f <- function(rd){
#     rd <- capture.output(rd)
#     con <- textConnection(rd)
#     l <- Rd2roxygen::parse_file(con)
#     l <- map_if(l,~length(.)==0,function(x)'')
#     map(l,paste,collapse = "_")
#   }
#   funs <- map(db,f)
#   funsMeta <- funs %>% bind_rows()
#   funsMeta <- funsMeta %>% filter(grepl("^hgch_",name))
#   meta <- funsMeta[c("name","title","desc","section")]
#   meta$ftype <- stringr::str_extract(meta$section,"(?<=\n).*?(?=\n)$")
#   meta$section <- NULL
#   write_csv(meta,file.path(dir,"meta.csv"))
#   #message(dir)
#
# }
#
