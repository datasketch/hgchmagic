library(tidyverse)
library(homodatum)
# Create meta data for funs
db <- tools::Rd_db("hgchmagic")
if(length(db)==0) stop("Restart session")
f <- function(rd){
  #rd <- db[[1]]
  rd <- capture.output(rd)
  con <- textConnection(rd)
  l <- Rd2roxygen::parse_file(con)
  l <- map_if(l,~length(.)==0,function(x)'')
  map(l,paste,collapse = "_")
}
funs <- map(db,f)
funsMeta <- funs %>% bind_rows()
funsMeta <- funsMeta %>% filter(grepl("^hgch_",name))
meta <- funsMeta[c("name","title","desc","section")]
meta$ctypes <- stringr::str_extract(meta$section,"(?<=\n).*?(?=\n)$")
meta$section <- NULL
meta$ftype <- ctypesToFtype(meta$ctypes, as_string = TRUE)
meta$group <- stringr::str_extract(meta$name,"(?<=_).*?(?=_)")
write_csv(meta,file.path("inst","meta.csv"))
