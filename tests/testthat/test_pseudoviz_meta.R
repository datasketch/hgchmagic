context("hgchmagic meta data")

test_that("Viz meta info", {
  #library(hgchmagic)
  db <- Rd_db("hgchmagic")
  meta <- map(db, tools:::.Rd_get_section, "section")
  meta <- meta[grepl("test_docs",names(meta))]
  ftype <- meta$gg_test_docs.Rd
  expect_error(hgchmagic:::cleanFtypeDoc(ftype),"No section name ftype")
  ftype <- meta$gg_test_docs2.Rd
  expect_equal(hgchmagic:::cleanFtypeDoc(ftype),c("Ca","Ca-Ca-Ca"))
  #ggFtype()
  #expect_error()
  #expect_true()
  #expect_false()
})


