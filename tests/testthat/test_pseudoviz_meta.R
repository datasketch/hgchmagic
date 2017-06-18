context("hgchmagic meta data")

test_that("Viz meta info", {
  #library(hgchmagic)
  db <- Rd_db("hgchmagic")
  meta <- map(db, tools:::.Rd_get_section, "section")
  meta <- meta[grepl("test_docs",names(meta))]
  ctypes <- meta$hgch_test_docs.Rd
  expect_error(hgchmagic:::cleanFtypeDoc(ctypes),"No section name ctypes")
  ctypes <- meta$hgch_test_docs2.Rd
  expect_equal(hgchmagic:::cleanFtypeDoc(ctypes),c("Ca","Ca-Ca-Ca"))
  #hgchFtype()
  #expect_error()
  #expect_true()
  #expect_false()
})


