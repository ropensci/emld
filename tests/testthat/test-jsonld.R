library(jsonld)
library(jsonlite)
library(magrittr)
library(xml2)
library(testthat)
library(emld)


test_that("we can roundtrip JSON-LD expansion and compaction", {

  context <- '{"@context": {"@vocab": "http://ecoinformatics.org/"}}'
  ex <- system.file("extdata/hf205.json", package = "emld")

  roundtrip <-
    jsonld_expand(ex) %>%
    jsonld_compact(context)

  A <- unlist(read_json(ex))
  B <- unlist(fromJSON(roundtrip,simplifyVector = FALSE))
  testthat::expect_equivalent(length(A),
                              length(B))

})


test_that("We can validate after roundtrip with default xml2 methods", {

  ## In order to roundtrip, we need to fix some mangling of read_xml and as_list
  ## The schemaLocation attribute needs to get its namespace prefix back.
  ## and the root eml node needs to be first be recreated, and then
  ## to get its namespace prefix back.  ICK
  rename_attr <- function(x, old, new){
    attr(x, new) <- attr(x, old)
    attr(x, old) <- NULL
    x
  }
  rename_root <- function(xml, n){
    root <- xml_root(xml)
    xml_name(root) <- n
    xml
  }

  xml <- system.file("extdata/example.xml", package = "emld")
  xml %>%
    xml2::read_xml() %>%
    xml2::as_list() %>%
    rename_attr("schemaLocation", "xsi:schemaLocation") %>%
    list('eml' = .) %>%
    xml2::as_xml_document() %>%
    rename_root("eml:eml") %>%
    xml2::write_xml("test.xml")
  testthat::expect_true(EML::eml_validate("test.xml"))
})

test_that("We can go from emld into valid EML", {
## NOT WORKING -- NEED TO WRITE CUSTOM as_xml_document METHOD STILL
context <- '{"@context": {"@vocab": "http://ecoinformatics.org/"}}'
system.file("extdata/hf205.json", package = "emld") %>%
  jsonld_expand() %>%
  jsonld_compact(context) %>%
  fromJSON(simplifyVector = FALSE) %>%
  getElement("eml") %>%
  list("eml" = .) %>%
  xml2::as_xml_document() %>%
  rename_root("eml:eml") %>%
  xml2::write_xml("test.xml", options = "format")
#EML::eml_validate("test.xml")
#testthat::expect_true(EML::eml_validate("test.xml"))
})


