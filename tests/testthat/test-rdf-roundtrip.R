


library(emld)
library(rdflib)
library(jsonlite)
library(jsonld)
library(magrittr)
library(testthat)

f <- system.file("tests/eml.xml", package="emld")

test_that("We can roundtrip into rdf and validate", {
x <-
  f %>%
  as_emld() %>%
  as_json() %>%
  rdf_parse("jsonld") %>%
  rdf_serialize("eml.json", "jsonld")

frame <- system.file("frame/eml-frame.json", package = "emld")
context <- system.file("context/eml-context.json", package = "emld")

jsonld_frame("eml.json", frame) %>%
  jsonld_compact(context) %>%
  as_emld() %>%
  as_xml("eml.xml")


  expect_true(EML::eml_validate("eml.xml"))
})

## Roundtrip out to RDFXML as well
x <-
  f %>%
  as_emld() %>%
  as_json() %>%
  rdf_parse("jsonld") %>%
  rdf_serialize("eml.rdf", "rdfxml")

## Not working
rdf_parse("eml.rdf", "rdfxml") %>%
  rdf_serialize("eml.json", "jsonld")

## Prove that as_emld frames and compacts as shown manually above
#as_emld("eml.json") %>%
#  as_xml("eml.xml")
#EML::eml_validate("eml.xml")

unlink("eml.rdf")
unlink("eml.json")
unlink("eml.xml")

