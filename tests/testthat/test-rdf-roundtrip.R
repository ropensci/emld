testthat::context("test rdf roundtrips")

library(rdflib)
library(jsonlite)
library(jsonld)
library(magrittr)
library(testthat)

f <- system.file("tests/eml.xml", package="emld")

test_that("We can roundtrip into rdf and validate", {

  f %>%
  as_emld() %>%
  as_json() %>%
  rdf_parse("jsonld") %>%
  rdf_serialize("eml.json", "jsonld")

## frame & compact explicitly, even though as_emld should now do this on json input
frame <- system.file("frame/eml-frame.json", package = "emld")
context <- system.file("context/eml-context.json", package = "emld")
jsonld_frame("eml.json", frame) %>%
  jsonld_compact(context) %>%
  as_emld() %>%
  as_xml("eml.xml")
  expect_true(eml_validate("eml.xml"))
})

test_that("We can roundtrip out to rdf-xml", {

## Into RDF-XML
f %>%
  as_emld() %>%
  as_json() %>%
  rdf_parse("jsonld") %>%
  rdf_serialize("eml.rdf", "rdfxml")

## Back into jsonld (via rdflib)
rdf_parse("eml.rdf", "rdfxml") %>%
  rdf_serialize("eml.json", "jsonld")


## Prove that as_emld frames and compacts automatically:
as_emld("eml.json") %>%
  as_xml("eml.xml")
expect_true(eml_validate("eml.xml"))

})


unlink("eml.rdf")
unlink("eml.json")
unlink("eml.xml")

