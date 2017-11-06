library(jsonld)
library(jsonlite)
library(emljson)
library(magrittr)

context <- '{"@context": {"@vocab": "http://ecoinformatics.org/"}}'
ex <- system.file("extdata/hf205.json", package = "emljson")


roundtrip <-
  jsonld_expand(ex) %>%
  jsonld_compact(context)

## Not a very thorough test
testthat::expect_equivalent(length(read_json(ex)),
                            length(fromJSON(roundtrip)))


### Validate after Round-trip via xml2::as_list, as_xml_doc,
xml <- system.file("extdata/hf205.xml", package = "emljson")
xml %>%
  xml2::read_xml() %>%
  xml2::as_list() %>%
  list(eml = .) %>%
  xml2::as_xml_document() %>%
  xml_set_namespace("eml", "eml://ecoinformatics.org/eml-2.1.1") %>%
  write_xml("test.xml")
testthat::expect_true(EML::eml_validate(xml))
