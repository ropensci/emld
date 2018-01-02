
library(emld)
library(rdflib)
library(jsonlite)
library(jsonld)
library(magrittr)

f <- system.file("tests/eml.xml", package="emld")
## FIXME must declare a @base or local ids get dropped when serializing to RDF
## Ideally this should be done by jsonld::jsonld_to_rdf; or at least somewhere
## inside rdf_parse.  Probably not a bad idea to do in as_emld() though
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


# EML::eml_validate("eml.xml")


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
#as_emld("eml.json") %>%
#  as_xml("eml.xml")
#EML::eml_validate("eml.xml")

unlink("eml.rdf")
unlink("eml.json")
unlink("eml.xml")

