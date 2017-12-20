context("json_to_xml")

## FIXME xml_to_json does not parse everything correctly!


library(xml2)
devtools::load_all()
f <- system.file("extdata/hf205.xml", package="emld")
json <- xml_to_json(f)
l <- jsonlite::fromJSON(json, simplifyVector = FALSE)

l <- jsonlite::read_json(system.file("extdata/hf205.json", package = "emld"))

xml <- as_eml_document(l)
xml2::write_xml(xml, "test.xml")



