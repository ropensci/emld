context("json_to_xml")



library(xml2)
devtools::load_all()
f <- system.file("extdata/hf205.xml", package="emld")
json <- xml_to_json(f)
l <- jsonlite::fromJSON(json, simplifyVector = FALSE)
xml <- as_eml_document(l)
xml2::write_xml(xml, "test.xml")



