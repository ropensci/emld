
eml2.1.1 <- jsonlite::read_json("data-raw/eml-2.1.1.json")
eml2.2.0 <- jsonlite::read_json("data-raw/eml-2.2.0.json")
eml_schema <- list(eml2.1.1 = eml2.1.1, eml2.2.0 = eml2.2.0)
devtools::use_data(eml_schema, overwrite = TRUE, internal = TRUE)

