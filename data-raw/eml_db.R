## 2018-11-28
##
## Creates the `eml_db` reference list objects as internal data
## (in R/sysdata.rda) used by many core EML functions.
##
## The JSON files are currently created by XSD parsing scripts in
## data-raw/eml_schema_parser.R


eml_db <- list("eml-2.1.1" = jsonlite::read_json("data-raw/eml-2.1.1.json"),
               "eml-2.2.0" = jsonlite::read_json("data-raw/eml-2.2.0.json"))
usethis::use_data(eml_db, overwrite = TRUE, internal = TRUE)
