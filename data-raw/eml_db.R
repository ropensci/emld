
eml_db <- list("eml-2.1.1" = jsonlite::read_json("data-raw/eml-2.1.1.json"),
                   "eml-2.2.0" = jsonlite::read_json("data-raw/eml-2.2.0.json"))
devtools::use_data(eml_db, overwrite = TRUE, internal = TRUE)

