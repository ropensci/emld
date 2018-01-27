
eml_db <- jsonlite::read_json("data-raw/eml_db.json")

devtools::use_data(eml_db, overwrite = TRUE, internal = TRUE)
