library(magrittr)
library(xml2)
library(jsonlite)

ex <- system.file("extdata/hf205.xml", package="emljson")
xml <- xml2::read_xml(ex)

as_list(xml) %>%
  jsonlite::write_json("ex.json", pretty=TRUE, auto_unbox = TRUE)
