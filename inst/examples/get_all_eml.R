library("httr")
library("jsonlite")
library("jsonld")
library("tidyverse")
source(system.file("examples/solr_functions.R", package="emld"))

## See: https://cn.dataone.org/cn/v2/query/solr

numFound <-
  GET(paste0("https://knb.ecoinformatics.org/knb/d1/mn/v2/query/solr",
             "?q=-obsoletedBy:*&fl=identifier&rows=1")) %>%
  content() %>%
  xml2::xml_find_first("//result") %>%
  xml2::xml_attr("numFound") %>%
  as.integer()
message(paste(numFound, "records found"))
rows <- 1000
n_calls <- numFound %/% rows



query <-
  paste0(
    "https://knb.ecoinformatics.org/knb/d1/mn/v2/query/solr?",
    "q=*:*&fl=identifier,",
    "formatId,dataUrl,fileName",
    "&formatType=METADATA",
    "&wt=json",
    "&-obsoletedBy:*",
    "&start=", rows*(0:n_calls), "&rows=", rows)

solr_df <- function(q){
  message(paste(q))
  resp <- GET(q)
  if(!grepl("json", headers(resp)$`content-type`))
    return(tibble())
  json <- jsonlite::fromJSON(content(resp, as = "text"))
  json$response$docs
}

df <- map_dfr(query, solr_df)

eml <- df %>%
  filter(grepl("eml://", formatId)) %>%
  mutate(fileName = paste0("emls/", basename(identifier), ".xml"))

safe_down <- safely(download.file)

dir.create("emls")
map2(eml$fileName, eml$dataUrl, ~safe_down(.y, .x))
