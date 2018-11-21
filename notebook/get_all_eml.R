library("httr")
library("jsonlite")
library("jsonld")
library("tidyverse")
library("xml2")
source(system.file("examples/solr_functions.R", package="emld"))

## See: https://cn.dataone.org/cn/v2/query/solr/

eml_solr_query <-
"http://cn.dataone.org/cn/v2/query/solr/?q=formatId:eml*+AND+-obsoletedBy:*"

numFound <-
  GET(eml_solr_query) %>%
  content() %>%
  xml2::xml_find_first("//result") %>%
  xml2::xml_attr("numFound") %>%
  as.integer()
message(paste(numFound, "records found"))
rows <- 1000
n_calls <- numFound %/% rows



query <-
  paste0(eml_solr_query,
    "&fl=identifier,formatId,dataUrl,fileName",
    "&wt=json",
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
  mutate(name = paste0("emls/", gsub("[\\/\\.]", "_", identifier), ".xml"))

readr::write_csv(eml, "eml-solrs.csv.bz2")


safe_down <- safely(function(url, dest){
  if(!file.exists(dest))
    download.file(url, dest)
})

dir.create("emls", FALSE)
map2(eml$name, eml$dataUrl, ~safe_down(.y, .x))
